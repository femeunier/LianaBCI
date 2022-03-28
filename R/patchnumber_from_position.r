patchnumber_from_position <- function(x,y,patch_X,patch_Y,X0 = 0,Y0 = 0,extr_x = NA,extr_y = NA){

  # x = BCI.all$gx
  # y = BCI.all$gy
  # patch_X = Delta_XY
  # patch_Y = Delta_XY

  if (is.na(extr_x[1])){
    extr_x <- round(extremum(x,na.rm = TRUE))
    extr_y <- round(extremum(y,na.rm = TRUE))
  }

  N_X <- ceiling((extr_x[2]-extr_x[1])/patch_X)
  N_Y <- ceiling((extr_y[2]-extr_y[1])/patch_Y)

  N_x_float <- (extr_x[2]-extr_x[1])/patch_X
  N_y_float <- (extr_y[2]-extr_y[1])/patch_Y

  x <- (x - X0)
  y <- (y - Y0)

  x[x < 0] <- x[x < 0] + N_x_float*patch_X
  y[y < 0] <- y[y < 0] + N_y_float*patch_Y

  N <- N_X*N_Y

  ix = 1 + (x - x%%patch_X)/patch_X
  iy = 1 + (y - y%%patch_Y)/patch_Y

  patch = (iy-1)*N_X + ix

  patch.size <- rep(patch_X*patch_Y,length(x))

  if (N_x_float != N_X) {
    patch.size[ix == N_X] <- (N_x_float - (N_X - 1))*patch.size[ix == N_X]
  }

  if (N_y_float != N_Y) {
    patch.size[iy == N_Y] <- (N_y_float - (N_Y - 1))*patch.size[iy == N_Y]
  }

  return(list(patch = patch,
              patch_X = ix,
              patch_Y = iy,
              patch_size = patch.size))
}
