#' triangulate by mle
#'
#' @param xytower 2 column matrix of xy coords
#' @param az azimuth angles in degrees
#' @param ijob whether to fit kappa
#' @param kappa value of kappa
#' @param sd value of sd
#' @export
trimle <- function(xytower,az,ijob=1,kappa=1,sd=2.5){

  ntower = nrow(xytower)
  itower = 1:ntower
  xytower=t(as.matrix(xytower))
  stopifnot(ntower==3)
  coor = c(99,99)
  vc = matrix(-1,2,2)
  az = (90.-abs(az))*(pi/180.)
  print(xytower)
  res = .Fortran("trimle",
    tutm = as.double(xytower),
    az = as.double(az),
    itower = as.integer(itower),
    ntower = as.integer(ntower),
    coor = as.double(coor),
    sd = as.double(sd),
    kappa = as.double(kappa),
    vc = as.double(vc),
    ijob = as.integer(ijob),
    ierr = as.integer(0)
    )
  return(res)
}
