set.seed(310366)


test_that("mle basics",{
  xytower=
    rbind(c(750142, 4393673,  48),
          c(751674, 4393630, 331),
          c(750985, 4395022, 160)
          )
  xytower = data.frame(xytower)
  names(xytower)=c("x","y","bearing")
  xytower$theta = theta(xytower$bearing)
  tri = trimle(xytower[,c("x","y")],xytower$bearing,ijob=1,sd=2.5)
#  print(tri)
})


