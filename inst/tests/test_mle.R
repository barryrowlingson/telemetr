set.seed(310366)

test_that("mle basics",{
  xytower=
    rbind(c(750142, 4393673,  48, 0.837758),
          c(751674, 4393630, 331, 5.777040),
          c(750985, 4395022, 160, 2.792527)
          )
  xytower = data.frame(xytower)
  names(xytower)=c("x","y","bearing","theta")
  tri = trimle(xytower[,c("x","y")],xytower$bearing,ijob=1,sd=2.5)
})
