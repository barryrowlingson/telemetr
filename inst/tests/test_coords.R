set.seed(310366)

test_that("coords",{
  xytower=
    rbind(c(750142, 4393673,  48),
          c(751674, 4393630, 331),
          c(750985, 4395022, 160)
          )
  xytower = data.frame(xytower)
  expect_error(.getCoords(xytower),"Can't get coords")
  names(xytower)=c("a","b","c")
  expect_error(.getCoords(xytower),"Can't get coords")
  names(xytower)=c("x","y","bearing")
  expect_equal(.getCoords(xytower),xytower[,c("x","y")])
  coordinates(xytower)=~x+y
  expect_equal(.getCoords(xytower),coordinates(xytower))
  
})
