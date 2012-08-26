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

## 3 readings for each of 4 animals on each of 2 days


testM <- function(m){
test_that("extraction",{

  expect_error(.getxybg())
  expect_error(.getxybg(~nope,m))
  expect_error(.getxybg(date~bearing,m),"must be empty") # nothing on LHS
  
  t1 = .getxybg(~bearing,m)
  #  expect_error(.getxybg(~bearing|day,m),"not found") # wrong name
  t2 = .getxybg(~bearing|date,m)
  t3 = .getxybg(~bearing|date+animal,m)
  
  expect_equal(nrow(t1),nrow(m))
  expect_equal(ncol(t1),3+1)      # extra ID column
  expect_equal(nrow(t2),nrow(m))
  expect_equal(ncol(t2),3+1)      # add date column
  expect_equal(nrow(t3),nrow(m))
  expect_equal(ncol(t3),3+2)      # add date and animal columns

  expect_equal(names(t1),c("x","y","bearing","ID"))
  expect_equal(names(t2),c("x","y","bearing","date"))
  expect_equal(names(t3),c("x","y","bearing","date","animal"))
  
  
})
}


m = makeMoreTriData(ntowers=3,animals=letters[1:4],dates=as.Date("2001/12/1")+0:1)
expect_equal(nrow(m),3*4*2)

testM(m)

