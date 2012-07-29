test_that("triang basics",{

  m = makeMoreTriData(ntowers=3,animals=letters[1:4],dates=as.Date("2001/12/1")+0:1)

  expect_error(triang())
  expect_error(triang(~bearing))
  expect_error(triang(m,~bearing))

  expect_error(triang(~bearing,m,method="xyzzy"),"invalid method")
  
  expect_true(inherits(m,"Spatial"))
  tri = triang(~bearing|animal,m)
  expect_true(inherits(tri,"SpatialPointsDataFrame"))

  expect_equal(nrow(tri),4) # 4 animals
  tri = triang(~bearing|animal+date,m)
  expect_true(inherits(tri,"SpatialPointsDataFrame"))
  expect_equal(nrow(tri),8) # 4 animals * 2 days

  tri = triang(~bearing|animal,m,subset=m$animal=="a")
  expect_true(inherits(tri,"SpatialPointsDataFrame"))
  expect_equal(nrow(tri),1)
  
  
})
