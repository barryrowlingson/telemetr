set.seed(310366)

test_that("conversions",{
expect_equal(theta(0),pi/2)
expect_equal(theta(90),0)
expect_equal(theta(180),1.5 * pi)
expect_equal(theta(270),pi)

expect_equal(bearing(0),90)
expect_equal(bearing(-pi/2),180)
expect_equal(bearing(pi/2),0)
expect_equal(bearing(pi),270)

})
