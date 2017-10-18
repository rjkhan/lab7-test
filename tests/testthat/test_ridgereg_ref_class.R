context("ridgereg")

data("iris")
Polygon <- setRefClass("Polygon", fields = c("sides"))
square <- Polygon$new(sides = 4)
test_that("ridgereg rejects errounous input", {
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(ridgereg_mod <- ridgereg$new(formula = Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})
#comparing cofficients
test_that("coef() method works", {
  a<- ridgereg(Petal.Length ~ Sepal.Width + Sepal.Length,data=iris, lambda= 2.6)
  lm.r<- MASS::lm.ridge(Petal.Length ~ Sepal.Width + Sepal.Length,data=iris, lambda= 2.6)
  expect_equal(round(a$coef()[-1],1), round(lm.r$coef,1))
})

#test calss
test_that("class is correct", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  expect_true(class(ridgereg_mod)[1] == "ridgereg")
})

#check the print output
test_that("print() output works", {
  
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris, lambda=2.6)
  expect_output(ridgereg_mod$print(),"ridgereg\\(formula = Petal\\.Length ~ Sepal\\.Width \\+ Sepal\\.Length, data = iris, lambda = 2\\.6\\)")
})

#predict test case values
test_that("predict() works correctly", {
  ridgereg_mod <- ridgereg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  expect_equal(round(unname(ridgereg_mod$predict()[c(1,5,7)]),2), c(1.85, 1.53, 1.09))
})

