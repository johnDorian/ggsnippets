context("Testing the facet_wrap_labeller function")

set.seed(101) # No need for this but it should be here I guess.

test_that("A working example without labels", {
   x_cars <- data.frame(cars, variable = sample(letters[1:5], 50, replace = TRUE))
   plt <- ggplot() + geom_point(aes(speed, dist), x_cars) + facet_wrap(~variable, ncol=1)
  expect_that(class(facet_wrap_labeller(plt))[2]=="ggplot", equals(TRUE))
}
)

test_that("A working example with labels", {
  x_cars <- data.frame(cars, variable = sample(letters[1:5], 50, replace = TRUE))
  plt <- ggplot() + geom_point(aes(speed, dist), x_cars) + facet_wrap(~variable, ncol=1)
  expect_that(class(facet_wrap_labeller(plt, 
                                        labels = c(expression(a^2), 
                                                   expression(b[1]), 
                                                   expression(frac(c,2)),
                                                   "d", 
                                                   expression(infinity)))
                    )[2]=="ggplot", equals(TRUE))
}
)

test_that("A non ggplot object should throw an error", {
  plt <- 1
  expect_error(facet_wrap_labeller(plt))
}
)
test_that("A ggplot object without any facets should throw an error", {
  plt <- ggplot() + geom_point(aes(speed, dist), x_cars)
  expect_error(facet_wrap_labeller(plt))
}
)


