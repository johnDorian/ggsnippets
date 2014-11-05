context("Testing the g_legend function")

test_that("A simple working example", {
  plt <- ggplot() + geom_boxplot(aes(factor(gear), mpg, fill=factor(gear)), mtcars)
  expect_that(class(g_legend(plt))[1]=="gtable", equals(TRUE))
}
)
test_that("A non ggplot object should throw an error", {
  plt <- 1
  expect_error(g_legend(plt))
}
)
test_that("A ggplot object without a plot should throw an error", {
  plt <- ggplot() + geom_boxplot(aes(factor(gear), mpg, fill=factor(gear)), mtcars) + 
    theme(legend.position="none")
  expect_error(g_legend(plt))
}
)

