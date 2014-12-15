context("Testing the rbind_ggplot_timeseries function")
library(lubridate)

test_that("A simple working example", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  combined_plot <- rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                              max_temp_plot
  ),
  limits = c(dmy("01011960", "31122014"))
  )
  expect_that(inherits(combined_plot, "grob"), equals(TRUE))
}
)

test_that("Only one limit - should throw an error", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(dmy("01011960"))
  ))
}
)

test_that("Non datetime limit - should throw an error ", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(as.Date("2000-01-01"), as.Date("2012-01-01"))
  ))
}
)

test_that("Non logical hide_x_labels - should throw an error ", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(as.Date("2000-01-01"), as.Date("2012-01-01")),
  hide_x_labels = 1
  ))
}
)

test_that("Non n=1 hide_x_labels - should throw an error ", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(as.Date("2000-01-01"), as.Date("2012-01-01")),
  hide_x_labels = c(TRUE, FALSE)
  ))
}
)

test_that("Non logical shrink_space - should throw an error ", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(as.Date("2000-01-01"), as.Date("2012-01-01")),
  shrink_space = 1
  ))
}
)

test_that("Non n=1 shrink_space - should throw an error ", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(as.Date("2000-01-01"), as.Date("2012-01-01")),
  shrink_space = c(TRUE, FALSE)
  ))
}
)

test_that("Non numeric shrink_factor - should throw an error ", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(as.Date("2000-01-01"), as.Date("2012-01-01")),
  shrink_factor = "a"
  ))
}
)

test_that("Non n=1 shrink_factor - should throw an error ", {
  data(breamardata)
  breamardata$date <- ymd(paste(breamardata$year, breamardata$month, "1"))
  min_temp_plot <- ggplot() + geom_line(aes(date, min_temp), breamardata) + theme_bw(20)
  max_temp_plot <- ggplot() + geom_line(aes(date, max_temp), breamardata) + theme_bw(20)
  expect_error(rbind_ggplot_timeseries(ggplot_list = list(min_temp_plot,
                                                          max_temp_plot
  ),
  limits = c(as.Date("2000-01-01"), as.Date("2012-01-01")),
  shrink_factor = c(1, 1)
  ))
}
)



