###############################################
################### TESTING ###################
###############################################

dataframe <- data.frame(x = 1:20,
                        y = 20:1)

test_that("data.frame.lag.lead works with default inputs", {
  expect_warning(data.frame_lag_lead(dataframe, covariates = c("x")))
})

