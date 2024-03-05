library(testthat)
library(ggplot2)
library(rstanarm)

test_that("Support is a string", {
  data <- simulate_data(10)
  expect_true(all(sapply(data$Support, is.character)))
})

test_that("Age_Group is a string", {
  data <- simulate_data(10)
  expect_true(all(sapply(data$Age_Group, is.character)))
})

test_that("Gender is a string", {
  data <- simulate_data(10)
  expect_true(all(sapply(data$Gender, is.character)))
})

test_that("Income_Group is a string", {
  data <- simulate_data(10)
  expect_true(all(sapply(data$Income_Group, is.character)))
})

test_that("Highest_Education is a string", {
  data <- simulate_data(10)
  expect_true(all(sapply(data$Highest_Education, is.character)))
})

test_that("Support contains only 'Yes' or 'No'", {
  data <- simulate_data(10)
  expect_true(all(data$Support %in% c("Yes", "No")))
})

test_that("Age_Group contains correct categories", {
  data <- simulate_data(10)
  expect_true(all(data$Age_Group %in% c('18-24', '25-34', '35-44', '45-54', '55-64', '65+')))
})

test_that("Gender contains correct categories", {
  data <- simulate_data(10)
  expect_true(all(data$Gender %in% c('Male', 'Female', 'Other')))
})

test_that("Income_Group contains correct categories", {
  data <- simulate_data(10)
  expect_true(all(data$Income_Group %in% c('Low', 'Middle', 'High')))
})

test_that("Dataset size is correct", {
  data <- simulate_data(1000)
  expect_equal(nrow(data), 1000)
})

test_dir("")