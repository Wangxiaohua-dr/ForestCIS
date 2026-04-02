# 测试计算函数

context("指标计算函数测试")

test_that("胸径分布计算正确", {
  dbh <- c(10, 15, 20, 25, 30)
  result <- calculate_dbh_distribution(dbh, h_class = 5)
  expect_type(result, "double")
  expect_true(result > 0)
})

test_that("树高基尼系数计算正确", {
  height <- c(10, 12, 15, 18, 20)
  result <- calculate_tree_height_gini(height)
  expect_type(result, "double")
  expect_true(result >= 0 && result <= 1)
})

test_that("林木密度计算正确", {
  result <- calculate_tree_density(n = 100, area = 0.1, mean_cw = 4)
  expect_type(result, "double")
  expect_true(result > 0)
})

test_that("优势树木比例计算正确", {
  height <- c(25, 20, 18, 15, 12, 10, 8, 6, 5)
  result <- calculate_dominant_tree_proportion(height)
  expect_type(result, "double")
  expect_true(result >= 0 && result <= 1)
})

test_that("潜在密度计算正确", {
  dbh <- c(10, 15, 20, 25, 30)
  result <- calculate_potential_density(dbh)
  expect_type(result, "double")
  expect_true(result >= 0 && result <= 1)
})

test_that("健康树木比例计算正确", {
  health_status <- c(1, 1, 0, 1, 0)
  result <- calculate_healthy_tree_proportion(health_status)
  expect_type(result, "double")
  expect_true(result >= 0 && result <= 1)
})

test_that("物种多样性指数计算正确", {
  species <- c("A", "A", "B", "B", "C")
  result <- calculate_simpson_index(species)
  expect_type(result, "double")
  expect_true(result >= 0 && result <= 1)
})

test_that("Pielou指数计算正确", {
  species <- c("A", "A", "B", "B", "C")
  result <- calculate_pielou_index(species)
  expect_type(result, "double")
  expect_true(result >= 0 && result <= 1)
})

test_that("标准化函数工作正常", {
  expect_true(standardize_HK(1.5, "H") >= 0)
  expect_true(standardize_HK(50, "K") >= 0)
})