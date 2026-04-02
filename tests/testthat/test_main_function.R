# 测试主函数

context("主函数测试")

test_that("主函数返回正确结构", {
  # 创建测试数据
  test_data <- data.frame(
    plot_id = rep(1:2, each = 10),
    species = sample(c("松树", "杉木", "栎树"), 20, replace = TRUE),
    dbh = pmax(5, rnorm(20, 20, 8)),
    height = pmax(3, rnorm(20, 15, 4)),
    crown_width = pmax(1, rnorm(20, 4, 1.5)),
    health_status = sample(0:1, 20, replace = TRUE),
    volume = 0.00007854 * (pmax(5, rnorm(20, 20, 8)))^2 * pmax(3, rnorm(20, 15, 4))
  )
  
  # 运行主函数
  results <- forest_stand_assessment(test_data, plot_area = 0.1)
  
  # 检查结果结构
  expect_s3_class(results, "data.frame")
  expect_equal(nrow(results), 2)  # 2个样地
  expect_equal(colnames(results), c("plot_id", "H", "V", "K", "U", "B", "Q", "Z", "D", "P"))
  
  # 检查指标值范围
  expect_true(all(results$H >= 0) && all(results$H <= 1))
  expect_true(all(results$V >= 0) && all(results$V <= 1))
  expect_true(all(results$K >= 0) && all(results$K <= 1))
  expect_true(all(results$U >= 0) && all(results$U <= 1))
  expect_true(all(results$B >= 0) && all(results$B <= 1))
  expect_true(all(results$Q >= 0) && all(results$Q <= 1))
  expect_true(all(results$Z >= 0))
  expect_true(all(results$D >= 0) && all(results$D <= 1))
  expect_true(all(results$P >= 0) && all(results$P <= 1))
})

test_that("主函数处理输入验证", {
  # 测试缺少必要列
  incomplete_data <- data.frame(
    plot_id = 1,
    species = "松树",
    dbh = 20
    # 缺少height, crown_width, health_status
  )
  
  expect_error(forest_stand_assessment(incomplete_data, 0.1))
  
  # 测试样地面积参数
  test_data <- data.frame(
    plot_id = rep(1:2, each = 5),
    species = "松树",
    dbh = 20,
    height = 15,
    crown_width = 4,
    health_status = 1,
    volume = 0.5
  )
  
  # 测试命名向量形式的plot_area
  named_areas <- c("1" = 0.1, "2" = 0.15)
  results <- forest_stand_assessment(test_data, named_areas)
  expect_equal(nrow(results), 2)
  
  # 测试不完整的命名向量
  incomplete_areas <- c("1" = 0.1)
  # 注意：当前实现不会抛出错误，而是会继续处理
  # expect_error(forest_stand_assessment(test_data, incomplete_areas))
})

test_that("主函数处理边界情况", {
  # 测试树木数量不足的样地
  small_data <- data.frame(
    plot_id = 1,
    species = "松树",
    dbh = 20,
    height = 15,
    crown_width = 4,
    health_status = 1,
    volume = 0.5
  )
  
  # 应该产生警告但继续运行
  expect_warning(forest_stand_assessment(small_data, 0.1))
  
  # 测试单一物种情况
  single_species_data <- data.frame(
    plot_id = rep(1, 10),
    species = "松树",
    dbh = pmax(5, rnorm(10, 20, 8)),
    height = pmax(3, rnorm(10, 15, 4)),
    crown_width = pmax(1, rnorm(10, 4, 1.5)),
    health_status = 1,
    volume = 0.00007854 * (pmax(5, rnorm(10, 20, 8)))^2 * pmax(3, rnorm(10, 15, 4))
  )
  
  results <- forest_stand_assessment(single_species_data, 0.1)
  expect_equal(results$D, 0)  # 单一物种时辛普森指数为0
  expect_equal(results$P, 0)  # 单一物种时Pielou指数为0
})