#' 示例森林调查数据
#' 
#' 模拟的森林调查数据，包含3个样地共45棵树的详细信息。
#' 该数据集可用于演示ForestCIS包的功能。
#' 
#' @format 数据框，包含45行7列：
#' \describe{
#'   \item{plot_id}{样地号，整数，取值1-3}
#'   \item{species}{树种，字符，包括"松树"、"杉木"、"栎树"、"桦木"、"杨树"}
#'   \item{dbh}{胸径，数值（厘米），范围5-40}
#'   \item{height}{树高，数值（米），范围3-25}
#'   \item{crown_width}{冠幅，数值（米），范围1-8}
#'   \item{health_status}{健康状态，整数（0=不健康，1=健康）}
#'   \item{volume}{单株材积，数值（立方米），范围0.01-1.5}
#' }
#' 
#' @details
#' 该数据集模拟了典型的森林调查数据，包含：
#' - 3个样地，每个样地15棵树
#' - 5个常见树种
#' - 合理的胸径、树高、冠幅范围
#' - 健康状态模拟（约70%健康）
#' - 基于胸径和树高计算的单株材积
#' 
#' @examples
#' \dontrun{
#' # 加载数据
#' data(forest_example)
#' 
#' # 查看数据结构
#' str(forest_example)
#' 
#' # 查看前几行
#' head(forest_example)
#' 
#' # 计算林分状态指标
#' results <- forest_stand_assessment(forest_example, plot_area = 0.1)
#' print(results)
#' }
#' 
#' @source 模拟数据
#' @keywords datasets
"forest_example"

#' 创建示例森林数据
#' 
#' 生成模拟的森林调查数据，用于测试和演示ForestCIS包的功能。
#' 
#' @param n_plots 样地数量，默认3个。
#' @param n_trees_per_plot 每个样地的树木数量，默认15棵。
#' @param species_list 树种列表，默认包含5个常见树种。
#' @param seed 随机种子，确保结果可重复。
#' 
#' @return 数据框，包含森林调查数据。
#' 
#' @examples
#' \dontrun{
#' # 创建默认示例数据
#' data <- create_forest_example()
#' 
#' # 创建更多样地的数据
#' data_large <- create_forest_example(n_plots = 5, n_trees_per_plot = 20)
#' 
#' # 使用特定树种
#' custom_species <- c("红松", "云杉", "冷杉", "落叶松")
#' data_custom <- create_forest_example(species_list = custom_species)
#' }
#' 
#' @export
create_forest_example <- function(n_plots = 3, n_trees_per_plot = 15, 
                                 species_list = c("松树", "杉木", "栎树", "桦木", "杨树"),
                                 seed = 123) {
  
  set.seed(seed)
  
  total_trees <- n_plots * n_trees_per_plot
  
  # 生成数据
  data <- data.frame(
    plot_id = rep(1:n_plots, each = n_trees_per_plot),
    tree_id = rep(1:n_trees_per_plot, times = n_plots),
    species = sample(species_list, total_trees, replace = TRUE),
    dbh = runif(total_trees, 5, 40),
    height = runif(total_trees, 3, 25),
    crown_width = runif(total_trees, 1, 8),
    health_status = sample(0:1, total_trees, replace = TRUE, prob = c(0.3, 0.7))
  )
  
  # 基于胸径和树高计算单株材积（简化公式）
  data$volume <- with(data, 0.0001 * dbh^2 * height * runif(total_trees, 0.8, 1.2))
  
  return(data)
}

# 创建并保存示例数据集
forest_example <- create_forest_example()

# 保存为R数据文件
usethis::use_data(forest_example, overwrite = TRUE)