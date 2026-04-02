#' 计算森林林分状态指标
#' 
#' 计算3大类9项林分状态指标，包括结构属性、林分活力和组成多样性指标。
#' 该函数基于森林调查数据评估林分健康状况，为森林管理和生态评估提供科学依据。
#' 
#' @param data 数据框，包含每棵树的以下字段：
#'   - plot_id: 样地号（整数或字符）
#'   - species: 树种（字符）
#'   - dbh: 胸径（厘米，数值）
#'   - height: 树高（米，数值）
#'   - crown_width: 冠幅（米，数值）
#'   - health_status: 健康状态（0=不健康，1=健康，整数）
#'   - volume: 单株材积（立方米，数值）
#' @param plot_area 样地面积（公顷）。可以是单个数值（所有样地使用相同面积），
#'   也可以是命名向量（为每个样地指定不同面积），向量名称应与样地号对应。
#' @param h_class DBH分组步长（厘米，默认2cm）。用于计算胸径分布时划分径阶。
#' 
#' @return 数据框，每行代表一个样地，包含以下9个指标：
#'   - H: 胸径分布指数（标准化后，0-1）
#'   - V: 树高分布基尼系数（0-1）
#'   - K: 林木密度指标（标准化后，0-1）
#'   - U: 优势树木比例（0-1）
#'   - B: 潜在密度（0-1）
#'   - Q: 健康树木比例（0-1）
#'   - Z: 树种组成指数（香农指数，≥0）
#'   - D: 辛普森多样性指数（0-1）
#'   - P: Pielou均匀度指数（0-1）
#' 
#' @details
#' 该函数计算以下3大类9项指标：
#' \enumerate{
#'   \item \strong{结构属性指标}：反映林分空间结构特征
#'     \itemize{
#'       \item H: 胸径分布指数（基于Meyer负指数分布）
#'       \item V: 树高分布基尼系数
#'       \item K: 林木密度指标（基于林木拥挤度）
#'     }
#'   \item \strong{林分活力指标}：反映林分生长和健康状况
#'     \itemize{
#'       \item U: 优势树木比例（优势冠层中优势树木的比例）
#'       \item B: 潜在密度（实际断面积与最大可能断面积之比）
#'       \item Q: 健康树木比例
#'     }
#'   \item \strong{组成多样性指标}：反映物种多样性和均匀度
#'     \itemize{
#'       \item Z: 树种组成指数（基于体积比例的香农多样性指数）
#'       \item D: 辛普森多样性指数（基于个体比例）
#'       \item P: Pielou均匀度指数
#'     }
#' }
#' 
#' @examples
#' # 创建示例数据
#' set.seed(123)
#' example_data <- data.frame(
#'   plot_id = rep(1:3, each = 15),
#'   species = sample(c("松树", "杉木", "栎树", "桦木", "杨树"), 45, replace = TRUE),
#'   dbh = runif(45, 5, 40),
#'   height = runif(45, 3, 25),
#'   crown_width = runif(45, 1, 8),
#'   health_status = sample(0:1, 45, replace = TRUE, prob = c(0.3, 0.7)),
#'   volume = runif(45, 0.01, 1.5)
#' )
#' 
#' # 计算林分状态指标（所有样地面积相同）
#' results <- forest_stand_assessment(example_data, plot_area = 0.1)
#' print(results)
#' 
#' # 为不同样地指定不同面积
#' plot_areas <- c("1" = 0.1, "2" = 0.12, "3" = 0.08)
#' results2 <- forest_stand_assessment(example_data, plot_area = plot_areas)
#' print(results2)
#' 
#' @seealso
#' 有关各个指标的计算方法，请参阅：
#' \code{\link{calculate_dbh_distribution}}, \code{\link{calculate_tree_height_gini}},
#' \code{\link{calculate_tree_density}}, \code{\link{calculate_dominant_tree_proportion}},
#' \code{\link{calculate_potential_density}}, \code{\link{calculate_healthy_tree_proportion}},
#' \code{\link{calculate_species_composition_index}}, \code{\link{calculate_simpson_index}},
#' \code{\link{calculate_pielou_index}}, \code{\link{standardize_HK}}
#' 
#' @export
forest_stand_assessment <- function(data, plot_area, h_class = 2) {

# 验证输入数据
  required_cols <- c("plot_id", "species", "dbh", "height", "crown_width", "health_status")
  if (!all(required_cols %in% colnames(data))) {
    stop("输入数据缺少必要列：", paste(setdiff(required_cols, colnames(data)), collapse = ", "))
  }
  
  # 处理样地面积参数
  if (length(plot_area) == 1) {
    # 单个面积值应用于所有样地
    plot_areas <- setNames(rep(plot_area, length(unique(data$plot_id))), unique(data$plot_id))
  } else {
    # 检查命名向量是否包含所有样地
    if (!all(unique(data$plot_id) %in% names(plot_area))) {
      stop("plot_area参数未提供所有样地的面积信息")
    }
    plot_areas <- plot_area
  }
  
  # 按样地分组计算
  results <- lapply(unique(data$plot_id), function(plot) {
    plot_data <- data[data$plot_id == plot, ]
    
    # 检查样地是否有足够树木
    if (nrow(plot_data) < 5) {
      warning(sprintf("样地 %s 树木数量不足（%d棵），结果可能不可靠", plot, nrow(plot_data)))
    }  
    
     # 结构属性指标
    H <- calculate_dbh_distribution(plot_data$dbh, h_class)
    V <- calculate_tree_height_gini(plot_data$height)
    K <- calculate_tree_density(nrow(plot_data), plot_areas[plot], mean(plot_data$crown_width))
    
    # 标准化H和K指标
    H_std <- standardize_HK(H, type = "H")
    K_std <- standardize_HK(K, type = "K")
    
    # 林分活力指标
    U <- calculate_dominant_tree_proportion(plot_data$height)
    B <- calculate_potential_density(plot_data$dbh)
    Q <- calculate_healthy_tree_proportion(plot_data$health_status)
    
    # 组成多样性指标
    Z <- calculate_species_composition_index(plot_data$species, plot_data$volume)
    D <- calculate_simpson_index(plot_data$species)
    P <- calculate_pielou_index(plot_data$species)
    
  # 返回结果
    data.frame(
      plot_id = plot,
      H = H_std,  # 胸径分布（标准化后）
      V = V,      # 树高分布
      K = K_std,  # 林木密度（标准化后）
      U = U,      # 林分优势度
      B = B,      # 林分生长
      Q = Q,      # 树木健康状况
      Z = Z,      # 树种组成
      D = D,      # 物种多样性
      P = P       # 物种均匀度
    )
  })
  
  # 合并所有样地结果
  do.call(rbind, results)
}