#' 计算胸径分布指数(H) - Meyer负指数分布q值
#' 
#' 使用Meyer负指数分布模型计算胸径分布的q值，反映林分胸径结构的合理性。
#' q值越接近1，表示胸径分布越接近理想的反J型分布。
#' 
#' @param dbh 胸径向量（厘米）。数值向量，包含样地内所有树木的胸径测量值。
#' @param h_class 径阶宽度（厘米，默认2）。用于划分胸径径阶的宽度。
#' 
#' @return q值（数值）。取值范围通常为0.8-1.2，理想值为1.0。
#' 
#' @details
#' 该函数使用Meyer负指数分布模型拟合胸径分布：
#' \deqn{N = k \cdot e^{-a \cdot d}}{N = k * exp(-a * d)}
#' 其中：
#' \itemize{
#'   \item N: 各径阶株数
#'   \item d: 径阶中值
#'   \item k, a: 模型参数
#' }
#' q值计算公式：
#' \deqn{q = e^{a \cdot h}}{q = exp(a * h)}
#' 其中h为径阶宽度。
#' 
#' @examples
#' # 示例数据
#' dbh_data <- c(12.5, 15.3, 18.7, 22.1, 25.6, 28.9, 32.4, 35.8, 39.2, 42.5)
#' 
#' # 计算q值（默认径阶宽度2cm）
#' q_value <- calculate_dbh_distribution(dbh_data)
#' print(q_value)
#' 
#' # 使用不同径阶宽度
#' q_value_5cm <- calculate_dbh_distribution(dbh_data, h_class = 5)
#' print(q_value_5cm)
#' 
#' @references
#' Meyer, H. A. (1952). Structure, growth, and drain in balanced uneven-aged forests.
#' Journal of Forestry, 50(2), 85-92.
#' 
#' @seealso \code{\link{standardize_HK}} 用于标准化H指标
#' @export
calculate_dbh_distribution <- function(dbh, h_class = 2) {
  # 创建径阶
  breaks <- seq(0, max(dbh) + h_class, by = h_class)
  dbh_class <- cut(dbh, breaks = breaks, right = FALSE)
  
  # 计算各径阶株数
  freq <- table(dbh_class)
  class_counts <- as.numeric(freq)
  
  # 计算径阶中值
  class_mids <- breaks[-length(breaks)] + h_class/2
  
  # 移除0值避免对数计算错误
  valid <- class_counts > 0
  if (sum(valid) < 2) return(NA)
  
  # 准备负指数分布模型数据
  model_data <- data.frame(
    d = class_mids[valid],
    N = class_counts[valid]
  )
  
  # 拟合负指数分布 N = k * exp(-a*d)
  model <- lm(log(N) ~ d, data = model_data)
  
  # 提取参数并计算q值: q = exp(a*h)
  a <- -coef(model)[2]
  q <- exp(a * h_class)
  
  return(q)
}

#' 计算树高分布指数(V) - 基尼系数
#' 
#' 计算树高分布的基尼系数，反映树高分布的不均匀性。
#' 基尼系数越小，表示树高分布越均匀；越大表示树高差异越大。
#' 
#' @param heights 树高向量（米）。数值向量，包含样地内所有树木的树高测量值。
#' 
#' @return 基尼系数（数值，0-1）。取值范围0-1，0表示完全均匀，1表示完全不均匀。
#' 
#' @details
#' 基尼系数计算公式：
#' \deqn{G = 1 - \frac{\sum_{i=1}^{n}(2Q_i - w_i)}{n}}{G = 1 - sum(2*Q_i - w_i)/n}
#' 其中：
#' \itemize{
#'   \item n: 树木数量
#'   \item h_i: 第i棵树的树高（按升序排列）
#'   \item w_i = h_i / sum(h)
#'   \item Q_i = cumsum(w_i)
#' }
#' 
#' @examples
#' # 示例数据
#' height_data <- c(8.5, 10.2, 12.7, 15.3, 18.1, 20.5, 22.8, 25.2, 27.6, 30.1)
#' 
#' # 计算树高基尼系数
#' gini_value <- calculate_tree_height_gini(height_data)
#' print(gini_value)
#' 
#' # 均匀分布的树高（基尼系数接近0）
#' uniform_heights <- rep(15, 10)
#' gini_uniform <- calculate_tree_height_gini(uniform_heights)
#' print(gini_uniform)
#' 
#' @references
#' Gini, C. (1912). Variabilità e mutabilità. Reprinted in Pizetti, E. & Salvemini, T. (Eds.) (1955).
#' Memorie di metodologica statistica. Rome: Libreria Eredi Virgilio Veschi.
#' 
#' @export
calculate_tree_height_gini <- function(heights) {
  n <- length(heights)
  if (n == 0) return(NA)
  
  # 按树高排序
  h_sorted <- sort(heights)
  total_height <- sum(h_sorted)
  
  # 计算Wi和Qi
  Wi <- h_sorted / total_height
  Qi <- cumsum(Wi)
  
  # 基尼系数公式: H = 1 - ∑(2Qi - wi)/n
  sum_term <- sum(2 * Qi - Wi)
  gini <- 1 - (sum_term / n)
  
  return(gini)
}

#' 计算林木密度指标(K) - 林木拥挤度
#' 
#' 计算林木拥挤度指标，反映林分密度对树木生长的空间限制。
#' 该指标考虑了单位面积株数和平均冠幅，值越小表示空间竞争越小。
#' 
#' @param n 样地内树木数量（整数）。样地内调查到的总株数。
#' @param area 样地面积（公顷）。数值，样地的实际面积。
#' @param mean_cw 平均冠幅（米）。数值，样地内所有树木冠幅的平均值。
#' 
#' @return 林木密度指数K（数值）。原始值，通常需要标准化处理。
#' 
#' @details
#' 林木拥挤度计算公式：
#' \deqn{K = \frac{100}{\sqrt{N}} \times \bar{cw}}{K = 100 / sqrt(N) * mean_cw}
#' 其中：
#' \itemize{
#'   \item N = n / area：每公顷株数
#'   \item \bar{cw}：平均冠幅
#' }
#' 
#' @examples
#' # 示例数据
#' n_trees <- 150      # 样地内150棵树
#' plot_area <- 0.1    # 样地面积0.1公顷
#' mean_crown_width <- 4.2  # 平均冠幅4.2米
#' 
#' # 计算林木密度指标
#' K_value <- calculate_tree_density(n_trees, plot_area, mean_crown_width)
#' print(K_value)
#' 
#' # 不同密度情况
#' K_low <- calculate_tree_density(100, 0.1, 4.2)   # 低密度
#' K_high <- calculate_tree_density(200, 0.1, 4.2)  # 高密度
#' print(c(K_low, K_high))
#' 
#' @seealso \code{\link{standardize_HK}} 用于标准化K指标
#' @export
calculate_tree_density <- function(n, area, mean_cw) {
  N <- n / area  # 每公顷株数
  return(100 / sqrt(N) * mean_cw)
}