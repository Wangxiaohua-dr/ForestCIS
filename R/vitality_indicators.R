#' 计算林分优势度(U) - 优势树木比例
#' 
#' 计算优势冠层中优势树木的比例
#' 
#' @param height 树高向量(m)
#' @return 优势树木比例(0-1)
#' @export
calculate_dominant_tree_proportion <- function(height) {
  # 按树高降序排列
  sorted_idx <- order(height, decreasing = TRUE)
  
  # 确定优势冠层样本量（总样本的前三分之一）
  n_dominant <- ceiling(length(height) / 3)
  
  # 优势冠层比例即为优势树木比例
  return(n_dominant / length(height))
}

#' 计算林分生长(B) - 潜在密度
#' 
#' 使用公式 B = G / Gmax 计算潜在密度
#' 
#' @param dbh 胸径向量(cm)
#' @return 潜在密度(0-1)
#' @export
calculate_potential_density <- function(dbh) {
  # 计算胸高断面积(m²)
  basal_area <- pi * (dbh/200)^2
  G <- sum(basal_area)  # 林分总断面积
  
  # 计算最大潜在断面积
  sorted_ba <- sort(basal_area, decreasing = TRUE)
  n50 <- ceiling(length(dbh) * 0.5)
  g_bar_max <- mean(sorted_ba[1:n50])  # 前50%最大个体的平均断面积
  Gmax <- g_bar_max * length(dbh)     # 潜在最大断面积
  
  return(G / Gmax)
}

#' 计算树木健康状况(Q) - 健康树木比例
#' 
#' 计算健康树木数量与总树木数量的比例
#' 
#' @param health_status 健康状态向量(0/1)
#' @return 健康树木比例(0-1)
#' @export
calculate_healthy_tree_proportion <- function(health_status) {
  return(mean(health_status))
}