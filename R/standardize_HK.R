#' 标准化H和K指标
#' 
#' 对胸径分布(H)和树木密度(K)进行标准化
#' 
#' @param x 指标的观测值
#' @param type 指标类型("H"或"K")
#' @return 标准化后的评分值(0.1-1.0)
#' @export
standardize_HK <- function(x, type = c("H", "K")) {
  type <- match.arg(type)
  
  # 处理无效值
  if (is.na(x)) return(0.1)
  
  if (type == "H") {
    # DBH分布(H)参数
    L <- 0    # 下限
    U <- 2.7  # 上限
    O1 <- 1.2 # 最优范围下限
    O2 <- 1.7 # 最优范围上限
  } else {
    # 树密度(K)参数
    L <- 0    # 下限
    U <- 1.0  # 上限
    O1 <- 0.6 # 最优范围下限
    O2 <- 0.8 # 最优范围上限
  }
  
  # 严格按照分段函数实现
  if (x <= L || x >= U) {
    return(0.1)
  } else if (x > L && x < O1) {
    return(0.1 + 0.9 * (x - L) / (O1 - L))
  } else if (x >= O1 && x <= O2) {
    return(1.0)
  } else if (x > O2 && x < U) {
    return(0.1 + 0.9 * (U - x) / (U - O2))
  }
}
