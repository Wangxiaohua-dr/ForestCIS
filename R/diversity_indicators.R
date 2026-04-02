#' 计算树种组成(Z) - 树种组成指数
#' 
#' 使用公式 Z = -∑Pi*ln(Pi) 计算树种组成指数
#' 基于体积比例计算
#' 
#' @param species 树种向量
#' @param volumes 树木体积向量
#' @return 树种组成指数
#' @export
calculate_species_composition_index <- function(species, volumes) {
  if (length(species) != length(volumes)) {
    stop("物种和体积向量长度必须一致")
  }
  
  # 计算每个树种的总体积
  species_volumes <- tapply(volumes, species, sum)
  total_volume <- sum(species_volumes)
  
  # 计算每个树种的比例
  P_i <- species_volumes / total_volume
  
  # 计算树种组成指数
  Z <- -sum(P_i * log(P_i))
  
  return(Z)
}

#' 计算物种多样性(D) - 辛普森指数
#' 
#' 使用公式 D = 1 - ∑Pi² 计算辛普森指数
#' 基于个体比例计算
#' 
#' @param species 树种向量
#' @return 辛普森指数(0-1)
#' @export
calculate_simpson_index <- function(species) {
  species_counts <- table(species)
  P_i <- species_counts / sum(species_counts)
  D <- 1 - sum(P_i^2)
  return(D)
}

#' 计算物种均匀度(P) - pielou指数
#' 
#' 使用公式 P = -∑Pi*ln(Pi) / ln(S) 计算指数
#' 
#' @param species 树种向量
#' @return pielou指数(0-1)
#' @export
calculate_pielou_index <- function(species) {
  species_counts <- table(species)
  S <- length(species_counts)
  if (S <= 1) return(0)  # 单一物种时均匀度为0
  
  P_i <- species_counts / sum(species_counts)
  H <- -sum(P_i * log(P_i))  # 香农多样性指数
  P <- H / log(S)            # pielou均匀度指数
  
  return(P)
}