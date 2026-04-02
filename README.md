# ForestCIS: Forest Stand Condition indicators System

## 概述

ForestCIS是一个用于森林林分状态综合评估的R包，通过计算3大类9项关键指标来评估森林生态系统的健康状况。

## 安装

```r
# 从GitHub安装开发版本
devtools::install_github("Wangxiaohua-dr/ForestCIS")

# 或者从本地安装
devtools::install("path/to/ForestCIS")
```

## 主要功能

### 结构属性指标 (H, V, K)
- **H**: 胸径分布指数
- **V**: 树高分布基尼系数  
- **K**: 林木密度指标

### 林分活力指标 (U, B, Q)
- **U**: 优势树木比例
- **B**: 潜在密度
- **Q**: 健康树木比例

### 组成多样性指标 (Z, D, P)
- **Z**: 树种组成指数
- **D**: 辛普森多样性指数
- **P**: Pielou均匀度指数

## 快速开始

```r
library(ForestCIS)

# 创建示例数据
data <- data.frame(
  plot_id = rep(1:3, each = 30),
  species = sample(c("松树", "杉木", "栎树"), 90, replace = TRUE),
  dbh = pmax(5, rnorm(90, 20, 8)),
  height = pmax(3, rnorm(90, 15, 4)),
  crown_width = pmax(1, rnorm(90, 4, 1.5)),
  health_status = sample(0:1, 90, replace = TRUE),
  volume = 0.00007854 * (pmax(5, rnorm(90, 20, 8)))^2 * pmax(3, rnorm(90, 15, 4))
)

# 进行林分状态评估
results <- forest_stand_assessment(data, plot_area = 0.1)
print(results)
```

## 文档

详细的使用说明和函数文档请参考包内帮助文档：

```r
?forest_stand_assessment
help(package = "ForestCIS")
```

## 许可证

本项目采用MIT许可证。详见LICENSE文件。