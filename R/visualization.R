#' 可视化森林林分评估结果
#' 
#' 提供柱状图可视化方法来展示森林林分评估结果，包含误差线和显著性标记。
#' 
#' @name plot_bar_chart
#' @param results 数据框，包含9项林分状态指标的结果。
#' @param indicators 要绘制的指标向量。如果为NULL，绘制所有指标。
#' @param group_by_plot 是否按样地分组（TRUE）或按指标分组（FALSE）。
#' @param title 图表标题（可选）。
#' @param ylab y轴标签。
#' @param significance_data 显著性标记数据框，包含x、y、label列，用于标识不同数据组间的统计显著性差异。
#' 
#' @return 返回柱状图（ggplot2对象）。
#' 
#' @examples
#' \dontrun{
#' # 按样地分组（默认）
#' plot_bar_chart(results)
#' 
#' # 按指标分组
#' plot_bar_chart(results, group_by_plot = FALSE)
#' 
#' # 只绘制部分指标
#' plot_bar_chart(results, indicators = c("H", "V", "K", "U"))
#' 
#' # 添加显著性标记
#' significance_data <- data.frame(
#'   x = c("H", "V"),
#'   y = c(1.2, 1.1),
#'   label = c("*", "**")
#' )
#' plot_bar_chart(results, significance_data = significance_data)
#' }
#' 
#' @importFrom ggplot2 ggplot aes geom_bar position_dodge scale_fill_brewer
#'                     labs theme_minimal theme element_text geom_errorbar
#'                     geom_text
#' @importFrom dplyr select group_by summarize
#' @importFrom tidyr pivot_longer
#' @export
plot_bar_chart <- function(results, indicators = NULL, group_by_plot = TRUE,
                          title = "林分状态指标比较", ylab = "指标值",
                          significance_data = NULL) {
  
  # 检查是否安装了必要的包
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请安装ggplot2包: install.packages('ggplot2')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请安装dplyr包: install.packages('dplyr')")
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("请安装tidyr包: install.packages('tidyr')")
  }
  
  # 选择要绘制的指标
  all_indicators <- c("H", "V", "K", "U", "B", "Q", "Z", "D", "P")
  if (!is.null(indicators)) {
    indicators <- intersect(indicators, all_indicators)
    if (length(indicators) == 0) {
      stop("没有找到有效的指标名称")
    }
  } else {
    indicators <- all_indicators
  }
  
  # 转换数据为长格式
  plot_data <- results %>%
    dplyr::select(plot_id, all_of(indicators)) %>%
    tidyr::pivot_longer(
      cols = -plot_id,
      names_to = "indicator",
      values_to = "value"
    ) %>%
    dplyr::mutate(
      plot_id = as.factor(plot_id),
      indicator = factor(indicator, levels = indicators)
    )
  
  # 计算误差（标准差）
  error_data <- plot_data %>%
    dplyr::group_by(indicator, plot_id) %>%
    dplyr::summarize(
      mean_value = mean(value),
      sd_value = sd(value),
      .groups = "drop"
    )
  
  # 创建柱状图
  if (group_by_plot) {
    p <- ggplot2::ggplot(error_data, ggplot2::aes(x = indicator, y = mean_value, fill = plot_id)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
        position = ggplot2::position_dodge(width = 0.9),
        width = 0.2,
        color = "black"
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2", name = "样地号")
  } else {
    p <- ggplot2::ggplot(error_data, ggplot2::aes(x = plot_id, y = mean_value, fill = indicator)) +
      ggplot2::geom_bar(stat = "identity", position = ggplot2::position_dodge()) +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value),
        position = ggplot2::position_dodge(width = 0.9),
        width = 0.2,
        color = "black"
      ) +
      ggplot2::scale_fill_brewer(palette = "Set3", name = "指标")
  }
  
  p <- p +
    ggplot2::labs(
      title = title,
      x = if (group_by_plot) "指标" else "样地号",
      y = ylab
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = if (group_by_plot) 0 else 45, hjust = 1),
      legend.position = "right"
    )
  
  # 添加显著性标记（如果提供了显著性数据）
  if (!is.null(significance_data)) {
    p <- p +
      ggplot2::geom_text(
        data = significance_data,
        ggplot2::aes(x = x, y = y, label = label),
        size = 4,
        vjust = -0.5
      )
  }
  
  return(p)
}