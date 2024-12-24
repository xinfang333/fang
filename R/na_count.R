#' Table NA counts of data.frames
#'
#' @param data data frame
#'
#' @return A data frame with columns for Column names, NA_Count, and NA_Ratio
#'
#'
#'
na_count <- function(data){

  cname_data <- colnames(data)

  # 创建一个空的数据框来存储结果
  na_counts_df <- data.frame(Column = character(), NA_Count = integer(), NA_Ratio = character(), stringsAsFactors = FALSE)

  # 使用for循环计算每个变量的缺失值数量
  for (column in cname_data) {
    na_count <- sum(is.na(data[[column]]))
    na_ratio <- sprintf("%.2f%%", na_count / nrow(data) * 100) # 保留两位小数
    na_counts_df <- rbind(na_counts_df, data.frame(Column = column, NA_Count = na_count, NA_Ratio = na_ratio, stringsAsFactors = FALSE))
  }
  return(na_counts_df)
}
