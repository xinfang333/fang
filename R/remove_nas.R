#' Remove NAs and count remaining data
#'
#' @param data data frame
#' @param var_names variable names for which to remove NAs
#'
#' @return A data frame with columns for Variable names, NA_Count, and Remaining_Count
#'
remove_nas <- function(data, var_names) {

  # 创建一个空的数据框来存储结果
  na_counts_df <- data.frame(Variable = character(),
                             NA_Count = integer(),
                             Remaining_Count = integer(),
                             stringsAsFactors = FALSE)

  # 初始数据量
  initial_count <- nrow(data)

  # 使用for循环处理每个变量
  for (var in var_names) {
    # 计算缺失值数量
    na_count <- sum(is.na(data[[var]]))
    # 更新数据框
    na_counts_df <- rbind(na_counts_df,
                          data.frame(Variable = var,
                                     NA_Count = na_count,
                                     Remaining_Count = initial_count - na_count,
                                     stringsAsFactors = FALSE))
    # 删除包含缺失值的行
    data <- data[!(is.na(data[[var]])),]
    # 更新剩余数据量
    initial_count <- nrow(data)
  }

  # 打印缺失值统计信息
  print(na_counts_df)

  # 返回结果数据框和清理后的数据框
  return(list(NA_Counts = na_counts_df, CleanedData = data))
}

