#' Multiple merges
#'
#' @param data_list list of data frames to be merged
#' @param by vector indicating the variable(s) to merge by
#' @param all TRUE or FALSE
#' @param all.x TRUE or FALSE
#'
#'
#'
#'
multmerge <- function(data_list, by, all = F, all.x = all){

  n <- 3:length(data_list)

  by_list <- list(by = by, all = all, all.x = all.x)

  d1 <- data_list[[1]]
  d2 <- data_list[[2]]
  dm <- do.call(merge, c(data_list[1:2], by_list))

  for(i in n){
    d3 <- data_list[[i]]

    d4 <- if(i == 3){dm} else {d5}

    d5 <- merge(d4, d3, by = by, all = all, all.x = all.x)
  }
  return(d5)
}
