#' cox models
#'
#' @param data data frame/weighted data.
#' @param mortality Is he/she in a state of death. 0/1.
#' @param follow_up_time follow-up time. numerical variable.
#' @param independent_rariables a vector composed of one or more independent variables. numerical variable.
#' @param covariates2 vector composed of covariates added to model2.
#' @param covariates3 vector composed of covariates added to model3.
#' @param weight TRUE or FALSE, Weighted or not.
#' @param group If the independent variable is a categorical variable, please fill in its number of groups.

cox_model <- function(data,
                      mortality,
                      follow_up_time,
                      independent_variables,
                      covariates2,
                      covariates3,
                      weight = F,
                      group = 1){
  # 读取包
  library(tidyverse)
  library(survey)
  library(survival)
  
  # 创建空白结果表格
  model_result_all <- NULL
  
    for(i_var in independent_variables) {
      for (i in 1:3) {
        
        # 构建模型公式
        if (i == 1) {
          # 对于model1，公式只包含响应变量和element
          formula <- as.formula(paste0('Surv(', follow_up_time, ',', mortality, ') ~ ', i_var))
        } else {
          if (i == 2){
            # 构建协变量公式 model2
            model_formulas <- paste0('+ ', paste(covariates2, collapse = " + "))
          } else {
            # 构建协变量公式 model3
            model_formulas <- paste0('+ ', paste(covariates3, collapse = " + "))
          }
          # 对于model2和model3，直接使用model_formulas中的公式，并添加element
          formula <- as.formula(paste0('Surv(', follow_up_time, ',', mortality, ') ~ ', i_var, model_formulas))
        }
        
        # 拟合模型
        if (weight == F){
          # 不加权
          model_fit <- coxph(formula, data)
        } else {
          # 加权
          model_fit <- svycoxph(formula, data_design)
        }
        
        # 获取模型结果
        if (group == 1){
          model_result  <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE)[1,] |>
            rename(HR = estimate) |>
            mutate(model = paste0('model', i),
                   independent_variable = term)
        } else {
          model_result  <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE)[1:(group-1),] |>
            rename(HR = estimate) |>
            mutate(model = paste0('model', i),
                   independent_variable = i_var)
        }
        
        
        # 汇总结果
        model_result_all <- rbind(model_result_all, model_result)
      }
    }
  return(model_result_all)
}