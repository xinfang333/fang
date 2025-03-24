#' logistic models
#'
#' @param data data frame/weighted data.
#' @param dependent_rariables a vector composed of one or more dependent variables.
#' @param independent_rariables a vector composed of one or more independent variables.
#' @param covariates2 vector composed of covariates added to model2.
#' @param covariates3 vector composed of covariates added to model3.
#' @param weight TRUE or FALSE, Weighted or not.
#'

logistic_model <- function(data,
                           dependent_variables,
                           independent_variables,
                           covariates2,
                           covariates3,
                           weight = F){
  # 读取包
  library(tidyverse)
  library(survey)

  # 创建空白结果表格
  model_result_all <- NULL

  for(d_var in dependent_variables) {
    for(i_var in independent_variables) {
      for (i in 1:3) {

        # 构建模型公式
        if (i == 1) {
          # 对于model1，公式只包含响应变量和element
          formula <- as.formula(paste0(d_var, ' ~ ', i_var))
        } else {
          if (i == 2){
            # 构建协变量公式 model2
            model_formulas <- paste0('+ ', paste(covariates2, collapse = " + "))
          } else {
            # 构建协变量公式 model3
            model_formulas <- paste0('+ ', paste(covariates3, collapse = " + "))
          }
          # 对于model2和model3，直接使用model_formulas中的公式，并添加element
          formula <- as.formula(paste0(d_var, ' ~ ', i_var, model_formulas))
        }

        # 拟合模型
        if (weight == F){
          # 不加权
          model_fit <- glm(formula, data, family = 'binomial')
        } else {
          # 加权
          model_fit <- svyglm(formula, data, family = 'binomial')
        }

        # 获取模型结果
        model_result  <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE)[2,] |>
          rename(OR = estimate) |>
          mutate(model = paste0('model', i),
                 independent_variable = i_var,
                 dependent_variable = d_var)

        # 汇总结果
        model_result_all <- rbind(model_result_all, model_result)
      }
    }
  }
  return(model_result_all)
}
