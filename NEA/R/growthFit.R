
#' Fit growth model
#' 
#' @description A wrapper function 
#' 
#'
#' @param data data.frame. 
#' @param modelFun string. Name of model function to be called. 
#'  ("laaGrowth", "lasso_BIC", "lasso_CV10perc", "lasso_CV20perc", 
#'  "growmod_BIC")
#' @param args list. Contains the arguments to be passed to the 
#'
#' @return Fitted object of model
#' @export
#'
#' @examples
#' 
#' 
growthFit <- function(data = NULL, modelFun = NULL, args = NULL){
  if(any(is.null(modelFun), is.null(args))){
    stop("You must define all arguments: data, modelFun, args")
  }
  gFit <- do.call(modelFun, args = args)
  gFit$modelFun <- modelFun
  gFit$args <- args
  gFit$ages <- unique(data$age)
  return(gFit)
}