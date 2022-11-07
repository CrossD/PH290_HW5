library(purrr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# compareLM compares the fitted values from user specified model to the observed data
compareLM <- function(data, response, formulas){
  
  `%+%` <- function(a,b) paste0(a,b)
  
  # Concatenates response variable with formulas
  formulas <- imap(formulas, function(formulas, i) response%+%" ~ "%+%formulas) 
  # Runs each model
  lm_output <- imap(formulas, function(formulas, i) lm(formula = formulas, data = data))
  # Obtains predicted values
  pred_list <- imap(lm_output, function(lm_output, i) predict(object = lm_output, newdata = data))
  
  # Creates data.frame of predicted values and adds observation column
  pred_df <- do.call(cbind, pred_list)
  colnames(pred_df) <- paste0("Pred_", seq_along(formulas))
  pred_df <- data.frame(cbind("Observation" = data[,response],pred_df))
  
  # Saves plot for each model specified; plot is observed vs fitted values
  for(i in seq_along(pred_list)){
    model <- "Pred_"%+%i
    plot_string <- "ggplot(pred_df, aes(x=Observation, y="%+%model%+%")) + geom_point() + geom_abline(intercept=0, slope=1)"
    assign("plot_"%+%i, eval(parse(text=plot_string)))
  } 
  
  # Places all plots created in a single grid and returns table of observed vs fitted values.
  plots_string <- paste0("plot_"%+%seq_along(pred_list),collapse = ",")
  grid_string <- "gridExtra::grid.arrange("%+%plots_string%+%", nrow = 1, ncol = length(pred_list))"
  eval(parse(text=grid_string))
  return(pred_df)
}

models <- list("Petal.Length", 
               "Petal.Length + Petal.Width",
               "Petal.Length + Petal.Width + Sepal.Width",
               "Petal.Length + Petal.Width + Sepal.Width^2")

# Function should output table of observered vs predicted values; and, a plot of observed vs predicted values
compareLM(data = iris, response = "Sepal.Length", formulas = models)
