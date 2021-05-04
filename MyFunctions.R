##################################################
# FUNCTION: get_data
# read in .csv files
# input: .csv file
# output: data frame
#------------------------------------------------- 
get_data <- function(file_name=NULL) {
  if(is.null(file_name)) {
    data_frame <-data.frame(ID=101:110,
                            varA=runif(10),
                            varA=runif(10))
  } else {
    data_frame <- read.table(file=file_name,
                             header = TRUE,
                             sep =",",
                             comment.char = "#")
  }
  
  return(data_frame)
}


##################################################
# FUNCTION: calculate_stuff
# fit an ordinary least squares regression
# input: x and y vector of same length
# output: entire summary of regression model
#------------------------------------------------- 
calculate_stuff <- function(x_var=runif(12),
                            y_var=runif(12)) {
  
  data.frame <- data.frame(x_var,y_var)
  reg_model <- lm(y_var~x_var)
  return(summary(reg_model))
}

##################################################
# FUNCTION: summarize_output
# pull elements from model summary list
# input: list frrom a summary call of a lm
# output: vector of regression residuals
#------------------------------------------------- 
summarize_output <- function(z=NULL) {
  if(is.null(z)) {
    z <- summary(lm(runif(12)~runif(12)))
  }
  
  return(z$residuals)
}

##################################################
# FUNCTION: graph_results
# one line description
# input: x
# output: x 
#------------------------------------------------- 
graph_results <- function(x_var=runif(12),
                          y_var=runif(12)) {
  data.frame <- data.frame(x_var,y_var)
# p1 <-ggplot2::qplot(data=data.frame,
#                     x=x_var,
#                     y=y_var,
#                     geom=c("smooth","point"))

p1 <-ggplot2::ggplot(data=data.frame) +
                    aes(x=x_var, y=y_var) + 
                    geom_point() +
                    stat_smooth(method="lm")

suppressMessages(print(p1))
#message("Regression graph created")
}

##################################################
# FUNCTION: fern_anova
# runs an ANOVA on the data
# input: data frame with a categorical predictor variable and a continues response variable. 
# output: summary stats: means of each group, F-statistic, and p-value
#------------------------------------------------- 
fern_anova <- function(x_var=runif(12),
                            y_var=runif(12)) {
  
  ferndata=as.data.frame(x_var,y_var)
  
  fern_model <- aov(y_var~x_var,ferndata)
  fernsum <-summary(fern_model)
  
  unlist(fernsum)
  fernsum_Anova <- list(Fval=unlist(fernsum)[7],probF=unlist(fernsum)[9],zone_mean=aggregate(y_var~x_var, date=ferndata,FUN=mean))


return(fernsum_Anova)
}

##################################################
# FUNCTION: plot_fernanova
# plots the data in a boxplot for each elevational zone
# input: data frame with a categorical predictor variable and a continues response variable. 
# output: ggplot boxplot figure
#------------------------------------------------- 
plot_fernanova <- function(x_var=runif(12),
                                   y_var=runif(12)) {
  
  ferndata2=as.data.frame(x_var,y_var)
  
  anova.plot <- ggplot(data=ferndata2, aes(x=x_var, y=y_var, fill=x_Var)) + geom_boxplot() + labs(title="Mean Atlanitc forest tree fern biomass by elevational zone",y="Biomass (Mg ha^-1)")

  return(anova.plot)
}