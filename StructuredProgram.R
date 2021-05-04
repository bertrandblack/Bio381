# Sample program to illustrate structured program with functions # Date 
# BKB
#--------------------------------------

# All functions declared at the start

##################################################
## Prelimiaries 
library(ggplot2)
set.seed(99)
source("MyFunctions.R")

#--------------------------------
# global variables 
antFile <- "antcountydata.csv"
xCol <- 7 # latitudinal center for each county
yCol <- 5 # number of ant species
#--------------------------------
# Program body 

# contruct data fram
temp_1 <-get_data(file_name = antFile)

x <-temp_1[,xCol] # extract predictor variable
y <-temp_1[,yCol] # extract response variable

temp_2 <- calculate_stuff(x_var=x,y_var=y)

# extract the residuals 
temp_3 <- summarize_output(temp_2)

# graph results 
graph_results(x_var = x, y_var = y)

print(temp_3)  # print residuals 

print(temp_2) # print model summary


