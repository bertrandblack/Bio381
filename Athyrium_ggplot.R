#-------------------------------
# Script for comparing A. filix-femina varieties mean annual temperature to mean annual percip
# 19 May 2021
# BKB
#-------------------------------

#Preliminaries------------------
library(ggplot2)
library(ggridges)
library(patchwork)
library(ggforce)
library(concaveman)

# Read in the data 
athyrium_data <- read.csv("AthyriumNAmerica_MAT_MAP.csv") 


# clean the data of NAs 
# clean_athyrium_data <-athyrium_data[complete.cases(athyrium_data),]
clean_athyrium_data <-na.omit(athyrium_data)

# Base ggplot 
plot_1 <- ggplot(data=clean_athyrium_data, mapping=(aes(x=Annual_Mean_Temp_1,y=AnnualPrecipitation_1,color=Accepted_binomial))) + 
  geom_point(size=1) + xlab("Mean Annual Temperature (C)") + ylab("Annual Precipitation (mm)") + scale_colour_discrete("Taxa")
plot_1 

# LM ggplot 
plot_2 <- ggplot(data=clean_athyrium_data, mapping=(aes(x=Annual_Mean_Temp_1,y=AnnualPrecipitation_1,color=Accepted_binomial))) + 
  geom_point(size=1) + geom_smooth(method=lm) + xlab("Mean Annual Temperature (C)") + ylab("Annual Precipitation (mm)")
plot_2

# Plot with hull traced around taxon groups
plot_7 <- ggplot(clean_athyrium_data, aes(x=Annual_Mean_Temp_1,y=AnnualPrecipitation_1,color=Accepted_binomial)) + 
  geom_mark_hull(concavity= 5, expand = 0, radius=0,  aes(fill=Accepted_binomial)) + xlab("Mean Annual Temperature (C)") + ylab("Annual Precipitation (mm)")

plot_7

# Plot with solid fill and in order of largest to smallest area 

plot_8 <- ggplot(clean_athyrium_data, aes(x=Annual_Mean_Temp_1,y=AnnualPrecipitation_1,color=Accepted_binomial)) + 
  geom_mark_hull(concavity= 5, expand = 0, radius=0, alpha=0.8, aes(fill=Accepted_binomial)) + xlab("Mean Annual Temperature (C)") + ylab("Annual Precipitation (mm)") 

plot_8

# Facet ggplot by Accpeted_binomial
plot_3 <- ggplot(data=clean_athyrium_data, mapping=(aes(x=Annual_Mean_Temp_1,y=AnnualPrecipitation_1,color=Accepted_binomial))) + 
  geom_point(size=1) + geom_smooth(method=lm) + xlab("Mean Annual Temperature (C)") + ylab("Annual Precipitation (mm)") 

plot_3 + facet_grid(.~Accepted_binomial)

# Facet ggplot by Accpeted_binomial with only regression line shown
plot_4 <- ggplot(data=clean_athyrium_data, mapping=(aes(x=Annual_Mean_Temp_1,y=AnnualPrecipitation_1,color=Accepted_binomial))) + xlab("Mean Annual Temperature (C)") + ylab("Annual Precipitation (mm)")

plot_4 + facet_grid(.~Accepted_binomial) +  geom_point(size=1, colour="grey") + geom_smooth(se=FALSE, method = "lm") 

# Density Ridges ggplot for mean annual temp by taxa 
plot_5 <- ggplot(clean_athyrium_data, aes(x=Annual_Mean_Temp_1, y=Accepted_binomial, fill=Accepted_binomial)) + geom_density_ridges() + theme(axis.title.y = element_blank()) + xlab("Mean Annual Temperature (C)")

plot_5

# Density Ridges ggplot for mean annual percip by taxa
plot_6 <- ggplot(clean_athyrium_data, aes(x=AnnualPrecipitation_1, y=Accepted_binomial, fill=Accepted_binomial)) + geom_density_ridges() + theme(axis.title.y = element_blank()) + xlab("Annual Precipitation (mm)")

plot_6

# Plot both next to each other 

plot_5 + theme(legend.position = "none") + plot_6 


