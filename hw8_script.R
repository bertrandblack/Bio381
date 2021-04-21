# create the data
tf_data=data.frame(
  plot=rep(1:12, times=1),
  elevation= 0,
  biomass=0) 

print(tf_data)



print(tf_data)
  
#data input

tf_data[1:12, "elevation"] = abs(rnorm(n= nrow(tf_data), mean = 462.33 , sd = 425.74)) 

print(tf_data)

tf_data[1:12, "biomass"] = abs(rnorm(n= nrow(tf_data), mean = 0.22 , sd = 0.27))  

print(tf_data)

# data analysis 

# model
tf_regModel <- lm(biomass~elevation,data=tf_data)

# model output
print(tf_regModel) # printed output is sparse
str(tf_regModel) # complicated, but has "coefficients"

head(tf_regModel$residuals) # contains residuals

# 'summary' of model has elements
summary(tf_regModel) # 
summary(tf_regModel)$coefficients
str(summary(regModel))

# examine entire matrix of coefficients:
summary(tf_regModel)$coefficients[] 

# Plot data
library(ggplot2)

tf_regPlot <- ggplot(
  data=tf_data,
  aes(x=biomass,y=elevation)) +
  geom_point() +
  stat_smooth(method=lm,se=0.99) 

print(tf_regPlot)

ggplot(tf_data) + 
  geom_point(aes(x = biomass, y = elevation)) + 
  geom_smooth(aes(x = biomass, y = elevation),
              se = FALSE, 
              col = "black",
              method = lm) +
  theme_bw()+ 
  ggtitle("Affects of elevational change on tree fern biomass") + 
  ylab("Elevation (m)") + 
  xlab("Biomass (Mg ha^-1)")

# Means 50% reduced 
tf_data2=data.frame(
  plot=rep(1:12, times=1),
  elevation= 0,
  biomass=0) 

tf_data2[1:12, "elevation"] = abs(rnorm(n= nrow(tf_data), mean = 231.165 , sd = 425.74)) 

tf_data2[1:12, "biomass"] = abs(rnorm(n= nrow(tf_data), mean = 0.11 , sd = 0.27))  

tf_regModel2 <- lm(biomass~elevation,data=tf_data2)
print(tf_regModel2)
summary(tf_regModel2)


########

tf_data=data.frame(
  plot=rep(1:12, times=1),
  elevation= 0,
  biomass=0) 

print(tf_data)

tf_data[1:12, "elevation"] = sample(0:1066, 12, replace = FALSE) 
 
print(tf_data)


#### 
tf_data5=data.frame(
  plot=rep(1:120, times=1),
  elevation= 0,
  biomass=0) 

tf_data5[1:120, "elevation"] = abs(rnorm(n= nrow(tf_data5), mean = 462.33, sd = 425.74)) 

tf_data5[1:120, "biomass"] = abs(rnorm(n= nrow(tf_data5), mean = 0.22, sd = 0.27))  

ggplot(tf_data5) + 
  geom_point(aes(x = biomass, y = elevation)) + 
  geom_smooth(aes(x = biomass, y = elevation),
              se = FALSE, 
              col = "black",
              method = lm) +
  theme_bw()+ 
  ggtitle("Affects of elevational change on tree fern biomass") + 
  ylab("Elevation (m)") + 
  xlab("Biomass (Mg ha^-1)")
  
                    