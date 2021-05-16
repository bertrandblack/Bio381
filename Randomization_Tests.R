#-----------------------------------------
# Randomization Tests 
# 25 Mar 2020
# BKB 
#-----------------------------------------
# 

# Statistical p is probability of obtaining the observed results (or something more extreme) if the null hypothesis were true p(data|H0)
# Null hypothesis is hypothesis of "no effect" variation is caused by measured error or other unspecified (and less important sources of variation. 

# Two advantages of randomization tests 
# Relaxes assumption of standard parametric tests (normality, balanced sample sized, common variance)
# Give a more intuitive understanding of statistical probability

# Steps in randomization test--------------------------

# 1. Define the metric X as a single number to represent pattern

# 2. Calculate X(obs) the metric for the empirical (=observed) data that we start with. 

# 3. Randomize or reshuffle the data. Randomize in a way that would uncouple the association between observed data and their assignment treatment groups. Ideally, the randomization only affects the patterns of treatment effects in the data. Other effects of the data (such as sample sizes) are preserved in the randomization. Simulate the null hypothesis. 

# 4. For this single randomization, calculate X(sim). If null hypothesis is true X(obs) similar to X(sim). If null hypothesis is false (Xobs) is very different from X(sim). 

# 5. Repeat steps (3) and (4) many times (typically n=1000). This will let us visualize as a histogram the distribution of X(sim); distribution of X values when the null hypothesis is ture. 

# 6. Estimate the tail probability of the observed metric (or something more extreme) given the null distribution (p(X(obs))|H0). 


# Preliminaries------------------------------
library(ggplot2)
library(TeachingDemos)

set.seed(100)
char2seed("Matte overload")
options(digits=10)
char2seed("Matte overload", set=FALSE)

Sys.time()
as.numeric(Sys.time())

my_seed <- as.numeric(Sys.time())
set.seed(my_seed)

char2seed("espresso withdrawal")

# create treatment groups 

trt_group <- c(rep("Control",4),rep("Treatment",5)) 
print(trt_group)

# create response variable 
z <- c(runif(4) + 1, runif(5) + 10)
print(z)

# combine vectors into data frame 
df <- data.frame(trt=trt_group, res=z)
print(df)

# look at means in the two groups 
obs <- tapply(df$res,df$trt,mean)
print(obs)

# create a simulated data set 

  # set up a new data frame 
  df_sim <- df
  df_sim$res <- sample(df_sim$res)
  print(df_sim)

  # look at the means in the two groups of randomized data 
  obs_sim <-tapply(df_sim$res, df_sim$trt, mean)
  print(obs_sim) # what we expect by chance.... 
  
# build functions for randomization ----------------
  
#-------------------------------------------------
# function: read_data
# description: read in (or generate) data set for analysis
# input: file name (or nothing, for this demo)
# output: 3 column data frame of observed data (ID,x,y)
#------------------------------------------------- 
  read_data <- function(z=NULL) {
    if(is.null(z)){
          x_obs <- 1:20
          y_obs <- x_obs + 10*rnorm(20)
          df <- data.frame(ID=seq_along(x_obs),
                       x_obs,
                       y_obs)} # set up data frame                 
   # df <-read.table(file=z,
  #                header=TRUE,
  #                 stringsAsFactors=FALSE)
 
    return(df)
  }
#---------------------------------------------------------------
#read_data()
  
#-------------------------------------------------
# function: get_metric
# description: calculate metric for randomization test
# input: 2-column data frame for regression
# output: regression slope
#------------------------------------------------- 
get_metric <- function(z=NULL) {
        if(is.null(z)){
                x_obs <- 1:20
                y_obs <-  x_obs + 10*rnorm(20)
                z <- data.frame(ID=seq_along(x_obs),
                                x_obs,
                                y_obs)} # set up data frame
  
        . <- lm(z[,3]~z[,2])
        . <- summary(.)
        . <- .$coefficients[2,1]
    
        slope <- .
        return(slope)
}
#-----------------------------------------------
#get_metric()
  
#-------------------------------------------------
# function: shuffle_data
# description: randomize data for regression analysis
# input: 3-column data frame (ID, xvar, yvar)
# output: 3-column data frame (ID, xvar, yvar)
#------------------------------------------------- 
#shuffle_data <- function(z=NULL) {
              if(is.null(z)){
                x_obs <- 1:20
                y_obs <-  x_obs + 10*rnorm(20)
                z <- data.frame(ID=seq_along(x_obs),
                                x_obs,
                                y_obs)} # set up data frame
    
          z[,3] <- sample(z[,3]) # use sample function with defaults to reshuffle column
    
    return(z)
}
#--------------------------------------------------
#shuffle_data()

#-------------------------------------------------
# function: get_pval
# description: calculate p value from simulation
# input: list of observed metric, and vector of simulated metrics
# output: lower, upper tail probability values
#------------------------------------------------- 
  get_pval <- function(z=NULL) {
          if(is.null(z)){
                z <- list(rnorm(1), rnorm(1000)) }
                p_lower <- mean(z[[2]]<=z[[1]])
                p_upper <- mean(z[[2]]>=z[[1]])
                
    return(c(pL=p_lower,pU=p_upper))
  }
#------------------------------------------------- 
#get_pval()

#------------------------------------------------- 
# function: plot_ran_test
# description: create ggplot of histogram of simulated values
# input: list of observed metric and vector of simulated metrics
# output: saved ggplot graph
#------------------------------------------------- 
plot_ran_test <- function(z=NULL) {
                  if(is.null(z)){
                          z <- list(rnorm(1),rnorm(1000)) }
          df <- data.frame(ID=seq_along(z[[2]]),sim_x=z[[2]])
          p1 <- ggplot(data=df,mapping=aes(x=sim_x))
          p1 + geom_histogram(mapping=aes(fill=I("goldenrod"),
                                          color=I("black"))) +
          geom_vline(aes(xintercept=z[[1]],col="blue"))

}
#------------------------------------------------- 
#plot_ran_test()

# Run the functions-------------------------------
n_sim <- 1000 # number simulated data sets 
x_sim <- rep(NA,n_sim) # set up empty vector for simulated slopes 

df <- read_data() # get (fake) data 
x_obs <-get_metric(df) # get slope of observed data 

for (i in seq_len(n_sim)) { 
  x_sim[i] <- get_metric(shuffle_data(df)) # run simulation 
}

slopes <-list(x_obs, x_sim) # 
get_pval(slopes)
plot_ran_test(slopes)

