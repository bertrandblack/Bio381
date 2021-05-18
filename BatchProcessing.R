#-------------------------------
# Script for file creation and batch processing
# 26 Mar 2020
# BKB
#-------------------------------
#

#------------------------------------------------
# FUNCTION: file_builder
# description: creates a random files for regression
# input: file_n = number of files to create 
#        file_folder = name of folder for random files
#        file_size = c(min,max) number of rows in file 
#        file_NA = average of number of NA values per column
# output: creats a set of random files 
#------------------------------------------------- 
file_builder <- function(file_n=10,
                         file_folder="RandomFiles/",
                         file_size=c(15,100),
                         file_NA=3) { # start of function file_builder
  
for (i in seq_len(file_n)) { # start of for loop 
  file_length <- sample(file_size[1]:file_size[2],size=1)
  var_x <- runif(file_length) # create random x
  var_y <- runif(file_length) # create random y
  df <- data.frame(var_x,var_y) # bind into a data frame
  bad_vals <- rpois(n=1, lambda=file_NA) # deterime NA number 
  df[sample(nrow(df),size=bad_vals),1] <-NA
  df[sample(nrow(df),size=bad_vals),2] <-NA
  
# create a label for the file name with padded zeros (01,02)
  file_label <- paste(file_folder,"ranFile",
                      formatC(i,
                              width=3,
                              format="d",
                              flag="0"),
                              ".csv",
                            sep="")
# set up data file and incorperate time stamp and minimal metadata 
write.table(cat("# Simulated ramdom data file for batch processing", 
                "\n",
                "# timestamp: ", as.character(Sys.time()),
                "\n",
                "# BKB", "\n",
                "# --------------------------", "\n",
                "\n",
                file=file_label,
                row.names="",
                col.name="",
                sep=""))

# now add the data.frame
write.table(x=df,
            file=file_label,
            sep=",",
            row.names=FALSE,
            append=TRUE)
} # end of for loop
} # end of function file_builder
#------------------------------------------------- 

#-------------------------------------------------
# FUNCTION: reg_stats
# description: fits linear models, extract model stats 
# input: 2-column data frame (x and y)
# output: slope, p-value, and r2
#------------------------------------------------- 
reg_stats <- function(d=NULL) { # start of function reg_stats
              if (is.null(d)) { # start of if statement 
                x_var <-runif(10)
                y_var <-runif(10)
                d <- data.frame(x_var,y_var)
              } # end of if statement 
  . <-lm(data=d,d[,2]~d[,1])
  . <-summary(.)
  stats_list <- list(Slope=.$coefficents[2,1],
                     pVal=.$coefficents[2,4],
                     r2=.$r.squared)
  
  
  return(stats_list)
} # end of function reg_stats
#------------------------------------------------- 

library(TeachingDemos)
char2seed("Flatpicking solo")

######################################################
# Global variables 
file_folder <- "RandomFiles/"
n_files <-100
file_out <- "StatsSummary1.csv"
######################################################

# Create random data sets 
dir.create(file_folder)
file_builder(file_n=n_files)
file_names <- list.files(path=file_folder)

# Create a data frame to hold the summary file statistics 
ID <- seq_along(file_names)
file_name <-file_names
slope <-rep(NA,length(file_names))
p_val <-rep(NA,length(file_names))
r2 <-rep(NA,length(file_names))

stats_out <-data.frame(ID, file_name, slope, p_val, r2)

# Batch process by looping through individual files 

for (i in seq_along(file_names)) { # start of for loop 
  data <- read.table(file=paste(file_folder, file_names[i],
                                sep=""),
                                sep=",",
                                header=TRUE)
  d_clean <-data[complete.cases(data),] # subset for clean cases 
  
  . <-reg_stats(d_clean) # pull out regression stats from clean file 
  stats_out[i,3:5] <- unlist(.) # unlist, copy into last 3 columns 
} # end of for loop

# set up an output file and incorporate a time stamp and minimal metadata

write.table(cat("# Summary stats for",
                "batch processing of regression models",
                "\n",
                "# timestamp: ", as.character(Sys.time()),
                "\n",
                file=file_out,
                row.names="",
                col.names="",
                sep=""))

# now add the data frame
write.table(x=stats_out,
            file=file_out,
            row.names = FALSE,
            col.names = TRUE,
            sep=",",
            append=TRUE)

