# Home wrok 4,  questions #3.1 & #3.4 from Helsel and Hirsh
library(tidyverse) # active hte package 
#create the normalize function  
normalize <- function(x) { 
  z=x
  if(min(x)<max(x)){ 
    z=(x - min(x)) / (max(x) - min(x))
  }
  return(z)
}
# Question 3.1
  # create the sample data of chloride concentration, in mg/L
  x <- c(6.0, 0.5, 0.4, 0.7, 0.8, 6.0,
         5.0, 0.6, 1.2, 0.3, 0.2, 0.5,
         0.5, 10, 0.2, 0.2, 1.7, 3.0)# Granodiorite
  # Data visualization
  hist(log(normalize(x)))
  # nonparametric 95% interval estimates with quantile function and your method 
  
  #Creating a function will allow easier application of the same procedure later
  np_median_int <- function(x){
    set.seed(1) #This will keep your results consistent by keeping the random samples generated the same
    bstrap <- c()
    for (i in 1:1000){
      bstrap <- c(bstrap, mean(sample(x, 8, replace = T)))
    }
    alfa <- 1-.95  
    lo_np <- quantile(bstrap,alfa/2) #lower band 
    up_np <- quantile(bstrap, 1-alfa/2) #upper band 
    print(paste0("(", round(lo_np,3), ", ", round(up_np,3), ")"))
  }
 np_median_int(x)
  
  # parametric 95% interval estimates median
  #Natural log of x , the method is from book
  p_median_int <- function(x){
    y <- log(x)
    me <- qt(0.975,length(y)-1)*sd(y)/sqrt(length(y))
    lo_p_ln <- mean(y)-me #lower band 
    up_p_ln <- mean(y)+me 
    lo_p <- exp(lo_p_ln)
    up_p <- exp(up_p_ln)
    print(paste0("For median","(", round(lo_p,3), ", ", round(up_p,3), ")"))
  }
 p_median_int(x)
  
  # Parametric by using function 'predict'for mean
  CI <- predict(lm(x~1),
                interval = "confidence",
                level = 0.95)
  print(paste0("For mean"))
  CI[1,]
  # Question 3.4
  Con_river <- read_csv("~/Desktop/ES207/HW/es207_hw4/Conecuh_River_apxc2.csv",
                        col_names =TRUE,
                        cols(
                          Year = col_double(),
                          `Flow (cfs)` = col_double()
                        ))
  # Data visualization
  Flow <- na.omit(Con_river$`Flow (cfs)`)
  hist(log(normalize(Flow))) #plot normalized data
  # nonparametric 95% interval estimates median?
  np_median_int(Flow)
  
  #Asymmetric Confidence Interval for the mean
  # again use the natural log 
  y <- log(Flow) #make ln function
  Mean_y <- mean(y)
  SD_y <- sd(y)
  Mean_Flow <- exp(Mean_y+0.5*SD_y^2) # apply the formula from the book 
  Mean_Flow 
  #symmetric Confidence Interval for the mean
  # I do this to just find the inetrval
  # parametric 95% interval estimates for mean
  me <- qt(0.975,length(Flow)-1)*sd(Flow)/sqrt(length(Flow))
  lo_p <- mean(Flow)-me # lower band
  up_p <- mean(Flow)+me #upper band
  #print the result
  print(paste0("(", round(lo_p,3), ", ", round(up_p,3), ")"))
  