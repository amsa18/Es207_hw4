# question 3.1
library(tidyverse) # active hte package 
#create the normalize function  
normalize <- function(x) { 
  z=x
  if(min(x)<max(x)){ 
    z=(x - min(x)) / (max(x) - min(x))
  }
  return(z)
  # create the sample data of chloride concentration, in mg/L
  x <- c(6.0, 0.5, 0.4, 0.7, 0.8, 6.0,
         5.0, 0.6, 1.2, 0.3, 0.2, 0.5,
         0.5, 10, 0.2, 0.2, 1.7, 3.0)# Granodiorite
  # Data visualization
  hist(log(normalize(x)))
  # nonparametric 95% interval estimates with quantile function and your method 
  bstrap <- c()
  # Do the iteration for making the vector of means samples. 
  for (i in 1:1000){
    bstrap <- c(bstrap, mean(sample(x, 8, replace = T)))
  }
  alfa <- 1-.95  
  lo_np <- quantile(bstrap,alfa/2) #lower band 
  up_np <- quantile(bstrap, 1-alfa/2) #upper band 
  print(paste0("(", round(lo_np,3), ", ", round(up_np,3), ")"))
  # nonparametric 95% interval estimates with quantile function and your method 
  bstrap <- c()
  # Do the iteration for making the vector of means samples. 
  for (i in 1:1000){
    bstrap <- c(bstrap, mean(sample(x, 8, replace = T)))
  }
  alfa <- 1-.95  
  lo_np <- quantile(bstrap,alfa/2) #lower band 
  up_np <- quantile(bstrap, 1-alfa/2) #upper band 
  print(paste0("(", round(lo_np,3), ", ", round(up_np,3), ")"))
  # parametric 95% interval estimates median
  #Natural log of x , the method is from book
  y <- log(x)
  me <- qt(0.975,length(y)-1)*sd(y)/sqrt(length(y))
  lo_p_ln <- mean(y)-me #lower band 
  up_p_ln <- mean(y)+me 
  lo_p <- exp(lo_p_ln)
  up_p <- exp(up_p_ln)
  
  print(paste0("For median","(", round(lo_p,3), ", ", round(up_p,3), ")"))
  
  
  # Parametric by using function 'predict'for mean
  CI <- predict(lm(x~1),
                interval = "confidence",
                level = 0.95)
  print(paste0("For mean"))
  CI[1,]
  ## Question 4.3
  ## Question 4 from Qian
  
  ```{r, include=FALSE}
  # collect the data
  "apa.dat"<-
    structure(.Data = list(apa = c(93.825, 96.1875, 120.6, 369.9, 299.113, 
                                   359.332, 380.968, 403.958, 214.933, 582.35299999999992, 319.235, 
                                   348.882, 71.5946, 139.626, 241.672, 194.283, 276.32100000000004, 
                                   190.817, 875.457, 705.587, 633.52099999999992, 723.176, 812.118, 
                                   756.529, 618.624, 1559.56, 797.196, 877.32, 421.2, 685.79999999999992,
                                   454.225, 536.405, 489.445, 379.874, 510.65, 399.854, 60.57, 47.97, 
                                   125.46, 108.768, 161.37200000000002, 173.13900000000002, 244.324, 
                                   207.04, 211.85, 844.357, 745.104, 928.722, 172.817, 181.218, 408.338, 
                                   84.826, 86.504, 933.111, 1027.6199999999998, 309.663, 277.385, 
                                   276.471, 292.325, 548.42499999999992, 537.08399999999992, 1183.77, 
                                   1157.47, 96.326, 102.73, 61.191, 173.02000000000002, 169.1, 155.16, 
                                   265.45, 191.16, 200.53, 192.44, 226.21, 313.93, 413.85, 
                                   261.14999999999996, 428.44, 551.79, 1327.6, 657.47, 834.11, 1106.3, 
                                   1240.3, 753.11, 891.18, 1013.9, 821.88, 971.93, 876.44, 141.966, 
                                   69.161, 57.338, 253.338, 217.47, 314.52499999999996, 
                                   179.90100000000002, 133.554, 213.815, 237.373, 252.995, 376.551, 
                                   22.92, 24.18, 29.01, 138.291, 171.066, 192.136, 240.68, 234.956, 
                                   267.392, 612.759, 585.486, 595.403), tp = c(11.3, 11.3, 11.3, 13, 13, 
                                                                               13, 9.1, 9.1, 9.1, 8.1, 8.1, 8.1, 13.7, 13.7, 13.7, 10.6, 10.6, 10.6, 
                                                                               15, 15, 15, 11, 11, 11, 9.1, 9.1, 9.1, 8.1, 8.1, 8.1, 8.1, 8.1, 8.1, 
                                                                               8.6, 8.6, 8.6, 11.1, 11.1, 11.1, 11.3, 11.3, 11.3, 7.2, 7.2, 7.2, 8.1,
                                                                               8.1, 8.1, 100.8, 100.8, 78.45, 87.65, 87.65, 24.7, 24.7, 12.35, 12.35,
                                                                               20.3, 20.3, 20.3, 20.3, 16.5, 16, 150.9, 150.9, 150.9, 100.5, 100.5, 
                                                                               100.5, 28.1, 28.1, 28.1, 28.1, 28.1, 28.1, 13.55, 13.55, 13.55, 18.9, 
                                                                               18.9, 18.9, 13.1, 13.1, 13.1, 8.7, 8.7, 8.7, 20.3, 20.3, 20.3, 95.1, 
                                                                               95.1, 95.1, 15, 15, 15, 10.6, 10.6, 10.6, 8.1, 8.1, 8.1, 92.9, 92.9, 
                                                                               92.9, 13.3, 13.3, 13.3, 9.1, 9.1, 9.1, 7.2, 7.2, 7.2)), row.names = 
                c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13",
                  "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                  "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", 
                  "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", 
                  "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", 
                  "58", "59", "60", "61", "62", "63", "64", "65", "66", "67", "68", 
                  "69", "70", "71", "72", "73", "74", "75", "76", "77", "78", "79", 
                  "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", "90", 
                  "91", "92", "93", "94", "95", "96", "97", "98", "99", "100", "101", 
                  "102", "103", "104", "105", "106", "107", "108", "109", "110", "111", 
                  "112", "113", "114"), class = "data.frame")
  #filter the data p < 30
  TP2 <- apa.dat %>% filter(tp < 30 )
  ggplot(TP2, mapping = aes(sample = 'apa', col = tp))+geom_qq(na.rm = T) + geom_qq_line(na.rm = T)+labs(title = "APA for TP less than 30 (mu.g/L)")+labs(x = "Concentration of Tp(mu.g/L) ", y = "Count")
  #filter the data tp >= 30
  TP1 <- apa.dat %>% filter(tp >= 30 )
  ggplot(TP1, mapping = aes(sample = 'apa', col = tp))+geom_qq(na.rm = T) + geom_qq_line(na.rm = T)+
    labs(title = "APA for TP greater than 30 (mu.g/L)") +
    labs(x = "Concentration of Tp(mu.g/L) ", y = "Count")
  x.data <- TP1[,2]
  y.data <- TP2[,2]
  # create the qqplot
  qqplot(x.data, y.data, xlim=range(c(x.data,y.data)),
         ylim=range(c(x.data,y.data)), xlab="TP greater than 30", ylab="TP less than 30")
  #draw the line
  abline(-40,.5, col=grey(0.5))
  # Finde the existance of  null and alternative  hypotheses
  t.test(x=TP2, y=TP1, alternative="greater", var.equal=T)
  