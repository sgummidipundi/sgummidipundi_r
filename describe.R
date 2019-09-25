#########################################################################################################################################
## Script Name: describe.R
## Script purpose: Useful R functions used to describe data
## Date: 9/25/19
## Author: Santosh
#########################################################################################################################################


#Desctable mean (sd)
mean_sd <- function(x){
  paste0(round(mean(x, na.rm = TRUE), digits = 2)," (",round(sd(x, na.rm = TRUE), digits = 1),")")
}

#Desctable median [IQR]
median_iqr <- function(x){
  paste0(round(median(x), digits = 1)," [",quantile(x, probs = 0.25),", ",quantile(x, probs = 0.75),"]")
}
