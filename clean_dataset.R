#########################################################################################################################################
## Script Name: R Tookit.R
## Script purpose: All of the useful R functions I use on an everyday basis in a script
## Date: 9/18/19
## Author: Santosh
#########################################################################################################################################


#Rename Dataset Variables
rename_fx <- function(df, vector, print = FALSE) {
  length <- length(vector)
  for(i in 1:length(vector)) {
    if(print) print(names(vector[i]))
    if(names(vector[i]) %in% colnames(df)) {
      colnames(df)[which(colnames(df) == names(vector[i]))] <- vector[i]
    } else {
      if(print){
        print("VARIABLE NOT FOUND")        
      }
      
    }
  }
  df
}