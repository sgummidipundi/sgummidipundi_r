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

#Bolding and rounding for CIs, odds ratios, betas, P-Values
#------------------------------P-Values-----------------------------#
cell.spec.p <- function(x, round.val, p.thresh, bold = TRUE) {
  new_vals <- as.numeric(x)
  mod_val <- c()
  for(value in new_vals)
  {
    if(is.na(value))
    {
      mod_val <- c(mod_val, value)
    } 
    if(is.numeric(value) & !is.na(value))
    {
      if(value < p.thresh)
      {
        if(value < 0.0001)
        {
          mod_val <- c(mod_val, paste0("\\textbf{< 0.0001}"))
        }
        else if(value >= 0.0001)
        {
          mod_val <- c(mod_val, paste0("\\textbf{", round(value, digits = round.val),"}"))
        }
      }
      else if(value >= p.thresh)
      {
        mod_val <- c(mod_val, as.character(round(value, digits = round.val)))
      }
    }
  }
  mod_val
}

#------------------------------Odds Ratio-----------------------------------#
cell.spec.or <- function(x, round.val, or.thresh = 1, italic = TRUE) {
  new_vals <- as.numeric(x)
  mod_val <- c()
  for(value in new_vals)
  {
    if(is.na(value))
    {
      mod_val <- c(mod_val, value)
    } 
    if(is.numeric(value) & !is.na(value))
    {
      if(value < or.thresh)
      {
        if(value < 0.0001)
        {
          mod_val <- c(mod_val, paste0("\\em{< 0.0001}"))
        } else {
          mod_val <- c(mod_val, paste0("\\em{", round(value, digits = round.val),"}"))
        }
      }
      else if(value >= or.thresh)
      {
        if(value > 10000)
        {
          mod_val <- c(mod_val, "> 10000")
        } else {
          mod_val <- c(mod_val, as.character(round(value, digits = round.val)))
        }
      }
    }
  }
  mod_val
}


cell.spec.ci.or <- function(x, round.val)
{
  new_vals <- as.numeric(x)
  mod_val <- c()
  for(value in new_vals)
  {
    if(is.na(value))
    {
      mod_val <- c(mod_val, value)
    }
    if(is.numeric(value) & !is.na(value))
    {
      if(value < 0.001)
      {
        mod_val <- c(mod_val, "< 0.001")
      }
      else if(value > 10000)
      {
        mod_val <- c(mod_val, "> 10,000")
      }
      else
      {
        mod_val <- c(mod_val, round(value, digits = round.val))
      }
    }
  }
  mod_val
}



#------------------------------Beta Estimate-----------------------------------#
cell.spec.beta <- function(x, round.val, beta.thresh = 0, italic = TRUE) {
  new_vals <- as.numeric(x)
  mod_val <- c()
  for(value in new_vals)
  {
    if(is.na(value))
    {
      mod_val <- c(mod_val, value)
    } 
    if(is.numeric(value) & !is.na(value))
    {
      if(value < beta.thresh)
      {
        if(value < -10000)
        {
          mod_val <- c(mod_val, paste0("\\em{< -10000}"))
        } else {
          mod_val <- c(mod_val, paste0("\\em{", round(value, digits = round.val),"}"))
        }
      }
      else if(value >= beta.thresh)
      {
        if(value > 10000)
        {
          mod_val <- c(mod_val, "> 10000")
        } else {
          mod_val <- c(mod_val, as.character(round(value, digits = round.val)))
        }
        
      }
    }
  }
  mod_val
}

cell.spec.ci <- function(x, round.val)
{
  new_vals <- as.numeric(x)
  mod_val <- c()
  for(value in new_vals)
  {
    if(is.na(value))
    {
      mod_val <- c(mod_val, value)
    }
    if(is.numeric(value) & !is.na(value))
    {
      if(value < -100000 & value > 100000)
      {
        mod_val <- c(mod_val, formatC(value, format = "e", digits = 2))
      }
      else
      {
        mod_val <- c(mod_val, round(value, digits = round.val))
      }
    }
  }
  mod_val
}