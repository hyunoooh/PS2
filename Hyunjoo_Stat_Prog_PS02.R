# Statistical Programming
# Problem Set 02
# Author: Hyunjoo Oh
# Date: Due Feb. 9
# ==============================


### Benford's law
### 1) Calculating violations


## generate a random sample vector:
prop_vote <- sample(1:1000000, size=100000)

## create a function for calculating violations:
            # m and t statistics are included as defaults
violations <- function(x, m=TRUE, d=TRUE){ 
  # extract first digit of x
  first.digit <- substr(as.character(x), start = 1, stop = 1) 
  # x as integer
  first.digit <- as.integer(first.digit)
  # calculate Xi (the proportion of each first digit in vote total number)
  Xi <- table(first.digit)/length(x) 
  # if m only:
  if(m==TRUE & d==FALSE){
    m=max(Xi - log10(1+1/c(1:9)))
                            # include the full digit distribution
    return(list("Leemis' m"=m, distribtion = Xi))
  }
  # if d only:
  if(d==TRUE & m ==FALSE){
    d=sqrt(sum(m^2))
    return(list("Cho-Gain's d"=d, distribtion = Xi))
  }
  # both m and d:
  if(m==TRUE & d==TRUE){
    m=max(Xi - log10(1+1/c(1:9)))
    d=sqrt(sum(m^2))
    return(list("Leemis' m"= m, "Cho-Gain's d"= d, distribtion = Xi))
  }
}  

## test the function:
# m statistic is calculated:
violations(prop_vote, d = FALSE)
# d statistic is calculated:
violations(prop_vote, m = FALSE)
# both m and d statistics are calculated:
violations(prop_vote)



### 2) Critical values

## create a function:
print.benfords <- function(x, m=TRUE, d=TRUE){ 
  # extract first digit of x
  first.digit <- substr(as.character(x), start = 1, stop = 1) 
  # x as integer
  first.digit <- as.integer(first.digit)
  # calculate Xi (the proportion of each first digit in vote total number)
  Xi <- table(first.digit)/length(x) 
  # a vector that explains the asterisks
  significance <- "Significance level: *** 0.01, ** 0.05, * 0.10"
  
  if(m==TRUE & d==FALSE){
    m = max(Xi - log10(1+1/c(1:9)))
    # adding  asterisks
    m <- if(m>=1.212){
      paste0(m, "***")
    } else if(m>=0.967){
      paste0("**")
    } else if(m>=0.851){
      paste0("*")
    } else m
    # create a list that include results of only m-statistics
    critical.values <- data.frame("Leemis' m" = m)
    return(list(critical.values = critical.values, "Significance Level" = significance))  
  }

  if(d==TRUE & m ==FALSE){
    d=sqrt(sum(m^2))
    # adding  asterisks
    d <- if(d>=1.569){
      paste0("***")
    } else if(d>=1.330){
      paste0("**")
    } else if(d>=1.212){
      paste0("*")
    } else d
    # create a list that include results of only d-statistics
    critical.values <- data.frame("Cho-Gain's d" = d)
    return(list(critical.values = critical.values, "Significance Level" = significance))
  }
  # both m and d:
  if(m==TRUE & d==TRUE){
    m=max(Xi - log10(1+1/c(1:9)))
    d=sqrt(sum(m^2))
    # adding  asterisks
    m <- if(m>=1.212){
        paste0(m, "***")
      } else if(m>=0.967){
        paste0("**")
      } else if(m>=0.851){
        paste0("*")
      } else m
    d <- if(d>=1.569){
        paste0("***")
      } else if(d>=1.330){
        paste0("**")
      } else if(d>=1.212){
        paste0("*")
      } else d
      # create a list that include results of both m and d-statistics
      critical.values <- data.frame("Leemis' m" = m, "Cho-Gain's d" = d)
      return(list(critical.values = critical.values, "Significance Level" = significance))
  }
}

## test the function:
# m statistic only:
print.benfords(prop_vote, d = FALSE)
# d statistic only:
print.benfords(prop_vote, m = FALSE)
# both m and d statistics:
print.benfords(prop_vote)


## create another function that uses print.benfords() 
## to create a csv file containing the table in a directory:

getwd()
setwd("/Users/hyunjoooh/Dropbox/2017_Spring_Washu/Stat_Prog/ProblemSet")
export.benfords <- function(x, name="benfords.output.csv"){
  benfords.output <- data.frame(print.benfords(x))
  print(benfords.table)
  sink(file = "benfords_output.csv")
}
export.benfords(prop_vote)
