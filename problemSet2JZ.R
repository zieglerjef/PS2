##############################
### Calculating violations ###
##############################

# Create function that:
# - takes in vector or matrix
# - controls test statistic ouput
# Extra:
# - accomodates any input length
# - throws error if input non-integer or numeric

violations <- function(x, m=TRUE, d=TRUE){
  # check input type
  if(!(is.integer(as.vector(x)) | is.numeric(as.vector(x)))){
    stop("Input must be integer or numeric!")}
  # take in matrix or vector of only significant digits
  newVec <- substr(as.vector(x),1,1)
  # generate vector of zeroes for #s 1-9
  freq <- rep(0, 9)
  # fill vector with proportions from imput vector
  freq[as.numeric(names(newVecProp))] <- newVecProp <- prop.table(table(newVec))
  
  # create general test statistic
  newData <- freq - log10(1+1/1:9)
  # Leemis' m Statistic
  mStat <- max(newData)
  # Cho-Gain's d
  dStat <- sqrt(sum(newData^2))
  
  # create print options
  # throw error for lack of test statistic 
  if(m==FALSE & d==FALSE){
    cat("\n Choose a test statistic! \n")
  }
  # only m stat
  if(m==TRUE & d==FALSE){
    cat("\n Table of Proportions: \n")
    print(freq)
    cat("\n Leemis' m Statistic: \n")
    print(mStat)
    invisible(list("mStat" = mStat))
  }
  # only d stat
  if(m==FALSE & d==TRUE){
    cat("\n Table of Proportions: \n")
    print(freq)
    cat("\n Cho-Gain's d: \n")
    print(dStat)
    invisible(list("dStat" = dStat))
  }
  # both d and m stat
  if(m==TRUE & d==TRUE){
    cat("\n Table of Proportions: \n")
    print(freq)
    cat("\n Leemis' m Statistic: \n")
    print(mStat)
    cat("\n Cho-Gain's d: \n")
    print(dStat)
    invisible(list("mStat" = mStat, "dStat" = dStat))
  }
}

### test different imputs ###

# imput: character vector
x1 <- as.character(sample(1:99, size=10, replace=T))
violations(x1, m=T, d=F)
# output: error

# imput: vector, length 10
x2 <- sample(1:99, size=10, replace=T)
# m=T, d=F
violations(x2, m=T, d=F)

# imput: vector, length = 50
x3 <- sample(1:99, size=10, replace=T)
# m=F, d=T
violations(x3, m=F, d=T)

# imput: matrix, length = 20
x4 <- matrix(sample(1:99, size=20, replace=T),
             ncol=2)
# m=T, d=T
violations(x4, m=T, d=T)
# m=F, d=F
violations(x4, m=F, d=F)



#######################
### Critical values ###
#######################

# create function
print.benfords <- function(x){
  # run violations function on input
  testStat <- violations(x)
  
  # create significance asterisk's
  # create empty vectors
  alphaM <- NULL
  alphaD<- NULL
  
  # fill in asterisks if critical value is large enough
  # mStat
  if(testStat$mStat <= 0.851){alphaM <- "Not significant!"}
  if(testStat$mStat > 0.851){alphaM <- "*"}
  if(testStat$mStat > 0.967){alphaM <- "**"}
  if(testStat$mStat > 1.212){alphaM <- "***"}
  
  # dStat
  if(testStat$dStat <= 1.212){alphaD <- "Not significant!"}
  if(testStat$dStat > 1.212){alphaD <- "*"}
  if(testStat$dStat > 1.330){alphaD <- "**"}
  if(testStat$dStat > 1.569){alphaD <- "***"}
  
  # print:
  # - critical value
  # - test statistic name
  # - how stat was calculated
  # - significance
  # table label
  cat("\n Critical Values \n")
  # table data
  print(data.frame("Statistic"=c("Leemis' m", "Cho-Gains' d"),
                   "Value"=c(testStat$mStat, testStat$dStat),
                   "Alpha"=c(alphaM, alphaD),
                   "Calculation"=c("max(freq - log10(1+1/1:9))", "sqrt(sum((freq - log10(1+1/1:9)^2)))")))
  # print legend explanation
  cat("\n Note: p < 0.1^{*}; p < 0.05^{**}; p < 0.01^{***} \n")
}

### test different imputs ###

# imput: character vector
print.benfords(x1)
# output: error

# imput: vector, length 10
print.benfords(x2)

# imput: vector, length = 50
print.benfords(x3)

# imput: matrix, length = 20
print.benfords(x4)



##############################
### Export critical values ###
##############################

# export data from print.benfords function
print.benfords2 <- function(x){
  # export function output to csv file
  sink("exportTable.csv", append=TRUE)
  # re-run print.benfords() function
  print.benfords(x)
  
  sink()
}

# set working directory
setwd("~/Google Drive/WashU/Spring2016/appliedStats")
# execute function
print.benfords2(x2)
