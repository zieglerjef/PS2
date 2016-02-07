#################################
### 1) Calculating violations ###
#################################

# Create function that:
# - takes in vector or matrix
# - controls test statistic ouput
# - output also contains full digit distribution
# Extra:
# - accomodates any input length
# - throws error if input non-integer

violations <- function(x, m=TRUE, d=TRUE){
  # check input type
  if(!(is.integer(as.vector(x)) | is.numeric(as.vector(x)))){
    stop("Input must be integers or numeric!")}
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
  if(m==FALSE & d==FALSE){
    cat("\n Choose a test statistic! \n")
  }
  if(m==TRUE & d==FALSE){
    cat("\n Table of Proportions: \n")
    print(freq)
    cat("\n Leemis' m Statistic: \n")
    print(mStat)
  }
  if(m==FALSE & d==TRUE){
    cat("\n Table of Proportions: \n")
    print(freq)
    cat("\n Cho-Gain's d: \n")
    print(dStat)
  }
  if(m==TRUE & d==TRUE){
    cat("\n Table of Proportions: \n")
    print(freq)
    cat("\n Leemis' m Statistic: \n")
    print(mStat)
    cat("\n Cho-Gain's d: \n")
    print(dStat)
  }
}

### test different imputs ###

# imput: character vector
x1 <- as.character(sample(1:99, size=10, replace=T))
violations(x1, m=T, d=F)


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
