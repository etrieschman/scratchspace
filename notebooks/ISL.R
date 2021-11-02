### Introduction to Statistical Learning exercises
### Installed Packages
# install.packages('ggplot2')

### Loaded packages
require(ISLR)
require(dplyr)
require(MASS)
require(ggplot2)

### variables
HOME <- '/Users/etriesch'
setwd(paste0(HOME,'/dev/scratchspace'))


## Chapter 2
college <- read.csv(paste0(getwd(),'/data/College.csv'))
rownames(college) <- college[,1]
college <- college[,-1]
college$Private <- as.factor(college$Private)
college$Elite <- as.factor(college$Top10perc > 50)
# Problem 2
auto <- Auto
col_classes <- lapply(auto, class)
col_ranges <- lapply(auto[,-dim(auto)[2]], range)
range(auto$weight)
# Relative freq definition
trials <- 1000
n <- NULL
heads <- NULL
for (i in 1:trials) {
  n[i] <- i
  heads[i] <- sample(0:1, 1)
}
pct_heads <- cumsum(heads) / n
coin_toss_df <- data.frame(n, heads, pct_heads)
ggplot(data=coin_toss_df, aes(y=pct_heads, x=n)) + 
         geom_line()

i <- 10
temp_calc <- 1
fc <- for(j in 0:i){
  factorial_calc <- factorial_calc*(365 - j)
  return(factorial_calc)
  }

# quick plot
trials <- 30
n <- NULL
p_samebday_1 <- NULL
p_samebday_2 <- NULL
fc <- 1
for(i in 2:trials){
  for(j in 0:(i-1)){
    fc <- fc*(365 - j)/365
    print(fc)
  }
  n[i] <- i
  p_samebday_1[i] <- (2 * factorial(max(0, i-2)))/factorial(i)
  p_samebday_2[i] <- (1/365)/(1-fc)
}
p_df <- data.frame(n, p_samebday_1, p_samebday_2)
ggplot(data=p_df, aes(x=n)) + 
  geom_line(aes(y=p_samebday_1), color="red") + 
  geom_line(aes(y=p_samebday_2), color="blue")
