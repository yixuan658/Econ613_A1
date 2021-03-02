getwd()
setwd("C:/Users/86185/Dropbox/Papers/Progress")
datstu = read.csv("datstu.csv")
datsss = read.csv("datsss.csv")
datjss = read.csv("datjss.csv")

summary(datstu)
summary(datsss)
summary(datjss)

library(AER)
library(tidyr)
library(dplyr)
library(tidyverse)

#Exercise 1---------------------------------------------------------------------
#1 Number of students
map(datstu,~sum(is.na(.)))
q1.1 = nrow(datstu)
cat('1.1 The number of students is:', q1.1, '\n')

#2 Number of Schools
number_of_schools <- unique(datsss$schoolcode)
number_of_schools <- data.frame(number_of_schools)
q1.2 = nrow(number_of_schools)
cat('1.2 The number of school is:',q1.2,'\n')

#3 Number of programs
program = select(datstu, choicepgm1, choicepgm2, choicepgm3, choicepgm4, choicepgm5, choicepgm6)
sum_prgm = gather(program, choice_program, program_name, choicepgm1, choicepgm2, choicepgm3, choicepgm4, choicepgm5, choicepgm6)
sum_prgm_unique = unique(sum_prgm)
cat('1.3 The number of program is:', nrow(sum_prgm_unique), '\n')

#4 Number of choices (school,program)
school = datstu %>% 
  select(schoolcode1, schoolcode2, schoolcode3, schoolcode4, schoolcode5, schoolcode6) %>% 
  gather(school, schoolcode, schoolcode1, schoolcode2, schoolcode3, schoolcode4, schoolcode5, schoolcode6)
choices = cbind(school[,2], sum_prgm[,2])
choices_unique = unique(choices)
cat('1.4 The number of choices is:', nrow(choices_unique), '\n')

#5 Missing test score
missing_test_score = sum(is.na(datstu$score) == 'TRUE', na.rm = TRUE)
cat('1.5 The number of missed test score is:', missing_test_score, '\n')

#6 Apply to the same school (different programs)
f1 <- function(x) {length(x[!is.na(x)]) - length(unique(x[!is.na(x)]))}
same_school = apply(datstu[, 5:10], MARGIN = 1, FUN = f1)
length(same_school[same_school != 0])
cat('1.6 The number of students apply to the same school but different programs:', length(same_school[same_school != 0]), '\n')

#7 Apply to less than 6 choices
less_than_6 = sum(is.na(datstu$schoolcode6)=='TRUE', na.rm = TRUE)
cat('1.7 The number of student apply to less than 6 choices is:', less_than_6, '\n')

#Exercise 2---------------------------------------------------------------------
datstu$admitted_by_schoolcode=ifelse(datstu$rankplace==1, datstu$schoolcode1,
                                     ifelse(datstu$rankplace==2, datstu$schoolcode2,
                                            ifelse(datstu$rankplace==3, datstu$schoolcode3,
                                                   ifelse(datstu$rankplace==4, datstu$schoolcode4,
                                                          ifelse(datstu$rankplace==5, datstu$schoolcode5,
                                                                 ifelse(datstu$rankplace==6, datstu$schoolcode6, NA))))))
datstu$admitted=ifelse(datstu$rankplace==1, datstu$choicepgm1,
                       ifelse(datstu$rankplace==2, datstu$choicepgm2,
                              ifelse(datstu$rankplace==3, datstu$choicepgm3,
                                     ifelse(datstu$rankplace==4, datstu$choicepgm4,
                                            ifelse(datstu$rankplace==5, datstu$choicepgm5,
                                                   ifelse(datstu$rankplace==6, datstu$choicepgm6, NA))))))
data_raw=datstu %>% 
  group_by(admitted) %>% 
  summarise(schoolcode=admitted_by_schoolcode, minscore=min(score), average=mean(score), number=n())
dataraw=data_raw %>% 
  rename(school_program = admitted)
dataraw=unique(dataraw)
datsss$X=NULL
datsss=unique(datsss)
data_q2<-merge(x=dataraw,y=datsss,by="schoolcode",all.x=TRUE)
data_q2=na.omit(data_q2)
names(data_q2)[names(data_q2) == "minscore"] <- "Cutoff"
names(data_q2)[names(data_q2) == "average"] <- "Quality"
names(data_q2)[names(data_q2) == "number"] <- "Size"

#Exercise 3---------------------------------------------------------------------
datjss=select(datjss, -X)
datstu_jss=left_join(datstu, datjss, by="jssdistrict")
datstu_jss_sss=left_join(datstu_jss, datsss, by=c("admitted_by_schoolcode"="schoolcode"))
datstu_jss_sss$distance = sqrt((69.172*(datstu_jss_sss$ssslong-datstu_jss_sss$point_x)*cos(datstu_jss_sss$point_y/57.3))^2+(69.172*(datstu_jss_sss$ssslat-datstu_jss_sss$point_y)^2))
datstu_jss_sss$distance

#Exercise 4---------------------------------------------------------------------
number_of_choices <- unique(choices_unique)
choices_unique <- [!(is.na(number_of_choices$schoolcode)),]
nrow(number_of_choices)
new_number_of_choices <- data.frame(number_of_choices)
colnames(new_number_of_choices)[colnames(new_number_of_choices) == "X1"] <- "schoolcode"
colnames(new_number_of_choices)[colnames(new_number_of_choices) == "X2"] <- "choicepgm"

datsss2<-datsss
datsss2<-datsss2[!(is.na(datsss2$ssslong)),]
datsss2<-datsss2[,-1]
datsss2<-as.data.frame(unique(datsss2))

datanew<-merge(x=new_number_of_choices,y=datsss2,by="schoolcode",all.x = TRUE, all.y = FALSE)

datstunew<-datstu
datstunew<-datstunew[!(is.na(datstunew$rankplace)),] 

for (i in 1:nrow(datstunew)){
  if (datstunew$rankplace[i]==1){
    datstunew$schoolcode[i]<-datstunew$schoolcode1[i]
    datstunew$choicepgm[i]<-datstunew$choicepgm1[i]
  }
  if (datstunew$rankplace[i]==2){
    datstunew$schoolcode[i]<-datstunew$schoolcode2[i]
    datstunew$choicepgm[i]<-datstunew$choicepgm2[i]
  }
  if (datstunew$rankplace[i]==3){
    datstunew$schoolcode[i]<-datstunew$schoolcode3[i]
    datstunew$choicepgm[i]<-datstunew$choicepgm3[i]
  }
  if (datstunew$rankplace[i]==4){
    datstunew$schoolcode[i]<-datstunew$schoolcode4[i]
    datstunew$choicepgm[i]<-datstunew$choicepgm4[i]
  }
  if (datstunew$rankplace[i]==5){
    datstunew$schoolcode[i]<-datstunew$schoolcode5[i]
    datstunew$choicepgm[i]<-datstunew$choicepgm5[i]
  }
  if (datstunew$rankplace[i]==6){
    datstunew$schoolcode[i]<-datstunew$schoolcode6[i]
    datstunew$choicepgm[i]<-datstunew$choicepgm6[i]
  }
}
datstunew<-datstunew[!(datstunew$rankplace == 99),]

data_final<-datstunew %>% 
  group_by(schoolcode,choicepgm) %>% 
  summarise(cutoff=min(score),quality = mean(score),size = n()) 
data_final<-merge(x=datanew,y=data_final,by= c("schoolcode", "choicepgm"))

datanew2<-merge(x=datstunew,y=data_final,by= c("schoolcode", "choicepgm"))
datanew2<-merge(x=datanew2,y=datjss,by="jssdistrict",all.x = TRUE, all.y = FALSE)
colnames(datanew2)[colnames(datanew2) == "point_x"] <- "jsslong"
colnames(datanew2)[colnames(datanew2) == "point_y"] <- "jsslat"

datanew2$distance <- 0
for (i in 1:nrow(datanew2)){
  datanew2$distance[i]<-sqrt((69.172 * (datanew2$ssslong[i]-datanew2$jsslong[i])*cos(datanew2$jsslat[i]/57.3))^2 + (69.172 * (datanew2$ssslat[i] - datanew2$jsslat[i]))^2)
}
datanew2<-datanew2[!(is.na(datanew2$score)),]
datanew2<-datanew2[!(is.na(datanew2$distance)),]
datanew2 %>%
  group_by(rankplace) %>%
  summarise(cutoff=min(score),quality = mean(score),distance=mean(distance))

install.packages("devtools")
devtools::install_github("moodymudskipper/cutr")
install.packages("cutr")
datanew2$quantile <- smart_cut(datanew2$score, 4, "g", output = "numeric")
datanew2$quantile <- replace(datanew2$quantile, datanew2$quantile==1, "0%-25%")
datanew2$quantile <- replace(datanew2$quantile, datanew2$quantile==2, "25%-50%")
datanew2$quantile <- replace(datanew2$quantile, datanew2$quantile==3, "50%-75%")
datanew2$quantile <- replace(datanew2$quantile, datanew2$quantile==4, "75%-100%")
datanew2 %>% 
  group_by(quantile) %>% 
  summarise(cutoff=min(score),quality = mean(score),distance=mean(distance))

# Part2
rm(list = ls())
#Exercise 5---------------------------------------------------------------------
set.seed(123)
x1 <- runif(10000, min = 1, max = 3)
x1 <- as.matrix(x1)
x2 <- rgamma(10000, shape = 3, rate = 1/2)
x2 <- as.matrix(x2)
x3 <- rbinom(10000, 1, 0.3)
x3 <- as.matrix(x3)
epsilon <- rnorm(10000, mean=2, sd=1)
epsilon <- as.matrix(epsilon)

y <- 0.5 + 1.2*x1 - 0.9*x2 + 0.1*x3 + epsilon

ydum <- y
for (i in 1:10000){
  ydum[i] <- 0
  if (y[i]>mean(y)){
    ydum[i] <- 1
  }
}

#Exercise 6---------------------------------------------------------------------
#6.1
cor(y,x1)
# Correlation between x1 and y is about 0.20, which very different from 1.2.

#6.2$6.3 Regression of Y on X
cons <- matrix(1,10000,1)
X <- matrix(c(x1,x2,x3),10000,3)
X <- cbind(cons,X)
r1 <- solve(t(X) %*% X) %*% t(X)%*% y

#6.4 SEs
sigma2 <- sum((y-X%*%beta)^2)/(nrow(X)-ncol(X))
var <- sigma2*solve(t(X)%*%X)
se_ols <- sqrt(diag(var))
se_ols

#Exercise 7---------------------------------------------------------------------
dataset <- as.data.frame(X)
set.seed(123)
start <- runif(4, 0.1, 0.5)

loglikelihood <- function(f, y, x, beta) {
  pr <- f(x, beta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  likelihood <- (pr^y)*((1-pr)^(1-y))
  return(sum(log(likelihood)))
}

#probit model
probit <- function(x, beta) {
  xbeta <- x %*% beta
  return(pnorm(xbeta))
}
probit_loglikelihood <- function(beta) {
  return(-loglikelihood(f = probit, y = ydum, x = X, beta))
}
pro <- optim(par = start, fn = probit_loglikelihood, method = "BFGS", hessian = T)
pro$par

#logit model
logit <- function(x, beta) {
  xbeta <- x %*% beta
  return(exp(xbeta)/(1+exp(xbeta)))
}
logit_loglikelihood <- function(x, beta) {
  return(-loglikelihood(f = logit, y = ydum, x = X, beta))
}
logi <- optim(par = start, fn = logit_loglikelihood, method = "BFGS", hessian = T)
logi$par

#linear model
linear <- function(x, beta) {
  return(x %*% b)
}
linear_loglikelihood <- function(x, beta) {
  return(-loglikelihood(f = linear, y = ydum, x = X, beta))
}
lin <- optim(par = start, fn = linear_loglikelihood, method = "BFGS", hessian = T)
lin$par

estimation <- cbind(pro$par, logi$par, lin$par)
colnames(estimation) <- c("Probit", "Logit", "Linear")
rownames(estimation) <- c("intercept", "x1", "x2", "x3")

#Exercise 8---------------------------------------------------------------------

