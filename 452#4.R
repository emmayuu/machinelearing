##Exercises 1.22
#######1
install.packages("freqparcoord")
library(freqparcoord)
data_fat=read.csv("bodyfat.csv")
#linear model
xvalpart <- function(data, p) {
  n <- nrow(data_fat)
  ntrain <- round(p*n)
  trainidxs <- sample(1:n, ntrain, replace=FALSE)
  list(train=data[trainidxs ,],
       valid=data[-trainidxs ,])
}


xvallm <- function(data, ycol, predvars, p, meanabs=TRUE){
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  trainy <- train[ , ycol]
  trainpreds <- train[ , predvars]
  trainpreds <- as.matrix(trainpreds)
  lmout <- lm(trainy ~ trainpreds)
  validpreds <- as.matrix(valid[ , predvars])
  predy <- cbind(1, validpreds)%*% coef(lmout)
  realy <- valid[ , ycol]
  if (meanabs) return(mean(abs(predy - realy)))
  list( predy = predy , realy = realy)
}

#predicter: age, height, prediction: weight
data(data_fat)
xvallm(data_fat, 7, c(6,8), 2/3)

#KNN
xvalknn <- function(data,ycol ,predvars ,k,p,meanabs=TRUE) {
  # cull out just Y and the Xs
  data <- data[ ,c(predvars, ycol)] 
  ycol <- length(predvars) + 1
  tmp <- xvalpart(data,p)
  train <- tmp$train
  valid <- tmp$valid
  valid <- as.matrix(valid)
  xd <- preprocessx(train[,-ycol],k)
  kout <- knnest(train[,ycol],xd,k)
  predy <- predict(kout, valid[,-ycol], TRUE) 
  realy <- valid [,ycol]
  if (meanabs) return(mean(abs(predy - realy))) 
  list (predy = predy , realy = realy)
}

install.packages('regtools')
library(regtools)
set.seed (9999)
xvalknn(data_fat,7 ,c(6 ,8) ,7 ,2/3)

#######2
prgeng=read.csv("prgeng.csv")
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13 ,]
pe <- tmp[ ,c(9,2,13,14,15,16)]

pe$agefem <- pe$age * pe$fem
pe$age2fem <- pe$age2 * pe$fem
model=lm(wageinc ~ age+age2+ms+phd+fem+agefem+age2fem, data=pe)
summary(model)

age <- 32
age2 <- 32 * 32
ms <- 1
phd <- 0
fem <- 1
agefem <- 32
age2fem <- 32*32
test <- data.frame(age,age2,ms,phd,fem,agefem,age2fem)
pe_test = predict(model, test)
pe_test

#######3
data_fat=read.csv("bodyfat.csv")
modelfat = lm(density ~ age + weight + height + neck + chest + abdomen + hip + thigh + knee + ankle + biceps + forearm + wrist, data=data_fat)
summary(modelfat)
#according to p-value, several variables in this model is not signifacant, so it is feasible to use an indirect method. 

#######4
#a. The overall height of people equal to a weighted average of the both gender mean heights.

#b. The overall proportion of people taller than 70 inches:
# a weighted average proportion of the both gender mean heights which taller than 70 inches. 

#######5
#5.a
prgeng=read.csv("prgeng.csv")
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13 ,]
pe <- tmp[ ,c(9,2,13,10,14,15,16)]
model=lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem, data=pe)
summary(model)
qbinom(0.975, 14066, 0.5)
qbinom(0.025, 14066, 0.5, lower.tail = FALSE)
interval_high <- qbinom(0.975, 14066, 0.5) - 11176.740
interval_low <- -11176.740 - qbinom(0.975, 14066, 0.5)
ci <- data.frame(interval_low, interval_high)
ci 

#5.b
prgeng=read.csv("prgeng.csv")
prgeng$age2 <- prgeng$age^2
edu <- prgeng$educ
prgeng$ms <- as.integer(edu == 14)
prgeng$phd <- as.integer(edu == 16)
prgeng$fem <- prgeng$sex - 1
tmp <- prgeng[edu >= 13 ,]
pe <- tmp[ ,c(9,2,13,10,14,15,16)]
pe$msfem <- pe$ms * pe$fem
pe$phdfem <- pe$phd * pe$fem
modelb=lm(wageinc ~ age+age2+wkswrkd+ms+phd+fem+msfem+phdfem, data=pe)
summary(modelb)
qbinom(0.975, 14066, 0.25)
female_effect = -9091.230 - 5088.779
female_effect
interval_high <- qbinom(0.975, 14066, 0.25) + female_effect
interval_low <- female_effect - qbinom(0.975, 14066, 0.25)
ci <- data.frame(interval_low, interval_high)
ci 

#######6
bike=read.csv("day.csv")
bike$temp2 <- bike$temp^2
yr <- bike$yr
bike$clearday <- as.integer(bike$weathersit == 1)
lmout <- lm(registered ~ temp + temp2 + workingday + clearday + yr , data=bike)
summary(lmout)
qbinom(0.975, 731, 0.5)
interval_high <- qbinom(0.975, 731, 0.5) + 1716.25
interval_low <- 1716.25 - qbinom(0.975, 731, 0.5)
ci <- data.frame(interval_low, interval_high)
ci

#######7
#Hij means the height of the ith child in the dateset when he/she is j years old
#the population distribution of Hi is k-variate normal with mean vector μ and covariance matrix Σ.
#Dij = Hij+1 − Hij, j=1,2,...,k−1. 
#Dij means how much the ith child in the dateset grow taller is his/her j+1 years
# for each element in Hij+1 = Dij + Hij on corresponding position
#when j=max, Hik - Hi(k-1) = Di(k-1)
#            Hi(k-1) - Hi(k-2) = Di(k-2)
#        so, Hik - Hi(k-2) = Di(k-1) + Di(K-2)
# which means, in Dij, [later + former] equals to [one element - second former] in Hij
# because, the population distribution of both Hij+1,Hij is k-variate normal
# So, The population distribution of Di is (k-1) variate normal distribution

#######8
# ρ^2 = 1 - Var(ε) / Var(Y) = 1 - Var(ε) / (Var(μ(X)) + Var(ε)) = 1 - p/(p+p) = 0.5


