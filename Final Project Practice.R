library(readxl)
dataformat<-  readline(prompt = "Rename the data file to datasets.xlsx or datasets.csv and then press 1 (for XLSX data) or 2 (for CSV): ")

if(dataformat=="1") {
  userdata <- read_excel('datasets.xlsx')
} else if(dataformat=="2") {
  userdata <- read.csv(file = 'datasets.csv')
} else {
  error <- "Please check the file name and respective selection based on XLSX or CSV (XLSX=1 or CSV=2 only)"
  error
}

yk <- as.matrix(userdata$yk)
xk <- as.matrix(userdata$xk)
x <- 1:20
xk <- matrix(c(xk, rep(1, length(x))), ncol = 2, nrow = 20)

# Part-1: Initial values for optimization obtained from the least-squares formula
model <- lm(yk~xk) #to check if beta was being calculated correctly.
summary(lm(yk~xk))$coef #to check if beta was being calculated correctly.
(beta <- MASS::ginv(t(xk) %*% xk) %*% t(xk) %*% yk) #to check if beta was being calculated correctly.

Log <- function(xk, base = exp(1)) {
  LOG <- base::log(as.complex(xk), base = base)
  if (all(Im(LOG) == 0)) { LOG <- Re(LOG) }
  LOG }

  n = nrow(xk)
  prob<-c()
  b<-c()
  n=length(yk)
  for (i in 1:n){
    prob[i]<-1/1+exp(t(-xk[i,])%*%beta)
  }

  for (i in 1:n){
    b[i]<-(-yk[i]*log(prob[i])-((1-yk[i])*Log(xk=1-prob[i])))
  }
 sum(b)



  betahat<-beta
  betan<-optim(betahat,optimizing,xk,yk)
  result<-list(betahat,betan)
  return(result)




#Part-2: Bootstrap Confidence intervals

Bootstrap_CI <- function(alpha){
##Initial Bootstrap

# Number of boostrap replications
B <- 20

# Compute the length of vector
n <- length(xk)/2

# Number of boostrap replications
B <- 20

# Confidence level
alpha <- alpha

# Initialisation of
boot_beta <- rep(NA, B)

# Step 1
for (i in 1:B){
  # Step 2
  xk_star <- xk[sample(1:n, replace = TRUE)]
  yk_star <- yk[sample(1:n, replace = TRUE)]

  #yk_star <- as.matrix(yk_star)
  #xk_star <- as.matrix(xk_star)
  x <- 1:20
  xk_star <- matrix(c(xk_star, rep(1, length(x))), ncol = 2, nrow = 20)
  dim(xk_star)

  # Step 3
  #yk_star <- as.matrix(yk_star)
  #xk_star <- as.matrix(xk_star)

  boot_beta[i] <- MASS::ginv(t(xk_star) %*% xk_star) %*% t(xk_star) %*% yk_star

}


# Step 4
quantile(boot_beta, c(alpha/2, 1 - alpha/2))
}

#Part-3: Plot of the fitted logistic curve to the actual values
#load dataset
a <- model.matrix(~xk+yk) [,-3]
data <- a
data <- as.data.frame(data)
a <- as.data.frame(a)
class(data)
#split dataset into training and testing set
set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.7,0.3))
train <- as.data.frame(data[sample, ])
test <- as.data.frame(data[!sample, ])

#fit logistic regression model
model <- glm(yk~xk1, family="binomial", data=train)
summary(model)

#plot logistic regression curve
library(ggplot2)
ggplot(data, aes(x=xk1, y=yk)) +
  geom_point(alpha=.5) +
  stat_smooth(method="glm", se=FALSE, method.args = list(family=binomial))


#Part-4: Creating Confusion Matrix
#use model to predict probability of default
predicted <- predict(model, test, type="response")

#convert to 1's and 0's based on Cutoff Value
predicted1 <- ifelse(predicted > 0.5, 1, 0)

#create confusion matrix
pak::pak("caret")
library(caret)
predicted1 <- as.factor(predicted1)
test$yk <- as.factor(test$yk)
confusionmatrix <- confusionMatrix(test$yk, predicted1)

zzz <- confusionmatrix[2]

TP <- zzz[[c(1, 1)]]
FP <- zzz[[c(1, 2)]]
FN <- zzz[[c(1, 3)]]
TN <- zzz[[c(1, 4)]]
TPR <- TP/(TP+FN)
TNR <- TN/(TN+FP)

FNR <- FN/(FN+FP)
TNR <- TN/(TN+FP)

LRplus <- TPR/TNR
LRminus <- FNR/TNR

Prevalence <- confusionmatrix[[c(4, 8)]]
Accuracy <- confusionmatrix[[c(3, 1)]]
Sensitivity <- confusionmatrix[[c(4, 1)]]
Specificity <- confusionmatrix[[c(4, 2)]]
False_Discovery_Rate <- FP/(FN+TN)
Diagnostic_Odds_Ratio <- LRplus/LRminus

zz <- cbind(Prevalence,Accuracy,Sensitivity,Specificity,False_Discovery_Rate,Diagnostic_Odds_Ratio)
zz <- as.data.frame(zz)

install.packages("gtExtras")
library(gtExtras)
library(gt)
library(caret)
umer1 <- matrix(data=NA,nrow=6,ncol=9)

for (i in 1 : 9){

  predicted1 <- ifelse(predicted > (i/10), 1, 0)

  #create confusion matrix

  predicted1 <- as.factor(predicted1)
  test$yk <- as.factor(test$yk)
  confusionmatrix <- confusionMatrix(test$yk, predicted1)

  zzz <- confusionmatrix[2]

  TP <- zzz[[c(1, 1)]]
  FP <- zzz[[c(1, 2)]]
  FN <- zzz[[c(1, 3)]]
  TN <- zzz[[c(1, 4)]]
  TPR <- TP/(TP+FN)
  TNR <- TN/(TN+FP)

  FNR <- FN/(FN+FP)
  TNR <- TN/(TN+FP)

  LRplus <- TPR/TNR
  LRminus <- FNR/TNR

  Prevalence <- confusionmatrix[[c(4, 8)]]
  Accuracy <- confusionmatrix[[c(3, 1)]]
  Sensitivity <- confusionmatrix[[c(4, 1)]]
  Specificity <- confusionmatrix[[c(4, 2)]]
  False_Discovery_Rate <- FP/(FN+TN)
  Diagnostic_Odds_Ratio <- LRplus/LRminus

  is.finite(c(Prevalence,Accuracy,Sensitivity,Specificity,False_Discovery_Rate,Diagnostic_Odds_Ratio))

  zz <- cbind(Prevalence,Accuracy,Sensitivity,Specificity,False_Discovery_Rate,Diagnostic_Odds_Ratio)
  Metrics <- colnames(zz)
  umer1[,i] <- zz
class(umer1)
}

umer2 <- umer1
#umer2 <- do.call(data.frame, lapply(umer2, function(x) replace(x, is.infinite(x), NA)))
umer2 <- as.data.frame(umer2)
colnames(umer2) <- c("0.1 Cutoff", "0.2 Cutoff","0.3 Cutoff","0.4 Cutoff","0.5 Cutoff","0.6 Cutoff","0.7 Cutoff","0.8 Cutoff","0.9 Cutoff","Metrics")
umer0 <- c('Prevalence', 'Accuracy','Sensitivity','Specificity','False_Discovery_Rate','Diagnostic_Odds_Ratio')
umer2$Metrics <- umer0
umer3 <- cbind(umer0,umer2[,-10])
colnames(umer3) <- c("Metrics","0.1 Cutoff", "0.2 Cutoff","0.3 Cutoff","0.4 Cutoff","0.5 Cutoff","0.6 Cutoff","0.7 Cutoff","0.8 Cutoff","0.9 Cutoff")

library(gtExtras)
library(gt)
head(as.data.frame(umer3)) %>%
gt() %>%
gt_theme_nytimes() %>%
tab_header(title = "Metrics VS Prediction Cutoff")


#Part-5: Plot of any of the  metrics evaluated over a grid of cut-off values for prediction going from 0.1 to 0.9 with steps of 0.1.
umer4 <- umer3
umer44 <- lapply(umer4,is.infinite)
metricscheck <- sum(umer44[[1]])
point1cutoff <- sum(umer44[[2]])
point2cutoff <- sum(umer44[[3]])
point3cutoff <- sum(umer44[[4]])
point4cutoff <- sum(umer44[[5]])
point5cutoff <- sum(umer44[[6]])
point6cutoff <- sum(umer44[[7]])
point7cutoff <- sum(umer44[[8]])
point8cutoff <- sum(umer44[[9]])
point9cutoff <- sum(umer44[[10]])

zzzz <- rbind(metricscheck, point1cutoff,point2cutoff,point3cutoff,point4cutoff,point5cutoff,point6cutoff,point7cutoff,point8cutoff,point9cutoff)
zzzz <- zzzz[-1,]
index1 <- which(zzzz!=1)

yaxis <- umer2[index1]

xaxis <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
xaxis <- xaxis[index1]
dataforplot <- cbind(umer0,xaxis,yaxis)
colnames(dataforplot) <- c('Metrics', 'Cutoff_Value', 'Metrics_Value')
library(ggplot2)
ggplot(dataforplot) +
  geom_point(aes(y= Metrics_Value, x=Cutoff_Value, color=Metrics))+scale_y_continuous(breaks=seq(0,1,0.1)) + scale_x_continuous(name='Cutoff Value for Prediction', limits=c(0.1,0.9), breaks=seq(0.1,0.9,0.1))+ggtitle("Metrics Values VS Cutoff Prediction Value(s)-for which All Six Metrics are Computable")

#Part-6: Help documentation
