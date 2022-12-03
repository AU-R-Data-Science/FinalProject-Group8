# Final_Project-Group_8
R package implementing logistic regression using numerical optimization. The package must contain the basic functions to perform logistic regression (e.g. estimate the coefficient vector β which includes the independent variables/predictors plus the intercept) and obtain different outputs from the procedure.
The outputs include:
initial values for optimization
bootstrap confidence intervals
Logistic curve
Confusion matrix
prevalence
accuracy
sensitivity
specificity
false discovery rate
diagnostic odds ratio

install.packages("devtools")


library(devtools)



install_github("AU-R-Data-Science/FinalProject-Group8/finalgroup8")


Descriptions of the relevant functions that can be used from this package


initialbeta():This function can help calculating the initial beta value with X,y which are a matrix of beta and numerical vector


optimizing():This function will based on the original logistic equation to calculte the summary of beta with X,y and beta  X a matrix of beta and y a numerical vector

estBeta(X,y): This function is used to calculte the beta estimation with list of beta hat and betan

interval(X,y,alpha,B=20): This function will help to calculate the confidence interval with bootsrap with default number of 20.

confusion_matrix(X,y,bhat,cut=0.5): This function will help to calculate the Confusion matrix

Prevalence(X,y,bhat,cut=0.5)
Accuracy(X,y,bhat,cut=0.5)
Sensitivity(X,y,bhat,cut=0.5)
Specificity(X,y,bhat,cut=0.5)
False_Discovery_Rate(X,y,bhat,cut=0.5)
Diagnostic_Odds_ration(X,y,bhat,cut=0.5)
The six functions shows above can be used for calcultaing the six different metrics.

prevalencegrid(X,y,bhat,cut=0.5): This one is used for ploting Prevalence over a grid of cut-off value.

logiplot(X,y): This function can help to draw the logistic curve
