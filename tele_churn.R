## ------------------------------------------------------------------------
library(plyr)
library(corrplot)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(caret)
library(MASS)
library(party)
library(RColorBrewer)
library(ROCR)
library(class)
library(rpart)
library(rattle)
library(rpart.plot)

## ------------------------------------------------------------------------
df_churn <- read.csv('Churn.csv', stringsAsFactors = T)
head(df_churn)


## ------------------------------------------------------------------------
str(df_churn)
summary(df_churn)

## ------------------------------------------------------------------------
sapply(df_churn, function(x) sum(is.na(x)))


## ------------------------------------------------------------------------
count(df_churn, 'gender')
table(df_churn$gender)

df_churn$SeniorCitizen= as.factor(df_churn$SeniorCitizen)
## ------------------------------------------------------------------------
#count(df_churn, 'SeniorCitizen')
#count(df_churn, ' Partner')
#count(df_churn, 'Dependents')
#count(df_churn, 'tenure')
#count(df_churn, ' CallService')
#count(df_churn, ' MultipleConnections')
#count(df_churn, ' InternetConnection')
#count(df_churn, ' OnlineSecurity')
#count(df_churn, ' OnlineBackup')
#count(df_churn, ' DeviceProtectionService')
#count(df_churn, ' TechnicalHelp')
#count(df_churn, ' OnlineTV')
#count(df_churn, ' OnlineMovies')
#count(df_churn, ' Agreement')
#count(df_churn, ' BillingMethod')
#count(df_churn, ' PaymentMethod')
#count(df_churn, ' MonthlyServiceCharges')
#count(df_churn, ' TotalAmount')
#count(df_churn, ' Churn')



## ------------------------------------------------------------------------
#ncol(df_churn)
cols_name <- c(10:15)
#col_name - c(10,12,14,16)
for(i in 1:ncol(df_churn[,cols_name])) 
    {
        df_churn[,cols_name][,i] <- as.factor(mapvalues
                                              (df_churn[,cols_name][,i], from =c("No internet service"),to=c("No")))}


## ------------------------------------------------------------------------
df_churn$MultipleConnections <- as.factor(mapvalues(df_churn$MultipleConnections, 
                                           from=c("No phone service"),
                                           to=c("No")))


## ------------------------------------------------------------------------
df_churn$SeniorCitizen = ifelse(df_churn$SeniorCitizen <=0.50, 0,1)
table(df_churn$SeniorCitizen)
df_churn$SeniorCitizen = as.factor(df_churn$SeniorCitizen)

df_churn$SeniorCitizen <- as.factor(mapvalues(df_churn$SeniorCitizen,
                                      from=c("0","1"),
                                      to=c("No", "Yes")))

table(df_churn$SeniorCitizen)
## ------------------------------------------------------------------------
df_churn$customerID <- NULL


## ------------------------------------------------------------------------
numeric.var <- sapply(df_churn, is.numeric) ## Find numerical variables
corr.matrix <- cor(df_churn[,numeric.var])  ## Calculate the correlation matrix
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numeric Variables", method="number")


## ------------------------------------------------------------------------
plot1 <- ggplot(df_churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot2 <- ggplot(df_churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot3 <- ggplot(df_churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(plot1, plot2, plot3, ncol=2)


## ------------------------------------------------------------------------
plot4 <- ggplot(df_churn, aes(x=CallService)) + ggtitle("Call Service") + xlab("Call Service") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot5 <- ggplot(df_churn, aes(x=MultipleConnections)) + ggtitle("Multiple Connections") + xlab("Multiple Connections") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot6 <- ggplot(df_churn, aes(x=InternetConnection)) + ggtitle("Internet Connection") + xlab("Internet Connection") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot7 <- ggplot(df_churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(plot4, plot5, plot6, plot7, ncol=2)


## ------------------------------------------------------------------------
plot12 <- ggplot(df_churn, aes(x=OnlineMovies)) + ggtitle("Online Movies") + xlab("Online Movies") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot13 <- ggplot(df_churn, aes(x=Agreement)) + ggtitle("Agreement") + xlab("Agreement") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot14 <- ggplot(df_churn, aes(x=BillingMethod)) + ggtitle("Billing Method") + xlab("Billing Method") + 
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
plot15 <- ggplot(df_churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = 100*(..count..)/sum(..count..)), width = 0.5) + ylab("Percentage") + coord_flip() + theme_minimal()
grid.arrange(plot12, plot13, plot14, plot15,  ncol=2)


## ------------------------------------------------------------------------
df_churn$tenure <- NULL
df_churn$PaymentMethod <- NULL
df_churn$gender <- NULL


## ------------------------------------------------------------------------
set.seed(2000)
intrain<- createDataPartition(df_churn$Churn,p=0.7,list=FALSE)

training<- df_churn[intrain,]
testing<- df_churn[-intrain,]


## ------------------------------------------------------------------------
dim(training); dim(testing)



## ------------------------------------------------------------------------
#help(rpart)
model_tree <- rpart(Churn ~ ., training, method = "class", control = list(maxdepth = 6))


## ------------------------------------------------------------------------
fancyRpartPlot(model_tree)

table(training$Churn)
4710/(4710+3925)
## ------------------------------------------------------------------------
printcp(model_tree) # display the results 



## ------------------------------------------------------------------------
plotcp(model_tree) # visualize cross-validation results 


## ------------------------------------------------------------------------
# Predict the values of the test set
pred_training <- predict(model_tree, training,type = "class")


## ------------------------------------------------------------------------
# Construct the confusion matrix: conf
conf_matrix <- table(training$Churn, pred_training)
conf_matrix
accuracy= (3816+2433)/(3816+2433+894+1492)

## ------------------------------------------------------------------------
# Predict the values of the test set
pred_test <- predict(model_tree, testing, type = "class")


## ------------------------------------------------------------------------
# Construct the confusion matrix: conf
conf_matrix <- table(testing$Churn, pred_test)
conf_matrix


## ------------------------------------------------------------------------
# Print out the accuracy
sum( diag(conf_matrix) ) / sum(conf_matrix)


## ------------------------------------------------------------------------
# Prune the tree: pruned
pruned <- prune(model_tree, cp = 0.02)

## ------------------------------------------------------------------------
# Draw pruned
fancyRpartPlot(pruned)


## ------------------------------------------------------------------------
pred_pruned <- predict(pruned, testing, type = "class")

## ------------------------------------------------------------------------
conf_i <- table(testing$Churn, pred_pruned)
conf_i


## ------------------------------------------------------------------------
# Print out the accuracy
sum( diag(conf_i) ) / sum(conf_i)


## ------------------------------------------------------------------------
# Change the first line of code to use information gain as splitting criterion
model_i <- rpart(Churn ~ ., training, method = "class",
                parms = list(split = "information"),control = rpart.control(cp = 0, maxdepth = 6,minsplit = 100))


## ------------------------------------------------------------------------
printcp(model_i) # display the results 



## ------------------------------------------------------------------------
plotcp(model_i) # visualize cross-validation results 


## ------------------------------------------------------------------------
pred_i <- predict(model_i, testing, type = "class")


## ------------------------------------------------------------------------
conf_i <- table(testing$Churn, pred_i)
conf_i

## ------------------------------------------------------------------------
# Print out the accuracy
sum( diag(conf_i) ) / sum(conf_i)


## ------------------------------------------------------------------------
# Prune the tree: pruned
pruned_i <- prune(model_i, cp = 0.01)


## ------------------------------------------------------------------------
# Draw pruned Tree
fancyRpartPlot(pruned_i)


## ------------------------------------------------------------------------
pred_pruned <- predict(pruned_i, testing, type = "class")


## ------------------------------------------------------------------------
confusionMatrix(testing$Churn, pred_pruned)


## ------------------------------------------------------------------------
all_probs <- predict(pruned_i, testing, type = "prob")


## ------------------------------------------------------------------------
probs <- all_probs[, 2]

## ------------------------------------------------------------------------
# Make a prediction object: pred
pred_test <- prediction(probs, testing$Churn)

# Make a performance object: perf
perf <- performance(pred_test, "tpr", "fpr")
plot(perf , col="blue")
abline(a=0,b=1)

