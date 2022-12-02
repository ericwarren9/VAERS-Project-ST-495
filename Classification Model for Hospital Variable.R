# Purpose: To make a classification model predicting if a patient will have to be hospitalized


# Load in data ------------------------------------------------------------

library(tidyverse)

vaccines <- read_csv("~/VAERS-Project-ST-495/CreatedData/2016-21VAERSDataUpdated.csv")
vaccines$HOSPITAL <- factor(vaccines$HOSPITAL)
vaccines$OCCUR_YEAR <- factor(vaccines$OCCUR_YEAR)

# Split into train and test data ------------------------------------------

set.seed(9)
n <- nrow(vaccines)
trainProp <- 0.7
trainSize <- ceiling(n * trainProp)
testSize <- n - trainSize
randomSample <- sample(n, trainSize)
trainingData <- vaccines[randomSample, ]
testingData <- vaccines[-randomSample, ]


# Look at logistic regression model ---------------------------------------

# Fit the whole data first to remove not needed variables
logReg1 <- glm(HOSPITAL ~ . - DIED - ER_VISIT - HOSPDAYS, trainingData, family = "binomial")
summary(logReg1)

# Fit new model to remove not needed variables and then check to make sure all variables are needed
logReg2 <- glm(HOSPITAL ~ . - DIED - ER_VISIT - HOSPDAYS - STATE, trainingData, family = "binomial")
summary(logReg2)

# See how it did on training data
logreg.pred <- predict(logReg2, trainingData, "response")
hospital.pred <- ifelse(logreg.pred >= .5, "Y", "N")
logTrainError <- sum(trainingData$HOSPITAL != factor(hospital.pred)) / nrow(trainingData)
logTrainError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixLogTrain <- confusionMatrix(factor(hospital.pred), trainingData$HOSPITAL)
confusionMatrixLogTrain

# Make predictions using test data
logreg.pred <- predict(logReg2, testingData, "response")
hospital.pred <- ifelse(logreg.pred >= .5, "Y", "N")
logTestError <- sum(testingData$HOSPITAL != factor(hospital.pred)) / nrow(testingData)
logTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixLogTest <- confusionMatrix(factor(hospital.pred), testingData$HOSPITAL)
confusionMatrixLogTest


# Look at LDA -------------------------------------------------------------

# Make LDA Model
library(MASS)
lda <- lda(HOSPITAL ~ . - DIED - ER_VISIT - HOSPDAYS - STATE, trainingData)

# Make Training Data predictions
pred.ldaTrain <- predict(lda, trainingData)

# Misclassification Error Train Data
ldaTrainError <- sum(trainingData$HOSPITAL != pred.ldaTrain$class) / nrow(trainingData)
ldaTrainError

# Make confusion matrix to show how model did on training data
library(caret)
confusionMatrixLDATrain <- confusionMatrix(pred.ldaTrain$class, trainingData$HOSPITAL)
confusionMatrixLDATrain

# Make Testing Data predictions
pred.ldaTest <- predict(lda, testingData)

# Misclassification Error Test Data
ldaTestError <- sum(testingData$HOSPITAL != pred.ldaTest$class) / nrow(testingData)
ldaTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixLDATest <- confusionMatrix(pred.ldaTest$class, testingData$HOSPITAL)
confusionMatrixLDATest

# Look at QDA -------------------------------------------------------------

# Make QDA Model
library(MASS)
qda <- qda(HOSPITAL ~ . - DIED - ER_VISIT - HOSPDAYS - STATE, trainingData)

# Make Training Data predictions
pred.qdaTrain <- predict(qda, trainingData)

# Misclassification Error Train Data
qdaTrainError <- sum(trainingData$HOSPITAL != pred.qdaTrain$class) / nrow(trainingData)
qdaTrainError

# Make confusion matrix to show how model did on training data
library(caret)
confusionMatrixQDATrain <- confusionMatrix(pred.qdaTrain$class, trainingData$HOSPITAL)
confusionMatrixQDATrain

# Make Testing Data predictions
pred.qdaTest <- predict(qda, testingData)

# Misclassification Error Test Data
qdaTestError <- sum(testingData$HOSPITAL != pred.qdaTest$class) / nrow(testingData)
qdaTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixQDATest <- confusionMatrix(pred.qdaTest$class, testingData$HOSPITAL)
confusionMatrixQDATest


# Look at Naive Bayes Model -----------------------------------------------

# Make NB Model
library(e1071)
nb <- naiveBayes(HOSPITAL ~ . - DIED - ER_VISIT - HOSPDAYS - STATE, trainingData)

# Make Training Data predictions
pred.nbTrain <- predict(nb, trainingData)

# Misclassification Error Train Data
nbTrainError <- sum(trainingData$HOSPITAL != pred.nbTrain) / nrow(trainingData)
nbTrainError

# Make confusion matrix to show how model did on training data
library(caret)
confusionMatrixNBTrain <- confusionMatrix(pred.nbTrain, trainingData$HOSPITAL)
confusionMatrixNBTrain

# Make Testing Data predictions
pred.nbTest <- predict(nb, testingData)

# Misclassification Error Test Data
nbTestError <- sum(testingData$HOSPITAL != pred.nbTest) / nrow(testingData)
nbTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixNBTest <- confusionMatrix(pred.nbTest, testingData$HOSPITAL)
confusionMatrixNBTest


# Cross Validation --------------------------------------------------------

Models <- c("Logistic Regression", "Linear Discriminant Analysis", "Quadratic Discriminant Analysis", "Naive Bayes")

# Find the error rates of each with training data
trainingErrorRates <- c(logTrainError, ldaTrainError, qdaTrainError, nbTrainError)
trainingErrorModel <- as_tibble(cbind(Models, trainingErrorRates))
trainingErrorModel$trainingErrorRates <- round(as.numeric(trainingErrorModel$trainingErrorRates), 4)
names(trainingErrorModel) <- c("Models", "Training Error Rates")
trainingErrorModel

ggplot(trainingErrorModel, aes(x = Models, y = `Training Error Rates`)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  geom_text(aes(label = `Training Error Rates`), vjust = 1.6, color = "white", size = 3.5) +
  labs(title = "Logistic Regression Model is the Best \nFor Training Data",
       x = "Model Type",
       y = "Error Rates of the Model",) +
  theme_bw() +
  theme(plot.title = element_text(color = "blue",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5))

# Now look at Test Error Rates
testingErrorRates <- c(logTestError, ldaTestError, qdaTestError, nbTestError)
testingErrorModel <- as_tibble(cbind(Models, testingErrorRates))
testingErrorModel$testingErrorRates <- round(as.numeric(testingErrorModel$testingErrorRates), 4)
names(testingErrorModel) <- c("Models", "Testing Error Rates")
testingErrorModel

ggplot(testingErrorModel, aes(x = Models, y = `Testing Error Rates`)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  geom_text(aes(label = `Testing Error Rates`), vjust = 1.6, color = "white", size = 3.5) +
  labs(title = "Logistic Regression Model is the Best \nFor Testing Data",
       x = "Model Type",
       y = "Error Rates of the Model") +
  theme_bw() +
  theme(plot.title = element_text(color = "blue",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5))


# Making predictions and assign to observations ---------------------------

# Do training data
logreg.pred.train <- predict(logReg2, trainingData, "response")
trainingData$HOSPITAL_PROBABILITY <- logreg.pred.train

# Do testing data
logreg.pred.test <- predict(logReg2, testingData, "response")
testingData$HOSPITAL_PROBABILITY <- logreg.pred.test

# Combine into one set
allPatients <- rbind(trainingData, testingData)

# Show Error Rates
hospital.pred <- ifelse(allPatients$HOSPITAL_PROBABILITY >= .5, "Y", "N")
totalError <- sum(allPatients$HOSPITAL != factor(hospital.pred)) / nrow(allPatients)
totalError

# Make confusion table showing how well model did
library(caret)
confusionMatrixTotalError <- confusionMatrix(factor(hospital.pred), allPatients$HOSPITAL)
confusionMatrixTotalError

# See if there is a relationship between age and probability of hospitalization
ggplot(allPatients, aes(x = AGE_YRS, y = HOSPITAL_PROBABILITY)) +
  geom_point(alpha = 0.05, color = "red") +
  labs(title = "Relationship Between Age \nand Probability of Going to Hospital",
       x = "Age (in years)",
       y = "Probability of Going to the Hospital") +
  theme_bw() +
  theme(plot.title = element_text(color = "blue",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5))


# Make updated file in case we wanted to use data set for R shiny  --------

# Pick only needed variables for app
allPatientsSelected <- allPatients %>%
  dplyr::select(-c(DIED,
                   ER_VISIT,
                   HOSPDAYS,
                   STATE))

# Write updated csv file
write_csv(allPatientsSelected, "~/VAERS-Project-ST-495/CreatedData/VAERS2016-21HospitalPredictions.csv")

# Save Model for app if app is made
write_rds(logReg2, "logRegModel4HospitalVAERS.rds")