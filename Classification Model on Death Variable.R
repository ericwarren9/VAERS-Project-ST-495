# Purpose: To see what type of classification method will be the best


# Load data ---------------------------------------------------------------

library(tidyverse)

vaccines <- read_csv("~/VAERS-Project-ST-495/CreatedData/2016-21VAERSData.csv")


# Manipulate data ---------------------------------------------------------

vaccines2 <- vaccines %>% 
  dplyr::select(-c(CAGE_YR, CAGE_MO, RPT_DATE, DATEDIED, VAX_DATE, ONSET_DATE, LAB_DATA, PRIOR_VAX, SPLTTYPE, FORM_VERS, TODAYS_DATE, ALLERGIES, OTHER_MEDS, HISTORY)) %>%
  mutate(OCCUR_YEAR = substr(RECVDATE, 1, 4),
         STATE = toupper(STATE),
         ER_VISIT = ifelse(is.na(ER_VISIT), "N", ER_VISIT),
         SYMPTOMS = ifelse(grepl("None", SYMPTOM_TEXT), "N", "Y"),
         CUR_ILL = ifelse(is.na(CUR_ILL), "No", CUR_ILL),
         CUR_ILL = ifelse(grepl("No", CUR_ILL), "N", "Y"),
         HOSPDAYS = ifelse(HOSPDAYS >= 50, 50, HOSPDAYS),
         DIED = factor(DIED)) %>%
  dplyr::select(-c(RECVDATE, SYMPTOM_TEXT)) %>%
  dplyr::select(OCCUR_YEAR, STATE, everything())

vaccines3 <- vaccines2[rowSums(is.na(vaccines2)) == 0, ] # Get rid of missing values

vaccines4 <- vaccines3[vaccines3$STATE %in% state.abb, ]

write_csv(vaccines4, "~/VAERS-Project-ST-495/CreatedData/2016-21VAERSDataUpdated.csv")


# Split into train and test data ------------------------------------------

# If starting with updated data, start with the following code below:
# vaccines4 <- read_csv("~/VAERS-Project-ST-495/CreatedData/2016-21VAERSDataUpdated.csv")
# vaccines4$DIED <- factor(vaccines4$DIED)

set.seed(9)
n <- nrow(vaccines4)
trainProp <- 0.7
trainSize <- ceiling(n * trainProp)
testSize <- n - trainSize
randomSample <- sample(n, trainSize)
trainingData <- vaccines4[randomSample, ]
testingData <- vaccines4[-randomSample, ]


# Look at logistic regression model ---------------------------------------

# Fit the whole data first to remove not needed variables
logReg1 <- glm(DIED ~ ., trainingData, family = "binomial")
summary(logReg1)

# Fit new model to remove not needed variables and then check to make sure all variables are needed
logReg2 <- glm(DIED ~ . - STATE - NUMDAYS - ER_VISIT - V_ADMINBY - V_FUNDBY - BIRTH_DEFECT, trainingData, family = "binomial")
summary(logReg2)

# See how it did on training data
logreg.pred <- predict(logReg2, trainingData, "response")
died.pred <- ifelse(logreg.pred >= .5, "Y", "N")
logTrainError <- sum(trainingData$DIED != factor(died.pred)) / nrow(trainingData)
logTrainError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixLogTrain <- confusionMatrix(factor(died.pred), trainingData$DIED)
confusionMatrixLogTrain

# Make predictions using test data
logreg.pred <- predict(logReg2, testingData, "response")
died.pred <- ifelse(logreg.pred >= .5, "Y", "N")
logTestError <- sum(testingData$DIED != factor(died.pred)) / nrow(testingData)
logTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixLogTest <- confusionMatrix(factor(died.pred), testingData$DIED)
confusionMatrixLogTest


# Look at LDA -------------------------------------------------------------

# Make LDA Model
library(MASS)
lda <- lda(DIED ~ . - STATE - NUMDAYS - ER_VISIT - V_ADMINBY - V_FUNDBY - BIRTH_DEFECT, trainingData)

# Make Training Data predictions
pred.ldaTrain <- predict(lda, trainingData)

# Misclassification Error Train Data
ldaTrainError <- sum(trainingData$DIED != pred.ldaTrain$class) / nrow(trainingData)
ldaTrainError

# Make confusion matrix to show how model did on training data
library(caret)
confusionMatrixLDATrain <- confusionMatrix(pred.ldaTrain$class, trainingData$DIED)
confusionMatrixLDATrain

# Make Testing Data predictions
pred.ldaTest <- predict(lda, testingData)

# Misclassification Error Test Data
ldaTestError <- sum(testingData$DIED != pred.ldaTest$class) / nrow(testingData)
ldaTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixLDATest <- confusionMatrix(pred.ldaTest$class, testingData$DIED)
confusionMatrixLDATest

# Look at QDA -------------------------------------------------------------

# Make QDA Model
library(MASS)
qda <- qda(DIED ~ . - STATE - NUMDAYS - ER_VISIT - V_ADMINBY - V_FUNDBY - BIRTH_DEFECT, trainingData)

# Make Training Data predictions
pred.qdaTrain <- predict(qda, trainingData)

# Misclassification Error Train Data
qdaTrainError <- sum(trainingData$DIED != pred.qdaTrain$class) / nrow(trainingData)
qdaTrainError

# Make confusion matrix to show how model did on training data
library(caret)
confusionMatrixQDATrain <- confusionMatrix(pred.qdaTrain$class, trainingData$DIED)
confusionMatrixQDATrain

# Make Testing Data predictions
pred.qdaTest <- predict(qda, testingData)

# Misclassification Error Test Data
qdaTestError <- sum(testingData$DIED != pred.qdaTest$class) / nrow(testingData)
qdaTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixQDATest <- confusionMatrix(pred.qdaTest$class, testingData$DIED)
confusionMatrixQDATest


# Look at Naive Bayes Model -----------------------------------------------

# Make NB Model
library(e1071)
nb <- naiveBayes(DIED ~ . - STATE - NUMDAYS - ER_VISIT - V_ADMINBY - V_FUNDBY - BIRTH_DEFECT, trainingData)

# Make Training Data predictions
pred.nbTrain <- predict(nb, trainingData)

# Misclassification Error Train Data
nbTrainError <- sum(trainingData$DIED != pred.nbTrain) / nrow(trainingData)
nbTrainError

# Make confusion matrix to show how model did on training data
library(caret)
confusionMatrixNBTrain <- confusionMatrix(pred.nbTrain, trainingData$DIED)
confusionMatrixNBTrain

# Make Testing Data predictions
pred.nbTest <- predict(nb, testingData)

# Misclassification Error Test Data
nbTestError <- sum(testingData$DIED != pred.nbTest) / nrow(testingData)
nbTestError

# Make confusion matrix to show how model did on test data
library(caret)
confusionMatrixNBTest <- confusionMatrix(pred.nbTest, testingData$DIED)
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
trainingData$PROBABILITY <- logreg.pred.train

# Do testing data
logreg.pred.test <- predict(logReg2, testingData, "response")
testingData$PROBABILITY <- logreg.pred.test

# Combine into one set
allPatients <- rbind(trainingData, testingData)

# Show Error Rates
died.pred <- ifelse(allPatients$PROBABILITY >= .5, "Y", "N")
totalError <- sum(allPatients$DIED != factor(died.pred)) / nrow(allPatients)
totalError

# Make confusion table showing how well model did
library(caret)
confusionMatrixTotalError <- confusionMatrix(factor(died.pred), allPatients$DIED)
confusionMatrixTotalError

# See if there is a relationship between age and probability of death
ggplot(allPatients, aes(x = AGE_YRS, y = PROBABILITY)) +
  geom_point(alpha = 0.05, color = "red") +
  labs(title = "Relationship Between Age \nand Probability of Death",
       x = "Age (in years)",
       y = "Probability of Death") +
  theme_bw() +
  theme(plot.title = element_text(color = "blue",
                                  face = "bold",
                                  size = 16,
                                  hjust = 0.5))


# Make updated file in case we wanted to use data set for R shiny  --------

# Pick only needed variables for app
allPatientsSelected <- allPatients %>%
  dplyr::select(-c(STATE, 
                   NUMDAYS, 
                   ER_VISIT,
                   V_ADMINBY,
                   V_FUNDBY,
                   BIRTH_DEFECT))

# Write updated csv file
write_csv(allPatientsSelected, "~/VAERS-Project-ST-495/CreatedData/VAERS2016-21DeathPredictions.csv")