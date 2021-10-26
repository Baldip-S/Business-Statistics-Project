library(data.table)
#Setting working directory where .csv files are stored
setwd("C:/Users/baldi/OneDrive/Desktop/Study/Second Year/Business Statistics/Assignment/pp_gas_emission/AllSets") 
files <- list.files(pattern = ".csv") #Assigning variable and signalling what file types to look for
temp <- lapply(files, fread, sep=",") #Assigning variable and reading number of files
data <- rbindlist(temp) #Merging all data sets
write.csv(predictedTest, file="testData") #Creating new .csv file with specified file name

data <- read.csv(file.choose(), sep = ";") 
summary(data)

library(Amelia)
missmap(data, main = "Missing values vs observed") #Plots showing any missing values

library(caTools)
set.seed(123)
sample <- sample.split(data, SplitRatio = 0.75)
trainingData = subset(data, sample == TRUE) #Creates a training set of data  where True has been assigned
testData = subset(data, sample == FALSE) #Creates a test set where False


model1 <- lm(TEY ~ CDP, data = trainingData) #Creating Regression model 

summary(model1) #View model statistics

plot(model1) #Plots to view model residuals

library(dplyr)

trainingPrediction <- mutate(trainingData, prediction = predict(model1), #Running the model on training data
              error = prediction-TEY, sq.error = error^2) #Adding error and sq.error columns for RMSE and MAPE calculations

applyModel <- mutate(testData, prediction = (predict(model1, testData)), #Running the model on Test data
            error = prediction-TEY, sq.error = error^2) #Adding error and sq.error columns for RMSE and MAPE calculations

predictedTest <- data.frame(applyModel$CDP,applyModel$TEY,
                  applyModel$prediction)
predictedTest


plot(applyModel$TEY, applyModel$prediction, #Plotting Actual vs Predicted values for Test data
     xlab = "Actual Energy Yield", ylab = "Predicted Energy Yield",
     main = "Testing Model on Test Sample")

plot(trainingPrediction$TEY,trainingPrediction$prediction, #Plotting Actual vs Predicted values for Training data
     xlab = "Actual Energy Yield", ylab = "Predicted Energy Yield", 
     main = "Testing Model on Trained Sample")

trainingRmse <- sqrt(mean(trainingPrediction$sq.error)) #RMSE Calculation for Training data
trainingRmse

trainingMape <- (mean(trainingPrediction$error/trainingPrediction$TEY))*100 #MAPE Calculation for Training data
trainingMape

testRmse <- sqrt(mean(applyModel$sq.error)) #RMSE Calculation for Test data
testRmse

testMape <- (mean(applyModel$error/applyModel$TEY))*100 #MAPE Calculation for Test data
testMape
