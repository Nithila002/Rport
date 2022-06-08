
##################################
# Building a Classification Model
##################################

# Importing Libraries
library(datasets)    # Contains the dataset
library(caret)       # Packages for machine learning algorithms 

# Importing the dhfr data set
data(dhfr)

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the dataset
TrainingIndex <- createDataPartition(dhfr$Y,p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,]   # Training Set
TestingSet <- dhfr[-TrainingIndex,]   # Test Set


###############################
# SVM model (polynomial kernel)

# Build Training model
Model <- train(Y ~ .,data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess = c("scale","center"),
               trcontrol = trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1))

# Build CV Model
Model.cv <- train(Y~., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess = c("scale","center"),
                  trcontrol=trainControl(method="cv",number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1))

# Apply model for prediction
Model.training <- predict(Model,TrainingSet)  # Apply model to make prediction on training set
Model.testing <- predict(Model,TestingSet)  # Apply model to make prediction on testing set
Model.cv <- predict(Model.cv,TrainingSet)  # Perform cross validation

# Model Performance (Displays confusion matrix and statistics)
Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <- confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion <- confusionMatrix(Model.cv, TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <-varImp(Model)
plot(Importance)
plot(Importance,col = "red")
