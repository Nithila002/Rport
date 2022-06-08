#######################
# Loading dhfr data set
#######################

library(caret)
data(dhfr)
     
# View the data
View(dhfr)


##################################
# Building a Classification Model
##################################

# Importing Libraries
library(datasets)    # Contains the dataset
library(caret)       # Packages for machine learning algorithms 

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

# Save model to RDS file
saveRDS(Model, "Model.rds")

# Read the model from RDS file
read_Model <- readRDS("Model.rds")

# Apply model for prediction
Model.training <- predict(read.Model,TrainingSet)  # Apply model to make prediction on training set
Model.testing <- predict(read.Model,TestingSet)  # Apply model to make prediction on testing set

# Model Performance (Displays confusion matrix and statistics)
Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <- confusionMatrix(Model.testing, TestingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)


# Feature importance
Importance <-varImp(Model)
plot(Importance)
plot(Importance,col = "red")







