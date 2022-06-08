#######################
# Loading Iris data set
#######################

# Method 1

library(datasets)
data(iris)

iris <- datasets::iris

# View the data
view(dataset)

############################
# Display Summary Statistics
############################

# head() / tail()
head(iris,4)
tail(iris,4)

# summary()
summary(iris)
summary(iris$Sepal.Length)

# Check to see if there are missing data
sum(is.na(iris))

# skimr() - expands on summary()by providing larger set of statistics
# Install.packages("skimr")
# https://github.com/ropensci/skimr

 library(skimr)

skim(iris) # Perform skim to display summary statistics

# Group data by species then perform skim
iris %>%
  dplyr::group_by(Species) %>%
  skim()

############################
# Quick data visualization
#
# R base plot()
############################

# Panel plots
plot(iris)
plot(iris,col = "red")

# Scatter plot
plot(iris$Sepal.Width, iris$Sepal.Length)

plot(iris$Sepal.Width, iris$Sepal.Length,col = "red")   # Makes red circles

plot(iris$Sepal.Width, iris$Sepal.Length,col = "red", xlab = "Sepal.Width", ylab = "Sepal.Length")  # Makes red circles + Add x and y axis lables

# Histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width,col = "red") # Makes red bars

# Feature plots
# https://www,machinelearningplus.com/machine.learning//caret.package
featureplot(x=iris[,1:4],
            y=iris$Species,
            plot = "box",
            strip=strip.custom(par.strip.text.list(cex=,7)),
            scales = list(x = list(relation= "free"),
                          y = list(relation="free")))


##################################
# Building a Classification Model
##################################

# Importing Libraries
library(datasets)    # Contains the dataset
library(caret)       # Packages for machine learning algorithms 

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the dataset
TrainingIndex <- createDataPartition(iris$Species,p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,]   # Training Set
TestingSet <- iris[-TrainingIndex,]   # Test Set


###############################
# SVM model (polynomial kernel)

# Build Training model
Model <- train(Species ~ .,data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess = c("scale","center"),
                  trcontrol = trainControl(method="none"),
                  tuneGrid = data.frame(degree=1,scale=1,C=1))

# Build CV Model
Model.cv <- train(Species~., data = TrainingSet,
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
Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <- confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <- confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance <-varImp(Model)
plot(Importance)
plot(Importance,col = "red")







