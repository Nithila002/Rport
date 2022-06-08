
#######################
# Loading dhfr data set
#######################

library(datasets) 
library(caret)    # Package for machine learnind and algortithms

# Importing DHFR data set
data(dhfr)

# Check to see if there are missing data
sum(is.na(dhfr))

# To achieve reproducible model; set the random seed number
set.seed(100)

# Performs stratified random split of the dataset
TrainingIndex <- createDataPartition = (dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,]   # Training Set
TestingSet <- dhfr[-TrainingIndex,]   # Test Set


##########################################
# Random forest

# Run normally without parallel processing
start.time <- proc.time()
Model <- train(T ~.,
               data = TrainingSet,  # Build model using trainingset
               method = "rf")  # Learning algorithm

stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

# Time taken is 80.50


#######################
# Parallel Computing

# Use doParallel
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(T ~.,
               data = TrainingSet,  # Build model using trainingset
               method = "rf")  # Learning algorithm

stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopcluster(cl)

# Time taken is 29.70
# 2.7 times faster with doparallel


##########################
# Hyperparameter tuning

start.time <- proc.time()
Model <- train(T ~.,
               data = TrainingSet,  # Build model using trainingset
               method = "rf",
               tuneGrid = data.frame(ntry=seq(5,15,by=5))
               )  

stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

# Time taken is 56.65

# Use doParallel
library(doParallel)
cl <- makePSOCKcluster(5)
registerDoParallel(cl)

start.time <- proc.time()
Model <- train(T ~.,
               data = TrainingSet,  # Build model using trainingset
               method = "rf",  # Learning algorithm
               tuneGrid = data.frame(ntry=seq(5,15,by=5))
              )

stop.time <- proc.time()
run.time <- stop.time - start.time
print(run.time)

stopcluster(cl)

# Time taken is 20.73
# 2.7 times faster with doparallel

#############################
# Apply model for prediction
Model.training <- predict(Model,TrainingSet)  # Apply model to make prediction on training set

# Model Performance (Displays confusion matrix and statistics)
Model.training.confusion <- confusionMatrix(Model.training, TrainingSet$Species)

print(Model.training.confusion)


# Feature importance
Importance <-varImp(Model)
plot(Importance, top =20)
plot(Importance, col = "red")







