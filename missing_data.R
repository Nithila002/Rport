
#####################
# Loading DHFR data 
####################


library(RCurl)
dhfr <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/dhfr.csv"))

# View the data
view(dhfr)


# Check for missing data
sum(is.na(dhfr))

# If data is clean, randomly introduce NA to the dataset

na.gen <- function(data,n) {
   i <- 1
   while (i < n+1) {
     idx1 <- sample(1:nrow(data), 1)
     idx2 <- sample(1:ncol(data), 1)
     data[idx1,idx2] <- NA
     i = i=1
   }
   return(data)
}

# Choose one of the following to run (they'll produce the same result)

dhfr <- na.gen(dhfr,100)
dhfr <- na.gen(n=100,data=dhfr)
dhfr <- na.gen(100,dhfr)         # this produces an error,why?

# Check again for missing data

sum(is.na(dhfr))
colSums(is.na(dhfr))
str(dhfr)
    
# List rows with missing data

missingdata <- dhfr[!complete.cases(dhfr),]

sum(is.na(missingdata))

# If the above sum is 0, this means there is no missing data and proceed to modeling
# If the above sum is greater than 0, then proceed to #5


############################
# Handling the missing data
###########################


# Simply delete all entries with missing data

clean.data <- na.omit(dhfr)
sum(is.na(clean.data))

# Imputation : replace missing data with the column's

# MEAN

dhfr.impute <- dhfr

for(i in which(sapply(dhfr.impute, is.numeric))) {
   dhfr.impute[is.na(dhfr.impute[, i]), i] <- mean(dhfr.impute[, i], na.rm = TRUE)
  }

sum(is.na(dhfr.impute))

# MEDIAN

dhfr.impute <- dhfr

for(i in which(sapply(dhfr.impute, is.numeric))) {
  dhfr.impute[is.na(dhfr.impute[, i]), i] <- median(dhfr.impute[, i], na.rm = TRUE)
}

sum(is.na(dhfr.impute)) 
   

