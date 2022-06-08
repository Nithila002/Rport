#######################
# Loading DHFR data set
#######################

# Method 1

library(caret)
data(dhfr)

# View the data
View(dhfr)

############################
# Display Summary Statistics
############################

# head() / tail()
head(dhfr,4)
tail(dhfr,4)

# summary()
summary(dhfr)
summary(dhfr$Y) # Check active and inactive

# Check to see if there are missing data
sum(is.na(dhfr))

# skimr() - expands on summary()by providing larger set of statistics
# Install.packages("skimr")
# https://github.com/ropensci/skimr

library(skimr)

skim(dhfr) # Perform skim to display summary statistics

# Group data by Y (biological activity) then perform skim
dhfr %>%
  dplyr::group_by(Y) %>%
  skim()


############################
# Quick data visualization
#
# R base plot()
############################


# Scatter plot
plot(dhfr$moe2D_GCUT_SLOGP_2, dhfr$moe2D_GCUT_PEOE_2)

plot(dhfr$moe2D_GCUT_SLOGP_2, dhfr$moe2D_GCUT_PEOE_2,col = "red")   # Makes red circles

plot(dhfr$moe2D_GCUT_SLOGP_2, dhfr$moe2D_GCUT_PEOE_2,col = "red", xlab = "moe2D_GCUT_SLOGP_2", ylab = "moe2D_GCUT_PEOE_2")  # Makes red circles + Add x and y axis lables

# Histogram
hist(dhfr$moe2D_GCUT_SLOGP_2)
hist(dhfr$moe2D_GCUT_SLOGP_2,col = "red") # Makes red bars

# Feature plots
# https://www,machinelearningplus.com/machine.learning//caret.package
FeaturePlot(x = dhfr[,2:5],
            y = dhfr$Y,
            plot = "box",
            strip=strip.custom(par.strip.text.list(cex=,7)),
            scales = list(x = list(relation= "free"),
                          y = list(relation="free")))