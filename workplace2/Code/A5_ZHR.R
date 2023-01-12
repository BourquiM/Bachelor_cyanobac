setwd("~/GitHub/Bachelor_cyanobac/workplace2/Code")

source("A0_functions.R")  # The functions needed to run the code are stored in a separate file. This command imports the functions.

# Loading the necessary libraries
load_libraries()


# loading the data files
setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new")
databases()  # loading the databases metaDBphyto and metaDBzoo
phyto_raw <- read.csv("phyto.76-21_ZHR.csv", sep = ",")  # loading the raw phytoplankton data
zoo_raw <- read.csv("largezoo.77-21_ZHR.csv")  # loading the raw zooplankton data

setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new/temp_chem")  # loading the raw temperature and chemistry data
temp_chem <- read.csv("temp.chem.77-20_ZHR.csv")

lake <- "ZHR"  # Defining the name of the lake. This will be used when saving data in external files, to identify them later on

# Getting the phytoplankton data ready
ready_phyto_selec()  # setting the right data formats for the columns and adding the corresponding information from the meta database, for each entry
treat_noBiovolP(lake)  # Isolating the phyto data missing a biovolume value in the meta database
ready_phyto_clean()  # Removing entries without a metaDB-biovolume value and calculating the biovolume for the ones we keep
ready_phyto_agg()  # Aggregating phytoplankton biovolume per order and date
ready_phyto_final()  # Making the phytoplankton data ready for analysis and calculating difference and ratio between the phyto orders and the total phyto biovolume per day

# Getting the zooplankton data ready
ready_zoo_raw()  # Setting the right data formats
ready_zoo_agg()  # Aggregating the abundance of chosen orders/phyla/families per day
ready_zoo_final()  # Making the zooplankton data ready for analysis

# Getting the chemistry and temperature data ready
ready_temp_chem()  # Preparing the chemistry and temperature data



# It is best not to make the plots in functions, since the axes have to be set manually for each lake
{
  # temp_chem %>% filter(NO3_N > 10)  # The line to show the outlier
  temp_chem <- temp_chem %>% filter(NO3_N < 10)  # there is an outlier that's falsifying the whole data (and it isn't realistic) -> this step excludes it
  
  # Creating the plots with the evolution of the temperature and chemistry of the lake over time
  # Plotting the temperature alone
  temp <- ggplot(data = temp_chem, aes(x = date, y = temperature)) + 
    geom_point(shape = 20, colour = "aquamarine4", size = 0.5) + 
    geom_smooth(colour = "aquamarine4") + 
    ggtitle(label = "Temperature over the years") + 
    labs(x = "Date", y = "Temperature [°C]")
  
  # Plotting the Phosphate alone
  po4 <- ggplot(data = temp_chem, aes(x = date, y = PO4_P)) + 
    geom_point(shape = 20, colour = "firebrick3", size = 0.5) + 
    geom_smooth(colour = "firebrick3") + 
    ggtitle(label = "PO4 over the years") + 
    labs(x = "Date", y = "PO4-P concentration [mg/L]")
  
  # Plotting the Ammonia alone
  nh4 <- ggplot(data = temp_chem, aes(x = date, y = NH4_N)) + 
    geom_point(shape = 20, colour = "cornflowerblue", size = 0.5) + 
    geom_smooth() + 
    ggtitle(label = "NH4 over the years") + 
    labs(x = "Date", y = "NH4-N concentration [mg/L]") + 
    scale_y_continuous(lim = c(0, 0.15))
  
  # Plotting the Nitrate alone
  no3 <- ggplot(data = temp_chem, aes(x = date, y = NO3_N)) + 
    geom_point(shape = 20, colour = "darkgoldenrod1", size = 0.5) + 
    geom_smooth(colour = "darkgoldenrod1") + # adding 'method = "gam"' shows the oscillation of the data points even more
    ggtitle(label = "NO3 over the years") + 
    labs(x = "Date", y = "NO3-N concentration [mg/L]")
  
  figure_tempchem <- ggarrange(temp, po4, 
                               nh4, no3, 
                               ncol = 2, nrow = 2)  # Putting the plots about chemistry and temperature together into a figure
  figure_tempchem <- annotate_figure(figure_tempchem, top = text_grob(paste(lake, "lake"), face = "bold", size = 20))  # Adding a title to the figure
  
  # Plotting the Nitrate and Ammonia in a single plot
  coeff1 <- 20  # This variable is used to change the right y-axis size
  no3_nh4 <- ggplot(data = temp_chem) + 
    geom_point(aes(x = date, y = NH4_N * coeff1, colour = "NH4"), size = 0.5) + 
    geom_point(aes(x = date, y = NO3_N, colour = "NO3"), size = 0.5) +
    geom_smooth(aes(x = date, y = NH4_N * coeff1, colour = "NH4")) + 
    geom_smooth(aes(x = date, y = NO3_N, colour = "NO3")) + 
    scale_y_continuous(name = "NO3-N [mg/L]", lim = c(0, 3.5), sec.axis = sec_axis(trans=~./coeff1, name = "NH4-N [mg/L]")) + 
    ggtitle(label = "NO3 and NH4 over the years") + 
    scale_color_manual(name = "", breaks = c("NO3", "NH4"), values = c("NO3" = "darkgoldenrod1", "NH4" = "cornflowerblue")) + 
    theme(legend.position = "bottom")
  
  # Plotting the Phosphate and temperature in a single plot
  coeff2 <- 60
  temp_po4 <- ggplot(data = temp_chem) + 
    geom_point(aes(x = date, y = temperature, colour = "Temperature"), size = 0.5) + 
    geom_point(aes(x = date, y = PO4_P * coeff2, colour = "PO4"), size = 0.5) + 
    geom_smooth(aes(x = date, y = temperature, colour = "Temperature")) + 
    geom_smooth(aes(x = date, y = PO4_P * coeff2, colour = "PO4")) + 
    scale_y_continuous(name = "Temperature [°C]", sec.axis = sec_axis(trans=~./coeff2, name = "PO4-P [mg/L]")) + 
    ggtitle(label = "Temperature and PO4 over the years") +
    scale_color_manual(name = "", breaks = c("Temperature", "PO4"), values = c("Temperature" = "aquamarine4", "PO4" = "firebrick3")) + 
    theme(legend.position = "bottom", plot.title = element_text(size = 10))
} #



# Putting the phytoplankton, zooplankton and chemistry + temperature data into a single frame
ready_end_frame()  # Gathering all the phytoplankton, zooplankton, temperature and chemistry data into a single data frame. And making it ready for analysis

save_plots(lake)  # plotting the phytoplankton data and the basic scatterplots for the linear regressions and saving them with the temp-chem data in a single pdf file



# Based on the pairs-plots, I think there might be a chance of linear relationship between no3 and po4 and between temp and tot phyto 
# Note from the author: linear relationships haven't been analyzed very thoroughly. More emphasis on Random Forests




# 9) Linear regressions (or log-linear regressions) ####
# Je pense que ça, on ne peut pas tout le faire en fonction, parce qu'il faut jeter un oeil à ce qui doit être modellisé avant de le faire

# these are simple scatterplots, for all variable relative to each other 
pairs_norm 
pairs_log10 


# Relationship between no3 and po4:
fit_no3_nh4 <- lm(data = end_frame, NO3_N ~ PO4_P)
summary(fit_no3_nh4)  
cor(end_frame %>% select(NO3_N, PO4_P) %>% na.omit() %>% select(NO3_N), end_frame %>% select(NO3_N, PO4_P) %>% na.omit() %>% select(PO4_P))
# highly significant pair-wise correlation of 0.691, p < 2e-16
par(mfrow = c(1,2))
plot(fit_no3_nh4, which = c(1:2))
par(mfrow = c(1,1))
# the QQ-plot looks ok, but the TA-plot is very messed up (wave shape)  --> shouldn't use the linear regression approach


# Relationship between log(tot_phyto) and temperature
fit_temp_totPhyto <- lm(data = end_frame, log(tot_phyto) ~ temperature)
summary(fit_temp_totPhyto)
cor(end_frame %>% select(temperature, tot_phyto) %>% na.omit() %>% select(tot_phyto) %>% summarize(tot_phyto = log(tot_phyto)), end_frame %>% select(tot_phyto, temperature) %>% na.omit() %>% select(temperature))
# highly significant pair-wise correlation of 0.561, p < 2e-16
par(mfrow = c(1,2))
plot(fit_temp_totPhyto, which = c(1:2))
par(mfrow = c(1,1))
# On this one, the TA-plot looks and the GG-plot look very messed up (TA has a triangular shape, and the QQ is too bent) --> shouldn't use the linear regression approach

fit_phyto <- lm(data = end_frame, log(tot_phyto) ~ temperature + NH4_N + NO3_N + PO4_P)
fit_phyto1 <- lm(data = end_frame, log(tot_phyto) ~ temperature * NH4_N * NO3_N * PO4_P)
anova(fit_phyto, fit_phyto1) #  This tests whether the interaction terms between temperature, nh4, no3 and po4 are necessary (i.e. if the interactions between them are significant). Since the p-value is high (>0.05), the interaction terms aren't needed, and we can thus say the "fit_phyto" is the model we need to look at
summary(fit_phyto)
par(mfrow = c(1,2))
plot(fit_phyto, which = c(1:2))
par(mfrow = c(1,1))
# Both the TA-plot and the QQ-plot don't look good enough (QQ is curved too much, and the TA plot has too much of a diamond/lozenge shape)



# 10) Random forest ####
#' 
#' I think these steps have to be done individually for each lake, because I have to analyse each summary to see where there are unbalanced features, and such
#' 

# The next two lines makes the code run in parallel (this means that the code runs faster). Not so important for the small size of the data sets I have, but it's a good habit to have
cl = makePSOCKcluster(5)
registerDoParallel(cl)
# Don't forget to stop the cluster when done working:
# stopCluster(cl)

summary(end_frame)
end_frame <- end_frame %>% select(-c(date, depth))  #removing the date and the depth from the DF - we don't need those anymore



# 10.1) Random Forest on r_cyanobac ####
#' 
#' Choosing r_cyanobac as the response variable and creating the models for this variable
#' 
featurePlot(x = end_frame[, c("tot_phyto", "NH4_N", "NO3_N", "temperature")], y = end_frame$r_cyanobac, plot = "scatter", labels = c("", "ratio cyanobac - tot_phyto"))


r_cyanobac_frame <- end_frame %>% select(r_cyanobac, NH4_N:calanoida)  

# Partitioning the data
set.seed(123) # doesn't need to be 123, but having a value here makes the test reproducible
trainIndex <- createDataPartition(r_cyanobac_frame$r_cyanobac, p = 0.7, list = FALSE)  # Choosing which indices of the data set will be for training, and which ones for testing. 70% are for training, 30% for testing
training <- r_cyanobac_frame[trainIndex, ]  # creating the training data set
test <- r_cyanobac_frame[-trainIndex, ]  # creating the testing data set

# Finding out if there's correlated variables in the data set 
frame_cor <- cor(r_cyanobac_frame)
findCorrelation(frame_cor, verbose = TRUE)
# We get the value "0" out of it, which means there are no highly correlated variables 

# Realizing the imputation:
# For the training set 
pre_knn <- preProcess(training, method = "knnImpute", k = round(sqrt(nrow(training))), verbose = T)
pre_bag <- preProcess(training, method = c("center", "scale", "bagImpute"), verbose = T)
pre_median <- preProcess(training, method = c("center", "scale", "medianImpute"), verbose = T)
#for the testing set: 
test_pre_knn <- preProcess(test, method = "knnImpute", k = round(sqrt(nrow(training))), verbose = T)
test_pre_bag <- preProcess(test, method = c("center", "scale", "bagImpute"), verbose = T)
test_pre_median <- preProcess(test, method = c("center", "scale", "medianImpute"), verbose = T)

# Applying it to the data: 
# For the training set
imputed_knn <- predict(pre_knn, newdata = training)  # Warning: kNN automatically centers and scales the data. Means that following formula is applied to it: (X - mean(column) / sd(column))
imputed_bag <- predict(pre_bag, newdata = training)
imputed_median <- predict(pre_median, newdata = training)
# For the testing set
test_imputed_knn <- predict(test_pre_knn, newdata = test)  # Warning: kNN automatically centers and scales the data. Means that following formula is applied to it: (X - mean(column) / sd(column))
test_imputed_bag <- predict(test_pre_bag, newdata = test)
test_imputed_median <- predict(test_pre_median, newdata = test)

# Evaluating algorithmically which variables contain low information, and that could thus be removed (pre-modelling):
subsets <- c(1:9)  # specifying the amount of variables I want to have in each test (here, the computed will be conducting tests with all the possible amounts of variables) 
# for the kNN-imputed data:
rfeCtrl <- rfeControl(functions = rfFuncs, method = "cv", verbose = FALSE)  # the first arguments allows to specify which method we want to use
system.time(rfProfile_knn <- rfe(x = imputed_knn[, -1], y = imputed_knn$r_cyanobac, sizes = subsets, rfeControl = rfeCtrl))  # this code always takes a bit of time, which is totally normal
# for the bag-imputed data:
system.time(rfProfile_bag <- rfe(x = imputed_bag[, -1], y = imputed_bag$r_cyanobac, sizes = subsets, rfeControl = rfeCtrl))  # this code always takes a bit of time, which is totally normal
# for the median-imputed data:
system.time(rfProfile_median <- rfe(x = imputed_median[, -1], y = imputed_median$r_cyanobac, sizes = subsets, rfeControl = rfeCtrl))  # this code always takes a bit of time, which is totally normal

# Displaying the results:
rfProfile_knn 
rfProfile_bag 
rfProfile_median  # tot_zoo, NO3 and Temperature seem to be important predictors

tg <- data.frame(mtry = c(1:9))  # set the tuneGrid argument to be the same for all models 
fitControl <- trainControl(method = "cv", number = 10)  # set the cross-validation control to be the same for all models

# Running the actual Random Forest models
set.seed(123)
system.time(rf_knn <- train(r_cyanobac ~., data = imputed_knn, method = "rf", importance = TRUE, tuneGrid = tg, trControl = fitControl))
system.time(rf_bag <- train(r_cyanobac ~., data = imputed_bag, method = "rf", importance = TRUE, tuneGrid = tg, trControl = fitControl))
system.time(rf_median <- train(r_cyanobac ~., data = imputed_median, method = "rf", importance = TRUE, tuneGrid = tg, trControl = fitControl))

# Displaying their results
rf_knn
rf_bag
rf_median

# The random forest with bagging imputation tends to have the best results
# Unexpected: mrty = 1

# Analyzing variable importance for each model (post-modelling):
varImp_knn <- varImp(rf_knn)
knnPlot <- plot(varImp_knn, main = "Variable importance (Random Forest, kNN imputation)")

varImp_bag <- varImp(rf_bag)
bagPlot <- plot(varImp_bag, main = "Variable importance (Random Forest, bagging imputation)")

varImp_median <- varImp(rf_median)
medianPlot <- plot(varImp_median, main = "Variable importance (Random Forest, median imputation")

print(impPlots <- ggarrange(knnPlot, bagPlot,medianPlot,
                            ncol = 1, nrow = 3, align = "hv"))
# tot_zoo, NO3 and temperature seem to be important predictors

# Testing the model:
# First, using the model on the testing data to create a test prediction
test_knn <- predict(rf_knn, newdata = test_imputed_knn)
test_bag <- predict(rf_bag, newdata = test_imputed_bag)
test_median <- predict(rf_median, newdata = test_imputed_median)

# Then evaluate how precise this test prediction was
defaultSummary(data = data.frame(obs = test$r_cyanobac, pred = test_knn))
defaultSummary(data = data.frame(obs = test$r_cyanobac, pred = test_bag))
defaultSummary(data = data.frame(obs = test$r_cyanobac, pred = test_median))  # Generally, the model with the median imputation has the best results (lowest RMSE)



# 10.2) Random Forest on d_cyanobac ####
#' 
#' Choosing d_cyanobac as the response variable and creating the models for this variable
#' Most of the code is the same as for r_cyanobac
#' 
featurePlot(x = end_frame[, c("r_cyanobac", "NH4_N", "NO3_N", "temperature")], y = end_frame$d_cyanobac, plot = "scatter", labels = c("", "ratio cyanobac - tot_phyto"))

d_cyanobac_frame <- end_frame %>% select(d_cyanobac, r_cyanobac, NH4_N:calanoida)  

# Partitioning the data
set.seed(123) # doesn't need to be 123, but having a value here makes the test reproducible
trainIndex <- createDataPartition(d_cyanobac_frame$d_cyanobac, p = 0.7, list = FALSE)  # Choosing which indices of the data set will be for training, and which ones for testing. 70% are for training, 30% for testing (p = 0.7)
training <- d_cyanobac_frame[trainIndex, ]  # creating the training data set
test <- d_cyanobac_frame[-trainIndex, ]  # creating the testing data set

# Finding out if there's correlated variables in the data set 
frame_cor <- cor(d_cyanobac_frame)
findCorrelation(frame_cor, verbose = TRUE)
# We get the value "0" out of it, which means there are no highly correlated variables 

# Realizing the imputation:
# For the training set 
pre_knn <- preProcess(training, method = "knnImpute", k = round(sqrt(nrow(training))), verbose = T)
pre_bag <- preProcess(training, method = c("center", "scale", "bagImpute"), verbose = T)
pre_median <- preProcess(training, method = c("center", "scale", "medianImpute"), verbose = T)
#for the testing set: 
test_pre_knn <- preProcess(test, method = "knnImpute", k = round(sqrt(nrow(training))), verbose = T)
test_pre_bag <- preProcess(test, method = c("center", "scale", "bagImpute"), verbose = T)
test_pre_median <- preProcess(test, method = c("center", "scale", "medianImpute"), verbose = T)

# Applying it to the data: 
# For the training set
imputed_knn <- predict(pre_knn, newdata = training)  # Warning: kNN automatically centers and scales the data. Means that following formula is applied to it: (X - mean(column) / sd(column))
imputed_bag <- predict(pre_bag, newdata = training)
imputed_median <- predict(pre_median, newdata = training)
# For the testing set
test_imputed_knn <- predict(test_pre_knn, newdata = test)  # Warning: kNN automatically centers and scales the data. Means that following formula is applied to it: (X - mean(column) / sd(column))
test_imputed_bag <- predict(test_pre_bag, newdata = test)
test_imputed_median <- predict(test_pre_median, newdata = test)

# Evaluating algorithmically which variables contain low information, and that could thus be removed (pre-modelling):
subsets <- c(1:10)  # specifying the amount of variables I want to have in each test (here, the computed will be conducting tests with all the possible amounts of variables) 
# for the kNN-imputed data:
rfeCtrl <- rfeControl(functions = rfFuncs, method = "cv", verbose = FALSE)  # the first arguments allows to specify which method we want to use
system.time(rfProfile_knn <- rfe(x = imputed_knn[, -1], y = imputed_knn$d_cyanobac, sizes = subsets, rfeControl = rfeCtrl))  # this code always takes a bit of time, which is totally normal
# for the bag-imputed data:
system.time(rfProfile_bag <- rfe(x = imputed_bag[, -1], y = imputed_bag$d_cyanobac, sizes = subsets, rfeControl = rfeCtrl))  # this code always takes a bit of time, which is totally normal
# for the median-imputed data:
system.time(rfProfile_median <- rfe(x = imputed_median[, -1], y = imputed_median$d_cyanobac, sizes = subsets, rfeControl = rfeCtrl))  # this code always takes a bit of time, which is totally normal

# Displaying the results:
rfProfile_knn 
rfProfile_bag 
rfProfile_median 
# r_cyanobac, daphniidae, tot_zoo and temperature seem to be very important predictors

tg <- data.frame(mtry = c(1:9))  # set the tuneGrid argument to be the same for all models 
fitControl <- trainControl(method = "cv", number = 10)  # set the cross-validation control to be the same for all models

# Running the actual Random Forest models
set.seed(123)
system.time(rf_knn <- train(d_cyanobac ~., data = imputed_knn, method = "rf", importance = TRUE, tuneGrid = tg, trControl = fitControl))
system.time(rf_bag <- train(d_cyanobac ~., data = imputed_bag, method = "rf", importance = TRUE, tuneGrid = tg, trControl = fitControl))
system.time(rf_median <- train(d_cyanobac ~., data = imputed_median, method = "rf", importance = TRUE, tuneGrid = tg, trControl = fitControl))

# Displaying their results
rf_knn
rf_bag
rf_median

# The random forest with kNN imputation tends to have the best results

# Analyzing variable importance for each model (post-modelling):
varImp_knn <- varImp(rf_knn)
knnPlot <- plot(varImp_knn, main = "Variable importance (Random Forest, kNN imputation)")

varImp_bag <- varImp(rf_bag)
bagPlot <- plot(varImp_bag, main = "Variable importance (Random Forest, bagging imputation)")

varImp_median <- varImp(rf_median)
medianPlot <- plot(varImp_median, main = "Variable importance (Random Forest, median imputation")

print(impPlots <- ggarrange(knnPlot, bagPlot,medianPlot,
                            ncol = 1, nrow = 3, align = "hv"))

# Testing the model:
# First, using the model on the testing data to create a test prediction
test_knn <- predict(rf_knn, newdata = test_imputed_knn)
test_bag <- predict(rf_bag, newdata = test_imputed_bag)
test_median <- predict(rf_median, newdata = test_imputed_median)

# Then evaluate how precise this test prediction was
defaultSummary(data = data.frame(obs = test$r_cyanobac, pred = test_knn))  # Generally, the model with the kNN imputation has the best results (lowest RMSE)
defaultSummary(data = data.frame(obs = test$r_cyanobac, pred = test_bag)) 
defaultSummary(data = data.frame(obs = test$r_cyanobac, pred = test_median))  
