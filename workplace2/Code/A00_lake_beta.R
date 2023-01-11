#' 
#' This file works with the data of the BAL lake as an example


setwd("~/GitHub/Bachelor_cyanobac/workplace2/Code")

source("A0_functions.R")  # The functions needed to run the code are stored in a separate file. This command imports the functions.

# Loading the necessary libraries
load_libraries()


# loading the data files
setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new")
databases()  # loading the databases metaDBphyto and metaDBzoo
phyto_raw <- read.csv("phyto.smallzoo.86-19_BAL.csv", sep = ",")  # loading the raw phytoplankton data
zoo_raw <- read.csv("largezoo.86-19_BAL.csv")  # loading the raw zooplankton data

setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new/temp_chem")  # loading the raw temperature and chemistry data
temp_chem <- read.csv("temp.chem.85-20_BAL.csv")

lake <- "BAL"  # Defining the name of the lake. This will be used when saving data in external files, to identify them later on

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
# Yep, but I need to look at the scatterplots again if we think it's important to analyze the linear relationship between the parameters




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

summary(end_frame)
end_frame <- end_frame %>% select(-c(date, depth))  #removing the date and the depth from the DF - we don't need those anymore

# Working with the r_cyanobac as a response variable
# Would make sense to create a function creating the 3D graphs he showed me 
featurePlot(x = end_frame[, c("tot_phyto", "NH4_N", "NO3_N", "temperature")], y = end_frame$r_cyanobac, plot = "scatter", labels = c("", "ratio cyanobac - tot_phyto"))

r_cyanobac_frame <- end_frame %>% select(r_cyanobac, NH4_N:calanoida)  

# Partitioning the data
set.seed(123) # doesn't need to be 123, but having a value here makes the test reproducible
trainIndex <- createDataPartition(r_cyanobac_frame$r_cyanobac, p = 0.7, list = FALSE)  # Choosing which indices of the data set will be for training, and which ones for testing. 70% are for training, 30% for testing
training <- r_cyanobac_frame[trainIndex, ]  # creating the training data set
test <- r_cyanobac_frame[-trainIndex, ]  # creating the testing data set

# Realizing the imputation 
pre_knn <- preProcess(training, method = "knnImpute", k = round(sqrt(nrow(training))), verbose = T)
pre_bag <- preProcess(training, method = "bagImpute")
pre_median <- preProcess(training, method = "medianImpute")

# Applying it to the data
imputed_knn <- predict(pre_knn, newdata = training)  # Warning: this automatically centers and scales the data. Means that following formula is applied to it: (X - mean(column) / sd(column))
imputed_bag <- predict(pre_bag, newdata = training)
imputed_median <- predict(pre_median, newdata = training)

# Ce que je veux faire: faire les modèles avec tous ces sets de données, les comparer, tester les modèles.


system.time(rf_knn <- train(r_cyanobac ~., data = trainTransformed, method = "rf", importance = TRUE, trControl = trainControl(method = "cv", number = 10)))
rf_knn


preProcValues <- preProcess(x = training, method = c("center", "scale" ,"medianImpute"))  # Creating the model that will transform the data
trainTransformed <- predict(preProcValues, newdata = training)  # transformation of the training data set into Z-scores, using the model we just created
# This one has 10 fold cross-validation and importance ranking of the variables
system.time(rf_r_cyanobac <- train(r_cyanobac ~., data = trainTransformed, method = "rf", importance = TRUE, trControl = trainControl(method = "cv", number = 10)))
rf_r_cyanobac


preProcValues <- preProcess(x = training, method = c("bagImpute"))  # Creating the model that will transform the data
trainTransformed <- predict(preProcValues, newdata = training)  # transformation of the training data set into Z-scores, using the model we just created
system.time(rf_r_cyanobac <- train(r_cyanobac ~., data = trainTransformed, method = "rf", importance = TRUE, trControl = trainControl(method = "cv", number = 10)))
rf_r_cyanobac


preProcValues <- preProcess(x = training, method = c("medianImpute"))  # Creating the model that will transform the data
trainTransformed <- predict(preProcValues, newdata = training)  # transformation of the training data set into Z-scores, using the model we just created
# This one has 10 fold cross-validation and importance ranking of the variables
system.time(rf_r_cyanobac <- train(r_cyanobac ~., data = training, method = "rf", na.action = na.omit, importance = TRUE, trControl = trainControl(method = "cv", number = 10)))
rf_r_cyanobac





 


system.time(rf_r_cyanobac1 <- train(r_cyanobac ~., data = trainTransformed, method = "rf", importance = TRUE, trControl = trainControl(method = "cv", number = 10)))

system.time(rf_r_cyanobac1 <- train(r_cyanobac ~ NH4_N + NO3_N + PO4_P + temperature, data = trainy, method = "rf", importance = TRUE, trControl = trainControl(method = "cv", number = 10), num.trees = 999, importance = "permutation"))

testTransformed <- predict(preProcValues, newdata = test)  # transformation of the testing data set into Z-scores, using the model we just created

