setwd("~/GitHub/Bachelor_cyanobac/workplace2/Code")

source("A0_functions.R")  # The functions needed to run the code are stored in a separate file. This command imports the functions.

# Loading the necessary libraries
load_libraries()

# importing the data from all lakes
setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new")
databases()
bal <- read.csv("processed_BAL.csv")
gre <- read.csv("processed_GRE.csv")
hal <- read.csv("processed_HAL.csv")
sem <- read.csv("processed_SEM.csv")
zhr <- read.csv("processed_ZHR.csv")

# Putting all this data into a single frame
df <- rbind(bal, gre, hal, sem, zhr)
df <- df %>% select(-c(X))  # Removing the columns for index


# Random Forests for all lakes: 
