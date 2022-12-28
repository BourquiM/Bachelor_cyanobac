# This script compiles all the taxa that don't have a biovolume 
# All the data handled in this script doesn't have a biovolume
# This script only handles ZOOPLANKTON data


library(dplyr)
library(ggplot2)

# 1) Zoo: Importing all the data ####
{
  # loading the meta database
  setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new")  # it's in a different folder than the noBiovolZ data
  meta_db_zoo <- read.delim("metaDBzoo_2020.csv", sep = ",", dec = ".")
  setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")
  
  
  
  # BAL data
  datapoints_BAL <- read.csv("BAL_zoo_datapoints_without_biovol.csv")
  datapoints_BAL$date <- as.Date(datapoints_BAL$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_BAL$id_CH <- factor(datapoints_BAL$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_BAL <- read.csv("BAL_zoo_taxa_without_biovol.csv", sep = ",")
  
  
  # GRE data
  datapoints_GRE <- read.csv("GRE_zoo_datapoints_without_biovol.csv")
  datapoints_GRE$date <- as.Date(datapoints_GRE$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_GRE$id_CH <- factor(datapoints_GRE$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_GRE <- read.csv("GRE_zoo_taxa_without_biovol.csv", sep = ",")
  
  
  # HAL data
  datapoints_HAL <- read.csv("HAL_zoo_datapoints_without_biovol.csv")
  datapoints_HAL$date <- as.Date(datapoints_HAL$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_HAL$id_CH <- factor(datapoints_HAL$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_HAL <- read.csv("HAL_zoo_taxa_without_biovol.csv", sep = ",") 
  
  
  # SEM data
  datapoints_SEM <- read.csv("SEM_zoo_datapoints_without_biovol.csv")
  datapoints_SEM$date <- as.Date(datapoints_SEM$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_SEM$id_CH <- factor(datapoints_SEM$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_SEM <- read.csv("SEM_zoo_taxa_without_biovol.csv", sep = ",")
  
  
  # ZHR data
  datapoints_ZHR <- read.csv("ZHR_zoo_datapoints_without_biovol.csv")
  datapoints_ZHR$date <- as.Date(datapoints_ZHR$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_ZHR$id_CH <- factor(datapoints_ZHR$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_ZHR <- read.csv("ZHR_zoo_taxa_without_biovol.csv", sep = ",")
  
}  # I often have to reload the whole data, and it's easier and faster if I write it in brackets



# 2) Zoo: Aggregating all the data about the taxa ####

taxa_all <- rbind(taxa_BAL, taxa_GRE, taxa_HAL, taxa_SEM, taxa_ZHR)  # putting all the frames together in one

taxa_all <- taxa_all %>% select(-c(X))  # removing the columns for X (useless at this point)
taxa_all <- taxa_all %>% arrange(desc(occurrences))

# The next lines aggregate the taxa frame, and add the rest of the data back (can't do it in a single line, because R can't aggregate "NA's" together)
taxa_aggregated <- aggregate(list(taxa_all$abundance, taxa_all$occurrences), by= list(taxa_all$id_CH), sum)
names(taxa_aggregated) <- c("id_CH", "abundance", "occurrences")
taxa_aggregated <- inner_join(taxa_aggregated, meta_db_zoo %>% select(id_CH, guild, order, genus, species, stage, volume_um.3), by="id_CH")  # putting back the data that can't be kept throughout the aggregating process (R can't aggregate by NA's)
taxa_aggregated$id_CH <- factor(taxa_aggregated$id_CH)

# Ways of sorting the data
taxa_aggregated <- taxa_aggregated %>% arrange(desc(occurrences)) 
# taxa_aggregated <- taxa_aggregated %>% arrange(id_CH)  # Sorting the data by id_CH
# taxa_aggregated <- taxa_aggregated %>% arrange(abundance)

# the taxa we keep / exclude
# still needs to be clarified for zoo, which one we keep and which ones we ditch
# taxa_excl <- taxa_aggregated %>% filter(id_CH %in% c())  # these are the taxa we exclude; mostly bacteria
# taxa_kept <- taxa_aggregated %>% filter(id_CH %in% c())  # these are the taxa we chose to keep

# exporting the aggregated data about the taxa without a biovolume
setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
write.csv(taxa_aggregated, "zoo_taxa_without_biovolume.csv")
setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")


# 3) Zoo: Taking a look at the abundances and their distribution ####
admb <- rbind(datapoints_BAL, datapoints_GRE, datapoints_HAL, datapoints_SEM, datapoints_ZHR)  # admb stands for "all datapoints missing biovolume"

nrow(admb)  # a total of 5424 occurrences
nrow(taxa_aggregated)  # representing 59 "taxa" (some are also families or even orders)
admb$lake <- factor(admb$lake)  # "lake" is now a factor
admb$id_CH <- factor(admb$id_CH) # id_CH is also a factor
admb$order <- factor(admb$order)
admb <- admb %>% arrange(id_CH)
admb <- admb %>% select(-c(X, unit, depth, occurrences))  # eliminating useless columns

# working on the distribution over time for the lakes, for each lake, and for multiple scales
relevant <- taxa_aggregated %>% filter(occurrences > 30)  #killing taxa observed less than 30 times, 24 are left
relevant <- relevant %>% arrange(id_CH)  # sorting them by id_CH
toPlot <- admb %>% filter(id_CH %in% relevant$id_CH)  # selecting the taxa having enough occurrences
toPlot <- toPlot %>% arrange(id_CH)

# 
plotCol <- c("6559" = "blue", "6586" = "aquamarine4", "6592" = "chartreuse", "6593" = "aquamarine", "6594" = "darkorchid1", "6916" = "darkgoldenrod1", "7031" = "aquamarine", "7032" = "darkgoldenrod1", "7033" = "darkorchid1", "7040" = "cornflowerblue", "7042" = "red", "7057" = "blue", "7059" = "cornflowerblue", "7060" = "aquamarine4", "7061" = "chartreuse", "7062" = "aquamarine", "7063" = "darkorchid1", "7064" = "darkgoldenrod1", "7067" = "cornflowerblue", "8000" = "red", "8004" = "blue", "9003" = "red", "9012" = "aquamarine4", "9014" = "chartreuse")

plotShape <- c("6559" = 20, "6586" = 20, "6592" = 20, "6593" = 6, "6594" = 6, "6916" = 20, "7031" = 20, "7032" = 6, "7033" = 20, "7040" = 6, "7042" = 6, "7057" = 6, "7059" = 20, "7060" = 6, "7061" = 6, "7062" = 1, "7063" = 1, "7064" = 1, "7067" = 1, "8000" = 1, "8004" = 1, "9003" = 20, "9012" = 1, "9014" = 1)
# plotShape <- c("Diplostraca" = 1, "Calanoida" = 4, "Cyclopoida" = 20, "Acari" = 5, "Diptera" = 6)

setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
pdf("zoo_nobiovol_all_lakes_per_order.pdf")
for (o in unlist(relevant %>% distinct(order))) {
  plot <- ggplot(data = toPlot %>% filter(order == o), aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH, shape = id_CH)) + 
    scale_colour_manual(values = plotCol) + 
    scale_shape_manual(values = plotShape) +
    ggtitle(label = paste("Abundance over time, all lakes, log-scaled,", o)) + 
    theme(plot.title = element_text(size=18)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    scale_y_continuous(trans = "log10")
  
  print(plot)
}
dev.off()
setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")



#' plotting all the pdf and putting them all into a single PDF
#' There are plots for all the lakes together, and plots for each lake on their own
setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
pdf("zoo_noBiovol_all_plots.pdf")  
# Global, for all the lakes together
ggplot(data = toPlot, aes(x = date, y = abundance)) + 
  geom_point(aes(colour = id_CH, shape = id_CH)) + 
  scale_colour_manual(values = plotCol) + 
  scale_shape_manual(values = plotShape) +
  ggtitle(label = paste("Abundance over the years, all lakes, log-scaled")) + 
  theme(plot.title = element_text(size=20)) + 
  labs(x = "Date", y = "Abundance (indv./L)") + 
  scale_y_continuous(trans = "log10")
for (j in c(2:4)) {
  plot <- ggplot(data = toPlot, aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH, shape = id_CH)) + 
    scale_colour_manual(values = plotCol) + 
    scale_shape_manual(values = plotShape) +
    ggtitle(label = paste("Abundance over the years, all lakes, abundance <=10^", j)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    ylim(0, 10**j)
  
  print(plot)
}

# Lake BAL
ggplot(data = toPlot %>% filter(lake == "BAL"), aes(x = date, y = abundance)) + 
  geom_point(aes(colour = id_CH), shape = 20) + 
  scale_colour_manual(values = plotCol) + 
  ggtitle(label = paste("Abundance over the years, BAL, log-scaled")) + 
  theme(plot.title = element_text(size=20)) + 
  labs(x = "Date", y = "Abundance (indv./L)") + 
  scale_y_continuous(trans = "log10")
for (j in c(3:6)) {
  plot <- ggplot(data = toPlot %>% filter(lake == "BAL"), aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH), shape = 20) + 
    scale_colour_manual(values = plotCol) + 
    ggtitle(label = paste("Abundance over the years, BAL, abundance <=10^", j)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    ylim(0, 10**j)
  
  print(plot)
}


# Lake GRE
ggplot(data = toPlot %>% filter(lake == "GRE"), aes(x = date, y = abundance)) + 
  geom_point(aes(colour = id_CH), shape = 20) + 
  scale_colour_manual(values = plotCol) + 
  ggtitle(label = paste("Abundance over the years, GRE, log-scaled")) + 
  theme(plot.title = element_text(size=20)) + 
  labs(x = "Date", y = "Abundance (indv./L)") + 
  scale_y_continuous(trans = "log10")
for (j in c(3:7)) {
  plot <- ggplot(data = toPlot %>% filter(lake == "GRE"), aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH), shape = 20) + 
    scale_colour_manual(values = plotCol) + 
    ggtitle(label = paste("Abundance over the years, GRE, abundance <=10^", j)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    ylim(0, 10**j)
  
  print(plot)
}


# Lake HAL
ggplot(data = toPlot %>% filter(lake == "HAL"), aes(x = date, y = abundance)) + 
  geom_point(aes(colour = id_CH), shape = 20) + 
  scale_colour_manual(values = plotCol) + 
  ggtitle(label = paste("Abundance over the years, HAL, log-scaled")) + 
  theme(plot.title = element_text(size=20)) + 
  labs(x = "Date", y = "Abundance (indv./L)") + 
  scale_y_continuous(trans = "log10")
for (j in c(3:5)) {
  plot <- ggplot(data = toPlot %>% filter(lake == "HAL"), aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH), shape = 20) + 
    scale_colour_manual(values = plotCol) + 
    ggtitle(label = paste("Abundance over the years, HAL, abundance <=10^", j)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    ylim(0, 10**j)
  
  print(plot)
}


# Lake SEM
ggplot(data = toPlot %>% filter(lake == "SEM"), aes(x = date, y = abundance)) + 
  geom_point(aes(colour = id_CH), shape = 20) + 
  scale_colour_manual(values = plotCol) + 
  ggtitle(label = paste("Abundance over the years, SEM, log-scaled")) + 
  theme(plot.title = element_text(size=20)) + 
  labs(x = "Date", y = "Abundance (indv./L)") + 
  scale_y_continuous(trans = "log10")
for (j in c(3:5)) {
  plot <- ggplot(data = toPlot %>% filter(lake == "SEM"), aes(x = date, y = abundance)) +
    geom_point(aes(colour = id_CH), shape = 20) +
    scale_colour_manual(values = plotCol) +
    ggtitle(label = paste("Abundance over the years, SEM, abundance <=10^", j)) +
    labs(x = "Date", y = "Abundance (indv./L)") +
    ylim(0, 10**j)

  print(plot)
}


# Lake ZHR 
ggplot(data = toPlot %>% filter(lake == "ZHR"), aes(x = date, y = abundance)) + 
  geom_point(aes(colour = id_CH, shape = id_CH)) + 
  scale_colour_manual(values = plotCol) + 
  scale_shape_manual(values = plotShape) +
  ggtitle(label = paste("Abundance over the years, ZHR, log-scaled")) + 
  theme(plot.title = element_text(size=20)) + 
  labs(x = "Date", y = "Abundance (indv./L)") + 
  scale_y_continuous(trans = "log10")
for (j in c(2:6)) {
  plot <- ggplot(data = toPlot %>% filter(lake == "ZHR"), aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH, shape = id_CH)) + 
    scale_colour_manual(values = plotCol) + 
    scale_shape_manual(values = plotShape) +
    ggtitle(label = paste("Abundance over the years, ZHR, abundance <=10^", j)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    ylim(0, 10**j)
  
  print(plot)
}


dev.off()

setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")

