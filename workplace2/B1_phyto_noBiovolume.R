# This script compiles all the taxa that don't have a biovolume 
# All the data handled in this script doesn't have a biovolume
# This script only handles PHYTOPLANKTON data


library(dplyr)
library(ggplot2)

# 1) Phyto: Importing all the data ####
{
  # loading the meta database
  setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new")  # it's in another folder
  meta_db_phyto <- read.delim("meta_db_phyto_2020.csv", sep = ";", dec = ",")
  setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")
  
  
  # BAL data
  datapoints_BAL <- read.csv("BAL_phyto_datapoints_without_biovol.csv")
  datapoints_BAL$date <- as.Date(datapoints_BAL$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_BAL$id_CH <- factor(datapoints_BAL$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_BAL <- read.csv("BAL_phyto_taxa_without_biovol.csv", sep = ",")
 
  
  # GRE data
  datapoints_GRE <- read.csv("GRE_phyto_datapoints_without_biovol.csv")
  datapoints_GRE$date <- as.Date(datapoints_GRE$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_GRE$id_CH <- factor(datapoints_GRE$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_GRE <- read.csv("GRE_phyto_taxa_without_biovol.csv", sep = ",")
  
  
  # HAL data
  datapoints_HAL <- read.csv("HAL_phyto_datapoints_without_biovol.csv")
  datapoints_HAL$date <- as.Date(datapoints_HAL$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_HAL$id_CH <- factor(datapoints_HAL$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_HAL <- read.csv("HAL_phyto_taxa_without_biovol.csv", sep = ",") 
  
  
  # SEM data
  datapoints_SEM <- read.csv("SEM_phyto_datapoints_without_biovol.csv")
  datapoints_SEM$date <- as.Date(datapoints_SEM$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_SEM$id_CH <- factor(datapoints_SEM$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_SEM <- read.csv("SEM_phyto_taxa_without_biovol.csv", sep = ",")
  
  
  # ZHR data
  datapoints_ZHR <- read.csv("ZHR_phyto_datapoints_without_biovol.csv")
  datapoints_ZHR$date <- as.Date(datapoints_ZHR$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  datapoints_ZHR$id_CH <- factor(datapoints_ZHR$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_ZHR <- read.csv("ZHR_phyto_taxa_without_biovol.csv", sep = ",")
  
}  # I often have to reload the whole data, and it's easier and faster if I write it in brackets



# 2) Phyto: Aggregating all the data about the taxa ####

taxa_all <- rbind(taxa_BAL, taxa_GRE, taxa_HAL, taxa_SEM, taxa_ZHR)  # putting all the frames together in one

taxa_all <- taxa_all %>% select(-c(X, mn_abundance))  # removing the columns for X and the mn_abundance (both useless at this point)
taxa_all <- taxa_all %>% arrange(desc(occurrences))

# The next lines aggregate the taxa frame, and add the rest of the data back (can't do it in a single line, because R can't aggregate "NA's" together)
taxa_aggregated <- aggregate(list(taxa_all$abundance, taxa_all$occurrences), by= list(taxa_all$id_CH), sum)
names(taxa_aggregated) <- c("id_CH", "abundance", "occurrences")
taxa_aggregated <- inner_join(taxa_aggregated, meta_db_phyto %>% select(id_CH, order, genus, species, volume_um.3, mdn_nu.biovol, volume_um.3.1), by="id_CH")  # putting back the data that can't be kept throughout the aggregating process (R can't aggregate by NA's)
taxa_aggregated$id_CH <- factor(taxa_aggregated$id_CH)

# Ways of sorting the data
taxa_aggregated <- taxa_aggregated %>% arrange(desc(occurrences)) 
# taxa_aggregated <- taxa_aggregated %>% arrange(id_CH)  # Sorting the data by id_CH
# taxa_aggregated <- taxa_aggregated %>% arrange(abundance)

# the taxa we keep / exclude
taxa_excl <- taxa_aggregated %>% filter(id_CH %in% c(1, 2, 4, 5, 7, 10, 13, 15, 50))  # these are the taxa we exclude; mostly bacteria
taxa_kept <- taxa_aggregated %>% filter(id_CH %in% c(209, 214, 388, 395, 397))  # these are the taxa we chose to keep

# exporting the aggregated data about the taxa without a biovolume
setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
write.csv(taxa_aggregated, "phyto_taxa_without_biovolume.csv")
setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")


# 3) Phyto: Taking a look at the abundances and their distribution ####
admb <- rbind(datapoints_BAL, datapoints_GRE, datapoints_HAL, datapoints_SEM, datapoints_ZHR)  # admb stands for "all datapoints missing biovolume"

nrow(admb)  # a total of 1470 occurrences
nrow(taxa_aggregated)  # representing 20 "taxa" (some are also families or even orders)
admb$lake <- factor(admb$lake)  # "lake" is now a factor
admb$id_CH <- factor(admb$id_CH) # id_CH is also a factor
admb <- admb %>% arrange(id_CH)
admb <- admb %>% select(-c(X, unit, depth, kremer_key, mn_cells.per.nu, species_buergi))  # eliminating useless columns

# working on the distribution over time for the lakes, for each lake, and for multiple scales

relevant <- taxa_aggregated %>% filter(occurrences > 30)  #killing taxa observed less than 30 times
relevant <- relevant %>% arrange(id_CH)  # sorting the data by id_CH
toPlot <- admb %>% filter(id_CH %in% relevant$id_CH)  # selecting the taxa having enough occurrences

plotCol <- c("1" = "blue", "2" = "aquamarine4", "15" = "chartreuse", "209" = "darkgoldenrod1", "388" = "darkorchid1", "397" = "cornflowerblue", "2812" = "firebrick1")  # One colour per id_CH present in the data

#' plotting all the pdf and putting them all into a single PDF
#' There are plots for all the lakes together, and plots for each lake on their own
setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
pdf("phyto_noBiovol_all_plots.pdf")  
# Global, for all the lakes together
ggplot(data = toPlot, aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH), shape = 20) + 
    scale_colour_manual(values = plotCol) + 
    ggtitle(label = paste("Abundance over the years, all lakes, log-scaled")) + 
    theme(plot.title = element_text(size=20)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    scale_y_continuous(trans = "log10")
for (j in c(2:4)) {
    plot <- ggplot(data = toPlot, aes(x = date, y = abundance)) + 
      geom_point(aes(colour = id_CH), shape = 20) + 
      scale_colour_manual(values = plotCol) + 
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
for (j in c(3:6)) {
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
for (j in c(3:7)) {
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
for (j in c(3:6)) {
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
  geom_point(aes(colour = id_CH), shape = 20) + 
  scale_colour_manual(values = plotCol) + 
  ggtitle(label = paste("Abundance over the years, ZHR, log-scaled")) + 
  theme(plot.title = element_text(size=20)) + 
  labs(x = "Date", y = "Abundance (indv./L)") + 
  scale_y_continuous(trans = "log10")
for (j in c(2:4)) {
  plot <- ggplot(data = toPlot %>% filter(lake == "ZHR"), aes(x = date, y = abundance)) + 
    geom_point(aes(colour = id_CH), shape = 20) + 
    scale_colour_manual(values = plotCol) + 
    ggtitle(label = paste("Abundance over the years, ZHR, abundance <=10^", j)) + 
    labs(x = "Date", y = "Abundance (indv./L)") + 
    ylim(0, 10**j)
  
  print(plot)
}


dev.off()



# ####
















# 4) Zoo: Importing all the data ####
{
  # loading the meta database
  setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new")  # it's in a different folder than the noBiovolZ data
  meta_db_zoo <- read.delim("metaDBzoo_2020.csv", sep = ",", dec = ".")
  setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")
  
  
  # BAL data
  zoo_BAL <- read.csv("BAL_zoo_datapoints_without_biovol.csv")
  zoo_BAL$date <- as.Date(zoo_BAL$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  zoo_BAL$id_CH <- factor(zoo_BAL$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_z_BAL <- read.csv("BAL_zoo_taxa_without_biovol.csv", sep = ",")
  
  
  # GRE data
  zoo_GRE <- read.csv("GRE_zoo_datapoints_without_biovol.csv")
  zoo_GRE$date <- as.Date(zoo_GRE$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  zoo_GRE$id_CH <- factor(zoo_GRE$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_z_GRE <- read.csv("GRE_zoo_taxa_without_biovol.csv", sep = ",")
  
  
  # HAL data
  zoo_HAL <- read.csv("HAL_zoo_datapoints_without_biovol.csv")
  zoo_HAL$date <- as.Date(zoo_HAL$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  zoo_HAL$id_CH <- factor(zoo_HAL$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_z_HAL <- read.csv("HAL_zoo_taxa_without_biovol.csv", sep = ",") 
  
  
  # SEM data
  zoo_SEM <- read.csv("SEM_zoo_datapoints_without_biovol.csv")
  zoo_SEM$date <- as.Date(zoo_SEM$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  zoo_SEM$id_CH <- factor(zoo_SEM$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_z_SEM <- read.csv("SEM_zoo_taxa_without_biovol.csv", sep = ",")
  
  
  # ZHR data
  zoo_ZHR <- read.csv("ZHR_zoo_datapoints_without_biovol.csv")
  zoo_ZHR$date <- as.Date(zoo_ZHR$date, "%Y-%m-%d")  # changing the class of the "date" column to a date format
  zoo_ZHR$id_CH <- factor(zoo_ZHR$id_CH)  # set the id_CH as a factor for the plots
  
  taxa_z_ZHR <- read.csv("ZHR_zoo_taxa_without_biovol.csv", sep = ",")
  
  } # I often have to reload the whole data, and it's easier and faster if I write it in brackets
# 5) Zoo: Aggregating all the data about the taxa ####
# TODO 
#' Put the data for all lakes together in one frame and plot the whole
see_all <- inner_join(taxa_noBiovol, meta_db_zoo %>% select(id_CH, order, stage, affiliation, volume_um.3, guild), by= "id_CH")
# 6) Zoo: Taking a look at the abundances and their distribution ####