load_libraries <- function(){
  library(dplyr)
  library(ggplot2)
  library(ggpubr)
  library(GGally)
  library(caret)
  }



# 0) loading the meta databases ####
databases <- function(){
  #' 
  #' This function imports the meta databases meta_db_phyto and meta_db_zoo and makes the necessary changes, so that they can then be used in the later code
  #' 
  cat("\n-----\n")  # This line only makes the console look clearer when running the functions after another. It draws five "-" and adds new lines
  cat("Importing the meta databases 'meta_db_phyto' and 'meta_db_zoo'\n\n")
  setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new")
  # for Phytoplankton:
  meta_db_phyto <- read.delim("metaDBphyto_2020.csv", sep = ";", dec = ",")
  
  # Cleaning the meta datatable, to make it ready for use
  meta_db_phyto <- meta_db_phyto %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 135, 2.5))  # For unknown reasons, this data point is encoded wrongly in the meta_db_phyto file
  meta_db_phyto <- meta_db_phyto %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 3910, NA))  # This must be due to a misclick, but the value was Na instead of NA
  meta_db_phyto <- meta_db_phyto %>% mutate_at(c("volume_um.3", "volume_um.3.1"), as.numeric) # changing the data type of the columns to "numeric"
  
  meta_db_phyto <<- meta_db_phyto  # making the database global
  cat(sum(is.na(meta_db_phyto$id_CH)), "entries don't have an id_CH in meta_db_phyto \n")  # In this database, there are no taxa without an id_CH
  cat(nrow(no_volP <<- inner_join(subset(meta_db_phyto, is.na(volume_um.3)), subset(meta_db_phyto, is.na(mdn_nu.biovol)), by="id_CH")), "entries don't have a volume_um.3 nor a mdn_nu.biovol in the meta_db_phyto \n\n") # this calculates how many taxa don't have a volume_um.3 nor a mdn_nu.biovol. It's 42 entries in the meta database for Phyto
  
  # For Zooplankton
  meta_db_zoo <<- read.csv("metaDBzoo_2020.csv")
  cat(sum(is.na(meta_db_zoo$id_CH)), "entries don't have an id_CH in meta_db_zoo \n")  # In this database, there are no taxa without an id_CH
  cat(nrow(no_volZ <<- subset(meta_db_zoo, is.na(volume_um.3))), "entries don't have a volume_um.3 in the meta_db_zoo \n\n") # this calculates how many taxa don't have a volume_um.3 . It's 153 entries in the meta database for Zoo
  cat("Meta databases imported successfully\n")
  cat("\n-----\n")
}



# 1) Getting the lake phyto data ready ####
ready_phyto_selec <- function(){
  #' 
  #' This function makes sure the data type of the columns is what it needs to be, and integrates the info from the meta database meta_db_phyto into the data, for calculations later on
  #' 
  cat("\n-----\n")
  cat(sum(is.na(phyto_raw$id_CH)), "data points miss an id_CH in the phyto data\n")  
  phyto_raw$depth <- factor(phyto_raw$depth)  # only one depth
  phyto_raw$unit <- factor(phyto_raw$unit)  # only one unit
  phyto_raw$date <- as.Date(phyto_raw$date, "%Y-%m-%d") # transforming the date to a Date format
  phyto_raw <- phyto_raw %>% filter(!is.na(abundance))  # Removing the data points that don't have an abundance (I don't know why, some data points don't have an abundance. Can't keep them because of the "aggregate" function)
  
  phyto_selec <- inner_join(phyto_raw, (meta_db_phyto %>% select(id_CH, order, genus, species, volume_um.3, kremer_key, mdn_nu.biovol, volume_um.3.1, species_buergi)), by="id_CH")  # Adding the needed info columns to the phyto data table
  
  # Making the data frames global (i.e. accessible outside the function)
  phyto_raw <<- phyto_raw
  phyto_selec <<- phyto_selec
  cat("\n-----\n")
}


# 2) Taking a look at the phyto taxa without a biovolume #####
treat_noBiovolP <- function(lake){
  #' 
  #' This function achieves multiple purposes:
  #' 1) Isolating the data missing a biovolume-per-cell value in the meta database (volume_um.3 or mdn_nu.biovol)
  #' 2) Presenting the proportion of the phytoplankton entries it represents, as a plot
  #' 3) Cleaning + exporting this data so it can be analyzed accross lakes (see "B1_phyto_noBiovolume.R)
  #' 
  cat("\n-----\n")
  nrow(noBiovolP <- subset(phyto_selec, is.na(volume_um.3)))  #there are 140 rows without a volume_um.3 biovolume...
  # Now combined with the kremer biovolume:
  noBiovolP <- subset(noBiovolP, is.na(mdn_nu.biovol))  # removes the rows missing a mdn_nu.biovol 
  cat(nrow(noBiovolP), "entries don't have a biovolume in the phyto data (no volume_um.3 AND no mdn_nu.biovol)..\n")
  
  # the data frame "plot_all" is used to create a plot showing when the taxa without a biovolume occur in the data set
  plot_all <- phyto_selec
  plot_all$vol <- ""
  plot_all$vol <- ifelse((is.na(plot_all$volume_um.3) & is.na(plot_all$mdn_nu.biovol)), "No", "Yes")  # if a data point doesn't have neither a volume_um.3 nor a mdn_nu.biovol, the value is 0. Else, it is 1
  nrow(plot_all %>% filter(vol == "No"))  #it's exactly the same number as is identified in the noBiovol frame -> great!
  plot_all$vol <- factor(plot_all$vol)  # setting it as a factor for the plot
  
  plot_all <- plot_all %>% arrange(desc(vol))  # this line makes it so that the taxa without a biovolume are plotted afterwards, meaning they'll be in the foreground
  
  # Plots the data: all taxa with a biovolume are in green, those without one are in red. 
  green_red <- ggplot(data = plot_all, aes(x = date, y = abundance)) + 
    geom_point(aes(colour = vol, shape = vol)) + 
    scale_shape_manual(values = c(19, 1)) + 
    scale_colour_manual(values = c("brown1", "forestgreen")) + 
    scale_y_continuous(trans = "log10") + 
    labs(x = "Date", y = "Abundance [indv/L]", colour = "Biovolume?", shape = "Biovolume?") + 
    ggtitle(label = paste("Abundance of Phyto taxa in", lake, "lake"))
  
  # Saving the plot in a pdf file in the "graphs" folder of the "no_biovol" folder
  setwd("~/GitHub/Bachelor_cyanobac/workplace2/graphs_lists")
  pdf(paste(lake, "_noBiovolP.pdf", sep = ""))
  print(green_red)
  dev.off()
  setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new/")
  
  # back to the data without a biovolume
  noBiovolP <- noBiovolP %>% arrange(id_CH)
  noBiovolP$occurrences <- 1
  
  taxa_noBiovolP <- aggregate(list(noBiovolP$abundance, noBiovolP$occurrences), by = list(noBiovolP$id_CH, noBiovolP$genus, noBiovolP$species, noBiovolP$kremer_key), sum)
  names(taxa_noBiovolP) <- c("id_CH", "genus", "species", "kremer_key", "abundance", "occurrences") # This extracts the taxa out of the noBiovol frame, and shows the cumulative abundance in the "abundance" column
  cat("This represents", nrow(taxa_noBiovolP), "taxa in the data")  # Prints out how many taxa don't have a biovolume
  
  taxa_noBiovolP <- inner_join(taxa_noBiovolP, meta_db_phyto %>% select(id_CH, volume_um.3.1), by="id_CH") # adding the volume_um.3.1 (can't add it before, in case multiple taxa have the same value -> wouldn't work with the "aggregate" step)
  
  taxa_noBiovolP$mn_abundance <- taxa_noBiovolP$abundance / taxa_noBiovolP$occurrences  # the mean abundance per occurence of each taxon with no defined biovolume value
  # sum(taxa_noBiovol$occurrences)  # making sure the total of occurrences is still correct
  
  noBiovolP$lake <- lake  # adding the lake id, to plot where the taxa missing a biolovume occur (see B files)
  
  # Sorting the data in the tables
  taxa_noBiovolP <- taxa_noBiovolP %>% arrange(id_CH)  # sort by id_CH
  noBiovolP <- noBiovolP %>% arrange(date)  # sort by date
  
  # exporting this data
  setwd("~/GitHub/Bachelor_cyanobac/workplace2/no_biovol")
  write.csv(noBiovolP, paste(lake, "_phyto_datapoints_without_biovol.csv", sep = ""))  # all data points whose taxa don't have a biovolume in the database
  write.csv(taxa_noBiovolP, paste(lake, "_phyto_taxa_without_biovol.csv", sep = ""))  # same, resumed by taxa
  setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new")
  
  # Making the data frames global
  taxa_noBiovolP <<- taxa_noBiovolP
  noBiovolP <<- noBiovolP
  cat("\n-----\n")
}



# 3) Now the phyto data with a Biovolume, preparing it for the final data table ####
ready_phyto_clean <- function(){
  #'
  #' This function adds a biovolume-per-cell value to a select few taxa that missed one (these values were found in online literature)
  #' Then removing the ones left without a biovolume value
  #' It also calculates the biovolume of each entry (abundance * biovolume-per-cell)
  #'
  
  # The following lines of code add a biovolume to the taxa that don't have one
  phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 209,  63))  # replace the NA by the biovolume of the taxa
  phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 214,  20))
  phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 388,  100))
  phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 395,  314))
  phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 397,  491))
  phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 2812, 282))
  
  # Sorting the phyto_selec data table, by date
  phyto_selec <- phyto_selec %>% arrange(date)
  
  # Tidying up the data to make it ready for use
  phyto_clean <- phyto_selec[complete.cases(phyto_selec$volume_um.3) | complete.cases(phyto_selec$mdn_nu.biovol),]  # removes data points without a volume_um.3 nor a mdn_nu.biovol
  phyto_clean$id_CH  <- factor(phyto_clean$id_CH)  # The id_CH is now a factor
  # str(phyto_clean) #now all the columns have the right format
  
  # calculating the biovolume
  phyto_clean$calc_biovol <- ifelse(!is.na(phyto_clean$volume_um.3), 
                                    phyto_clean$abundance * phyto_clean$volume_um.3,
                                    phyto_clean$abundance * exp(phyto_clean$mdn_nu.biovol)) # need to exp() the value, to de-log it
  # What this does: If there is a volume_um.3 value, use it to calculate biovolume, if there is none, use the mdn_nu.biovol instead
  # The unit is um^3/L 
  
  # Sorting the phyto_clean data table, by date
  phyto_clean <- phyto_clean %>% arrange(date)
  
  # Making the data frames global
  phyto_selec <<- phyto_selec
  phyto_clean <<- phyto_clean
}

ready_phyto_agg <- function(){
  #' 
  #' This function aggregates the phytoplankton biovolume per order separately for each date
  #' 
  cat("\n-----\n")
  # Aggregating by order, for each date
  phyto_agg <- aggregate(list(phyto_clean$calc_biovol), by= list(phyto_clean$order, phyto_clean$date), sum)
  names(phyto_agg) <- c("order", "date", "biovolume")
  
  tot_phyto <- aggregate(phyto_agg$biovolume, by = list(phyto_agg$date), sum)  #Summing all the phytoplankton data, per day
  names(tot_phyto) <- c("date", "tot_phyto")
  phyto_agg <- inner_join(phyto_agg, tot_phyto, by = "date")  # adding the total phyto to the table
  
  phyto_agg <- phyto_agg %>% filter(order == "Chroococcales" | order == "Oscillatoriales"| order == "Nostocales")
  
  # Next 3 lines show that the orders weren't always observed --> there are NA's in the data frame
  cat(nrow(phyto_agg %>% filter(order == "Chroococcales")), "observations of Chroococcales\n")
  cat(nrow(phyto_agg %>% filter(order == "Nostocales")), "observations of Nostocales\n")
  cat(nrow(phyto_agg %>% filter(order == "Oscillatoriales")), "observations of Oscillatoriales")
  
  # Making the variable global
  phyto_agg <<- phyto_agg
  cat("\n-----\n")
}



ready_phyto_final <- function(){
  #' 
  #' This function has multiple purposes:
  #' 1) Create a data frame where the aggregated biovolumes are in columns instead of rows --> each date appears only once in the "date" column
  #' 2) calculate the ratios of order biovolume to total phytoplankton biovolume (order:tot_phyto) 
  #' 3) calculate the differences between total phytoplankton biovolume and order biovolume (tot_phyto - order) 
  
  
  # The values are in rows, and we want them in columns (column names would be date, chrooco, nosto, oscillato, tot_phyto). This is what the next blocks do:
  phyto_final <- phyto_agg
  
  # Creating the columns for the total biovolumes of  the orders Chroococcales, Nostocales, Oscillatoriales
  phyto_final$chrooco <- ""
  phyto_final$nosto <- ""
  phyto_final$oscillato <- ""
  
  # Putting the total biovolume of the orders in them
  phyto_final <- phyto_final %>% mutate(chrooco = replace(chrooco, order == "Chroococcales", biovolume[order == "Chroococcales"]))
  phyto_final <- phyto_final %>% mutate(nosto = replace(nosto, order == "Nostocales", biovolume[order == "Nostocales"]))
  phyto_final <- phyto_final %>% mutate(oscillato = replace(oscillato, order == "Oscillatoriales", biovolume[order == "Oscillatoriales"]))
  
  phyto_final <- phyto_final %>% mutate_at(c("chrooco", "nosto", "oscillato"), as.numeric)  # Converting the columns to the right format, numeric
  
  phyto_final <- aggregate(list(phyto_final$chrooco, phyto_final$nosto, phyto_final$oscillato), by= list(phyto_final$date, phyto_final$tot_phyto), sum, na.rm = TRUE)
  names(phyto_final) <- c("date", "tot_phyto", "chrooco", "nosto", "oscillato")
  # Note: after this step, the NA's are replaced by 0. I write it as an NA again later
  
  # Calculating the total cyanobacteria biovolume
  phyto_final <- phyto_final %>% mutate(tot_cyanobac = chrooco + nosto + oscillato)
  
  # Putting the NA back into the data frame
  phyto_final <- phyto_final %>% mutate(across(c(chrooco, nosto, oscillato), ~na_if(., 0)))
  
  # Now that we have them in columns, we can calculate the ratio between the orders and the total phytoplankton biovolume:
  phyto_final <-  phyto_final %>% mutate(r_chrooco = chrooco / tot_phyto)
  phyto_final <-  phyto_final %>% mutate(r_nosto = nosto / tot_phyto)
  phyto_final <-  phyto_final %>% mutate(r_oscillato = oscillato / tot_phyto)
  phyto_final <-  phyto_final %>% mutate(r_cyanobac = tot_cyanobac / tot_phyto)
  
  # And the difference between the total phytoplankton biovolume and the orders:
  phyto_final <-  phyto_final %>% mutate(d_chrooco = tot_phyto - chrooco)
  phyto_final <-  phyto_final %>% mutate(d_nosto = tot_phyto - nosto)
  phyto_final <-  phyto_final %>% mutate(d_oscillato = tot_phyto - oscillato)
  phyto_final <-  phyto_final %>% mutate(d_cyanobac = tot_phyto - tot_cyanobac)
  
  # Now the phyto_final is ready to be put into the big data table. Time to make the variable global:
  phyto_final <<- phyto_final
}


# 4) Getting the zoo data ready ####
ready_zoo_raw <- function(){
  #' 
  #' This function makes sure the data type of the columns is what it needs to be
  #' 
  zoo_raw$depth <- factor(zoo_raw$depth)  # only one depth
  zoo_raw$unit <- factor(zoo_raw$unit)  # only one unit
  zoo_raw$date <- as.Date(zoo_raw$date, "%Y-%m-%d") # transforming the date to a Date format
  
  # making the data frame global
  zoo_raw <<- zoo_raw
}






# 5) The zoo taxa ####
ready_zoo_agg <- function(){
  #' 
  #' This function calculates the total abundance of the Zooplankton, and of the orders Cyclopoida and Calanoida, as well as the family Daphniidae and the created category of Microzooplankton (Rotifera, Ciliophora and Naupleii) by date
  #' 
  #' 
  cat("\n-----\n")
  zoo_selec <- inner_join(zoo_raw, (meta_db_zoo %>% select(id_CH, phylum, order, family, genus, species, stage, volume_um.3, guild)), by="id_CH")  # Adding the needed info columns to the data table
  
  # Tidying up the data to make it ready for use
  zoo_agg <- zoo_selec[complete.cases(zoo_selec$abundance),] # removes data points without an abundance
  nrow(zoo_raw) - nrow(zoo_agg)  # Checks how many data points didn't have an abundance
  
  # this makes the AGGREGATED table with id, guild, order, genus, species, abundance, occurrences. Makes for a summary of all zoo taxa observed in the lake
  {zoo_taxa <- zoo_agg
  zoo_taxa$occurrences = 1
  
  nrow(zoo_taxa <- aggregate(list(zoo_taxa$abundance, zoo_taxa$occurrences), by = list(zoo_taxa$id_CH, zoo_taxa$genus, zoo_taxa$species), sum))
  names(zoo_taxa) <- c("id_CH", "genus", "species", "abundance", "occurrences")
  
  zoo_taxa <- inner_join(zoo_taxa, meta_db_zoo %>% select(id_CH, volume_um.3, phylum, order, stage, family, guild), by = "id_CH")  # adding the info from the meta_db_zoo to the data table
  zoo_taxa <- zoo_taxa %>% select(id_CH, phylum, guild, order, family, genus, species, stage, volume_um.3, abundance, occurrences)  # re-ordering the columns 
  }
  
  
  tot_zoo <- aggregate(zoo_agg$abundance, by = list(zoo_agg$date), sum)  # Summing all the zooplankton abundance, per day.
  names(tot_zoo) <- c("date", "tot_zoo")
  
  # Extracting the microzooplankton from the data frame
  naupleii <- zoo_agg %>% filter(((order == "Calanoida" | order == "Cyclopoida") & stage == "juvenile") | stage == "nauplia")  # extracting the younger states of the copepods, called "naupleii", "nauplii" or "nauplia" depending on the literature
  roti_cilio <- zoo_agg %>% filter(phylum == "Rotifera" | phylum == "Ciliophora")  # extracting the Rotifera and Ciliophora (since they are phyla, they can't be aggregated by order  -> need to be aggregated separately)
  
  # aggregating the microzooplankton on its own, by date
  microzoo <- bind_rows(naupleii, roti_cilio)  # creating the data frame with Ciliates, Rotifers and Naupleii
  microzoo_taxa <- microzoo %>% distinct(id_CH)  # This creates a list of the microzooplankton, so we can then exclude them from the zoo_agg data frame
  microzoo <- aggregate(list(microzoo$abundance), by = list(microzoo$date), sum)
  names(microzoo) <- c("date", "microzoo")
  
  zoo_agg <- zoo_agg %>% anti_join(microzoo_taxa, by = "id_CH")  # removing the microzooplankton from the rest of the data frame (they will be put back into the table later on)
  
  zoo_agg <- zoo_agg %>% filter(order == "Calanoida"| order == "Cyclopoida"| family == "Daphniidae")  # only keeping the relevant orders and families
  
  # Aggregating by order, for each date
  zoo_agg <- aggregate(list(zoo_agg$abundance), by= list(zoo_agg$order, zoo_agg$date), sum)
  names(zoo_agg) <- c("order", "date", "abundance")
  
  # Adding the total zooplankton and the microzoo back
  zoo_agg <- inner_join(zoo_agg, tot_zoo, by = "date")  # adding the total zoo to the table
  zoo_agg <- inner_join(zoo_agg, microzoo, by = "date")  # Adding the microzooplankton back to the table (Rotifera, Ciliophora, Naupleii)
  
  # Next 3 lines show that the orders weren't always observed --> consequence: there are NA's in the data frame
  cat("Daphniidae were observed", nrow(zoo_agg %>% filter(order == "Diplostraca")), "times\n")
  cat("Calanoida were observed", nrow(zoo_agg %>% filter(order == "Calanoida")), "times\n")
  cat("Cyclopoida were observed", nrow(zoo_agg %>% filter(order == "Cyclopoida")), "times")
  
  
  # making the data frames global
  zoo_agg <<- zoo_agg
  zoo_taxa <<- zoo_taxa
  microzoo_taxa <<- microzoo_taxa
  cat("\n-----\n")
}



ready_zoo_final <- function(){
  #' 
  #' This function transforms the shape of the zoo_agg data frame: where the orders were represented as rows before, after the function they are columns
  #' This means there is only one row per date
  #'
  cat("\n-----\n")
  zoo_final <- zoo_agg
  
  # The values are in rows, and we want them in columns
  zoo_final$daphniidae <- ""  # First, create empty columns to put the values in
  zoo_final$cyclopoida <- ""
  zoo_final$calanoida <- ""
  
  # Then, put the total abundance of the orders and families in them 
  zoo_final <- zoo_final %>% mutate(daphniidae = replace(daphniidae, order == "Diplostraca", abundance[order == "Diplostraca"]))
  zoo_final <- zoo_final %>% mutate(cyclopoida = replace(cyclopoida, order == "Cyclopoida", abundance[order == "Cyclopoida"]))
  zoo_final <- zoo_final %>% mutate(calanoida = replace(calanoida, order == "Calanoida", abundance[order == "Calanoida"]))
  
  zoo_final <- zoo_final %>% mutate_at(c("daphniidae", "cyclopoida", "calanoida"), as.numeric)  # Converting the columns to numeric
  
  zoo_final <- aggregate(list(zoo_final$daphniidae, zoo_final$cyclopoida, zoo_final$calanoida), by = list(zoo_final$date, zoo_final$tot_zoo, zoo_final$microzoo), sum, na.rm = TRUE)  # After this step, all NA's are replaced by 0's. I will change back to NA again later
  names(zoo_final) <- c("date", "tot_zoo", "microzoo", "daphniidae", "cyclopoida", "calanoida")
  
  # Removing the 0's and replacing them with NA's
 zoo_final <- zoo_final %>% mutate(across(c(daphniidae, cyclopoida, calanoida), ~na_if(., 0)))
 
  # making the data frame global
  zoo_final <<- zoo_final
  cat("\n-----\n")
}



# 7) importing + plotting the temp-chem  data ####
ready_temp_chem <- function(){
  #' 
  #' This function prepares the chemistry and temperature data to be put into the end data frame
  #' 
  temp_chem$date <- as.Date(temp_chem$date, "%Y-%m-%d") # transforming the date to a Date format
  temp_chem$depth <- factor(temp_chem$depth)  #Only 1 depth -> factor
  
  # Making the data frame global
  temp_chem <<- temp_chem
}



# 8) Creating the big data frame, and plots ####
ready_end_frame <- function(){
  #' 
  #' This function puts the prepared phytoplankton data, the zooplankton data with the chemistry and temperature data into a single data frame
  #' 
  end_frame <- phyto_final
  
  # Adding the temperature and chemistry data
  end_frame <- inner_join(end_frame, temp_chem, by= "date")
  
  # Adding the Zooplankton data
  end_frame <- inner_join(end_frame, zoo_final, by = "date")
  
  
  # Making the data frame global
  end_frame <<- end_frame
}


save_plots <- function(lake){
  #' 
  #' This function plots the data for each phytoplankton order (Nosto, Chrooco, Oscillato) and for the total phytoplankton biovolume
  #' It also begins the work of the linear regressions, by plotting the data (it doesn't show any plots)
  #' All plots are then saved in a single pdf file (the phytoplankton data, basic scatterplots and the temp-chem data)
  #' 
  #' Note: In each plot, there is the warning message "Removed XX rows containing missing values ('geom_point()')". This is normal and indicates the ggplot() is removing the NA values
  #' 
  
  # Plotting Nostocales and Oscillatoriales
  nosto_oscillato <- ggplot(data = end_frame) + 
    geom_point(aes(x = date, y = nosto, colour = "Nosto"), size = 0.5) + 
    geom_point(aes(x = date, y = oscillato, colour = "Oscillato"), size = 0.5) + 
    scale_y_continuous(name = "Nostocales [um^3/L]", trans = "log10", sec.axis = sec_axis(trans =~., name = "Oscillatoriales [um^3/L]")) + 
    scale_colour_manual(name = "", breaks = c("Nosto", "Oscillato"), values = c("Nosto" = "blue", "Oscillato" = "gold")) +
    theme(legend.position = "bottom")
  
  # Plotting Chroococcales and total phytoplankton biovolume
  chrooco_phyto <- ggplot(data = end_frame) + 
    geom_point(aes(x = date, y = tot_phyto, colour = "Tot phyto"), size = 0.5) + 
    geom_point(aes(x = date, y = chrooco, colour = "Chrooco"), size = 0.5) + 
    scale_y_continuous(name = "Total biovolume [um^3/L]", trans = "log10", sec.axis = sec_axis(trans =~., name = "Chroococcales [um^3/L]")) + 
    scale_colour_manual(name = "", breaks = c("Chrooco", "Tot phyto"), values = c("Chrooco" = "deeppink", "Tot phyto" = "chocolate1")) +
    theme(legend.position = "bottom")
  
  # Plotting total phyto biovolume and the biovolume of the 3 orders Oscillatoriales, Chroococcales, Nostocales
  phyto <- ggplot(data = end_frame) + 
    geom_point(aes(x = date, y = tot_phyto, colour = "Tot phyto"), size = 0.5) + 
    geom_point(aes(x = date, y = oscillato, colour = "Oscillato"), size = 0.5) +
    geom_point(aes(x = date, y = nosto, colour = "Nosto"), size = 0.5) + 
    geom_point(aes(x = date, y = chrooco, colour = "Chrooco"), size = 0.5) + 
    geom_smooth(aes(x = date, y = tot_phyto, colour = "Tot phyto")) +
    geom_smooth(aes(x = date, y = nosto, colour = "Nosto")) +
    geom_smooth(aes(x = date, y = oscillato, colour = "Oscillato")) +
    geom_smooth(aes(x = date, y = chrooco, colour = "Chrooco")) +
    scale_y_continuous(name = "Biovolume [um^3/L]", trans = "log10") + 
    scale_colour_manual(name = "", breaks = c("Tot phyto", "Oscillato", "Nosto", "Chrooco"), values = c("Tot phyto" = "limegreen", "Oscillato" = "gold", "Nosto" = "blue",  "Chrooco" = "deeppink")) +
    theme(legend.position = "bottom")
  
  # Plotting the ratios of the orders Oscillatoriales, Nostocales and Chroococcales
  ratios <- ggplot(data = end_frame) +
    geom_point(aes(x = date, y = r_oscillato, colour = "Ratio Oscillato"), size = 0.5) +
    geom_point(aes(x = date, y = r_nosto, colour = "Ratio Nosto"), size = 0.5) + 
    geom_point(aes(x = date, y = r_chrooco, colour = "Ratio Chrooco"), size = 0.5) +
    geom_smooth(aes(x = date, y = r_nosto, colour = "Ratio Nosto")) +
    geom_smooth(aes(x = date, y = r_oscillato, colour = "Ratio Oscillato")) +
    geom_smooth(aes(x = date, y = r_chrooco, colour = "Ratio Chrooco")) +
    scale_y_continuous(name = "Ratio of the phyto orders", labels = scales::percent, sec.axis = sec_axis(trans =~., name = "", labels = scales::percent)) + 
    scale_colour_manual(name = "", breaks = c("Ratio Nosto", "Ratio Chrooco", "Ratio Oscillato"), values = c("Ratio Nosto" = "blue", "Ratio Chrooco" = "deeppink", "Ratio Oscillato" = "gold")) +
    theme(legend.position = "bottom")
  

  
  # Plotting the biovolume of the 2 cyanobacteria orders, the total phytoplankton biovolume and the total abundance of each Zooplankton group
  zoo_phyto <- ggplot(data = end_frame) + 
    geom_point(aes(x = date, y = tot_phyto, colour = "Tot phyto"), size = 0.5) + 
    geom_point(aes(x = date, y = tot_zoo, colour = "Tot zoo"), size = 0.5) + 
    geom_point(aes(x = date, y = chrooco, colour = "Chrooco"), size = 0.5) + 
    geom_point(aes(x = date, y = oscillato, colour = "Oscillato"), size = 0.5) + 
    geom_point(aes(x= date, y = nosto, colour = "Nosto"), size = 0.5) + 
    scale_y_continuous(name = "Biovolume [um^3/L]", trans = "log10", sec.axis = sec_axis(trans =~., name = "Tot zoo abundance [indv.]")) + 
    scale_colour_manual(name = "", breaks = c("Tot zoo", "Tot phyto", "Oscillato", "Nosto", "Chrooco"), values = c("Tot zoo" = "chocolate1", "Tot phyto" = "limegreen", "Oscillato" = "gold", "Nosto" = "blue",  "Chrooco" = "deeppink")) +
    theme(legend.position = "bottom")
  
  zoo_phyto_trends <- ggplot(data = end_frame) + 
    geom_smooth(aes(x = date, y = tot_phyto, colour = "Tot phyto")) + 
    geom_smooth(aes(x = date, y = tot_zoo, colour = "Tot zoo")) + 
    geom_smooth(aes(x = date, y = chrooco, colour = "Chrooco")) + 
    geom_smooth(aes(x = date, y = oscillato, colour = "Oscillato")) + 
    geom_smooth(aes(x= date, y = nosto, colour = "Nosto")) + 
    scale_y_continuous(name = "Biovolume [um^3/L]", trans = "log10", sec.axis = sec_axis(trans =~., name = "Tot zoo abundance [indv.]")) + 
    scale_colour_manual(name = "", breaks = c("Tot zoo", "Tot phyto", "Oscillato", "Nosto", "Chrooco"), values = c("Tot zoo" = "chocolate1", "Tot phyto" = "limegreen", "Oscillato" = "gold", "Nosto" = "blue",  "Chrooco" = "deeppink")) +
    theme(legend.position = "bottom")
  
  # Arranging the plots into nicer, cleaner figures
  figure_all <- ggarrange(nosto_oscillato, chrooco_phyto, 
                          no3_nh4, temp_po4,
                          ncol = 2, nrow = 2)
  figure_trends <- ggarrange(zoo_phyto_trends, ratios, 
                             zoo_phyto, phyto,
                             ncol = 2, nrow = 2, align = "hv",
                             common.legend = TRUE, legend = "top")
  
  
  
  
  #' Ajuster la scale right, puis faire un graph pour les trends uniquement
  #' Il faut que je regarde si c'est vraiment nécessaire de faire des graphes pour ça
  #' --> pour ça, il faut les mettre dans figure_trends 
  
  
  
  # Creating the data frames for the linear regressions. The second one has log10-values for the biovolumes
  # First, chemistry - phytoplankton
  chem_phyto <- end_frame %>% select(NH4_N, NO3_N, PO4_P, temperature, tot_phyto, chrooco, nosto, oscillato)
  chem_log10_phyto <- bind_cols(end_frame %>% select(NH4_N, NO3_N, PO4_P, temperature), end_frame %>% summarize(across(tot_phyto:oscillato, log)))
  
  # Second, zooplankton - phytoplankton
  zoo_phyto <- end_frame %>% select(tot_zoo, microzoo, daphniidae, cyclopoida, calanoida, tot_phyto, chrooco, nosto, oscillato)
  zoo_phyto_log10  <- end_frame %>% summarize(across(c(tot_zoo, microzoo, daphniidae, cyclopoida, calanoida, tot_phyto, chrooco, nosto, oscillato), log))
  
  # Third, chemistry - zooplankton
  chem_zoo <- end_frame %>% select(NH4_N, NO3_N, PO4_P, temperature, tot_zoo, microzoo, daphniidae, cyclopoida, calanoida)
  chem_log10_zoo <- bind_cols(end_frame %>% select(NH4_N, NO3_N, PO4_P, temperature), end_frame %>% summarize(across(.cols = c(tot_zoo, microzoo, daphniidae, cyclopoida, calanoida), log)))
  

  # these are simple scatterplots, for all variable relative to each other 
  # First, chemistry - phytoplankton
  pairs_chem_phyto <- ggpairs(chem_phyto, lower = list(continuous = wrap("smooth", colour = "cornflowerblue", size = 0.1)), title = "Scatter plots, Chemistry - Phytoplankton")
  pairs_chem_log10_phyto <- ggpairs(chem_log10_phyto, lower = list(continuous = wrap("smooth", colour = "cornflowerblue", size = 0.1)), title = "Scatter plots, chemistry - log10(phytoplankton)")
  
  # Second, zooplankton - phytoplankton
  pairs_zoo_phyto <- ggpairs(zoo_phyto, lower = list(continuous = wrap("smooth", colour = "cornflowerblue", size = 0.1)), title = "Scatter plots, zooplankton - phytoplankton")
  pairs_zoo_phyto_log10 <- ggpairs(zoo_phyto_log10, lower = list(continuous = wrap("smooth", colour = "cornflowerblue", size = 0.1)), title = "Scatter plots, log10(zooplankton) - log10(phytoplankton)")
  
  # Third, chemistry - zooplankton
  pairs_chem_zoo <- ggpairs(chem_zoo, lower = list(continuous = wrap("smooth", colour = "cornflowerblue", size = 0.1)), title = "Scatter plots, chemistry - zooplankton")
  pairs_chem_log10_zoo <- ggpairs(chem_log10_zoo, lower = list(continuous = wrap("smooth", colour = "cornflowerblue", size = 0.1)), title = "Scatter plots, chemistry - log10(zooplankton)")
  
  # Making a pdf file out of the figure with all the plots, and saving it in the "graphs_lists" folder
  setwd("~/GitHub/Bachelor_cyanobac/workplace2/graphs_lists")
  pdf(paste(lake, "_plot_temp_chem.pdf", sep = ""))
  print(figure_tempchem)
  print(figure_all)
  print(figure_trends)
  
  # the pairs plots
  print(pairs_chem_phyto)
  print(pairs_chem_log10_phyto)
  print(pairs_zoo_phyto)
  print(pairs_zoo_phyto_log10)
  print(pairs_chem_zoo)
  print(pairs_chem_log10_zoo)
  dev.off()
  setwd("~/GitHub/Bachelor_cyanobac/workplace2/data_new")
  
  # Making the data frames for the linear regressions and their plots global
  chem_phyto <<- chem_phyto
  chem_log10_phyto <<- chem_log10_phyto
  zoo_phyto <<- zoo_phyto
  zoo_phyto_log10 <<- zoo_phyto_log10
  chem_zoo <<- chem_zoo
  chem_log10_zoo <<- chem_log10_zoo
  
  # and the plots
  figure_all <<- figure_all
  figure_trends <<- figure_trends
  }











# ####
#' TODO Ce que je peux encore faire:
#' 
#' 
#' il Faut enlever les noms des axes chroococcales dans le pdf temp_chem
#' 
#' 
#' 
#' 
#' 
#' 
#' Mettre à jour les x-axes des plots noBiovolP et noBiovolZ. C'est à faire si je veux tous les mettres dans un seul pdf, ils ne commencent pas tous la même année