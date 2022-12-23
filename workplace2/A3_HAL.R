library(dplyr)
library(ggplot2)
library(ggpubr)

setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new")


# 0) loading the meta databases ####
# for Phytoplankton:
metaDBphyto <- read.delim("metaDBphyto_2020.csv", sep = ";", dec = ",")
dim(subset(metaDBphyto, is.na(id_CH)))  # In this database, there are no taxa without an id_CH
dim(no_volP <- inner_join(subset(metaDBphyto, is.na(volume_um.3)), subset(metaDBphyto, is.na(mdn_nu.biovol)), by="id_CH")) # this calculates how many taxa have an id_cH, but don't have a volume_um.3 nor a mdn_nu.biovol. It's 42 entries in the meta database for Phyto

# For Zooplankton
metaDBzoo <- read.csv("metaDBzoo_2020.csv")
dim(subset(metaDBzoo, is.na(id_CH)))  # In this database, there are no taxa without an id_CH
dim(no_volZ <- subset(metaDBzoo, is.na(volume_um.3))) # this calculates how many taxa have an id_cH, but don't have a volume_um.3 . It's 153 entries in the meta database for Zoo



# 1) Getting the HAL phyto data ready ####
phyto_raw <- read.csv("phyto.smallzoo.85-20_HAL.csv", sep = ",")
dim(subset(phyto_raw, is.na(id_CH)))  # All collected datapoints have an id_CH
phyto_raw$depth <- factor(phyto_raw$depth)  # only one depth
phyto_raw$unit <- factor(phyto_raw$unit)  # only one unit
phyto_raw$date <- as.Date(phyto_raw$date, "%Y-%m-%d") # transforming the date to a Date format

phyto_selec <- inner_join(phyto_raw, (metaDBphyto %>% select(id_CH, order, genus, species, volume_um.3, kremer_key, mdn_nu.biovol, mn_cells.per.nu, volume_um.3.1, species_buergi)), by="id_CH")  # Adding the needed info columns to the phyto data table



# 2) Taking a look at the phyto taxa without a biovolume #####
dim(noBiovol <- subset(phyto_selec, is.na(volume_um.3)))  #there are 2208 rows without a volume_um.3 biovolume...
# Now combined with the kremer biovolume:
dim(noBiovol <- subset(noBiovol, is.na(mdn_nu.biovol))) # only 305 entries left

# the data frame "plot_all" is used to create a plot showing when the taxa without a biovolume occur in the data set
plot_all <- phyto_selec
plot_all$vol <- ""
plot_all$vol <- ifelse((is.na(plot_all$volume_um.3) & is.na(plot_all$mdn_nu.biovol)), "No", "Yes")  # if a data point doesn't have neither a volume_um.3 nor a mdn_nu.biovol, the value is 0. Else, it is 1
dim(plot_all %>% filter(vol == "No"))  #it's exactly the same number as is identified in the nobiovol frame -> great!
plot_all$vol <- factor(plot_all$vol)  # setting it as a factor for the plot

plot_all <- plot_all %>% arrange(desc(vol))  # this line makes it so that the taxa without a biovolume are plotted afterwards, meaning they'll be in the foreground

# Plots the data: all taxa with a biovolume are in green, those without one are in red. And saving the plot in a pdf in the "graphs" folder of the "no_biovol" folder
setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
pdf("HAL_nobiovolP.pdf")
ggplot(data = plot_all, aes(x = date, y = abundance)) + 
  geom_point(aes(colour = vol, shape = vol)) + 
  scale_shape_manual(values = c(19, 1)) + 
  scale_colour_manual(values = c("brown1", "forestgreen")) + 
  scale_y_continuous(trans = "log10") + 
  labs(x = "Date", y = "Abundance [indv/L]", colour = "Biovolume?", shape = "Biovolume?") + 
  ggtitle(label = "Abundance of Phyto taxa in HAL lake")
dev.off()
setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new/")

# Back to the data without a biovolume
noBiovol <- noBiovol %>% arrange(id_CH)
noBiovol$occurrences <- 1

dim(taxa_noBiovol <- aggregate(list(noBiovol$abundance, noBiovol$occurrences), by = list(noBiovol$id_CH, noBiovol$genus, noBiovol$species, noBiovol$kremer_key), sum))  # There are 4 taxa without a biovolume
names(taxa_noBiovol) <- c("id_CH", "genus", "species", "kremer_key", "abundance", "occurrences")  # This extracts the taxa out of the noBiovol frame, and shows the cumulative abundance in the "abundance" column
# There are 78 taxa without a biovolume

taxa_noBiovol <- inner_join(taxa_noBiovol, metaDBphyto %>% select(id_CH, volume_um.3.1), by="id_CH") # adding the volume_um.3.1

taxa_noBiovol$mn_abundance <- taxa_noBiovol$abundance / taxa_noBiovol$occurrences  # the mean abundance per occurence of each taxon with no defined biovolume value
# sum(taxa_noBiovol$occurrences)  # making sure the total of occurrences is still correct

# Sorting the data in the table
taxa_noBiovol <- taxa_noBiovol %>% arrange(id_CH)  # sort by id_CH
# taxa_noBiovol <- taxa_noBiovol %>% arrange(abundance)  # or sort by cumulative abundance

noBiovol$lake <- "HAL"  # adding the lake id, to plot where the taxa missing a biolovume occur (see B files)

# exporting this data
setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")
write.csv(noBiovol, "HAL_phyto_datapoints_without_biovol.csv")  # all data points whose taxa don't have a biovolume in the database
write.csv(taxa_noBiovol, "HAL_phyto_taxa_without_biovol.csv")  # same, resumed by taxa
setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new")



# 3) Now the phyto data with a Biovolume, preparing it for the final data table ####
# TODO These lines of code add a biovolume to the taxa that don't have one. It is ready. Just need to replace the NA by the found biovolume
phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 209,  63))  # replace the NA by the biovolume of the taxa
phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 214,  20))
phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 388,  100))
phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 395,  314))
phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 397,  491))
phyto_selec <- phyto_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 2812, 282))

# Tidying up the data to make it ready for use
phyto_clean <- phyto_selec[complete.cases(phyto_selec$volume_um.3) | complete.cases(phyto_selec$mdn_nu.biovol),]  # removes data points without a volume_um.3 nor a mdn_nu.biovol
dim(phyto_selec) - dim(phyto_clean)  # removes 305 points. Seems correct, since the number corresponds with the number of points saved in nobiovol in 2)

# tidying up the data to make it ready for use
phyto_clean$id_CH  <- factor(phyto_clean$id_CH)  
phyto_clean$volume_um.3 <- as.double(phyto_clean$volume_um.3)  # the R file reader interprets these 4 variables as Characters (volume_um.3, mdn_nu.biovol, mn_cells.per.nu, volume_um.3.1), for a reason I don't get
phyto_clean$mdn_nu.biovol <- as.double(phyto_clean$mdn_nu.biovol)
phyto_clean$mn_cells.per.nu <- as.double(phyto_clean$mn_cells.per.nu)
phyto_clean$volume_um.3.1 <- as.double(phyto_clean$volume_um.3.1)
str(phyto_clean) #now all the columns have the right format

# calculating the biovolume
phyto_clean$calc_biovol <- ifelse(!is.na(phyto_clean$volume_um.3), 
                                  phyto_clean$abundance * phyto_clean$volume_um.3,
                                  phyto_clean$abundance * exp(phyto_clean$mdn_nu.biovol)) # need to exp() the value, to de-log it
# If there is a volume_um.3 value, use it to calculate biovolume, if there is none, use the mdn_nu.biovol instead

# Aggregating by order, for each date
phyto_agg <- aggregate(list(phyto_clean$calc_biovol), by= list(phyto_clean$order, phyto_clean$date), sum)
names(phyto_agg) <- c("order", "date", "biovolume")

tot_phyto <- aggregate(phyto_agg$biovolume, by = list(phyto_agg$date), sum)  #Summing all the phytoplankton data, per day
names(tot_phyto) <- c("date", "tot_phyto")
phyto_agg <- inner_join(phyto_agg, tot_phyto, by = "date")  # adding the total phyto to the table

phyto_agg <- phyto_agg %>% filter(order == "Chroococcales" | order == "Oscillatoriales"| order == "Nostocales")

# Next 3 lines show that the orders weren't always observed --> there are NA's in the data frame
dim(phyto_agg %>% filter(order == "Chroococcales"))
dim(phyto_agg %>% filter(order == "Nostocales"))
dim(phyto_agg %>% filter(order == "Oscillatoriales"))

# The values are in rows, and we want them in columns (column names would be date, chrooco, nosto, oscillato, tot_phyto). This is what the next lines do:
phyto_final <- phyto_agg

# Creating the columns for the total biovolumes of the orders Chroococcales, Nostocales, Oscillatoriales
phyto_final$chrooco <- ""
phyto_final$nosto <- ""
phyto_final$oscillato <- ""

# Putting the total biovolume of the orders in them
phyto_final <- phyto_final %>% mutate(chrooco = replace(chrooco, order == "Chroococcales", biovolume[order == "Chroococcales"]))
phyto_final <- phyto_final %>% mutate(nosto = replace(nosto, order == "Nostocales", biovolume[order == "Nostocales"]))
phyto_final <- phyto_final %>% mutate(oscillato = replace(oscillato, order == "Oscillatoriales", biovolume[order == "Oscillatoriales"]))

# Converting the columns to the right format, numeric
phyto_final$chrooco <- as.numeric(phyto_final$chrooco)
phyto_final$nosto <- as.numeric(phyto_final$nosto)
phyto_final$oscillato <- as.numeric(phyto_final$oscillato)

phyto_final <- aggregate(list(phyto_final$chrooco, phyto_final$nosto, phyto_final$oscillato), by= list(phyto_final$date, phyto_final$tot_phyto), sum, na.rm = TRUE)
names(phyto_final) <- c("date", "tot_phyto", "chrooco", "nosto", "oscillato")
# Note: after this step, the NA's are replaced by 0. I can write it as an NA again later

# Calculating the total cyanobacteria biovolume
phyto_final <- phyto_final %>% mutate(tot_cyanobac = chrooco + nosto + oscillato)

# Putting the NA back into the data frame
phyto_final <- phyto_final %>% mutate(chrooco = replace(chrooco, chrooco == 0, NA))
phyto_final <- phyto_final %>% mutate(nosto = replace(nosto, nosto == 0, NA))
phyto_final <- phyto_final %>% mutate(oscillato = replace(oscillato, oscillato == 0, NA))

# Now that we have them in columns, we can calculate the ratio between the orders and the total phytoplankton biovolume:
phyto_final <-  phyto_final %>% mutate(r_chrooco = chrooco / tot_phyto)
phyto_final <-  phyto_final %>% mutate(r_nosto = nosto / tot_phyto)
phyto_final <-  phyto_final %>% mutate(r_oscillato = oscillato / tot_phyto)
phyto_final <-  phyto_final %>% mutate(r_cyanobac = tot_cyanobac / tot_phyto)

# And the difference between the total phytoplankton biovolume and the orders:
phyto_final <-  phyto_final %>% mutate(d_chrooco = tot_phyto - chrooco)
phyto_final <-  phyto_final %>% mutate(d_nosto = tot_phyto - nosto)
phyto_final <-  phyto_final %>% mutate(d_oscillato = tot_phyto - oscillato)
phyto_final <-  phyto_final %>% mutate(r_cyanobac = tot_cyanobac - oscillato)

phyto_final$lake <- "HAL"

# Now the phyto_final is ready to be put into the big data table



# 4) Getting the zoo data ready ####
zoo_raw <- read.csv("largezoo.85-20_HAL.csv")
zoo_raw$depth <- factor(zoo_raw$depth)  # only one depth
zoo_raw$unit <- factor(zoo_raw$unit)  # only one unit
zoo_raw$date <- as.Date(zoo_raw$date, "%Y-%m-%d") # transforming the date to a Date format

zoo_selec <- inner_join(zoo_raw, (metaDBzoo %>% select(id_CH, order, genus, species, stage, volume_um.3, guild)), by="id_CH")  # Adding the needed info columns to the data table



# 5) The zoo taxa without a biovolume ####
dim(noBiovolZ <- subset(zoo_selec, is.na(volume_um.3)))  #there are 4463 rows without a volume_um.3 BiovolZume -> that's 29.97% of the data

# the data frame "plot_all" is used to create a plot showing when the taxa without a biovolume occur in the data set
plot_allZ <- zoo_selec
plot_allZ$vol <- ""
plot_allZ$vol <- ifelse((is.na(plot_allZ$volume_um.3)), "No", "Yes")  # if a data point doesn't have neither a volume_um.3 nor a mdn_nu.biovol, the value is 0. Else, it is 1
dim(plot_allZ %>% filter(vol == "No"))  #it's exactly the same number as is identified in the nobiovolZ frame -> great!
plot_allZ$vol <- factor(plot_allZ$vol)  # setting it as a factor for the plot

plot_allZ <- plot_allZ %>% arrange(desc(vol))  # this line makes it so that the taxa without a biovolume are plotted afterwards, meaning they'll be in the foreground

# Plots the data: all taxa with a biovolume are in green, those without one are in red. And saving the plot in a pdf in the "graphs" folder of the "no_biovol" folder
setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
pdf("HAL_nobiovolZ.pdf")
ggplot(data = plot_allZ, aes(x = date, y = abundance)) + 
  geom_point(aes(colour = vol, shape = vol)) + 
  scale_shape_manual(values = c(19, 1)) + 
  scale_colour_manual(values = c("brown1", "forestgreen")) + 
  scale_y_continuous(trans = "log10") + 
  labs(x = "Date", y = "Abundance [indv/L]", colour = "Biovolume?", shape = "Biovolume?") + 
  ggtitle(label = "Abundance of Zoo taxa in HAL lake")
dev.off()

setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new/") # setting the working directory back to the folder for this script.

# Back to the data without a biovolume:
noBiovolZ <- noBiovolZ %>% arrange(id_CH)
noBiovolZ$occurrences <- 1

dim(taxa_noBiovolZ <- aggregate(list(noBiovolZ$abundance, noBiovolZ$occurrences), by = list(noBiovolZ$id_CH, noBiovolZ$genus, noBiovolZ$species), sum))  # There are 5 id_CH without a Biovolume
names(taxa_noBiovolZ) <- c("id_CH", "genus", "species", "abundance", "occurrences")

taxa_noBiovolZ <- inner_join(taxa_noBiovolZ, metaDBzoo %>% select(id_CH, order, stage, guild), by = "id_CH")  # adding the order, guild and development stage 
taxa_noBiovolZ <- taxa_noBiovolZ %>% select(id_CH, guild, order, genus, species, stage, abundance, occurrences)  # re-ordering the columns of the data frame, so they make sense

sum(taxa_noBiovolZ %>% filter(guild == "cyclopoid copepods") %>% select(occurrences))  # the cyclopod copepods represent XX occurrences out of XX
sum(taxa_noBiovolZ %>% filter(guild == "calanoid copepods") %>% select(occurrences))  # the calanoid copepods represent XX occurrences out of XX
sum(taxa_noBiovolZ %>% filter(guild %in% c("calanoid copepods", "herbivore cladocerans")) %>% select(occurrences))  # the herbivores represent XX occurrences out of XX And if we remove the cyclopoid copepods from that, it's XX out of XX

# taxa_noBiovolZ  <- taxa_noBiovolZ %>% filter(!id_CH %in% unlist(taxa_noBiovolZ %>% filter(guild == "cyclopoid copepods") %>% select(id_CH)))  # removes the cyclopoid copepods from taxa_noBiovolZ

taxa_noBiovolZ <- taxa_noBiovolZ %>% arrange(desc(occurrences))

noBiovolZ$lake <- "HAL"  # adding the lake id, to plot where the taxa missing a biolovume occur (see B files)

# exporting the data without a biovolume
setwd("~/ETHZ/Bachelor thesis/R/workplace2/no_biovol")
write.csv(noBiovolZ, "HAL_zoo_datapoints_without_biovol.csv")  # all data points whose taxa don't have a biovolume in the database
write.csv(taxa_noBiovolZ, "HAL_zoo_taxa_without_biovol.csv")  # same, resumed by taxa
setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new")



# 6) The zoo taxa with a biovolume ####
# TODO This line of code adds a biovolume to the taxa that don't have one. It is ready. Just need to replace the NA by the found biovolume
zoo_selec <- zoo_selec %>% mutate(volume_um.3 = replace(volume_um.3, id_CH == 6586,  NA))  # replace the NA by the biovolume of the taxa
# This line can be copy-pasted multiple times to add a biovolume to more id_CH

# Tidying up the data to make it ready for use
zoo_clean <- zoo_selec[complete.cases(zoo_selec$volume_um.3),] # removes data points without a volume_um.3
dim(zoo_selec) - dim(zoo_clean)  # the step removes 4463, exactly the amount that didn't have a volume_um.3

# this makes the AGGREGATED table with id, guild, order, genus, species, abundance, occurrences. Makes for a summary of all zoo taxa observed in the lake
zoo_taxa <- zoo_selec
zoo_taxa$occurrences = 1

dim(zoo_taxa <- aggregate(list(zoo_taxa$abundance, zoo_taxa$occurrences), by = list(zoo_taxa$id_CH, zoo_taxa$genus, zoo_taxa$species), sum))
names(zoo_taxa) <- c("id_CH", "genus", "species", "abundance", "occurrences")

zoo_taxa <- inner_join(zoo_taxa, metaDBzoo %>% select(id_CH, volume_um.3, order, guild), by = "id_CH")
zoo_taxa <- zoo_taxa %>% select(id_CH, guild, order, genus, species, volume_um.3, abundance, occurrences)  # re-ordering the columns

# Back to the cleaned data
zoo_clean$calc_biovol <- zoo_clean$abundance * zoo_clean$volume_um.3  # calculates the biovolume based on abundance and volume_um.3

# See which guilds are represented, to guess if they are relevant keeping or not
test <- aggregate(list(zoo_clean$calc_biovol), by= list(zoo_clean$order, zoo_clean$date, zoo_clean$guild), sum)
names(test) <- c("order", "date", "guild", "biovolume")

# selecting the orders there are 
View(zoo_clean %>% distinct(order))  # Compact, shows the orders present in the data. There's 7

# Aggregating by order, for each date
zoo_agg <- aggregate(list(zoo_clean$calc_biovol), by= list(zoo_clean$order, zoo_clean$date), sum)
names(zoo_agg) <- c("order", "date", "biovolume")

# not sure that the 2 next steps are relevant. To me, it would make more sense to only keep the herbivore orders, as a proxy for predatory pressure on the cyanobacteria we're looking at, but we need to keep the lines if we need the ratio of the orders relative to the total zoo biovolume
tot_zoo <- aggregate(zoo_agg$biovolume, by = list(zoo_agg$date), sum)  # Summing all the zooplankton data, per day.
names(tot_zoo) <- c("date", "tot_zoo")
zoo_agg <- inner_join(zoo_agg, tot_zoo, by = "date")  # adding the total zoo to the table

# The "temp" name can be replaced by zoo_agg once we've established that these are the relevant orders
temp <- zoo_agg %>% filter(order == "Veneroida" | order == "Calanoida"| order == "Diplostraca")  # only selecting the relevant orders (the herbivore ones, directly grazing on Phytoplankton)
temp <- aggregate(temp$biovolume, by = list(temp$date), sum)  # Aggregating the biovolume of all relevant orders
names(temp) = c("date", "biovolume")  # changing the column names for clarity and to allow for the inner_join with the end_frame 

# zoo_agg <- aggregate(zoo_agg$biovolume, by = list(zoo_agg$date), sum)  # aggregating the biovolume of the relevant orders to a single biovolume for all zoo, per date. This value can directly be added to the end_frame, see there 


# TODO then there's zoo_final
# But, if we don't need to differentiate between the orders, then we don't need a zoo_final table, we can just add the total biovolume per date to the end_frame as a separate column, like "tot_zoo"



# 7) importing + plotting the temp-chem  data ####
setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new/temp_chem")
temp_chem <- read.csv("temp.chem.69-20_HAL.csv")
temp_chem$date <- as.Date(temp_chem$date, "%Y-%m-%d") # transforming the date to a Date format
temp_chem$depth <- factor(temp_chem$depth)  #Only 1 depth -> factor


# Creating the plots with the evolution of the temperature and chemistry of the lake over time
# Plotting the temperature alone
temp <- ggplot(data = temp_chem, aes(x = date, y = temperature)) + 
  geom_point(shape = 20, colour = "aquamarine4") + 
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
  labs(x = "Date", y = "NH4-N concentration [mg/L]")

# Plotting the Nitrate alone
no3 <- ggplot(data = temp_chem, aes(x = date, y = NO3_N)) + 
  geom_point(shape = 20, colour = "darkgoldenrod1", size = 0.5) + 
  geom_smooth(colour = "darkgoldenrod1") + # adding 'method = "gam"' shows the oscillation of the data points even more
  ggtitle(label = "NO3 over the years") + 
  labs(x = "Date", y = "NO3-N concentration [mg/L]")

figure_tempchem <- ggarrange(temp, po4, 
                             nh4, no3, 
                             ncol = 2, nrow = 2)  # Putting the plots about chemistry and temperature together into a figure
figure_tempchem <- annotate_figure(figure_tempchem, top = text_grob("HAL lake", face = "bold", size = 20))  # Adding a title to the figure

# Plotting the Nitrate and Ammonia in a single plot
coeff1 <- 1  # This variable is used to change the right y-axis size
no3_nh4 <- ggplot(data = temp_chem) + 
  geom_point(aes(x = date, y = NH4_N * coeff1, colour = "NH4"), size = 0.5) + 
  geom_point(aes(x = date, y = NO3_N, colour = "NO3"), size = 0.5) +
  geom_smooth(aes(x = date, y = NH4_N * coeff1, colour = "NH4")) + 
  geom_smooth(aes(x = date, y = NO3_N, colour = "NO3")) + 
  scale_y_continuous(name = "NO3-N [mg/L]", sec.axis = sec_axis(trans=~./coeff1, name = "NH4-N [mg/L]")) + 
  ggtitle(label = "NO3 and NH4 over the years") + 
  scale_color_manual(name = "", breaks = c("NO3", "NH4"), values = c("NO3" = "darkgoldenrod1", "NH4" = "cornflowerblue")) + 
  theme(legend.position = "bottom")

# Plotting the Phosphate and temperature in a single plot
coeff2 <- 40
temp_po4 <- ggplot(data = temp_chem) + 
  geom_point(aes(x = date, y = temperature, colour = "Temperature"), size = 0.5) + 
  geom_point(aes(x = date, y = PO4_P * coeff2, colour = "PO4"), size = 0.5) + 
  geom_smooth(aes(x = date, y = temperature, colour = "Temperature")) + 
  geom_smooth(aes(x = date, y = PO4_P * coeff2, colour = "PO4")) + 
  scale_y_continuous(name = "Temperature [°C]", sec.axis = sec_axis(trans=~./coeff2, name = "PO4-P [mg/L]")) + 
  ggtitle(label = "Temperature and PO4 over the years") +
  scale_color_manual(name = "", breaks = c("Temperature", "PO4"), values = c("Temperature" = "aquamarine4", "PO4" = "firebrick3")) + 
  theme(legend.position = "bottom", plot.title = element_text(size = 10))



# 8) Creating the big data frame, and plots ####
end_frame <- phyto_final

# Adding the temperature and chemistry data
end_frame <- inner_join(end_frame, temp_chem, by= "date")

# end_frame <- inner_join(end_frame, zoo_relevant, by = "date")  # Adding the total zooplankton biovolume


# Plotting the data for each order (Nosto, Chrooco, Oscillato) and for the total phyto biovolume
# In each plot, there is the warning message "Removed XX rows containing missing values ('geom_point()'). This is normal, the ggplot() is removing the NA values
# Plotting Nostocales and Oscillatoriales
nosto_oscillato <- ggplot(data = end_frame) + 
  geom_point(aes(x = date, y = nosto, colour = "Nosto"), size = 0.5) + 
  geom_point(aes(x = date, y = oscillato, colour = "Oscillato"), size = 0.5) + 
  scale_y_continuous(name = "Nostocales", trans = "log10", sec.axis = sec_axis(trans =~., name = "Oscillatoriales")) + 
  scale_colour_manual(name = "", breaks = c("Nosto", "Oscillato"), values = c("Nosto" = "blue", "Oscillato" = "gold")) +
  theme(legend.position = "bottom")
# En enlevant le trans = "log10", je trouve qu'il y a presque une tendance à faire variation sinusoidale chez les oscillato

# Plotting Chroococcales and total phytoplankton biovolume
chrooco_phyto <- ggplot(data = end_frame) + 
  geom_point(aes(x = date, y = tot_phyto, colour = "Tot phyto"), size = 0.5) + 
  geom_point(aes(x = date, y = chrooco, colour = "Chrooco"), size = 0.5) + 
  scale_y_continuous(name = "Total biovolume", trans = "log10", sec.axis = sec_axis(trans =~., name = "Chroococcales")) + 
  scale_colour_manual(name = "", breaks = c("Chrooco", "Tot phyto"), values = c("Chrooco" = "deeppink", "Tot phyto" = "chocolate1")) +
  theme(legend.position = "bottom")

# Plotting the ratios of the orders Oscillatoriales, Nostocales and Chroococcales
ratios <- ggplot(data = end_frame) +
  geom_point(aes(x = date, y = r_oscillato, colour = "Ratio Oscillato"), size = 0.5) +
  geom_point(aes(x = date, y = r_nosto, colour = "Ratio Nosto"), size = 0.5) + 
  geom_point(aes(x = date, y = r_chrooco, colour = "Ratio Chrooco"), size = 0.5) +
  geom_smooth(aes(x = date, y = r_nosto, colour = "Ratio Nosto")) +
  geom_smooth(aes(x = date, y = r_oscillato, colour = "Ratio Oscillato")) +
  geom_smooth(aes(x = date, y = r_chrooco, colour = "Ratio Chrooco")) +
  scale_y_continuous(name = "Ratio Nostocales", labels = scales::percent, sec.axis = sec_axis(trans =~., name = "Ratio Chroococcales", labels = scales::percent)) + 
  scale_colour_manual(name = "", breaks = c("Ratio Nosto", "Ratio Chrooco", "Ratio Oscillato"), values = c("Ratio Nosto" = "blue", "Ratio Chrooco" = "deeppink", "Ratio Oscillato" = "gold")) +
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
  scale_y_continuous(name = "Biovolume", trans = "log10") + 
  scale_colour_manual(name = "", breaks = c("Tot phyto", "Oscillato", "Nosto", "Chrooco"), values = c("Nosto" = "blue", "Oscillato" = "gold", "Chrooco" = "deeppink", "Tot phyto" = "chocolate1")) +
  theme(legend.position = "bottom")

# Arranging the plots into nicer, cleaner figures
figure_all <- ggarrange(nosto_oscillato, chrooco_phyto, 
                        no3_nh4, temp_po4,
                        ncol = 2, nrow = 2)
figure_trends <- ggarrange(phyto, ratios, 
                           ncol = 2, nrow = 2, align = "hv",
                           common.legend = TRUE, legend = "top")



# 9) Linear regressions (or log-linear regressions) ####

# Both these functions make the pairs_plot look better
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "forestgreen", lwd = 0.2, ...)
}
# Based on "panel.cor"
panel.cor2 <- function(x, y, digits = 2, prefix = "", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, method="spearman")
  # r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8 / strwidth(txt)
  text(0.5, 0.5, txt, cex = 1)
}

# Creating the data frames for the linear regressions. The second one has log10-values for the biovolumes
norm_biovol <- end_frame %>% select(NH4_N, NO3_N, PO4_P, temperature, tot_phyto, chrooco, nosto, oscillato)
logged_biovol <- bind_cols(end_frame %>% select(NH4_N, NO3_N, PO4_P, temperature), end_frame %>% summarize(across(tot_phyto:oscillato, log)))

# these are simple scatterplots, for all variable relative to each other 
pairs_plot <- pairs(norm_biovol,
                    labels = c("NH4-N", "NO3-N", "PO4-P", "Temp", "Tot phyto", "Chrooco", "Nosto", "Oscillato"),
                    main = "Übersicht: Streudiagramme",
                    diag.panel = panel.hist,
                    upper.panel = panel.cor2,
                    pch = 20,
                    cex = 0.5)

# Same scatterplots, but with the log10 values for the biovolumes
pairs_plot_logged <- pairs(logged_biovol,
                           labels = c("NH4-N", "NO3-N", "PO4-P", "Temp", "Tot phyto", "Chrooco", "Nosto", "Oscillato"),
                           main = "Übersicht: Streudiagramme",
                           diag.panel = panel.hist,
                           upper.panel = panel.cor2,
                           pch = 20,
                           cex = 0.5)
# Based on these plots, I think there might be a chance of linear relationship between log(tot_phyto) and po4 

# Relationship between tot_phyto and po4:
fit_totPhyto_po4 <- lm(data = end_frame, tot_phyto ~ PO4_P)
summary(fit_totPhyto_po4)  
cor(end_frame %>% select(PO4_P, tot_phyto) %>% na.omit() %>% select(tot_phyto) %>% summarize(tot_phyto = log(tot_phyto)), end_frame %>% select(tot_phyto, PO4_P) %>% na.omit() %>% select(PO4_P))
# significant pair-wise correlation of -0.451, p = 1.73e-05
par(mfrow = c(1,2))
plot(fit_totPhyto_po4, which = c(1:2))
par(mfrow = c(1,1))
# The QQ-plots is way too bent, and the TA-Plot has a very clear triangle shape --> shouldn't use the linear regression approach


fit_phyto <- lm(data = end_frame, log(tot_phyto) ~ temperature + NH4_N + NO3_N + PO4_P)
fit_phyto1 <- lm(data = end_frame, log(tot_phyto) ~ temperature * NH4_N * NO3_N * PO4_P)
anova(fit_phyto, fit_phyto1) #  This tests whether the interaction terms between temperature, nh4, no3 and po4 are necessary (i.e. if the interactions between them are significant). Since the p-value is low (<0.05), the interaction terms are needed, and we can thus say the "fit_phyto1" is the model we need to look at
summary(fit_phyto1)
par(mfrow = c(1,2))
plot(fit_phyto1, which = c(1:2))
par(mfrow = c(1,1))
# Both the TA-plot and the QQ-plot look acceptable

# Making a pdf file out of the figure with all the plots, and saving it in the "graphs_lists" folder
setwd("~/ETHZ/Bachelor thesis/R/workplace2/graphs_lists")
pdf("HAL_plot_temp_chem.pdf")
figure_tempchem
figure_all
figure_trends

# The pairs-plots can't be saved in a variable, they have to be called in the pdf file directly
# these are simple scatterplots, for all variable relative to each other 
pairs(norm_biovol,
      labels = c("NH4-N", "NO3-N", "PO4-P", "Temp", "Tot phyto", "Chrooco", "Nosto", "Oscillato"),
      main = "Overview: Scatter plots",
      diag.panel = panel.hist,
      upper.panel = panel.cor2,
      pch = 20,
      cex = 0.1)

# Same scatterplots, but with the log10 values for the biovolumes
pairs(logged_biovol,
      labels = c("NH4-N", "NO3-N", "PO4-P", "Temp", "Tot phyto", "Chrooco", "Nosto", "Oscillato"),
      main = "Overview: Scatter plots (phytoplankton are log10)",
      diag.panel = panel.hist,
      upper.panel = panel.cor2,
      pch = 20,
      cex = 0.1)

dev.off()
setwd("~/ETHZ/Bachelor thesis/R/workplace2/data_new")
# I'm saving the pdf file only here because I want the pairs-Plots to be included, and one can't save them in a variable and simply call it

