The files in this folder are to treat data, aggregate it and then model it using a random forest (RF) algorithm, from the randomForest package

---

Files A1-A5 are divided in multiple steps:
1) importing the data and making an overview
2) take a close look at the data, sorting out the data missing a biovolume (columns "volume_um.3" and "mdn_nu.biovol"), and then aggregating it by taxa, to see how often each taxon 
3) Working on the taxa that have a Biovolume.

General notes about these files:
 - In the "phyto_final" table:
	- The columns "chrooco", "nosto", "oscillato" are the biovolume of the orders "Chroococcales", "Nostocales", "Oscillatoriales" on the date specified in the column "date"
	- NA's have been replaced with "0", otherwise R can't aggregate the data. This can very easily be reversed if/where needed
	- The "r-chrooco", "r-nosto", "d-oscillato" column are ratios / differences between the named order and the total (no ratios between orders)

---

File B1 takes a closer look at the data from the data sets that don't have a biovolume (i.e. no value in the columns "volume_um.3" and "mdn_nu.biovol") to try and assess if these taxa are too recurrent to be ignored, or not

---

Notes about the data without an id_CH in the Greifensee data (file "A2_GRE.R"):
- "Scenedesmus linearis":it's a synonym for the taxa with id_CH 3015 (Scenedesmus ellipticus), which has a defined volume_um.3 --> this one I will integrate in the data
	Question: do I change the .csv data file, or only in the R script?
- "Amöben":
- Heliozoa div.: This one appears in the metaDBzoo file --> it's a  zooplankton phylum, and has a defined biovolume (but it includes multiple taxa, so I'm not sure what to do with them -->ask)
- Oocystidae (F.): I found the Oocystaceae family (in the metaDBphyto), but no Oocystidae. Google doesn't find anything to "Oocystidae", but finds the "Oocystaceae" family. I Think it's a typo / mis-understanding and it's all the same family, but I'll ask


---

General notes, for when I'll write the final README:
- Unit of the biovolume is um^3/L (multiplication of um^3/individual and individual/L)

