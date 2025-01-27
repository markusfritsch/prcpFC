
################################################################
###	R-Basics and data
################################################################

###
###	Basics
###

###	Set work directories

#	setwd("C:/Work/Research/Papers/Rainfall_Salzburg")
#	setwd("C:/JSchnurbus/Research/Papers/Rainfall_Salzburg")
#	setwd("D:/Markus/Work/20_Projekte/280_Rainfall/R/10_data")
	setwd("D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate")
	setwd("E:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate")
#	setwd("C:/Users/Admin/OneDrive/Dokumente/280_Rainfall/R/10_data")




###	Load libraries

#	install.packages("dplyr")
	library(dplyr)
#	install.packages("lubridate")
	library(lubridate)
#	install.packages("anytime")
	library(anytime)
#	install.packages("xtable")
	library(xtable)
#	install.packages("raster")
	library(raster)
#	install.packages("usmap")
#	library(usmap)




###	Clear Workspace

rm(list=ls())








###
###	Load rainfall data and compile anomaly data
###

###	Compile anomaly data based on imputed time series

load("raindata.RData")
dat.tmp			<- raindata[,-1]
mean(colMeans(dat.tmp))
median(colMeans(dat.tmp))

monthly_refMeans	<- function(x){
  refMeans	 <- rowMeans(matrix(x[1:360], nrow = 12))							# 1960-01 to 1989-12
  return(refMeans)
}

monthly_refMedians	<- function(x){
  mat.tmp		<- matrix
  refMedians	<- apply(X = matrix(x[1:360], nrow=12), FUN = median, MARGIN = 1)		# 1960-01 to 1989-12
  return(refMedians)
}

refMeans	<- sapply(dat.tmp, function(x) monthly_refMeans(unname(unlist(x))))
refMeds		<- sapply(dat.tmp, function(x) monthly_refMedians(unname(unlist(x))))

dat_demeaned	<- dat.tmp - apply(refMeans, FUN = rep, MARGIN = 2, times = 65)
dat_demeded	<- dat.tmp - apply(refMeds, FUN = rep, MARGIN = 2, times = 65)

#dat_demeaned	<- dat.tmp - apply(refMeans, FUN = rep, MARGIN = 2, times = 64)		#when using 'Tend' = 2023-12-31
#dat_demeded	<- dat.tmp - apply(refMeds, FUN = rep, MARGIN = 2, times = 64)		#when using 'Tend' = 2023-12-31

#saveRDS(object = dat_demeaned, file = "raindata_demeaned_2025-01-16.rds")
#saveRDS(object = dat_demeded, file = "raindata_demeded_2025-01-16.rds")






###
###	Add Koppen climate zones to station meta data and generate objects with station IDs for world regions
###


###	Read in koppen climate zone classification data

kop2			<- read.table("1976-2000_ASCII.txt", header = TRUE)	# source: 
kop2[, 3]		<- factor(kop2[, 3])
names(kop2)
coordinates(kop2)	<- ~ Lon + Lat
gridded(kop2)	<- TRUE
raster2		<- raster(kop2)
#levelplot(raster2)

k.dat2	<- raster2


###	Read in monitoring station meta data

#data.frame for all stations
stat		<- readLines("D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate/ghcnd-stations_2025-01-16.txt")
stat2		<- matrix(data = NA, ncol = 8, nrow = length(stat))
for(i in 1:length(stat)){
  stat2[i,1]	<- gsub(" ", "", substr(stat[i], start = 1, stop = 11), fixed = TRUE)
  stat2[i,2]	<- gsub(" ", "", substr(stat[i], start = 13, stop = 20), fixed = TRUE)
  stat2[i,3]	<- gsub(" ", "", substr(stat[i], start = 22, stop = 30), fixed = TRUE)
  stat2[i,4]	<- gsub(" ", "", substr(stat[i], start = 32, stop = 37), fixed = TRUE)
  stat2[i,5]	<- substr(stat[i], start = 42, stop = 70)
  stat2[i,6]	<- gsub(" ", NA, substr(stat[i], start = 39, stop = 40))
  stat2[i,7]	<- gsub(" ", NA, substr(stat[i], start = 73, stop = 76))
  stat2[i,8]	<- substr(stat[i], start = 81, stop = 86)
}
stat2				<- data.frame(stat2)
stat2[, c(2:4, 8)]	<- lapply(stat2[, c(2:4, 8)], FUN = as.numeric)
names(stat2)		<- c("statID", "lat", "lon", "alt", "statName", "stateNorthAm", "network", "aa")


#add Koppen classification to station meta data
statK2 <- extract(x = k.dat2, y = stat2[ , c(3,2)], factors = TRUE, df = TRUE)	# Koppen-Dataset ist 'long-lat'; Stationen-Dataset 'lat-long'

stat2.2 <- cbind(stat2[,c(1:6)], statK2[,2])
names(stat2.2)	<- c("statID", "lat", "lon", "alt", "statName", "stateUS", "koppen2")

stat2.2$koppen2.tmp	<- sapply(stat2.2$koppen2, nchar)
stat2.2$koppen2.cz	<- substr(stat2.2$koppen2, start = 1, stop = 1)
stat2.2$koppen2.ps	<- ifelse(stat2.2$koppen2.tmp == 3
						, yes = substr(stat2.2$koppen2, start = 2, stop = 2)
						, no = "")
stat2.2$koppen2.ts	<- ifelse(stat2.2$koppen2.tmp == 3
						, yes = substr(stat2.2$koppen2, start = 3, stop = 3)
						, no = substr(stat2.2$koppen2, start = 2, stop = 2))

stat2.2 		<- cbind(stat2.2, stat2$statID %in% colnames(dat_demeded))
names(stat2.2)[12]	<- "is.in.data"


###	Create objects with station IDs according to region

statAll	<- stat2.2$statID[stat2.2$is.in.data]
statAll.id	<- substr(stat2.2$statID[stat2.2$is.in.data], start = 1, stop = 2)
#all stations that contain no more missing vales after imputation

#Australia
statAus	<- "AS"
statAus.id	<- grep(pattern = statAus, x = statAll.id)
length(statAus.id)
statID.Aus	<- stat2.2$statID[stat2.2$statID %in% statAll[statAus.id]]

#Europe
statEU	<- c("AL", "BO", "BE", "BK", "BU", "DA", "GM", "EN",
"FI", "FR", "GG", "GR", "HR", "EI", "IC", "IT", "LG", "LH", "LU",
"MT", "MD", "MJ", "NL", "MK", "NO", "AU", "PL", "PO", "RO", "SW",
"SZ", "RI", "SI", "LO", "SP", "EZ", "UP", "HU", "UK")
statEU.id	<- grep(pattern = paste(statEU, collapse = "|"), x = statAll.id)
length(statEU.id)
statID.EU	<- stat2.2$statID[stat2.2$statID %in% statAll[statEU.id]]

#United States
usStatesCont	<- c(
				"AL",
#				"AK",		# Alaska
				"AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
#				"HI",		#Hawaii
				"ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
				"MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
				"NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
				"OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
				"UT", "VT", "VA", "WA", "WV", "WI", "WY"
)

usStatesAll	<- c(
				"AL",
				"AK",		# Alaska
				"AZ", "AR", "CA", "CO", "CT", "DC", "DE", "FL", "GA",
				"HI",		#Hawaii
				"ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME",
				"MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE",
				"NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
				"OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
				"UT", "VT", "VA", "WA", "WV", "WI", "WY",
				"AS", "GU", "MP", "PR", "VI"
)

statID.usCont		<- stat2.2$statID[(stat2.2$stateUS %in% usStatesCont) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
statID.usContN		<- stat2.2$statID[!(stat2.2$stateUS %in% usStatesCont) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
statID.usAll		<- stat2.2$statID[(stat2.2$stateUS %in% usStatesAll) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
statID.usAllN		<- stat2.2$statID[!(stat2.2$stateUS %in% usStatesAll) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]

statID.USall	<- statID.usAll[statID.usAll %in% statAll]
statID.UScont	<- statID.usCont[statID.usCont %in% statAll]
length(statID.USall)
length(statID.UScont)


stat_Aus		<- stat2.2[stat2.2$statID %in% statID.Aus, ]
nrow(stat_Aus)
sum(is.na(stat_Aus$koppen2))
statID.AusWOna	<- stat_Aus[!is.na(stat_Aus$koppen2), "statID"]
length(statID.AusWOna)
length(statID.Aus)

stat_EU		<- stat2.2[stat2.2$statID %in% statID.EU, ]
nrow(stat_EU)
sum(is.na(stat_EU$koppen2))
statID.EUWOna	<- stat_EU[!is.na(stat_EU$koppen2), "statID"]
length(statID.EUWOna)
length(statID.EU)

stat_usAll		<- stat2.2[stat2.2$statID %in% statID.USall, ]
nrow(stat_usAll)
sum(is.na(stat_usAll$koppen2))
statID.USallWOna	<- stat_usAll[!is.na(stat_usAll$koppen2), "statID"]
length(statID.USallWOna)
length(statID.USall)

stat_usCont		<- stat2.2[stat2.2$statID %in% statID.UScont, ]
nrow(stat_usCont)
sum(is.na(stat_usCont$koppen2))
statID.UScontWOna	<- stat_usCont[!is.na(stat_usCont$koppen2), "statID"]
length(statID.UScontWOna)
length(statID.UScont)

#save("statID.Aus", "statID.EU", "statID.USall", "statID.UScont",
#	file = "D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate/statIDregions.RData")
#save("statID.AusWOna", "statID.EUWOna", "statID.USallWOna", "statID.UScontWOna",
#	file = "D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate/statIDregionsWOnas.RData")

stat_usAll		<- stat_usAll[!is.na(stat_usAll$koppen2), ]
stat_usCont		<- stat_usCont[!is.na(stat_usCont$koppen2), ]

#stat_usContmT	<- usmap_transform(stat_usCont)
#stat_usAllmT	<- usmap_transform(stat_usAll)





###
###	Random subsample of time series for empirical evaluation of d estimates and rule of thumb
###


#employ koppen climate zones to sample from precipitation anomaly data
load(file = "rainresults.RData")
dat.tmp<- rainresults[rainresults$statID %in% colnames(raindata)[-1], ]
nrow(dat.tmp[dat.tmp$ddiff != 0, ])/nrow(dat.tmp)
#fraction of stations in dataset where d estimates differ

#set seed and extract stations for random subsampling
n_perCZ		<- 3
seed			<- 42
dat.tmp_rs1		<- dat.tmp[(dat.tmp$statID %in% colnames(dat_demeded)) & dat.tmp$ddiff != 0 & dat.tmp$statID %in% statID.usCont & !is.na(dat.tmp$zone_c) & dat.tmp$bestd < -0.1, ]
dat.tmp_rs2		<- dat.tmp[(dat.tmp$statID %in% colnames(dat_demeded)) & dat.tmp$ddiff != 0 & dat.tmp$statID %in% statID.usCont & !is.na(dat.tmp$zone_c) & (dat.tmp$bestd > -0.1 & dat.tmp$bestd < 0.1), ]
dat.tmp_rs3		<- dat.tmp[(dat.tmp$statID %in% colnames(dat_demeded)) & dat.tmp$ddiff != 0 & dat.tmp$statID %in% statID.usCont & !is.na(dat.tmp$zone_c) & dat.tmp$bestd > 0.1, ]

#stations in main climate zone B
dat.tmp_rs1[dat.tmp_rs1$zone_c == "B", ]
dat.tmp_rs2[dat.tmp_rs2$zone_c == "B", ]
dat.tmp_rs3[dat.tmp_rs3$zone_c == "B", ]
set.seed(seed)
id.vec_B1	<- sample(1:nrow(dat.tmp_rs1[dat.tmp_rs1$zone_c == "B", ]))
id.vec_B2	<- sample(1:nrow(dat.tmp_rs2[dat.tmp_rs2$zone_c == "B", ]))
id.vec_B3	<- sample(1:nrow(dat.tmp_rs3[dat.tmp_rs3$zone_c == "B", ]))

#stations in main climate zone C
dat.tmp_rs1[dat.tmp_rs1$zone_c == "C", ]
dat.tmp_rs2[dat.tmp_rs2$zone_c == "C", ]
dat.tmp_rs3[dat.tmp_rs3$zone_c == "C", ]
id.vec_C1	<- sample(1:nrow(dat.tmp_rs1[dat.tmp_rs1$zone_c == "C", ]))
id.vec_C2	<- sample(1:nrow(dat.tmp_rs2[dat.tmp_rs2$zone_c == "C", ]))
id.vec_C3	<- sample(1:nrow(dat.tmp_rs3[dat.tmp_rs3$zone_c == "C", ]))

#stations in main climate zone D
dat.tmp_rs1[dat.tmp_rs1$zone_c == "D", ]
dat.tmp_rs2[dat.tmp_rs2$zone_c == "D", ]
dat.tmp_rs3[dat.tmp_rs3$zone_c == "D", ]
id.vec_D1	<- sample(1:nrow(dat.tmp_rs1[dat.tmp_rs1$zone_c == "D", ]))
id.vec_D2	<- sample(1:nrow(dat.tmp_rs2[dat.tmp_rs2$zone_c == "D", ]))
id.vec_D3	<- sample(1:nrow(dat.tmp_rs3[dat.tmp_rs3$zone_c == "D", ]))

#subsample from d estimates and combine subsamples of the three main climate zones
dat_rs	<- rbind(dat.tmp_rs1[dat.tmp_rs1$zone_c == "B", ][id.vec_B1 < n_perCZ+1, ],
			dat.tmp_rs2[dat.tmp_rs2$zone_c == "B", ][id.vec_B2 < n_perCZ+1, ],
			dat.tmp_rs3[dat.tmp_rs3$zone_c == "B", ][id.vec_B3 < n_perCZ+1, ],
			dat.tmp_rs1[dat.tmp_rs1$zone_c == "C", ][id.vec_C1 < n_perCZ+1, ],
			dat.tmp_rs2[dat.tmp_rs2$zone_c == "C", ][id.vec_C2 < n_perCZ+1, ],
			dat.tmp_rs3[dat.tmp_rs3$zone_c == "C", ][id.vec_C3 < n_perCZ+1, ],
			dat.tmp_rs1[dat.tmp_rs1$zone_c == "D", ][id.vec_D1 < n_perCZ+1, ],
			dat.tmp_rs2[dat.tmp_rs2$zone_c == "D", ][id.vec_D2 < n_perCZ+1, ],
			dat.tmp_rs3[dat.tmp_rs3$zone_c == "D", ][id.vec_D3 < n_perCZ+1, ])

#extract respective columns of 'dat_demeded' (actual time series data)
dat_demeded_rs	<- dat_demeded[, colnames(dat_demeded) %in% dat_rs$statID]

#extract respective columns of 'refMeds' (monthly Medians for computing anomalies)
refMeds_rs		<- refMeds[, colnames(refMeds) %in% dat_rs$statID]

#save dataset
save(dat_rs, dat_demeded_rs, refMeds_rs, file = "raindata_demeded_RS_2025-01-16.RData")
