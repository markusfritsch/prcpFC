###########################################################################################
### R-Basics and data
###########################################################################################

###
###	Basics
###


###	Load libraries

#	install.packages("sf")
	library(sf)
#	install.packages("terra")
	library(terra)
#	install.packages("sp")
	library(sp)
#	install.packages("raster")
	library(raster)
#	install.packages("rasterVis") #bietet einfachere Visualisierung von Raster
#	library(rasterVis)
#	install.packages("rgdal")
#	library(rgdal)
#	install.packages("rworldmap")
	library(rworldmap)
#	install.packages("usmap")
	library(usmap)
#	install.packages("maps")
#	library(maps)
#	install.packages("mapdata")
#	library(mapdata)
#	install.packages("ggplot2")
	library(ggplot2)
#	install.packages("RColorBrewer")
#	library(RColorBrewer)
#	install.packages("viridis")
	library(viridis)
#	install.packages("gridExtra")
	library(gridExtra)
#	install.packages("ggmap")
	library(ggmap)
#	install.package("dplyr")
	library(dplyr)
#	install.packages("devtools")
	library(devtools)
#	install.packages("xtable")
	library(xtable)
	install_github("markusfritsch/prcpFC")
	library(prcpFC)



###	Clear Workspace

rm(list=ls())






###	Specify colors


cols.tmp		<- c(
#				"#f58080"	# lightred
				"#efa540"	# orange
				,"#2266ee"	# blue
				,"#aabb55"	# lightgreen
				,"#c4bdb7"	# lightgrey
				,"#827f7b"	# grey
				,"#6b625d"	# darkgrey
#				,"#830433"	# darkred
				,"#c10f0f"	# red
				,"#6978c6" 	# blue velvet
				,"#FAFAD2"	# light golden rod	
				)






###
### Load station meta data with koppen climate zone classification and precipitation time series
###


data(raindata6019)
data(ghcndStations)


stat2.2 		<- cbind(ghcndStations, ghcndStations$statID %in% colnames(raindata6019))
names(stat2.2)[12]	<- "is.in.data"


###	Compile anomaly data based on imputed time series

data("raindata6019")
dat.tmp			<- raindata6019[,-1]
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

dat_demeaned	<- dat.tmp - apply(refMeans, FUN = rep, MARGIN = 2, times = 60)
dat_demeded	<- dat.tmp - apply(refMeds, FUN = rep, MARGIN = 2, times = 60)




###
###	Create objects with station IDs according to region
###


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

stat_usContmT	<- usmap::usmap_transform(stat_usCont)
stat_usAllmT	<- usmap::usmap_transform(stat_usAll)



mp <- NULL
mapWorld <- ggplot2::borders("world", colour = cols.tmp[5], fill=NA) # create a layer of borders
















###
###	Fig.1: Map of Contiguous US with points colored according to precipitation
###


stations		<- sf::st_as_sf(stat_usCont, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
stations$group	<- 1

us 		<- ggplot2::map_data("usa")
usStates	<- ggplot2::map_data("state")

data(koppen)
koppen[, 3]		<- factor(koppen[, 3])
names(koppen)
coordinates(koppen)	<- ~ Lon + Lat
gridded(koppen)		<- TRUE
raster2		<- raster::raster(koppen)


ras			<- as(raster2, "SpatRaster")
boundsUS		<- sf::st_as_sf(us[, c(1:2)], coords = c("long", "lat"))
rasterUS		<- raster::crop(ras, extent(boundsUS))
#rasterUS		<- raster::mask(rasterUS, boundsUS)
rasterUSDF		<- as.data.frame(rasterUS, xy = TRUE)
colnames(rasterUSDF)[3]		<- "KoppenCliZs"

rasterUSDF_sf	<- sf::st_as_sf(rasterUSDF, coords = c("x", "y"), crs = "EPSG:4326")
rasterUSDF_sf	<- sf::st_transform(x = rasterUSDF_sf, crs = "EPSG:3857", desired_accuracy = 1, allow_ballpark = FALSE)

bndryUS		<- sf::st_as_sf(maps::map("usa", plot = FALSE, fill = FALSE))
bndryUS		<- sf::st_transform(x = bndryUS, crs = "EPSG:3857", desired_accuracy = 1, allow_ballpark = FALSE)
bndryUSc		<- bndryUS[1,]
bndryUS		<- sf::st_transform(x = bndryUS, crs = "EPSG:4326", desired_accuracy = 1, allow_ballpark = FALSE)

rasterUSDF_sf	<- sf::st_intersection(st_make_valid(rasterUSDF_sf), st_make_valid(bndryUSc), sparse = FALSE)
rasterUSDF_sf	<- sf::st_transform(rasterUSDF_sf, crs = "EPSG:4326", desired_accuracy = 1)
rasterUSDF		<- cbind(as.data.frame(rasterUSDF_sf, xy = TRUE),
				x = sf::st_coordinates(rasterUSDF_sf)[,1], y = sf::st_coordinates(rasterUSDF_sf)[,2])

statesUS		<- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

mapUSstates	<- ggplot2::borders(database = "state", colour = cols.tmp[5], fill = NA) # create a layer of borders

m.p	<- ggplot() + 
	geom_raster(data = rasterUSDF, aes(x = x, y = y, fill = KoppenCliZs)) +
	scale_fill_viridis_d(name = "Climate Zone",
			limits = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk",
				"Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc",
				"Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
				"EF", "ET"), alpha = 0.5) +
	mapUSstates +
	geom_point(data = ghcndStations[ghcndStations$statID %in% stat_usContmT$statID, ], aes(y = lat, x = lon),					# ts used in analysis
		 color = cols.tmp[2], cex = 0.8, pch = 20) +
	coord_equal() +
#	coord_fixed(1.5) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
		axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
		legend.title = element_text(size = 20), legend.text = element_text(size = 20),
		panel.background=element_blank())
#pdf(file = "img/koppenClimZsUSstates.pdf", width=12, height=8)
m.p
#dev.off()


tib.index			<- names(dat_demeded) %in% stat_usContmT$statID
dat.tmp			<- as.data.frame(dat_demeded[,tib.index])
median(apply(dat.tmp, FUN = median, MARGIN = 2))
range(apply(dat.tmp, FUN = median, MARGIN = 2))
quantile(apply(dat.tmp, FUN = median, MARGIN = 2),
	probs = c(0, 0.005, 0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 0.995, 1) )

#check:
median(apply(dat.tmp[1:360, ], FUN = median, MARGIN = 2))	#ok
range(apply(dat.tmp[1:360, ], FUN = median, MARGIN = 2))	#ok
quantile(apply(dat.tmp[1:360, ], FUN = median, MARGIN = 2),
	probs = c(0, 0.005, 0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 0.995, 1) )	#ok

median(apply(dat.tmp[361:720, ], FUN = median, MARGIN = 2))
range(apply(dat.tmp[361:720, ], FUN = median, MARGIN = 2))
quantile(apply(dat.tmp[361:720, ], FUN = median, MARGIN = 2),
	probs = c(0, 0.005, 0.01, 0.02, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.98, 0.99, 0.995, 1) )



dat.tmp1			<- data.frame(unlist(apply(dat.tmp[361:720, ], FUN = median, MARGIN = 2)))
stations1			<- cbind(stations, dat.tmp1)
colnames(stations1)[14]	<- "medianPrcpAno"
median(dat.tmp[, "USC00253185"])
mean(dat.tmp[, "USC00253185"])
max(dat.tmp[, "USC00253185"])
which.max(dat.tmp[, "USC00253185"])
median(dat.tmp[361:720, "USC00253185"])
mean(dat.tmp[361:720, "USC00253185"])
max(dat.tmp[361:720, "USC00253185"])
which.max(dat.tmp[361:720, "USC00253185"])
#note that obs. 57 has very high precipitation measurement
#stations1[which.min(stations1$meanPrcpAno), "meanPrcpAno"]	<- min(stations1$meanPrcpAno[-which.min(stations1$meanPrcpAno)])
#stations1			<- stations1[-which.min(stations1$meanPrcpAno), ]


m.p1	<- ggplot() + 
#	geom_raster(data = rasterUSDF, aes(x = x, y = y, fill = KoppenCliZs)) +
#	scale_fill_viridis_d(name = "Climate zone",
#			limits = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk",
#				"Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc",
#				"Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
#				"EF", "ET"), alpha = 0.5) +
	mapUSstates +
	geom_point(data = stations1, aes(y = lat, x = lon, color = medianPrcpAno),		# ts used in analysis
		 cex = 4.0, pch = 20) +
	coord_equal() +
	scale_color_gradient2(name = "Precipitation\nanomaly",
					low = cols.tmp[7], mid = cols.tmp[9], high = cols.tmp[6]) +
	guides(fill = guide_legend(order = 2), color = guide_colorbar(order = 1)) +
#	scale_color_viridis(name = "Median precipitation anomaly", option = "viridis") +
#	coord_fixed(1.5) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
		axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
		legend.title = element_text(size = 20), legend.text = element_text(size = 20),
		panel.background=element_blank())
#pdf(file = "img/medianPrcpAnoKoppenUSstatesWoKoppen.pdf", width=12, height=8)
m.p1
#dev.off()



m.p2	<- ggplot() + 
	geom_raster(data = rasterUSDF, aes(x = x, y = y, fill = KoppenCliZs)) +
	scale_fill_viridis_d(name = "Climate zone",
			limits = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk",
				"Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc",
				"Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
				"EF", "ET"), alpha = 0.5) +
	mapUSstates +
	geom_point(data = stations1, aes(y = lat, x = lon, color = medianPrcpAno),		# ts used in analysis
		 cex = 3.0, pch = 20) +
	coord_equal() +
	scale_color_gradient2(name = "Precipitation\nanomaly", limits = c(-200,200),
					low = cols.tmp[7], mid = cols.tmp[9], high = cols.tmp[6]) +
	guides(fill = guide_legend(order = 2), color = guide_colorbar(order = 1)) +
#	scale_color_viridis(name = "Median precipitation anomaly", option = "viridis") +
#	coord_fixed(1.5) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
		axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
		legend.title = element_text(size = 20), legend.text = element_text(size = 20), 
		panel.background=element_blank())
#pdf(file = "img/medianPrcpAnoKoppenUSstates.pdf", width=12, height=8)
m.p2
#dev.off()


#pdf(file = "img/meanPrcpAnoKoppenUSstates.pdf", width=16, height=10)
gridExtra::grid.arrange(m.p, m.p1, 
	nrow = 2)
#dev.off()








###
###	Fig.2: Violin plot and cumulative empirical distribution function
###


dat	<- dat_demeded

dat	<- dat[dat$statID %in% stat_usContmT$statID, ]



#JS created figure and also has code!




#statement on medians of precipitation anomalies for 1990 until 2019

mat6089	<- matrix(nrow = 6, ncol = 12)
mat9019	<- matrix(nrow = 6, ncol = 12)

for(i in 1:12){
  mat6089[1:5,i]	<- quantile(dat[dat$timeIndex < 361 & dat$month == i, "precAno"], probs = c(0.1,0.25, 0.5, 0.75, 0.9))
  mat6089[6,i]	<- mean(dat[dat$timeIndex < 361 & dat$month == i, "precAno"])
  mat9019[1:5,i]	<- quantile(dat[dat$timeIndex > 360 & dat$month == i, "precAno"], probs = c(0.1,0.25, 0.5, 0.75, 0.9))
  mat9019[6,i]	<- mean(dat[dat$timeIndex > 360 & dat$month == i, "precAno"])
}
mat6089
mat9019

mat9019[3,] - mat6089[3,]
#increases in medians for all months, but Sep and Nov

mat9019[5,] - mat6089[5,]
mat9019[4,] - mat6089[4,]
#increases in 90- and 75%-quantiles for all months - except Mar, Sep, and Nov

mat9019[2,] - mat6089[2,]
mat9019[1,] - mat6089[1,]
#3(4) decreases in 25(10)%-quantiles for all months - except Feb, Mar, May, Sep, and Nov

mat6089[5,] - mat6089[1,]
mat9019[5,] - mat9019[1,]
#difference in range of 90 and 10%-quantile increases

mat6089[4,] - mat6089[2,]
mat9019[4,] - mat9019[2,]
#difference in range of 75 and 25%-quantile increases

mat6089[6,] - mat6089[3,]
mat9019[6,] - mat9019[3,]
#difference between mean and median increase for all months compared to reference period - except March, September, November










###
###	Fig.3: Plots for three main climate zones in Contiguous US from 1960 to 2019 (time series plots)
###


czs.tmp	<- c("B", "C", "D")
ymin		<- -800
ymax		<- 1200

for(i in 1:length(czs.tmp)){

  cz.tmp	<- czs.tmp[i]

  poi.index	<- stat_usContmT$statID[stat_usContmT$koppen2.cz == cz.tmp]

  tib.index	<- colnames(dat_demeded) %in% poi.index

  dat.tmp	<- dat_demeded[ , tib.index]
  dat.tmp1	<- data.frame(cbind(timeIndex = 1:720, prcpAno = dat.tmp[,2], year = (1:720)/12 + 1960))
  dat.tmp2	<- data.frame(cbind(timeIndex = 1:720, medAno = apply(X = dat.tmp, MARGIN = 1, FUN = median)), year = (1:720)/12 + 1960)
  dat.tmp3	<- data.frame(cbind(timeIndex = 1:720, quantLoAno = apply(X = dat.tmp, MARGIN = 1, FUN = quantile, probs = 0.1),
					quantUpAno = apply(X = dat.tmp[i+(0:59)*12,], MARGIN = 1, FUN = quantile, probs = 0.9)), year = (1:720)/12 + 1960)
  dat.tmp4	<- data.frame(cbind(timeIndex = 1:720, quantLoAno = apply(X = dat.tmp, MARGIN = 1, FUN = quantile, probs = 0.25),
					quantUpAno = apply(X = dat.tmp, MARGIN = 1, FUN = quantile, probs = 0.75)), year = (1:720)/12 + 1960)

  if(i != 3){
    l.p	<- ggplot() +
		geom_hline(yintercept = 0, lty = 1, col = cols.tmp[6]) +
		geom_ribbon(data = dat.tmp3, aes(x = year, ymax = quantUpAno, ymin = quantLoAno), fill = cols.tmp[4], alpha = .5) +
		geom_ribbon(data = dat.tmp4, aes(x = year, ymax = quantUpAno, ymin = quantLoAno), fill = cols.tmp[5], alpha = .5) +
		geom_line(data = dat.tmp2, aes(x = year, y = medAno), col = cols.tmp[8], lwd = 0.3) +
		xlab("") + ylab(cz.tmp) +
#		xlab("") + ylab(paste(m.tmp)) + ggtitle("Main climate zone B") +
		ylim(ymin,ymax) +
		theme(panel.background=element_blank(),
			axis.text.x=element_blank(),
			axis.ticks.x=element_blank())
  } else{
    l.p	<- ggplot() +
		geom_hline(yintercept = 0, lty = 1, col = cols.tmp[6]) +
		geom_ribbon(data = dat.tmp3, aes(x = year, ymax = quantUpAno, ymin = quantLoAno), fill = cols.tmp[4], alpha = .5) +
		geom_ribbon(data = dat.tmp4, aes(x = year, ymax = quantUpAno, ymin = quantLoAno), fill = cols.tmp[5], alpha = .5) +
		geom_line(data = dat.tmp2, aes(x = year, y = medAno), col = cols.tmp[8], lwd = 0.3) +
		xlab("") + ylab(cz.tmp) +
#		xlab("") + ylab(paste(m.tmp)) + ggtitle("Main climate zone B") +
		ylim(ymin,ymax) +
		theme(panel.background=element_blank())
  }

  assign(paste("l.p", cz.tmp, sep = ""), value = l.p)

}


#pdf(file = "img/stationsPrcpBCD.pdf", width = 10, height = 10)
gridExtra::grid.arrange(l.pB, l.pC, l.pD,
	nrow = 3)
#dev.off()








dat.tmp0 			<- data.frame(unlist(dat.tmp))
dat.tmp0			<- cbind(rep(colnames(dat.tmp), each = length(rownames(dat.tmp))), dat.tmp0)
dat.tmp0			<- cbind(rep(1:length(rownames(dat.tmp)), times = length(colnames(dat.tmp))),
					rep(1:12, times = length(unique(dat.tmp0[,1]))), 
					dat.tmp0, year = rep((1:720)/12 + 1960, times = ncol(dat.tmp)))
rownames(dat.tmp0)	<- NULL
colnames(dat.tmp0)	<- c("timeIndex", "month", "statID", "prcpAno", "year")
#number and percentatge outside of range of values when setting limit of y axis to 1500
nrow(dat.tmp0[dat.tmp0$prcpAno > ymax, ])
(nrow(dat.tmp0[dat.tmp0$prcpAno > ymax, ])/nrow(dat.tmp0))*100
quantile(dat.tmp0[dat.tmp0$prcpAno > ymax, "prcpAno"], probs = c(0.1, 0.25, 0.5, 0.75, 0.9))
#number and percentatge outside of range of values when setting limit of y axis to -1000
nrow(dat.tmp0[dat.tmp0$prcpAno < ymin, ])
(nrow(dat.tmp0[dat.tmp0$prcpAno < ymin, ])/nrow(dat.tmp0))*100
quantile(dat.tmp0[dat.tmp0$prcpAno < ymin, "prcpAno"], probs = c(0.1, 0.25, 0.5, 0.75, 0.9))

#for plotting, replace all values > 1500 or < -1000 by this value
#dat.tmp[dat.tmp >= ymax-200]	<- ymax - 200
#dat.tmp[dat.tmp <= ymin+200]	<- ymin + 200




# analysis of extreme precipitation

#more than 10 meters
dat.tmp[dat.tmp> 10000]
length(dat.tmp[dat.tmp> 10000])
which(apply(FUN = max, dat.tmp, MARGIN = 2) > 10000)

#more than 20 meters
dat.tmp[dat.tmp> 20000]
length(dat.tmp[dat.tmp> 20000])
which(dat.tmp> 20000)
which(apply(FUN = max, dat.tmp, MARGIN = 2) > 20000)

#more than 30 meters
dat.tmp[dat.tmp> 30000]
length(dat.tmp[dat.tmp> 30000])
which(dat.tmp> 30000)
which(apply(FUN = max, dat.tmp, MARGIN = 2) > 30000)










###
###	Sec. 5.1 and B.1: Analysis of long range dependence parameter estimates
###


data(rainresults)

dat	<- rainresults
dat	<- na.omit(dat)

dat$estType		<- NA
for(i in 1:nrow(dat)){
  if(sum(dat[i, c(9:11, 13)]) > 0){
    if(sum(dat[i, c(12)]) > 0){
      dat$estType[i]	<- "houLWmixed"
    } else{
      dat$estType[i]	<- "houLW"
    }
  } else{
    if(sum(dat[i, c(12)]) > 0){
      dat$estType[i]	<- "LWmixed"
    } else{
      dat$estType[i]	<- "LW"
    }
  }
}


#stations in Contiguous US
stat_contUS		<- dat[dat$statID %in% stat_usContmT$statID, ]
colnames(stat_contUS)[18]	<- "dstar"
# column 'dstar' is LRD estimate accounting for potential contaminations
# column 'LW' contains LRD estimate not accounting for contaminations


###	Diagnosed sources of spurious long memory, mean breaks, and short-run dynamics

#Tab. 2: Contiguous US 
mat_contUS	<- rbind(c(sum(stat_contUS$quspurious5), sum(stat_contUS$meanbreak), sum(stat_contUS$potentially_contaminated),
			sum(stat_contUS$armanoise)),
		c(sum(!stat_contUS$quspurious5), sum(!stat_contUS$meanbreak), sum(!stat_contUS$potentially_contaminated),
			sum(!stat_contUS$armanoise)))
mat_contUS	<- rbind(mat_contUS, round(mat_contUS[1,]/(mat_contUS[1,] + mat_contUS[2,]), digits = 3)*100 )
colnames(mat_contUS)	<- c("quSprs", "meanBrk", "potCtmd", "armaNoise")
mat_contUS


#Tab B.7: World
colnames(dat)[18]	<- "dstar"
stat_world	<- dat[dat$statID %in% statID.AusWOna | dat$statID %in% statID.EUWOna | dat$statID %in% statID.USallWOna, ]
mat_world	<- rbind(c(sum(stat_world$quspurious5), sum(stat_world$meanbreak), sum(stat_world$potentially_contaminated),
			sum(stat_world$armanoise)),
		c(sum(!stat_world$quspurious5), sum(!stat_world$meanbreak), sum(!stat_world$potentially_contaminated),
			sum(!stat_world$armanoise)))
mat_world	<- rbind(mat_world, round(mat_world[1,]/(mat_world[1,] + mat_world[2,]), digits = 3)*100 )
colnames(mat_world)	<- c("quSprs", "meanBrk", "potCtmd", "armaNoise")
mat_world



###	Variation in memory parameter estimates

#Contiguous US: Numbers in text
quantile(stat_contUS$LW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(stat_contUS$LW)
quantile(stat_contUS$houLW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
quantile(stat_contUS$dstar, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(stat_contUS$dstar)
#increase in standard deviation of LRD parameter estimate
(sd(stat_contUS$dstar) - sd(stat_contUS$LW))/sd(stat_contUS$LW)
#standard deviation does not change much when excluding the very low value
(sd(stat_contUS$dstar[-969]) - sd(stat_contUS$LW[-969]))/sd(stat_contUS$LW[-969])

stat_contUS$LW[969]
stat_contUS$dstar[969]
#LRD parameter estimate changes substantially and indicates anti-persistence after the adjustment (before: LRD)

stat_contUS$potentially_contaminated[stat_contUS$meanbreak]
#why does this occur? This does not match our definition.

quantile(stat_contUS$ddiff, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
plot(sort(stat_contUS$ddiff))
#susbtantial changes in LRD parameter estimates are the exception
plot(density(stat_contUS$ddiff))
mean(stat_contUS$ddiff)
#difference in memory parameter estimates is right-skewed, which means that
# the memory parameter tends to decrease when accounting for potential
# contaminations and short-range dependence (ddiff = LW-dstar)



#World: Numbers in text
quantile(dat$LW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(dat$LW)
quantile(dat$houLW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
quantile(dat$dstar, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(dat$dstar)
#increase in standard deviation of LRD parameter estimate
(sd(dat$dstar) - sd(dat$LW))/sd(dat$LW)





#Fig. 4: Contiguous US: histogram long range dependence parameter when accounting for potential contaminations
h.pLW	<- ggplot(data = stat_contUS, aes(x = LW)) +
	geom_vline(xintercept = 0, lty = 2, col = cols.tmp[6], alpha = 0.5) +
#	geom_vline(xintercept = mean(stat_contUS$LW), lty = 2, col = cols.tmp[2], alpha = 0.5) +
#	geom_vline(xintercept = mean(stat_contUS$LW) - 2*sd(stat_contUS$LW), lty = 2) +
#	geom_vline(xintercept = mean(stat_contUS$LW) + 2*sd(stat_contUS$LW), lty = 2) +
	geom_histogram(col = cols.tmp[4], alpha = 0.5) +
	xlim(c(-0.52, 0.52)) + ylim(c(0,300)) +
	xlab(expression(italic(hat(d)[LW]))) + ylab("") +
	theme(panel.background=element_blank())

h.pDstar	<- ggplot(data = stat_contUS, aes(x = dstar)) +
	geom_vline(xintercept = 0, lty = 2, col = cols.tmp[6], alpha = 0.5) +
#	geom_vline(xintercept = mean(stat_contUS$dstar), lty = 2, col = cols.tmp[2], alpha = 0.5) +
#	geom_vline(xintercept = mean(stat_contUS$dstar) - 2*sd(stat_contUS$dstar), lty = 2) +
#	geom_vline(xintercept = mean(stat_contUS$dstar) + 2*sd(stat_contUS$dstar), lty = 2) +
	geom_histogram(col = cols.tmp[4], alpha = 0.5) +
	xlim(c(-0.52, 0.52)) + ylim(c(0,300)) +
	xlab(expression(italic(hat(d))~"*")) + ylab("") +
	theme(panel.background=element_blank())

#pdf(file = "img/histsLRD.pdf", width=8, height=3)
gridExtra::grid.arrange(h.pLW, h.pDstar, 
	nrow = 1)
#dev.off()



#Fig. B.11: World: histogram long range dependence parameter when accounting for potential contaminations (world)
h.pLW_world	<- ggplot(data = dat, aes(x = LW)) +
	geom_vline(xintercept = 0, lty = 2, col = cols.tmp[6], alpha = 0.5) +
#	geom_vline(xintercept = mean(dat$LW), lty = 2, col = cols.tmp[2], alpha = 0.5) +
#	geom_vline(xintercept = mean(dat$LW) - 2*sd(dat$LW), lty = 2) +
#	geom_vline(xintercept = mean(dat$LW) + 2*sd(dat$LW), lty = 2) +
	geom_histogram(col = cols.tmp[4], alpha = 0.5) +
	xlim(c(-0.52, 0.52)) + ylim(c(0,500)) +
	xlab(expression(italic(hat(d)[LW]))) + ylab("") +
	theme(panel.background=element_blank())

h.pDstar_world	<- ggplot(data = dat, aes(x = dstar)) +
	geom_vline(xintercept = 0, lty = 2, col = cols.tmp[6], alpha = 0.5) +
#	geom_vline(xintercept = mean(dat$dstar), lty = 2, col = cols.tmp[2], alpha = 0.5) +
#	geom_vline(xintercept = mean(dat$dstar) - 2*sd(dat$dstar), lty = 2) +
#	geom_vline(xintercept = mean(dat$dstar) + 2*sd(dat$dstar), lty = 2) +
	geom_histogram(col = cols.tmp[4], alpha = 0.5) +
	xlim(c(-0.52, 0.52)) + ylim(c(0,500)) +
	xlab(expression(italic(hat(d))~"*")) + ylab("") +
	theme(panel.background=element_blank())

#pdf(file = "img/histsLRDworld.pdf", width=8, height=3)
gridExtra::grid.arrange(h.pLW_world, h.pDstar_world, 
	nrow = 1)
#dev.off()



# Contiguous US: Five number summary and mean
mat2_contUS	<- rbind(dLW = summary(stat_contUS$LW), dstar = summary(stat_contUS$dstar))
xtable(mat2_contUS, digits = 2)



# World: Five number summary and mean
mat2_world	<- rbind(dLW = summary(dat$LW), dstar = summary(dat$dstar))
xtable(mat2_world, digits = 2)






###
###	 Scatterplots and tables of the two long range dependence parameter estimates
###


#Fig.5: Contiguous US: scatterplot LW vs. dstar
d.p	<- ggplot() +
	geom_abline(intercept = 0, slope = 1, col = cols.tmp[4], lwd = 1) +
	geom_point(data = stat_contUS, aes(x = LW, y = dstar), col = cols.tmp[8], cex = 1.2) +
#	geom_point(data = stat_contUS, aes(x = LW, y = dstar), col = cols.tmp[6], cex = 1.2) +
	xlab(expression(italic(hat(d)[LW]))) + ylab(expression(italic(hat(d))~"*")) +
	theme(panel.background=element_blank())
#pdf(file = "img/scPlotDandDstar.pdf", width=8, height=4)
d.p
#dev.off()



#Fig B.12: World: scatterplot LW vs. dstar
d.p	<- ggplot() +
	geom_abline(intercept = 0, slope = 1, col = cols.tmp[4], lwd = 1) +
	geom_point(data = dat, aes(x = LW, y = dstar), col = cols.tmp[8], cex = 1.2) +
#	geom_point(data = dat, aes(x = LW, y = dstar), col = cols.tmp[6], cex = 1.2) +
	xlab(expression(italic(hat(d)[LW]))) + ylab(expression(italic(hat(d))~"*")) +
	theme(panel.background=element_blank())
#pdf(file = "img/scPlotDandDstarWorld.pdf", width=8, height=4)
d.p
#dev.off()



#Tab.3: Contiguous US: Table number of anomaly series falling into three regions

matLRD	<- cbind(c(
		sum(stat_contUS$LW <= -0.1),
		sum(stat_contUS$LW <= 0.1 & stat_contUS$LW >= -0.1),
		sum(stat_contUS$LW >=0.1)),
		c(sum(stat_contUS$dstar <= -0.1),
		sum(stat_contUS$dstar <= 0.1 & stat_contUS$dstar >= -0.1),
		sum(stat_contUS$dstar >=0.1))
		)
xtable(matLRD)



#Tab.B.8: World: Table number of anomaly series falling into three regions

matLRDworld	<- cbind(c(
			sum(dat$LW <= -0.1),
			sum(dat$LW <= 0.1 & dat$LW >= -0.1),
			sum(dat$LW >=0.1)),
			c(sum(dat$dstar <= -0.1),
			sum(dat$dstar <= 0.1 & dat$dstar >= -0.1),
			sum(dat$dstar >=0.1))
			)
xtable(matLRDworld)







###
###	Fig.6: Maps on long range dependence parameter estimates
###


#map on LW estimates

d.range	<- c(min(stat_contUS$LW, stat_contUS$dstar), max(stat_contUS$LW, stat_contUS$dstar))

m.pD	<- ggplot() + 
	mapUSstates +
	geom_point(data = stat_contUS, aes(y = lat, x = lon, color = LW),		# ts used in analysis
		 cex = 2.0, pch = 20) +
	coord_equal() +
	scale_color_gradient2(name = expression(italic(hat(d)[LW])), limits = d.range, 
					low = cols.tmp[7], mid = cols.tmp[9], high = cols.tmp[6]) +
	guides(fill = guide_legend(order = 2), color = guide_colorbar(order = 1)) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
	  panel.background=element_blank())
#pdf(file = "img/meanPrcpAnoKoppenUSstatesWoKoppen.pdf", width=12, height=8)
m.pD
#dev.off()



#map on adjusted LW estimates

m.p2	<- ggplot() + 
	mapUSstates +
	geom_point(data = stat_contUS, aes(y = lat, x = lon, color = dstar),		# ts used in analysis
		 cex = 2.0, pch = 20) +
	coord_equal() +
	scale_color_gradient2(name = expression(italic(hat(d))~"*"), limits = d.range, 
					low = cols.tmp[7], mid = cols.tmp[9], high = cols.tmp[6]) +
	guides(fill = guide_legend(order = 2), color = guide_colorbar(order = 1)) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
	  panel.background=element_blank())
#pdf(file = "img/mapsLWestimates.pdf", width=8, height=8)
gridExtra::grid.arrange(m.pD, m.p2, nrow = 2)
#dev.off()


#pdf(file = "img/mapsLWestimates2plots.pdf", width=8, height=8)
gridExtra::grid.arrange(m.p, m.p2, nrow = 2)
#dev.off()







###
###	Fig.7: Pairwise precipitation anomaly correlations vs. pairwise Euclidean Distance for each memory behavior
###



#JS created figure and also has code! 









###
###	Appendix Fig A.9: Season plot of anomaly series of Contiguous US for months from 1960 until 2020 next to each other (seasonal plot)
###


stat_usContmT	<- stat_usContmT[stat_usContmT$koppen2.cz == "B" |
					stat_usContmT$koppen2.cz == "C" |
					stat_usContmT$koppen2.cz == "D" ,]


statIDs		<- stat_usContmT$statID

dat.tmp		<- dat_demeded


dat.tmp1		<- as.data.frame(cbind(index = 1:720, month_idx = rep((0:11)*60, times = 60) + rep(1:60, each = 12),
				month = rep(1:12, times = 60), year = (0:59 + 1960),
				monthlyMean = monthMedians_allUSStat, monthlyRefMean = rep(monthRefMedians_allUSStat, times = 60),
				x.start = rep((0:11)*60, times = 60) + 1, x.end = rep((1:12)*60, times = 60)))
#dat.tmp1		<- as.data.frame(cbind(index = 1:360, month_idx = rep((0:11)*30, times = 30) + rep(1:30, each = 12),
#				month = rep(1:12, times = 30), year = (0:29 + 1960),
#				monthlyMean = monthMedians_allUSStat, monthlyRefMean = rep(monthRefMedians_allUSStat, times = 30),
#				x.start = rep((0:11)*30, times = 30) + 1, x.end = rep((1:12)*30, times = 30)))

lab			<- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

id.tmp1		<- (0:11)*60 + 1
id.tmp2		<- (1:12)*60
#id.tmp1		<- (0:11)*30 + 1
#id.tmp2		<- (1:12)*30

seasPlot	<- ggplot(data = dat.tmp1) +
	geom_segment(aes(x = x.start, xend = x.end, y = monthlyRefMean, yend = monthlyRefMean, col = "Mean monthly prec. \n 1960-1989")) +
	geom_line(aes(x = month_idx, y = monthlyMean, group = month, col = "Mean prec. \n 1961-2019")) +
	geom_vline(xintercept = id.tmp2, lty = 2, col = cols.tmp[4]) +
	labs(y = "", x = "") +
#	labs(y = "Monthly mean precipitation (mm)", x = "Month") +
	scale_color_manual(values = c("Mean monthly prec. \n 1960-1989" = cols.tmp[6], "Mean prec. \n 1961-2019" = cols.tmp[8]), 
                       name = element_blank()) + 
	scale_x_continuous(breaks = (id.tmp1 + id.tmp2)/2, labels = lab) +
	theme(legend.position = "none",
		axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
		panel.background = element_blank())

#pdf(file = "img/seasonPlot.pdf", width=8, height=4)
seasPlot
#dev.off()











###
###	Descriptives for Appendix
###


nrow(stat_contUS)
mean(stat_contUS$lon)
mean(stat_contUS$lat)

table(stat_contUS$zone_c)
table(stat_contUS$zone_p)
table(stat_contUS$zone_t)

table(stat_contUS$quspurious5)/nrow(stat_contUS)
table(stat_contUS$meanbreak)/nrow(stat_contUS)
table(stat_contUS$potentially_contaminated)/nrow(stat_contUS)
table(stat_contUS$armanoise)/nrow(stat_contUS)



# for world, see R script 'Rain_cat_np_2023-04-19.R'





























###
###	Map of all stations with climate zone classification and table of climate zone classification for Continental US
###


#worldwide

nrow(stat2.2[stat2.2$is.in.data & (stat2.2$statID %in% RTS.Adj.Ti_Tend[, "statID"][RTS.Adj.Ti_Tend[ , "misObs"] == 0]), ])
stat.tmp	<- stat2.2[stat2.2$is.in.data & (stat2.2$statID %in% RTS.Adj.Ti_Tend[, "statID"][RTS.Adj.Ti_Tend[ , "misObs"] == 0]), ]
stat.tmp2	<- stat.tmp[!is.na(stat.tmp$koppen2), ]

gmapK		<- ggplot() + 
	mapWorld +
	geom_raster(data = raster2DF, aes(x = x, y = y, fill = KoppenCliZs)) +
	scale_fill_viridis_d(name = "Climate Zone",
			limits = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk",
				"Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc",
				"Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
				"EF", "ET"), alpha = 0.5) +
	geom_point(data = stat.tmp2, aes(x = lon, y = lat), color = cols.tmp[2], cex = 0.4, pch = 20) +
	labs(y = "", x = "") +
#	guides(color = guide_legend(title = "Koppen Climate Zones")) +
	theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
		axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
		panel.background = element_blank())
pdf(file = "img/koppenClimZs.pdf", width=12, height=8)
gmapK
dev.off()



#US

m.p	<- plot_usmap(regions = "states") + 
#	geom_point(data = stat_usAllmT, aes(y = y, x = x),				# all available ts
#		col = "grey", cex = 0.3, pch = 20) +
	geom_point(data = stat_usContmT, aes(y = y, x = x),					# ts used in analysis
		 color = cols.tmp[8], cex = 0.4, pch = 20) +
#		 color = "purple", cex = 0.4, pch = 20) +
	geom_point(data = stat_usContmT[poi.index,], aes(y = y, x = x),			# plot exemplary station
		 color = "darkgrey", cex = 4, pch = 1) +
	theme(panel.background=element_blank())
#pdf(file = "img/stationsUSall.pdf", width = 12, height = 8)
plot(m.p)
#dev.off()



us 		<- map_data("usa")
usStates	<- map_data("state")

stations		<- st_as_sf(stat_usContmT, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4326")
stations$group	<- 1

m.p	<- ggplot(data = us, aes(x = long, y = lat, group = group)) + 
	geom_polygon(fill = cols.tmp[4], alpha = 0.5) + 
	geom_polygon(data = usStates, color = "white", fill = NA) + 
	geom_point(data = stations, aes(y = lat, x = lon, shape = koppen2.cz),			# ts used in analysis
		 color = cols.tmp[8], cex = 0.9) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
	coord_fixed(1.3) +
	labs(shape = "Climate zone") +
	theme(panel.background=element_blank())
#pdf(file = "img/stationsContUS.pdf", width = 12, height = 8)
plot(m.p)
#dev.off()


table(stat_usContmT$koppen2.cz)
table(stat_usContmT$koppen2.ps)
table(stat_usContmT$koppen2.ts)


xtable(t(table(stat_usContmT$koppen2)))





#full worldmap with koppen climate zones
raster2DF			<- as.data.frame(raster2, xy = TRUE)
names(raster2DF)[3]	<- "KoppenCliZs"

ggplot(data = raster2DF) + 
	mapWorld +
	geom_raster(aes(x = x, y = y, fill = KoppenCliZs)) +
	scale_fill_viridis_d(alpha = 0.5) +
#	coord_equal() +
#	coord_fixed(1.5) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
	  panel.background=element_blank())

#full world map with climate zones only colored for US
ras			<- as(raster2, "SpatRaster")
boundsUS		<- st_as_sf(us[, c(1:2)], coords = c("long", "lat"))
rasterUS		<- crop(ras, extent(boundsUS))
#rasterUS		<- mask(rasterUS, boundsUS)
rasterUSDF		<- as.data.frame(rasterUS, xy = TRUE)
colnames(rasterUSDF)[3]		<- "KoppenCliZs"

rasterUSDF_sf	<- st_as_sf(rasterUSDF, coords = c("x", "y"), crs = "EPSG:4326")
rasterUSDF_sf	<- st_transform(x = rasterUSDF_sf, crs = "EPSG:3857", desired_accuracy = 1, allow_ballpark = FALSE)

bndryUS		<- sf::st_as_sf(maps::map("usa", plot = FALSE, fill = FALSE))
bndryUS		<- st_transform(x = bndryUS, crs = "EPSG:3857", desired_accuracy = 1, allow_ballpark = FALSE)
bndryUSc		<- bndryUS[1,]
bndryUS		<- st_transform(x = bndryUS, crs = "EPSG:4326", desired_accuracy = 1, allow_ballpark = FALSE)

rasterUSDF_sf	<- st_intersection(st_make_valid(rasterUSDF_sf), st_make_valid(bndryUSc), sparse = FALSE)
rasterUSDF_sf	<- st_transform(rasterUSDF_sf, crs = "EPSG:4326", desired_accuracy = 1)
rasterUSDF		<- cbind(as.data.frame(rasterUSDF_sf, xy = TRUE),
				x = st_coordinates(rasterUSDF_sf)[,1], y = st_coordinates(rasterUSDF_sf)[,2])

statesUS		<- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))


#worldmap where only US is colored
#ggplot() + 
#	mapWorld +
#	geom_raster(data = rasterUSDF, aes(x = x, y = y, fill = KoppenCliZs)) +
#	scale_fill_viridis_d(alpha = 0.5) +
##	coord_equal() +
##	coord_fixed(1.5) +
#	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
#        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
#	  panel.background=element_blank())


#map of Contiguous US with climate zones and monitoring stations
#mapUS		<- borders(database = "usa", colour = cols.tmp[5], fill = NA) # create a layer of borders
mapUSstates	<- borders(database = "state", colour = cols.tmp[5], fill = NA) # create a layer of borders

m.p	<- ggplot() + 
	geom_raster(data = rasterUSDF, aes(x = x, y = y, fill = KoppenCliZs)) +
	scale_fill_viridis_d(name = "Climate Zone",
			limits = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk",
				"Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc",
				"Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
				"EF", "ET"), alpha = 0.5) +
	mapUSstates +
	geom_point(data = stations, aes(y = lat, x = lon),					# ts used in analysis
		 color = cols.tmp[2], cex = 0.8, pch = 20) +
	coord_equal() +
#	coord_fixed(1.5) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
	  panel.background=element_blank())
#pdf(file = "img/koppenClimZsUSstates.pdf", width=12, height=8)
m.p
#dev.off()

































































