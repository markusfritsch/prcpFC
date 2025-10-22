
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
#	install.packages("rworldmap")
	library(rworldmap)
#	install.packages("usmap")
	library(usmap)
#	install.packages("ggplot2")
	library(ggplot2)
#	install.packages("viridis")
	library(viridis)
#	install.packages("gridExtra")
	library(gridExtra)
#	install.packages("ggmap")
	library(ggmap)
#	install.package("dplyr")
	library(dplyr)
#	install.packages("forecast")
  	library(forecast)
#	install.packages("arfima")
  	library(arfima)
#	install.packages("devtools")
	library(devtools)
#	install.packages("xtable")
	library(xtable)
	install_github("markusfritsch/prcpFC")
	library(prcpFC)






###	Clear Workspace

rm(list=ls())




###	Set working directory

  setwd("D:/Work/20_Projekte/280_Rainfall/submission/IJoF/revision/img")





###
###	Data preprocessing function to create all required objects
###


dataPrepr.fct	<- function(
				x = data(raindata6019),
				y = data(ghcndStations)
				){


###	Clear workspace

  rm(list=ls())




###	Specify colors

  cols.tmp		<<- c(
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


  stat2.2 			<<- cbind(ghcndStations, ghcndStations$statID %in% colnames(raindata6019))
  names(stat2.2)[12]	<<- "is.in.data"


###	Compile anomaly data based on imputed time series

  dat.tmp		<<- raindata6019[,-1]
  mean(colMeans(dat.tmp))
  median(colMeans(dat.tmp))

  monthly_refMeans	<<- function(x){
    refMeans	 <- rowMeans(matrix(x[1:360], nrow = 12))							# 1960-01 to 1989-12
    return(refMeans)
  }

  monthly_refMedians	<<- function(x){
    mat.tmp		<- matrix
    refMedians	<- apply(X = matrix(x[1:360], nrow=12), FUN = median, MARGIN = 1)		# 1960-01 to 1989-12
    return(refMedians)
  }

  refMeans		<<- sapply(dat.tmp, function(x) monthly_refMeans(unname(unlist(x))))
  refMeds		<<- sapply(dat.tmp, function(x) monthly_refMedians(unname(unlist(x))))

  dat_demeaned	<<- dat.tmp - apply(refMeans, FUN = rep, MARGIN = 2, times = 60)
  dat_demeded	<<- dat.tmp - apply(refMeds, FUN = rep, MARGIN = 2, times = 60)



###
###	Create objects with station IDs according to region
###


  statAll		<<- stat2.2$statID[stat2.2$is.in.data]
  statAll.id	<<- substr(stat2.2$statID[stat2.2$is.in.data], start = 1, stop = 2)
#all stations that contain no more missing vales after imputation

#Australia
  statAus		<<- "AS"
  statAus.id	<<- grep(pattern = statAus, x = statAll.id)
#  length(statAus.id)
  statID.Aus	<<- stat2.2$statID[stat2.2$statID %in% statAll[statAus.id]]

#Europe
  statEU	<<- c("AL", "BO", "BE", "BK", "BU", "DA", "GM", "EN",
		"FI", "FR", "GG", "GR", "HR", "EI", "IC", "IT", "LG", "LH", "LU",
		"MT", "MD", "MJ", "NL", "MK", "NO", "AU", "PL", "PO", "RO", "SW",
		"SZ", "RI", "SI", "LO", "SP", "EZ", "UP", "HU", "UK")
  statEU.id	<<- grep(pattern = paste(statEU, collapse = "|"), x = statAll.id)
#  length(statEU.id)
  statID.EU	<<- stat2.2$statID[stat2.2$statID %in% statAll[statEU.id]]

#United States
  usStatesCont	<<- c(
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

  usStatesAll	<<- c(
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

  statID.usCont	<<- stat2.2$statID[(stat2.2$stateUS %in% usStatesCont) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
  statID.usContN	<<- stat2.2$statID[!(stat2.2$stateUS %in% usStatesCont) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
  statID.usAll	<<- stat2.2$statID[(stat2.2$stateUS %in% usStatesAll) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]
  statID.usAllN	<<- stat2.2$statID[!(stat2.2$stateUS %in% usStatesAll) & !(is.na(stat2.2$stateUS)) & stat2.2$is.in.data]

  statID.USall	<<- statID.usAll[statID.usAll %in% statAll]
  statID.UScont	<<- statID.usCont[statID.usCont %in% statAll]
#  length(statID.USall)
#  length(statID.UScont)


  stat_Aus		<<- stat2.2[stat2.2$statID %in% statID.Aus, ]
#  nrow(stat_Aus)
#  sum(is.na(stat_Aus$koppen2))
  statID.AusWOna	<<- stat_Aus[!is.na(stat_Aus$koppen2), "statID"]
#  length(statID.AusWOna)
#  length(statID.Aus)

  stat_EU		<<- stat2.2[stat2.2$statID %in% statID.EU, ]
#  nrow(stat_EU)
#  sum(is.na(stat_EU$koppen2))
  statID.EUWOna	<<- stat_EU[!is.na(stat_EU$koppen2), "statID"]
#  length(statID.EUWOna)
#  length(statID.EU)

  stat_usAll		<<- stat2.2[stat2.2$statID %in% statID.USall, ]
#  nrow(stat_usAll)
#  sum(is.na(stat_usAll$koppen2))
  statID.USallWOna	<<- stat_usAll[!is.na(stat_usAll$koppen2), "statID"]
#  length(statID.USallWOna)
#  length(statID.USall)

  stat_usCont		<- stat2.2[stat2.2$statID %in% statID.UScont, ]
#  nrow(stat_usCont)
#  sum(is.na(stat_usCont$koppen2))
  statID.UScontWOna	<<- stat_usCont[!is.na(stat_usCont$koppen2), "statID"]
#  length(statID.UScontWOna)
#  length(statID.UScont)

#save("statID.Aus", "statID.EU", "statID.USall", "statID.UScont",
#	file = "D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate/statIDregions.RData")
#save("statID.AusWOna", "statID.EUWOna", "statID.USallWOna", "statID.UScontWOna",
#	file = "D:/Work/20_Projekte/280_Rainfall/R/10_data/2025-01-16_dataUpdate/statIDregionsWOnas.RData")

  stat_usAll	<<- stat_usAll[!is.na(stat_usAll$koppen2), ]
  stat_usCont	<<- stat_usCont[!is.na(stat_usCont$koppen2), ]

  stat_usContmT	<<- usmap::usmap_transform(stat_usCont)
  stat_usAllmT	<<- usmap::usmap_transform(stat_usAll)

  stat_usContmT	<<- stat_usContmT[!is.na(stat_usContmT$koppen2.cz) &
					(stat_usContmT$koppen2.cz == "B" |
					stat_usContmT$koppen2.cz == "C" |
					stat_usContmT$koppen2.cz == "D") ,]

  stat_Aus		<<- stat_Aus[!is.na(stat_Aus$koppen2.cz) &
					(stat_Aus$koppen2.cz == "B" |
					stat_Aus$koppen2.cz == "C" |
					stat_Aus$koppen2.cz == "D") ,]

  stat_EU		<<- stat_EU[!is.na(stat_EU$koppen2.cz) &
					(stat_EU$koppen2.cz == "B" |
					stat_EU$koppen2.cz == "C" |
					stat_EU$koppen2.cz == "D") ,]


}






















###
###	Fig.1: Map of Contiguous US with points colored according to precipitation
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()

#load data
data(rainresults)

#use only data in main climate zones B, C, and D
rainresults <- rainresults[!is.na(rainresults$zone_c) &
					(rainresults$zone_c == "B" |
					rainresults$zone_c == "C" |
					rainresults$zone_c == "D") ,]

stations		<- sf::st_as_sf(stat_usContmT, coords = c("lon", "lat"), remove = FALSE, crs = "EPSG:4267")
#stations$group	<- 1

us 		<- ggplot2::map_data("usa")
usStates	<- ggplot2::map_data("state")

data(koppen)
koppen[, 3]			<- factor(koppen[, 3])
koppen			<- data.frame(Lon = koppen[,2], Lat = koppen[,1], Cls = koppen[,3])
names(koppen)
coordinates(koppen)	<- ~ Lon + Lat
proj4string(koppen)	<-  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
gridded(koppen)		<- TRUE
raster2			<- raster::raster(koppen)


ras			<- as(raster2, "SpatRaster")
boundsUS		<- sf::st_as_sf(us[, c(1:2)], coords = c("long", "lat"))
rasterUS		<- raster::crop(ras, extent(boundsUS))
#rasterUS		<- raster::mask(rasterUS, boundsUS)
rasterUSDF		<- as.data.frame(rasterUS, xy = TRUE)
colnames(rasterUSDF)[3]		<- "KoppenCliZs"

rasterUSDF_sf	<- sf::st_as_sf(rasterUSDF, coords = c("x", "y"), crs = "EPSG:4326")
rasterUSDF_sf	<- sf::st_transform(x = rasterUSDF_sf, crs = "EPSG:3857", desired_accuracy = 1, allow_ballpark = FALSE)

bndryUS		<- sf::st_as_sf(maps::map("usa", plot = FALSE, fill = FALSE))
bndryUS		<- st_set_crs(x = bndryUS, value = "EPSG:4326")
bndryUS		<- sf::st_transform(x = bndryUS, crs = "EPSG:3857", desired_accuracy = 1, allow_ballpark = FALSE)
bndryUSc		<- bndryUS[1,]
bndryUS		<- sf::st_transform(x = bndryUS, crs = "EPSG:4326", desired_accuracy = 1, allow_ballpark = FALSE)

rasterUSDF_sf	<- sf::st_intersection(st_make_valid(rasterUSDF_sf), st_make_valid(bndryUSc), sparse = FALSE)
rasterUSDF_sf	<- sf::st_intersection(rasterUSDF_sf, bndryUSc, sparse = FALSE)
rasterUSDF_sf	<- sf::st_transform(rasterUSDF_sf, crs = "EPSG:4326", desired_accuracy = 1)
rasterUSDF		<- cbind(as.data.frame(rasterUSDF_sf, xy = TRUE),
				x = sf::st_coordinates(rasterUSDF_sf)[,1], y = sf::st_coordinates(rasterUSDF_sf)[,2])

statesUS		<- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

mapUSstates	<- ggplot2::borders(database = "state", colour = cols.tmp[5], fill = NA) # create a layer of borders

m.p	<- ggplot() + 
	geom_raster(data = rasterUSDF, aes(x = x, y = y, fill = KoppenCliZs), show.legend = TRUE) +
	scale_fill_viridis_d(name = "Climate Zone",
			limits = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk",
				"Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc",
				"Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
				"EF", "ET"), alpha = 0.5, drop = FALSE) +
	mapUSstates +
	geom_point(data = rainresults[rainresults$statID %in% stat_usContmT$statID, ], aes(y = lat, x = lon),					# ts used in analysis
		 color = cols.tmp[2], cex = 0.8, pch = 20) +
	coord_equal() +
#	coord_fixed(1.5) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
		axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
		legend.title = element_text(size = 20), legend.text = element_text(size = 20),
		panel.background=element_blank())
m.p


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
colnames(stations1)[11]	<- "medianPrcpAno"
stations1			<- st_transform(stations1, crs = "EPSG:4326")
stations1$lon		<- st_coordinates(stations1)[ , 1]
stations1$lat		<- st_coordinates(stations1)[ , 2]
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
#	stat_sf_coordinates(data = stations1, aes(y = lat, x = lon, color = medianPrcpAno),		# ts used in analysis
#		 cex = 4.0, pch = 20)
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
m.p1



#Fig.1
#pdf(file = paste(getwd(), "/meanPrcpAnoKoppenUSstates.pdf", sep = ""), width=16, height=10)
gridExtra::grid.arrange(m.p, m.p1, 
	nrow = 2)
#dev.off()










###
###	Fig.2: Violin plot and cumulative empirical distribution function
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()

dat	<- dat_demeded

dat	<- dat[, colnames(dat) %in% stat_usContmT$statID]



left.dens	<- density(unlist(dat[1:360, ]), n = 10000)
rght.dens	<- density(unlist(dat[361:720, ]), n = 10000)


col.set	<- c("#6b625d", "#2266ee", "#c4bdb7")


ord.range	<- c(-1, 1)*1200



#Fig.2
#	pdf(file = paste(getwd(), "/Density_CDens.pdf", sep = ""), height = 2, width = 8)

par(mfrow = c(1, 2), mgp = c(2, 1, 0), mai = c(0.4, 0.4, 0.1, 0.2))


plot(
  x		= c(0, 1),
  y		= c(0, 1),
  xlim	= ord.range,
  ylim	= c(-0.5, 0.5),
  type	= "n",
  xlab	= "",		# Month",
  ylab	= "",		# Precipitation anomaly",
  xaxs	= "i",
  yaxs	= "i",
  yaxt	= "n",
# xaxt	= "n", 
  frame.plot = FALSE
)

axis(side = 1, at = c(-1000, -500, 0, 500, 1000), tick = FALSE)
abline(v = 0, col = col.set[3])

polygon(
  x	= rght.dens$x,
  y	= rght.dens$y/max(rght.dens$y)/2.05,
  border = col.set[2], lwd = 1, lty = 1
)
polygon(
  x	= left.dens$x,
  y	= -1*left.dens$y/max(left.dens$y)/2.05,
  border = col.set[1], lwd = 1, lty = 1
)
abline(h = 0, col = col.set[3])

    lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.5), times = 2), y = c(-0.5, 0), col = col.set[1], lwd = 1)
    lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.5), times = 2), y = c(0.5, 0), col = col.set[2], lwd = 1)

    lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.25), times = 2), y = c(-0.5, 0), col = col.set[1], lwd = 1)
    lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.25), times = 2), y = c(0.5, 0), col = col.set[2], lwd = 1)
    lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.75), times = 2), y = c(-0.5, 0), col = col.set[1], lwd = 1)
    lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.75), times = 2), y = c(0.5, 0), col = col.set[2], lwd = 1)

    lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.1), times = 2), y = c(-0.5, 0), col = col.set[1], lwd = 1)
    lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.1), times = 2), y = c(0.5, 0), col = col.set[2], lwd = 1)
    lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.9), times = 2), y = c(-0.5, 0), col = col.set[1], lwd = 1)
    lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.9), times = 2), y = c(0.5, 0), col = col.set[2], lwd = 1)


text(x = -900, y = -0.25, labels = "1960-1989", col = col.set[1])
text(x = -900, y = 0.25, labels = "1990-2019", col = col.set[2])




plot(
  x		= c(0, 1),
  y		= c(0, 1),
  xlim	= ord.range,
  ylim	= c(0, 1),
  type	= "n",
  xlab	= "",		# Month",
  ylab	= "",		# Precipitation anomaly",
  xaxs	= "i",
  yaxs	= "i",
  yaxt	= "n",
# xaxt	= "n", 
  frame.plot = FALSE
)

#	axis(side = 1, at = c(-1000, -500, 0, 500, 1000), tick = FALSE)
axis(side = 2, at = 0:10/4, las = 1)

#	abline(v = 0, col = col.set[3])

lines(
  x	= rght.dens$x,
  y	= cumsum(rght.dens$y)/sum(rght.dens$y),
  col = col.set[2], lwd = 1, lty = 1
)
lines(
  x	= left.dens$x,
  y	= cumsum(left.dens$y)/sum(left.dens$y),
  col = col.set[1], lwd = 1, lty = 1
)

lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.5), times = 2), y = c(0, 0.5), col = col.set[1], lwd = 1)
lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.5), times = 2), y = c(0, 0.5), col = col.set[2], lwd = 1)

lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.9), times = 2), y = c(0, 0.9), col = col.set[1], lwd = 1)
lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.9), times = 2), y = c(0, 0.9), col = col.set[2], lwd = 1)

lines(x = rep(quantile(unlist(dat[1:360, ]), probs = 0.1), times = 2), y = c(0, 0.1), col = col.set[1], lwd = 1)
lines(x = rep(quantile(unlist(dat[361:720, ]), probs = 0.1), times = 2), y = c(0, 0.1), col = col.set[2], lwd = 1)


#dev.off()





















###
###	Fig.3: Plots for three main climate zones in Contiguous US from 1960 to 2019 (time series plots)
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()


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
			axis.ticks.x=element_blank(), axis.title.y = element_text(angle = 0))
  } else{
    l.p	<- ggplot() +
		geom_hline(yintercept = 0, lty = 1, col = cols.tmp[6]) +
		geom_ribbon(data = dat.tmp3, aes(x = year, ymax = quantUpAno, ymin = quantLoAno), fill = cols.tmp[4], alpha = .5) +
		geom_ribbon(data = dat.tmp4, aes(x = year, ymax = quantUpAno, ymin = quantLoAno), fill = cols.tmp[5], alpha = .5) +
		geom_line(data = dat.tmp2, aes(x = year, y = medAno), col = cols.tmp[8], lwd = 0.3) +
		xlab("") + ylab(cz.tmp) +
#		xlab("") + ylab(paste(m.tmp)) + ggtitle("Main climate zone B") +
		ylim(ymin,ymax) +
		theme(panel.background=element_blank(), axis.title.y = element_text(angle = 0))
  }

  assign(paste("l.p", cz.tmp, sep = ""), value = l.p)

}


#Fig.3
#pdf(file = paste(getwd(), "/stationsPrcpBCD.pdf", sep = ""), width = 10, height = 10)
gridExtra::grid.arrange(l.pB, l.pC, l.pD,
	nrow = 3)
#dev.off()





if(FALSE){
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
}







###
###	Fig.4: ACF and PACF plots for simulated ARFIMA(0,d,0) data
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()


#simulate ARFIMA(0,d,0) data
seed		<- 42
n		<- 1000
alph		<- 0.05
d_lm		<- 0.49

d_ap		<- -0.49 

d_sm		<- 0


set.seed(seed)

sim_lm	<- arfima::arfima.sim(n = n, model = list(dfrac = d_lm))
sim_sm	<- arfima::arfima.sim(n = n, model = list(dfrac = d_sm))
sim_ap	<- arfima::arfima.sim(n = n, model = list(dfrac = d_ap))

#plots of raw time series
#par(mfrow = c(3,1))
#plot(sim_lm, type = "l", ylim = c(-5,5))
#plot(sim_sm, type = "l", ylim = c(-5,5))
#plot(sim_ap, type = "l", ylim = c(-5,5))


#acf- and pacf-plots for anti-persistent-, short memory-, and long memory behavior
exAcf_df <- data.frame(lag = 1:30, ap = forecast::Acf(sim_ap)$acf[-1],
                        sm = forecast::Acf(sim_sm)$acf[-1], lm = forecast::Acf(sim_lm)$acf[-1])
exPacf_df <- data.frame(lag = 1:30, ap = forecast::Pacf(sim_ap)$acf,
                        sm = forecast::Pacf(sim_sm)$acf, lm = forecast::Pacf(sim_lm)$acf)

ci_upper	<- qnorm(p = (1-alph/2))/sqrt(n)
ci_lower	<- -qnorm(p = (1-alph/2))/sqrt(n)

lp1	<- ggplot(data = exAcf_df, mapping = aes(x = lag, y = lm)) +
  geom_hline(aes(yintercept = 0), col = cols.tmp[6]) +
  geom_hline(aes(yintercept = ci_upper), col = cols.tmp[6], linetype = "dashed") +
  geom_hline(aes(yintercept = ci_lower), col = cols.tmp[6], linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), col = cols.tmp[2]) +
  ylab("ACF") + xlab("Lag") + ylim(c(-0.4,0.75)) + xlim(0,30) +
  theme(panel.background = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,1,1,1.9), "cm"))

lp2	<- ggplot(data = exAcf_df, mapping = aes(x = lag, y = sm)) +
  geom_hline(aes(yintercept = 0), col = cols.tmp[6]) +
  geom_hline(aes(yintercept = ci_upper), col = cols.tmp[6], linetype = "dashed") +
  geom_hline(aes(yintercept = ci_lower), col = cols.tmp[6], linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), col = cols.tmp[2]) +
  ylab("ACF") + xlab("") + ylim(c(-0.4,0.75)) + xlim(0,30) +
  theme(panel.background = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,1,1,1.9), "cm"))

lp3	<- ggplot(data = exAcf_df, mapping = aes(x = lag, y = ap)) +
  geom_hline(aes(yintercept = 0), col = cols.tmp[6]) +
  geom_hline(aes(yintercept = ci_upper), col = cols.tmp[6], linetype = "dashed") +
  geom_hline(aes(yintercept = ci_lower), col = cols.tmp[6], linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), col = cols.tmp[2]) +
  ylab("ACF") + xlab("") + ylim(c(-0.4,0.75)) + xlim(0,30) +
  theme(panel.background = element_blank(), axis.title.y = element_text(angle = 0),
        plot.margin = unit(c(1,1,1,1), "cm"))

lp4	<- ggplot(data = exPacf_df, mapping = aes(x = lag, y = lm)) +
  geom_hline(aes(yintercept = 0), col = cols.tmp[6]) +
  geom_hline(aes(yintercept = ci_upper), col = cols.tmp[6], linetype = "dashed") +
  geom_hline(aes(yintercept = ci_lower), col = cols.tmp[6], linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), col = cols.tmp[2]) +
  ylab("PACF") + xlab("Lag") + ylim(c(-0.4,0.75)) + xlim(0,30) +
  theme(panel.background = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,1,1,2.2), "cm"))

lp5	<- ggplot(data = exPacf_df, mapping = aes(x = lag, y = sm)) +
  geom_hline(aes(yintercept = 0), col = cols.tmp[6]) +
  geom_hline(aes(yintercept = ci_upper), col = cols.tmp[6], linetype = "dashed") +
  geom_hline(aes(yintercept = ci_lower), col = cols.tmp[6], linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), col = cols.tmp[2]) +
  ylab("PACF") + xlab("") + ylim(c(-0.4,0.75)) + xlim(0,30) +
  theme(panel.background = element_blank(), axis.title.y = element_blank(),
        plot.margin = unit(c(1,1,1,2.2), "cm"))

lp6	<- ggplot(data = exPacf_df, mapping = aes(x = lag, y = ap)) +
  geom_hline(aes(yintercept = 0), col = cols.tmp[6]) +
  geom_hline(aes(yintercept = ci_upper), col = cols.tmp[6], linetype = "dashed") +
  geom_hline(aes(yintercept = ci_lower), col = cols.tmp[6], linetype = "dashed") +
  geom_segment(mapping = aes(xend = lag, yend = 0), col = cols.tmp[2]) +
  ylab("PACF") + xlab("") + ylim(c(-0.4,0.75)) + xlim(0,30) +
  theme(panel.background = element_blank(), axis.title.y = element_text(angle = 0),
        plot.margin = unit(c(1,1,1,1), "cm"))


#Fig.4
#pdf(file = paste(getwd(), "/acfPacf.pdf", sep = ""), width=9, height=10)
gridExtra::grid.arrange(lp3, lp6, lp2, lp5, lp1, lp4,
                        nrow = 3)
#dev.off()







###
###	Fig.5: log periodogram vs. log frequency for simulated ARFIMA(0,d,0) data
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()


#simulate ARFIMA(0,d,0) data
seed		<- 42
n		<- 1000
alph		<- 0.05
d_lm		<- 0.49

d_ap		<- -0.49 

d_sm		<- 0


set.seed(seed)

sim_lm	<- arfima::arfima.sim(n = n, model = list(dfrac = d_lm))
sim_sm	<- arfima::arfima.sim(n = n, model = list(dfrac = d_sm))
sim_ap	<- arfima::arfima.sim(n = n, model = list(dfrac = d_ap))

#plots of raw time series
#par(mfrow = c(3,1))
#plot(sim_lm, type = "l", ylim = c(-5,5))
#plot(sim_sm, type = "l", ylim = c(-5,5))
#plot(sim_ap, type = "l", ylim = c(-5,5))


#Figure according to Beran et al. (2012), Chapter 1.2 (f): Log(periodogram) vs. log(lambda)
T	<- length(sim_ap)
nj	<- floor(T/2)
ir	<- complex(real = 0, imaginary = 1)
freqs	<- 2*pi*(1:nj)/T
peri_df	<- data.frame(freq_disc = log(1:nj), freq_cont = log(freqs), per_ap = log(as.vector(1/(2*pi*T)*abs(t(sim_ap)%*%exp(-ir*(1:T)%*%t(freqs)))^2)),
                      per_sm = log(as.vector(1/(2*pi*T)*abs(t(sim_sm)%*%exp(-ir*(1:T)%*%t(freqs)))^2)),
                      per_lm = log(as.vector(1/(2*pi*T)*abs(t(sim_lm)%*%exp(-ir*(1:T)%*%t(freqs)))^2)))

#for adding coefficient estimates
lm4		<- lm(per_ap ~ freq_disc, data = peri_df)
#summary(lm4)
lm5		<- lm(per_sm ~ freq_disc, data = peri_df)
#summary(lm5)
lm6		<- lm(per_lm ~ freq_disc, data = peri_df)
#summary(lm6)

fp4	<- ggplot(data = peri_df, mapping = aes(x = freq_disc, y = per_ap)) +
  geom_point(col = cols.tmp[2]) +
  geom_smooth(method = "lm", col = cols.tmp[5]) +
  ylab(expression(paste("log(I(", lambda[j], "))", sep = ""))) + xlab("") + ylim(-11,6) +
  annotate("text", label = paste("slope: ", round(as.numeric(coef(lm4)[2]), digits = 4), sep = ""), x = 0, hjust="left", y = -8.4, col = cols.tmp[5]) +
  theme(panel.background = element_blank(), axis.title.y = element_text(angle = 0),
        plot.margin = unit(c(1,1,1,1), "cm"))

fp5	<- ggplot(data = peri_df, mapping = aes(x = freq_disc, y = per_sm)) +
  geom_point(col = cols.tmp[2]) +
  geom_smooth(method = "lm", col = cols.tmp[5]) +
  ylab("") + xlab("") + ylim(-11,6) +
  annotate("text", label = paste("slope: ", round(as.numeric(coef(lm5)[2]), digits = 4), sep = ""), x = 0, hjust="left", y = -4.6, col = cols.tmp[5]) +
  theme(panel.background = element_blank(), axis.title.y = element_text(angle = 0),
        plot.margin = unit(c(1,1,1,2.4), "cm"))

fp6	<- ggplot(data = peri_df, mapping = aes(x = freq_disc, y = per_lm)) +
  geom_point(col = cols.tmp[2]) +
  geom_smooth(method = "lm", col = cols.tmp[5]) +
  ylab("") + xlab(expression(lambda[j])) + ylim(-11,6) +
  annotate("text", label = paste("slope: ", round(as.numeric(coef(lm6)[2]), digits = 4), sep = ""), x = 0, hjust="left", y = 0.2, col = cols.tmp[5]) +
  theme(panel.background = element_blank(), axis.title.y = element_text(angle = 0),
        plot.margin = unit(c(1,1,1,2.4), "cm"))

#Fig.5
#pdf(file = paste(getwd(), "/logPeri.pdf", sep = ""), width=9, height=10)
gridExtra::grid.arrange(fp4, fp5, fp6,
                        nrow = 3)
#dev.off()








################################################################################
###	Sec. 4.1 and B.1: Analysis of long range dependence parameter estimates
################################################################################


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()


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





###
###	Numbers in text at beginning of Sec. 4.1
###


table(stat_contUS$quspurious5)/nrow(stat_contUS)
table(stat_contUS$meanbreak)/nrow(stat_contUS)
table(stat_contUS$potentially_contaminated)/nrow(stat_contUS)
table(stat_contUS$armanoise)/nrow(stat_contUS)






###
###	Tab.2 & Tab.B.9 Diagnosed sources of spurious long memory, mean breaks, and short-run dynamics
###


#Tab.2: Contiguous US 
mat_contUS	<- rbind(c(sum(stat_contUS$quspurious5), sum(stat_contUS$meanbreak), sum(stat_contUS$potentially_contaminated),
			sum(stat_contUS$armanoise)),
		c(sum(!stat_contUS$quspurious5), sum(!stat_contUS$meanbreak), sum(!stat_contUS$potentially_contaminated),
			sum(!stat_contUS$armanoise)))
mat_contUS	<- rbind(mat_contUS, round(mat_contUS[1,]/(mat_contUS[1,] + mat_contUS[2,]), digits = 3)*100 )
colnames(mat_contUS)	<- c("quSprs", "meanBrk", "potCtmd", "armaNoise")
mat_contUS

#numbers in text on Tab.2 (contiguous US)
nrow(stat_contUS)		#number of stations in contiguous US
50/1077			#share of indications for spurious lm
126/1077			#share of indications for mean break
253/1077			#share of indications for pot. contaminations
387/1077			#share of indications for ARMA noise


#Tab.B.9: World
colnames(dat)[18]	<- "dstar"
stat_world	<- dat
mat_world	<- rbind(c(sum(stat_world$quspurious5), sum(stat_world$meanbreak), sum(stat_world$potentially_contaminated),
			sum(stat_world$armanoise)),
		c(sum(!stat_world$quspurious5), sum(!stat_world$meanbreak), sum(!stat_world$potentially_contaminated),
			sum(!stat_world$armanoise)))
mat_world	<- rbind(mat_world, round(mat_world[1,]/(mat_world[1,] + mat_world[2,]), digits = 3)*100 )
colnames(mat_world)	<- c("quSprs", "meanBrk", "potCtmd", "armaNoise")
mat_world

#numbers in text on B.9 (World)
nrow(stat_world)		#number of worldwide stations
149/2118			#share of indications for spurious lm
223/2118			#share of indications for mean break
536/2118			#share of indications for pot. contaminations
777/2118			#share of indications for ARMA noise



###	Variation in memory parameter estimates

#Contiguous US: Numbers in text in Sec. 4.1
#quantile(stat_contUS$LW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(stat_contUS$LW)
#quantile(stat_contUS$houLW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
#quantile(stat_contUS$dstar, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(stat_contUS$dstar)
#increase in standard deviation of LRD parameter estimate
(sd(stat_contUS$dstar) - sd(stat_contUS$LW))/sd(stat_contUS$LW)

#quantile(stat_contUS$ddiff, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
#plot(sort(stat_contUS$ddiff))
##substantial changes in LRD parameter estimates are the exception
#plot(density(stat_contUS$ddiff))
#mean(stat_contUS$ddiff)
#difference in memory parameter estimates is right-skewed, which means that
# the memory parameter tends to decrease when accounting for potential
# contaminations and short-range dependence (ddiff = LW-dstar)



#World: Numbers in text in Sec. B.1
#quantile(dat$LW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(dat$LW)
#quantile(dat$houLW, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
#quantile(dat$dstar, probs = c(0, 0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99, 1))
sd(dat$dstar)
#increase in standard deviation of LRD parameter estimate
(sd(dat$dstar) - sd(dat$LW))/sd(dat$LW)






###
###	Tab.3 & Tab.B.10: Table number of anomaly series falling into three regions
###


#Tab.3: Contiguous US

matLRD	<- cbind(c(
		sum(stat_contUS$LW <= -0.1),
		sum(stat_contUS$LW <= 0.1 & stat_contUS$LW >= -0.1),
		sum(stat_contUS$LW >=0.1)),
		c(sum(stat_contUS$dstar <= -0.1),
		sum(stat_contUS$dstar <= 0.1 & stat_contUS$dstar >= -0.1),
		sum(stat_contUS$dstar >=0.1))
		)
xtable(matLRD)



#Tab.B.10: World

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
###	Tab.4 & Tab.B.11: Five number summary and mean
###


#Tab.4: Contiguous US
mat2_contUS	<- rbind(dLW = summary(stat_contUS$LW), dstar = summary(stat_contUS$dstar))
xtable(mat2_contUS, digits = 2)


#Tab. B.11: World
mat2_world	<- rbind(dLW = summary(dat$LW), dstar = summary(dat$dstar))
xtable(mat2_world, digits = 2)











###
###	Fig.6 & Fig.B.11: histogram long range dependence parameter when accounting for potential contaminations
###


#Fig. 6: Contiguous US
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

#pdf(file = paste(getwd(), "/histsLRD.pdf", sep = ""), width=8, height=3)
gridExtra::grid.arrange(h.pLW, h.pDstar, 
	nrow = 1)
#dev.off()



#Fig. B.11: World
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

#pdf(file = paste(getwd(), "/histsLRDworld.pdf", sep = ""), width=8, height=3)
gridExtra::grid.arrange(h.pLW_world, h.pDstar_world, 
	nrow = 1)
#dev.off()














###
###	 Fig.7 & Fig.B.12: Scatterplots and tables of the two long range dependence parameter estimates (LW vs. dstar)
###


#Fig.7: Contiguous US
d.p	<- ggplot() +
	geom_abline(intercept = 0, slope = 1, col = cols.tmp[4], lwd = 1) +
	geom_point(data = stat_contUS, aes(x = LW, y = dstar), col = cols.tmp[8], cex = 1.2) +
#	geom_point(data = stat_contUS, aes(x = LW, y = dstar), col = cols.tmp[6], cex = 1.2) +
	xlab(expression(italic(hat(d)[LW]))) + ylab(expression(italic(hat(d))~"*")) +
	theme(panel.background=element_blank(), axis.title.y = element_text(angle = 0))
#pdf(file = paste(getwd(), "/scPlotDandDstar.pdf", sep = ""), width=8, height=4)
d.p
#dev.off()



#Fig B.12: World: scatterplot LW vs. dstar
d.p	<- ggplot() +
	geom_abline(intercept = 0, slope = 1, col = cols.tmp[4], lwd = 1) +
	geom_point(data = dat, aes(x = LW, y = dstar), col = cols.tmp[8], cex = 1.2) +
#	geom_point(data = dat, aes(x = LW, y = dstar), col = cols.tmp[6], cex = 1.2) +
	xlab(expression(italic(hat(d)[LW]))) + ylab(expression(italic(hat(d))~"*")) +
	theme(panel.background=element_blank(), axis.title.y = element_text(angle = 0))
#pdf(file = paste(getwd(), "/scPlotDandDstarWorld.pdf", sep = ""), width=8, height=4)
d.p
#dev.off()











###
###	Fig.8: Maps on long range dependence parameter estimates
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
m.pD


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


#Fig.8
#pdf(file = paste(getwd(), "/mapsDandDstar.pdf", sep = ""), width=8, height=6)
gridExtra::grid.arrange(m.pD, m.p2, nrow = 2)
#dev.off()














###
###	Appendix Fig A.9: Season plot of anomaly series of Contiguous US for months from 1960 until 2020 next to each other (seasonal plot)
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()


stat_usContmT	<- stat_usContmT[stat_usContmT$koppen2.cz == "B" |
					stat_usContmT$koppen2.cz == "C" |
					stat_usContmT$koppen2.cz == "D" ,]


statIDs		<- stat_usContmT$statID


data(raindata6019)
dat.tmp		<- raindata6019[ , -1]
dat.tmp		<- dat.tmp[ , colnames(dat.tmp) %in% stat_usContmT$statID]


monthly_refMedians	<- function(x){
  mat.tmp		<- matrix
  refMedians	<- apply(X = matrix(x[1:360], nrow=12), FUN = median, MARGIN = 1)		# 1960-01 to 1989-12
  return(refMedians)
}

refMediansUS			<- sapply(dat.tmp, function(x) monthly_refMedians(unname(unlist(x))))
monthRefMedians_allUSStat	<- apply(X = refMediansUS, FUN = median, MARGIN = 1)
monthMedians_allUSStat		<- apply(X = dat.tmp, FUN = median, MARGIN = 1)

#check:
#apply(matrix(as.data.frame(dat.tmp[,1])[1:360,], nrow = 12), FUN = median, MARGIN = 1)	# ok
#rowMeans(matrix(as.data.frame(dat.tmp[,1])[1:360,], nrow = 12))					#compare



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

#Fig.A.9
#pdf(file = paste(getwd(), "/seasonPlot.pdf", sep = ""), width=8, height=4)
seasPlot
#dev.off()











###
###	Descriptives for Appendix
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()


#Appendix A.1
nrow(stat_usContmT)	#number of monitoring stations in contiguous US

#Appendix A.2
nrow(rainresults)							#number of worldwide monitoring stations
nrow(stat_usContmT) + nrow(stat_Aus) + nrow(stat_EU)	#number of monitoing stations in contiguous US, Australia, and Europe

#Appendix Tab.A.8
table(rainresults$koppen)
table(rainresults[rainresults$statID %in% stat_usContmT$statID, "koppen"])



















###
###	Fig.A.10: Map of all stations with climate zone classification and table of climate zone classification for Continental US
###


#run 'dataPrepr.fct()' to clear workspace and create all required objects
dataPrepr.fct()


mp <- NULL
mapWorld <- ggplot2::borders("world", colour = cols.tmp[5], fill=NA) # create a layer of borders

data(koppen)
koppen[, 3]			<- factor(koppen[, 3])
koppen			<- data.frame(Lon = koppen[,2], Lat = koppen[,1], Cls = koppen[,3])
names(koppen)
coordinates(koppen)	<- ~ Lon + Lat
proj4string(koppen)	<-  CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
gridded(koppen)		<- TRUE
raster2			<- raster::raster(koppen)


#worldmap with koppen climate zones
raster2DF			<- as.data.frame(raster2, xy = TRUE)
names(raster2DF)[3]	<- "KoppenCliZs"

gmapK		<- ggplot() + 
	mapWorld +
	geom_raster(data = raster2DF, aes(x = x, y = y, fill = KoppenCliZs)) +
	scale_fill_viridis_d(name = "Climate Zone",
			limits = c("Af", "Am", "As", "Aw", "BSh", "BSk", "BWh", "BWk",
				"Cfa", "Cfb", "Cfc", "Csa", "Csb", "Csc", "Cwa", "Cwb", "Cwc",
				"Dfa", "Dfb", "Dfc", "Dfd", "Dsa", "Dsb", "Dsc", "Dsd", "Dwa", "Dwb", "Dwc", "Dwd",
				"EF", "ET"), alpha = 0.5) +
	geom_point(data = rainresults, aes(x = lon, y = lat), color = cols.tmp[2], cex = 0.4, pch = 20) +
	labs(y = "", x = "") +
#	scale_fill_viridis_d(alpha = 0.5) +
#	coord_equal() +
#	coord_fixed(1.5) +
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
	  panel.background=element_blank())
	theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
		axis.ticks.x = element_blank(), axis.ticks.y = element_blank(),
		panel.background = element_blank())
#pdf(file = paste(getwd(), "/koppenClimZs.pdf", sep = ""), width=12, height=8)
gmapK
#dev.off()

















