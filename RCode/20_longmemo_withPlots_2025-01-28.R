#############################################################################
### R-Basics and data
#############################################################################


###
###	Basics
###

###	Load libraries

#	install.packages("devtools")
	library(devtools)
#	install.packages("dplyr")
	library(dplyr)
#	install.packages("snowfall")
	library(snowfall)
	install_github("markusfritsch/prcpFC")
	library(prcpFC)



###	Clear Workspace

rm(list = ls())




###
###	Load rainfall data and compile anomaly data
###

###	Compile anomaly data based on imputed time series

data(raindata6019)

#monthly_demean <- function(x){
#  refmeans <- rowMeans(matrix(x[1:360], nrow=12)) # 1960-01 to 1989-12
#  anomalies <- x-rep(refmeans,times=length(x)/12)
#  return(anomalies)
#}
#results_anomalies <- sapply(raindata6019[,-1], function(x) monthly_demean(unname(unlist(x))))
#raindata6019_demeaned <- data.frame(raindata6019[,1], data.frame(results_anomalies))
#colnames(raindata6019_demeaned)[1]	<- "date"


monthly_demed <- function(x){
  refmeds <- apply(matrix(x[1:360], nrow=12), FUN = median, MARGIN = 1) # 1960-01 to 1989-12
  anomalies <- x-rep(refmeds,times=length(x)/12)
  return(anomalies)
}
results_anomalies <- sapply(raindata6019[,-1], function(x) monthly_demed(unname(unlist(x))))
raindata6019_demeded <- data.frame(raindata6019[,1], data.frame(results_anomalies))
colnames(raindata6019_demeded)[1]	<- "date"





###
###	Prewhitening
###


raintib <- filter(raindata6019_demeded, as.numeric(substr(date, 1, 4))>=1990)


#################################

sfInit(parallel=TRUE, cpus=15)
sfExport("prcpFC::my_arfimafit")
sfExport("raintib")
results_prewhitening <- sfLapply(raintib[,-1], prcpFC::my_prewhitening)
sfStop()

raintib_prewhitened <- tibble(date=raintib$date,
                              as_tibble(lapply(results_prewhitening, function(i) i$prewhitened)))
prewhitening_phis <- lapply(results_prewhitening, function(i) i$arfimafit$fit$modes[[1]]$phi)
prewhitening_phis <- sapply(prewhitening_phis, function(i) ifelse(length(i)==0,0,i))
prewhitening_ds <- sapply(results_prewhitening, function(i) i$arfimafit$fit$modes[[1]]$dfrac)
prewhitening_thetas <- lapply(results_prewhitening, function(i) i$arfimafit$fit$modes[[1]]$theta)
prewhitening_thetas <- sapply(prewhitening_thetas, function(i) ifelse(length(i)==0,0,i))
results_prewhitening_coefs <- cbind(prewhitening_phis,prewhitening_ds,prewhitening_thetas)





###
###	Long Memory Estimation and Testing
###


raintib <- filter(raindata6019_demeded, as.numeric(substr(date, 1, 4))>=1990)


#################################

n <- nrow(raintib)    # keep epsilon=0.05 for both sample sizes
m0.7 <- floor(n^0.7)
m0.6 <- floor(n^0.6)  # only for short-run dynamics

LManalysis <- function(x){
  Qu <- prcpFC::my_Qu(x, m=m0.7, epsilon=0.05)$W.stat
  LW0.6 <- prcpFC::my_LW(data=x,m=m0.6)
  LW <- prcpFC::my_LW(data=x,m=m0.7)
  houLW0.6 <- prcpFC::my_houLW(data=x,m=m0.6)
  houLW <- prcpFC::my_houLW(data=x,m=m0.7)
  results <- c("Qu"=Qu,"LW0.6"=LW0.6,"LW"=LW,"houLW0.6"=houLW0.6,"houLW"=houLW)
  return(results)
}

sfInit(parallel=TRUE, cpus=15)
sfExportAll()
LMresults <- cbind("Qu_pre"=sfSapply(raintib_prewhitened[,-1], function(x) my_Qu(x, m=m0.7, epsilon=0.05)$W.stat),
                   t(sfApply(raintib[,-1], 2, function(x) LManalysis(x=x))))
sfStop()


# prepare best LW and houLW according to short-run dynamics
armanoise <- ifelse(results_prewhitening_coefs[,1]!=0 | results_prewhitening_coefs[,3]!=0,TRUE,FALSE)
LWmixed <- ifelse(armanoise, LMresults[,"LW0.6"], LMresults[,"LW"])
houLWmixed <- ifelse(armanoise, LMresults[,"houLW0.6"], LMresults[,"houLW"])
LMresults <- cbind(LMresults, LWmixed, houLWmixed)





###
###	Testing for Mean Breaks
###


raintib	<- as_tibble(raindata6019_demeded)
raintib <- filter(raindata6019_demeded, as.numeric(substr(date, 1, 4))>=1990)


#################################

SWtests <- function(x,d){
  SNtest <- memochange::snsupwald(x,d,tau=0.15)
  FBtest <- memochange::fixbsupw(x,d,bandw=0.1,tau=0.15)
  SNdecision <- ifelse(SNtest[4]>SNtest[3],"break1",
                       ifelse(SNtest[4]>SNtest[2],"break5",
                              ifelse(SNtest[4]>SNtest[1],"break10","constant")))
  FBdecision <- ifelse(FBtest[4]>FBtest[3],"break1",
                       ifelse(FBtest[4]>FBtest[2],"break5",
                              ifelse(FBtest[4]>FBtest[1],"break10","constant")))
  return(c(SNSW=unname(SNdecision),FBSW=unname(FBdecision)))
}

sfInit(parallel=TRUE, cpus=15)
sfExportAll()
SWresults <- t(sfSapply(1:ncol(raintib[,-1]),
                        function(i) c(SWtests(x=raintib[[i+1]], d=LMresults[i,"LW"]),
                                      SWtests(x=raintib[[i+1]], d=LMresults[i,"LWmixed"]))))
sfStop()
colnames(SWresults) <- c("SNSW","FBSW","SNSWmixed","FBSWmixed")





###
###	Building Binary Variables and dstar
###

# Qu critical values 10%=1.022, 5%=1.155, 1%=1.426


allresults <- as_tibble(data.frame(LMresults, SWresults, results_prewhitening_coefs))

allresults <- allresults %>%
  mutate(armanoise = prewhitening_phis!=0 | prewhitening_thetas!=0) %>%
  mutate(meanbreak =
           (SNSWmixed=="break1" | FBSWmixed=="break1") |
           (SNSWmixed=="break5" | FBSWmixed=="break5") |
           (SNSWmixed=="break10" & FBSWmixed=="break10")) %>%
  mutate(quspurious5 = Qu_pre>=1.155) %>%  # 5% rejection
  mutate(quspurious10 = Qu_pre>=1.022) %>%  # 10% rejection
  mutate(potentially_contaminated = quspurious10 | SNSWmixed!="constant" | FBSWmixed!="constant") %>%
  mutate(bestd = ifelse(potentially_contaminated, houLWmixed, LWmixed)) %>%
  mutate(ddiff = LW-bestd)


rainresults <- tibble(xvars,
                  select(allresults,  # zvars and destims
                         quspurious5, quspurious10, meanbreak, armanoise, potentially_contaminated,
                         LW, LWmixed, houLW, houLWmixed, bestd, ddiff))

#save(rainresults, file = "rainresults.RData"))











#######################################################################
###	Plot results
#######################################################################


###
###	First Stage Results - Plot Preparation
###


#	install.packages("ggplot2")
	library(ggplot2)
#	install.packages("ggpubr")
	library(ggpubr)
#	install.packages("RColorBrewer")
	library(RColorBrewer)


###	Read in station meta data with koppen climate zone classification

data("ghcndStations")
stat2.2 		<- cbind(ghcndStations, ghcndStations$statID %in% colnames(raindata6019_demeded))
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


rainresults_UScont <- rainresults[rainresults$statID %in% statID.UScont, ]




rainresults_UScont	<- rainresults[rainresults$statID %in% statID.UScontWOna, ]
rainresults_UScont	<- rainresults_UScont[!is.na(rainresults_UScont$zone_t), ]

#exclude monitoring stations with NAs in climate zone classification
#results_UScont	<- subset(x = results_UScont, subset = results_UScont$statID %in% statID.UScontWOna)


#############################################################################
### Categorization of spurious LM
# quspurious5 and meanbreak might overlap
# potentially_contaminated might have extra series (rejection of qu or meanbreak test at 10 percent)
# armanoise and potentially_contaminated are independent
spuriouslm <- rep("No indications", nrow(rainresults_UScont))
spuriouslm[rainresults_UScont$potentially_contaminated] <- "Contaminated"
spuriouslm[rainresults_UScont$meanbreak] <- "Mean break"
spuriouslm[rainresults_UScont$quspurious5] <- "Qu spurious"
spuriouslm[rainresults_UScont$meanbreak & rainresults_UScont$quspurious5] <- "Qu spurious and Mean break"
results_UScont <- mutate(rainresults_UScont, spuriouslm=factor(spuriouslm,levels=rev(c("Qu spurious and Mean break", "Qu spurious","Mean break","Contaminated","No indications"))))


#############################################################################
### Passau colors
cols.tmp		<- c(
  #				"#f58080"	# lightred
  "#efa540"	# orange
  ,"#2266ee"	# blue
  ,"#aabb55"	# lightgreen
  ,"#c4bdb7"	# lightgrey
  ,"#827f7b"	# grey
  ,"#6b625d"	# darkgrey
  ,"#830433"	# darkred
  ,"#c10f0f"	# red
  ,"#6978c6" 	# blue velvet
)
# continuous: scale_fill_viridis_d()


#############################################################################
### Category colors
# spuriouslm ("No indications","Contaminated","Mean break","Qu spurious and Mean break","Qu spurious")
#spuriouslm_colors <- c("azure","azure3","darkblue","darkcyan","forestgreen")  # Hannover
#spuriouslm_colors <- c("#827f7b","#6978c6","#2266ee","darkcyan","#aabb55")  # Passau
spuriouslm_colors <- c("#aabb55","#6978c6","#2266ee","darkcyan","#6b625d")  # Passau
#spuriouslm_colors <- c("azure","azure3","#2266ee","cyan3","forestgreen")  # Mix
#spuriouslm_colors <- c("rosybrown3","plum3","#2266ee","cyan3","forestgreen")  # Mix2

# armanoise (FALSE, TRUE)
#arma_colors <- c("burlywood1","firebrick")  # Hannover
#arma_colors <- c("#6978c6","#efa540")  # Passau
arma_colors <- c("#827f7b","#efa540")  # Passau"#827f7b"
#arma_colors <- c("burlywood1","#c10f0f")  # Mix

# nothing
nocol <- "gray"
nocol <- "#c4bdb7"



#############################################################################
# First Stage Results - Bar Plots
#############################################################################

# remove zones with too few stations
summary(as.factor(results_UScont$zone_c))
which_zones_c <- c("B","C","D")
summary(as.factor(results_UScont$zone_p))
which_zones_p <- c("f","s","S","W")
summary(as.factor(results_UScont$zone_t))
which_zones_t <- c("a","b","c","h","k")


#############################################################################
### Bar plots for spurious LM according to different zones

barplot_base_lm <- list(
  geom_bar(stat="count", position="fill"),
  scale_fill_manual(values=spuriouslm_colors, name = "Potential contamination"),
  ylab(""),
  theme(panel.background=element_blank())
)

gg_bar_lm_climate <- ggplot(filter(results_UScont, zone_c %in% which_zones_c), aes(x=zone_c, fill=spuriouslm)) +
  barplot_base_lm +
  xlab("Climate zone")
gg_bar_lm_precip <- ggplot(filter(results_UScont, zone_p %in% which_zones_p), aes(x=zone_p, fill=spuriouslm)) +
  barplot_base_lm +
  xlab("Precipitation subgroup")
gg_bar_lm_temp <- ggplot(filter(results_UScont, zone_t %in% which_zones_t), aes(x=zone_t, fill=spuriouslm)) +
  barplot_base_lm +
  xlab("Temperature subgroup")

ggarrange(gg_bar_lm_climate, gg_bar_lm_precip, gg_bar_lm_temp, nrow=1, ncol=3, common.legend=TRUE, legend="bottom")
#ggsave("../img/plotBinVarsKoppen.pdf", width=4*3, height=4*1)


#############################################################################
### Bar plots for ARMA noise according to different zones

barplot_base_arma <- list(
  geom_bar(stat="count", position="fill"),
  scale_fill_manual(values=arma_colors, name = "Short-run dynamics", labels=c("None","ARMA noise")),
  ylab(""),
  theme(panel.background=element_blank())
)

gg_bar_arma_climate <- ggplot(filter(results_UScont, zone_c %in% which_zones_c), aes(x=zone_c, fill=armanoise)) +
  barplot_base_arma +
  xlab("Climate zone")
gg_bar_arma_precip <- ggplot(filter(results_UScont, zone_p %in% which_zones_p), aes(x=zone_p, fill=armanoise)) +
  barplot_base_arma +
  xlab("Precipitation subgroup")
gg_bar_arma_temp <- ggplot(filter(results_UScont, zone_t %in% which_zones_t), aes(x=zone_t, fill=armanoise)) +
  barplot_base_arma +
  xlab("Temperature subgroup")

ggarrange(gg_bar_arma_climate, gg_bar_arma_precip, gg_bar_arma_temp, nrow=1, ncol=3, common.legend=TRUE, legend="bottom")
#ggsave("../img/plotBinVarsArma.pdf", width=4*3, height=4*0.7)




#############################################################################
# First Stage Results - Scatter Plots
#############################################################################

scatterplot_base <- ggplot(results_UScont, aes(x=LW, y=bestd)) +
  geom_abline(intercept=0, slope=1, col=nocol, lwd=1) +
#  geom_point(col=nocol) +
  xlim(-0.15,0.25) + ylim(-0.5,0.5) + 
  #xlim(-0.15,0.25) + ylim(-0.25,0.35) + # removes one data point!
  xlab("") + ylab("") +
#  xlab(quote(italic(hat(d)[LW]))) + ylab(expression(italic(hat(d))~"*")) +
  theme(legend.position="bottom", panel.background=element_blank(), legend.key = element_rect(colour = "transparent", fill = "white")) #strip.background=element_blank()


#############################################################################
### Scatter plots for d estimated by LW and HPmixed according to spurious LM with colors

gg_scatter_notcont <- ggplot(data=subset(results_UScont, !potentially_contaminated), aes(x=LW, y=bestd)) +
  geom_abline(intercept=0, slope=1, col=nocol, lwd=1) +
  geom_point(col = spuriouslm_colors[1]) +
  xlim(-0.15,0.25) + ylim(-0.5,0.5) + 
  xlab("") + ylab("") +
  scale_color_manual(name="Potential contaminations",
                     limits=levels(results_UScont$spuriouslm),
                     values=spuriouslm_colors,
                     na.value=nocol) +
  theme(legend.position="bottom", panel.background=element_blank(), legend.key = element_rect(colour = "transparent", fill = "white")) #strip.background=element_blank()

gg_scatter_potential <- ggplot(data=subset(x = results_UScont, subset = potentially_contaminated), aes(x=LW, y=bestd)) +
  geom_abline(intercept=0, slope=1, col=nocol, lwd=1) +
  geom_point(aes(color=spuriouslm)) +
  xlim(-0.15,0.25) + ylim(-0.5,0.5) + 
  xlab("") + ylab("") +
  scale_color_manual(name="Potential contaminations",
                     limits=levels(results_UScont$spuriouslm),
                     values=spuriouslm_colors,
                     na.value=nocol) +
  theme(legend.position="bottom", panel.background=element_blank(), legend.key = element_rect(colour = "transparent", fill = "white")) #strip.background=element_blank()
ggarrange(gg_scatter_notcont, gg_scatter_potential, nrow=1, ncol=2, common.legend=TRUE, legend="bottom")
#ggsave("../img/scatterCtmd.pdf", width=3*3, height=4*1)


#############################################################################
### ALTERNATIVE Scatter plots for d estimated by LW and HPmixed according to spurious LM with grid

gg_scatter_contamination <- scatterplot_base +
#  geom_point(data=select(results_UScont,-spuriouslm), color=nocol) +
#  geom_point() +
  geom_point(aes(color=spuriouslm)) +
  scale_color_manual(name="Potential contaminations", values=spuriouslm_colors, na.value=nocol) +
  #theme(legend.position = "none") +
  theme(strip.text.x = element_blank()) +
  facet_wrap(vars(spuriouslm), ncol=5)
#ggsave("../img/scatterCtmType.pdf", width=3*5, height=4*1)



#############################################################################
### Scatter plots for d estimated by LW and HPmixed according to different zones

gg_scatter_climate <- scatterplot_base %+% subset(results_UScont,zone_c %in% which_zones_c) +
#  geom_point(data=select(results_UScont,-zone_c), color=nocol) +
  geom_point(col = "#830433") +
  theme(strip.text.x = element_blank()) +
  facet_wrap(vars(zone_c), ncol=3)
#ggsave("../img/scatterClimate.pdf", width=3*3, height=4*1)

gg_scatter_precip <- scatterplot_base %+% subset(results_UScont,zone_p %in% which_zones_p) +
#  geom_point(data=select(results_UScont,-zone_p), color=nocol) +
  geom_point(col = "#830433") +
  theme(strip.text.x = element_blank()) +
  facet_wrap(vars(zone_p), ncol=4)
#ggsave("../img/scatterPrecip.pdf", width=3*4, height=4*1)


#############################################################################
### Scatter plots for d estimated by LW and HPmixed according to ARMA noise
# variable "potentially_contaminated" contains all contamination categories of "spuriouslm"


gg_scatter_notcontA <- ggplot(data=subset(results_UScont, !potentially_contaminated), aes(x=LW, y=bestd)) +
  geom_abline(intercept=0, slope=1, col=nocol, lwd=1) +
  geom_point(aes(color=armanoise)) +
  xlim(-0.15,0.25) + ylim(-0.5,0.5) + 
  xlab("") + ylab("") +
  scale_color_manual(values=arma_colors, name = "Short-run dynamics", labels=c("None","ARMA noise"), na.value=nocol) +
  theme(legend.position="bottom", panel.background=element_blank(), legend.key = element_rect(colour = "transparent", fill = "white")) #strip.background=element_blank()

gg_scatter_potentialA <- ggplot(data=subset(x = results_UScont, subset = potentially_contaminated), aes(x=LW, y=bestd)) +
  geom_abline(intercept=0, slope=1, col=nocol, lwd=1) +
  geom_point(aes(color=armanoise)) +
  xlim(-0.15,0.25) + ylim(-0.5,0.5) + 
  xlab("") + ylab("") +
  scale_color_manual(values=arma_colors, name = "Short-run dynamics", labels=c("None","ARMA noise"), na.value=nocol) +
  theme(legend.position="bottom", panel.background=element_blank(), legend.key = element_rect(colour = "transparent", fill = "white")) #strip.background=element_blank()
ggarrange(gg_scatter_notcontA, gg_scatter_potentialA, nrow=1, ncol=2, common.legend=TRUE, legend="bottom")
#ggsave("../img/scatterArma.pdf", width=3*3, height=4*1)



