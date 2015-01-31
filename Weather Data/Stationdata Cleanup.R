# Weather station data cleaning for Common Garden project
# Dec 2014 flynn@fas.harvard.edu

# Goal: make a compiled megafile with hourly temp and light data from each site.
# Make a summary table which has monthly summary data: 
# temperature: max, min, daytime mean, nighttime mean.
# light: sunrise, sunset, daylength means by month. If no station data, use
# http://www.esrl.noaa.gov/gmd/grad/solcalc/

setwd("~/Dropbox/Work/Harvard/Budburst Experiment/Weather Data/Raw Data Files")
setwd("~/Dropbox/Budburst Experiment/Weather Data/Raw Data Files")



# Start with HF

hf <- read.csv("Harvard Forest (Fisher) Raw Weather Data.csv")
# deal with bad 1st line
hf <- hf[-1,]
head(hf) # now make everything numeric, and skip weird columns of "F M"
todelete <- grep("^F", names(hf)) # Find columns which have a name of F (use ^ to make sure this is the first character in the column name).
hf <- hf[,-todelete]

for(i in 2:ncol(hf)){ 
	hf[,i] <- as.numeric(as.character(hf[,i])) 
	}

summary(hf)

# try to format the dates as necessary -- only year and month, R doesn't like it as a date
#hf[,1] <- format(
#	strptime(paste(as.character(hf[,1]), "-01", sep=""), "%Y-%m-%d") 
#	, "%Y-%m")
	# have to manually add -01 to date to get it to be recognized as a valid date
hf[,1] <- as.character(hf[,1])

# add in monthly data from previous weather station. Here we have daily data.. let's get Fisher daily.

hf2 <- read.csv("Harvard Forest (Shaler) Raw Weather Data.csv")
hf2 <- hf2[-1,]
head(hf2)
todelete <- grep("^F", names(hf2))
hf2 <- hf2[,-todelete]

for(i in 2:ncol(hf2)){ hf2[,i] <- as.numeric(as.character(hf2[,i])) }
hf2[,1] <- as.character(hf2[,1])
summary(hf2)


# Merging the two. 2 years of overlap... 
names(hf2)[3:4] <- c("AirTMax", "AirTMin")

hfx <- merge(hf, hf2, all = T)

# Monthly. Extract month from Date column

hfx$month <- substr(hfx[,1], 6, 7) # take 6th and 7th values of the Date column.

hf.summary <- as.data.frame(t(rbind(
tapply(hfx$AirTMax, hfx$month, mean, na.rm=T),
tapply(hfx$AirTMin, hfx$month, mean, na.rm=T),
tapply(hfx$ParT, hfx$month, mean, na.rm=T) # total PAR?
)))

colnames(hf.summary) <- c("tmax","tmin","part")

#
