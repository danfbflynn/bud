# Chamber logs vs hobos, Dec 2014 test by Tim
# R script by Dan to demo some data analysis and also to explore discrepancies between the logs


################# Prep

# Load packages. If you don't have these, type install.packages("x", dependancies = T) and follow the instrucitons to install on your machine.

library(gdata) # for read.xls function 

# Set working directory. On a mac, you can drag a file from the Finder window into an R script, and it will copy the path for you. This is for setup.

setwd("~/Dropbox/Work/harvard/Budburst Experiment")

larry.c <- read.xls("Chambers vs. Hobos 8Dec-14.xls", sheet = 1)
nick.c  <- read.xls("Chambers vs. Hobos 8Dec-14.xls", sheet = 2)
larry.h <- read.xls("Chambers vs. Hobos 8Dec-14.xls", sheet = 3)
nick.h <- read.xls("Chambers vs. Hobos 8Dec-14.xls", sheet = 4)

# or working from already-cleaned data (see below)

load("ChamberHobo.RData")

################# Cleaning up data

# deal with time and date columns appropriately

head(larry.c)
head(larry.h)

# First column of chamber data is date
larry.c[,1] # this is being read as a factor, with a 'level' for each unique value

# Convert it to date

larry.c[,1] <- as.Date(larry.c[,1], "%d-%b-%y") # see ?as.Date for help file

# But third column is what we really want, which has both date and time. Making a new column called 'time'. 

larry.c$time <- strptime(larry.c[,3], "%m/%d/%y %H:%M")
# repeat for nick

nick.c$time <- strptime(nick.c[,3], "%m/%d/%y %H:%M")

# fix hobo data

larry.h$time <- strptime(larry.h[,1], "%m-%d-%y %H:%M")
nick.h$time <- strptime(nick.h[,1], "%m-%d-%y %H:%M")

# cleaning up light data
larry.h$lux <- as.numeric(sub(",", "", as.character(larry.h[,"Intensity..Lux"])))
nick.h$lux <- as.numeric(sub(",", "", as.character(nick.h[,"Intensity..Lux"])))

# and last, changing names for convenient typing
names(larry.c) <- names(nick.c) <- c("Date", "Time", "Adj.Time", "temp", "temp.set", "humid", "humid.set", "co2", "co2.set", "light", "light.set1","light.set2", "time")

names(larry.h)[1:3] <- c("Time", "temp", "light")
names(nick.h)[1:4] <- c("Time", "Adj.Time.", "temp", "light")

############### Saving cleaned data

# Since the xls files take a relatively long time to load, I would then save the R versions of these cleaned sheets for future use as below:

save(file = "ChamberHobo.RData", list = c("larry.c","nick.c","larry.h","nick.h"))

# This can then be in the future read in directly as

load("ChamberHobo.RData")

################# Initial look

# pdf("ChamberHobo.pdf", width = 11, height = 8.5) # un-comment to create pdf

cols = c("darkred","darkblue")


par(mfrow = c(2,1)) # for 2 rows, 1 column of multi-panel plot

plot(larry.c$time, larry.c$temp, 
	type = "l", 
	col = cols[1],
	lwd = 1.5,
	main = "Larry",
	xlab = "Time",
	ylab = "Temp °C",
	ylim = c(17, 33)
	)

lines(larry.h$time, larry.h$temp,
	col = cols[2],
	lwd = 1.5
	)

legend("bottomleft", 
	cex = 0.5,
	col = cols, 
	lwd = 1.5, 
	legend = c("Chamber", "Hobo"))

# nick

plot(nick.c$time, nick.c$temp, 
	type = "l", 
	col = cols[1],
	lwd = 1.5,
	main = "Nick",
	xlab = "Time",
	ylab = "Temp °C",
	ylim = c(17, 33)
	)

lines(nick.h$time, nick.h$temp,
	col = cols[2],
	lwd = 1.5
	)

######## light. Convert PPF to lux using conversion factor 54... http://www.apogeeinstruments.com/conversion-ppf-to-lux/. Need to check units

plot(larry.c$time, larry.c$light, 
	type = "l", 
	col = cols[1],
	lwd = 1.5,
	main = "Larry",
	xlab = "Time",
	ylab = "Light (µmol/m2/sec)",
	ylim = c(0, 800)
	)

lines(larry.h$time, larry.h$lux/54,
	col = cols[2],
	lwd = 1.5
	)

legend("bottomleft", 
	cex = 0.5,
	col = cols, 
	lwd = 1.5, 
	legend = c("Chamber", "Hobo"))

# nick
plot(nick.c$time, nick.c$light, 
	type = "l", 
	col = cols[1],
	lwd = 1.5,
	main = "Nick",
	xlab = "Time",
	ylab = "Light (µmol/m2/sec)",
	ylim = c(0, 800)
	)

lines(nick.h$time, nick.h$lux/54,
	col = cols[2],
	lwd = 1.5
	)

	
# dev.off();system("open ChamberHobo.pdf") # un-comment if saving as pdf

### plotting correlations

# To do: match time points by rounding to nearest 15 min increment, for both chamber and hobo...

# hobo is 2 or 7 min off the chamber data. Could try to susbset the hobo data to only the points which are within a given interval of chamber data. Simpler would be to just subtract the difference form the hobo to force it to be equal
(larrydiff <- diff(c(larry.c$time[1], larry.h$time[1])))
(nickdiff <- diff(c(nick.c$time[1], nick.h$time[1])))

larry.h$time2 <- larry.h$time - larrydiff
nick.h$time2 <- nick.h$time - nickdiff

# use match, converting to character (can't use match() with time data. For some reason... chamber starts recording 2 min later on 2014-12-03 18:07:00 instead of 18:05:00..

nick.h2 <- nick.h[as.character(nick.h$time2) %in% as.character(nick.c$time),]
nick.h2 <- nick.h[as.character(nick.c$time) %in% as.character(nick.h2$time2),]