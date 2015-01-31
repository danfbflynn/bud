# Creating maps and assigning treatments for Budburst experiment 2015

# Goal: test control of daylength, temperature, and chilling on timing of budburst for woody species of NE North America

# This script will assign treatments and chamber locations for each cutting.

# Latest: 2015-01-29


setwd("~/Dropbox/Work/Harvard/Budburst Experiment/")
library(gdata) # for read.xls

d <- read.xls("../Common Garden_SHARE/Winter 2015 Individuals.xlsx")
spoverview <- read.xls("../Common Garden_SHARE/Species Counts 2015-01-30.xlsx", sheet = 6)

d <- d[,c(1:4,14:21)]

names(d) <- c("Individual","Site","Lat","Long","collected","cuttings","height","dbh1","dbh2","dbh3","dbh4","notes")

# Find out if we took cuttings of these replacment Aromel and Lyolig; first three definitely did not do. For now eliminate all of these
#  data.frame(d[is.na(d$cuttings),"Individual"])
# 1                        SPIALB04_SH
# 2                        LONCAN02_SH
# 3                        SPIALB02_SH
# 4                        AROMEL02_HF
# 5                        AROMEL03_HF
# 6                        LYOLIG04_HF
# 7                        AROMEL05_HF

d <- d[!is.na(d$cuttings),] # 275 individuals left

# Which species which have chilling. Of 28 species, 15 will have chilling treatments. 
d$sp <- substr(d$Individual, 1, 6)
# chillsp <- unique(d[as.numeric(d$cuttings) > 12,"sp"])  # can't base chill treatment species off of actual clippings taken, since sometimes took extra.

chillsp <- as.character(spoverview[spoverview$Total.cuttings.per.individual > 8 & !is.na(spoverview$Total.cuttings.per.individual), "Code"])

allsp <- sort(unique(d[,"sp"]))
nonchillsp <- allsp[!allsp %in% chillsp]

# Chilling treatment combinations
chill <- gl(3, 4, labels = c("chill0", "chill1","chill2"))
warm <- gl(2, 1, 12, labels = c("warm", "cool"))
photo <- rep(sort(rep(gl(2, 1, labels = c("long", "short")), 2)), 3)

treatcode <- apply(toupper(cbind(substr(warm, 1, 1), substr(photo, 1, 1), substr(chill, 6, 6))), 1, function(x) paste(x, collapse=""))

chilltreat <- data.frame(treatcode, warm, photo, chill)

# Nonchilling treatment combinations

warm <- gl(2, 1, 4, labels = c("warm", "cool"))
photo <- gl(2, 2, labels = c("long", "short"))
chill <- "chill0"

treatcode <- apply(toupper(cbind(substr(warm, 1, 1), substr(photo, 1, 1), substr(chill, 6, 6))), 1, function(x) paste(x, collapse=""))

nonchilltreat <- data.frame(treatcode, warm, photo, chill)

# Make twig-wise data frame. Assign treatments by individual, using chilltreat and nonchilltreat dataframes.

dx <- vector()

for(i in 1:nrow(d) ) { # i = 1
	# For nonchill species, want four cuttings per individual. Ignore the actual number of cuttings, since it includes tissues samples for some species
	# Chill species, 12 cuttings: 2 temp x 2 photo x 3 chill
#		 xx <- paste(d[i,"Individual"], formatC(1:(d[i,"cuttings"]-4), width = 2, flag = "0"), sep = "_")

	 if(d[i,"sp"] %in% chillsp){
		xx <- paste(d[i,"Individual"], formatC(1:12, width = 2, flag = "0"), sep = "_")
		}
	else { xx <- paste(d[i,"Individual"], formatC(1:4, width = 2, flag = "0"), sep = "_") }

	xx <- data.frame(xx)
	
	xx$sp <- substr(xx[,1], 1, 6)
	xx$rep <- substr(xx[,1], 7, 8)
	xx$site <- substr(xx[,1], 10, 11)
	xx$ind <- paste(substr(xx[,1], 1, 8), xx$site, sep = "_")
	xx$twig <- substr(xx[,1], 13, 14)

	names(xx)[1] = "id"

	# Assign treatments. Randomize rows of treatment dataframes and apply to this individual
	if(d[i,"sp"] %in% chillsp){	
		xx <- data.frame(xx, chilltreat[sample(1:nrow(chilltreat)),])
		}	
	if(!d[i,"sp"] %in% chillsp){	
		xx <- data.frame(xx, nonchilltreat[sample(1:nrow(nonchilltreat)),])
		}

	dx <- rbind(dx, xx)
		
	}

write.csv(dx, paste("Budburst Twig Datasheet w Treatments ", Sys.Date(), ".csv", sep = ""), row.names=F)

# <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>>
# make maps. By chamber, starting now with just chill0 treatments
# <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>>

# Jan 2015 maps for chill0 twigs

j <- dx[dx$chill == "chill0",] # 1100 individuals, 275 per chamber

# Divide by chamber/treatment.

jz <- vector()

for(i in unique(j$treatcode)){ # i = "CS0" 
	jx <- j[j$treatcode == i,]
	# Randomize and make into pairs; make sure to not have congeners in pairs.
	# Positions: 1:137, with 138th being one twig only.
	jx$position <- sample(c(1:137, 1:137, 138))
	
	jx <- jx[order(jx$position),]

	# Check to see if a pair has a congener. If so, repeat shuffling until no pairs
	jx$gen <- substr(jx$sp, 1, 3)

	while(any(unlist(tapply(jx$gen, jx$position, duplicated)))){
		jx$position <- sample(c(1:137, 1:137, 138))
	
		jx <- jx[order(jx$position),]
				
		}
	
	jz <- rbind(jz, jx)
	}


# Write it out. Important: if the script is re-run, will get different assignments of treatments! So only do this once.
# Here using Sys.Date to prevent accidental overwriting of original csv.

# Adding in row and column values for each twig
jz$row = rep(c(rep(sort(rep(1:8, 2)), 17), 1, 1, 2), 4)
jz$col = rep(c(gl(17, 16), 18, 18, 18), 4)

# jz[1:100, c(1, 11:14)]

write.csv(jz, paste("Budburst Twig Datasheet chill0 ", Sys.Date(), ".csv", sep = ""), row.names=F)

# Make maps.

maplist <- list()

# 138 positions. 8 rows, 18 columns

counter = 1
for(i in unique(jz$treatcode)){ # i = "CS0" 
	jx <- jz[jz$treatcode == i,]
	
	mat <- matrix(nrow = 8, ncol = 18, byrow = T)
	
	for(k in unique(jx$position)){ # k = 1
		mat[k] = paste(jx[jx$position == k,"id"], collapse = "\n")
		
		}
	
	maplist[[counter]] = mat
	
	counter = counter + 1
	}

write.csv(maplist, "Mapchill0.csv")



# <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>>
# Maps Chill1
# <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>>

# Jan 2015 maps for chill1 twigs

j <- dx[dx$chill == "chill1",] # 520, extra 130 twigs per chamber in 65 flasks

# Divide by chamber/treatment.

jz <- vector()

for(i in unique(j$treatcode)){ # i = "CL1" 
	jx <- j[j$treatcode == i,]
	# Randomize and make into pairs; make sure to not have congeners in pairs.
	# Positions: 1:65
	jx$position <- sample(c(1:65, 1:65))
	
	jx <- jx[order(jx$position),]

	# Check to see if a pair has a congener. If so, repeat shuffling until no pairs
	jx$gen <- substr(jx$sp, 1, 3)

	while(any(unlist(tapply(jx$gen, jx$position, duplicated)))){
		jx$position <- sample(c(1:65, 1:65))
	
		jx <- jx[order(jx$position),]
				
		}
	
	jz <- rbind(jz, jx)
	}


# Write it out. Important: if the script is re-run, will get different assignments of treatments! So only do this once.
# Here using Sys.Date to prevent accidental overwriting of original csv.

# Adding in row and column values for each twig. 8 per column, 8 columns plus one extra
jz$row = rep(c(rep(sort(rep(1:8, 2)), 8), 1, 1), 4)
jz$col = rep(c(gl(8, 16), 9, 9), 4)

# jz[1:100, c(1, 11:14)]

write.csv(jz, paste("Budburst Twig Datasheet chill1 ", Sys.Date(), ".csv", sep = ""), row.names=F)

# Make maps.

maplist <- list()

# 65 positions. 8 rows, 8 full columns, one extra

counter = 1
for(i in unique(jz$treatcode)){ # i = "CS1" 
	jx <- jz[jz$treatcode == i,]
	
	mat <- matrix(nrow = 8, ncol = 9, byrow = T)
	
	for(k in unique(jx$position)){ # k = 1
		mat[k] = paste(jx[jx$position == k,"id"], collapse = "\n")
		
		}
	
	maplist[[counter]] = mat
	
	counter = counter + 1
	}

write.csv(maplist, "Mapchill1.csv")


# <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>>
# Maps Chill2
# <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>> <<>>

# Jan 2015 maps for chill1 twigs

j <- dx[dx$chill == "chill2",] # 520, extra 130 twigs per chamber in 65 flasks

# Divide by chamber/treatment.

jz <- vector()

for(i in unique(j$treatcode)){ # i = "CL2" 
	jx <- j[j$treatcode == i,]
	# Randomize and make into pairs; make sure to not have congeners in pairs.
	# Positions: 1:65
	jx$position <- sample(c(1:65, 1:65))
	
	jx <- jx[order(jx$position),]

	# Check to see if a pair has a congener. If so, repeat shuffling until no pairs
	jx$gen <- substr(jx$sp, 1, 3)

	while(any(unlist(tapply(jx$gen, jx$position, duplicated)))){
		jx$position <- sample(c(1:65, 1:65))
	
		jx <- jx[order(jx$position),]
				
		}
	
	jz <- rbind(jz, jx)
	}


# Write it out. Important: if the script is re-run, will get different assignments of treatments! So only do this once.
# Here using Sys.Date to prevent accidental overwriting of original csv.

# Adding in row and column values for each twig. 8 per column, 8 columns plus one extra
jz$row = rep(c(rep(sort(rep(1:8, 2)), 8), 1, 1), 4)
jz$col = rep(c(gl(8, 16), 9, 9), 4)

# jz[1:100, c(1, 11:14)]

write.csv(jz, paste("Budburst Twig Datasheet chill2 ", Sys.Date(), ".csv", sep = ""), row.names=F)

# Make maps.

maplist <- list()

# 65 positions. 8 rows, 8 full columns, one extra

counter = 1
for(i in unique(jz$treatcode)){ # i = "CS2" 
	jx <- jz[jz$treatcode == i,]
	
	mat <- matrix(nrow = 8, ncol = 9, byrow = T)
	
	for(k in unique(jx$position)){ # k = 1
		mat[k] = paste(jx[jx$position == k,"id"], collapse = "\n")
		
		}
	
	maplist[[counter]] = mat
	
	counter = counter + 1
	}

write.csv(maplist, "Mapchill2.csv")

