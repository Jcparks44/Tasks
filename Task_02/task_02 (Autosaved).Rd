setwd('~/Desktop/Evolution/Tasks/Task_02')
Data <- read.csv('http://jonsmitchell.com/data/beren.csv', stringsAsFactors=F)
write.csv(Data, 'rawdata.csv', quote=F)

#ALWAYS SAVE THINGS INTO OBJECTS - If you type in ‘Data’ and hit Enter, then you see all of the numbers but if you just type in read.csv(… without storing anything, it’ll show up but you can’t utilize it 

length(Data)
nrow(Data)

ncol(Data)

colnames(Data)
   
head(Data)

Data[1,]

Data[2,]
Data[1:3,]
 
Data[1:3, 4]
 
Data[1:5, 1:3]
  

#How do you find the date of the 257th observation?
Data[257, 1:3]


#Parsing an object means pulling out only the data we care about. For this, we use the which() fun which allows us to find which rows are of interest and then keep only these rows 

Feeds <- which(Data[,9] == 'bottle')
berenMilk <- Data[Feeds,]
head(berenMilk)


nrow(berenMilk)

berenMilk[1,]


#The 9th column is event, so we utilized ‘9’ as the data set we wanted, then subset ‘bottle’ as the event that we wanted. The code reads in each, Feeds is an assigned variable of which data we want, from column 9. This is set == to bottle, as we are looking for the bottle only. Row 145 method, using column names, sets the Data by column name, rather than number. Row 148, we use the dollar sign instead of the square brackets to clarify which data set we want. To prove it, I will repeat the process using each technique to set ‘Feeds’ as a variable of the event bottle, and then display the data. 

Feeds <- which(Data[,9] == 'bottle')
Feeds
  

Feeds <- which(Data[, 'event'] == 'bottle')
Feeds


Feeds <- which(Data$event == 'bottle')
Feeds
  

dayID <- apply(Data, 1, function(x) paste (x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
head(Data)

beren2 <- Data
beren2

#Reorganize the object so that it is sorted by date.  We do this by applying the order function to the age column. We then subset the object by that order. 

beren3 <- beren2[order(beren2$age) ,]
beren3
head(Data)

head(beren2)


#The headings on Data and beren2 are going to be the same.. because we just set beren2 as a variable of Data, essentially a copy. However beren3 will be different, as we used the order function to order beren2 columns by the date. Organizing by date brought the most important events to the top such as baby’s hospital measurements, like his mass, length, birthday, and head circumference.  

head(beren3)

write.csv(beren3, 'beren_new.csv', quote=F, row.names=FALSE)

# 	Question 1: They don't weigh the baby everyday, and he would gain weight regardless, as he grows. 
#	How much he drinks and how much he naps wouldn't have a relationship because he'll drink more and sleep less as he grows.
#	Redefine the Feeds Vector

Feeds <- which(beren3$event == 'bottle')
Feeds
avgMilk <- mean(beren3$value[Feeds])
avgMilk
#	The units are in ounces.
#	The value column contains numbers for any given measurement. We know these are 		average ounces though because we changed Feeds to wehere it only focused on bottle 	events so that mL of tylenol or degrees fahrenheit for a temperature were not 		included in the data. 
#	Feeds in square brackets means we're only finding the data from beren3 that 		corresponds to the bottle. It is important because we only want to know the 		numbers of milk consumption.
# The T apply function takes some data (here, value) and some tretament (here, age in days) and applies some other function (here, mean) to those data
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
length(avgFeed)
nrow(avgFeed)
head(avgFeed)
avgFeed[1]
avgFeed[1:4]
avgFeed[1:5, 1:3]
# This data has no columns

varFeed <-  tapply(beren3$value[Feeds], beren3$age[Feeds], var)
varFeed
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
totalFeed
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
numFeeds

# The cor () function tells us the correlation between two sets of numbers (here, age and amount consumed per day). ?cor gives different options for different types of correlations
cor(beren3$value[Feeds], beren3$age[Feeds])
# cor.test function works like cor(), but it conducts the appropriate test for the type of correlation you chose. 
cor.test(beren3$value[Feeds], beren3$age[Feeds])
# When you get a statistical output, save it as an object!
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
# t.test() is the T test, chi.test() is a Chi-squared test, wilcox.test() Wilcoxon's test, ks.test() is a Kolmogorov-Smirnov test
berenANOVA <- aov(beren3$value[Feeds] ~beren3$caregiver[Feeds])
berenANOVA

#Let's plot how much Beren eats (value) as a function of who his caregiver was for the feeding. The ~ means as a function of 
boxplot( beren3$value[Feeds] ~ beren3$caregiver[Feeds], xlab = 'who gave the bottle', ylab = 'amount of milk consumed (oz)')
# We can edit a plot precisely using the parameter function 
?par
# las is numeric in {0,1,2,3}; the style of axis labels, 0 means always parallel to the axis, 1 means always horizontal, 2 means always perpendicular, 3 means always vertical
# mar is a numerical vector of the form c(bottom, left, top, right) which gives the number of lines of margin to be specified on the four sides of the plot. The default is c(5, 4, 4, 2) + 0.1.
# mgp is the margin line for the axis title, labels, and line. mgp[1] would effect the titel while mgp[2:3] affect the axis. The default values are c(3, 1, 0).
# tck is the lengh of tick marks as a fraction of the heigh tof a line of text. tck = -0.01 is S' default.
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
# We then make a simple plot. 
plot(as.numeric(names(totalFeed)), totalFeed, type= 'b', pch= 16, xlab= 'age in days', ylab= 'ounces of milk') 
abline(h= mean(totalFeed), lty= 2, col= 'red')
# We are saving the graph we made using the following:
pdf('r02b-totalMilkByDay.pdf', height = 4, width = 4)
par(las=1, mar=c(5, 5, 1, 1), mgp=c(2, 0.5, 0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type= 'b', pch= 16, xlab= 'age in days', ylab= 'ounces of milk') 
abline(h= mean(totalFeed), lty= 2, col= 'red')
dev.off()
# The graph is hard to interpret because everything is so close together, and it really says nothing about how much his milk consumption has increased over time because it isn't in terms of rate.
pdf('r02b-cumulativeMilkByTime.pdf', height = 4, width = 6)
# summarize the day count
Days <- unique(beren3$age[Feeds])
ndays <- length(Days)

# create a set of custom colors
Alpha <- 0.75
divCols <- c(rgb(158/255,1/255,66/255, Alpha),rgb(213/255,62/255,79/255, Alpha),rgb(244/255,109/255,67/255, Alpha),rgb(253/255,174/255,97/255, Alpha),rgb(254/255,224/255,139/255, Alpha),rgb(255/255,255/255,191/255, Alpha),rgb(230/255,245/255,152/255, Alpha),rgb(171/255,221/255,164/255, Alpha),rgb(102/255,194/255,165/255, Alpha),rgb(50/255,136/255,189/255, Alpha),rgb(94/255,79/255,162/255, Alpha))
Pal <- colorRampPalette(divCols, interpolate = "spline", alpha = T)

# subset the custom colors
Cols <- Pal(ndays)
names(Cols) <- Days

# make a blank plot
plot(1, 1, xlim=c(7, 16), ylim=c(0, 20), type="n", xlab="time of day", ylab="total milk (oz)")

# add data to the plot
for (Day in Days)	{
	FeedsOnDay <- which(beren3$age[Feeds] == Day)
	Times <- beren3$start_hour[Feeds[FeedsOnDay]] + (beren3$start_minute[Feeds[FeedsOnDay]] / 60)
	Amounts <- cumsum(beren3$value[Feeds[FeedsOnDay]])
	points(Times, Amounts, col=Cols[as.character(Day)], type="b", pch=16)
}

# add a legend to the plot
dayVec <- c(min(Days), Days[floor(0.25*ndays)], median(Days), Days[ceiling(0.75*ndays)], max(Days))
legend("top", legend=dayVec, pch=21, pt.bg=Cols[as.character(dayVec)], horiz = T, bty = "n", pt.cex=2, title = "age in days")
dev.off()
unique(beren3$event)

#Extra Credit I
Naps <- which(Data$event == 'nap')
Naps
beren4 <- which(beren3$event == 'nap')
beren4
starttime <- apply(Data, 1, function(x) paste(x[5:6], collapse=':'))
startID <- sapply(starttime, as.POSIXct, format = '%H:%M', tz = 'America/New_York', units = 'min')
startID
endtime <- apply(Data, 1, function(x) paste(x[7:8], collapse = ':'))
endtime
endID <- sapply(endtime, as.POSIXct, format = '%H:%M', tz = 'America/New_York', units = 'min')
endID[11]
Data$Duration <- as.numeric(endID) - as.numeric(startID)
beren5 <- Data
beren6 <- beren5[order(beren5$Duration) , ]
Naps <- which(beren6$event == 'nap')
totalNap <- tapply(beren6$age[Naps], beren6$Duration[Naps], sum)
totalNap
plot(as.numeric(names(totalNap)), totalNap, type='b', pch=16, xlab='age in days', ylab='min asleep')
cor.test(beren6$start_hour, beren6$Duration)
# There is a negative correlation between time of day and length of the nap.





dayID <- apply(Data, 1, function(x) paste (x[1:3], collapse='-'))
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin = "2019-04-18")
Data$age <- dateID - dateID[which(Data$event == 'birth')]
beren3 <- Data
Feeds <- which(Data$event == 'bottle')
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
rateFeeds <- totalFeed/numFeeds


plot(as.numeric(names(rateFeeds)), rateFeeds, type='b', pch=16, xlab= 'age in days', ylab= 'rate of milk consumption(oz/day)')
#My hypothesis was that the rate of milk consumption at daycare decreases over time. My graph is kind of like a derivative of the one we did in part B. Amount of milk per day is measured in terms of how that changes as Beren ages. My hypothesis was not entirely supported. The end showed an increase in the rate of milk consumption toward the end, but I also could be wrong in my interpretation of how to develop a type of graph that supports this hypothesis. 
