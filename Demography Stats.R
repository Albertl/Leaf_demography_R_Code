# Loren Albert Aug 2013
# Script to calculate stats on early versus late dry season
#Do search for words "check" and "note" before using for future analyses.
# Resources:
# Making date format recognized by R: http://stackoverflow.com/questions/11891321/convert-factor-to-date-in-r


###Set working directory
setwd('/Users/lalbert/Documents/Amazon research/summer 2012 TNF/ESA talk 2013/Demography analysis for ESA talk')

###Controllable Options
input="Leaf Metrics Data Organization (Final)+sums+date-filled.csv"
#Note: some column numbers are referenced below, so look for the hard-coding if input file changes.
#Choose a species for graph in graphing section because code is set up to graph one tree at a time

###Import data
Dem.data<-read.csv(input, header=TRUE, na.strings=c("","n/a","NA"))
head(Dem.data)
dim(Dem.data)
#View(Dem.data)
summary(Dem.data)

###Exclude rows marked for exclusion
#Dem.dat.1Br<- Dem.data[-which(is.na(Dem.data$Date)), ] #from when I excluded all but first row for a day, which had the date
Dem.dat.1Br<-Dem.data[-which(Dem.data$Excluded.for.ESA.Talk=='X'), ]
# Dem.dat.1m<-Dem.dat.1Br[which(Dem.dat.1Br$Branch.was.1.meter=='X'), ] #do this after calculating other columns
# Check that exclusion worked
#View(Dem.dat.1Br)
dim(Dem.dat.1Br)
is.na(Dem.data$Date)
is.na(Dem.dat.1Br$Date)
sum(is.na(Dem.data$Date))
sum(is.na(Dem.dat.1Br$Date))

###Convert 'Date' to Date recognized by R
#class(Dem.dat.1Br$Date) #check current class
Dem.dat.1Br$R.dates <- as.Date(Dem.dat.1Br$Date, "%m.%d.%Y")
hist(Dem.dat.1Br$R.dates, breaks=30)

###Get rid of any non-numeric values before analysis, and make sure class is numeric for leaf counts
summary(Dem.dat.1Br)

###Sum of all leaves for a collection day
Dem.dat.1Br$L.sum<-rowSums(Dem.dat.1Br[ ,c(6:18)])

###Group Y1, Y1/Y2, Y2, Y3, Y and Y/M together.  (These groupings may change with a future ASD by leaf age model)
Dem.dat.1Br$Y.sum<-rowSums(Dem.dat.1Br[ ,c(6:11)])

###Group M1, M2, M, M/O together.  (These groupings may change with a future ASD by leaf age model)
Dem.dat.1Br$M.sum<-rowSums(Dem.dat.1Br[ ,c(12:15)])

###Group O1. O2 and O together.  (These groupings may change with a future ASD by leaf age model)
#(For future analyses, check if "O1" referred to most recent old cohort.  If so, maybe it should
#be grouped with "O" and "O2" should be seperate).
Dem.dat.1Br$O.sum<-rowSums(Dem.dat.1Br[ ,c(16:18)])

###Calculate proportions belonging to each age cohort (Y, M, or O)
Dem.dat.1Br$Y.prop<-Dem.dat.1Br$Y.sum/Dem.dat.1Br$L.sum
Dem.dat.1Br$M.prop<-Dem.dat.1Br$M.sum/Dem.dat.1Br$L.sum
Dem.dat.1Br$O.prop<-Dem.dat.1Br$O.sum/Dem.dat.1Br$L.sum

#Check that proportions all add to 1
Dem.dat.1Br$QC.check<-Dem.dat.1Br$Y.prop+Dem.dat.1Br$M.prop+Dem.dat.1Br$O.prop

#Create df with only 1 meter branches
Dem.dat.1m<-Dem.dat.1Br[which(Dem.dat.1Br$Branch.was.1.meter=='X'), ]

#Pool by early and late
Dem.dat.1m$early.or.late<-ifelse(Dem.dat.1m$R.dates<"2012-10-15","early","late")

#Subset species of interest
tr9.1m<-subset(Dem.dat.1m, tag..=='9')
tr500.1m<-subset(Dem.dat.1m, tag..=='500')
tr504.1m<-subset(Dem.dat.1m, tag..=='504')
tr14.1m<-subset(Dem.dat.1m, tag..=='14')

###Calculate the means and summary stats
require(plyr)
#Because the length command doesn't include na.rm as an option, check the length two ways
length(Dem.dat.1m$M.sum)
sum(!is.na(Dem.dat.1m$M.sum))
#The two commands above should yeild the same sample size
cdata <- ddply(Dem.dat.1m, .(tag.., early.or.late), summarise, 
               N    = length(M.prop),
               mean = mean(M.prop),
               sd   = sd(M.prop),
               se   = sd(M.prop) / sqrt(length(M.sum)))

tr9.c<-subset(cdata, tag..=='9')
tr14.c<-subset(cdata, tag..=='14')
tr500.c<-subset(cdata, tag..=='500')
tr504.c<-subset(cdata, tag..=='504')

#Changes in mean proportion of different leaf ages from early to late dry season
#Anova
#Define x and y and data
data<-tr9.1m
x<-data$early.or.late
#y<-tr9.1m$Y.sum
#y<-data$M.sum
#y<-tr9.1m$O.sum

#percentage to scale values
#y<-data$Y.prop
y<-data$M.prop
#y<-data$O.prop

#Logit transformations
#y<-logit(tr9$percent.Y)
#y<-logit(tr9$percent.M)
#y<-logit(tr9$percent.O)

lm.y<-lm(y~x)
qqPlot(lm.y)
plot(lm.y)
fligner.test(y~x, data=data)
bartlett.test(y~x, data=data)
boxplot(y~x)
anova(lm.y)