#Loren Albert, summer 2013
# Updated December 2014
#This script is to make graphs of percentage of leaves at different ages through the dry season
# 
# To fix:
# Do search for words "check" and "note" before using for future analyses.
# Make sure that any column numbers referenced below (hard-coded) are correct if input changes
# Right now: Choose a species for graph in graphing section because code is set up to graph one tree at a time
# so make it a function instead?
# Note: I edited this script (spring 2016) and changed the header and demography data source.  I think I successfully
# reverted it to the version used for AGU 2014.
#
# Resources:
     # Making date format recognized by R: http://stackoverflow.com/questions/11891321/convert-factor-to-date-in-r

###remove all objects from workspace if necessary
rm(list=ls())

###Set working directory
setwd('/Users/lalbert/Documents/Amazon research/AGU 2014 poster/Analysis for AGU 2014/Leaf_demography_R_Code')

###Controllable Options
input="Leaf Metrics Data Organization (Final) copy (version 3)_copy2_LPA.csv"
#Note: some column numbers are referenced below, so look for the hard-coding if input file changes.

###Import data
Dem.data<-read.csv(input, header=TRUE, na.strings=c("","n/a"))
head(Dem.data)
dim(Dem.data)
#View(Dem.data)
summary(Dem.data)

###Exclude rows marked for exclusion
#Dem.dat.1Br<- Dem.data[-which(is.na(Dem.data$Date)), ] #from when I excluded all but first row for a day, which had the date
Dem.dat.1Br<-Dem.data[-which(Dem.data$Excluded.QC=='X'), ]
# Dem.dat.1m<-Dem.dat.1Br[which(Dem.dat.1Br$Branch.was.1.meter=='X'), ] #do this after calculating other columns
# Check that exclusion worked
#View(Dem.dat.1Br)
dim(Dem.dat.1Br)
is.na(Dem.data$Date)
is.na(Dem.dat.1Br$Date)
sum(is.na(Dem.data$Date))
sum(is.na(Dem.dat.1Br$Date))

###Add column where year is the same
# This is a hack so that data can be plotted by month over course of one dry to wet season
temp<-gsub(2012,1000,Dem.dat.1Br$Date)
temp1<-gsub(2013,1000,temp)
Dem.dat.1Br$No.Year.Date<-gsub(2014,1001,temp1)

###Convert 'Date' to Date recognized by R
#class(Dem.dat.1Br$Date) #check current class
Dem.dat.1Br$R.dates <- as.Date(Dem.dat.1Br$Date, "%m.%d.%Y")
Dem.dat.1Br$R.dates.noYear <- as.Date(Dem.dat.1Br$No.Year.Date, "%m.%d.%Y")
hist(Dem.dat.1Br$R.dates, breaks=30)

###Add day of year (doy) column
library(lubridate)
Dem.dat.1Br$DOY<-yday(Dem.dat.1Br$R.dates)

###Get rid of any non-numeric values before analysis, and make sure class is numeric for leaf counts
summary(Dem.dat.1Br)

###Sum of all leaves for a collection day
Dem.dat.1Br$L.sum<-rowSums(Dem.dat.1Br[ ,c(12:24)])

###Group Y1, Y1/Y2, Y2, Y3, and Y together.  (These groupings may change with a future ASD by leaf age model)
Dem.dat.1Br$Y.sum<-rowSums(Dem.dat.1Br[ ,c(12:15)])

###Group M1, M2, M, M/O, and Y/M together.  (These groupings may change with a future ASD by leaf age model)
Dem.dat.1Br$M.sum<-rowSums(Dem.dat.1Br[ ,c(16:19)])

###Group O1. O2 M3 and O together.  (These groupings may change with a future ASD by leaf age model)
#(For future analyses, check if "O1" referred to most recent old cohort.  If so, maybe it should
#be grouped with "O" and "O2" should be seperate).
Dem.dat.1Br$O.sum<-rowSums(Dem.dat.1Br[ ,c(20:24)])

###Calculate proportions belonging to each age cohort (Y, M, or O)
Dem.dat.1Br$Y.prop<-Dem.dat.1Br$Y.sum/Dem.dat.1Br$L.sum
Dem.dat.1Br$M.prop<-Dem.dat.1Br$M.sum/Dem.dat.1Br$L.sum
Dem.dat.1Br$O.prop<-Dem.dat.1Br$O.sum/Dem.dat.1Br$L.sum

#Check that proportions all add to 1
Dem.dat.1Br$QC.check<-Dem.dat.1Br$Y.prop+Dem.dat.1Br$M.prop+Dem.dat.1Br$O.prop

#Create df with only 1 meter branches
#(requires an x in at least one row of Excluded.Branch.Length)
# To do: Add X to all non 1 m branches in excel sheet!
Dem.dat.1m<-Dem.dat.1Br[-which(Dem.dat.1Br$Excluded.Branch.Length=='X'), ]

#Subsetting the data for each species for all non-excluded branches
tr9<-subset(Dem.dat.1Br, tag..=='9')
tr500<-subset(Dem.dat.1Br, tag..=='500')
tr504<-subset(Dem.dat.1Br, tag..=='504')
tr118<-subset(Dem.dat.1Br, tag..=='118')
tr11<-subset(Dem.dat.1Br, tag..=='11')
all<-rbind(tr9,tr500,tr504,tr118,tr11)

#Subsetting the data for each species for all non-excluded, 1 meter branches
tr9.1m<-subset(Dem.dat.1m, tag..=='TR9_')
tr500.1m<-subset(Dem.dat.1m, tag..=='TR500_')
tr504.1m<-subset(Dem.dat.1m, tag..=='TR504_')
tr118.1m<-subset(Dem.dat.1m, tag..=='TR118_')
tr11.1m<-subset(Dem.dat.1m, tag..=='TR11_')

#Subsetting each species subset to include sun branches for all non-excluded branches
tr9.sun<-subset(tr9, Sun.Shade=='sun')
tr500.sun<-subset(tr500, Sun.Shade=='sun')
tr504.sun<-subset(tr504, Sun.Shade=='sun')
tr118.sun<-subset(tr118, Sun.Shade=='sun')
tr11.sun<-subset(tr11, Sun.Shade=='sun')
all.sun<-rbind(tr9.sun,tr500.sun,tr504.sun,tr118.sun,tr11.sun)

#Subsetting each species subset to include sun branches for all non-excluded, 1 meter branches
tr9.sun.1m<-subset(tr9.1m, Sun.Shade=='sun')
tr500.sun.1m<-subset(tr500.1m, Sun.Shade=='sun')
tr504.sun.1m<-subset(tr504.1m, Sun.Shade=='sun')
tr118.sun.1m<-subset(tr118.1m, Sun.Shade=='sun')
tr11.sun.1m<-subset(tr11.1m, Sun.Shade=='sun')

#Subsetting by light level, all species, all non-excluded branches
sun.light<-subset(Dem.dat.1Br, Sun.Shade=='sun')
shade.light<-subset(Dem.dat.1Br, Sun.Shade=='shade')
high.light<-subset(Dem.dat.1Br, Sun.Shade=='high')
low.light<-subset(Dem.dat.1Br, Sun.Shade=='low')
MC.light<-subset(Dem.dat.1Br, Sun.Shade=='MC')
US.light<-subset(Dem.dat.1Br, Sun.Shade=='US')
all.light<-rbind(sun.light,shade.light,high.light,low.light,MC.light,US.light)

###Make graphs
###Point and line graph of proportion of each age class over time
#Based on http://rgraphgallery.blogspot.com/2013/04/rg-basic-point-and-line-graph-with.html
require(ggplot2)
require(grid)

#Choose a species for the two graphs (proportion of ages graph and sum of leaves graph)
sp<-tr11.sun.1m
species<-"Mezelaurus itauba Sun leaves 1m"
sp1<-tr11.sun.1m
species1<-"Mezelaurus itauba Sun leaves 1m"

###Point and line graph of leaf number on 1 meter branch over time
#Add age layers are added individually with curves
t<-ggplot(data=sp1)
tl<-t+geom_point(aes(x = R.dates, y = L.sum), color='black', size=4)+
  stat_smooth(aes(x = R.dates, y = L.sum), method = "loess", se = FALSE, colour="black")
tl_axes<-tl +  scale_x_date("Date") +
  scale_y_continuous("Total number of leaves on sampled 1 meter branches") +theme_bw()
tl_theme <- tl_axes + theme(panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(),
                            axis.text.x = element_text(size = 11),
                            axis.text.y = element_text(size = 11),
                            legend.title = element_blank(),
                            legend.text = element_text(size=12)) 
tl_final<-tl_theme+ggtitle(species1)

tl_final

###Point and line graph of proportion of leaves of different ages on branch over time
#Add age layers are added individually with curves
# Choose whether to include year (time series, R.dates) or compile all years (as if all 
# collected same year,R.dates.noYear).  Note that this needs to be changed for all layers below

t<-ggplot(data=sp)
ty<-t+geom_point(aes(x = R.dates.noYear, y = Y.prop), color='grey50', size=5)+
  geom_point(aes(x = R.dates.noYear, y = Y.prop), color='yellow', size=4)+
  #stat_smooth(aes(x = R.dates.noYear, y = Y.prop), method = "loess", se = FALSE, colour = "yellow")
  stat_smooth(aes(x = R.dates.noYear, y = Y.prop), method = "lm", formula = y~poly(x,2), size = 1, se = FALSE, colour = "yellow")
tym<-ty+geom_point(aes(x = R.dates.noYear, y = M.prop), color='grey50', size=5)+
  geom_point(aes(x = R.dates.noYear, y = M.prop), color='forest green', size=4)+
  #stat_smooth(aes(x = R.dates.noYear, y = M.prop), method = "loess", se = FALSE, colour="forest green")
  stat_smooth(aes(x = R.dates.noYear, y = M.prop), method = "lm", formula = y~poly(x,2), size = 1, se = FALSE, colour="forest green")
tymo<-tym+geom_point(aes(x = R.dates.noYear, y = O.prop), color='grey50', size=5)+
  geom_point(aes(x = R.dates.noYear, y = O.prop), color='tan4', size=4)+
  #stat_smooth(aes(x = R.dates.noYear, y = O.prop), method = "loess", se = FALSE, colour="tan4")
  stat_smooth(aes(x = R.dates.noYear, y = O.prop), method = "lm", formula = y~poly(x,2), size = 1, se = FALSE, colour="tan4")
tymo_axes<-tymo +  scale_x_date("Date") +
  scale_y_continuous("Proportion of leaves on sampled 1 meter branches", limits = c(0, 1))+theme_bw()
tymo_theme <-tymo_axes + theme(panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           axis.text.x = element_text(size = 11),
                           axis.text.y = element_text(size = 11),
                           legend.title = element_blank(),
                           legend.text = element_text(size=12))
tymo_final<-tymo_theme+ggtitle(species)

tymo_final

### DOY leaf age proportion
###Point and line graph of proportion of leaves of different ages on branch by DOY
#Add age layers are added individually with curves
d<-ggplot(data=sp)
dy<-d+geom_point(aes(x = DOY, y = Y.prop), color='yellow', size=4)+
  stat_smooth(aes(x = DOY, y = Y.prop), method = "loess", se = FALSE, colour = "yellow")
dym<-dy+geom_point(aes(x = DOY, y = M.prop), color='forest green', size=4)+
  stat_smooth(aes(x = DOY, y = M.prop), method = "loess", se = FALSE, colour="forest green")
dymo<-dym+geom_point(aes(x = DOY, y = O.prop), color='tan4', size=4)+
  stat_smooth(aes(x = DOY, y = O.prop), method = "loess", se = FALSE, colour="tan4")
dymo_axes<-dymo +  scale_x_continuous("DOY") +
  scale_y_continuous("Proportion of leaves on sampled 1 meter branches", limits = c(0, 1))+theme_bw()
dymo_theme <-dymo_axes + theme(panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank(),
                               axis.text.x = element_text(size = 11),
                               axis.text.y = element_text(size = 11),
                               legend.title = element_blank(),
                               legend.text = element_text(size=12))
dymo_final<-dymo_theme+ggtitle(species)

dymo_final

###Save figure as eps
#fname = paste0(species, ".eps")
#ggsave(file=fname, width=7, height = 4)


########################################################################################
###Code that I didn't end up using
#Calculate means and errors
require(plyr)
#ds <- ddply(sp, .(Tx, Temp.), summarise, 
#            N    = length(Fv.Fm),
#            mean = mean(Fv.Fm),
#            sd   = sd(Fv.Fm),
#            se   = sd(Fv.Fm) / sqrt(length(Fv.Fm)) )
#Make the graph for one species.  Need to choose a different species above to make more.
#I think I can seperate by sun and shade if I add a sun/shade column and say f1 = ggplot(data = Dem.dat.1Br, aes(x = R.dates., y = Y.prop, group = sun/shade) )
#f1 = ggplot(data = sp, aes(x = R.dates, y = Y.prop), color='yellow', size=4)
#f2<-f1+
#  geom_point(aes(shape="."), color='yellow', size=4)+
#  geom_point(aes(x = R.dates, y = M.prop, shape="."), color='forest green', size=4)+
#  geom_point(aes(x = R.dates, y = O.prop, shape="k"), color='tan4', size=4)
#geom_smooth(method = "loess", size = 1.5)
#f3 <- f2 +  scale_x_date("Date") +
#  scale_y_continuous("Proportion of leaves on sampled branch", limits = c(0, 1)) +
#  #scale_shape_manual(values=c(24,21)) + #chooses the shapes
#  #scale_colour_manual(values=c("#4D4D4D","#E6E6E6")) + #col = gray.colors(2) #These are the colors I want
#  #scale_fill_manual(values=c("#4D4D4D","#E6E6E6")) +
#  #stat_abline(intercept=0, slope=0, linetype=1) +      #Can add a line at zero for x axis
#  #annotate("text", x=32, y=.5, label="X") +            #This could be used to add a star
#  theme_bw()
#f4 <- f3 + opts(panel.grid.major = theme_blank(), 
#                panel.grid.minor = theme_blank(),
#                title=species,
#                #axis.title.x = theme_text(face="bold", size=12),
#                axis.text.x = theme_text(size = 11),
#                axis.text.y = theme_text(size = 11),
#                legend.title = theme_blank(),
#                legend.text = theme_text(size=12))
#legend.position = "bottom")
#f4