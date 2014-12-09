#Loren Albert, summer 2013
#This script is to make graphs of percentage of leaves at different ages through the dry season
#Do search for words "check" and "note" before using for future analyses.
# Resources:
     # Making date format recognized by R: http://stackoverflow.com/questions/11891321/convert-factor-to-date-in-r


###Set working directory
setwd('/Users/lalbert/Documents/Amazon research/AGU 2014 poster/Analysis for AGU 2014/Leaf_demography_R_Code')

###Controllable Options
input="Leaf Metrics Data Organization (Final) copy (version 3)_copy2.csv"
#Note: some column numbers are referenced below, so look for the hard-coding if input file changes.
#Choose a species for graph in graphing section because code is set up to graph one tree at a time

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

###Convert 'Date' to Date recognized by R
#class(Dem.dat.1Br$Date) #check current class
Dem.dat.1Br$R.dates <- as.Date(Dem.dat.1Br$Date, "%m.%d.%Y")
hist(Dem.dat.1Br$R.dates, breaks=30)

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
#test of adding X to see if indexing works
Dem.dat.1m<-Dem.dat.1Br[-which(Dem.dat.1Br$Excluded.Branch.Length=='X'), ]

#Subsetting the data for each species for all non-excluded branches
tr9<-subset(Dem.dat.1Br, tag..=='9')
tr500<-subset(Dem.dat.1Br, tag..=='500')
tr504<-subset(Dem.dat.1Br, tag..=='504')
tr118<-subset(Dem.dat.1Br, tag..=='118')
tr11<-subset(Dem.dat.1Br, tag..=='11')
all<-rbind(tr9,tr500,tr504,tr118,tr11)

#Subsetting the data for each species for all non-excluded, 1 meter branches
tr9.1m<-subset(Dem.dat.1m, tag..=='9')
tr500.1m<-subset(Dem.dat.1m, tag..=='500')
tr504.1m<-subset(Dem.dat.1m, tag..=='504')
tr118.1m<-subset(Dem.dat.1m, tag..=='118')
tr11.1m<-subset(Dem.dat.1m, tag..=='11')

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
sp<-tr9.sun.1m
#sp1<-tr504.sun.1m
species<-"Erisma Sun leaves 1m"

###Point and line graph of leaf number on 1 meter branch over time
#Add age layers are added individually with curves
t<-ggplot(data=sp1)
tl<-t+geom_point(aes(x = R.dates, y = L.sum), color='black', size=4)+
  stat_smooth(aes(x = R.dates, y = L.sum), method = "loess", se = FALSE, colour="black")
tl_axes<-tl +  scale_x_date("Date") +
  scale_y_continuous("Total number of leaves on sampled 1 meter branches") +theme_bw()
tl_final <- tl_axes + opts(panel.grid.major = theme_blank(), 
                panel.grid.minor = theme_blank(),
                title=species,
                axis.text.x = theme_text(size = 11),
                axis.text.y = theme_text(size = 11))
tl_final

###Point and line graph of proportion of leaves of different ages on branch over time
#Add age layers are added individually with curves
t<-ggplot(data=sp)
ty<-t+geom_point(aes(x = R.dates, y = Y.prop), color='yellow', size=4)+
  stat_smooth(aes(x = R.dates, y = Y.prop), method = "loess", se = FALSE, colour = "yellow")
tym<-ty+geom_point(aes(x = R.dates, y = M.prop), color='forest green', size=4)+
  stat_smooth(aes(x = R.dates, y = M.prop), method = "loess", se = FALSE, colour="forest green")
tymo<-tym+geom_point(aes(x = R.dates, y = O.prop), color='tan4', size=4)+
  stat_smooth(aes(x = R.dates, y = O.prop), method = "loess", se = FALSE, colour="tan4")
tymo_axes<-tymo +  scale_x_date("Date") +
  scale_y_continuous("Proportion of leaves on sampled 1 meter branches", limits = c(0, 1))+theme_bw()
tymo_final <- tymo_axes + opts(panel.grid.major = theme_blank(), 
                               panel.grid.minor = theme_blank(),
                               title=species,
                               axis.text.x = theme_text(size = 11),
                               axis.text.y = theme_text(size = 11))
tymo_final

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