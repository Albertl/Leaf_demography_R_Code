# Loren Albert
# Dec 2014
# Branch level Vcmax estimations
# Goals is Multiply leaf age proportion by the mean Vcmax for each age class, then make graphs
# Inputs: Leaf demography time series, and mean Vcmax for each leaf age category
# Note: right now this is set up to work with variables in workspace after running A_Ci_parameter_graphs copy R and
# leaf demography graphs.R from the 'Analysis for AGU' analyses
# Note: Search for 'note' before adapting code for other future analyses

### Identify Mean Vcmax by leaf age from workspace dataframes for trees of interest
# ymo.sun.means includes mean Vcmax of sun leaves for trees 118, 500, 9 and 504
# Note that code below assumes Vcmax column is fourth column (excluding column of row numbers from R)
tr9.sun.y.vcmax <-ymo.sun.means[ which(ymo.sun.means$Tree=='TR9'& ymo.sun.means$Leaf.age.ordered=="young"), 4]
tr9.sun.m.vcmax <-ymo.sun.means[ which(ymo.sun.means$Tree=='TR9'& ymo.sun.means$Leaf.age.ordered=="mature"), 4]
tr9.sun.o.vcmax <-ymo.sun.means[ which(ymo.sun.means$Tree=='TR9'& ymo.sun.means$Leaf.age.ordered=="old"), 4]

### Multiply leaf age proportion by the mean Vcmax for each age class
tr9.sun.1m$vcmax.Y.prop<-tr9.sun.y.vcmax * tr9.sun.1m$Y.prop
tr9.sun.1m$vcmax.M.prop<-tr9.sun.m.vcmax * tr9.sun.1m$M.prop
tr9.sun.1m$vcmax.O.prop<-tr9.sun.o.vcmax * tr9.sun.1m$O.prop
# Sum all the weighted Vcmax for a branch-level total
tr9.sun.1m$vcmax.weighted<-tr9.sun.1m$vcmax.O.prop+tr9.sun.1m$vcmax.M.prop+tr9.sun.1m$vcmax.Y.prop

### DOY leaf age vcmax proportion
###Point and line graph of vcmax * proportion of leaves of different ages on branch by DOY
#Add age layers are added individually with curves
# First choose tree and make name for title
sp<-tr9.sun.1m
species<-"Erisma Sun leaves 1m"
upper.y.lim<-max(max(sp$vcmax.Y.prop),max(sp$vcmax.M.prop),max(sp$vcmax.M.prop)) #find upper y limit

b<-ggplot(data=sp)
by<-b+geom_point(aes(x = DOY, y = vcmax.Y.prop), color='yellow', size=4)+
  stat_smooth(aes(x = DOY, y = vcmax.Y.prop), method = "loess", se = FALSE, colour = "yellow")
bym<-by+geom_point(aes(x = DOY, y = vcmax.M.prop), color='forest green', size=4)+
  stat_smooth(aes(x = DOY, y = vcmax.M.prop), method = "loess", se = FALSE, colour="forest green")
bymo<-bym+geom_point(aes(x = DOY, y = vcmax.O.prop), color='tan4', size=4)+
  stat_smooth(aes(x = DOY, y = vcmax.O.prop), method = "loess", se = FALSE, colour="tan4")
bymo_axes<-bymo +  scale_x_continuous("DOY") +
  scale_y_continuous("Vcmax scaled by proportion of leaves on sampled 1 meter branches", limits = c(0, upper.y.lim))+theme_bw()
bymo_theme <-bymo_axes + theme(panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank(),
                               axis.text.x = element_text(size = 11),
                               axis.text.y = element_text(size = 11),
                               legend.title = element_blank(),
                               legend.text = element_text(size=12))
bymo_final<-bymo_theme+ggtitle(species)

bymo_final

### Vcmax scaled to 1 m branch
# Point and line graph of total Vcmax for branch (weighted by leaf age proportion)
# DOY version
vb<-ggplot(data=sp)
vbtot<-vb+geom_point(aes(x = DOY, y = vcmax.weighted), color='black', size=4)+
  stat_smooth(aes(x = DOY, y = vcmax.weighted), method = "loess", se = FALSE, colour = "#0072B2")
vbtot_axes<-vbtot +  scale_x_continuous("DOY") +
  scale_y_continuous("Branch-level Vcmax weighted by leaf age proportion")+theme_bw()

### Vcmax scaled to 1 m branch
# Point and line graph of total Vcmax for branch (weighted by leaf age proportion)
# month tick version (using date hack from 'leaf demography graphs.R)
mvb<-ggplot(data=sp)
#mvbtot<-mvb+geom_point(aes(x = R.dates.noYear, y = vcmax.weighted), color='black', size=4)+
#  stat_smooth(aes(x = R.dates.noYear, y = vcmax.weighted), method = "loess", se = FALSE, colour = "#0072B2")
mvbtot<-mvb+geom_point(aes(x = R.dates.noYear, y = vcmax.weighted), color='black', size=4)+
  stat_smooth(aes(x = R.dates.noYear, y = vcmax.weighted), method = "lm", formula = y~poly(x,2), size = 1, se = FALSE, colour = "#0072B2")
mvbtot_axes<-mvbtot +  scale_x_date("Date") +
  scale_y_continuous("Branch-level Vcmax weighted by leaf age proportion")+theme_bw()
mvbtot_theme<-mvbtot_axes+ theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        legend.title = element_blank(),
        legend.text = element_text(size=12))
mvbtot_final<-mvbtot_theme+ggtitle(species)

mvbtot_final

###Save last figure as eps
#fname = paste0(species, "branch-level.eps")
#ggsave(file=fname, width=7, height = 4)
