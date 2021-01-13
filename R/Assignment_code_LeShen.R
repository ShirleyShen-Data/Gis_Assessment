# install package
install.packages("HH")

# library package
library(HH)

# Read data
LanduseData <- read.csv("Landuse2000_2010.csv")
LanduseData
attach(LanduseData)

#  balanced design
table(Time,Distance)

# provide the cell means and standard deviations of Commercial
aggregate(Commercial,by=list(Time,Distance),FUN=mean,na.rm=TRUE)
aggregate(Commercial,by=list(Time,Distance),FUN=sd,na.rm=TRUE)

# ANOVA analysis of Commercial
fit<-aov(Commercial~Time*Distance)
summary(fit)

#  produce a plot of both main effects and two-way interactions
interaction2wt(Commercial~Time*Distance)

# provide the cell means and standard deviations of Industrial
aggregate(Industrial,by=list(Time,Distance),FUN=mean,na.rm=TRUE)
aggregate(Industrial,by=list(Time,Distance),FUN=sd,na.rm=TRUE)

# ANOVA analysis of Industrial
fit<-aov(Industrial~Time*Distance)
summary(fit)

#  produce a plot of both main effects and two-way interactions
interaction2wt(Industrial~Time*Distance)

# provide the cell means and standard deviations of Public_service
aggregate(Public_service,by=list(Time,Distance),FUN=mean)
aggregate(Public_service,by=list(Time,Distance),FUN=sd)

# ANOVA analysis of Public_service
fit<-aov(Public_service~Time*Distance)
summary(fit)

#  produce a plot of both main effects and two-way interactions
interaction2wt(Public_service~Time*Distance)

# provide the cell means and standard deviations of Residential
aggregate(Residential,by=list(Time,Distance),FUN=mean)
aggregate(Residential,by=list(Time,Distance),FUN=sd)

# ANOVA analysis of Residential
fit<-aov(Residential~Time*Distance)
summary(fit)

#  produce a plot of both main effects and two-way interactions
interaction2wt(Residential~Time*Distance)
