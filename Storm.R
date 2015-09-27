# Install package and include the library
# install.packages("R.utils")
library(R.utils)
library(plyr)
library(ggplot2)
library(gridExtra)

# download file
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "~/data/storm.csv.bz2")

# unzip file and read data
bunzip2("~/data/storm.csv.bz2", "~/data/storm.csv", remove = FALSE)
data <- read.csv("~/data/storm.csv")

#check dataset
format(object.size(data), units = "Mb")
head(data,3)
names(data)
dim(data)

#Across the United States, 
#which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?
health <- subset (data, select = c(EVTYPE,FATALITIES,INJURIES))
stat <- ddply(health, .(EVTYPE), summarize,fatalities = sum(FATALITIES),injuries = sum(INJURIES))
fatalities <- head(stat[order(stat$fatalities, decreasing = TRUE),c(1,2)],10)
#injuries <- stat[order(stat$injuries, decreasing = TRUE),c(1,3)]
injuries <- head(stat[order(stat$injuries, decreasing = TRUE),c(1,3)],10)

#Across the United States, which types of events have the greatest economic consequences?
economic <- subset (data, select = c(EVTYPE,PROPDMG,PROPDMGEXP,CROPDMG,CROPDMGEXP))
#A multiplier where Hundred (H), Thousand (K), Million (M), Billion (B)
convert <- function (dmexp) {
  dmexp <- toupper(dmexp)
  dmexp <- ifelse(dmexp == ''|dmexp == '0', 1, dmexp)
  dmexp <- ifelse(dmexp == '1', 10, dmexp)
  dmexp <- ifelse(dmexp == 'H'|dmexp == '2', 100, dmexp) 
  dmexp <- ifelse(dmexp == 'K'|dmexp == '3', 1e+03, dmexp)
  dmexp <- ifelse(dmexp == '4', 1e+04, dmexp)
  dmexp <- ifelse(dmexp == '5', 1e+05, dmexp)
  dmexp <- ifelse(dmexp == 'M'|dmexp == '6', 1e+06, dmexp) 
  dmexp <- ifelse(dmexp == '7', 1e+07, dmexp)
  dmexp <- ifelse(dmexp == '8', 1e+08, dmexp)
  dmexp <- ifelse(dmexp == 'B',1e+09, dmexp) 
  dmexp <- ifelse(dmexp == '?'|dmexp == '+'|dmexp == '-', 0, dmexp) 
  return (as.numeric(dmexp))
}
economic$prop <- convert(economic$PROPDMGEXP)*economic$PROPDMG
economic$crop <- convert(economic$CROPDMGEXP)*economic$CROPDMG
economstat <- ddply(economic, .(EVTYPE), summarize,prop = sum(prop)/1000000,crop = sum(crop)/1000000)
prop <- economstat[order(economstat$prop, decreasing = TRUE),c(1,2)]
crop <- economstat[order(economstat$crop, decreasing = TRUE),c(1,3)]

#Results
head(fatalities)
head(injuries)
head(prop)
head(crop)

#Plots for health stat
fplot <- ggplot(fatalities, aes(x=reorder(EVTYPE, -fatalities), y=fatalities)) + 
  geom_bar(stat="identity", fill="green") +
  labs(title="Number of fatalities", x="Type of weather events", y="Number of fatalities") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), aspect.ratio = 1)
iplot <- ggplot(injuries, aes(x=reorder(EVTYPE, -injuries), y=injuries)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(title="Number of injuries", x="Type of weather events", y="Number of injuries") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), aspect.ratio = 1) 
grid.arrange(fplot, iplot, ncol=2)

#Plots for economic stat
pplot <- ggplot(head(prop,10), aes(x=reorder(EVTYPE, -prop), y=prop)) + 
  geom_bar(stat="identity", fill="green") +
  labs(x="Type of weather events", y="Number of property damaged") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
cplot <- ggplot(head(crop,10), aes(x=reorder(EVTYPE, -crop), y=crop)) + 
  geom_bar(stat="identity", fill="blue") +
  labs(x="Type of weather events", y="Number of crop") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
grid.arrange(pplot, cplot, ncol=2)


