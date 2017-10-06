rm(list=ls())
library(dplyr)
library(ggplot2)

setwd("~/Documents/datascipop/LabWeek1_baby_names/state_names/")

data <- read.csv("./CO.TXT", head=F)
testname <- "Alex"

head(data)
names(data)

names(data) <- c("State", "Sex", "Birthyear", "Name", "Frequency")
names(data)

testdatasubset <- subset(data, Name == testname)
head(testdatasubset)

yearrange <- range(data$Birthyear)
allyears <- yearrange[1]:yearrange[2]
testnamepercentage <- allyears * 0


# loop through all the indexs
for(i in 1:length(allyears)){
    year <- allyears[i]
    allnamesyearsubset <- subset(data, Birthyear == year)
    testnameyearsubset <- subset(testdatasubset, Birthyear == year)
    totalbabies <- sum(allnamesyearsubset$Frequency)
    testbabies <- sum(testnameyearsubset$Frequency)
    testnamepercentage[i] <- testbabies / totalbabies
}

plot(allyears, testnamepercentage)
qplot(allyears, testnamepercentage)

data %>% filter(Name=="Neal") %>% group_by(Birthyear) %>% 
    summarise(sumf=Frequency)

state.abb

for(state in state.abb){
    filename <- paste0(state, ".TXT")
    print(head(read.csv(filename, header=F)))
}

data <- do.call(rbind, lapply(state.abb, function(x) 
    read.csv(paste0(x, ".TXT"), header=F)))
