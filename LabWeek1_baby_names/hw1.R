rm(list=ls())
library(dplyr)
library(ggplot2)

setwd("~/Documents/datascipop/LabWeek1_baby_names/")

# Read in all state specific data at once 
data <- do.call(rbind, lapply(c(state.abb, "DC"), function(x) 
    read.csv(paste0("./statenames/", x, ".TXT"), header=F)))
# Reassign column names
names(data) <- c("State", "Sex", "Birthyear", "Name", "Frequency")

# Question 1
testname <- "Alex"
teststate <- "CO"

testdatasubset <- data %>% 
    filter(Name == testname & State == teststate) %>%
    group_by(Birthyear) %>% 
    summarize(Namecount=sum(Frequency))

statedatasubset <- data %>% 
    filter(State == teststate) %>%
    group_by(Birthyear) %>% 
    summarize(Birthcount=sum(Frequency))

namepdata <- left_join(statedatasubset, testdatasubset, by="Birthyear") %>% 
    mutate(Namecount = replace(Namecount, is.na(Namecount), 0), 
           Namep=Namecount / Birthcount)

ggplot(namepdata, aes(x=Birthyear, Namep)) + geom_point()

# Question 2

sexratiodata <- data %>% 
    filter(Name == testname & State == teststate) %>%
    group_by(Birthyear, Sex) %>% 
    summarize(Namecount=sum(Frequency)) %>%
    right_join(unique(data[,c("Sex", "Birthyear")])) %>% 
    group_by(Birthyear) %>% 
    summarize(Ratio = Namecount[Sex=="F"]/Namecount[Sex=="M"])

ggplot(data=sexratiodata, aes(x=Birthyear, y=Ratio)) + geom_point()

# Question 3 

(LT <- read.table("./bltper_1x1.txt", skip=2, header=T) %>%
        mutate(Age=as.numeric(as.character(Age))) %>% 
        filter(Year+Age==2015))

(palive <- LT %>% 
    mutate(Fsurvive=lx/100000) %>%
    select(Year, Age, Fsurvive))

(namesubset <- data %>% 
    filter(Name == testname) %>%
    group_by(Birthyear) %>% 
    summarize(Namecount=sum(Frequency)) %>%
    mutate(Year=Birthyear) %>%
    left_join(palive, by="Year") %>%
    mutate(Alive=Namecount*Fsurvive))

(namesubsetstate <- data %>% 
        filter(Name == testname) %>%
        group_by(Birthyear, State) %>% 
        summarize(Namecount=sum(Frequency)) %>%
        mutate(Year=Birthyear) %>%
        left_join(palive, by="Year") %>%
        mutate(Alive=Namecount*Fsurvive))

(meanageallstates <- namesubsetstate %>% mutate(Alivew=Alive*Age) %>%
        group_by(State) %>% 
        summarise(personyears=sum(Alive, na.rm=T), 
                  personyearsw=sum(Alivew, na.rm=T)) %>%
        mutate(mean_year=2015-personyearsw/personyears))

qplot(meanageallstates$mean_year)
