rm(list=ls())
library(dplyr)
library(ggplot2)

setwd("~/Documents/datascipop/LabWeek1_baby_names/")

# Read in all state specific data at once 
data <- do.call(rbind, lapply(c(state.abb, "DC"), function(x) 
    read.csv(paste0("./state_names/", x, ".TXT"), header=F)))
# Reassign column names
names(data) <- c("State", "Sex", "Birthyear", "Name", "Frequency")

# Question 1
## Name and state to use in analysis
testname <- "Alex"
teststate <- "CO"

testdatasubset <- data %>% 
    # filter data by name and state
    filter(Name == testname & State == teststate) %>%
    # group by the year
    group_by(Birthyear) %>%
    # get the sum of the counts for each year
    summarize(Namecount=sum(Frequency))

statedatasubset <- data %>% 
    # filter by state
    filter(State == teststate) %>%
    # group by year
    group_by(Birthyear) %>% 
    # get the count of all names for each year
    summarize(Birthcount=sum(Frequency))

namepdata <- statedatasubset %>%
    # join the two data sets together using the all name data as full key
    left_join(testdatasubset, by="Birthyear") %>% 
    # Calculate the probability of test name
    mutate(Namecount = replace(Namecount, is.na(Namecount), 0), 
           Namep=Namecount / Birthcount)

# plot the time series
ggplot(namepdata, aes(x=Birthyear, Namep)) + geom_point()

# Question 2

sexratiodata <- data %>% 
    # filter by name and state
    filter(Name == testname & State == teststate) %>%
    # group by year and sex
    group_by(Birthyear, Sex) %>%
    # get the count of each year for both sexs
    summarize(Namecount=sum(Frequency)) %>%
    # join on a dataset that has all year sex combinations
    right_join(unique(data[,c("Sex", "Birthyear")])) %>%
    # group by the birth year
    group_by(Birthyear) %>%
    # calculate the ratio
    summarize(Ratio=Namecount[Sex=="F"]/Namecount[Sex=="M"])

# plot the time series
ggplot(data=sexratiodata, aes(x=Birthyear, y=Ratio)) + geom_point()

# Question 3 

# read in the life table data set filtering only for 2015
(LT <- read.table("./bltper_1x1.txt", skip=2, header=T) %>%
        mutate(Age=as.numeric(as.character(Age))) %>% 
        filter(Year+Age==2015))

# calculate the probability of being alive for all ages in 2015
(palive <- LT %>% 
    mutate(Fsurvive=lx/100000) %>%
    select(Year, Age, Fsurvive))

(namesubsetstate <- data %>% 
        # subset by name
        filter(Name == testname) %>%
        # group by year and state
        group_by(Birthyear, State) %>%
        # get the sum of the name count 
        summarize(Namecount=sum(Frequency)) %>%
        # Cretae new year variable so that the names match
        mutate(Year=Birthyear) %>%
        # join datatsets on years
        left_join(palive, by="Year") %>%
        # estimate number of individuals alive based on survival prob
        mutate(Alive=Namecount*Fsurvive))

(meanageallstates <- namesubsetstate %>% 
        # Create an age weighted alive number variable
        mutate(Alivew=Alive*Age) %>%
        # group by state across all years
        group_by(State) %>%
        # calcualte aggregated alive and alive weighted numbers
        summarise(personyears=sum(Alive, na.rm=T), 
                  personyearsw=sum(Alivew, na.rm=T)) %>%
        # get the mean year of birth by subtracting age from 2015
        mutate(mean_year=2015-personyearsw/personyears))

# histogram of data
qplot(meanageallstates$mean_year)
