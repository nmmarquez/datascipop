## SOC 401/538
## Baby names analysis
## data source: Social Security http://www.ssa.gov/oact/babynames/limits.html
## We will learn about data manipulation by replicating some of the results in this
## blog post: http://fivethirtyeight.com/features/how-to-tell-someones-age-when-all-you-know-is-her-name/ 

## Make sure that your code is running from the directory "LabWeek1_baby_names" where all the materials
## are stored.

## you can do this by using the command 
## setwd("PATH TO YOUR FOLDER")

## Alternatively, in Rstudio:
## 1- Click on the menu "Session"
## 2- Click "Set working directory"
## 3- Choose "To Source File Location"

rm(list=ls())
## Let's start by reading in data for births in 1950
data<- read.csv("names/yob1950.txt", header=F)

head(data)
names(data)<- c("name","sex","frequency")
head(data)

## frequency of a name
testname <- "Jerry"
testsex<- "M"

testfreq <- data$frequency[data$name == testname & data$sex== testsex]
testfreq

subset(data, name==testname)

##total births for a give sex
testtot <- sum(data$frequency[data$sex == testsex])
testtot

## percentage of people with testname
(testfreq/testtot)*100

## Several names

testname <- c("Jerry","Jerald")
testsex<- "M"

data$frequency[data$name %in% testname & data$sex== testsex]

testfreq <- sum(data$frequency[data$name %in% testname & data$sex== testsex])
testfreq

subset(data, name %in% testname)

testtot <- sum(data$frequency[data$sex== testsex])

## percentage of people with testname
(testfreq/testtot)*100

###
## What if we are interested in the name "Mary" and all of its variations?
## we can use the regular expression "grep" to identify the variations

example_txt <- c("Paul","Mary","Ann", "Maryann")
example_txt
grep("Mary", example_txt)
example_txt[grep("Mary", example_txt)]

##back to our vector of names
indexMary<- grep("Mary",data$name)
data$name[indexMary]

indexmary<- grep("mary",data$name)
data$name[indexmary]

index_all_mary <- grep("mary", tolower(data$name))
data$name[index_all_mary]

mary_variations<- data$name[index_all_mary]

data$frequency[data$name %in% mary_variations & data$sex== "F"]

marybirths <- sum(data$frequency[data$name %in% mary_variations & data$sex== "F"])

femalebirths <- sum(data$frequency[data$sex== "F"])

##percentage of 1950 births that include "mary" in their names
(marybirths/femalebirths)*100

####
## How many "Mary" born in  1950 are still alive?
## Let's get a rough approximation based on a period life table for 1950.
## Here the purspose is to practice with R. There are a number of ways to make
## the estimate more accurate. 
## Think of some of those...

##let's read in a life table from the Human Mortality database
lifetable <- read.table("bltper_1x1.txt", header=T,skip=2)
head(lifetable)
## we'll look at the "lx" column, which gives the number of survivors at age x
## out of a hypothetical initial cohort of 100,000 people

## we want to know the fraction of people born in 1950 and alive, say in 2015. Thus we want:
## a.the lifetable  for 1950
## b.the lx for age 65 (Those born in 1950 qould be 65 years old)
## c.we want the number as a fraction of the initial hypothertical cohort of 100,000 

lifetable$lx[lifetable$Year==1950 & lifetable$Age== 65]

fraction_survivors<- lifetable$lx[lifetable$Year==1950 & lifetable$Age== 65]/100000
fraction_survivors

## How many "Mary" born in 1950 are expected to be alive in 2015?
marybirths*fraction_survivors


### That was for one single year and for a single name
### Let's make things a bit more general

### read all the files with baby names and place them in a list
### Let's limit our analysis to the period 1933-2014, since we have period life tables starting
### from 1933

#Check what the paste function does
paste("yob",1950,".txt", sep="")

##lest's create an empty list
births_data<- list()

years <- c(1933:2014)

for (index in 1:length(years)){
  file_to_import<- paste("names/yob",years[index],".txt", sep="")
  births_data[[index]] <- read.csv(file_to_import,header=F,
                                   col.names=c("name","sex","frequency"))
}

##We assign to each data frame in the list the associated year 
names(births_data) <- c(1933:2014)

##if I want to see the first 3 rows and all columns for births in 1942...
births_data[["1942"]][1:3,]


## how many female "Kate" were born in 1990?
with(births_data[["1990"]],frequency[name == "Kate" & sex== "F"])


##We can create a function that takes as input a specific year, name and sex, and returns 
## the number of births
nof_births<- function(birth_year,name_person,sex_person){
	##birth_year is numeric
	##name_person is character
	##sex_person is equal to "M" or "F"
  with(births_data[[as.character(birth_year)]],frequency[name == name_person & sex== sex_person])
}


#test the function 
nof_births(1950,"Jerry","M")
nof_births(1980,"Brittany","F")
nof_births(2000,"Brittany","F") 

##Now, let's write another function that uses the previous function and a life table to return the number of people born on a given year, with a specific name and who are alive today
 
nof_alive_in_2015 <- function(year_born,first_name,sex_individual){
##sex is either "M" or "F"  
age_in_2015<- 2015 - year_born
  
fraction_survivors<- lifetable$lx[lifetable$Year==year_born & lifetable$Age== age_in_2015]/100000
births_for_given_first_name <- nof_births(year_born,first_name,sex_individual)
births_surviving<- births_for_given_first_name*fraction_survivors
return(births_surviving)
}


## example
nof_alive_in_2015(1950,"Mary","F")




##Let's consider the follwoing name and sex:
name_to_consider<- "Hannah"
sex_to_consider<- "F"


## Let's then create a matrix.
## The first column contains the year of birth. The second column contains
## the frequency of people alive today
## First we create a matrix filled of NAs
freq_over_time<- matrix(NA,81,2)
  
## Then we assign years to the first column
freq_over_time[,1] <- c(1933:2013)

## check what the try() function does...
for (i in 1:dim(freq_over_time)[1]){
 try( freq_over_time[i,2]<-   nof_alive_in_2015(freq_over_time[i,1],
                                                name_to_consider,
                                                sex_to_consider) , silent=TRUE)
}

## The average year at birth is a weighted average of the years at birth with weights given by 
## the frequency of births for the specific year
average_year_at_birth<-sum(na.omit(freq_over_time[,1]*freq_over_time[,2]))/sum(na.omit(freq_over_time[,2]))

plot(freq_over_time[,1],
     freq_over_time[,2],
     ylab="Frequency of people alive in 2015",
     xlab="Year of birth",
     main=name_to_consider,
     type ="b",
     lwd=2.5)
abline(v=average_year_at_birth, col="blue",lwd=2)




########################################
#### Extension #########################
#######################################
#Download the data for birth names by state from the Social Security website:
#http://www.ssa.gov/oact/babynames/limits.html

#Choose a state and a name of your choice. 
#1.Evaluate the popularity of the name over time by looking at the fraction/percentage of births with that name in the state over time, and plot it
#2.Evaluate whether the ratio between boys and girls for that name has been relatively stable or has changed over time.
#3.Compute the average year at birth for people with that name who are alive today across all states and report it in a table. Is there hetherogeneity across states or are the patterns similar?

   
  


