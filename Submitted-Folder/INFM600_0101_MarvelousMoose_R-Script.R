# Data Cleaning 
# The data can be cleaned by running the below code. We intend to assign a category for each air crash based on the words found in the summary.

#Category - Technical

#Function to assign a category name based on keyword search.Initially we assign unknown to all rows and then start replacing it with the matching category as we compare the summary. 
categorizeTech <- function(d, searchString, category) {
  d$Reason = "unknown"; 
  for(i in seq(1, length(searchString), 1)) {
    list <- grep(searchString[i], d$Summary, ignore.case=TRUE)
    if (length(list) > 0) {
      for(j in seq(1, length(list), 1)) {
        d$Reason[list[j]] <- category[1]
      }
    }
  }
  d
}

keywords <- c("engine","gear", "weight", "takeoff", "taking off", "landing", "wingtip", "wing tip", "wing", "server", "engine failure", "electrical", "engine fail","propeller","in-flight","aileron","cable","faulty","circuit","microswitch","out of fuel","navigation error","navigational","fuel exhaust","lost contact","radar","elevator boost","ran out of","structural failure","malfunct","fuselage","instrument","flap","loss of control","mechanical fail","failure of autopilot","failure of the autopilot","autopilot failure","autopilot failed","shaft","elevator","fracture")
categoryToAssign <- c("Technical Failure")
#Function call 
updatedData <- categorizeTech(originalaircrash, keywords, categoryToAssign)

#Category - Bad Weather
#We run the same function again for each category.

categorizeWeather <- function(d, searchString, category) {
  for(i in seq(1, length(searchString), 1)) {
    list <- grep(searchString[i], d$Summary, ignore.case=TRUE)
    if (length(list) > 0) {
      for(j in seq(1, length(list), 1)) {
        if(d$Reason[list[j]] == "unknown"){
          d$Reason[list[j]] <- category[1]
        }
      }
    }
  }
  d
}

keywords <- c("rain","cloud","dust","fog","weather","snow","storm","ice","lightening","thunder","icing","wind","gust","haze","hail","visibility","poor visibility","visib","turbulence")
categoryToAssign <- c("Bad Weather")
updatedData <- categorizeWeather(updatedData, keywords, categoryToAssign)


#Category - Shot Down

categorizeShot <- function(d, searchString, category) {
  for(i in seq(1, length(searchString), 1)) {
    list <- grep(searchString[i], d$Summary, ignore.case=TRUE)
    if (length(list) > 0) {
      for(j in seq(1, length(list), 1)) {
        if(d$Reason[list[j]] == "unknown"){
          d$Reason[list[j]] <- category[1]
        }
      }
    }
  }
  d
}

keywords <- c("shot","shot down","military","shell","destroy","aircraft fire","firing","by British","by German","by Japanese","attacked")
categoryToAssign <- c("Shot Down")
updatedData <- categorizeShot(updatedData, keywords, categoryToAssign)

#Category - Pilot Error

categorizePilotError <- function(d, searchString, category) {
  for(i in seq(1, length(searchString), 1)) {
    list <- grep(searchString[i], d$Summary, ignore.case=TRUE)
    if (length(list) > 0) {
      for(j in seq(1, length(list), 1)) {
        if(d$Reason[list[j]] == "unknown"){
          d$Reason[list[j]] <- category[1]
        }
      }
    }
  }
  d
}

keywords <- c("pilot error","miscommunication","jumped","abandoned","pilot's failure","failure of the pilot","co-pilot","inability of","misjudg","procedure","prescribed","approved","error of judgement","pilot did not follow","procedural","errors by the pilot","pilot failed to","lack of pilot","overconfidence","disregarded","lack of experience","inadvertent","inadvertant","misinterpret","misread","pilots failed to")
categoryToAssign <- c("Pilot Error")
updatedData <- categorizePilotError(updatedData, keywords, categoryToAssign)

#Category - Collision

categorizeCollision <- function(d, searchString, category) {
  for(i in seq(1, length(searchString), 1)) {
    list <- grep(searchString[i], d$Summary, ignore.case=TRUE)
    if (length(list) > 0) {
      for(j in seq(1, length(list), 1)) {
        if(d$Reason[list[j]] == "unknown"){
          d$Reason[list[j]] <- category[1]
        }
      }
    }
  }
  d
}

keywords <- c("mid-air","mid air collision","midair collision","collision","collided","collision occured","struck","struck a hill","hit a hill","crashed into a hill","struck a hill","hit a hill","crashed into hill","crashed into a mountain","into a hill","into a mountain","into mountain","into hill","steep")
categoryToAssign <- c("Collision")
updatedData <- categorizeCollision(updatedData, keywords, categoryToAssign)

#Category - Hijacked

categorizeHijacked <- function(d, searchString, category) {
  for(i in seq(1, length(searchString), 1)) {
    list <- grep(searchString[i], d$Summary, ignore.case=TRUE)
    if (length(list) > 0) {
      for(j in seq(1, length(list), 1)) {
        if(d$Reason[list[j]] == "unknown"){
          d$Reason[list[j]] <- category[1]
        }
      }
    }
  }
  d
}

keywords <- c("hijack","hijackers","Hijack","hijack")
categoryToAssign <- c("Hijacked")
updatedData <- categorizeHijacked(updatedData, keywords, categoryToAssign)


#Cleaning the time column and adding a new nominal variable (time bucket - Day, Night)

#We remove the time enteries which have null values and convert the time in decimal format.
crashDataWithTime = subset(updatedData,updatedData$Time != '')
newtime = sapply(strsplit(as.character(crashDataWithTime$Time),":"),
                 function(x) {
                   x <- as.numeric(x)
                   x[1]+x[2]/60
                 }
)

crashDataWithTime$converted_time = newtime

#Assign nominal variables to a new column time_bucket with labels Day, Night.
crashDataWithTime$time_bucket = ifelse(crashDataWithTime$converted_time > 5 & crashDataWithTime$converted_time < 17,"Day","Night")

#End of Data Cleaning

#Draft for research questions:
#Research Question 1- Do more air crash accidents happen during day as compared to the night?

table(crashDataWithTime$time_bucket)

#Research Question 2 - Which reason causes the maximum number of air crashes? 

barplot(table(updatedData$Reason))

#Research Question 3 - Is there a specific operator of airplanes that is prevalent in many of the air crashes that have happened? 

#We install a package 'plyr'
install.packages("plyr")
library(plyr)

#Count the frequency of each operator
OperatorFrequency = count(updatedData,"Operator")
attach(OperatorFrequency)

#Sort the result in descending order of frequency
OperatorFrequency = OperatorFrequency[order(-freq),]

#Research Question 4 - Which type of aircrash results in maximum number of fatalities? 

#Create a new subset where the fatalities in not null.
crashDataWithFatalities = subset(updatedData,updatedData$Fatalities != "")
#We create a summary of the total number of deaths for each category.
sumOfFatalities = tapply(crashDataWithFatalities$Fatalities,crashDataWithFatalities$Reason,sum)

#Perform a one-way ANOVA to test the effect of different categories on the number of deaths.
summary(aov(crashDataWithFatalities$Fatalities ~ crashDataWithFatalities$Reason))

#Perform a pairwise t-test within each categories to find the most significant category.
pairwise.t.test(crashDataWithFatalities$Fatalities,crashDataWithFatalities$Reason,p.adj ="bonferroni")

#plot graphs for our result.
barplot(tapply(crashDataWithFatalities$Fatalities,crashDataWithFatalities$Reason,sum),beside=T, ylab="Fatalities",xlab="Reason for Air-Crash", ylim=c(0,80000))

#load the gglpot2 package
library(ggplot2)

#plot ggplot for Reason and fatalities with error bars.
ggplot (crashDataWithFatalities,aes(x=Reason,y=Fatalities))+stat_summary( fun.y = "mean",geom = "bar",position = "dodge")+stat_summary(fun.data=mean_cl_normal,geom="errorbar")


#Descriptive statistics for some of the variables we are making use of:


##Mean and Standard Deviations for Ratio variables
#Converted Time

#calculates the mean of the converted_time variable
mean(converted_time,na.rm = TRUE) 

#calculates the standard deviation of the converted_time variable
sd(converted_time,na.rm = TRUE)

#displays a histogram for the converted_time variable
hist(converted_time)

convertedTimeDensity <- density(converted_time,na.rm = TRUE) 
plot(convertedTimeDensity)# To display a density plot for the converted_time variable

#Aboard

#calculates the mean of the aboard variable
mean(aboard)

#calculates the standard deviation of the aboard variable
sd(aboard)

#displays a histogram for the aboard variable
hist(aboard)
aboardDensity <- density(aboard)

# To display a density plot for the aboard variable
plot(aboardDensity)

#Fatalities

#calculates the mean of the fatalities variable
mean(fatalities)

#calculates the standard deviation of the fatalities variable
sd(fatalities)

#displays a histogram for the fatalities variable
hist(fatalities)

fatalitiesDensity <- density(newTime) 
# To display a density plot for the fatalities variable
plot(fatalitiesDensity)


##Mode for Nominal variables

#New method added to calculate the mode of a nominal variable 
Mode <- function(a) {
     b <- unique(a)
     b[which.max(tabulate(match(a, b)))]
 }

#Calculating the mode of the nominal variable 'Operator'
Mode(Operator)

#Calculating the mode of the nominal variable 'Type'
Mode(Type) 

 

