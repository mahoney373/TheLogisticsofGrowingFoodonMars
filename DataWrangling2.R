library(rvest)
library(xml2)
library(stringr)
mCal=read.csv(file="mCal.csv")
fCal=read.csv(file="fCal.csv")
mCal[,2]=as.numeric(gsub(",","",mCal[,2]))
mCal[,3]=as.numeric(gsub(",","",mCal[,3]))
mCal[,4]=as.numeric(gsub(",","",mCal[,4]))
fCal[,2]=as.numeric(gsub(",","",fCal[,2]))
fCal[,3]=as.numeric(gsub(",","",fCal[,3]))
fCal[,4]=as.numeric(gsub(",","",fCal[,4]))
rownames(mCal)=mCal[,1]
rownames(fCal)=fCal[,1]
MarsWeather.df=read.csv(file="MarsWeather030920.csv")
MarsWeather=function(File,df){
  weather<-read.csv(file=File)
  MarsWeather.df<<-merge.data.frame(MarsWeather.df,weather,all=TRUE)
}
#The function is in case I want to add more data
#I had to take the degrees fahrenheit symbol off the columns below
options(digits=4)
MarsWeather.df[,3]=as.numeric(as.character(str_extract(MarsWeather.df[,3],"[[:punct:]]?[0-9]*+[[:punct:]]?[0-9]*")))
MarsWeather.df[,4]=as.numeric(as.character(str_extract(MarsWeather.df[,4],"[[:punct:]]?[0-9]*+[[:punct:]]?[0-9]*")))
MarsWeather.df[,5]=as.numeric(as.character(str_extract(MarsWeather.df[,5],"[[:punct:]]?[0-9]*+[[:punct:]]?[0-9]*")))
MarsWeather.df[,6]=as.numeric(as.character(MarsWeather.df$Wind.Max.))
MarsWeather.df[,7]=as.numeric(as.character(MarsWeather.df$Wind.Avg.))
MarsWeather.df[,8]=as.numeric(as.character(MarsWeather.df$Wind.Min.))
MarsWeather.df[,11]=as.numeric(as.character(MarsWeather.df$Pressure.Avg.))
library(tidyr)
library(dplyr)
MarsWeather.df=MarsWeather.df %>% arrange(MarsWeather.df$Sol)
rownames(MarsWeather.df)=MarsWeather.df[,2] #Mars sols became row names
MarsWeather.df=MarsWeather.df[,-2]
nrow(MarsWeather.df)
RegolithpH=8.3
#bins=c(Feb.,May,Jan.,Mar.,July,Sept.,Oct.)
#plot(MarsWeather.df$Wind.Direction,xlab="Average Wind Direction",ylab="Number of Days with Specific Wind Direction")
#plot(MarsWeather.df$Temp.Avg...F.,main="Daily Temperature Averages",ylab="Temperature (°F)")
#plot(MarsWeather.df$Wind.Avg.,main="Daily Wind Speed Averages",ylab="Speed (mph)")
#plot(MarsWeather.df$Pressure.Avg.,main="Daily Pressure Averages",ylab="Pressure (Pa)")
#boxplot(MarsWeather.df$Wind.Avg.[MarsWeather.df$Wind.Direction=="SSE"],MarsWeather.df$Wind.Avg.[MarsWeather.df$Wind.Direction=="W"],MarsWeather.df$Wind.Avg.[MarsWeather.df$Wind.Direction=="WNW"],MarsWeather.df$Wind.Avg.[MarsWeather.df$Wind.Direction=="SE"],MarsWeather.df$Wind.Avg.[MarsWeather.df$Wind.Direction=="SSW"],MarsWeather.df$Wind.Avg.[MarsWeather.df$Wind.Direction=="SW"],names=c("SSE","W","WNW","SE","SSW","SW"),ylab="Daily Average Wind Speed (mph)",main="Wind Speed Distributions for Average Wind Direction",xlab="Average Wind Direction")

#boxplot(MarsWeather.df$Temp.Avg...F.[MarsWeather.df$Wind.Direction=="SSE"],MarsWeather.df$Temp.Avg...F.[MarsWeather.df$Wind.Direction=="W"],MarsWeather.df$Temp.Avg...F.[MarsWeather.df$Wind.Direction=="WNW"],MarsWeather.df$Temp.Avg...F.[MarsWeather.df$Wind.Direction=="SE"],MarsWeather.df$Temp.Avg...F.[MarsWeather.df$Wind.Direction=="SSW"],MarsWeather.df$Temp.Avg...F.[MarsWeather.df$Wind.Direction=="SW"],names=c("SSE","W","WNW","SE","SSW","SW"),ylab="Daily Average Temperature (°F)",main="Temperature Distributions for Average Wind Direction",xlab="Average Wind Direction")

#plot(MarsWeather.df$Temp.Avg...F.~MarsWeather.df$Pressure.Avg.,main="Avg. Temperture vs Pressure",xlab="Pressure (Pa)",ylab="Temperature (°F)")
#plot(MarsWeather.df$Wind.Avg.~MarsWeather.df$Pressure.Avg.,main="Avg. Wind Speed vs Pressure",xlab="Pressure (Pa)",ylab="Wind Speed (mph)")
#plot(MarsWeather.df$Wind.Avg.~MarsWeather.df$Temp.Avg...F.,main="Avg. Temperture vs Wind Speed",ylab="Wind Speed (mph)",xlab="Temperature (°F)")
nrow(MarsWeather.df)
MarsWeather.df[MarsWeather.df$Wind.Direction=="W",]
Harvest=read.csv(file="Harvest.csv")
Harvest$Avg..Yield.per.100.feet..lb.=as.numeric(Harvest$Avg..Yield.per.100.feet..lb.)
Harvest$Min.Time.Needed.to.Grow..days.=as.numeric(Harvest$Min.Time.Needed.to.Grow..days.)
Harvest$Min.pH=as.numeric(Harvest$Min.pH)
Harvest$Max.pH=as.numeric(Harvest$Max.pH)
Harvest$Calories=as.numeric(Harvest$Calories)
Harvest$X100.g=as.numeric(Harvest$X100.g)
Harvest$Avg..Yield.per.100.feet..lb.=as.numeric(Harvest$Avg..Yield.per.100.feet..lb.)
summary(Harvest)
HarvestM=na.omit(Harvest)
nrow(Harvest)
nrow(HarvestM)
Harvest$`Calories per lb.New`=(Harvest$Calories/Harvest$X100.g)*453.59
#plot(Harvest$Avg..Yield.per.100.feet..lb.~log(Harvest$`Calories per lb.New`),main="Average Yield vs Calories Per Pound",xlab="log(Calories Per Pound)",ylab="Pounds Produced per 100 Feet")
#plot(log(Harvest$Min.Time.Needed.to.Grow..days.)~log(Harvest$`Calories per lb.New`),main="Grow Time vs Calories Per Pound",xlab="Calories Per Pound",ylab="log(Min. Amount of Time Needed to Grow (days))")
#plot(Harvest$Max.pH~log(Harvest$`Calories per lb.New`),main="Max pH vs Calories Per Pound",xlab="log(Calories Per Pound)",ylab="pH")
#plot(Harvest$Max.pH~log(Harvest$Avg..Yield.per.100.feet..lb.),main="Max pH vs Average Yield",xlab="log(average yield in pounds)",ylab="pH")
#plot(log(Harvest$Avg..Yield.per.100.feet..lb.)~log(Harvest$Min.Time.Needed.to.Grow..days.),main="Grow Times vs Average Yield",xlab="log(Grow Times in days)",ylab="log(Average Yield in Pounds)")
rownames(Harvest)=Harvest[,1]
Harvest.Tree=Harvest[,-8]
Harvest.Tree=Harvest.Tree[,-9]
Harvest.Tree=Harvest.Tree[,-4]
Harvest.Tree=Harvest.Tree[,-4]
Harvest.Tree=Harvest.Tree[,-4]
Harvest.Tree=Harvest.Tree[,-2]
Harvest.Tree=Harvest.Tree[,-1]
Harvest.Tree=Harvest.Tree[,-8]
Harvest.Tree=Harvest.Tree[,-4]
Harvest.TreeF=na.omit(Harvest.Tree)
efficiency=function(n){
  eff=c(isTRUE(Harvest.Tree[n,2]<59),
        isTRUE(Harvest.Tree[n,1]>50),
        isTRUE(Harvest.Tree[n,7]>289),
        isTRUE(Harvest.Tree[n,5]==8))
  sum(eff, na.rm = TRUE)
}
#This function counts the number of the inequalities above that are true for each plant in Harvest.Tree and then a for loop adds the number to a column named Efficiency. Inequalities were determined based on quartile value in the summary of Harvest.Tree. Since the pH of Martian soil is about 8.3, ideally we want plants that can handle a pH of 8. 
for (row in 1:nrow(Harvest.Tree)){
  Harvest.Tree$Efficiency[row]=efficiency(row)
}
#This function converts wind speed in mph to windspeed in kph.
km.per.hr.from.mile.per.hr=function(wind){
  wind/0.62137119
}
#This function takes 4 arguments in order to give an estimate for the amount of energy a half cylinder shaped greenhouse will need in a day. The wind speed has to be in miles per hour, the temperature must be in fahrenheit, and the length and width must be in feet. The returned result is in kilowatts. This way of calculating energy needed came from https://www.dpi.nsw.gov.au/agriculture/horticulture/greenhouse/structures-and-technology/heating#:~:text=Wind%20increases%20the%20removal%20of%20heat%20from%20the,it%20is%20a%20good%20idea%20to%20include%20it.
GHenergy=function(length,width,aveTemp,wind){
  vol=(0.5*3.14159*(0.5*width)^2)*length
  #calculating surface area and volume of the greenhouse
  SA=(0.5*width*3.14159*length)+(3.14159*(0.5*width)^2)
  #converting to mph to kph
  windy=km.per.hr.from.mile.per.hr(wind)
  #taking into account the wind shear
  if(windy<=25){
    wind.shear=1.0
  }
  if(windy>25){
    wind.shear=1.025
  }
  if(windy>30){
    wind.shear=1.050
  }
  if(windy>35){
    wind.shear=1.075
  }
  #calculating the temperature difference between inside and outside of the greenhouse
  temp=abs(aveTemp-65)
  #These two constants are the smallest constants possible for materials we currently use to build greenhouse. I am assuming here that air leakage         and thermal properties for the materials used to build greenhouses on Mars will the same or better than the best materials currently used on            Earth. 
  c1=0.5
  c2=0.7
  #calculating heat loss by conduction
  Qc=(SA*temp*c1*wind.shear)/1000
  #calculating heat loss by air leakage
  Ql= 0.373*temp *vol*c2*wind.shear
  #used for converting to kilowatts
  conversion.factor=0.293
  #total heat loss
  BTU=Qc+Ql
  #converting to kilowatts
  kWatts=(BTU*conversion.factor)/1000
  return(as.numeric(kWatts))
}
#gives the time, number of green houses, and energy needed to produce enough food to feed a certain number and age group of people for one year growing chosen plants
#the arguments are: plants=a vector containing the plants you want to grow, nmale and nfemale=the number of male and female Martians you are feeding, ave_age= the average age of the Marians (After 17 years old, the ages become a range. These ranges can be seen in the row names of mCal or fCal),temp=average minimum temperature in fahrenheit, and wind=average wind speed in mph
#this function minimizes time
tse_maxt=function(plants,nmale,nfemale,ave_age,temp,wind){
  #determines how many calories will be needed for the people on Mars
  CalNeeded=(mCal[ave_age,3]*nmale)+(fCal[ave_age,3]*nfemale)
  #Calories needed from plants for one year assuming half of calories consumed are going to come from plants 
  CalYear=CalNeeded*365*.5
  #split the calories needed by number of plants being grown to determine the number of calories that need to be produced by each plant
  CalperPlant=CalYear/length(plants)
  #this for loop estimates the time needed to grow, the number of greenhouses needed, and the amount of energy needed if you wanted to produce all of     the needed calories in one harvest
  y=data.frame()
  for(value in plants){
    lbs=CalperPlant/Harvest.Tree[value,7]
    days=Harvest.Tree[value,2]
    space=(lbs/Harvest.Tree[value,1])*100
    lengthT=100
    #100ft will be the max row length before a new row is started and the max diameter of each green house is 10 ft
    widthT=(space/100)/(Harvest[value,4]/12)
    width=10
    numOfGreenH=widthT/10
    energy=(numOfGreenH*GHenergy(lengthT,width,temp,wind)*days)
    tmp <-round(c(days,numOfGreenH,energy),digits=2)
    y <- rbind.data.frame(y, tmp)
  }
  kable(y,col.names = c("# of Days Needed to Grow","# of Greenhouses Needed","Kilowatts of Energy Needed"))
}
#this function minimizes space
List=list()
tse_maxs=function(plants,nmale,nfemale,ave_age,temp,wind){
  CalNeeded=(mCal[ave_age,3]*nmale)+(fCal[ave_age,3]*nfemale)
  CalYear=CalNeeded*365*.5
  #Assuming half of calories consumed are going to come from plants
  CalperPlant=CalYear/length(plants)
  #this for loop estimates the number of greenhouses needed and the amount of energy needed if you wanted to produce the calories needed in a year's       time
  y=data.frame()
  for(value in plants){
    harvest=floor(365/Harvest.Tree[value,2])
    lbs=(CalperPlant/Harvest.Tree[value,7])
    space=((lbs/Harvest.Tree[value,1])*100)/harvest
    lengthT=100
    #100ft will be the max row length before a new row is started and the max diameter of each green house is 10 ft
    widthT=(space/100)/(Harvest[value,4]/12)
    width=10
    numOfGreenH=widthT/10
    energy=(numOfGreenH*GHenergy(lengthT,width,temp,wind)*365)
    tmp <- ceiling(c(numOfGreenH,energy))
    y <- rbind.data.frame(y, tmp)
  }
  kable(y,col.names = c("# of Greenhouses Needed","Kilowatts of Energy Needed"))
}