#install.packages("RSocrata")
#install.packages("pastecs")
library(pastecs)
library(RSocrata)
library(lubridate)
library(data.table)
library(dplyr)

#download data via API
evictions<-read.socrata("https://data.sfgov.org/resource/93gi-sfd2.json")


#evictions general since 2010
evictions10plus<-evictions[year(evictions$file_date)>2009,]



#logical columns to be converted to numbers
k<-c(1,3, 4,8,9,10,11,13,15,16,17,18,20,21,22,23,24,26,28)

for (i in k){
  print(evictions10plus[,i]<-as.integer(as.logical(evictions10plus[,i])))
  }

names_cols<-colnames(evictions10plus)

for (m in k){
print(evictions10plus %>% 
  summarise(names_cols[m] = sum(evictions10plus[,m])))
}
            
            
for(m in k){
  print( sum(evictions10plus[,m]))
}


#ellis acts
movein<-evictions10plus[which(evictions10plus$owner_move_in==1),]
for ( y in 2010:2016){
  print(paste("There were",nrow(movein[year(movein$file_date)==y,]), "ellis act convictions in", y))
}

#create new data frame with owner move in evictions and date
ellis<-data.frame(movein$owner_move_in, movein$address, 
                       movein$neighborhood, 
                       movein$file_date)





library(zoo)


ellis$MonthYear <- as.Date(as.yearmon(ellis$movein.file_date))
ellis$Year <- as.numeric(format(ellis$movein.file_date,'%Y'))
#meetupMembers$quarterYear <- as.Date(as.yearqtr(meetupMembers$joinDate))
ellis$MonthN <- as.numeric(format(as.Date(ellis$Month),"%m")) # Month's number
ellis$Month<-month.abb[ellis$MonthN]
ellis$Month  <- months(as.Date(ellis$Month), abbreviate=TRUE) # Month's abbr.


ellis_stats<-ellis %>% group_by(MonthYear) %>% summarise(n = n())


#plotting a graph



require(ggplot2)

jpeg('SFevitcitons.jpg')

p<-ggplot() + 
  geom_line(data=ellis_stats, aes(x=MonthYear, y=n), color='red') + 
  labs(list(title = "Owner Move-in Evictions in San Francisco 2010-2016", x = "Years", y = "Number of Evictions", caption = "(Data from SF OpenData)"))
    
  p
  
  dev.off()
  