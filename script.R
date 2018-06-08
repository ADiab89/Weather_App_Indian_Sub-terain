## Set the working directory



## Import the necessary modules

library(leaflet)
library(owmr)
library(ggplot2)
library(gganimate)
library(gganimate)
library(ggplot2)
library(yarrr)
## Set the API key with Open Weather Map

API_Key<-owmr_settings("0183f77e04642b59be717ba83a840141")

## Get the longitude and latitude for Indian cities

India<-owm_cities[owm_cities$countryCode=="IN",]
India_unique<-India[!duplicated(India[,2]),]
India_unique<-India_unique[,c(2,3,4)]
nrow(India_unique)

## List of my favourite cities

favourite_cities<-c("Leh","Bikaner","Jaisalmer","Ajmer",
                    "Manali","Darjiling","Gangtok","Varanasi","Kolkata",
                    "Guwahati","Bangalore","Chennai","Mumbai","Pune",
                    "Mirik","Rishikesh","Kargil")




get_city<-function(i){
  x<-India_unique[India_unique$nm == favourite_cities[i],]
  return(x)
}


India_Fav<-lapply(seq(1:17),get_city)
India_Fav_df<-as.data.frame(matrix(unlist(India_Fav), nrow=17, byrow=TRUE),stringsAsFactors = FALSE)

## Renames the columns and concvert the lat/lon to numerics
colnames(India_Fav_df)<-c("City","lat","lng")

India_Fav_df$lat<-as.numeric(India_Fav_df$lat)
India_Fav_df$lng<-as.numeric(India_Fav_df$lng)



## India North Eastern part

##India_NE<-India[India$lon>=87.00 & India$lon<=96.16,]
##India_NE<-India_NE[India_NE$lat>=20.00 & India_NE$lat<=28.00,]


## Plot with the map

##India_Fav_map<-leaflet()%>%addProviderTiles(providers$Stamen.TerrainBackground)%>%addCircleMarkers(data=India_Fav_df,lat=~lat,lng=~lng,
                                                 ##  popup = ~paste0("<br>{{name}}</b></br>")%$$%India_Weather)

## Get the weather data for the desired cities

get_weather_data<-function(i){
  weather_df<-get_current(
    India_Fav_df[i,1],
    units="metric")%>%flatten()
return(weather_df[c("weather.icon","coord.lat","coord.lon","weather.main",
                    "name","main.pressure","main.humidity","main.temp","wind.speed")])%>%data.frame()
  
}

## Get the colnames for the dataframe

Names<-read.csv("names.csv",stringsAsFactors = FALSE)

## Convert the factor variable into character class


India_Weather<-lapply(seq(1:17),get_weather_data)
for(i in 1:17){
India_Weather[i][[1]]$weather.icon=as.character(India_Weather[i][[1]]$weather.icon)}

for(i in 1:17){
India_Weather[i][[1]]$weather.main=as.character(India_Weather[i][[1]]$weather.main)}

for(i in 1:17){
  India_Weather[i][[1]]$name=as.character(India_Weather[i][[1]]$name)}


India_Weather_df<-as.data.frame(matrix(unlist(India_Weather), nrow=17, byrow=TRUE),stringsAsFactors = FALSE)
colnames(India_Weather_df)<-Names$x

## COnvert the latitude and longitude into numerics

India_Weather_df$coord_lat<-as.numeric(India_Weather_df$coord_lat)
India_Weather_df$coord_lon<-as.numeric(India_Weather_df$coord_lon)
India_Weather_df$main_temp<-as.numeric(India_Weather_df$main_temp)
India_Weather_df$main_humidity<-as.numeric(India_Weather_df$main_humidity)



## Add Weather to the above map

## Icon

##popup_tpl <- paste0(
  ##"<b>{{name}}</b></br>",
  ##"{{coord_lon}}<, {{coord_lat}}</br>",
  ##"{{main_temp}}?C, ",
##"{{main_humidity}}, ",
##"<b>{{weather_main}}</b>")

##popup_tpl%$$%India_Weather_df

##Map<-leaflet()%>%addProviderTiles(providers$Stamen.TerrainBackground)

#India_WeatherMap<-Map%>%add_weather(India_Weather_df,icon = India_Weather_df$weather_icon,template = popup_tpl)

############

### Five Years temperature range for Summer, Monsoon and Winter

##read the dataset




## Get the data

get_data<-function(i){
  location_df<-read.csv(as.character(i),stringsAsFactors = FALSE)
  clean_df<-location_df[,c(-1,-2,-5,-7,-9,-10,-11,-13,-15,-16,-17,-22,-23)]

  
  
  ## User defined fucntion to convert farenheit to celcius
  
  celcius<-function(x){
    c=(x-32)/1.8
    return(c)
  }
  
  
  
  ## Replace the "*" by gsub
  
  clean_df$MAX<-gsub("[*].*$","",clean_df$MAX)
  clean_df$MIN<-gsub("[*].*$","",clean_df$MIN)
  
  ## Convert the temperature to Celcius
  
  clean_df$TEMP<-celcius(clean_df$TEMP)
  clean_df$DEWP<-celcius(clean_df$DEWP)
  clean_df$MAX<-celcius(as.numeric(clean_df$MAX))
  clean_df$MIN<-celcius(as.numeric(clean_df$MIN))
  
  
  ## Convert the dates into character in order to split the coloumn into dd mm yy columns
  clean_df$YEARMODA<-as.Date(as.character(clean_df$YEARMODA),format="%Y%m%d")
  
  list<-strsplit(as.character(clean_df$YEARMODA),"-")
  ## Convert the list intok dataframe
  library(plyr)
  Date<-ldply(list)
  colnames(Date)<-c("Year","Month","Day")
  
  ## Column bind with the main dataframe
  clean_df<-cbind(clean_df,Date)
  ## Change the Date to numeric
  clean_df$Year=as.numeric(clean_df$Year)
  
  
  ## Return the final Dataset
  return(clean_df)
  }


###############################################

## check any of the datset


#p2<-ggplot(Kolkata, aes(x=Month, y=TEMP, color=factor(Month))) +
  #geom_boxplot()+
  #geom_boxplot(aes(frame = factor(Year)))+
  #xlab("Months")+
  #ylab("Average Temperature")+
  #ggtitle("Average Temperature for last 4 Years")+
  #theme(panel.border = element_blank(), 
   #     panel.grid.major = element_blank(),
    #    panel.grid.minor = element_blank(),
    #    plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
     #   axis.line = element_line(colour = "black"))+
  #theme(legend.position="none")



#gganimate(p2)

####################

## Rainfall (1990-2015)

#rainfall<-read.csv("rainfall.csv",stringsAsFactors = FALSE)

## Divide the dataset into pre and post 1950

#pre_1950<-rainfall[rainfall$YEAR<=1950,]

#post_1950<-rainfall[rainfall$YEAR>1950,]


#district<-group_by(pre_1950,Division)



#pre_1950_modified<-summarize(district,count=n(),Winter_Rainfall=mean(Jan.Feb,na.rm=TRUE),
 #         Summer_Rainfall=mean(Mar.May,na.rm=TRUE),
  #        Monsoon_Rainfall=mean(Jun.Sep,na.rm=TRUE),
   #       Fall_Rainfall=mean(Oct.Dec,na.rm=TRUE))
#
## Add new column of Yearly average
#pre_1950_modified<-mutate(pre_1950_modified,Yearly_Avg=(Winter_Rainfall+Summer_Rainfall+Monsoon_Rainfall+Fall_Rainfall)/4)
#pre_1950_sorted<-pre_1950_modified[order(-pre_1950_modified$Yearly_Avg),]

## pirateplot




#pirateplot(formula = Jan.Feb ~ Division, 
 #          data = pre_1950[pre_1950$Division == c("KERALA","COASTAL KARNATAKA",
  #                                                "ARUNACHAL PRADESH","ANDAMAN & NICOBAR", "KONKAN & GOA"),],
   #                    main = "Pirateplot for top 4  Indian sub terrain pre 1950s",
    #       pal="pony",
     #      theme=1
      #      )


##############################################

## Surface Temperature for Heat map and gganimate

## Get the co-ordiantes for the country as whole and plot it on ggmap

#Location_center = as.numeric(geocode("India"))
#LocationMap = ggmap(get_googlemap(center=Location_center, scale=1, zoom=4), extent="device")





