library(shiny)
library(shinythemes)
library(leaflet)
library(owmr)
library(ggplot2)
library(gganimate)
library(plyr)
library(ggthemes)
library(dplyr)
library(yarrr)
library(ggmap)


## Set the API key with Open Weather Map

API_Key<-owmr_settings("0183f77e04642b59be717ba83a840141")


server<-shinyServer(function(input,output){
  
  output$map<-renderLeaflet({
    
    
    ## Get the longitude and latitude for Indian cities
    
    India<-owm_cities[owm_cities$countryCode=="IN",]
    India_unique<-India[!duplicated(India[,2]),]
    India_unique<-India_unique[,c(2,3,4)]
    
    
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
    
    popup_tpl <- paste0(
      "<b>{{name}}</b></br>",
      "{{coord_lon}}<, {{coord_lat}}</br>",
      "{{main_temp}}°C, ",
      "{{main_humidity}}, ",
      "<b>{{weather_main}}</b>")
    
    Map<-leaflet()%>%addProviderTiles(providers$Stamen.TerrainBackground)
    
    India_WeatherMap<-Map%>%add_weather(India_Weather_df,icon = India_Weather_df$weather_icon,template = popup_tpl)
    India_WeatherMap
    
  })
  
  output$plot1<-renderImage({
    
    ## Load the data for each  location and clean
    outfile <- tempfile(fileext='.gif')
    
    get_data<-function(i){
      location_df<-read.csv(i,stringsAsFactors = FALSE)
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
    
    ## Get the clean data
  
    Station<-get_data(as.character(paste(input$station,".csv",sep="")))
    
      
    
    ## gganimate
    p2<-ggplot(Station, aes(x=Month, y=TEMP, color=factor(Month))) +
      geom_boxplot()+
      geom_boxplot(aes(frame = factor(Year)))+
      xlab("Months")+
      ylab("Average Temperature")+
      ggtitle("Average Temperature for last 4 Years")+
      theme(panel.border = element_blank(),
            panel.background = element_rect(fill=as.character(paste(input$color))),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
            axis.line = element_line(colour = "black"))+
      theme(legend.position="none")
    
    
    
    gganimate(p2,"outfile.gif")
    
    
    # Return a list containing the filename
    list(src = "outfile.gif",
         contentType = 'image/gif',
          width = 500,
          height = 450
         # alt = "This is alternate text"
    )}, 
    
    
    deleteFile = TRUE)
  
  
  output$yarrplot1<-renderPlot({
    
    rainfall<-read.csv("rainfall.csv",stringsAsFactors = FALSE)
    colnames(rainfall)<-c("Division","YEAR","Winter","Summer","Monsoon","Fall")
    ## Divide the dataset into pre and post 1950
    
    data<-if(input$Rainfall=="pre 1950"){
      rainfall[rainfall$YEAR<=1950,]
    }
    else{rainfall[rainfall$YEAR>1950,]}
    
    
    district<-group_by(data,Division)
    
    
    
    data_modified<-summarize(district,count=n(),Winter_Rainfall=mean(Winter,na.rm=TRUE),
                                 Summer_Rainfall=mean(Summer,na.rm=TRUE),
                                 Monsoon_Rainfall=mean(Monsoon,na.rm=TRUE),
                                 Fall_Rainfall=mean(Fall,na.rm=TRUE))
    
    ## Add new column of Yearly average
    data_modified<-mutate(data_modified,Yearly_Avg=(Winter_Rainfall+Summer_Rainfall+Monsoon_Rainfall+Fall_Rainfall)/4)
    data_sorted<-data_modified[order(-data_modified$Yearly_Avg),]
    data_sorted<-data_sorted[1:5,]
    
    list<-as.list(data_sorted)
    target<-c(list$Division[1],list$Division[2],
              list$Division[3],list$Division[4],
              list$Division[5])
    final_data=filter(rainfall,Division %in% target)
    
    ## pirateplot
    
    
    pirateplot(formula = as.formula(paste(input$Season, "~ Division")), 
               xlab="Indian Sub Terrain (Top 4)",
               ylab = "Rainfall in mm",
               data = final_data,
               main = "Pirateplot for top 4  Indian sub terrain pre 1950s",
               pal="pony",
               theme=1
               )
    
    
  })
  
  
  output$plot2<-renderImage({
    
    source("script.R")
    ## Create temporary gif file
    outfile1 <- tempfile(fileext='.gif')
    
## Load the saved India Map
    
     saveRDS(LocationMap, file = "Map")
     x <- readRDS("Map")
     ggmap(x)
  
    ## Get the data loaded
    
    
    kol_data<-get_data("Kolkata.csv")
    Leh_data<-get_data("Leh.csv")
    Bikaner_data<-get_data("Bikaner.csv")
    Dehradun_data<-get_data("Dehradun.csv")
    Gangtok_data<-get_data("Gangtok.csv")
    Bangalore_data<-get_data("Bangalore.csv")
    
    # Get the latitude and longitude for the location and merge it with the dataset
    
    kol<-mutate(kol_data,City="Kolkata",lat=India_Fav_df[9,2],lon=India_Fav_df[9,3])
    Leh<-mutate(Leh_data,City="Leh",lat=India_Fav_df[1,2],lon=India_Fav_df[1,3])
    Bikaner<-mutate(Bikaner_data,City="Bikaner",lat=India_Fav_df[2,2],lon=India_Fav_df[2,3])
    Dehradun<-mutate(Dehradun_data,City="Dehradun",lat=30.31649,lon=78.03129)
    Gangtok<-mutate(Gangtok_data,City="Gangtok",lat=India_Fav_df[7,2],lon=India_Fav_df[7,3])
    Bangalore<-mutate(Bangalore_data,City="Bangalore",lat=India_Fav_df[11,2],lon=India_Fav_df[11,3])
    
    ## Row bind all the cities together
    
    Master_data<-rbind(kol,Leh,Bikaner,Dehradun,Gangtok,Bangalore)
    
    ## Get mean temperature
    
    Mean_Temp_India<-mean(Master_data$TEMP)
    
    ## Get the regions with temperature higher than average temperature
    
    Region_AboveMean<-Master_data[Master_data$TEMP>=Mean_Temp_India,]
    
    ## Get the Region with temperature lower than average temperature
    
    Region_BelowMean<-Master_data[Master_data$TEMP<Mean_Temp_India,]
    
    ## Plot it on the map with the help of ggplot
    
    
    p3 <- ggmap(x) + geom_point(data = if(input$Temperature=="Above Mean Temp of 4 years"){Region_AboveMean}else{Region_BelowMean}, 
                                  aes(x = lon, y = lat,size=30,color=City, 
                                      frame = Year))+
      
      ggtitle("Average Temperature for last 4 Years")+
      theme(panel.border = element_blank(),
            panel.background = element_rect(fill=as.character(paste(input$color))),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5,size=18,colour="indianred4"),
            axis.line = element_line(colour = "black"))+
      theme(legend.position="none")
    
  
    gganimate(p3,"outfile1.gif")
    
    
    # Return a list containing the filename
    list(src = "outfile1.gif",
         contentType = 'image/gif',
         width = 500,
         height = 400
         # alt = "This is alternate text"
    )}, 
    
    
    deleteFile = TRUE)
    
    
    
    
    
    
  
  
})

    
    
    

  
  
