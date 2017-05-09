library(ggplot2)
library(GGally)
library(plyr)
library(readr)
library(dplyr)
library(ggmap)
library(countrycode)


#Read data
filenames <- list.files(path=getwd()) #List of csv files (just related to matches)

#Transform list of csv files in a list
All <- lapply(filenames,function(i){
  i <- paste(i,sep="")
  read_csv(i)
})
filenames <- gsub("-",".",filenames)
names(All) <- gsub(".csv","",filenames)

#Transform list of csv files on a data frame
df<-ldply(All, rbind,.id=NULL)

#Subset by columns of interest
df_subset<-df[,c('tourney_date','winner_hand', 'winner_ioc', 'loser_hand','loser_ioc')]
#Extract year 
df_subset$tourney_date<-substring(df_subset$tourney_date,1,4)

#Extract frequencies of win/lost games per year per country and per hand type (per year is not the best idea
#because there are too many requests to the google maps API)

#counts_winners<-data.frame ( table ( df_subset$tourney_date, df_subset$winner_hand,df_subset$winner_ioc) )
#colnames(counts_winners) <- c("year","hand_type","country","count")

#WINNERS
counts_winners<-data.frame ( table ( df_subset$winner_hand,df_subset$winner_ioc) )
colnames(counts_winners) <- c("hand_type","country","count")

#Extract latitude and longitude of the countries involved
countries_codes_winners<-counts_winners$country
countries_names_winners<-countrycode(countries_codes_winners, "ioc", "country.name")
#To avoid confusion with us states
countries_names_winners<-paste ("country",countries_names_winners)

#Extract latitude and longitude
locations_winners<-geocode(countries_names_winners)
data.frame(locations_winners)
counts_winners$lat<-locations_winners$lat
counts_winners$lon<-locations_winners$lon


#LOSERS
counts_losers<-data.frame ( table ( df_subset$tourney_date, df_subset$loser_hand,df_subset$loser_ioc) )
colnames(counts_losers) <- c("hand_type","country","count")

#Extract latitude and longitude of the countries involved
countries_codes_losers<-counts_losers$country
countries_names_losers<-countrycode(countries_codes_losers, "ioc", "country.name")
#To avoid confusion with us states
countries_names_losers<-paste ("country",countries_names_losers)

#Extract latitude and longitude
locations_losers<-geocode(countries_names_losers)
data.frame(locations_losers)
counts_losers$lat<-locations_losers$lat
counts_losers$lon<-locations_losers$lon


world_map <- map_data("world")

ditch_the_axes <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)

winners_map<-ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group))+ 
  coord_fixed(1.3)+
  scale_size("# of won matches",range = c(1,15))+
  geom_point(data = counts_winners, alpha=0.5,aes(x = lon, y =lat,size=count, colour=hand_type))+
  theme(axis.ticks=element_blank(), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="right",legend.text=element_text(size = 10),legend.key = element_blank(),legend.title=element_text(size=10))+
  ditch_the_axes

losers_map<-ggplot() + 
  geom_polygon(data = world_map, aes(x=long, y = lat, group = group))+ 
  coord_fixed(1.3)+
  scale_size("# of lost matches",range = c(1,15))+
  geom_point(data = counts_losers, alpha=0.5,aes(x = lon, y =lat,size=count, colour=hand_type))+
  theme(axis.ticks=element_blank(), panel.grid.major=element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(),legend.position="right",legend.text=element_text(size = 10),legend.key = element_blank(),legend.title=element_text(size=10))+
  ditch_the_axes

ggsave(width=15, height=8, dpi=100,limitsize=FALSE,filename="../prototypes/geolocation_winners_hand_type_total.pdf", plot=winners_map)
ggsave(width=15, height=8, dpi=100,limitsize=FALSE,filename="../prototypes/geolocation_losers_hand_type_total.pdf", plot=losers_map)




