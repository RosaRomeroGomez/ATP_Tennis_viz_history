library(ggplot2)
library(GGally)
library(plyr)
library(readr)
library(dplyr)
library(grid)
library(gridExtra)


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
df_subset<-df[,c('tourney_date','winner_hand','winner_age','loser_hand',"loser_age")]
#Extract year 
df_subset$tourney_date<-substring(df_subset$tourney_date,1,4)

boxplot_winners<-ggplot(df_subset, aes(x=df_subset$tourney_date, y=df_subset$winner_age,color=df_subset$winner_hand)) +
 geom_boxplot()+
  xlab("Date")+
  ylab("Players' age")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

boxplot_losers<-ggplot(df_subset, aes(x=df_subset$tourney_date, y=df_subset$loser_age,color=df_subset$winner_hand)) +
  geom_boxplot()+
  xlab("Date")+
  ylab("Players' age")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(width=15, height=8, dpi=100,limitsize=FALSE,filename="../prototypes/aging_over_time_winners_per_hand_type.pdf", plot=boxplot_winners)
ggsave(width=15, height=8, dpi=100,limitsize=FALSE,filename="../prototypes/aging_over_time_losers_per_hand_type.pdf", plot=boxplot_losers)






