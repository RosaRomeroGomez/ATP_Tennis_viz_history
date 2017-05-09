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
df_subset<-df[,c('tourney_date','winner_hand','loser_hand')]
#Extract year 
df_subset$tourney_date<-substring(df_subset$tourney_date,1,4)


#Extract counts for number of matches won by hand type per year

counts_winners<-data.frame ( table ( df_subset$tourney_date, df_subset$winner_hand) )
colnames(counts_winners) <- c("year","hand_type","count")
counts_winners$count<-as.numeric(counts_winners$count)

counts_losers<-data.frame ( table ( df_subset$tourney_date, df_subset$loser_hand) )
colnames(counts_losers) <- c("year","hand_type","count")


bar_chart_winners<-ggplot(data=counts_winners, aes(x=counts_winners$year, y=counts_winners$count,fill=counts_winners$hand_type)) +
  geom_bar(stat="identity")+
  #geom_text(data=counts_winners,aes(x=counts_winners$year,y=counts_winners$count,label=counts_winners$count),vjust=0)+
   xlab("Date")+
  ylab("# of matches won per year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


bar_chart_losers<-ggplot(data=counts_losers, aes(x=counts_losers$year, y=counts_losers$count,fill=counts_losers$hand_type)) +
  geom_bar(stat="identity")+
  scale_y_reverse(limits=c(4500,0),breaks=c(0,1000,2000,3000,4000))+
  #geom_text(data=counts_losers,aes(x=counts_losers$year,y=counts_losers$count,label=counts_losers$count),vjust=0)+
  xlab("Date")+
  ylab("# of matches lost per year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


mirror_plot<-grid.arrange(bar_chart_winners, bar_chart_losers, ncol=1)

  
ggsave(width=15, height=8, dpi=100,limitsize=FALSE,filename="../prototypes/number_of_matches_won_per_year_player_hand.pdf", plot=bar_chart_winners)
ggsave(width=15, height=8, dpi=100,limitsize=FALSE,filename="../prototypes/number_of_matches_lost_per_year_player_hand.pdf", plot=bar_chart_losers)
ggsave(width=15, height=8, dpi=100,limitsize=FALSE,filename="../prototypes/mirror_number_of_matches_per_year_player_hand.pdf", plot=mirror_plot)

