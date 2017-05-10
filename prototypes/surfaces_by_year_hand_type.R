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
df_subset<-df[,c('tourney_date','winner_hand','loser_hand','surface')]
#Extract year 
df_subset$tourney_date<-substring(df_subset$tourney_date,1,4)


#Filter data by winner hand
df_subset_winners_left<-subset(df_subset, df_subset$winner_hand == 'L')
df_subset_winners_right<-subset(df_subset, df_subset$winner_hand == 'R')



#Filter data by loser hand
df_subset_losers_left<-subset(df_subset, df_subset$loser_hand == 'L')
df_subset_losers_right<-subset(df_subset, df_subset$loser_hand == 'R')


#Counts winners lefties
counts_winners_left<-data.frame ( table ( df_subset_winners_left$tourney_date, df_subset_winners_left$surface) )
colnames(counts_winners_left) <- c("year","surface","count")
counts_winners_left$count<-as.numeric(counts_winners_left$count)

#Counts winners right hand
counts_winners_right<-data.frame ( table ( df_subset_winners_right$tourney_date, df_subset_winners_right$surface) )
colnames(counts_winners_right) <- c("year","surface","count")
counts_winners_right$count<-as.numeric(counts_winners_right$count)

#Counts losers lefties
counts_losers_left<-data.frame ( table ( df_subset_losers_left$tourney_date, df_subset_losers_left$surface) )
colnames(counts_losers_left) <- c("year","surface","count")
counts_winners_left$count<-as.numeric(counts_losers_left$count)

#Counts losers right hand
counts_losers_right<-data.frame ( table ( df_subset_losers_right$tourney_date, df_subset_losers_right$surface) )
colnames(counts_losers_right) <- c("year","surface","count")
counts_winners_right$count<-as.numeric(counts_losers_right$count)



#Clay:darkorange2
#Grass:forestgreen
#Carpet:firebrick3
#Hard:darkseagreen


bar_chart_winners_left<-ggplot(data=counts_winners_left, aes(x=counts_winners_left$year, y=counts_winners_left$count,fill=counts_winners_left$surface)) +
  geom_bar(stat="identity")+
  ylim(0, 4000)+
  scale_fill_manual(name = "Surface",values = c("Grass" = "#2ecc71", "Clay" = "#e67e22", "Carpet" = "#e74c3c", "Hard" = "#A2DED0")) + 
  #geom_text(data=counts_winners_left,aes(x=counts_winners_left$year,y=counts_winners_left$count,label=counts_winners_left$count),vjust=0)+
  xlab("Date")+
  ylab("# of matches won by lefties per year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bar_chart_winners_right<-ggplot(data=counts_winners_right, aes(x=counts_winners_right$year, y=counts_winners_right$count,fill=counts_winners_right$surface)) +
  geom_bar(stat="identity")+
  ylim(0, 4000)+
  scale_fill_manual(name = "Surface",values = c("Grass" = "#2ecc71", "Clay" = "#e67e22", "Carpet" = "#e74c3c", "Hard" = "#A2DED0")) + 
  #geom_text(data=counts_winners_right,aes(x=counts_winners_right$year,y=counts_winners_right$count,label=counts_winners_right$count),vjust=0)+
  xlab("Date")+
  ylab("# of matches won by right handed per year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




bar_chart_losers_left<-ggplot(data=counts_losers_left, aes(x=counts_losers_left$year, y=counts_losers_left$count,fill=counts_losers_left$surface)) +
  geom_bar(stat="identity")+
  scale_y_reverse(limits=c(4000,0),breaks=c(0,1000,2000,3000,4000))+
  scale_fill_manual(name = "Surface",values = c("Grass" = "#2ecc71", "Clay" = "#e67e22", "Carpet" = "#e74c3c", "Hard" = "#A2DED0")) + 
  #geom_text(data=counts_losers_left,aes(x=counts_losers_left$year,y=counts_losers_left$count,label=counts_losers_left$count),vjust=0)+
  xlab("Date")+
  ylab("# of matches lost by lefties per year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

bar_chart_losers_right<-ggplot(data=counts_losers_right, aes(x=counts_losers_right$year, y=counts_losers_right$count,fill=counts_losers_right$surface)) +
  geom_bar(stat="identity")+
  scale_y_reverse(limits=c(4000,0),breaks=c(0,1000,2000,3000,4000))+
  scale_fill_manual(name = "Surface",values = c("Grass" = "#2ecc71", "Clay" = "#e67e22", "Carpet" = "#e74c3c", "Hard" = "#A2DED0")) + 
  #geom_text(data=counts_losers_right,aes(x=counts_losers_right$year,y=counts_losers_right$count,label=counts_losers_right$count),vjust=0)+
  xlab("Date")+
  ylab("# of matches lost by right handed per year")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

final_plot_winners<-grid.arrange(bar_chart_winners_left, bar_chart_winners_right, ncol=1)
final_plot_losers<-grid.arrange(bar_chart_losers_left, bar_chart_losers_right, ncol=1)
final_plot_combination<-grid.arrange(bar_chart_winners_left,bar_chart_winners_right,bar_chart_losers_left, bar_chart_losers_right, ncol=2)


ggsave(width=15, height=10, dpi=100,limitsize=FALSE,filename="../prototypes/number_of_matches_won_per_year_by_surface.pdf", plot=final_plot_winners)
ggsave(width=15, height=10, dpi=100,limitsize=FALSE,filename="../prototypes/number_of_matches_lost_per_year_by_surface.pdf", plot=final_plot_losers)
ggsave(width=25, height=20, dpi=100,limitsize=FALSE,filename="../prototypes/number_of_matches_win_lost_per_year_by_surface.pdf", plot=final_plot_combination)


