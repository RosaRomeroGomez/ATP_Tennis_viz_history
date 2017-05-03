library(ggplot2)
library(GGally)
library(plyr)
library(readr)


#Read your data
filenames <- list.files(path=getwd()) #List of csv files under that folder

#Transform list of csv files in a list
 All <- lapply(filenames,function(i){
   i <- paste(i,sep="")
   read_csv(i)
 })
 filenames <- gsub("-",".",filenames)
 names(All) <- gsub(".csv","",filenames)
 
#Transform list on a data frame
 result<-ldply(All, rbind,.id=NULL)

 plot<-ggplot(ds, aes(x=ds$winner_age, y=ds$tourney_name, color=ds$winner_hand)) + geom_point(shape=2)
 ggsave(filename="atp_matches_1968.pdf", plot=plot)
 
 
#Export data to csv file
 write.table(result,"output.csv",sep=",",row.names=FALSE)
 
