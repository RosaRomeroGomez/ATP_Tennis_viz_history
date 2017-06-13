#Install required packages
if (!require(ggplot2)){ 
  install.packages(ggplot2) 
} 

if (!require(plyr)){ 
  install.packages(plyr) 
} 

if (!require(readr)){ 
  install.packages(readr) 
} 

if (!require(dplyr)){ 
  install.packages(readr) 
} 

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

#Save dataframe in just one csv file (output.csv is within /data)

write.csv(df,file="output.csv")


