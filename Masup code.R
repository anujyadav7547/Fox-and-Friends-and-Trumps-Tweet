install.packages("timeSeries")
library(tm)
library(timeSeries)

setwd("E:/Tutorial/Fox and Friends and Trumps Tweet/Fox and Friends and Trumps Tweet")
rm(list=ls())

TrumpTweet <- read.csv("Trump Tweets.csv")
TrumpTweet$created_at <- as.POSIXct(TrumpTweet$created_at, format='%m-%d-%Y %H:%M:%S', tz="GMT")
TrumpTweet$created_at <- as.POSIXlt(TrumpTweet$created_at, tz="EST")

FnFCleanedTranscript <- read.csv("FnFCleanedTranscript.csv")


FnFCleanedTranscript$Date<- gsub('(February.*[0-9].*2018).*',"\\1",FnFCleanedTranscript$Date )
FnFCleanedTranscript$Date<- as.Date(FnFCleanedTranscript$Date,'%B %d, %Y')
#FnFCleanedTranscript$Date <- as.POSIXct(FnFCleanedTranscript$Date)
FnFCleanedTranscript$Time <- gsub('\t(.*) AM',"\\1",FnFCleanedTranscript$Time)
FnFCleanedTranscript$Time<- paste(FnFCleanedTranscript$Time,":00",sep="")
FnFCleanedTranscript$DateTime <-paste(FnFCleanedTranscript$Date, FnFCleanedTranscript$Time,sep = " ")

FnFCleanedTranscript$DateTime1 <- as.POSIXct(as.character(FnFCleanedTranscript$DateTime))
FnFCleanedTranscript$DateTimeEST <- FnFCleanedTranscript$DateTime1 + 3*60*60

#Merging the Transcript for a Broadcast
## now merging all the records to form a single row datasource
FnFCleanedTranscript$tmpDate<- as.character(round(FnFCleanedTranscript$DateTimeEST, units="hours"))
tmpDate <- unique(FnFCleanedTranscript$tmpDate)

MergedTranscript <- sapply(unique(FnFCleanedTranscript$tmpDate),
                  function(x){
                    trimws(paste(FnFCleanedTranscript[FnFCleanedTranscript$tmpDate==x,"Transcript"] , collapse = "\n" ))
                  })
MergedTranscript <- gsub(pattern = ","," ", MergedTranscript)
MergedBoardCast<- as.data.frame(cbind(tmpDate, MergedTranscript))
rownames(MergedBoardCast) <- 1:nrow(MergedBoardCast)
colnames(MergedBoardCast) <- c('DateTimeEST', 'Transcript')
MergedBoardCast$DateTimeEST <- as.POSIXct(as.character(MergedBoardCast$DateTimeEST), format = "%Y-%m-%d %H:%M:%S", tz = "EST")


# finding the Influence of Tweet from Fox and Friends
Day1<- round(TrumpSortedTweet$created_at[1], units="days")
TrumpTweet$IsInfluenced <-""
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))



###############################################################################################


for (i in 1: nrow(TrumpTweet)){
  Day2<- round(TrumpTweet$created_at[i], units="days")
  if (Day1 != Day2){
    Day1 <- Day2
    doc1<- as.character(TrumpTweet$text[i])
    doc2 <- trimws(paste(
                    FnFCleanedTranscript[FnFCleanedTranscript$Date==as.Date(Day1),"Transcript"] ,
                    collapse = " " ))
    doc1<- Corpus(VectorSource(doc1))
    doc2<- Corpus(VectorSource(doc2))
    doc1 <- tm_map(doc1, toSpace, "/")
    doc1 <- tm_map(doc1, toSpace, "@")
    doc1 <- tm_map(doc1, toSpace, "\\|")
    
    doc2 <- tm_map(doc2, toSpace, "/")
    doc2 <- tm_map(doc2, toSpace, "@")
    doc2 <- tm_map(doc2, toSpace, "\\|")
    
    # Convert the text to lower case
    doc1 <- tm_map(doc1, content_transformer(tolower))
    doc2 <- tm_map(doc2, content_transformer(tolower))
    # Remove numbers
    doc1 <- tm_map(doc1, removeNumbers)
    doc2 <- tm_map(doc2, removeNumbers)
    # Remove english common stopwords
    doc1 <- tm_map(doc1, removeWords, stopwords("english"))
    doc2 <- tm_map(doc2, removeWords, stopwords("english"))
    # Remove punctuations
    doc1 <- tm_map(doc1, removePunctuation)
    doc2 <- tm_map(doc2, removePunctuation)
    # Eliminate extra white spaces
    doc1 <- tm_map(doc1, stripWhitespace)
    doc2 <- tm_map(doc2, stripWhitespace)
    # Text stemming
    doc1 <- tm_map(doc1, stemDocument)
    doc2 <- tm_map(doc2, stemDocument)
    doc1 <- as.character(doc1[[1]])
    doc2 <- as.character(doc2[[1]])
    d1 <- unlist(strsplit(doc1, " "))
    d2 <- unlist(strsplit(doc2, " "))
    if(length(unique(d1[d1 %in% d2]))/length(unique(d1)) > .85)
    {
      print("TRUE")
      TrumpTweet$IsInfluenced[i] <- "Yes"
    }else{
      TrumpTweet$IsInfluenced[i] <- ""
    }
    
  }
  
}
###################################################################################################


# Sorting both the files
FnFCleanedSortedTranscript<-MergedBoardCast[order(MergedBoardCast$DateTimeEST),]
TrumpSortedTweet <- TrumpTweet[order(TrumpTweet$created_at),]


# Merging the files

rm(MergedFile)
rm(ts)
ts <- timeSequence(from = as.POSIXct("2018-02-01 00:00:00",tz='EST'), to = as.POSIXct("2018-02-10 00:00:00",tz='EST'), by = "min")
ts<- as.data.frame(ts)

colnames(ts) <- "TimeSeries"
ts$TimeSeries <- as.POSIXlt(ts$TimeSeries, tz="EST")

ts$BroadcastTime <- ""
ts$Transcript<- ""
ts$TweetTime <- ""
ts$Tweet<- ""
ts$RTCount <- ""
ts$IsRetweet <- ""
ts$IsInfluenced <- ""

for (i in 1:nrow(FnFCleanedSortedTranscript)){
  RowNum <- which(ts$TimeSeries == FnFCleanedSortedTranscript$DateTimeEST[i], arr.ind = TRUE)
  ts$BroadcastTime[RowNum] <- as.character(FnFCleanedSortedTranscript$DateTimeEST[i])
  ts$Transcript[RowNum] <- as.character(FnFCleanedSortedTranscript$Transcript[i])
}

for (i in 1:nrow(TrumpSortedTweet)){
  tmpDate<- round(TrumpSortedTweet$created_at[i], units="mins")
  RowNum <- which(ts$TimeSeries == tmpDate, arr.ind = TRUE)
  ts$TweetTime[RowNum] <- as.character(TrumpSortedTweet$created_at[i])
  ts$Tweet[RowNum] <- as.character(TrumpSortedTweet$text[i])
  ts$RTCount[RowNum] <- as.character(TrumpSortedTweet$retweet_count[i])
  ts$IsRetweet[RowNum] <- as.character(TrumpSortedTweet$is_retweet[i])
  ts$IsInfluenced[RowNum] <- as.character(TrumpSortedTweet$IsInfluenced[i])
}

write.csv(ts, "TimeLine.csv")

write.csv(MergedBoardCast, "MergedBoardCast.csv")
