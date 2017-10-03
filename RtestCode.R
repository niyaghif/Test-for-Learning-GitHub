install.packages("knitr")
library(rmarkdown)
options(tibble.width=Inf)

#####invesigating NA's in Text
options(tibble.width=Inf)
maindf$MsgID<-seq.int(nrow(maindf))
maindf[which(is.na(maindf$Text)),c(5,6,13)]
##Number of NA texts in total
table(is.na(maindf$Text))

##Number of NA texts in outgoing messages
OutgoingMsg<-maindf[maindf$Direction=="Outgoing",]
table(is.na(OutgoingMsg$Text))

##Number of NA texts in incoming messages
IncomingMsg<-maindf[maindf$Direction=="Incoming",]
table(is.na(IncomingMsg$Text))

#####Removing non-eng characters
df$MsgID<-seq.int(nrow(df)) #Indexing Messages
tmp <- strsplit(df$Text, split = " ")
tmp2<-data.frame(MsgID = rep(df$MsgID, sapply(tmp, length)), Words = unlist(tmp))
tmp3 <- grep("NonEngCatch", iconv(tmp2$Words, "latin1", "ASCII", sub="NonEngCatch")) #subseting messages with non-English characters
df<-df[-tmp2[tmp3,]$MsgID,] #Removing non_English messages from users

#####Finding the number of words in messages
df$MsgID<-seq.int(nrow(df))
tmp<-df%>%unnest_tokens(word,Text)
TextLength<-sqldf("SELECT MsgID,count(*) FROM tmp GROUP BY MsgID")
df$TextLength<-TextLength$`count(*)`
setdiff(df$MsgID, TextLength$MsgID)
dim(df)[1]-dim(TextLength)[1]
length(setdiff(df$MsgID, TextLength$MsgID))
df$Text[setdiff(df$MsgID, TextLength$MsgID)]
kable(t(summary(df$TextL)))

###Find human requests
test<-grep("Please say 'QUIT' to return to the bot experience.",df$Text)
grep('\\b(?i)Quit\\b',as.vector(df$Text[test-1]),value = TRUE)

grep('(?i)quit',df[test-1,"Text"][1:5,1])
table(df[test-1,"Text"]=="quit")
head(test)
table(df[test,c("Text","Flow")]$Flow)
#Find all human regex with catchall
tmp<-subset(df[test,c("Contact UUID","Text","Flow")],df[test,c("Text","Flow")]$Flow=="catchall")
tmp2<-subset(df,df$`Contact UUID` %in% tmp$`Contact UUID`)
tmp3<-tmp2 %>% group_by('Contact UUID') %>% filter(Flow=="CAO")




head(df$Text)
df[test[1:5],]
123456
###########data cleaning
df$ConvID<-seq.int(nrow(df))
IncomingMsg<-df[df$Direction=="Incoming",]
tmp <- strsplit(df$Text, split = " ")
tmp2<-data.frame(ConvID = rep(IncomingMsg$ConvID, sapply(tmp, length)), Words = unlist(tmp))
tmp3 <- grep("NonEngCatch", iconv(tmp2$Words, "latin1", "ASCII", sub="NonEngCatch"))
head(tmp3)
head(tmp2[tmp3,]$ConvID)
df%>%group_by('Contact UUID')  
length(unique(df$`Contact UUID`))
#################Follow up on this
df$ConvID<-seq.int(nrow(df))
tmp<-df%>%unnest_tokens(word,Text)
TextLength<-sqldf("SELECT ConvID,count(*) FROM tmp GROUP BY ConvID")
df$TextLength<-TextLength$`count(*)`
tmp[which(is.na(tmp$ConvID)),]
df[rowSums(is.na(df))==dim(df)[2], ]
df %>% group_by(ConvID) %>% mutate(dfdf=unlist(strsplit(Text,split="")))






df$ConvID<-seq.int(nrow(df))
tmp<-df%>%unnest_tokens(word,Text)
table(is.na(tmp$word))
TextLength<-sqldf("SELECT ConvID,count(*) FROM tmp GROUP BY ConvID")
df$TextLength<-TextLength$`count(*)`






################
tmp<-df[1:100,]
BottonTexts<-sqldf("SELECT Text FROM df GROUP BY `Contact UUID`")
subset(tmp,tmp$ConvID==NA)
table(is.na(tmp$ConvID))


