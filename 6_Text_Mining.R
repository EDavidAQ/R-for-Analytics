
## Curso : Analytics

################################################################################

# Unidad 4: Análisis de Texto
# Capacitador : Ebson David Allende Quintana
# email: david.allende@outlook.com
# Script Version : 1.0

################################################################################

##### Installing libraries required --

if (!require("pacman")) install.packages("pacman")
pacman::p_load(twitteR, sentiment, plyr, ggplot2, wordcloud, RColorBrewer, 
               httpuv, RCurl, base64enc)

options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", 
                                                 package = "RCurl")))

install.packages("NLP")
install.packages("tm")
install.packages("twitteR")

##### Configuration of Twitter app  --

api_key<- "GGtlIfbhhfBlE8N878Ne044hi"
api_secret<- "pgsz58XvPADpFon3IqOVmHf0ceyXwrnSoSb4zcyvYAfCC4FSz8"
access_token<- "245575636-D4MofjiBXL1laessc63FjBE4t41dkB23TJ23iyvV"
access_token_secret<- "N81AMbszc2KUTGC8Q3joQS9bhcOTCmh266WihP914Bjhx"

library(Rstem)
library(sentiment)
library(tm)
library(NLP)
library(twitteR)

api_key<-"4uVbVieGCsNixaVwF7wBo7BB2"
api_secret<-"PPf628myPv4lWhFpYJaUh9HwQ34RyGuRu1oILJuaAqradVk6s0"
access_token<-"777727640388055040-kFTBgkDlRAGuCii7Z152EWdP38nhxHE"
access_token_secret<-"SIqtoAL6Fi91Hh3Er8hwfMRZP9SFrTi3l8vDCDPHwlV3A"

##### Establish configuration  --

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##### Extraction from Twitter API --

some_tweets<-searchTwitter("TripAdvisorES",n=1000,since='2016-10-20', 
                            until='2016-10-30')  # TripAdvisor mas de 1000

##getwd()
##setwd("D:/Cursos/14.Promperu/2_Data")
##write.csv(df,"some_tweets.csv")

##some_tweets[c(1:5)]

#transforms the information of the data frame tweets

library(tm)
df <- twListToDF(some_tweets)
head(df)

# tweets selecting user and non-user

no_user_tweets<-df[df$screenName!="TripAdvisorES",]
nrow(no_user_tweets)

user_tweets<-df[df$screenName=="TripAdvisorES",]
nrow(user_tweets)

# gets text in tweets
some_txt <- no_user_tweets$text

# remove retweet entities
some_txt <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt <- gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt <- gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt <- gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt <- gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt <- gsub("[ \t]{2,}", "", some_txt)
some_txt <- gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function
try.error <- function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply
head(some_txt)
some_txt<- sapply(some_txt, try.error)
head(some_txt)

# builds a corpus
library(tm)
corpus<- Corpus(VectorSource(some_txt))

# converted to lowercase
corpus<- tm_map(corpus, tolower)

# removes empty words (stopwords) in Spanish
corpus<- tm_map(corpus, removeWords, c(stopwords("spanish"), "...."))

# Custom file loads empty words and converts it to ASCII
sw<- readLines(file.choose(""),encoding="UTF-8")
sw<- iconv(sw, to="ASCII//TRANSLIT")

# Personalized removes empty words
corpus <- tm_map(corpus, removeWords, sw)

# remove extra white space
corpus <- tm_map(corpus, stripWhitespace)
corpus_clean<- tm_map(corpus, PlainTextDocument)

# creates an array of terms
tdm<- TermDocumentMatrix(corpus_clean)

# becomes a matrix
m <- as.matrix(tdm)

# word count in decreasing order
wf<- sort(rowSums(m),decreasing=TRUE)

# creates a data frame with the words and their frequencies
dm<- data.frame(word = names(wf), freq=wf)
#fix(dm)#### si se desea cambiar algo 

# graphic word cloud (wordcloud)
wordcloud(dm$word, dm$freq, random.order=FALSE, min.freq=7,
          colors=brewer.pal(8, "Dark2"))
