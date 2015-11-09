#we are scoring the sentiments based on the no of times apositive or negative 
#word has occured in a sentence 


score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # coverting to list
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms which is saved in the shiny path already 
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}





#this function performs emotion sentiment analysis using Naive Bayes Classification 
#input : text(tweets)
#output : emotionclass 

emotionSentimentAnalysis <- function (inText) {
  
  emotionclass <-  classify_emotion(inText, algorithm="bayes", prior=1.0)
  
  #extract emotion with the best possible fit 
  emotion <-  emotionclass[,7]
  
  #setting emaotion having NA to unkonw 
  emotion[is.na(emotion)] = "unknown"
  
  return (emotion)
}

#This function Clasifiys polarity of the of the tweets bases on naive bayes algorithm 
#Input : text(tweets)
#Output : polarityclass

polaritySentimentAnalysis <- function (inText) {
  #polarity clasification 
  polarityclass <- classify_polarity(inText,algorithm = "bayes")
  
  #polarity best fit for plottin 
  polarity <- polarityclass[,4]
  
  #setiing polarity having NA to unkown 
  polarity[is.na(polarity)] <-"unknown"
  
  return(polarity)
  
}


