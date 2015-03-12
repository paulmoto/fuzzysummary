require(tm)
require(frbs)
require(SnowballC)
require(NLP)
require(stringr)
require(openNLP)
require(openNLPmodels.en)
require(lsa)
require(igraph)

text<-gsub("\n","\n\n",text)
paranot <-
  Annotator(function(s, a = Annotation()) {
    spans <- blankline_tokenizer(s)
    n <- length(spans)
    ## Need n consecutive ids, starting with the next "free"
    ## one:
    from <- next_id(a$id)
    Annotation(seq(from = from, length.out = n),
               rep.int("paragraph", n),
               spans$start,
               spans$end)
  },
  "A paragraph token annotator based on blankline_tokenizer().")



#text<-gsub("\n"," ",text)
#corpus<-VCorpus(VectorSource(text))
anot<-Maxent_Sent_Token_Annotator(language = "en", probs = FALSE, model = NULL)
wanot <- Maxent_Word_Token_Annotator()
posanot <- Maxent_POS_Tag_Annotator()
ann <- annotate(text,
               list(anot,
                    wanot,
                    posanot,paranot))

anntext<-AnnotatedPlainTextDocument(text,ann)
sent_ann<-annotate(text,anot)

paralength<-vector()
for(i in 1:length(paras(anntext))){
  paralength[i]<-length(paras(anntext)[[i]])  
}

parapos<-vector()
for(i in 1:length(paralength)){
  for(j in 1:paralength[i]){
    parapos<-c(parapos,paralength[i]-(j-1))
  }
}




origsent<-vector()
for(i in 1:length(sent_ann)){
  origsent[i]<-substr(text,sent_ann$start[i],sent_ann$end[i])
}

corpus<-VCorpus(VectorSource(origsent))
corpus<-tm_map(corpus, stripWhitespace)
corpus<-tm_map(corpus,content_transformer(tolower))
corpus<-tm_map(corpus,removeWords,stopwords("english"))
corpus<-tm_map(corpus,stemDocument)

sent_words<-list()
for(i in 1:length(corpus)){
sent_words[[i]]<- MC_tokenizer(corpus[[i]])[!MC_tokenizer(corpus[[i]])==""]
}

dict<-TermDocumentMatrix(VCorpus(VectorSource(sent_words)), control = list())

#Title words Feature
title<-"New Orleans Saints' Will Smith plans to be back as long as the Saints want him"
twords<-stripWhitespace(title)
twords<-tolower(unlist(strsplit(title," ")))
twords<-twords[!twords %in% stopwords('english')]
twords<-stemDocument(twords)


#wordcount counts number of times x appears in y
wordcount<- function(x,y){
  return(length(grep(sprintf("^%s$",x),y)))
  }


#frequency of words in document
wordfreq<-vector()
for(i in 1:nrow(dict)){
  wordfreq[i]<-sum(dict[i,])
}
wordfreq<-data.frame(dict[[6]]$Terms,wordfreq)
thematic<-wordfreq[order(-wordfreq[,2])[1:5],1]

sim<-matrix(NA,length(corpus),length(corpus))
maxes<-vector()
#token matching
image<-inspect(dict)
for(i in 1:nrow(sim)){
  for(j in 1:ncol(sim)){
    sim[i,j]<-sum(pmin(image[,i],image[,j]))                        
  }}
diag(sim)<-0
for(i in 1:nrow(sim)){
  if(max(sim[,i])>0){
  sim[,i]<-sim[,i]/max(sim[,i])
  }
}

features<-matrix(0,length(corpus),8)
for(i in 1:nrow(features)){
  if(length(sent_words[[i]])>0){features[i,1]<- sum(unlist(lapply(twords,wordcount,y=sent_words[[i]])))/length(twords)} #Title Words
  features[i,2]<- length(sent_words[[i]]) #sentence length
  features[i,3]<- NA#sentence position
  if(length(sent_words[[i]])>0){features[i,4]<- sum(!is.na(as.numeric(strsplit(origsent[[i]]," ")[[1]])))/length(sent_words[[i]])}#numerical data
  features[i,5]<- sum(unlist(lapply(thematic,wordcount,y=sent_words[[i]])))#thematic words top 5 words
  features[i,6]<- sum(sim[,i]) #similarity
  features[i,7]<-sum(image[,i]*wordfreq[,2]) #word frequency
  features[i,8]<-str_count(origsent[[i]],"[A-Z][a-z]") #Proper nouns?
}
#normalizing features
features[1:length(parapos),3]<-parapos
features[length(parapos):nrow(features),3]<-1
features[,3]<-features[,3]/max(features[,3])
features[,2]<-features[,2]/max(features[,2])
features[,5]<-features[,5]/max(features[,5])
features[,7]<-features[,7]/max(features[,7])



#SVD<-lsa(dict)$dk
#
#cosadj<-matrix(NA,nrow(SVD),nrow(SVD))
#cosine distance = angle between vectors [0,pi/2]
#for(i in 1:nrow(cosadj)){
#  for(j in 1:ncol(cosadj)){
#    if (!i==j){cosadj[i,j]<-1/acos((SVD[i,]%*%SVD[j,])/((sqrt(sum(SVD[i,]^2)))*(sqrt(sum(SVD[j,]^2)))))}
#  }
#}
#diag(cosadj)<-0

#g<-graph.adjacency(cosadj,weighted=T,mode='undirected')
#pgr<-page.rank(g)$vector
#summ<-data.frame(origsent,pgr)[order(-pgr),]
#cosine distance = cosine(inspect(dict))
