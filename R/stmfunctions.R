#' HLSTM
#'
#' @import dplyr
#' @import stm
#' @import quanteda
#' @import tidyr
#' @import reshape2
#' @import utils
#' @import tidytext
#' @importFrom grDevices dev.off png
#' @importFrom graphics par
#' @importFrom magrittr %>%

#' @name globalvars
globalVariables(c('topic_id','topic_score','doc_id','ave','top75','topicsum','term','title','V1','V3', 'V5'))
NULL

#' Create DFM object
#' @name prepDFM
#' @usage
#' prepDFM(db = NULL, numbers = TRUE, punct = TRUE, stopwords = TRUE, symbols = TRUE, stem = TRUE, minchars = 3, textfield=NULL, idfield=NULL)
#' @param db Dataframe with document text and ID variables
#' @param numbers Eliminate numbers from the DFM
#' @param punct Eliminate punction from the DFM
#' @param stopwords Eliminate stopwords from the DFM
#' @param symbols Eliminate symbols from the DFM
#' @param stem Stem tokens when creating the DFM
#' @param minchars Minimum number of characters for token to appear in DFM
#' @param textfield Name of text variable in dataframe
#' @param idfield Name of document id variable in dataframe
#' @export
#' @return Returns Quanteda DFM object
prepDFM<-function(db = NULL, numbers = TRUE, punct = TRUE, stopwords = TRUE, symbols = TRUE, stem = TRUE, minchars = 3, textfield=NULL, idfield=NULL) {

#' @importFrom quanteda corpus  tokens dfm  dfm_remove dfm_wordstem  dfm_select

  trcorp<-quanteda::corpus(db, docid_field = idfield,text_field = textfield)
  trtok<-quanteda::tokens(trcorp, what="word", remove_punct=punct,remove_numbers=numbers, remove_symbols = symbols)
  trdfm<-quanteda::dfm(trtok)
  if(stopwords==TRUE){
    trdfm<-quanteda::dfm_remove(trdfm,stopwords('english'))
  }
  if(stem==TRUE) {
    print("Stemming")
    trdfm<-quanteda::dfm_wordstem(trdfm)
  }
  trdfm<-quanteda::dfm_select(trdfm,min_nchar=minchars)
  return(trdfm)
}


#' Create STM object from a DFM object
#' @name prepLDA
#' @usage prepLDA(db = NULL,  lt = .025, ut = .4)
#' @param db Name of existing DFM object to convert to STM object
#' @param lt Lower threshold of document percentage (0 to 1 scale). Terms that occur in less than this percenage of documents will be dropped.
#' @param ut Upper threshold of document percentage (0 to 1 scale). Terms that occur in more than this percentage of documents will be dropped.
#' @export
#' @return Returns STM object

prepLDA<-function(db = NULL,  lt = .025, ut = .4) {
#' @importFrom stm prepDocuments
#' @importFrom quanteda convert

l = nrow(db@docvars)
lt = round(l*lt)
ut = round(l*ut)

processed<-quanteda::convert(db, to="stm")
out<-stm::prepDocuments(processed$documents, processed$vocab, processed$meta, lower.thresh = lt, upper.thresh=ut)
return(out)
}

#' Wrapper for STM's searchK function
#' @name searchKbroad
#' @usage searchKbroad(db=NULL,outfile=NULL, trange=NULL)
#' @export
#' @param db Name of STM object
#' @param outfile File location to save graph
#' @param trange Range of topic numbers to search
#' @return Returns png file of searchK graph

searchKbroad<-function(db=NULL,outfile=NULL, trange=NULL){
  out<-db
searchK <- searchK(out$documents, out$vocab, K = trange, init.type = "Spectral", data = out$meta)
plot(searchK)
png(outfile, width = 6, height = 6, units = 'in', res = 300)
par(ps=12,cex=1, cex.main=1.1)
plot(searchK)
dev.off()
return(searchK)
}

create_topic_doc<-function(stm_results = NULL, outfile=NULL){
  topic_doc <- stm_results  %>% gather(topic_id, topic_score, starts_with('V')) %>% arrange(doc_id, desc(topic_score))
  topic_doc$topic_id <- as.numeric(gsub("V","",topic_doc$topic_id))
  topic_doc<-subset(topic_doc, topic_score>=.01)
  topic_doc$topicsum<-ave(topic_doc$topic_score,topic_doc$doc_id, FUN=cumsum)
  topic_doc[((topic_doc$topicsum<=.75) | (topic_doc$topicsum>.75 & (topic_doc$topicsum-topic_doc$topic_score<.75))) ,'top75']<-1
  topic_doc<-subset(topic_doc,top75==1 & topic_score>=.05)
  topic_doc<-subset(topic_doc,select=-c(topicsum,top75))
  write.csv(topic_doc,outfile, row.names=FALSE)
  print("Saved topic_doc.csv")
}

#' Runs STM for given number of topics
#' @name runSTM
#' @usage runSTM(db = NULL, stmobj = NULL, k = NULL,...)
#' @param db  Name to use to prefix saved files
#' @param stmobj STM object to run analysis on
#' @param k Number of topics to estimate
#' @param ... Not used
#' @export
runSTM<-function(db = NULL, stmobj = NULL, k = NULL,...){
  name<-NULL
  #k<-60
  stmmodel<- stm(stmobj$documents, stmobj$vocab, K=k,
                 max.em.its = 300,
                 data = stmobj$meta, init.type = "Spectral")

  stmresults<-cbind(data.frame(doc_id=names(stmobj$documents)), as.data.frame(stmmodel$theta))
  create_topic_doc(stmresults,paste0(db,"_topic_doc.csv"))
  tokens<-as.data.frame(stmobj$vocab) %>% mutate(id = row_number()) %>% rename('value' = 'stmobj$vocab' )
  utils::write.csv(tokens,paste0(db,"_tokens.csv"), row.names=FALSE)
  print("Saved tokens.csv")
  topic_token <- tidy(stmmodel, matrix='beta') %>% merge(tokens, by.x='term',by.y='value') %>% select(-term)
  utils::write.csv(topic_token,paste0(db,"_topic_token.csv"), row.names=FALSE)
  print("Saved topic_token.csv")
  topics<-as.data.frame(labelTopics(stmmodel, n = 5)$prob) %>% mutate(id = row_number()) %>% unite(title,V1:V3, remove=FALSE, sep=", ") %>% unite(name,V1:V5, remove=TRUE, sep=", ")
  utils::write.csv(topics,paste0(db,"_topics.csv"), row.names=FALSE)
  print("Saved topics.csv")
  saveRDS(stmmodel,paste0(db,'_stm.rds'))
  print('Saved stm model')
    }

runSTM_from_model<-function(db = NULL, stmobj = NULL, stmmodel = NULL){
  name<-NULL
stmresults<-cbind(data.frame(doc_id=names(stmobj$documents)), as.data.frame(stmmodel$theta))
create_topic_doc(stmresults,paste0(db,"_topic_doc.csv"))
tokens<-as.data.frame(stmobj$vocab) %>% mutate(id = row_number()) %>% rename('value' = 'stmobj$vocab' )
write.csv(tokens,paste0(db,"_tokens.csv"), row.names=FALSE)
print("Saved tokens.csv")
topic_token <- tidy(stmmodel, matrix='beta') %>% merge(tokens, by.x='term',by.y='value') %>% select(-term)
write.csv(topic_token,paste0(db,"_topic_token.csv"), row.names=FALSE)
print("Saved topic_token.csv")
topics<-as.data.frame(labelTopics(stmmodel, n = 5)$prob) %>% mutate(id = row_number()) %>% unite(title,V1:V3, remove=FALSE, sep=", ") %>% unite(name,V1:V5, remove=TRUE, sep=", ")
write.csv(topics,paste0(db,"_topics.csv"), row.names=FALSE)
print("Saved topics.csv")
}
