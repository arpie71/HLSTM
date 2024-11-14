# HLSTM
Repository to run STM on History Lab collections 

This repository contains a series of functions that were used to run Structural Topic Models on the History Lab collections. It uses Quanteda to convert a dataframe with at least a document id and text field 
into a DFM object. A separate function converts the DFM into a STM object. 


The functions can be used with any textual data. 



# Description of functions

## Convert dataframe to Quanteda DFM object


```prepDFM<-function(db = NULL, numbers = TRUE, punct = TRUE, stopwords = TRUE, symbols = TRUE, stem = TRUE, minchars = 3, textfield=NULL, idfield=NULL)```

The options are largely how to preprocess the DFM. Default options will remove numbers, punctuation, symbols, and stopwords. Tokens will also be stemmed and tokens with fewer than 3 characters will be removed. 

The `db` option specifies the name of the dataframe to convert to a DFM. The `textfield` and `idfield` options are used to pass the variables containing the document text and document id, respectively. 

The function will return a DFM object.

## Convert Quanteda DFM object to STM object
```prepLDA<-function(db = NULL,  lt = .025, ut = .4)```

The `lt` and `ut` options are short for "lower threshold" and "upper threshold." They specify the minimum percentage of documents a token must appear in to be included in the analysis and the maximum percentage. If there are 1000 documents in a collection, the default `lt` of 0.025 would drop any token that appears in fewer than 25 documents. The `ut` of 0.40 would drop any token that appears in more than 400 documents.


## Run diagnostic tests for a given range of topics

This function is a wrapper for the searchK functon in STM. Given a range of number of topics, it will run the searchK function and save the graph to a local drive. 

```searchKbroad<-function(db=NULL,outfile=NULL, trange=NULL)```

The `db` option specifies the name of the STM object while `outfile` specifies the file name for the resulting searchK graph. The `trange` option is short for topic range, and is structured like a range object in R. The first value is the starting number and the second number is the ending value. The third value provides the increment value. So if the option is set as trange(10,120,10), the function will run STM for a number of topics between 10 and 120 topics, adding 10 after each run. 

## The runSTM function will run an STM model for a given number of topics on an STM object. 


```runSTM<-function(db = NULL, stmobj = NULL, k = NULL,...)```


The function  will save the resulting STM results as well 4 csv files that the History Lab uses for its database. The four csv files are:

`db`_topic_doc.csv - Contains the topics and topic scores for each document. The file only has information on topic scores that are >0.05 for a document. The topics are sorted in descending order by topic score and we drop any topics once the cumulative topic score exceeds 0.75. 
`db`_tokens.csv - Contains a list of all of the tokens used in the model
`db`_topic_token.csv - Contains a list of all topics as well as each token's contribution to the topic
`db`_topics.csv - Contains the top 5 tokens for each topic

# Example

unDFM<-prepDFM(un,textfield='body',idfield='id')

cout<-prepLDA(unDFM,lt=.005, ut=.5)

csK<-searchKbroad(cout,'un_searchk.png', trange(10,120,10))
runSTM(db='un_test',stmobj=cout,k = 60)
