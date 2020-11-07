
library(data.table, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(quanteda, quietly = T, warn.conflicts = F)
library(doParallel)

Sys.setlocale('LC_ALL','English')  

datadir <- './data'
profpath <- paste0(datadir,'/profwords.txt')

profwords <- fread(text=profpath,
                   header = F,
                   sep = '',
                   sep2='',
                   data.table = T,
                   quote='')

texts <- fread(text=paste0(datadir,'/sample.txt'),
               header = F,
               sep = '',
               data.table = T,stringsAsFactors = F)


modelcorpus <- corpus(texts$V1)

rm(texts)

tokenizer <- function(corpus){
    words <- tokens(corpus,
                    remove_punct = TRUE,
                    remove_symbols = T,
                    remove_separators = T,
                    remove_numbers=T,
                    what = 'word') %>%
        tokens_remove(stopwords('english')) %>%
        tokens_remove(pattern = '[^A-Za-z]|^[a-zA-Z]$',valuetype = 'regex') %>%
        tokens_remove(profwords$V1)
    return(words)
}

cluster <- makeCluster(detectCores() - 1) 
registerDoParallel(cluster)

modeltokens <- tokenizer(modelcorpus)

rm(modelcorpus)

makengramfreq <- function(n=1){
    
    model <- modeltokens %>% 
        tokens_ngrams( n = n , concatenator = ' ') %>% 
        dfm() %>% docfreq() %>%
        data.table(ngram = names(.), frequency = .) %>%
        arrange(desc(frequency)) %>% filter(frequency>1)
    
    return(model)
}

model1 <-  makengramfreq(1)
write.table(model1,paste0(datadir,'/model1.csv'),row.names = F,col.names = T,sep = ',')
rm(model1)
model2 <-  makengramfreq(2)
write.table(model2,paste0(datadir,'/model2.csv'),row.names = F,col.names = T,sep = ',')
rm(model2)
model3 <-  makengramfreq(3)
write.table(model3,paste0(datadir,'/model3.csv'),row.names = F,col.names = T,sep = ',')
rm(model3)
stopCluster(cluster)
registerDoSEQ()



