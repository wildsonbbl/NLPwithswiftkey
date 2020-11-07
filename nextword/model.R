
library(data.table, quietly = T, warn.conflicts = F)
library(dplyr, quietly = T, warn.conflicts = F)
library(quanteda)

datadir <- './data'
profpath <- paste0(datadir,'/profwords.txt')

profwords <- fread(text=profpath,
                   header = F,
                   sep = '',
                   sep2='',
                   data.table = T,
                   quote='')

model1 <- fread(paste0(datadir,'/model1.csv'))
model <- rbind(model1,fread(paste0(datadir,'/model2.csv')))
model <- rbind(model,fread(paste0(datadir,'/model3.csv')))


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

nextword <- function(input = ' New york new york nek york! !'){
    words <-  tokenizer(char_tolower(input))
    
    
    for(i in 2:1){
        put <- words[[1]] %>% 
            tail(i) %>% 
            paste(., collapse = ' ')
        
        nminus1gram <- model %>%
            filter(ngram == put)
        
        if(length(nminus1gram$frequency)==0){
            next
        }
        
        output <- model %>%
            filter(grepl(pattern = paste0('^',put,'\\s.*'),x = ngram)) %>%
            arrange(desc(frequency)) %>% rowwise() %>%
            mutate(prob = (frequency-runif(1,max = .5))/nminus1gram$frequency) %>%
            head(1)
        
        
        if(lengths(output[,1])!=0){
            return(tail(strsplit(output$ngram,' ')[[1]],1))
            
        }
        
    }
    
    output <- model1 %>%
        arrange(desc(frequency)) %>% 
        mutate(prob = frequency/sum(frequency)) %>%
        head(1)
    return(tail(strsplit(output$ngram,' ')[[1]],1))
}




