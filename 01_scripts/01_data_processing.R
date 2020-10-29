# Script is used for data processing ----
    # Contains functions for data processing

# Clean column names ----
clean_column_names_function <- function(data = raw_data){

    raw_data <- raw_data %>% 
        janitor::clean_names()
    
    return(raw_data)
}

clean_question_column_function <- function(data){
    
    data %>% 
        mutate(
            question = str_replace(question, "open_",""),
            question = str_replace_all(question, "_"," "),
            question = str_to_title(question)
        )
}

# Tokenize data ----
tokenize_data_function <- function(data = raw_data){
    
    tokenized_data <- data %>% 
        select(respondent_id,starts_with("open")) %>% 
        pivot_longer(
            cols = c(2:5),
            names_to = "question",
            values_to = "text"
            
        ) %>% 
        unnest_tokens(word, text) %>% 
        filter(!is.na(word))
    
    return(tokenized_data)
}

# Tokenize data, bigrams ----

ngram_data_function <- function(data = raw_data){
    
    ngram_tokenized_data <- data %>% 
        select(respondent_id,starts_with("open")) %>% 
        pivot_longer(
            cols = c(2:5),
            names_to = "question",
            values_to = "text"
            
        ) %>% 
        unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
        separate(word, c("word1", "word2"), sep = " ") %>% 
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>% 
        filter(!is.na(word1)) %>% 
        filter(word1 != 'NA') %>% 
        filter(word1 != 'n a') %>% 
        filter(!is.na(word2)) %>% 
        filter(word2 != 'NA') %>% 
        filter(word2 != 'n a') %>% 
        unite(word, word1, word2, sep = " ")
        
    return(ngram_tokenized_data)    
    
}

# Remove stop words ----
remove_stop_words_function <- function(data = tokenized_data){
    
    tidy_data_unigrams <- data %>% 
        filter(!(word %in% stopwords(source = "stopwords-iso")))   
    
    return(tidy_data_unigrams)
    
}



# Test functions ----
# data <- raw_data %>% 
#     clean_column_names_function()
# 
# tokenized_data <- tokenize_data_function(data = raw_data)
# 
# data <- tokenized_data
# remove_stop_words_function(data)
# raw_data <- raw_data %>%
#     clean_column_names_function()
# 
# ngram_data_function()
# 
# ngram_data_function()
