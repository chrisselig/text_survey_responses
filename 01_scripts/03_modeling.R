# Script is used to create models 

lda_model_function <- function(data,k = 10, n = 10){
    
    lda_topics <- LDA(data,k = k, control = list(seed = 1234)) %>% 
        tidy(matrix = "beta") %>% 
        group_by(topic) %>%
        top_n(n, beta) %>%
        ungroup() %>%
        arrange(topic, -beta)
    
    return(lda_topics)
    
}


# Testing functions ----

# tdm_function(unigram_tidy_data, 'open_change_1_thing') %>% 
#     lda_model_function(10,10)
