# Script is used for data processing ----
    # Contains functions for data processing

# Clean column names ----
clean_column_names_function <- function(data){

    raw_data <- raw_data %>% 
        janitor::clean_names()
    
    return(raw_data)
}

# Tokenize data ----
tokenize_data_function <- function(data){
    
    tokenized_data <- data %>% 
        select(respondent_id,starts_with("open")) %>% 
        pivot_longer(
            cols = c(2:5),
            names_to = "question",
            values_to = "text"
            
        ) %>% 
        unnest_tokens(word, text)
    
    return(tokenized_data)
}



# Test functions ----
data <- raw_data

tokenize_data_function(data = raw_data)
