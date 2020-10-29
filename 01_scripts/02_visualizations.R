# Script is used for functions ----
    # Contains functions for creating data visualizations


# Bar Chart ----

barchart_function <- function(
    data = unigram_tidy_data,
    title = NA_character_,
    number_of_rows = 10
                              ){
    data %>% 
        count(question, word) %>%
        group_by(question) %>%
        arrange(desc(n)) %>%
        top_n(number_of_rows) %>% 
        ungroup() %>% 
        mutate(
            question = as.factor(question),
            word = reorder_within(word, n, question)
        ) %>% 
        mutate(
            question = str_replace(question, "open_",""),
            question = str_replace_all(question, "_"," "),
            question = str_to_title(question)
        ) %>% 
        ggplot(aes(word,n)) +
        geom_bar(stat = 'identity',aes(fill = n), fill = '#A2AAB0') +
        coord_flip() +
        geom_text(aes(label = n), size = 3, hjust = 1.2, color = 'black') +
        facet_wrap(~question,scales = "free") +
        scale_x_reordered() +
        labs(
            x = '',
            y = '',
            title = title
        ) +
        theme(
            plot.title = element_text(size = 14, family = "memphis",color = '#4C586F', face = 'bold'),
            plot.subtitle = element_text(hjust = 0.01, size = 11,family = "ArnoProLightDisplay"),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            legend.position = "none"
        )
    
}


# Test function ----
# title = ""
# barchart_function()
