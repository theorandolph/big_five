
setwd('/Users/theodorerandolph/Documents/R/big_five')

library(tidyverse)

#### ---------------------------------------------------

df <- readr::read_csv('big_five_scores.csv') %>% 
    mutate(sex = case_when(
    sex == 1 ~ 'male',
    sex == 2 ~ 'female'
))

map(df, ~sum(is.na(.)))
map(df, ~length(unique(.)))

df %>% select(everything()) %>% 
    summarize_all(funs(sum(is.na(.))))
    
tidy %>% na.omit() %>% 
    group_by(country) %>% 
    summarize(count = n(),
              avg_agree = mean(agreeable_score),
              avg_con = mean(conscientiousness_score),
              avg_ext = mean(extraversion_score),
              avg_neuro = mean(neuroticism_score),
              avg_open = mean(openness_score)) %>% 
    filter(count >= 100) %>% 
    pivot_longer(cols = avg_agree:avg_open) %>% 
    ggplot() +
    geom_col(aes(x = country, y = value, 
                 group = country, fill = country),
             position = 'dodge') +
    facet_wrap(~name)


tidy %>% na.omit() %>% 
    group_by(sex) %>% 
    summarize(avg_agree = mean(agreeable_score),
              avg_con = mean(conscientiousness_score),
              avg_ext = mean(extraversion_score),
              avg_neuro = mean(neuroticism_score),
              avg_open = mean(openness_score))






