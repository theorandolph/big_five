
setwd('/Users/theodorerandolph/Documents/R/big_five')

library(tidyverse)
library(cluster)
library(factoextra)
library(dendextend)

df <- scale(USArrests)

# dissimilarity matrix
d <- dist(df, method = 'euclidean')
# hierarchical clustering using compelete linkage
hc1 <- hclust(d, method = 'complete')
# plot dendogram
plot(hc1, cex = 0.6, hang = -1)

hc2 <- agnes(df, method = 'complete')
hc2$ac

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
    agnes(df, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(df, method = 'ward')
pltree(hc3, cex = .6, hang = -1, 
       main = 'dendogram of agnes')

# divisional hierarchical clustering
hc4 <- diana(df)
# devise coefficient; amount of clustering structure found
hc4$dc
pltree(hc4, cex = 0.6, hang = -1,
       main = 'Dendogram of diana')

hc5 <- hclust(d, method = 'ward.D2')
sub_grp <- cutree(hc5, k = 4)
table(sub_grp)

plot(hc5, cex = .6)
rect.hclust(hc5, k = 4, border = 2:5)

fviz_cluster(list(data = df, cluster = sub_grp))

#### ---------------------------------------------------

df <- readr::read_csv('big_five_scores.csv')

tidy <- df %>% mutate(sex = case_when(
    sex == 1 ~ 'male',
    sex == 2 ~ 'female'
    ))

map(tidy, ~sum(is.na(.)))
tidy %>% select(everything()) %>% 
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






