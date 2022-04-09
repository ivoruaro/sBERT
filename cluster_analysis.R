library(tidyverse)
library(stylo)

# The natural measure is cosine-similarity so 
# we cannot use the k-means clustering

#### Loading and wrangling data (q&d) ####
embeddings <- read_csv('embeddings_20k.csv') %>%
    rename_with(~ paste0("dim", str_pad(.x, 3, pad = '0'))) %>%
    rename(id = dim0X1) 

embeddings

# Get random rows
set.seed(1)
embeddings_sampled <- embeddings %>%
    slice_sample(n = 1000L)


#### cosine distance ####
embeddings_dist_cos <- dist.cosine(as.matrix(embeddings_sampled[,-1]))


#### Hierarchical clustering #### 
# Analizzando visivamente le 3 categorie di linkage (complete, single e average)
# si puo vedere che la separazione più bilanciata è data da complete
hc_complete <- hclust(
    embeddings_dist_cos,
    method = "complete"
)
plot(hc_complete, main = 'Complete Linkage')


hc_single <- hclust(
    embeddings_dist_cos,
    method = "single"
)
plot(hc_single, main = 'Single Linkage')

hc_average <- hclust(
    embeddings_dist_cos,
    method = "average"
)
plot(hc_average, main = 'Average Linkage')

# Taglio del dendogramma a 20 (arbitrario) significati
clusters_k20 <- cutree(hc_complete, k = 20)

embeddings_k20_complete <- embeddings_sampled %>%
    mutate(cluster = clusters_k20)

count(embeddings_k20_complete, cluster) %>%
    arrange(desc(n)) %>%
    ggplot(
        aes(
            x = reorder(cluster, -n),
            y = n
        )
    ) +
    geom_col() +
    theme_minimal() +
    labs(
        title = 'Distribuzione numero documenti per cluster',
        x = 'cluster',
        y = '',
        caption = 'Campione: 1000 documenti'
    )


embeddings_k20_complete %>%
    select(id, cluster)


#### PCA #####
# Taglio del dendogramma a 10 (arbitrario) significati
clusters_kx <- cutree(hc_complete, k = 5)

embeddings_kx_complete <- embeddings_sampled %>%
    mutate(cluster = clusters_kx)

embeddings_pca <- 
    prcomp(
        embeddings_sampled[,-1],
        center = TRUE
    )

embeddings_reduced <- embeddings_kx_complete %>%
    mutate(
        PC1 = embeddings_pca$x[,1],
        PC2 = embeddings_pca$x[,2]
    )

embeddings_reduced %>%
    ggplot(
        aes(
            x = PC1,
            y = PC2,
            color = factor(cluster)
        )
    ) +
    geom_point(alpha = 0.8) +
    theme_minimal()

summary(embeddings_pca)
# PC1 e PC 2 spiegano il (0.08976 + 0.03586) 12.6% dei dati
