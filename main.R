library(tidyverse)
library(lsa)

embeddings_raw <- read_csv('embeddings_20k.csv')

# TODO: da migliorare l'analisi
embeddings <- embeddings_raw %>%
    # head(10) %>%
    select(!X1) %>%
    rename_with( ~ paste0("dim", str_pad(.x, 3, pad = '0'))) %>%
    mutate(
        type = if_else(row_number() <= 10000L, 'A', 'B')
    ) %>%
    rowwise() %>%
    mutate(
        norm = sum(c_across(starts_with('dim'))^2)^0.5,
        min = min(c_across(starts_with('dim'))),
        max = max(c_across(starts_with('dim')))
    )


#### I vettori sono normalizzati ad 1 ####
min(embeddings$'norm')
max(embeddings$'norm')


#### Il significato non si adagia su nessuna delle dimensioni ####
min(embeddings$'min')
max(embeddings$'max')


#### Distribuzione ####
# 1:49 50:98 99:147 148:196 197:245 246:294 295:343 344:393
#             ^               ^               ^
#            127             223             319    <- dimensioni a zero
# view(embeddings %>% select(dim319))
f <- paste0('dim', str_pad(c(319), 3, pad = '0'))
embeddings %>%
    select(starts_with('dim')) %>%
    pivot_longer(everything())  %>% # name, value
    filter(name %in% f) %>%
    ggplot(
        aes(
            x = value
        )
    ) +
    geom_density(alpha=.2, fill="#FF6666") +
    theme_minimal() +
    facet_wrap( ~ name)


#### Confronto dimensioni ####
embeddings %>%
    ggplot(
        aes(
            x = dim203,
            y = dim204,
            color = type
        )
    ) +
    geom_point() +
    xlim(-1, 1) + 
    ylim(-1, 1) +
    theme_minimal()


#### cosine similarity ####
docs_a <- embeddings %>%
    filter(type == 'A') %>%
    select(starts_with('dim'))
    
docs_b <- embeddings %>%
    filter(type == 'B') %>%
    select(starts_with('dim'))

cosine(
    unlist(docs_a[1,], use.names = FALSE), 
    unlist(docs_a[2,], use.names = FALSE)
)

cosine(
    unlist(docs_a[1,], use.names = FALSE), 
    unlist(docs_a[200,], use.names = FALSE)
)

cosine(
    unlist(docs_a[1,], use.names = FALSE), 
    unlist(docs_b[1,], use.names = FALSE)
)


#### Lento 
# Ricordati di azzerare df
df <- as_tibble(
    data.frame(
        x = numeric(),          
        y = numeric(),
        cos = numeric()
    )
)

for (i in 1:100) {
    for (j in 1:100) {
        d <- cosine(
            unlist(docs_a[[i]], use.names = FALSE),
            unlist(docs_a[[j]], use.names = FALSE)
        )
        df <- df %>%
            add_row(x = i, y = j, cos = d)
    }
}

df %>%
    ggplot(
        aes(
            x = x,
            y = y,
            fill = cos
        )
    ) +
    geom_tile() +
    scale_fill_gradient2(
        limits = c(-1,1),
        low = "red",
        mid = "white",
        high = "blue",
        midpoint = 0
    ) +
    theme_void() +
    labs(
        title = 'A vs A',
        caption = 'Primi 100 documenti'
    )

#### media ####
a <-embeddings %>%
    group_by() %>%
    summarize_if(is.numeric, mean) %>%
    select(starts_with('dim'))
