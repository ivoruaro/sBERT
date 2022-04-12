library(tidyverse)

#### Caricamento dati ####
embeddings <- 
    read_csv('embeddings_20k.csv') %>%
    select(!X1) %>%
    rename_with( ~ paste0("dim", str_pad(.x, 3, pad = '0')))


#### Norma del vettore ####
embeddings_norm <- embeddings %>%
    rowwise() %>%
    mutate(
        norm = sum(c_across(starts_with('dim'))^2)^0.5,
    ) %>%
    select(norm)

# Min/max -> vettori a norma 1
min(embeddings_norm$norm); max(embeddings_norm$norm)



#### Deviazione standard ####
embeddings_sd <- embeddings %>%
    summarize(across(everything(), sd)) %>%
    pivot_longer(everything())

# Top 5
embeddings_sd %>%
    arrange(desc(value)) %>%
    head(5)

# Bottom 5
embeddings_sd %>%
    arrange(value) %>%
    head(5)

# Plot
embeddings_sd %>%
    arrange(desc(value)) %>%
    rowid_to_column("id") %>%
    ggplot(
        aes(
            x = id,
            y = value
        )
    ) +
    geom_line() +
    theme_minimal() +
    labs(
        title = 'Variazione della deviazione standard per dimensione',
        subtitle = 'Ordinato per deviazione standard decrescente',
        x = '',
        y = ''
    )


#### Media, Max, Min ####
embeddings_mean <- embeddings %>%
    summarize(across(everything(), mean)) %>%
    pivot_longer(everything())

embeddings_max <- embeddings %>%
    summarize(across(everything(), max)) %>%
    pivot_longer(everything())

embeddings_min <- embeddings %>%
    summarize(across(everything(), min)) %>%
    pivot_longer(everything())

embeddings_mean %>%
    rename(mean = value) %>%
    select(mean) %>%
    mutate(
        min = embeddings_min$value,
        max = embeddings_max$value,
        ) %>%
    # arrange(desc(mean)) %>%
    rowid_to_column("id") %>%
    pivot_longer(!id) %>%
    ggplot(
        aes(
            x = id,
            y = value,
            color = name
        )
    ) +
    geom_line() +
    theme_minimal() +
    labs(
        title = 'Confronto tra max, min e mean per dimensione',
        subtitle = '',
        x = '',
        y = '',
        color = ''
    )


#### Analisi riga ####
embeddings[100,] %>%
    pivot_longer(everything()) %>%
    mutate(value = abs(value)) %>%
    arrange(desc(value)) %>%
    rowid_to_column("id") %>%
    ggplot(
        aes(
            x = id,
            y = value
        )
    ) +
    geom_col()


#### Boxplot ###
# 1:49 50:98 99:147 148:196 197:245 246:294 295:343 344:393
f <- paste0('dim', str_pad(c(9), 3, pad = '0'))
embeddings %>%
    pivot_longer(everything()) %>%
    filter(name %in% f) %>%
    ggplot(
        aes(
            x = 0,
            y = value,
            fill = name
        )
    ) +
    #geom_boxplot(fill = 'purple') +
    geom_violin(alpha=0.2) +
    geom_jitter(color = "black", size=0.4, alpha=0.2) +
    theme_minimal() +
    theme(
        axis.text.x = element_blank(),
        legend.position = 'none'
        ) +
    facet_wrap( ~ name) +
    labs(
        x = '',
        y = ''
    )


f <- paste0('dim', str_pad(c(27:35), 3, pad = '0'))
embeddings %>%
    pivot_longer(everything()) %>%
    filter(name %in% f) %>%
    ggplot(
        aes(
            x = value
        )
    ) +
    geom_histogram(bins = 500, alpha = 0.5) +
    facet_wrap(~ name)
