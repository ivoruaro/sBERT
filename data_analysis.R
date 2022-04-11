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


