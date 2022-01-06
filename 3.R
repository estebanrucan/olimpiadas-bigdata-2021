require(tidyverse)
require(factoextra)
require(parameters) # install.packages("parameters", dependencies = TRUE)

data_raw <- read_delim("data/countries.csv")

data <- data_raw %>% select(-country)

rownames(data) <- data_raw$country

factoextra::fviz_nbclust(
    data,
    kmeans,
    method = "wss"
) +
    geom_vline(xintercept = 3)

cuantos_clusters <- n_clusters(data)
plot(cuantos_clusters)

algoritmo_km <- eclust(
    x          = data,
    FUNcluster = "kmeans",
    k          = 3,
    graph      = FALSE,
    stand      = TRUE
)

fviz_cluster(
    algoritmo_km,
    ellipse       = TRUE,
    ellipse.type  = "norm",
    ellipse.level = 0.95,
    ellipse.alpha = 0.1
) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal()

data_raw %>% 
    select(-country) %>% 
    mutate(lab = as.factor(algoritmo_km$cluster)) %>% 
    group_by(lab) %>% 
    summarise(
        across(everything(), list(media = mean, dev.est = sd))
    )

rownames(data) <- data_raw$country

algoritmo_hc <- hcut(
    data, 
    k         = 3, 
    hc_method = "ward.D",
    stand     = TRUE
)

fviz_dend(algoritmo_hc, rect = TRUE)

fviz_cluster(
    algoritmo_hc,
    ellipse       = TRUE,
    ellipse.type  = "norm",
    ellipse.level = 0.95,
    ellipse.alpha = 0.1
) +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal()
