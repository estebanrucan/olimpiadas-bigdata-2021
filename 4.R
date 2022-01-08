require(tidyverse)
require(leaflet)
require(lubridate)
require(gganimate)

data_raw <- read_delim("data/gapminder.csv")
data_raw %>% skimr::skim()

data_raw %>% glimpse()
data <- data_raw %>% mutate(collected = dmy(collected))

data_chile <- data %>% filter(country == "Chile")
data_2010  <- data %>% filter(year(collected) == 2010) 

ggplot(data_chile) +
    aes(x = collected, y = population) +
    geom_line(size = 1, colour = "#421146") +
    theme_minimal()

data_2010 %>% 
    mutate(gdppercap = gdp / population) %>% 
    ggplot(aes(
        gdppercap, 
        life_expectancy, 
        color = continent,
        size = population
    )) +
    geom_point(show.legend = FALSE) +
    scale_color_viridis_d() +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    theme_minimal() +
    labs(
        title    = "PIB per-cápita y Esperanza de vida",
        subtitle = "Año: 2010"
    ) +
    theme_bw()

data %>% 
    mutate(gdppercap = gdp / population) %>% 
    ggplot(aes(
        gdppercap, 
        life_expectancy, 
        color = continent,
        size  = population
    )) +
    geom_point(show.legend = FALSE) +
    scale_color_viridis_d() +
    scale_size(range = c(2, 12)) +
    scale_x_log10() +
    theme_minimal() +
    labs(
        title    = "PIB per-cápita y Esperanza de vida",
        subtitle = "Fecha: {frame_time}"
    ) +
    theme_bw() +
    transition_time(collected)

data_2010 %>% 
    mutate(label = str_glue("Country: {country}<br>
                            Continent: {continent}<br>
                            Life Expectancy: {life_expectancy}")) %>% 
    leaflet() %>% 
    addTiles() %>% 
    addMarkers(label = ~ lapply(label, htmltools::HTML))

map_data("world") %>% 
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(color = "white", fill = "gray20") +
    geom_point(
        data = data_2010,
        mapping = aes(
            x     = longitude,
            y     = latitude, 
            color = life_expectancy,
            group = NULL
        )
    ) +
    scale_color_viridis_c(option = "magma") +
    labs(title = "Esperanza de vida", subtitle = "Año: 2010") +
    ggthemes::theme_map() 

map_data("world") %>% 
    ggplot(aes(long, lat, group = group)) +
    geom_polygon(color = "white", fill = "gray20") +
    geom_point(
        data = data_2010,
        mapping = aes(
            x     = longitude,
            y     = latitude, 
            color = log10(gdp / population),
            group = NULL
        ),
        size = 3
    ) +
    scale_color_viridis_c(option = "magma") +
    ggthemes::theme_map()

