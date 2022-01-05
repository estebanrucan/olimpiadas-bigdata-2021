require(tidyverse)
require(pspline)

data <- read_csv("data/mcdonalds.csv")


data %>% 
    select(where(is.numeric)) %>% 
    GGally::ggpairs()

data %>% 
    correlation::correlation() %>% 
    as_tibble() %>% 
    arrange(-abs(r))
    
data %>% 
    mutate(categoria = fct_reorder(categoria, calorias)) %>% 
    ggplot(aes(categoria, calorias)) +
    geom_boxplot() +
    coord_flip()

# NW

bw <- seq(50, 200, 10)
R  <- matrix(nrow = length(bw), ncol = nrow(data))

for (i in seq_along(bw)) {
    
    for (j in seq_along(data$articulo)) {
        
        y_hat <- ksmooth(
            x         = data$sodio[-j],
            y         = data$grasa_saturada[-j],
            kernel    = "normal",
            bandwidth = bw[i],
            x.points  = data$sodio[j]
        )$y
        
        R[i, j] <- data$grasa_saturada[j] - y_hat
        
    }
    
}

CV <- apply(R, 1, crossprod) / nrow(data)

grafico <- tibble(
    bw = bw,
    cv = CV
) %>% 
    ggplot(aes(bw, cv)) +
    geom_point(color = "darkorange") +
    geom_line(color = "dodgerblue") +
    theme_minimal()

grafico %>% plotly::ggplotly()

ancho_de_banda <- bw[which.min(CV)]

ajuste_nw <- ksmooth(
    x         = data$sodio,
    y         = data$grasa_saturada,
    kernel    = "normal",
    bandwidth = ancho_de_banda
)

# Gráfico final

data %>% 
    ggplot(aes(sodio, grasa_saturada)) +
    geom_point() +
    geom_line(
        aes(x = ajuste_nw$x, y = ajuste_nw$y)
    )

## Splines

splines_cv <- crossing(
    norder = 2:4,
    alpha  = 10 ** seq(-5, 0, len = 100)
) %>% 
    mutate(
        cv = map2(
            norder, alpha,
            ~ sm.spline(
                x      = data$sodio,
                y      = data$grasa_saturada,
                norder = .x,
                spar   = .y
            )$cv
        )
    ) %>% 
    unnest(cv)

splines_cv %>% 
    ggplot(aes(alpha, cv, color = factor(norder))) +
    geom_line()

splines_cv %>% 
    slice_min(cv, n = 1)

spline_final <- sm.spline(
    x      = data$sodio,
    y      = data$grasa_saturada,
    norder = 2,
    spar   = 1
)

spline_pred <- predict(
    spline_final, 
    x = seq(0, 3600, len = 500)
) %>%
    as.data.frame() %>% 
    mutate(x = seq(0, 3600, len = 500))

data %>% 
    ggplot(aes(sodio, grasa_saturada)) +
    geom_point(color = "darkorange") +
    geom_line(
        aes(x, V1), 
        data = spline_pred,
        color = "dodgerblue"
    ) +
    theme_minimal()
