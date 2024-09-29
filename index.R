---
  title: "GAPMINDER | Happiness Over the Years"
format:
  dashboard:
  embed-resources: true
theme: lux
---
  
  ```{r}
# Load packages 
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, gapminder,
               bslib, bsicons, shiny, 
               rnaturalearth, plotly, 
               countrycode, htmltools, 
               reactable, here
)

```

```{r}
pop <- read_csv(here("data/pop.csv"))

pop_iso <- pop %>% 
  pivot_longer(-country, names_to = "year", values_to = "pop") %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(pop = case_when(
    str_detect(pop, 'k$') ~ as.numeric(str_remove(pop, "k")) * 1000,
    str_detect(pop, 'M$') ~ as.numeric(str_remove(pop, "M")) * 1000000,
    str_detect(pop, 'B$') ~ as.numeric(str_remove(pop, "M")) * 1000000000,
    TRUE ~ as.numeric(pop))) %>% 
  mutate(country_iso = countrycode(country, "country.name", "iso3c"))

happy_data <- read_csv(here("data/hapiscore_whr.csv")) %>% 
  pivot_longer(`2005`:`2022`, names_to = "year", values_to = "happiness") %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(country_iso = countrycode(country, "country.name", "iso3c")) %>% 
  left_join(pop_iso) #join population data


```


```{r}
gap_22 <- happy_data %>% 
  left_join(pop_iso) %>% 
  filter(year == 2022)

happiest_country <- 
  gap_22 %>% 
  arrange(-happiness) %>% 
  head(1) %>% 
  pull(country)

happiest_country_value <- 
  gap_22 %>% 
  arrange(-happiness) %>% 
  head(1) %>% 
  pull(happiness)

lowest_happiness_country <- 
  gap_22 %>% 
  arrange(happiness) %>% 
  head(1) %>% 
  pull(country)

lowest_happiness_value <- 
  gap_22 %>% 
  arrange(happiness) %>% 
  head(1) %>% 
  pull(happiness)

average_happiness_value <-
  gap_22 %>%
  summarise(
    weighted_mean = sum(happiness * pop/ sum(pop, na.rm = TRUE), na.rm = TRUE)
  ) %>%
  round(1)

#correlate population size and child deaths!
```


```{r warning = FALSE}
country_shapes <- rnaturalearth::ne_countries()


# gap_map <-
#   left_join(country_shapes, gap_22,
#           by = c("adm0_a3" = "country_iso")) %>%
#   mutate(tooltip_label = paste(country,
#                                year,
#                                happiness,
#                                sep = ": ")) %>%
#   ggplot(aes(fill = happiness, text = tooltip_label, frame = year)) +
#   geom_sf()+
#   theme_void()+
#   theme(legend.position = "none")+
#   scale_fill_viridis_c()


gap_map_ggplotly <- ggplotly(gap_map, tooltip = "text")

#using plot_ly here since geom_sf in combination with the "frame" option for the slider produced buggy output
gap_map_plot_ly <-
  left_join(country_shapes, happy_data,
            by = c("adm0_a3" = "country_iso")) %>%
  mutate(tooltip_label = paste0(country, ", "
                                year, ": "
                                happiness)) %>% 
  plot_ly(stroke = I("black"), split=~adm0_a3, color=~happiness, text=~tooltip_label, showlegend = FALSE, hoverinfo = "text", hoveron = "fills", frame=~year, alpha = 1) %>%
  style(hoverlabel = list(bgcolor = "white")) %>% 
  animation_slider(currentvalue = list(prefix = "year", font = list(color="red")))

```

```{r}
# top_20_countries <- gap_07 %>% 
#   arrange(desc(lifeExp)) %>% 
#   head(20) %>% 
#   mutate(tooltip_label = paste(country, 
#                                round(lifeExp, 1), 
#                                sep = ": ")) %>% 
#   ggplot(aes(y = reorder(country, lifeExp), 
#              x = lifeExp, 
#              fill = lifeExp, 
#              text = tooltip_label)) + 
#   geom_col() + 
#   geom_text(aes(label = round(lifeExp, 1)), 
#             nudge_x = -10, 
#             color = "white"
#             ) + 
#   labs(y = "Country", 
#        x = "Life Exp") + 
#   theme(legend.position = "none")
# 
# top_20_countries_ggplotly <- 
#   ggplotly(top_20_countries, tooltip = "text")
```



# HOME

## Row 1 {height=25%}

```{r}
value_box(
  title = "Highest Happiness Score",
  value = happiest_country_value,
  showcase = bsicons::bs_icon("heart"),
  theme = value_box_theme(bg = "#518fd6"),
  p(paste0("(", happiest_country, ")"))
)
```

```{r}
value_box(
  title = "Lowest Happiness score",
  value = lowest_happiness_value,
  showcase = bsicons::bs_icon("thermometer"),
  theme = value_box_theme(bg = "#214773"),
  p(paste0("(", lowest_happiness_country, ")"))
)
```

```{r}
value_box(
  title = "Average Happiness",
  value = average_happiness_value,
  showcase = bsicons::bs_icon("graph-up"),
  theme = value_box_theme(bg = "#3f71ab")
)
```


## Row 2 {height=75%}

### {width=70%}

```{r title = "Map of Countries by Life Expectancy"}
gap_map_ggplotly
```


### {width=30%}

```{r title = "Top 20 Countries by Life Expectancy"}
#top_20_countries_ggplotly
```

# DOWNLOAD DATA

The data used in this dashboard is shown below and can be downloaded as a CSV. 

```{r}
# library(htmltools)
# 
# htmltools::browsable(
#   tagList(
#     reactable(gapminder, 
#               elementId = "gapminder-table", 
#               searchable = T, 
#               filterable = T), 
#     
# tags$button("Download as CSV", 
#             onclick = "Reactable.downloadDataCSV('gapminder-table')")
#   )
# )
```



# ABOUT

This data comes from the r `gapminder` package, and is originally sourced from the Gapminder Foundation.

The Gapminder Foundation is a non-profit venture registered in Stockholm, Sweden, that promotes sustainable global development and achievement of the United Nations Millennium Development Goals by increased use and understanding of statistics and other information about social, economic, and environmental development at local, national, and global levels.

Gapminder was founded in 2005 by Ola Rosling, Anna Rosling Rönnlund, and Hans Rosling. The name Gapminder was derived from the "Mind the Gap" warning messages on the London Underground.

An example of one of Hans Rosling's videos is shown below: 

<iframe width="560" height="315" src="https://www.youtube.com/embed/hVimVzgtD6w?si=hjxf5ryxh7mx5A4R" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>