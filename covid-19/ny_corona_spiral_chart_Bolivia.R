# Recreating the New York Times COVID-19 Spiral Graph
# Ref: https://bydata.github.io/nyt-corona-spiral-chart/
# Adaptado para Bolivia

# Paquetes
#pacman::p_load("tidyverse", "ggtext", "here", "lubridate")

library(tidyverse)
library(ggtext)
library(here)
library(lubridate)

# Cargar y preparar los datos
owid_url <- "https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true"
country <- "Bolivia"
covid <- read_csv(owid_url)
covid_cases <- covid %>% 
  filter(location == country) %>% 
  select(date, new_cases, new_cases_smoothed) %>% 
  arrange(date) %>% 
  # Add the dates before the 1st confirmed case
  add_row(date = as_date("2020-01-01"), new_cases = 0, new_cases_smoothed = 0,
          .before = 1) %>% 
  complete(date = seq(min(.$date), max(.$date), by = 1),
           fill = list(new_cases = 0, new_cases_smoothed = 0)) %>% 
  mutate(day_of_year = yday(date),
         year = year(date)
  )

# Un gráfico muy básico
p <- covid_cases %>% 
  ggplot() +
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date))) +
  coord_polar()
p

# Se aplica theme_void para quitar cualquier elemento del tema
p + theme_void()

# Visualización  
size_factor <- 3000

# Colors
outline_color <- "#D97C86"
fill_color <- "#F0C0C1"
base_grey <- "grey28"

p <- covid_cases %>% 
  ggplot() +
  # area to encode the number of cases
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(date) - new_cases_smoothed / 2 * size_factor,
                  ymax = as.POSIXct(date) + new_cases_smoothed / 2 * size_factor,
                  group = year),
              size = 0.3, col = outline_color, fill = fill_color, show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date)),
               col = base_grey, size = 0.3) +
  coord_polar() +
  theme_void()
p

# Líneas de cuadrícula
month_length <- c(31, 28, 31, 30, 31, 30,
                  31, 31, 30, 31, 30, 31)

month_breaks <- cumsum(month_length) - 30

# Aplicar la cuadrícula
p + scale_x_continuous(minor_breaks = month_breaks, 
                       breaks = month_breaks[c(1, 4, 7, 10)],
                       labels = c("Ene.", "Abril", "Julio", "Oct.")) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5),
  )


p <- covid_cases %>% 
  # 2020 is a leap year, we could drop Feb 29, 2020 for the sake of 365-day years
  filter(date != as_date("2020-02-29")) %>%
  group_by(year) %>%
  mutate(day_of_year = row_number()) %>%
  ungroup() %>%
  ggplot() +
  # area
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(date) - new_cases_smoothed / 2 * size_factor,
                  ymax = as.POSIXct(date) + new_cases_smoothed / 2 * size_factor,
                  group = year),
              color = outline_color, size = 0.3, fill = fill_color, show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date)),
               col = base_grey, size = 0.3) +
  scale_x_continuous(minor_breaks = month_breaks, 
                     breaks = month_breaks[c(1, 4, 7, 10)],
                     labels = c("Ene.", "Abril", "Julio", "Oct."),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  coord_polar() +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5)
  )
p


# Anotaciones
text_color <- rgb(18, 18, 18, maxColorValue = 255)
base_family <- "Libre Franklin Medium"
# base_family <- "Helvetica"
subtitle_date <- max(covid_cases$date) %>% 
  format("%b. %d, %Y")

# Annotations for the years in a list (used in annotate())
year_annotations <- list(
  year = 2020:2022,
  x = rep(3, 3),
  y = as.POSIXct(paste(2020:2022, "01", "01", sep = "-"))
)

p <- covid_cases %>% 
  # 2020 is a leap year, we could drop Feb 29, 2020 for the sake of 365-day years
  filter(date != as_date("2020-02-29")) %>%
  group_by(year) %>%
  mutate(day_of_year = row_number()) %>%
  ungroup() %>%
  ggplot() +
  # area
  geom_ribbon(aes(x = day_of_year, 
                  ymin = as.POSIXct(date) - new_cases_smoothed / 2 * size_factor,
                  ymax = as.POSIXct(date) + new_cases_smoothed / 2 * size_factor,
                  group = year),
              color = outline_color, size = 0.3, fill = fill_color, show.legend = FALSE) +
  # basic line
  geom_segment(aes(x = day_of_year, xend = day_of_year + 1, 
                   y = as.POSIXct(date), yend = as.POSIXct(date)),
               col = base_grey, size = 0.3) +
  
  # annotation: 7d average
  annotate("richtext", 
           label = "promedio<br>7 días",
           x = 20, y = as.POSIXct("2021-08-01"),
           family = base_family, size = 2, color = text_color,
           label.colour = NA, fill = NA) +
  annotate("segment",
           x = 20, xend = 22.5, 
           y = as.POSIXct("2021-06-01"), yend = as.POSIXct("2021-03-15"),
           color = text_color, size = 0.3) +
  
  # annotation: years
  annotate("text", label = paste0(year_annotations$year, "\u2192"), x = year_annotations$x, 
           y = year_annotations$y, 
           family = "Arial",
           size = 3.0, vjust = -0.6, hjust = 0.15) +   
  
  scale_x_continuous(minor_breaks = month_breaks, 
                     breaks = month_breaks[c(1, 4, 7, 10)],
                     labels = c("Ene.", "Abril", "Julio", "Oct."),
                     limits = c(1, 365),
                     expand = c(0, 0)
  ) +
  #' set the lower limit of the y-axis to a date before 2020 
  #' so that the spiral does not start in the center point
  scale_y_continuous(limits = c(as.POSIXct("2019-07-01"), NA),
                     expand = c(0, 0)) +
  coord_polar() +
  labs(
    subtitle = subtitle_date
  ) +
  theme_void(base_family = base_family) +
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
    panel.grid.major.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    panel.grid.minor.x = element_line(color = "grey70", size = 0.2, linetype = "dotted"),
    axis.text.x = element_text(color = base_grey, size = 5, hjust = 0.5),
    text = element_text(color = text_color),
    plot.subtitle = element_text(hjust = 0.5, size = 7)
  )
p





library(patchwork)

p_legend <- 
  tibble(
    cases = c(0, 3000),
    ymin = c(0, -1500),
    ymax = c(0, 1500),
  ) %>% 
  ggplot(aes(cases)) +
  geom_ribbon(aes(ymin = size_factor * ymin, ymax = size_factor * ymax),
              color = outline_color, fill = fill_color, size = 1.5) +
  geom_line(aes(y = 1), color = base_grey) +
  geom_text(aes(label = ifelse(cases == 0, 0, "3000 casos"), 
                y = 1, hjust = ifelse(cases == 0, 1.5, -0.1)),
            size = 5) +
  coord_cartesian(xlim = c(0, 100000), 
                  ylim = c(-as.numeric(as.POSIXct("1971-01-01")), NA), 
                  clip = "off") + 
  labs(title = "Nuevos casos de COVID,<br>Bolivia") +
  theme_void() +
  theme(plot.title = element_markdown(color = text_color, 
                                      family = "Helvetica",
                                      face = "bold", size = 8, hjust = 0.5,
                                      lineheight = 1.1))

ragg::agg_png(here("plots", "nyt_spiral_with-legend.png"),
              res = 300, width = 1200, height = 1200 * 746/615)
p + inset_element(p_legend, left = 0.05, bottom = 0.725, right = 0.25, top = 0.95)
invisible(dev.off())
