# Load packages -----------------------------------------------------------
library(cowplot)
library(ggtext)
library(glue)
library(grid)
library(reactable)
library(showtext)
library(tidyverse)


# Load and clean data ---------------------------------------------------------------
days <- as.character(1:31)
types <- c("H", "H,R", "M", "M,H", "M,H,R", "M,H,R,U", "M,R", "R", "U", "H,R", "U,M", "U,M,H", "U,H,R")

df |> reactable()
df <- 
    readxl::read_excel("2025/2025-01-07/data/raw_data.xlsx") |> 
    mutate(event =  str_trim(event, side = "both")) |> 
    mutate(day = ifelse(event %in% c(days, "11–12"  , "18-19", "31–6", "22–23", "12–13", "26–27", "3–4", "4–5", "10–11", "17–18", "24–25","14–15", "18–22", "28–29", "5–6", "15–16", "6–7", "12–14", "13–14", "20–21", "27–28", "18–19", "7–9", "8–9", "×", "tbc"), event, NA)) |> 
    mutate(month = lubridate::month(date)) |> 
    mutate(day2 = day) |> 
    fill(month, day) |> 
    filter(is.na(day2)) |> 
    select(-c(date, day2)) |> 
    mutate(country_type = case_when(
        str_detect(event, "M,H,R") ~ event,
        str_detect(event, "M,H") ~ event,
        str_detect(event, "H,R") ~ event,
        str_ends(event, "M") ~ event, 
        str_ends(event, "H") ~ event, 
        str_ends(event, "R") ~ event, 
        str_detect(event, "\\bM\\b") ~ event,
        str_detect(event, "\\bU\\b") ~ event,
        TRUE ~ NA
  )) |> 
  mutate(country_type = ifelse(country_type == "Semi Marathon Geoparc M’Goun Azilal", NA, country_type)) |> 
  filter(!is.na(event)) |> 
  fill(country_type, .direction = c("up")) |> 
  filter(event != country_type) |> 
  separate(country_type, into = c("a", "country", "type_note"), sep = "\\b", extra = "merge", fill = "right") |> 
  select(-a) |> 
  mutate(type_note =  str_trim(type_note, side = "both")) |> 
  mutate(type = ifelse(type_note %in% types, type_note, NA)) |> 
    mutate(type = ifelse(is.na(type), str_extract(type_note, "M,H,R"), type)) |> 
    mutate(type = ifelse(is.na(type), str_extract(type_note, "H,R"), type)) |> 
  mutate(type = ifelse(is.na(type), str_extract(type_note, "M,H,U"), type)) |> 
    mutate(type = ifelse(is.na(type), str_extract(type_note, "M,H\\b"), type)) |> 
  mutate(type = ifelse(is.na(type), str_extract(type_note, "M\\b"), type)) |> 
  mutate(type = ifelse(is.na(type), str_extract(type_note, "U\\b"), type)) |> 
  select(-type_note)


# Check for entries expanding to more than one row
# Some marathons are entered more than once with the same or different name, but in most of the cases these are marathons in the same country on the same day, but in different cities.
df |> 
    group_by(month, day, country) |> 
    mutate(n = n()) |> 
    arrange(desc(n), month, day, country) |> 
    reactable()

# From here we will have unique rows for month, day, country and type, we will ignore the event name
df2 <- 
    df |> 
    select(country, month, day, type) |> 
    unique()

# Check for duplicates
df2 |> 
    group_by(country, month, day) |>
    mutate(n = n()) |> 
    arrange(desc(n), month, day, country) |>
    reactable() 

# For the duplicates I will comnbine all types in different rows, so I will have a dataset where I have the date of the event, country and all possible types that day.
# then I drop the cancelled and date not decided events
# if date is in more than one days I take the first day
df3 <- 
    df2 |>
    separate_rows(type, sep = ",\\s*") |>  
    group_by(country, month, day) |>    
    summarize(
        merged_type = paste(unique(type), collapse = ", "), 
        n = n(),                                          
        .groups = "drop"                                 
  ) |> 
    filter(!day %in% c("×", "tbc")) |> 
    separate(day, into = c("day", "day2"), sep = "–") |> 
    mutate(date = make_date(year = 2025, month = month, day = day))

df4 |> reactable()

# for the plot I only take the date and country,and merge countries
df4 <- 
    df3 |> 
    select(date, country) |> 
    unique() |> 
    group_by(date) |>    
    summarize(
        countries = paste(unique(country), collapse = ", "), 
        n = as.character(n()),                                          
        .groups = "drop"                                 
  ) 

# Load fonts --------------------------------------------------------------

font_add_google("Fraunces")
font_add_google("Commissioner")
showtext_auto()
showtext_opts(dpi = 300)

# Define colours and fonts-------------------------------------------------

body_font <- "Commissioner"
title_font <- "Fraunces"

# Data wrangling ----------------------------------------------------------

plot_data <- df4 |>
  group_by(date, n) |>
  mutate(
    n2 = row_number()
  ) |>
  ungroup() |>
  arrange(date, desc(n2)) |>
  mutate(
    month = month(date, label = TRUE, abbr = FALSE),
    dom = mday(date)
  )

plot_data |> arrange(desc(n))
df |> reactable()

# Define text -------------------------------------------------------------

title <- "Marathons and Distance Races, 2025"
st <- glue("In 2025, there are <span style='color:blue'>**{nrow(df4)}**</span> days on which a marathon or a distance race is scheduled.")
cap <- paste0(
  "**Source: Association of International Marathons and Distance Races**"
)


# Plot --------------------------------------------------------------------

p <- 
  ggplot() +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = dom,
      y = 1,
      size = n2
    ),
    colour = "white"
  ) +
  geom_point(
    data = plot_data,
    mapping = aes(
      x = dom,
      y = 1,
      size = n2,
      fill = n
    ),
    alpha = 0.7,
    pch = 21 ) +
  facet_wrap(~month, ncol = 1, drop = FALSE, strip.position = "left") +
  scale_x_continuous(
    breaks = 1:31,
    limits = c(0, 31),
    expand = c(0, 0)
  ) +
  labs(
    x = "",
    y = NULL,
    title = title,
    subtitle = st,
    caption = cap
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = body_font, base_size = 10) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(

    ),
    panel.background = element_rect(
      
      colour = NA
    ),
    strip.text.y.left = element_text(
      family = title_font,
     
      angle = 0
    ),
    strip.background = element_rect(
      
      colour = NA
    ),
    axis.text.y = element_blank(),
    axis.text.x = element_text(
      
      size = rel(0.8)
    ),
    axis.ticks.x = element_line(
      linewidth = 0.5
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(
    
      linewidth = 0.2
    ),
    plot.title = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 5),
      lineheight = 0.5,
      family = title_font,
      face = "bold",
      size = rel(1.4)
    ),
    plot.subtitle = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 5, t = 0),
      family = body_font
    ),
    plot.caption = element_textbox_simple(
      hjust = 0.5,
      halign = 0.5,
      margin = margin(b = 0, t = 0),
      lineheight = 0.5,
      family = body_font
    )
  )

ggdraw(p) +
  draw_text(
    x = 0.54, y = 0.74,
    size = 8,
    family = body_font,
    text = "8\nRaces"
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.52, y1 = 0.74,
      x2 = 0.455, y2 = 0.70,
      curvature = 0.3,
      gp = gpar(lwd = 2),
      arrow = arrow(type = "closed", length = unit(0.05, "inches"))
    )
  ) +
  draw_text(
    x = 0.32, y = 0.18,
    size = 8,
    family = body_font,
    text = "8\nRaces"
  ) +
    draw_grob(
    curveGrob(
      x1 = 0.30, y1 = 0.18,
      x2 = 0.22, y2 = 0.215,
      curvature = -0.3,
      gp = gpar(lwd = 2),
      arrow = arrow(type = "closed", length = unit(0.05, "inches"))
    )
  ) +
  draw_grob(
    curveGrob(
      x1 = 0.34, y1 = 0.18,
      x2 = 0.42, y2 = 0.215,
      curvature = 0.3,
      gp = gpar(lwd = 2),
      arrow = arrow(type = "closed", length = unit(0.05, "inches"))
    )
  )



