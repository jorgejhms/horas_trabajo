# Define archivo de trabajo
input_file <- here::here("horas_trabajo.csv")

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8")

# Carga de librerías
library(dplyr)
library(lubridate) #manejo de fechas
library(plotly)
library(flexdashboard)
library(janitor)

df <- data.table::fread(input_file, sep = ";")

# Limpieza y calculos iniciales
df <- df |>
  clean_names() |>
  mutate(
    inicio = as.POSIXct(inicio, format = "%d/%m/%Y %H:%M:%S"),
    fin = as.POSIXct(fin, format = "%d/%m/%Y %H:%M:%S"),
    minutos = difftime(fin, inicio, units = "min"),
    horas = (minutos / 60) |> as.numeric(),
    pomodoros = (minutos / 25) |> round() |> as.numeric(),
  )

# Creación de variables para semana
week_start <- floor_date(today(), "week")
week_end <- ceiling_date(today(), "week")

day <- df  |>
  filter(date(inicio) == today())

week <- df |>
    filter(inicio >= week_start & inicio <= week_end)

get_results <- function(df, grouping = NULL) {
  grouping <- enquo(grouping)

  if (is.null(grouping)) {
    df  <- df |>
      summarise(
        minutos = sum(minutos),
        pomodoros = sum(pomodoros),
        horas = sum(horas)
      )
    return(df)
  } else {
    df <- df |>
      group_by(!!grouping) |>
      summarise(
        minutos = sum(minutos),
        pomodoros = sum(pomodoros),
        horas = sum(horas)
      )
    return(df)
  }
}

plot_barras  <- function(df, category, value) {
  category <- enquo(category)
  value    <- enquo(value)

  ggplot(df, aes(!!category, !!value, fill = !!category)) +
    theme_minimal() +
    geom_col(color = "black", size = 0.5) +
    viridis::scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    labs(x = element_blank(),
         y = element_blank(),
         title = element_blank()) +
    theme(legend.position = "none") +
    coord_flip()
}
