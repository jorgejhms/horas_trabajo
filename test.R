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
