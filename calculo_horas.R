library(dplyr)

data <- data.table::fread("horas_trabajo.csv", sep = ";") |>
  janitor::clean_names() |>
  mutate(
    inicio = as.POSIXct(inicio, format = "%d/%m/%Y %H:%M"),
    fin = as.POSIXct(fin, format = "%d/%m/%Y %H:%M")
  )


trabajos <- data |>
  mutate(fecha = as.Date(inicio)) |>
  group_by(trabajo) |>
  summarise(horas = sum(difftime(fin, inicio, units = "hours")))
