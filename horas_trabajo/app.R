# Setup ---------------------------------------------------------------------

library(shiny)
library(shiny.semantic)
library(here)
library(dplyr)
library(lubridate)

Sys.setlocale(category = "LC_ALL", locale = "es_ES.UTF-8" ) # idioma español
input_file <- here::here("horas_trabajo.csv") # base de datos
options("lubridate.week.start" = 1)    # Semana inicia lunes

# Carga de datos ------------------------------------------------------------

db <-
  data.table::fread(input_file, sep = ";") |>
  janitor::clean_names() |>
  dplyr::mutate(
    inicio = inicio |> as.POSIXct(format = "%d/%m/%Y %H:%M:%S"),
    fin = fin |> as.POSIXct(format = "%d/%m/%Y %H:%M:%S")
  )

# Preprocesamiento de datos

db <- db |>
  mutate(minutos = difftime(fin, inicio, units = "min")
         |> as.numeric()
         |> round())

# Funciones ---------------------------------------------------------------


# UI programa ---------------------------------------------------------------
ui <- semanticPage(
  
  grid(
    grid_template(default = list(
      areas = rbind(
        c("header", "header", "header"),
        c("menu", "main", "main"),
        c("menu", "main", "main")
      ),
      rows_height = c("50px", "auto", "100px"),
      cols_width = c("100px", "2fr", "1fr")
    )),
    container_style = "border: 1px solid #f00",
    area_styles = list(header = "background: #0099f9",
                       menu = "border-right: 1px solid #0099f9"),
    
    header = list(
      menu(menu_item("Home")) ,
    h1("Prueba")
    ),
    menu = "menu",
    main = "main"
  ),

  menu(menu_item("Home")),
  title = "Test",
  h1("Prueba"),
  
  card(div(
    class = "content",
    calendar(
      "fecha_inicio",
      type = "date",
      value = today() - 90,
      placeholder = "Escoje fecha de inicio",
    ),
    br(),
    calendar(
      "fecha_fin",
      type = "date",
      value = today(),
      placeholder = "Escoje fecha de inicio"
    ),
  )),
  
  dateRangeInput(
    # Selector de fechas
    inputId = "fecha",
    label = "Fechas",
    start = today() - 90,
    end = today(),
#    min = min(positivos$fecha_resultado),
 # max = fecha_actualizacion
  ),
)

# Server --------------------------------------------------------------------
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)
