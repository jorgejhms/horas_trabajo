#' @export
get_results <- function(df, grouping = NULL) {
  # Imports
  box::use(dplyr[enquo, summarise, group_by])
  
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

#' @export
plot_trabajos   <-  function(df, category, value, flip = F) {
  # Imports
  box::use(dplyr[enquo, summarise, group_by],
           stats[reorder],
           ggplot2[...],
           viridis[scale_fill_viridis])
  
  # Enquo variables
  category <- enquo(category)
  value    <- enquo(value)
  
  # Prepare df
  df <- df |>
    group_by(!!category) |>
    summarise(pomodoros = sum(pomodoros))
  
  g <- ggplot(df, aes(reorder(!!category, !!value),
                      !!value,
                      fill = !!category)) +
    theme_minimal() +
    geom_col(color = "black", size = 0.5) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    labs(x = element_blank(),
         y = element_blank(),
         title = element_blank()) +
    theme(legend.position = "none")
  
  if (flip) {
    g <- g + coord_flip()
    return(g)
  } else {
    return(g)
  }
}

#' @export
graph_diario <- function(df, category, value, dias) {
  # Imports
  box::use(dplyr[filter, enquo],
           lubridate[date, today],
           ggplot2[...])
  
  # Genera equo
  category <- enquo(category)
  value    <- enquo(value)
  
  # Filtra dataframe
  df <- df |> filter(date(inicio) >= today() - dias)
  
  # Genera gráficos
  g <- ggplot(df, aes(!!category, !!value)) +
    theme_minimal() +
    geom_col() +
    labs(x = element_blank(),
         y = element_blank(),
         title = element_blank()) +
    geom_hline(yintercept = 8,
               # Límite menor
               linetype = "dashed",
               color = "green3") +
    geom_hline(yintercept = 12,
               # Límite mayor
               linetype = "dashed",
               color = "red")
  return(g)
  
}

#' @export
graph_promedio_dias <- function(df){
  # Imports
  box::use(
    dplyr[group_by, ungroup, summarise, select, rename],
    lubridate[date, week, year, wday]
  )
  
  # Genera totales de horas trabajadas en cada día
  test <- df |>
    group_by(date(inicio), week(inicio), year(inicio)) |>
    summarise(pomodoros = sum(pomodoros)) |>
    ungroup() |>
    select(`date(inicio)`, pomodoros) |>
    rename(inicio = `date(inicio)`)
}


