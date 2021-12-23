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

plot_barras   <-  function(df, category, value) {
  # Imports
  box::use(dplyr[enquo, summarise],
           ggplot2[...],
           viridis[scale_fill_viridis])
  
  category <- enquo(category)
  value    <- enquo(value)
  
  ggplot(df, aes(!!category, !!value, fill = !!category)) +
    theme_minimal() +
    geom_col(color = "black", size = 0.5) +
    scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
    labs(x = element_blank(),
         y = element_blank(),
         title = element_blank()) +
    theme(legend.position = "none")
}