box::use(
  ggplot2[
    aes,
    ggplot,
    geom_line,
    geom_linerange,
    geom_point,
    geom_text,
    guides,
    margin,
    scale_y_continuous
  ],
  dplyr[`%>%`, mutate, summarise, filter, group_by, between],
  scales[label_number_si, comma],
  glue[glue],
  lubridate[ymd],
  shiny[NS, moduleServer, reactive, plotOutput, renderPlot]
)

box::use(
  app/logic/plot_theme[guide_legend_def, scale_x_date_def, theme_def]
)

ui  <- function(id) {
  ns  <- NS(id)
  plotOutput(ns("linechart"))
}

server  <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    data_sal <- reactive({
      dat  <- data %>%
        group_by(date)  %>%
        summarise(
            salary_avg = round(mean(salary_avg), 2),
            ymax = salary_avg + 20
      )
      return(dat)
    })

    output$linechart <- renderPlot({
      dat <- data_sal()

      `USD per Hour` <- "#ff7345" # nolint: object_name_linter.

      ggplot(dat, aes(x = date, y = salary_avg)) +
        geom_line(aes(color = `USD per Hour`), linewidth = 1.2) +
        geom_point(aes(color = `USD per Hour`), size = 2.2) +
        geom_linerange(
            aes(ymin = salary_avg, ymax = ymax),
            color = "gray"
        ) +
        geom_text(
            aes(label = comma(salary_avg, prefix = "$")),
            color = "red",
            vjust = -2.5,
        ) +
        guides(
            color = guide_legend_def()
        ) +
        scale_y_continuous(labels = label_number_si(), expand = c(0, 10)) +
        scale_x_date_def() +
        theme_def()
    })
  })
}