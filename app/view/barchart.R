box::use(
  ggplot2[
    aes,
    ggplot,
    geom_col,
    geom_hline,
    geom_linerange,
    geom_text,
    guides,
    guide_legend,
    margin,
    scale_fill_manual,
    scale_x_date,
    scale_y_continuous,
    unit
  ],
  plotly[ggplotly],
  dplyr[`%>%`, mutate, summarise, group_by, ungroup],
  elementalist[element_rect_round],
  scales[label_number_si, comma],
  glue[glue],
  lubridate[ymd],
  shiny[div,NS, moduleServer, reactive, plotOutput, renderPlot]
)

box::use(
  app/logic/plot_theme[guide_legend_def, theme_def]
)

ui  <- function(id) {
  ns  <- NS(id)
  div(
    class = "chart-container",
    plotOutput(ns("barchart"))
  )
}

server  <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    data_rev <- reactive({
        dat  <- data %>%
            group_by(date)  %>%
            summarise(
                    revenue = sum(revenue)
            ) %>%
            ungroup()

        line_height  <- mean(dat$revenue) * 0.06
        line_gap  <- mean(dat$revenue) * 0.035

        dat %>%
          mutate(
            ymax = revenue + line_height,
            coor_label = ymax + line_gap
          )
    })

    output$barchart <- renderPlot({
        dat <- data_rev()

        blue  <- "#0971ef"
        `Monthly Goal` <- "#ff7345" # nolint: object_name_linter.
        `Final Revenue` <- "#63b5f6" # nolint: object_name_linter.

        y_break  <- 52000
        y_max  <- max(dat$revenue) + y_break

        ggplot(dat, aes(x = date, y = revenue, fill = `Final Revenue`)) +
          geom_col(width = 20) +
          geom_linerange(aes(ymin = revenue, ymax = ymax), color = "gray") +
          geom_hline(aes(yintercept = 300000, color = `Monthly Goal`), linewidth = 1.5) +
          geom_text(
            aes(y = coor_label, label = comma(revenue, prefix = "$")),
            size = 4,
            color = blue,
            fontface = "bold",
          ) +
          guides(
            fill = guide_legend_def(),
            color = guide_legend_def()
          ) +
          scale_fill_manual(
            values = `Final Revenue`
          ) +
          scale_x_date(
            date_labels = "%Y-%m",
            date_breaks = "2 month",
            expand = c(0, 10)
          ) +
          scale_y_continuous(
            labels = label_number_si(),
            expand = c(0, 10),
            breaks = seq(0, y_max, y_break),
            limits = c(0, y_max)
          ) +
          theme_def()
    })
  })
}