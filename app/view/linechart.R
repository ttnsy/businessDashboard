box::use(
  ggplot2[
    aes,
    ggplot,
    geom_line,
    geom_linerange,
    geom_point,
    geom_text,
    guides,
    scale_x_date,
    scale_y_continuous
  ],
  dplyr[`%>%`, mutate, summarise, group_by, ungroup, lag, lead],
  scales[label_number_si, comma],
  shiny[div, NS, moduleServer, reactive, plotOutput, renderPlot]
)

box::use(
  app/logic/plot_theme[guide_legend_def, theme_def]
)

ui  <- function(id) {
  ns  <- NS(id)
  div(
    class = "chart-container",
    plotOutput(ns("linechart"))
  )
}

server  <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    data_sal <- reactive({
      dat <- data %>%
        group_by(date)  %>%
        summarise(
            salary_avg = round(mean(salary_avg), 2),
        ) %>%
        ungroup()

      mean_salary  <- mean(dat$salary_avg)
      max_salary  <- max(dat$salary_avg)

      line_height  <- mean_salary * 0.03
      line_gap  <- mean_salary * 0.01

      dat  <-  dat %>%
        mutate(
            ymin = ifelse(
                lag(salary_avg) > salary_avg | lead(salary_avg) > salary_avg,
                salary_avg - line_height,
                salary_avg
            ),
            ymax = ifelse(
                lag(salary_avg) > salary_avg | lead(salary_avg) > salary_avg,
                salary_avg,
                salary_avg + line_height
            ),
            coor_label = ifelse(
                ymin == salary_avg, ymax + line_gap, ymin - line_gap
            )
        )

      return(dat)
    })

    output$linechart <- renderPlot({
      dat <- data_sal()

      `USD per Hour` <- "#ce4325" # nolint: object_name_linter.
      mean_salary  <- mean(dat$salary_avg)
      max_salary  <- max(dat$salary_avg)

      y_max  <- max_salary + max_salary * 0.1
      y_breaks  <- round((mean_salary / 2) / 1000) * 1000

      ggplot(dat, aes(x = date, y = salary_avg)) +
        geom_line(aes(color = `USD per Hour`), linewidth = 1.2) +
        geom_linerange(
            aes(ymin = ymin, ymax = ymax),
            color = "gray"
        ) +
        geom_point(aes(color = `USD per Hour`), size = 2.2) +
        geom_text(
            aes(y = coor_label, label = comma(salary_avg, prefix = "$")),
            color = "red",
            size = 4,
            fontface = "bold"
        ) +
        guides(
            color = guide_legend_def()
        ) +
        scale_y_continuous(
            labels = label_number_si(),
            expand = c(0, 10),
            breaks = seq(0, y_max, y_breaks),
            limits = c(1500, y_max)
        ) +
        scale_x_date(
            date_labels = "%Y-%m",
            date_breaks = "2 month",
            expand = c(0, 20)
        ) +
        theme_def(panel = FALSE, panel_grid_y = TRUE)
    })
  })
}