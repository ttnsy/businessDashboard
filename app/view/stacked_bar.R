box::use(
    ggplot2[
        aes,
        ggplot,
        geom_col,
        geom_text,
        margin,
        scale_x_reverse,
        scale_fill_manual
    ],
    dplyr[`%>%`, mutate, summarise, group_by, ungroup, n],
    scales[label_number_si, comma],
    stringr[str_to_title],
    shiny[div, NS, moduleServer, reactive, plotOutput, renderPlot]
)

box::use(
  app/logic/utils[...],
  app/logic/plot_theme[theme_def]
)

ui  <- function(id) {
    ns  <- NS(id)
    div(
      class = "chart-container",
      plotOutput(ns("stacked_bar"))
    )
}

server  <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        data_pay <- reactive({
            data %>%
                group_by(payment)  %>%
                summarise(
                     n = n(),
                ) %>%
                ungroup() %>%
                mutate(
                    payment = str_to_title(payment),
                    perc = n / sum(n),
                    coord_text = c(0.725, 0.28, 0.065)
                ) %>%
                mutate(cat = "payment")
        })

        output$stacked_bar <- renderPlot({
            dat <- data_pay()

            ggplot(dat, aes(y = cat, x = perc, fill = payment)) +
                geom_col(color = "white") +
                scale_fill_manual(values = c(blue, lightblue, lightblue2)) +
                scale_x_reverse() +
                geom_text(aes(x = coord_text, label = payment), size = 6) +
                theme_def(axis = FALSE, panel = FALSE, legend = FALSE)
        })
    })
}
