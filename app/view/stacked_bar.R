box::use(
    ggplot2[
        aes,
        element_text,
        element_blank,
        ggplot,
        geom_col,
        geom_text,
        annotate,
        margin,
        scale_x_reverse,
        scale_fill_manual,
        theme
    ],
    dplyr[`%>%`, mutate, summarise, filter, group_by, ungroup, between, n],
    scales[label_number_si, comma],
    stringr[str_to_title],
    glue[glue],
    lubridate[ymd],
    shiny[NS, moduleServer, reactive, plotOutput, renderPlot]
)

box::use(
  app/logic/plot_theme[theme_def]
)

ui  <- function(id) {
    ns  <- NS(id)
    plotOutput(ns("stacked_bar"))
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

            lightblue <- "#63b5f6"
            lightestblue <- "#97c5fa"
            blue  <- "#0971ef"

            ggplot(dat, aes(y = cat, x = perc, fill = payment)) +
                geom_col(color = "white") +
                scale_fill_manual(values = c(blue, lightblue, lightestblue)) +
                scale_x_reverse() +
                geom_text(aes(x = coord_text, label = payment), size = 12) +
                theme_def(axis = FALSE, panel = FALSE, legend = FALSE)
        })
    })
}