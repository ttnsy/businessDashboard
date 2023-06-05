box::use(
    ggplot2[
        aes,
        element_text,
        element_blank,
        ggplot,
        geom_col,
        annotate,
        margin,
        scale_x_reverse,
        scale_fill_manual,
        theme
    ],
    dplyr[`%>%`, mutate, summarise, filter, group_by, ungroup, between, n],
    scales[label_number_si, comma],
    glue[glue],
    lubridate[ymd],
    shiny[NS, moduleServer, reactive, plotOutput, renderPlot]
)

ui  <- function(id) {
    ns  <- NS(id)
    plotOutput(ns("stacked_bar"))
}

server  <- function (id, data) {
    moduleServer(id, function(input, output, session) {
        data_pay <- reactive({
            data %>%
                filter(between(date, ymd("2019-08-01"), ymd("2020-12-31"))) %>%
                group_by(payment)  %>%
                summarise(
                     n = n()
                ) %>%
                ungroup() %>%
                mutate(cat = "payment")
        })

        output$stacked_bar <- renderPlot({
            dat <- data_pay()

            lightblue <- "#63b5f6"
            lightestblue <- "#97c5fa"
            blue  <- "#0971ef"

            ggplot(dat, aes(x = n, y = cat, fill = payment)) + 
                geom_col(color = "white") + 
                scale_fill_manual(values = c(blue, lightblue, lightestblue)) +
                scale_x_reverse() +
                annotate("text", label = "Cash", x = 400, y = 1) +
                annotate("text", label = "Fiverr", x = 200, y = 1) +
                annotate("text", label = "PayPal", x = 55, y = 1) +
                theme(
                    axis.title = element_blank(),
                    axis.text = element_blank(),
                    legend.position = "none"
                )
        })
    })
}