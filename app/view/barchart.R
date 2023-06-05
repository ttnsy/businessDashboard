box::use(
    ggplot2[
        aes,
        element_text,
        element_blank,
        ggplot,
        geom_col,
        geom_hline,
        geom_linerange,
        geom_text,
        margin,
        scale_y_continuous,
        scale_x_date,
        theme
    ],
    dplyr[`%>%`, mutate, summarise, filter, group_by, between, ungroup],
    scales[label_number_si, comma],
    glue[glue],
    lubridate[ymd],
    shiny[NS, moduleServer, reactive, plotOutput, renderPlot]
)

ui  <- function(id) {
    ns  <- NS(id)
    plotOutput(ns("barchart"))
}

server  <- function (id, data) {
    moduleServer(id, function(input, output, session) {
        data_rev <- reactive({
            data %>%
                filter(between(date, ymd("2019-08-01"), ymd("2020-12-31"))) %>%
                group_by(date)  %>%
                summarise(
                     revenue = sum(revenue)
                ) %>%
                mutate(
                    ymax = revenue + 10000
                ) %>%
                ungroup()
        })

        output$barchart <- renderPlot({
            dat <- data_rev()

            blue  <- "#0971ef"
            orange <- "#ff7345"
            lightblue <- "#63b5f6"
            
            ggplot(dat, aes(x = date, y = revenue)) +
                geom_col(fill = lightblue) +
                geom_linerange(aes(ymin = revenue, ymax = ymax), color = "gray") +
                geom_hline(yintercept = 300000, color = orange, size = 1.5) +
                geom_text(aes(label = comma(revenue, prefix = "$")), vjust = -1.2, color = blue) +
                scale_y_continuous(labels = label_number_si(), expand = c(0,10)) +
                scale_x_date(expand = c(0,10)) +
                theme(
                    axis.title = element_blank(),
                    axis.text.y = element_text(margin = margin(0,5,0,0)),
                    axis.ticks = element_blank(),
                    
                )
        })
    })
}