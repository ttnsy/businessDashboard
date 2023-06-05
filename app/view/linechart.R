box::use(
    ggplot2[
        aes,
        element_text,
        element_blank,
        ggplot,
        geom_line,
        geom_linerange,
        geom_point,
        geom_text,
        margin,
        scale_y_continuous,
        scale_x_date,
        theme
    ],
    dplyr[`%>%`, mutate, summarise, filter, group_by, between],
    scales[label_number_si, comma],
    glue[glue],
    lubridate[ymd],
    shiny[NS, moduleServer, reactive, plotOutput, renderPlot]
)

ui  <- function(id) {
    ns  <- NS(id)
    plotOutput(ns("linechart"))
}

server  <- function (id, data) {
    moduleServer(id, function(input, output, session) {
        data_sal <- reactive({
            dat  <- data %>%
                filter(between(date, ymd("2019-08-01"), ymd("2020-12-31"))) %>%
                group_by(date)  %>%
                summarise(
                    salary_avg = round(mean(salary_avg),2),
                    ymax = salary_avg + 20
                )
            return(dat)
        })

        output$linechart <- renderPlot({
            dat <- data_sal()
            
            orange <- "#ff7345"
            
            ggplot(dat, aes(x = date, y = salary_avg)) +
                geom_line(color = orange, size = 1.2)+
                geom_point(color = orange, size = 2.2) +
                geom_linerange(aes(ymin = salary_avg, ymax = ymax), color = "gray") +
                geom_text(aes(label = comma(salary_avg, prefix = "$")), color = orange, vjust = -2.5) +
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