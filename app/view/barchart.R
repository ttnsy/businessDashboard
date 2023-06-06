box::use(
    ggplot2[
        aes,
        element_blank,
        element_line,
        element_rect,
        element_text,
        ggplot,
        geom_col,
        geom_hline,
        geom_linerange,
        geom_text,
        guides,
        guide_legend,
        margin,
        scale_fill_manual,
        scale_y_continuous,
        scale_x_date,
        theme,
        unit
    ],
    dplyr[`%>%`, mutate, summarise, group_by, ungroup],
    elementalist[element_rect_round],
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
                group_by(date)  %>%
                summarise(
                     revenue = sum(revenue)
                ) %>%
                ungroup() %>%
                mutate(
                    ymax = revenue + 15000,
                ) 
        })

        output$barchart <- renderPlot({
            dat <- data_rev()

            blue  <- "#0971ef"
            `Monthly Goal` <- "#ff7345"
            `Final Revenue` <- "#63b5f6"

            break_y  <- 52000
            max_y  <- max(dat$revenue) + break_y
            legend_props  <- guide_legend(
                direction = "horizontal",
                title.position = "right",
                label = FALSE,
                keywidth = 2.5
            )
            ggplot(dat, aes(x = date, y = revenue, fill = `Final Revenue`)) +
                geom_col(width = 20) +
                geom_linerange(aes(ymin = revenue, ymax = ymax), color = "gray") +
                geom_hline(aes(yintercept = 300000, color = `Monthly Goal`), linewidth = 1.5) +
                geom_text(
                    aes(label = comma(revenue, prefix = "$")),
                    vjust = -2,
                    size = 4,
                    color = blue
                ) +
                guides(
                    fill = legend_props,
                    color = legend_props
                ) +
                scale_fill_manual(
                    values = `Final Revenue`
                ) +
                scale_x_date(
                    date_labels = "%Y-%m",
                    date_breaks = "2 month",
                    expand = c(0,10)
                ) +
                scale_y_continuous(
                    labels = label_number_si(),
                    expand = c(0, 10),
                    breaks = seq(0, max_y, 52000),
                    limits = c(0, max_y)
                ) +
                theme(
                    axis.title = element_blank(),
                    axis.text = element_text(size = 14, color = "white"),
                    axis.text.y = element_text(margin = margin(0,5,0,0)),
                    axis.line.x = element_line(color = "white"),
                    axis.ticks = element_blank(),
                    legend.background = element_blank(),
                    legend.position = "top",
                    legend.key = element_rect(fill = "NA"),
                    legend.justification = "left",
                    legend.margin = margin(0,10,10,1),
                    legend.title = element_text(size = 14, colour = "white"),
                    legend.spacing.x = unit(0.2, "lines"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "#3c4042"),
                    plot.background = element_rect_round(
                        fill = "#2c2e38", radius = unit(0.8, "lines")
                    ),
                    plot.margin = unit(c(1,2,2,1), "lines")
                )
        })
    })
}