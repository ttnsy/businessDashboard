box::use(
    dplyr[`%>%`, between, filter, group_by, summarise, right_join, ungroup],
    lubridate[ymd],
    echarts4r[
        echarts4rOutput,
        e_charts,
        e_color,
        e_map,
        e_visual_map,
        renderEcharts4r
    ],
    shiny[div, moduleServer, NS, plotOutput, reactive, renderPlot]
)

ui  <- function(id) {
    ns  <- NS(id)
    div(
      class="map-container",
      echarts4rOutput(ns("map"))
    )
}

server  <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        data_map  <- reactive({
            data %>%
              group_by(country) %>%
              summarise(revenue = sum(revenue)) %>%
              ungroup()
        })

        output$map  <- renderEcharts4r({
            dat <- data_map()

            dat %>%
              e_charts(country)  %>%
              e_map(revenue, center = list(0, 0))  %>%
              e_visual_map(revenue, inRange = list(color = c("#0971ef", "#97c5fa"))) %>%
              e_color(background = "#2c2e38")
        })
    })
}