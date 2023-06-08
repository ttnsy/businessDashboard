box::use(
    dplyr[`%>%`, group_by, summarise, ungroup],
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

box::use(
  app/logic/utils[...]
)

ui  <- function(id) {
    ns  <- NS(id)
    div(
      class = "map-container",
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
              e_visual_map(revenue, inRange = list(color = c(blue, lightblue2))) %>%
              e_color(background = darkteal2)
        })
    })
}
