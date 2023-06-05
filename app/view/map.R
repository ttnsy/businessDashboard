box::use(
    dplyr[`%>%`, between, filter, group_by, summarise, right_join, ungroup],
    lubridate[ymd],
    ggplot2[aes, element_blank, ggplot, geom_sf, scale_fill_continuous, theme],
    shiny[moduleServer, NS, plotOutput, reactive, renderPlot],
    rnaturalearth[ne_countries],
    sf[st_as_sf],
)

ui  <- function(id) {
    ns  <- NS(id)
    plotOutput(ns("map"))
}

server  <- function(id, data) {
    moduleServer(id, function(input, output, session) {
        data_map  <- reactive({
            world <- ne_countries(scale = "medium", returnclass = "sf")
            
            dat  <- data %>%
              filter(between(date, ymd("2019-08-01"), ymd("2020-12-31")))   %>%
              group_by(country) %>%
              summarise(revenue = sum(revenue)) %>%
              ungroup() %>%
              right_join(world, by = c("country" = "name"))
            
            st_as_sf(dat)
        })

        output$map  <- renderPlot({
            data_map() %>%

            ggplot() +
                geom_sf(aes(fill = revenue)) +
                scale_fill_continuous(na.value="white") +
                theme(
                  legend.position = c(0.1,0.1),
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  legend.text = element_blank()
                )
        }) 
    })
}