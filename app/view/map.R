box::use(
    dplyr[`%>%`, between, filter, group_by, summarise, right_join, ungroup],
    lubridate[ymd],
    ggplot2[aes, coord_sf, element_blank, ggplot, geom_sf, 
    scale_x_continuous, scale_y_continuous, scale_fill_continuous, theme],
    shiny[moduleServer, NS, plotOutput, reactive, renderPlot],
    rnaturalearth[ne_countries],
    sf[st_as_sf],
)

box::use(
  app/logic/plot_theme[theme_def]
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
            dat <- data_map()

            ggplot(dat) +
                geom_sf(aes(fill = revenue)) +
                coord_sf(datum = NA) +
                scale_fill_continuous(high = "#0971ef", low = "#97c5fa", na.value = "white") +
                scale_x_continuous(limits = c(-180, 180))+
                scale_y_continuous(limits = c(-85, 85))+
                theme_def(panel = FALSE, axis = FALSE) +
                theme(
                  legend.position = c(0.1, 0.1),
                  legend.direction = "horizontal",
                  legend.title = element_blank(),
                  legend.text = element_blank(),
                  plot.margin=grid::unit(c(0,0,0,0), "mm")
                )
        })
    })
}