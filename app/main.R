box::use(
  dplyr[`%>%`, between, filter, mutate],
  shiny[div, moduleServer, NS, h1, dateRangeInput, fluidPage, icon],
  shinyWidgets[airDatepickerInput],
  lubridate[ymd],
  bslib[bs_theme, font_google],
  imola[flexPage]
)

box::use(
  app/view/summary,
  app/view/barchart,
  app/view/linechart,
  app/view/stacked_bar,
  app/view/map,
  app/view/table
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  flexPage(
    template = "one-three-alternate",
    div(
      class = "header",
      h1(
        class = "title",
        "Business Summary"
      ),
      airDatepickerInput(
        "daterange",
        range = TRUE,
        minDate = ymd("2019-01-01"),
        maxDate = ymd("2020-12-31")
      )
    ),
    div(
      class = "main-left",
      summary$ui(ns("info_card")),
      barchart$ui(ns("barchart")),
      linechart$ui(ns("linechart")),
      stacked_bar$ui(ns("stacked_bar"))
    ),
    div(
      class = "main-right",
      map$ui(ns("map")),
      table$ui(ns("table"))
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    options(scipen = 99)

    data  <- utils::read.csv("app/data/mock_data.csv")
    data  <- data %>%
      mutate(date = ymd(ymd(data$date)))  %>%
      filter(between(date, ymd("2019-08-01"), ymd("2020-06-01")))

    barchart$server("barchart", data)
    linechart$server("linechart", data)
    stacked_bar$server("stacked_bar", data)
    map$server("map", data)
    table$server("table", data)
  })
}
