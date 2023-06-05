box::use(
    shiny[div, moduleServer, NS, reactive],
    dplyr[`%>%`, arrange, between, desc, filter, group_by, summarise, ungroup],
    lubridate[ymd],
    reactable[reactable, reactableOutput, renderReactable]
)

ui  <- function(id) {
    ns  <- NS(id)
    reactableOutput(ns("table"))
}

server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    data_tbl <- reactive({
        data %>%
          filter(between(date, ymd("2019-08-01"), ymd("2020-12-31"))) %>%
          group_by(client)  %>%
          summarise(
            revenue = round(sum(revenue),2),
            salary_avg = round(mean(salary_avg),2)
          ) %>%
          ungroup() %>%
          arrange(desc(revenue))
    })

    output$table  <- renderReactable({
        dat  <- data_tbl()
        reactable(dat)
    })

  })
}
