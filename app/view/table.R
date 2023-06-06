box::use(
    shiny[div, moduleServer, NS, reactive],
    dplyr[`%>%`, arrange, between, desc, filter, group_by, summarise, ungroup],
    lubridate[ymd],
    reactable[colDef, reactable, reactableOutput, renderReactable, reactableTheme],
    htmltools[div],
)

ui  <- function(id) {
    ns  <- NS(id)
    reactableOutput(ns("table"))
}

server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    data_tbl <- reactive({
        dat <- data %>%
          group_by(client)  %>%
          summarise(
            revenue = round(sum(revenue)),
            salary_avg = round(mean(salary_avg))
          ) %>%
          ungroup() %>%
          arrange(desc(revenue))
    })

    bar_chart <- function(
      label,
      width = "100%",
      height = "0.875rem",
      fill = "#00bfc4",
      background = NULL
    ) {
      bar <- div(
        style = list(background = fill, width = width, height = height)
      )

      chart <- div(
        style = list(
          flexGrow = 1,
          marginLeft = "0.375rem",
          background = background
        ),
        bar
      )

      div(
        style = list(display = "flex", alignItems = "center"),
        label,
        chart
      )
    }

    output$table  <- renderReactable({
        dat  <- data_tbl()
        reactable(
          dat,
          defaultSorted = "revenue",
          columns = list(
            client = colDef(
              name = "Client"
            ),
            revenue = colDef(
              name = "Revenue",
              defaultSortOrder = "desc",
              cell = function(value) {
                width <- paste0(value * 100 / max(dat$revenue),"%")
                value <- paste0("$",format(value, big.mark = ","))
                value <- format(value, width = 9, justify = "right")
                bar_chart(value, width = width, fill = "#0971ef")
              },
              align = "left",
              style = list(whiteSpace = "pre")
            ),
            salary_avg = colDef(
              name = "Avg Salary",
              cell = function(value) {
                width <- paste0(value * 100 / max(dat$salary_avg),"%")
                value <- paste0("$",format(value, big.mark = ","))
                value <- format(value, width = 9, justify = "right")
                bar_chart(value, width = width, fill = "#e64a18")
              },
              align = "left",
              style = list(whiteSpace = "pre")
            )
          ),
          theme = reactableTheme(
            color = "white",
            backgroundColor = "#2c2e38"
          )
        )
    })

  })
}
