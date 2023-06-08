box::use(
    shiny[div, moduleServer, NS, reactive],
    dplyr[`%>%`, arrange, desc, group_by, summarise, ungroup],
    reactable[colDef, reactable, reactableOutput, renderReactable, reactableTheme],
    htmltools[div],
)

box::use(
  app/logic/utils[...]
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
      fill = blue,
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
                width <- paste0(value * 100 / max(dat$revenue), "%")
                value <- paste0("$", format(value, big.mark = ","))
                value <- format(value, width = 9, justify = "right")
                bar_chart(value, width = width)
              },
              align = "left",
              style = list(whiteSpace = "pre")
            ),
            salary_avg = colDef(
              name = "Avg Salary",
              cell = function(value) {
                width <- paste0(value * 100 / max(dat$salary_avg), "%")
                value <- paste0("$", format(value, big.mark = ","))
                value <- format(value, width = 9, justify = "right")
                bar_chart(value, width = width, fill = red)
              },
              align = "left",
              style = list(whiteSpace = "pre")
            )
          ),
          theme = reactableTheme(
            borderColor = "gray",
            color = "white",
            backgroundColor = darkteal2,
            headerStyle = list(
              background = darkteal3,
              borderColor = darkteal3
            )
          )
        )
    })
  })
}
