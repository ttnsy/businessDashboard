box::use(
    shiny[div, NS, icon]
)

box::use(
  app/logic/info_card[infoCard]
)

ui  <- function(id) {
    ns  <- NS(id)
    div(
        class = "info-card-container",
        infoCard(
            "Revenue",
            "$2,983,257",
            icon("arrow-up"),
            "0.1%"
        ),
        infoCard(
            "Work Orders",
            "69",
            icon("arrow-down"),
            "-32.4%"
        ),
        infoCard(
            "Employees",
            "1,191",
            icon("arrow-down"),
            "-13.0%"
        ),
        infoCard(
            "Avg Salary",
            "$2,505.89",
            icon("arrow-up"),
            "15.1%"
        )
    )
}

server  <- function (id) {
    moduleServer(id, function(input, output, session) {
        
    })
}