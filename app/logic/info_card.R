box::use(
    shiny[div, h5, span]
)

infoCard <- function(header = NULL, text = NULL, numberIcon = NULL, number = NULL) {
  div(
    class = "info-card",
    h5(
      class = "card-header",
      header
    ),
    span(
      class = "card-text",
      text
    ),
    span(
      class = "card-number", 
      numberIcon,
      number
    )
  )
}