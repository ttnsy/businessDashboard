box::use(
    shiny[div, h5, span]
)

infoCard <- function(header = NULL, text = NULL, numberIcon = NULL, number = NULL) {
  div(
    class = "info-card",
    h5(header),
    span(
      class = "text",
      text
    ),
    span(
      class = "number",
      numberIcon,
      number
    )
  )
}