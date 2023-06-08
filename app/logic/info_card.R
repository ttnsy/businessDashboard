box::use(
    shiny[div, h5, span]
)

infoCard <- function(header = NULL, text = NULL, icon = NULL, number = NULL) { # nolint
  div(
    class = "info-card",
    h5(header),
    span(
      class = "text",
      text
    ),
    span(
      class = "number",
      icon,
      number
    )
  )
}
