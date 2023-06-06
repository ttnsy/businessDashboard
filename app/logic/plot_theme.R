box::use(
  ggplot2[
    aes,
    element_blank,
    element_line,
    element_rect,
    element_text,
    guide_legend,
    margin,
    scale_x_date,
    theme,
    unit
  ],
  elementalist[element_rect_round]
)

guide_legend_def  <- function() {
  guide_legend(
    direction = "horizontal",
    title.position = "right",
    label = FALSE,
    keywidth = 2.5
  )
}

scale_x_date_def  <- function() {
  scale_x_date(
    date_labels = "%Y-%m",
    date_breaks = "2 month",
    expand = c(0, 10)
  ) 
}

theme_def  <- function() {
  theme(
    axis.title = element_blank(),
    axis.text = element_text(
        size = 14,
        color = "white"
    ),
    axis.text.y = element_text(margin = margin(0,5,0,0)),
    axis.line.x = element_line(color = "white"),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.position = "top",
    legend.key = element_rect(fill = "NA"),
    legend.justification = "left",
    legend.margin = margin(0,10,10,1),
    legend.title = element_text(
        size = 14,
        colour = "white"
    ),
    legend.spacing.x = unit(0.2, "lines"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#3c4042"),
    plot.background = element_rect_round(
        fill = "#2c2e38",
        radius = unit(0.8, "lines")
    ),
    plot.margin = unit(c(1,2,2,1), "lines")
  )
}