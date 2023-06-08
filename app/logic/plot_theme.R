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
  grDevices[rgb]
)

box::use(
  app/logic/utils[...]
)

guide_legend_def  <- function() {
  guide_legend(
    direction = "horizontal",
    title.position = "right",
    label = FALSE,
    keywidth = 2.5
  )
}

theme_def  <- function(
  panel = TRUE,
  legend = TRUE,
  axis = TRUE,
  panel_grid_x = FALSE,
  panel_grid_y = FALSE
) {
  col_grid <- rgb(235, 235, 235, 80, maxColorValue = 255)

  theme(
    axis.title = element_blank(),
    axis.text = if (!axis) element_blank() else element_text(size = 12, color = "white"),
    axis.text.y = if (!axis) element_blank() else element_text(margin = margin(0, 5, 0, 0)),
    axis.line.x = if (!axis) element_blank() else element_line(color = "white"),
    axis.ticks = element_blank(),
    legend.background = element_blank(),
    legend.position = if (!legend) "none" else "top",
    legend.key = element_rect(fill = "NA"),
    legend.justification = "left",
    legend.margin = margin(0, 10, 5, 1),
    legend.title = element_text(
        size = 12,
        colour = "white"
    ),
    legend.spacing.x = unit(0.2, "lines"),
    panel.grid.major.x = if (!panel_grid_x) element_blank() else element_line(color = col_grid),
    panel.grid.minor.x = if (!panel_grid_x) element_blank() else element_line(color = col_grid),
    panel.grid.major.y = if (!panel_grid_y) element_blank() else element_line(color = col_grid),
    panel.grid.minor.y = if (!panel_grid_y) element_blank() else element_line(color = col_grid),
    panel.background = if (!panel) element_blank() else element_rect(fill = darkteal),
    plot.background = element_rect(
        fill = darkteal2,
        color = NA,
    ),
    plot.margin = unit(c(1, 2, 11, 1), "lines"),
    panel.margin = unit(c(0, 0, 0, 0), "cm")
  )
}
