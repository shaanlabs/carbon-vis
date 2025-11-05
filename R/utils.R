# Utility helpers for the dashboard

# Consistent org palette (fallback)
org_palette <- c(
  "OpenAI" = "#1f77b4",
  "Google" = "#2ca02c",
  "Google DeepMind" = "#17becf",
  "DeepMind" = "#17becf",
  "Meta" = "#9467bd",
  "NVIDIA" = "#ff7f0e",
  "AI21 Labs" = "#8c564b",
  "Anthropic" = "#e377c2",
  "TII UAE" = "#bcbd22",
  "BigScience" = "#7f7f7f",
  "University of Toronto" = "#d62728",
  "Other" = "#2ca02c"
)

# Plotly config: add download image button and responsive behavior
add_plotly_config <- function(p) {
  plotly::config(p,
    displaylogo = FALSE,
    modeBarButtonsToAdd = list("toImage"),
    toImageButtonOptions = list(format = "png", filename = paste0("plot_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  )
}
