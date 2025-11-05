suppressPackageStartupMessages({
  source("R/packages.R"); setup_packages()
  source("R/visualization.R")
  source("R/data_prep.R")
  source("R/utils.R")
})

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dplyr)
library(readr)
library(ggplot2)
library(scales)
library(DT)
library(bslib)
library(ggcorrplot)
library(tidyr)

# ---- Data loading helpers ----
safe_read <- function(path) {
  if (file.exists(path)) readr::read_csv(path, show_col_types = FALSE) else NULL
}

load_all_data <- function() {
  # Create derived CSVs if they are missing
  ensure_datasets()
  base <- safe_read(file.path("data", "ai_carbon_footprint.csv"))
  model_size <- safe_read(file.path("data", "model_size.csv"))
  emissions <- safe_read(file.path("data", "carbon_footprint.csv"))
  efficiency <- safe_read(file.path("data", "efficiency.csv"))

  if (is.null(base)) {
    # best-effort merge of separate files
    if (!is.null(model_size) && !is.null(emissions)) {
      df <- model_size %>% left_join(emissions, by = c("Model", "Year"))
    } else {
      stop("No data found in data/ directory")
    }
  } else {
    df <- base
  }

  # If efficiency not present, compute
  if (!"CO2_per_Parameter" %in% names(df) && all(c("CO2_Tons","Parameters_Billion") %in% names(df))) {
    df$CO2_per_Parameter <- round(df$CO2_Tons / df$Parameters_Billion, 2)
  }
  if (!"Energy_Efficiency" %in% names(df) && all(c("Parameters_Billion","EnergyUsed_MWh") %in% names(df))) {
    df$Energy_Efficiency <- round(df$Parameters_Billion / df$EnergyUsed_MWh, 4)
  }

  df <- df %>% tidyr::replace_na(list(CO2_Tons = 0, EnergyUsed_MWh = 0))
  df
}

data_updated_on <- function() {
  f <- file.path("data", "ai_carbon_footprint.csv")
  if (file.exists(f)) format(file.info(f)$mtime, "%Y-%m-%d %H:%M") else "unknown"
}

# ---- UI ----
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = span(icon("leaf"), "CO2lytics: AI Energy & Carbon Intelligence (2017–2025)")),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("house")),
      menuItem("Carbon Emission Trend", tabName = "emissions", icon = icon("chart-line")),
      menuItem("Model Efficiency", tabName = "efficiency", icon = icon("bullseye")),
      menuItem("Energy vs Model Size", tabName = "size_energy", icon = icon("circle")),
      menuItem("Organization Comparison", tabName = "org", icon = icon("building")),
      menuItem("Distribution Insights", tabName = "dist", icon = icon("chart-bar")),
      menuItem("Dataset Explorer", tabName = "explorer", icon = icon("table"))
    ),
    hr(),
    div(class = "footer-note", paste("Built in R Shiny (Windsurf)"))
  ),
  dashboardBody(
    theme = bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Inter")),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tabItems(
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("vb_models", width = 3),
          valueBoxOutput("vb_co2", width = 3),
          valueBoxOutput("vb_energy", width = 3),
          valueBoxOutput("vb_largest", width = 3)
        ),
        fluidRow(
          boxPlus(width = 12, title = "About", status = "primary", solidHeader = TRUE,
              p("This dashboard analyzes AI model energy usage and carbon emissions from 2017 to 2025."),
              p("Use filters to explore by year and organization. Charts are interactive and exportable."),
              p("Credits: Dataset within this project; organizations include OpenAI, Google DeepMind, Meta, NVIDIA, etc."))
        ),
        fluidRow(
          boxPlus(width = 12, title = "Model Releases Timeline", status = "primary", solidHeader = TRUE,
              plotlyOutput("timeline_bar", height = 280))
        )
      ),
      tabItem(tabName = "emissions",
        fluidRow(
          boxPlus(width = 12, title = tagList(icon("chart-line"), "Growth of AI Carbon Emissions (2017–2025)"), status = "primary", solidHeader = TRUE,
              plotlyOutput("emissions_plot", height = 480),
              plotlyOutput("emissions_anim", height = 420),
              downloadButton("dl_emissions_png", "Download PNG")),
          boxPlus(width = 12, title = "Insights", status = "info",
              textOutput("ins_emissions"))
        )
      ),
      tabItem(tabName = "efficiency",
        fluidRow(
          boxPlus(width = 12, title = tagList(icon("bullseye"), "AI Model Training Efficiency Over Time"), status = "primary", solidHeader = TRUE,
              plotlyOutput("efficiency_plot", height = 480),
              downloadButton("dl_efficiency_png", "Download PNG")),
          boxPlus(width = 12, title = "Insights", status = "info",
              textOutput("ins_efficiency"))
        )
      ),
      tabItem(tabName = "size_energy",
        fluidRow(
          boxPlus(width = 12, title = tagList(icon("circle"), "AI Model Size vs Energy Used"), status = "primary", solidHeader = TRUE,
              plotlyOutput("size_energy_plot", height = 500),
              downloadButton("dl_size_png", "Download PNG")),
          boxPlus(width = 12, title = "Insights", status = "info",
              textOutput("ins_size"))
        )
      ),
      tabItem(tabName = "org",
        fluidRow(
          boxPlus(width = 12, title = "Total Emissions by Organization", status = "primary", solidHeader = TRUE,
              plotlyOutput("org_bar", height = 460),
              downloadButton("dl_org_png", "Download PNG"))
        ),
        fluidRow(
          boxPlus(width = 6, title = "Efficiency by Organization (Box)", status = "primary", solidHeader = TRUE,
              plotlyOutput("eff_by_org", height = 360)),
          boxPlus(width = 6, title = "Top 5 Most Carbon-Intensive Labs", status = "warning", solidHeader = TRUE,
              reactable::reactableOutput("top5_table"))
        )
      ),
      tabItem(tabName = "dist",
        fluidRow(
          boxPlus(width = 6, title = "CO₂ Emissions Distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("hist_co2", height = 360)),
          boxPlus(width = 6, title = "Energy Used by Organization (Boxplot)", status = "primary", solidHeader = TRUE,
              plotlyOutput("box_energy", height = 360))
        ),
        fluidRow(
          boxPlus(width = 6, title = "Proportion of Total Emissions by Organization", status = "primary", solidHeader = TRUE,
              plotlyOutput("pie_org", height = 360)),
          boxPlus(width = 6, title = "Correlation: Size, Energy, CO₂", status = "primary", solidHeader = TRUE,
              plotOutput("corr_plot", height = 360))
        )
      ),
      tabItem(tabName = "explorer",
        fluidRow(
          boxPlus(width = 12, title = "Dataset Explorer", status = "primary", solidHeader = TRUE,
              reactable::reactableOutput("table"),
              br(),
              downloadButton("download_csv", "Download Filtered CSV"))
        )
      )
    )
  )
  , rightsidebar = rightSidebar(
      h4("Global Filters"),
      uiOutput("year_range_ui"),
      uiOutput("org_select_ui"),
      checkboxInput("show_labels", "Show model labels", value = TRUE),
      div(class = "footer-note", textOutput("data_updated")),
      width = 250
    )
)

# ---- Server ----
server <- function(input, output, session) {
  df_all <- reactiveVal(load_all_data())

  # Filters
  output$year_range_ui <- renderUI({
    df <- df_all()
    sliderInput("year_range", "Year range", min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE),
                value = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)), step = 1, sep = "")
  })
  output$org_select_ui <- renderUI({
    df <- df_all()
    orgs <- sort(unique(df$Organization))
    selectInput("orgs", "Organization", choices = orgs, selected = orgs, multiple = TRUE)
  })

  # Tooltips for filters
  shinyBS::bsTooltip("year_range", "Filter records by training year range", "right", options = list(container = "body"))
  shinyBS::bsTooltip("orgs", "Select one or more organizations to focus analysis", "right", options = list(container = "body"))

  filtered <- reactive({
    req(input$year_range, input$orgs)
    df_all() %>% filter(Year >= input$year_range[1], Year <= input$year_range[2], Organization %in% input$orgs)
  })

  output$data_updated <- renderText({ paste("Data Updated On:", data_updated_on()) })

  # Summary cards
  output$vb_models <- renderValueBox({ valueBox(value = nrow(filtered()), subtitle = "Total Models", icon = icon("database"), color = "green") })
  output$vb_co2    <- renderValueBox({ valueBox(value = scales::comma(sum(filtered()$CO2_Tons, na.rm = TRUE)), subtitle = "Total CO₂ (tons)", icon = icon("leaf"), color = "teal") })
  output$vb_energy <- renderValueBox({ valueBox(value = scales::comma(round(mean(filtered()$EnergyUsed_MWh, na.rm = TRUE))), subtitle = "Avg Energy per Model (MWh)", icon = icon("bolt"), color = "yellow") })
  output$vb_largest<- renderValueBox({
    mx <- filtered() %>% filter(Parameters_Billion == max(Parameters_Billion, na.rm = TRUE)) %>% slice(1)
    valueBox(value = paste0(mx$Model, " (", mx$Parameters_Billion, "B)"), subtitle = "Largest Model", icon = icon("brain"), color = "purple")
  })

  # Emissions trend
  output$emissions_plot <- renderPlotly({
    p <- create_emissions_trend_plot(filtered(), interactive = FALSE)
    ggplotly(p, tooltip = c("Year", "Total_CO2")) %>% add_plotly_config()
  })
  # Animated emissions by year
  output$emissions_anim <- renderPlotly({
    yearly <- filtered() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons), Models = n())
    plot_ly(yearly, x = ~Year, y = ~Total_CO2, frame = ~Year, type = 'scatter', mode = 'lines+markers',
            text = ~paste('Year:', Year, '<br>Total CO2:', scales::comma(Total_CO2), 'tons', '<br>Models:', Models),
            hoverinfo = 'text', line = list(color = '#1e88e5')) %>%
      animation_opts(frame = 800, transition = 0, easing = 'linear') %>% add_plotly_config()
  })
  output$ins_emissions <- renderText({
    df <- filtered() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons))
    if (nrow(df) < 2) return("")
    change <- 100 * (last(df$Total_CO2) - first(df$Total_CO2)) / first(df$Total_CO2)
    paste0("💡 Observation: Total emissions changed by ", round(change, 1), "% between ", input$year_range[1], " and ", input$year_range[2], ".")
  })

  # Efficiency
  output$efficiency_plot <- renderPlotly({
    p <- create_efficiency_plot(filtered())
    if (!isTRUE(input$show_labels)) p <- p + ggrepel::geom_text_repel(aes(label = NULL))
    ggplotly(p) %>% add_plotly_config()
  })
  # Efficiency by org boxplot
  output$eff_by_org <- renderPlotly({
    p <- filtered() %>% mutate(Efficiency = CO2_Tons / Parameters_Billion) %>%
      ggplot(aes(x = Organization, y = Efficiency, fill = Organization)) + geom_boxplot() + coord_flip() +
      labs(x = 'Organization', y = 'CO₂ per B params') + theme_minimal(base_family = 'Inter')
    ggplotly(p) %>% add_plotly_config()
  })
  # Top5 ranking table
  output$top5_table <- reactable::renderReactable({
    df <- filtered() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = 'drop') %>% arrange(desc(Total_CO2)) %>% head(5)
    reactable::reactable(df, columns = list(Total_CO2 = reactable::colDef(format = reactable::colFormat(separators = TRUE))), striped = TRUE)
  })
  output$ins_efficiency <- renderText({
    df <- filtered()
    avg <- mean(df$CO2_per_Parameter, na.rm = TRUE)
    paste0("💡 Observation: Average CO₂/parameter = ", round(avg, 2), " across current filters.")
  })

  # Size vs Energy
  output$size_energy_plot <- renderPlotly({
    p <- create_model_size_energy_plot(filtered(), interactive = FALSE)
    ggplotly(p) %>% add_plotly_config()
  })
  output$ins_size <- renderText({
    df <- filtered()
    cor_val <- suppressWarnings(cor(log10(df$Parameters_Billion), log10(df$EnergyUsed_MWh)))
    paste0("💡 Observation: Log-log correlation between size and energy = ", round(cor_val, 2))
  })

  # Organization comparison (bar)
  output$org_bar <- renderPlotly({
    df <- filtered() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = "drop") %>% arrange(Total_CO2)
    p <- ggplot(df, aes(x = reorder(Organization, Total_CO2), y = Total_CO2, fill = Organization)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(x = "Organization", y = "Total CO₂ (tons)", title = "Total Emissions by Organization") +
      theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
    ggplotly(p)
  })

  # Distributions
  output$hist_co2 <- renderPlotly({
    p <- ggplot(filtered(), aes(x = CO2_Tons, fill = Organization)) + geom_histogram(bins = 20, alpha = 0.8) +
      labs(x = "CO₂ (tons)", y = "Count") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config()
  })
  output$box_energy <- renderPlotly({
    p <- ggplot(filtered(), aes(x = Organization, y = EnergyUsed_MWh, fill = Organization)) +
      geom_boxplot(outlier.alpha = 0.4) + coord_flip() + labs(x = "Organization", y = "Energy (MWh)") + theme_minimal(base_family = "Inter")
    ggplotly(p)
  })
  output$pie_org <- renderPlotly({
    df <- filtered() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = "drop")
    plot_ly(df, labels = ~Organization, values = ~Total_CO2, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = 'Emissions Share by Organization')
  })

  # Correlation plot (static ggplot image for reliability)
  output$corr_plot <- renderPlot({
    df <- filtered() %>% select(Parameters_Billion, EnergyUsed_MWh, CO2_Tons)
    df <- df[complete.cases(df), ]
    if (nrow(df) < 2) return()
    corr <- cor(df, method = "pearson")
    ggcorrplot::ggcorrplot(corr, lab = TRUE) + theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
  })

  # Downloads (server-side PNG using ggplot version of plots)
  output$dl_emissions_png <- downloadHandler(
    filename = function() paste0("emissions_trend_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      yearly <- filtered() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons))
      p <- ggplot(yearly, aes(x = Year, y = Total_CO2)) + geom_area(fill = "#90caf9", alpha = .5) + geom_line(color = "#1e88e5", linewidth = 1) +
        labs(title = "Growth of AI Carbon Emissions", x = "Year", y = "Total CO₂ (tons)") + theme_minimal(base_family = "Inter")
      ggsave(file, p, width = 10, height = 6)
    }
  )
  output$dl_efficiency_png <- downloadHandler(
    filename = function() paste0("efficiency_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      p <- filtered() %>% mutate(Efficiency = CO2_Tons / Parameters_Billion) %>%
        ggplot(aes(x = Year, y = Efficiency, color = Organization)) + geom_point() + labs(x = "Year", y = "CO₂ per B params") + theme_minimal(base_family = "Inter")
      ggsave(file, p, width = 10, height = 6)
    }
  )
  output$dl_size_png <- downloadHandler(
    filename = function() paste0("size_energy_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      p <- ggplot(filtered(), aes(x = Parameters_Billion, y = EnergyUsed_MWh, color = Organization, size = CO2_Tons)) +
        geom_point(alpha = .8) + scale_x_log10() + scale_y_log10() + labs(x = "Model Size (B params, log)", y = "Energy (MWh, log)") + theme_minimal(base_family = "Inter")
      ggsave(file, p, width = 10, height = 6)
    }
  )

  # Data table (reactable)
  output$table <- reactable::renderReactable({
    reactable::reactable(filtered(), searchable = TRUE, pagination = TRUE, striped = TRUE, resizable = TRUE)
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("ai_carbon_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) readr::write_csv(filtered(), file)
  )
}

shinyApp(ui, server)
