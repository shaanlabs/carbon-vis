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
library(here)

# ---- Data loading helpers ----
safe_read <- function(path) {
  full <- here::here(path)
  if (file.exists(full)) readr::read_csv(full, show_col_types = FALSE) else NULL
}

load_all_data <- function() {
  message("Loading all datasets...")
  if (!dir.exists(here::here("data"))) stop("Data folder missing: ", here::here("data"))
  # Ensure required CSVs exist
  ensure_datasets()

  base <- safe_read(file.path("data", "ai_carbon_footprint.csv"))
  model_size <- safe_read(file.path("data", "model_size.csv"))
  emissions <- safe_read(file.path("data", "carbon_footprint.csv"))
  efficiency <- safe_read(file.path("data", "efficiency.csv"))

  if (!is.null(base)) {
    df <- base
  } else if (!is.null(model_size) && !is.null(emissions)) {
    df <- model_size %>% left_join(emissions, by = c("Model", "Year", "Organization"))
    if (!is.null(efficiency)) df <- df %>% left_join(efficiency, by = c("Model", "Year", "Organization"))
  } else {
    df <- tibble::tibble()
  }

  # Failsafe mock dataset when empty
  if (nrow(df) == 0) {
    message("No rows after load; generating mock dataset for UI...")
    df <- tibble::tibble(
      Year = 2017:2025,
      Model = paste0("Model-", 2017:2025),
      Parameters_Billion = seq(0.1, 1000, length.out = 9),
      EnergyUsed_MWh = seq(1000, 1000000, length.out = 9),
      CO2_Tons = seq(100, 800, 100),
      Organization = rep(c("OpenAI", "DeepMind", "Google"), length.out = 9)
    )
  }

  # Compute missing metrics
  if (!"CO2_per_Parameter" %in% names(df) && all(c("CO2_Tons","Parameters_Billion") %in% names(df))) {
    df$CO2_per_Parameter <- round(df$CO2_Tons / pmax(df$Parameters_Billion, 1e-9), 4)
  }
  if (!"Energy_Efficiency" %in% names(df) && all(c("Parameters_Billion","EnergyUsed_MWh") %in% names(df))) {
    df$Energy_Efficiency <- round(df$Parameters_Billion / pmax(df$EnergyUsed_MWh, 1e-9), 6)
  }

  df <- df %>% tidyr::replace_na(list(CO2_Tons = 0, EnergyUsed_MWh = 0))
  message(sprintf("✅ Data loaded: %s rows, cols: %s", nrow(df), paste(names(df), collapse = ", ")))
  return(df)
}

data_updated_on <- function() {
  f <- file.path("data", "ai_carbon_footprint.csv")
  if (file.exists(f)) format(file.info(f)$mtime, "%Y-%m-%d %H:%M") else "unknown"
}

# ---- UI ----
ui <- shinydashboardPlus::dashboardPage(
  header = dashboardHeader(title = span(icon("leaf"), "CO2lytics: AI Energy & Carbon Intelligence (2017–2025)")),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("house")),
      menuItem("Carbon Emission Trend", tabName = "emissions", icon = icon("chart-line")),
      menuItem("Model Efficiency", tabName = "efficiency", icon = icon("bullseye")),
      menuItem("Energy vs Model Size", tabName = "size_energy", icon = icon("circle")),
      menuItem("Organization Comparison", tabName = "org", icon = icon("building")),
      menuItem("Distribution Insights", tabName = "dist", icon = icon("chart-bar")),
      menuItem("Dataset Explorer", tabName = "explorer", icon = icon("table"))
    ),
    NULL
  ),
  body = dashboardBody(
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
          box(width = 12, title = "About", status = "primary", solidHeader = TRUE,
              p("This dashboard analyzes AI model energy usage and carbon emissions from 2017 to 2025."),
              p("Use filters to explore by year and organization. Charts are interactive and exportable."),
              p("Credits: Dataset within this project; organizations include OpenAI, Google DeepMind, Meta, NVIDIA, etc."))
        ),
        fluidRow(
          box(width = 12, title = "Model Releases Timeline", status = "primary", solidHeader = TRUE,
              plotlyOutput("timeline_bar", height = 280))
        )
      ),
      tabItem(tabName = "emissions",
        fluidRow(
          box(width = 12, title = tagList(icon("chart-line"), "Growth of AI Carbon Emissions (2017–2025)"), status = "primary", solidHeader = TRUE,
              plotlyOutput("emissions_plot", height = 480),
              plotlyOutput("emissions_anim", height = 420),
              downloadButton("dl_emissions_png", "Download PNG")),
          box(width = 12, title = "Insights", status = "info",
              textOutput("ins_emissions"))
        )
      ),
      tabItem(tabName = "efficiency",
        fluidRow(
          box(width = 12, title = tagList(icon("bullseye"), "AI Model Training Efficiency Over Time"), status = "primary", solidHeader = TRUE,
              plotlyOutput("efficiency_plot", height = 480),
              downloadButton("dl_efficiency_png", "Download PNG")),
          box(width = 12, title = "Insights", status = "info",
              textOutput("ins_efficiency"))
        )
      ),
      tabItem(tabName = "size_energy",
        fluidRow(
          box(width = 12, title = tagList(icon("circle"), "AI Model Size vs Energy Used"), status = "primary", solidHeader = TRUE,
              plotlyOutput("size_energy_plot", height = 500),
              downloadButton("dl_size_png", "Download PNG")),
          box(width = 12, title = "Insights", status = "info",
              textOutput("ins_size"))
        )
      ),
      tabItem(tabName = "org",
        fluidRow(
          box(width = 12, title = "Total Emissions by Organization", status = "primary", solidHeader = TRUE,
              plotlyOutput("org_bar", height = 460),
              downloadButton("dl_org_png", "Download PNG"))
        ),
        fluidRow(
          box(width = 6, title = "Efficiency by Organization (Box)", status = "primary", solidHeader = TRUE,
              plotlyOutput("eff_by_org", height = 360)),
          box(width = 6, title = "Top 5 Most Carbon-Intensive Labs", status = "warning", solidHeader = TRUE,
              reactable::reactableOutput("top5_table"))
        )
      ),
      tabItem(tabName = "dist",
        fluidRow(
          box(width = 6, title = "CO₂ Emissions Distribution", status = "primary", solidHeader = TRUE,
              plotlyOutput("hist_co2", height = 360)),
          box(width = 6, title = "Energy Used by Organization (Boxplot)", status = "primary", solidHeader = TRUE,
              plotlyOutput("box_energy", height = 360))
        ),
        fluidRow(
          box(width = 6, title = "Proportion of Total Emissions by Organization", status = "primary", solidHeader = TRUE,
              plotlyOutput("pie_org", height = 360)),
          box(width = 6, title = "Correlation: Size, Energy, CO₂", status = "primary", solidHeader = TRUE,
              plotOutput("corr_plot", height = 360))
        )
      ),
      tabItem(tabName = "explorer",
        fluidRow(
          box(width = 12, title = "Dataset Explorer", status = "primary", solidHeader = TRUE,
              reactable::reactableOutput("table"),
              br(),
              downloadButton("download_csv", "Download Filtered CSV"))
        )
      )
    )
  ),
  controlbar = shinydashboardPlus::dashboardControlbar(
      h4("Global Filters"),
      uiOutput("year_range_ui"),
      uiOutput("org_select_ui"),
      checkboxInput("show_labels", "Show model labels", value = TRUE),
      div(class = "footer-note", textOutput("data_updated")),
      div(class = "footer-note", textOutput("loading_text"))
    ),
  skin = "black"
)

# ---- Server ----
server <- function(input, output, session) {
  df_all <- reactiveVal(load_all_data())
  message("Server initialized. df_all set.")

  # Filters
  output$year_range_ui <- renderUI({
    df <- df_all(); validate(need(!is.null(df) && any(!is.na(df$Year)), "No Year data"))
    sliderInput("year_range", "Year range", min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE),
                value = c(min(df$Year, na.rm = TRUE), max(df$Year, na.rm = TRUE)), step = 1, sep = "")
  })
  output$org_select_ui <- renderUI({
    df <- df_all(); orgs <- sort(unique(df$Organization))
    if (length(orgs) == 0 || all(is.na(orgs))) orgs <- c("OpenAI","DeepMind","Google")
    selectInput("orgs", "Organization", choices = orgs, selected = orgs, multiple = TRUE)
  })

  # Tooltips for filters
  shinyBS::bsTooltip("year_range", "Filter records by training year range", "right", options = list(container = "body"))
  shinyBS::bsTooltip("orgs", "Select one or more organizations to focus analysis", "right", options = list(container = "body"))

  filtered_data <- reactive({
    req(df_all(), input$year_range, input$orgs)
    out <- df_all() %>% filter(Year >= input$year_range[1], Year <= input$year_range[2], Organization %in% input$orgs)
    out
  })

  output$data_updated <- renderText({ paste("Data Updated On:", data_updated_on()) })
  output$loading_text <- renderText({
    df <- df_all(); cnt <- if (is.null(df)) 0 else nrow(df)
    paste("Data loaded:", cnt, "records | Filtered:", tryCatch(nrow(filtered_data()), error = function(e) 0))
  })

  # Summary cards
  output$vb_models <- renderValueBox({ valueBox(value = nrow(filtered_data()), subtitle = "Total Models", icon = icon("database"), color = "green") })
  output$vb_co2    <- renderValueBox({ valueBox(value = scales::comma(sum(filtered_data()$CO2_Tons, na.rm = TRUE)), subtitle = "Total CO₂ (tons)", icon = icon("leaf"), color = "teal") })
  output$vb_energy <- renderValueBox({ valueBox(value = scales::comma(round(mean(filtered_data()$EnergyUsed_MWh, na.rm = TRUE))), subtitle = "Avg Energy per Model (MWh)", icon = icon("bolt"), color = "yellow") })
  output$vb_largest<- renderValueBox({
    mx <- filtered_data() %>% filter(Parameters_Billion == max(Parameters_Billion, na.rm = TRUE)) %>% slice(1)
    valueBox(value = paste0(mx$Model, " (", mx$Parameters_Billion, "B)"), subtitle = "Largest Model", icon = icon("brain"), color = "purple")
  })

  # Emissions trend
  output$emissions_plot <- renderPlotly({
    print("Rendering emissions plot...")
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    p <- create_emissions_trend_plot(df, interactive = FALSE)
    ggplotly(p, tooltip = c("Year", "Total_CO2")) %>% add_plotly_config()
  })
  # Animated emissions by year
  output$emissions_anim <- renderPlotly({
    yearly <- filtered_data() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons), Models = n())
    plot_ly(yearly, x = ~Year, y = ~Total_CO2, frame = ~Year, type = 'scatter', mode = 'lines+markers',
            text = ~paste('Year:', Year, '<br>Total CO2:', scales::comma(Total_CO2), 'tons', '<br>Models:', Models),
            hoverinfo = 'text', line = list(color = '#1e88e5')) %>%
      animation_opts(frame = 800, transition = 0, easing = 'linear') %>% add_plotly_config()
  })
  output$ins_emissions <- renderText({
    df <- filtered_data() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons))
    if (nrow(df) < 2) return("")
    change <- 100 * (last(df$Total_CO2) - first(df$Total_CO2)) / first(df$Total_CO2)
    paste0("💡 Observation: Total emissions changed by ", round(change, 1), "% between ", input$year_range[1], " and ", input$year_range[2], ".")
  })

  # Efficiency
  output$efficiency_plot <- renderPlotly({
    print("Rendering efficiency plot...")
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    p <- create_efficiency_plot(df)
    if (!isTRUE(input$show_labels)) p <- p + ggrepel::geom_text_repel(aes(label = NULL))
    ggplotly(p) %>% add_plotly_config()
  })
  # Top 5 carbon-intensive labs
  output$top5_table <- reactable::renderReactable({
    df <- filtered_data() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = 'drop') %>% arrange(desc(Total_CO2)) %>% head(5)
    reactable::reactable(df, columns = list(Total_CO2 = reactable::colDef(format = reactable::colFormat(separators = TRUE))), striped = TRUE)
  })
  output$ins_efficiency <- renderText({
    df <- filtered_data()
    avg <- mean(df$CO2_per_Parameter, na.rm = TRUE)
    paste0("💡 Observation: Average CO₂/parameter = ", round(avg, 2), " across current filters.")
  })

  # Size vs Energy
  output$size_energy_plot <- renderPlotly({
    print("Rendering size vs energy plot...")
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    p <- create_model_size_energy_plot(df, interactive = FALSE)
    ggplotly(p) %>% add_plotly_config()
  })
  output$ins_size <- renderText({
    df <- filtered_data()
    cor_val <- suppressWarnings(cor(log10(df$Parameters_Billion), log10(df$EnergyUsed_MWh)))
    paste0("💡 Observation: Log-log correlation between size and energy = ", round(cor_val, 2))
  })

  # Organization comparison (bar)
  output$org_bar <- renderPlotly({
    print("Rendering org bar plot...")
    df <- filtered_data() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = "drop") %>% arrange(Total_CO2)
    p <- ggplot(df, aes(x = reorder(Organization, Total_CO2), y = Total_CO2, fill = Organization)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(x = "Organization", y = "Total CO₂ (tons)", title = "Total Emissions by Organization") +
      theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
    ggplotly(p) %>% add_plotly_config()
  })

  # Efficiency by Organization (Box)
  output$eff_by_org <- renderPlotly({
    print("Rendering efficiency by org boxplot...")
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    df <- df %>% mutate(Efficiency = CO2_Tons / pmax(Parameters_Billion, 1e-9))
    p <- ggplot(df, aes(x = Organization, y = Efficiency, fill = Organization)) +
      geom_boxplot(outlier.alpha = 0.4) +
      coord_flip() +
      labs(x = "Organization", y = "CO₂ per Billion Parameters", title = "Efficiency by Organization") +
      theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
    ggplotly(p) %>% add_plotly_config()
  })

  # Distributions
  output$hist_co2 <- renderPlotly({
    print("Rendering CO2 histogram...")
    p <- ggplot(filtered_data(), aes(x = CO2_Tons, fill = Organization)) + geom_histogram(bins = 20, alpha = 0.8) +
      labs(x = "CO₂ (tons)", y = "Count") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config()
  })
  output$box_energy <- renderPlotly({
    print("Rendering energy boxplot...")
    p <- ggplot(filtered_data(), aes(x = Organization, y = EnergyUsed_MWh, fill = Organization)) +
      geom_boxplot(outlier.alpha = 0.4) + coord_flip() + labs(x = "Organization", y = "Energy (MWh)") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config()
  })
  output$pie_org <- renderPlotly({
    print("Rendering org pie plot...")
    df <- filtered_data() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = "drop")
    plot_ly(df, labels = ~Organization, values = ~Total_CO2, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = 'Emissions Share by Organization')
  })

  # Timeline bar on overview: count models per year
  output$timeline_bar <- renderPlotly({
    df <- filtered_data() %>% group_by(Year) %>% summarise(Models = n(), .groups = 'drop')
    p <- ggplot(df, aes(x = Year, y = Models)) + geom_col(fill = '#17a2b8') +
      labs(title = 'Model Releases Timeline', x = 'Year', y = 'Models') + theme_minimal(base_family = 'Inter')
    ggplotly(p) %>% add_plotly_config()
  })

  # Correlation plot (static ggplot image for reliability)
  output$corr_plot <- renderPlot({
    df <- filtered_data() %>% select(Parameters_Billion, EnergyUsed_MWh, CO2_Tons)
    df <- df[complete.cases(df), ]
    if (nrow(df) < 2) return()
    corr <- cor(df, method = "pearson")
    ggcorrplot::ggcorrplot(corr, lab = TRUE) + theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
  })

  # Downloads (server-side PNG using ggplot version of plots)
  output$dl_emissions_png <- downloadHandler(
    filename = function() paste0("emissions_trend_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      yearly <- filtered_data() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons))
      p <- ggplot(yearly, aes(x = Year, y = Total_CO2)) + geom_area(fill = "#90caf9", alpha = .5) + geom_line(color = "#1e88e5", linewidth = 1) +
        labs(title = "Growth of AI Carbon Emissions", x = "Year", y = "Total CO₂ (tons)") + theme_minimal(base_family = "Inter")
      ggsave(file, p, width = 10, height = 6)
    }
  )
  output$dl_efficiency_png <- downloadHandler(
    filename = function() paste0("efficiency_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      p <- filtered_data() %>% mutate(Efficiency = CO2_Tons / Parameters_Billion) %>%
        ggplot(aes(x = Year, y = Efficiency, color = Organization)) + geom_point() + labs(x = "Year", y = "CO₂ per B params") + theme_minimal(base_family = "Inter")
      ggsave(file, p, width = 10, height = 6)
    }
  )
  output$dl_size_png <- downloadHandler(
    filename = function() paste0("size_energy_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      p <- ggplot(filtered_data(), aes(x = Parameters_Billion, y = EnergyUsed_MWh, color = Organization, size = CO2_Tons)) +
        geom_point(alpha = .8) + scale_x_log10() + scale_y_log10() + labs(x = "Model Size (B params, log)", y = "Energy (MWh, log)") + theme_minimal(base_family = "Inter")
      ggsave(file, p, width = 10, height = 6)
    }
  )

  # Data table (reactable)
  output$table <- reactable::renderReactable({
    print("Rendering reactable table...")
    reactable::reactable(filtered_data(), searchable = TRUE, pagination = TRUE, striped = TRUE, resizable = TRUE)
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("ai_carbon_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) readr::write_csv(filtered_data(), file)
  )
}

shinyApp(ui, server)
