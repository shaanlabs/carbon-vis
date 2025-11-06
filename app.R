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
library(leaflet)
library(shinyjs)
library(reactable)

# ---- Data loading helpers ----
safe_read <- function(path) {
  full <- here::here(path)
  if (file.exists(full)) readr::read_csv(full, show_col_types = FALSE) else NULL
}

load_all_data <- function() {
  message("Loading all datasets...")
  if (!dir.exists(here::here("data"))) stop("Data folder missing: ", here::here("data"))

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
    sidebarMenu(id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("house")),
      menuItem("Carbon Emission Trend", tabName = "emissions", icon = icon("chart-line")),
      menuItem("Model Efficiency", tabName = "efficiency", icon = icon("bullseye")),
      menuItem("Energy vs Model Size", tabName = "size_energy", icon = icon("circle")),
      menuItem("Organization Comparison", tabName = "org", icon = icon("building")),
      menuItem("Distribution Insights", tabName = "dist", icon = icon("chart-bar")),
      menuItem("Dataset Explorer", tabName = "explorer", icon = icon("table")),
      menuItem("Forecast", tabName = "forecast", icon = icon("chart-area")),
      menuItem("Sustainability", tabName = "sustain", icon = icon("leaf")),
      menuItem("What-If", tabName = "whatif", icon = icon("sliders")),
      menuItem("Data Integrity", tabName = "integrity", icon = icon("shield")),
      menuItem("Correlation", tabName = "corr", icon = icon("braille")),
      menuItem("CO₂ Map", tabName = "map", icon = icon("globe")),
      menuItem("ML Regression", tabName = "ml", icon = icon("robot"))
    )
  ),
  body = dashboardBody(
    useShinyjs(),
    theme = bs_theme(version = 5, bootswatch = "flatly", base_font = font_google("Inter")),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/animate.css/4.1.1/animate.min.css"),
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/wow/1.1.2/wow.min.js"),
      tags$script(HTML("new WOW().init();")),
      tags$style(".hero{background:linear-gradient(90deg,#004d61,#00bfa5);color:#fff;padding:50px;border-radius:10px;margin-bottom:20px} .kpi-box .small-box{transition:transform .2s} .kpi-box .small-box:hover{transform:translateY(-3px)} .card{background:#ffffff10;border:1px solid rgba(0,0,0,0.05);border-radius:10px;padding:18px;transition:.3s} .card:hover{transform:scale(1.03);box-shadow:0 10px 20px rgba(0,0,0,0.15)} .insight{background:#f5f5f5;border-radius:8px;padding:12px}")
    ),
    tags$style(HTML(
      ".theme-dark .content-wrapper, .theme-dark .right-side { background-color: #1f2a33 !important; color: #e0e0e0; }\n" ,
      ".theme-dark .box { background-color: #26323b !important; color: #e0e0e0; border-color: #3a4752; }\n",
      ".theme-dark .box .box-header { background-color: #2b3944 !important; color: #e0e0e0; }\n",
      ".theme-dark .sidebar-menu > li > a { color: #cfd8dc !important; }\n",
      ".theme-dark .navbar { background-color: #2b3944 !important; }\n",
      ".theme-dark .control-sidebar { background-color: #26323b !important; color: #e0e0e0; }\n"
    )),
    tabItems(
      tabItem(tabName = "overview",
        fluidRow(
          column(12,
            div(class = "hero animate__animated animate__fadeInUp animate__delay-1s wow",
              h1("\U0001F30D CO\u2082lytics: AI Carbon Intelligence Dashboard"),
              h4("Visualizing the energy, efficiency, and emissions of artificial intelligence models from 2017–2025."),
              br(),
              actionButton("explore_btn", "Explore Dashboard \u2192", icon = icon("arrow-right"),
                style="background:#00e676;border:none;font-weight:bold;padding:12px 24px;color:#003;"))
          )
        ),
        br(),
        fluidRow(class = "kpi-box",
          valueBoxOutput("total_models", width = 3),
          valueBoxOutput("total_co2", width = 3),
          valueBoxOutput("avg_energy", width = 3),
          valueBoxOutput("largest_model", width = 3)
        ),
        br(),
        fluidRow(
          column(4, div(class="card animate__animated animate__fadeInUp wow",
                        h3("\U0001F331 Mission"), p("To analyze AI’s energy use and carbon impact transparently and visually."))),
          column(4, div(class="card animate__animated animate__fadeInUp animate__delay-1s wow",
                        h3("\u26A1 Insights"), p("Visualizing emissions and efficiency across major AI research labs."))),
          column(4, div(class="card animate__animated animate__fadeInUp animate__delay-2s wow",
                        h3("\U0001F4CA Data Sources"), p("Based on datasets from OpenAI, Google DeepMind, Meta, NVIDIA, and more.")))
        ),
        br(),
        fluidRow(
          box(width = 6, title = "AI Model Release Timeline", status = "primary", solidHeader = TRUE,
              shinycssloaders::withSpinner(plotlyOutput("model_timeline", height = 320), type = 6, color = "#00bfa5")),
          box(width = 6, title = "Growth of AI Carbon Emissions", status = "success", solidHeader = TRUE,
              shinycssloaders::withSpinner(plotlyOutput("sustain_plot", height = 320), type = 6, color = "#00bfa5"),
              div(class="insight", h4("\U0001F4A1 Insight:"), textOutput("insight_text")))
        ),
        div(style="text-align:center;margin:40px;",
            actionButton("goto_dashboard", "Enter Full Dashboard", icon = icon("chart-line"),
                         style="background:#00796b;color:white;padding:14px 32px;border:none;border-radius:8px;font-size:18px;")),
        div(style="background:#004d61;color:white;text-align:center;padding:20px;border-radius:8px;",
            p(" 2025 CO\u2082lytics | Built with \u2764\uFE0F in R Shiny + Plotly"),
            tags$a(href="https://github.com/YourUsername/CO2lytics", "View on GitHub", target="_blank", style="color:#00e676;")),
        tags$div(class = "fab-container",
          actionButton("fab_top", label = NULL, icon = icon("arrow-up"), class = "fab fab-top", title = "Scroll to top"),
          actionButton("fab_dark", label = NULL, icon = icon("moon"), class = "fab fab-dark", title = "Toggle dark/light"),
          actionButton("fab_feedback", label = NULL, icon = icon("comment-dots"), class = "fab fab-feedback", title = "Feedback")
        ),
        tags$div(class = "data-indicator",
          span(icon("sync")),
          span(textOutput("data_status"))
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
      ),
      tabItem(tabName = "forecast",
        fluidRow(
          box(width = 4, title = "Controls", status = "primary", solidHeader = TRUE,
              sliderInput("years_ahead", "Years ahead", min = 1, max = 10, value = 5, step = 1),
              helpText("Forecast uses yearly total CO₂. ARIMA via 'forecast' if available, otherwise base arima.")),
          box(width = 8, title = "CO₂ Forecast", status = "primary", solidHeader = TRUE,
              plotlyOutput("forecast_plot", height = 420),
              textOutput("forecast_summary"))
        )
      ),
      tabItem(tabName = "sustain",
        fluidRow(
          box(width = 7, title = "Green AI Score — Radar", status = "success", solidHeader = TRUE,
              plotlyOutput("radar_org", height = 460)),
          box(width = 5, title = "Organization Scores", status = "success", solidHeader = TRUE,
              reactable::reactableOutput("score_table"))
        )
      ),
      tabItem(tabName = "whatif",
        fluidRow(
          box(width = 4, title = "What-If Controls", status = "warning", solidHeader = TRUE,
              sliderInput("renewables_pct", "% Renewable Energy", min = 0, max = 100, value = 30, step = 5),
              numericInput("carbon_credits", "Carbon credits (tons)", value = 0, min = 0, step = 100),
              helpText("Applies to filtered data; shows baseline vs net emissions.")),
          box(width = 8, title = "Baseline vs Net Emissions", status = "warning", solidHeader = TRUE,
              plotlyOutput("whatif_plot", height = 420),
              textOutput("whatif_summary"))
        )
      ),
      tabItem(tabName = "integrity",
        fluidRow(
          box(width = 4, title = "Checks", status = "info", solidHeader = TRUE,
              valueBoxOutput("vb_missing_years"),
              valueBoxOutput("vb_negative_co2"),
              valueBoxOutput("vb_dupes")),
          box(width = 8, title = "Issue Details", status = "info", solidHeader = TRUE,
              reactable::reactableOutput("integrity_table"))
        )
      ),
      tabItem(tabName = "corr",
        fluidRow(
          box(width = 3, title = "Controls", status = "primary", solidHeader = TRUE,
              uiOutput("corr_controls")),
          box(width = 9, title = "Correlation Matrix", status = "primary", solidHeader = TRUE,
              plotOutput("corr_live", height = 420))
        )
      ),
      tabItem(tabName = "map",
        fluidRow(
          box(width = 12, title = "Global CO₂ Map", status = "primary", solidHeader = TRUE,
              leafletOutput("map_co2", height = 520))
        )
      ),
      tabItem(tabName = "ml",
        fluidRow(
          box(width = 4, title = "Model", status = "primary", solidHeader = TRUE,
              helpText("Predict CO₂_Tons from features using Random Forest (ranger)."),
              verbatimTextOutput("ml_summary")),
          box(width = 8, title = "Feature Importance", status = "primary", solidHeader = TRUE,
              plotlyOutput("ml_importance", height = 420))
        )
      )
    )
  ),
  controlbar = shinydashboardPlus::dashboardControlbar(
    h4("Global Filters"),
    uiOutput("year_range_ui"),
    uiOutput("org_select_ui"),
    checkboxInput("show_labels", "Show model labels", value = TRUE),
    selectInput("theme_choice", "Theme", choices = c("flatly","cyborg","solar","minty","sandstone"), selected = "flatly"),
    div(class = "footer-note", textOutput("data_updated")),
    div(class = "footer-note", textOutput("loading_text"))
  ),
  skin = "black"
)

# ---- Server ----
server <- function(input, output, session) {
  df_all <- reactiveVal(load_all_data())
  message("Server initialized. df_all set.")

  plotly_template <- reactive({ if ((input$theme_choice %||% "flatly") %in% c("cyborg","solar")) "plotly_dark" else "plotly_white" })
  apply_plotly_theme <- function(p) { plotly::layout(p, template = plotly_template()) }

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

  filtered_data <- reactive({
    req(df_all(), input$year_range, input$orgs)
    out <- df_all() %>% filter(Year >= input$year_range[1], Year <= input$year_range[2], Organization %in% input$orgs)
    out
  })

  output$total_models <- renderValueBox({
    df <- filtered_data()
    valueBox(value = format(dplyr::n_distinct(df$Model), big.mark = ","), subtitle = "Total Models", icon = icon("database"), color = "green")
  })

  output$total_co2 <- renderValueBox({
    df <- filtered_data()
    valueBox(value = format(sum(df$CO2_Tons, na.rm = TRUE), big.mark = ","), subtitle = "Total CO₂ Emissions (tons)", icon = icon("leaf"), color = "aqua")
  })

  output$avg_energy <- renderValueBox({
    df <- filtered_data()
    valueBox(value = format(round(mean(df$EnergyUsed_MWh, na.rm = TRUE), 2), big.mark = ","), subtitle = "Avg Energy per Model (MWh)", icon = icon("bolt"), color = "orange")
  })

  output$largest_model <- renderValueBox({
    df <- filtered_data()
    if (nrow(df) == 0) return(NULL)
    top <- df[which.max(df$Parameters_Billion), ]
    valueBox(value = paste0(top$Model, " (", top$Parameters_Billion, "B)"), subtitle = "Largest Model", icon = icon("brain"), color = "purple")
  })

  output$model_timeline <- renderPlotly({
    df <- filtered_data() %>% group_by(Year) %>% summarise(Models = n(), .groups = 'drop')
    plot_ly(df, x = ~Year, y = ~Models, type = 'bar', marker = list(color = '#00bfa5')) %>%
      layout(title = "AI Model Release Timeline", xaxis = list(title = "Year"), yaxis = list(title = "Models"))
  })

  output$sustain_plot <- renderPlotly({
    df <- filtered_data() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = 'drop')
    plot_ly(df, x = ~Year, y = ~Total_CO2, type = 'scatter', mode = 'lines+markers',
            fill = 'tozeroy', line = list(color = '#4caf50', width = 3)) %>%
      layout(title = "Growth of AI Carbon Emissions", yaxis = list(title = "Total CO₂ (tons)"))
  })

  output$insight_text <- renderText({
    df <- filtered_data() %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = 'drop') %>% arrange(Year)
    if (nrow(df) < 2 || min(df$Total_CO2) <= 0) return("")
    pct <- 100 * (max(df$Total_CO2) - min(df$Total_CO2)) / min(df$Total_CO2)
    paste("Since", min(df$Year), "total emissions have increased by", round(pct, 1), "%.")
  })

  observeEvent(input$explore_btn, { updateTabItems(session, "tabs", "emissions") })
  observeEvent(input$goto_dashboard, { updateTabItems(session, "tabs", "emissions") })

  observeEvent(input$fab_top, ignoreInit = TRUE, {
    shinyjs::runjs("window.scrollTo({top:0, behavior:'smooth'})")
  })

  observeEvent(input$fab_dark, ignoreInit = TRUE, {
    current <- isolate(input$theme_choice)
    nextTheme <- if (identical(current, "cyborg")) "flatly" else "cyborg"
    updateSelectInput(session, "theme_choice", selected = nextTheme)
  })

  observeEvent(input$fab_feedback, ignoreInit = TRUE, {
    shinyjs::runjs("window.open('https://github.com/YourUsername/CO2lytics/issues','_blank')")
  })

  output$data_updated <- renderText({ paste("Data Updated On:", data_updated_on()) })

  output$loading_text <- renderText({
    df <- df_all(); cnt <- if (is.null(df)) 0 else nrow(df)
    paste("Data loaded:", cnt, "records | Filtered:", tryCatch(nrow(filtered_data()), error = function(e) 0))
  })

  output$data_status <- renderText({
    paste0("Updated ", data_updated_on(), " · Filtered ", tryCatch(nrow(filtered_data()), error = function(e) 0))
  })

  observeEvent(input$theme_choice, {
    dark <- (input$theme_choice %in% c("cyborg","solar"))
    if (dark) shinyjs::addClass(selector = "body", class = "theme-dark") else shinyjs::removeClass(selector = "body", class = "theme-dark")
    tryCatch({
      th <- bs_theme(version = 5, bootswatch = input$theme_choice, base_font = font_google("Inter"))
      if (dark) th <- bs_theme_update(th, bg = "#1f2a33", fg = "#e0e0e0", primary = "#17a2b8")
      session$setCurrentTheme(th)
    }, error = function(e) { NULL })
  }, ignoreInit = TRUE)

  output$emissions_plot <- renderPlotly({
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    yearly <- df %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons))
    plot_ly(yearly, x = ~Year, y = ~Total_CO2, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Growth of AI Carbon Emissions", yaxis = list(title = "Total CO₂ (tons)"))
  })

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

  output$efficiency_plot <- renderPlotly({
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    p <- ggplot(df, aes(x = Year, y = CO2_per_Parameter, color = Organization)) + geom_point() + labs(x = "Year", y = "CO₂ per Billion Parameters") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config()
  })

  output$top5_table <- reactable::renderReactable({
    df <- filtered_data() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = 'drop') %>% arrange(desc(Total_CO2)) %>% head(5)
    reactable::reactable(df, columns = list(Total_CO2 = reactable::colDef(format = reactable::colFormat(separators = TRUE))), striped = TRUE)
  })

  output$ins_efficiency <- renderText({
    df <- filtered_data()
    avg <- mean(df$CO2_per_Parameter, na.rm = TRUE)
    paste0("💡 Observation: Average CO₂/parameter = ", round(avg, 2), " across current filters.")
  })

  output$size_energy_plot <- renderPlotly({
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    p <- ggplot(df, aes(x = Parameters_Billion, y = EnergyUsed_MWh, color = Organization, size = CO2_Tons)) +
      geom_point(alpha = .8) + scale_x_log10() + scale_y_log10() + labs(x = "Model Size (B params, log)", y = "Energy (MWh, log)") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config()
  })

  output$ins_size <- renderText({
    df <- filtered_data()
    cor_val <- suppressWarnings(cor(log10(df$Parameters_Billion), log10(df$EnergyUsed_MWh)))
    paste0("💡 Observation: Log-log correlation between size and energy = ", round(cor_val, 2))
  })

  output$org_bar <- renderPlotly({
    df <- filtered_data() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = "drop") %>% arrange(Total_CO2)
    p <- ggplot(df, aes(x = reorder(Organization, Total_CO2), y = Total_CO2, fill = Organization)) +
      geom_col(show.legend = FALSE) + coord_flip() +
      labs(x = "Organization", y = "Total CO₂ (tons)", title = "Total Emissions by Organization") +
      theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
    ggplotly(p) %>% add_plotly_config()
  })

  output$eff_by_org <- renderPlotly({
    df <- filtered_data(); validate(need(nrow(df) > 0, "No data for selected filters"))
    df <- df %>% mutate(Efficiency = CO2_Tons / pmax(Parameters_Billion, 1e-9))
    p <- ggplot(df, aes(x = Organization, y = Efficiency, fill = Organization)) +
      geom_boxplot(outlier.alpha = 0.4) +
      coord_flip() +
      labs(x = "Organization", y = "CO₂ per Billion Parameters", title = "Efficiency by Organization") +
      theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
    ggplotly(p) %>% add_plotly_config()
  })

  output$hist_co2 <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = CO2_Tons, fill = Organization)) + geom_histogram(bins = 20, alpha = 0.8) +
      labs(x = "CO₂ (tons)", y = "Count") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config()
  })

  output$box_energy <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Organization, y = EnergyUsed_MWh, fill = Organization)) +
      geom_boxplot(outlier.alpha = 0.4) + coord_flip() + labs(x = "Organization", y = "Energy (MWh)") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config()
  })

  output$pie_org <- renderPlotly({
    df <- filtered_data() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = "drop")
    plot_ly(df, labels = ~Organization, values = ~Total_CO2, type = 'pie', textinfo = 'label+percent', insidetextorientation = 'radial') %>%
      layout(title = 'Emissions Share by Organization', template = plotly_template())
  })

  output$corr_plot <- renderPlot({
    df <- filtered_data() %>% select(Parameters_Billion, EnergyUsed_MWh, CO2_Tons)
    df <- df[complete.cases(df), ]
    if (nrow(df) < 2) return()
    corr <- cor(df, method = "pearson")
    ggcorrplot::ggcorrplot(corr, lab = TRUE) + theme_minimal(base_family = "Inter") + theme(plot.title = element_text(face = "bold", size = 16))
  })

  output$corr_controls <- renderUI({
    df <- df_all()
    nums <- names(df)[vapply(df, is.numeric, logical(1))]
    prefer <- c("Parameters_Billion","EnergyUsed_MWh","CO2_Tons")
    nums <- unique(c(prefer[prefer %in% nums], setdiff(nums, prefer)))
    tagList(
      checkboxGroupInput("corr_vars", "Variables", choices = nums, selected = head(nums, 3)),
      radioButtons("corr_method", "Method", choices = c("pearson","spearman","kendall"), selected = "pearson")
    )
  })

  output$corr_live <- renderPlot({
    df <- filtered_data(); vars <- input$corr_vars
    validate(need(length(vars) >= 2, "Pick at least 2 variables"))
    X <- df[, vars, drop = FALSE]
    X <- X[complete.cases(X), ]
    if (nrow(X) < 3) return()
    M <- suppressWarnings(cor(X, method = input$corr_method))
    ggcorrplot::ggcorrplot(M, lab = TRUE) + theme_minimal(base_family = "Inter")
  })

  output$map_co2 <- renderLeaflet({
    df <- filtered_data() %>% group_by(Organization) %>% summarise(Total_CO2 = sum(CO2_Tons), .groups = 'drop')
    org_loc <- tibble::tibble(
      Organization = c("OpenAI","DeepMind","Google","Meta","Anthropic","NVIDIA"),
      lat = c(37.7749, 51.5074, 37.4220, 37.4848, 37.7749, 37.3700),
      lon = c(-122.4194, -0.1278, -122.0841, -122.1484, -122.4194, -121.9669)
    )
    m <- df %>% left_join(org_loc, by = "Organization") %>% mutate(r = pmax(3, sqrt(pmax(Total_CO2,0))/10)) %>%
      filter(is.finite(lat), is.finite(lon))
    leaflet(m) %>% addTiles() %>% addCircleMarkers(~lon, ~lat, radius = ~r,
      color = "#e53935", stroke = FALSE, fillOpacity = 0.6,
      label = ~paste0(Organization, ": ", scales::comma(round(Total_CO2)), " tons"))
  })

  output$ml_importance <- renderPlotly({
    df <- filtered_data()
    validate(need(nrow(df) >= 20, "Not enough rows to train (need >= 20)"))
    dat <- df %>% select(CO2_Tons, Parameters_Billion, EnergyUsed_MWh, Organization) %>% na.omit()
    dat$Organization <- factor(dat$Organization)
    importance <- NULL
    if (requireNamespace("ranger", quietly = TRUE)) {
      fit <- ranger::ranger(CO2_Tons ~ ., data = dat, importance = 'impurity', num.trees = 300, seed = 42)
      im <- sort(fit$variable.importance, decreasing = TRUE)
      importance <- tibble::tibble(Feature = names(im), Importance = as.numeric(im))
      output$ml_summary <- renderText({ paste("OOB R2:", round(fit$r.squared, 3)) })
    } else {
      fit <- stats::lm(CO2_Tons ~ ., data = dat)
      co <- abs(stats::coef(fit)[-1])
      importance <- tibble::tibble(Feature = names(co), Importance = as.numeric(co)) %>% arrange(desc(Importance))
      output$ml_summary <- renderText({ paste("Adj R2:", round(summary(fit)$adj.r.squared, 3)) })
    }
    p <- ggplot(importance, aes(x = reorder(Feature, Importance), y = Importance, fill = Feature)) +
      geom_col(show.legend = FALSE) + coord_flip() + labs(x = NULL, y = "Importance") + theme_minimal(base_family = "Inter")
    ggplotly(p) %>% add_plotly_config() %>% apply_plotly_theme()
  })

  output$table <- reactable::renderReactable({
    reactable::reactable(filtered_data(), searchable = TRUE, pagination = TRUE, striped = TRUE, resizable = TRUE)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("ai_carbon_filtered_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) readr::write_csv(filtered_data(), file)
  )

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
}

shinyApp(ui, server)
