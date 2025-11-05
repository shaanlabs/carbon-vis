# 🌍 CO2lytics
    
    Visualizing the Energy & Carbon Impact of Artificial Intelligence (2017–2025)
    
    ![R](https://img.shields.io/badge/R-4.5-blue)
    ![Shiny](https://img.shields.io/badge/Shiny-Dashboard-lightblue)
    ![Plotly](https://img.shields.io/badge/Plotly-Interactive-orange)
    ![License](https://img.shields.io/badge/License-MIT-green)
    
    ---
    
    ## 🖥️ Slide 1: Title + Project Introduction
    CO2lytics is a research-grade R Shiny dashboard that analyzes AI model training energy use, CO₂ emissions, and efficiency across major labs (OpenAI, Google DeepMind, Meta, NVIDIA, etc.). It presents interactive, presentation-ready charts to explore sustainability trends in modern AI systems.
    
    ---
    
    ## 🎯 Slide 2: Project Overview (Motivation & Goals)
    - Expose how model scale and compute demand affect carbon emissions and energy usage.
    - Compare organizations on efficiency and footprint over time (2017–2025).
    - Provide a polished dashboard for classroom or conference presentations.
    
    Key questions answered:
    - Are emissions accelerating with larger models?
    - How does efficiency (CO₂ per billion parameters) change over time and across orgs?
    - Which labs contribute the most total emissions?
    
    ---
    
    ## ⚙️ Slide 3: Tech Stack (Logos & Badges)
    - Frontend: Shiny, shinydashboardPlus, bslib
    - Interactivity: Plotly
    - Visualization: ggplot2 → ggplotly
    - Data Processing: dplyr, tidyr, readr, data.table
    - Extras: ggcorrplot, ggrepel, reactable, shinyBS
    
    ```
    R (4.5+) · Shiny · Plotly · ggplot2 · dplyr · tidyr · bslib · shinydashboardPlus · reactable
    ```
    
    ---
    
    ## 🧠 Slide 4: Data Flow & Architecture Diagram
    ```mermaid
    flowchart LR
        A[Datasets: data/*.csv] --> B[data_prep.R: ensure_datasets()]
        B --> C[app.R server: reactive filtering]
        C --> D[visualization.R: ggplot functions]
        D --> E[Plotly interactive charts]
        E --> F[Shiny UI: tabs, value boxes, right sidebar]
    ```
    Supporting files:
    - `R/data_prep.R`: creates/validates CSVs; generates realistic dummy data if missing
    - `R/visualization.R`: ggplot helpers used by the app
    - `R/utils.R`: palette + Plotly config (download buttons)
    - `R/packages.R`: robust, Windows-friendly, binary installs
    
    ---
    
    ## 📊 Slide 5: Key Features
    - Global filters (year range, organization) in a right sidebar
    - Summary cards: Total CO₂, Avg Energy per Model, Largest Model, Total Models
    - Emission trend (line/area) + animated year marker
    - Efficiency scatter/bubble (CO₂ per B params) with label toggle
    - Energy vs Model Size (log–log bubble) + insight text
    - Organization comparison (bar + efficiency boxplot)
    - Distributions & correlations (histogram, boxplot, pie, heatmap)
    - Dataset Explorer with `reactable` and filtered CSV download
    - Consistent theme via `bslib::bs_theme(bootswatch = "flatly", Inter font)`
    
    ---
    
    ## 🧩 Slide 6: Code Breakdown
    ### ui/server structure
    ```r
    # app.R (excerpt)
    ui <- dashboardPage(
      dashboardHeader(title = span(icon("leaf"), "CO2lytics")),
      dashboardSidebar(...),
      dashboardBody(...),
      rightsidebar = rightSidebar(...)
    )
    
    server <- function(input, output, session) {
      df_all <- reactiveVal(load_all_data())
      filtered <- reactive({
        df_all() %>% filter(Year >= input$year_range[1], Year <= input$year_range[2], Organization %in% input$orgs)
      })
      output$emissions_plot <- renderPlotly({
        p <- create_emissions_trend_plot(filtered(), interactive = FALSE)
        ggplotly(p) %>% add_plotly_config()
      })
    }
    ```
    
    ### 🧩 Code Module: Data Preparation
    Cleans, merges, and ensures CSVs exist.
    ```r
    # R/data_prep.R
    ensure_datasets <- function() {
      canon <- "data/ai_carbon_footprint.csv"
      if (!file.exists(canon)) {
        df <- data.frame(Model=c("AlexNet","BERT","GPT-3","GPT-4"),
                         Year=c(2017,2018,2020,2023),
                         Parameters_Billion=c(0.06,0.34,175,1000),
                         EnergyUsed_MWh=c(50,400,1300000,3800000),
                         CO2_Tons=c(5,35,552,5184),
                         Organization=c("University of Toronto","Google","OpenAI","OpenAI"))
        df$Energy_Efficiency <- round(df$Parameters_Billion/df$EnergyUsed_MWh,4)
        df$CO2_per_Parameter <- round(df$CO2_Tons/df$Parameters_Billion,2)
        write.csv(df, canon, row.names = FALSE)
      }
    }
    ```
    
    ### 📈 Plot helpers
    ```r
    # R/visualization.R (excerpt)
    create_emissions_trend_plot <- function(data, interactive = FALSE) {
      yearly <- data %>% group_by(Year) %>% summarise(Total_CO2 = sum(CO2_Tons))
      ggplot(yearly, aes(Year, Total_CO2)) +
        geom_area(fill = "#90caf9", alpha = .5) +
        geom_line(color = "#1e88e5", linewidth = 1.2) +
        labs(title = "Growth of AI Carbon Emissions", y = "Total CO₂ (tons)") +
        theme_minimal()
    }
    ```
    
    ---
    
    ## 🖼️ Slide 7: Dashboard Preview
    > Replace with real screenshots from your outputs directory.
    
    ![Overview](screenshots/overview.png)
    ![Carbon Trend](screenshots/carbon_trend.png)
    ![Efficiency Plot](screenshots/efficiency_plot.png)
    ![Full Dashboard](screenshots/dashboard_full.png)
    
    ---
    
    ## 💡 Slide 8: Insights / Findings
    - Emissions increase aligns with parameter growth; post-2020 escalation is visible.
    - Average CO₂ per billion parameters shows signs of improvement after 2022 for some orgs.
    - Organization-level comparisons reveal which labs dominate total emissions vs. efficiency.
    
    > Example automated insight in app: “Total emissions changed by X% between selected years.”
    
    ---
    
    ## 🧪 Slide 9: How to Run Locally
    Clone and start (Windows PowerShell):
    ```powershell
    # 1) Install dependencies (Windows-friendly, binary installs)
    & "C:\\Program Files\\R\\R-4.5.1\\bin\\R.exe" -e "source('R/packages.R'); setup_packages()"
    
    # 2) Launch the dashboard
    & "C:\\Program Files\\R\\R-4.5.1\\bin\\R.exe" -e "shiny::runApp('app.R', host='127.0.0.1', port=8787, launch.browser=TRUE)"
    ```
    Pipeline (optional):
    ```powershell
    C:\\Progra~1\\R\\R-4.5.1\\bin\\Rscript.exe analysis\\run_all.R
    ```
    Report rendering (requires Quarto/Pandoc):
    ```powershell
    & "C:\\Program Files\\R\\R-4.5.1\\bin\\R.exe" -e "rmarkdown::render('analysis/ai_carbon_analysis.Rmd', output_format='html_document')"
    ```
    
    ---
    
    ## 👥 Slide 10: Contributors, References, and Future Scope
    **Contributors**
    - Project Lead & Developer: Your Name
    
    **References**
    - OpenAI Carbon Reports
    - Google DeepMind Efficiency Papers
    - NVIDIA Sustainability Reports
    
    **Future Scope**
    - Renewable offset comparisons and targets
    - Predictive trends with time-series models
    - Exportable academic report bundles (PDF/HTML) from the app
    
    ---
    
    ## 📁 Project Structure
    ```
    app.R
    R/
      data_prep.R
      visualization.R
      utils.R
      packages.R
    analysis/
      main.R
      outputs/<YYYYMMDD>/
    www/
      style.css
      favicon.ico (optional)
    data/
      ai_carbon_footprint.csv
      model_size.csv
      carbon_footprint.csv
      efficiency.csv
    ```
    
    ---
    
    ## 📌 Notes
    - This repository is designed for GitHub readability with clear slide-like sections and visuals.
    - Screenshots should be placed under `screenshots/` and `assets/` as referenced above.
    - The app name is consistently used as **CO2lytics** across files.
