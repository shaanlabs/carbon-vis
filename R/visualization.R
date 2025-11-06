#' AI Carbon Footprint Visualization Functions
#' This script contains functions for creating visualizations of AI model environmental impact
#' Last updated: October 29, 2025

#' Create theme for consistent visualization styling
#' @return A ggplot2 theme object
create_custom_theme <- function() {
    theme_minimal(base_size = 13) +
        theme(
            plot.title = element_text(face = "bold", size = 15),
            plot.subtitle = element_text(size = 12, color = "gray40"),
            axis.title = element_text(size = 12),
            legend.title = element_text(size = 11),
            legend.text = element_text(size = 10),
            panel.grid.minor = element_line(color = "gray95"),
            panel.grid.major = element_line(color = "gray90")
        )
}

#' Create Carbon Footprint Bar Plot
#' @param data The AI model dataset
#' @param interactive Boolean to determine if plot should be interactive
#' @return A ggplot2 object
create_carbon_footprint_plot <- function(data, interactive = FALSE) {
    p <- ggplot(data, aes(x = reorder(Model, CO2_Tons), y = CO2_Tons, fill = Organization)) +
        geom_col(show.legend = TRUE, width = 0.7) +
        geom_text(aes(label = paste0(format(CO2_Tons, big.mark = ","), " tons")),
                 hjust = -0.1, size = 3) +
        coord_flip() +
        labs(
            title = "🌍 Carbon Footprint of Major AI Models",
            subtitle = "Estimated CO₂ emissions during model training (2017–2025)",
            x = "AI Model",
            y = "CO₂ Emitted (tons)",
            caption = "Data updated: October 29, 2025"
        ) +
        scale_fill_brewer(palette = "Set2") +
        scale_y_continuous(labels = scales::comma) +
        create_custom_theme()
    
    if (interactive) {
        return(ggplotly(p))
    }
    return(p)
}

#' Create Model Size vs Energy Usage Plot
#' @param data The AI model dataset
#' @param interactive Boolean to determine if plot should be interactive
#' @param show_labels Boolean to determine if labels should be shown
#' @return A ggplot2 object
create_model_size_energy_plot <- function(data, interactive = TRUE, show_labels = TRUE) {
    p <- ggplot(data, aes(x = Parameters_Billion, y = EnergyUsed_MWh,
                         color = Organization, size = CO2_Tons)) +
        geom_point(alpha = 0.8) +
        { if (isTRUE(show_labels)) ggrepel::geom_text_repel(aes(label = Model), size = 3, max.overlaps = 20) } +
        scale_x_log10(labels = scales::comma_format()) +
        scale_y_log10(labels = scales::comma_format()) +
        scale_size_continuous(labels = scales::comma_format()) +
        labs(
            title = "⚛️ AI Model Size vs Energy Used",
            subtitle = "Larger models consume exponentially more energy",
            x = "Model Size (Billions of Parameters, log scale)",
            y = "Energy Used (MWh, log scale)",
            size = "CO₂ Emissions (tons)",
            caption = "Data updated: October 29, 2025"
        ) +
        create_custom_theme() +
        theme(legend.position = "bottom")
    
    if (interactive) {
        return(ggplotly(p))
    }
    return(p)
}

#' Create Emissions Trend Plot
#' @param data The AI model dataset
#' @param interactive Boolean to determine if plot should be interactive
#' @return A ggplot2 object
create_emissions_trend_plot <- function(data, interactive = FALSE) {
    # Aggregate data by year
    yearly_data <- data %>%
        group_by(Year) %>%
        summarise(
            Total_CO2 = sum(CO2_Tons),
            Models_Count = n(),
            Avg_CO2 = mean(CO2_Tons)
        )
    
    p <- ggplot(yearly_data, aes(x = Year)) +
        geom_line(aes(y = Total_CO2), color = "darkgreen", linewidth = 1.3) +
        geom_point(aes(y = Total_CO2, text = paste(
            "Year:", Year,
            "\nTotal CO₂:", format(Total_CO2, big.mark = ","), "tons",
            "\nModels Released:", Models_Count,
            "\nAvg CO₂/Model:", format(round(Avg_CO2), big.mark = ","), "tons"
        )), size = 4, color = "forestgreen") +
        labs(
            title = "📈 Growth of AI Carbon Emissions (2017–2025)",
            subtitle = "AI models are becoming exponentially more carbon-intensive",
            x = "Year",
            y = "Total CO₂ Emitted (tons)",
            caption = "Data updated: October 29, 2025"
        ) +
        scale_y_continuous(labels = scales::comma_format()) +
        create_custom_theme()
    
    if (interactive) {
        return(ggplotly(p))
    }
    return(p)
}

#' Create Energy Efficiency Analysis Plot
#' @param data The AI model dataset
#' @param show_labels Boolean to determine if labels should be shown
#' @return A ggplot2 object
create_efficiency_plot <- function(data, show_labels = TRUE) {
    data %>%
        mutate(Efficiency = CO2_Tons / Parameters_Billion) %>%
        ggplot(aes(x = Year, y = Efficiency, color = Organization)) +
        geom_point(size = 3) +
        { if (isTRUE(show_labels)) ggrepel::geom_text_repel(aes(label = Model), size = 3) } +
        labs(
            title = "🎯 AI Model Training Efficiency Over Time",
            subtitle = "CO₂ emissions per billion parameters",
            x = "Year",
            y = "CO₂ Tons per Billion Parameters",
            caption = "Data updated: October 29, 2025"
        ) +
        scale_y_log10(labels = scales::comma_format()) +
        create_custom_theme() +
        theme(legend.position = "bottom")
}