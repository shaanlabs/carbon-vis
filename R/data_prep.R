# Data prep helpers: ensure required CSVs exist, creating from canonical data or dummy data

ensure_datasets <- function() {
  dir.create("data", showWarnings = FALSE, recursive = TRUE)
  canon <- file.path("data", "ai_carbon_footprint.csv")
  if (!file.exists(canon)) {
    # Fallback to create a small realistic dataset
    df <- data.frame(
      Model = c("AlexNet","BERT","GPT-2","GPT-3","LLaMA","GPT-4"),
      Year = c(2017,2018,2019,2020,2023,2023),
      Parameters_Billion = c(0.06,0.34,1.5,175,65,1000),
      EnergyUsed_MWh = c(50,400,1200,1300000,900000,3800000),
      CO2_Tons = c(5,35,120,552,360,5184),
      Organization = c("University of Toronto","Google","OpenAI","OpenAI","Meta","OpenAI"),
      Training_Location = c("Canada","USA","USA","USA","USA","USA")
    )
    df$Energy_Efficiency <- round(df$Parameters_Billion / df$EnergyUsed_MWh, 4)
    df$CO2_per_Parameter <- round(df$CO2_Tons / df$Parameters_Billion, 2)
    utils::write.csv(df, canon, row.names = FALSE)
  }
  df <- tryCatch(read.csv(canon, check.names = FALSE), error = function(e) NULL)
  if (is.null(df)) return(invisible(FALSE))

  # model_size.csv
  ms_path <- file.path("data","model_size.csv")
  if (!file.exists(ms_path)) {
    ms <- df[, c("Model","Year","Parameters_Billion","Organization")]
    utils::write.csv(ms, ms_path, row.names = FALSE)
  }
  # carbon_footprint.csv
  cf_path <- file.path("data","carbon_footprint.csv")
  if (!file.exists(cf_path)) {
    cf <- df[, c("Model","Year","CO2_Tons","Organization")]
    utils::write.csv(cf, cf_path, row.names = FALSE)
  }
  # efficiency.csv
  eff_path <- file.path("data","efficiency.csv")
  if (!file.exists(eff_path)) {
    if (!"CO2_per_Parameter" %in% names(df)) {
      df$CO2_per_Parameter <- round(df$CO2_Tons / df$Parameters_Billion, 2)
    }
    eff <- df[, c("Model","Year","CO2_per_Parameter","Energy_Efficiency","Organization")]
    utils::write.csv(eff, eff_path, row.names = FALSE)
  }
  invisible(TRUE)
}
