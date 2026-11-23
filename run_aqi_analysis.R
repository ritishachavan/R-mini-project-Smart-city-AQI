# ==============================================================================
# SMART CITY URBAN AIR QUALITY FORECASTING & HEALTH RISK MAPPING
# Dataset: aqi_dataset.csv (14,235 rows; 6 columns)
# ==============================================================================

# ------------------------------------------------------------------------------
# STEP 1: INSTALL & LOAD REQUIRED PACKAGES
# ------------------------------------------------------------------------------
packages <- c(
  "tidyverse", "corrplot", "randomForest", "forecast",
  "zoo", "viridis", "scales", "gridExtra"
)

missing_pkgs <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(missing_pkgs) > 0) {
  install.packages(missing_pkgs, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(corrplot)
  library(randomForest)
  library(forecast)
  library(zoo)
  library(viridis)
  library(scales)
  library(gridExtra)
})

dir.create("outputs", showWarnings = FALSE)
set.seed(42)

cat("=== STEP 1: Packages loaded. Reading aqi_dataset.csv ===\n")

# ------------------------------------------------------------------------------
# STEP 2: LOAD & CLEAN THE DATASET
# ------------------------------------------------------------------------------
raw_df <- read_csv("aqi_dataset.csv", show_col_types = FALSE)

cat("Raw dimensions:", nrow(raw_df), "rows x", ncol(raw_df), "columns\n")
cat("Missing values before cleaning:\n")
print(colSums(is.na(raw_df)))

# Clean physically invalid values & impute missing predictors using median
clean_df <- raw_df %>%
  mutate(
    # Clamp physically impossible values
    AQI = ifelse(AQI < 0, NA, AQI),
    wind_speed = ifelse(wind_speed < 0, NA, wind_speed),
    humidity = ifelse(humidity < 0 | humidity > 100, NA, humidity),
    industrial_activity = ifelse(industrial_activity < 0, NA, industrial_activity),
    temperature = ifelse(temperature < -30 | temperature > 60, NA, temperature)
  ) %>%
  mutate(
    across(c(temperature, humidity, wind_speed, industrial_activity, AQI),
           ~ ifelse(is.na(.), median(., na.rm = TRUE), .)),
    traffic_density = ifelse(is.na(traffic_density),
                             as.numeric(names(sort(table(traffic_density), decreasing = TRUE))[1]),
                             traffic_density)
  ) %>%
  mutate(
    obs_id = row_number(),
    aqi_category = factor(
      case_when(
        AQI <= 50  ~ "Good (0-50)",
        AQI <= 100 ~ "Moderate (51-100)",
        AQI <= 150 ~ "Unhealthy for Sensitive (101-150)",
        AQI <= 200 ~ "Unhealthy (151-200)",
        AQI <= 300 ~ "Very Unhealthy (201-300)",
        TRUE       ~ "Hazardous (300+)"
      ),
      levels = c(
        "Good (0-50)", "Moderate (51-100)",
        "Unhealthy for Sensitive (101-150)", "Unhealthy (151-200)",
        "Very Unhealthy (201-300)", "Hazardous (300+)"
      )
    )
  )

cat("\nMissing values after cleaning:", sum(is.na(clean_df)), "\n")
cat("Cleaned AQI summary:\n")
print(summary(clean_df$AQI))

write_csv(clean_df, "outputs/cleaned_aqi_dataset.csv")

# ------------------------------------------------------------------------------
# STEP 3: EXPLORATORY DATA ANALYSIS (EDA) & GRAPHS
# ------------------------------------------------------------------------------
cat("\n=== STEP 3: Generating EDA charts ===\n")

# Graph 1: AQI Distribution & Health Category Breakdown
p1_hist <- ggplot(clean_df, aes(x = AQI)) +
  geom_histogram(bins = 45, fill = "#2B5C8F", color = "white", alpha = 0.9) +
  geom_vline(aes(xintercept = mean(AQI)), color = "#D95F02", linetype = "dashed", linewidth = 1) +
  geom_vline(aes(xintercept = median(AQI)), color = "#1B9E77", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Distribution of Air Quality Index (AQI)",
    subtitle = sprintf("Mean: %.1f (orange dashed) | Median: %.1f (green dotted)", mean(clean_df$AQI), median(clean_df$AQI)),
    x = "AQI", y = "Frequency"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p1_cat <- clean_df %>%
  count(aqi_category) %>%
  mutate(pct = n / sum(n) * 100) %>%
  ggplot(aes(x = reorder(aqi_category, pct), y = pct, fill = aqi_category)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  scale_y_continuous(limits = c(0, max(clean_df %>% count(aqi_category) %>% pull(n) / nrow(clean_df) * 100) + 10)) +
  scale_fill_viridis_d(option = "plasma", end = 0.85) +
  labs(
    title = "Share of Observations by AQI Category",
    x = "", y = "Percentage of Dataset (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p1_combined <- grid.arrange(p1_hist, p1_cat, ncol = 2)
ggsave("outputs/01_aqi_distribution_and_categories.png", p1_combined, width = 12, height = 5.5, dpi = 300)

# Graph 2: Correlation Matrix of All Numeric Variables
png("outputs/02_correlation_matrix.png", width = 800, height = 750, res = 120)
num_cols <- clean_df %>%
  select(temperature, humidity, wind_speed, traffic_density, industrial_activity, AQI)
cor_matrix <- cor(num_cols)
corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 35,
  col = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(200),
  title = "Correlation Matrix: Weather, Activity & AQI",
  mar = c(0, 0, 2, 0)
)
dev.off()

# Graph 3: AQI vs Traffic Density & Industrial Activity
p3_traffic <- clean_df %>%
  group_by(traffic_density) %>%
  summarise(mean_aqi = mean(AQI), sd_aqi = sd(AQI), .groups = "drop") %>%
  ggplot(aes(x = factor(traffic_density), y = mean_aqi)) +
  geom_col(fill = "#D95F02", width = 0.65) +
  geom_errorbar(aes(ymin = pmax(0, mean_aqi - sd_aqi), ymax = mean_aqi + sd_aqi),
                width = 0.2, color = "gray25") +
  labs(
    title = "Mean AQI by Traffic Density Level (1-10)",
    subtitle = "Error bars show +/- 1 SD",
    x = "Traffic Density", y = "Mean AQI"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p3_scatter <- ggplot(clean_df, aes(x = industrial_activity, y = AQI, color = traffic_density)) +
  geom_point(alpha = 0.25, size = 1.2) +
  geom_smooth(method = "lm", color = "black", linewidth = 1, se = TRUE) +
  scale_color_viridis_c(name = "Traffic\nDensity") +
  labs(
    title = "AQI vs. Industrial Activity",
    subtitle = "Colored by traffic density with linear trendline",
    x = "Industrial Activity Index", y = "AQI"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

p3_combined <- grid.arrange(p3_traffic, p3_scatter, ncol = 2)
ggsave("outputs/03_traffic_and_industrial_impact.png", p3_combined, width = 12, height = 5.5, dpi = 300)

# ------------------------------------------------------------------------------
# STEP 4: FORECASTING MODELS (ARIMA + RANDOM FOREST)
# ------------------------------------------------------------------------------
cat("\n=== STEP 4: Training ARIMA and Random Forest forecasting models ===\n")

model_df <- clean_df %>%
  arrange(obs_id) %>%
  mutate(
    aqi_lag1  = lag(AQI, 1),
    aqi_lag2  = lag(AQI, 2),
    aqi_lag3  = lag(AQI, 3),
    aqi_roll5 = rollmean(AQI, k = 5, fill = NA, align = "right")
  ) %>%
  drop_na()

# 80% Train / 20% Test split
split_row <- floor(0.80 * nrow(model_df))
train_data <- model_df[1:split_row, ]
test_data  <- model_df[(split_row + 1):nrow(model_df), ]

# Model 1: ARIMA on AQI
arima_fit <- auto.arima(train_data$AQI, seasonal = FALSE, stepwise = TRUE, approximation = TRUE)
arima_pred <- forecast(arima_fit, h = nrow(test_data))
test_data$pred_arima <- as.numeric(arima_pred$mean)

# Model 2: Random Forest Regressor
rf_fit <- randomForest(
  AQI ~ aqi_lag1 + aqi_lag2 + aqi_lag3 + aqi_roll5 +
    temperature + humidity + wind_speed + traffic_density + industrial_activity,
  data = train_data,
  ntree = 200,
  importance = TRUE
)
test_data$pred_rf <- predict(rf_fit, newdata = test_data)

# Model Performance Metrics
metrics_tbl <- tibble(
  Model = c("ARIMA", "Random Forest"),
  RMSE = c(
    sqrt(mean((test_data$AQI - test_data$pred_arima)^2)),
    sqrt(mean((test_data$AQI - test_data$pred_rf)^2))
  ),
  MAE = c(
    mean(abs(test_data$AQI - test_data$pred_arima)),
    mean(abs(test_data$AQI - test_data$pred_rf))
  ),
  R2 = c(
    cor(test_data$AQI, test_data$pred_arima)^2,
    cor(test_data$AQI, test_data$pred_rf)^2
  )
)

print(metrics_tbl)
write_csv(metrics_tbl, "outputs/forecast_model_metrics.csv")

# Graph 4: Actual vs Predicted AQI (First 200 test observations for clear visibility)
plot_window <- test_data %>%
  slice_head(n = 200) %>%
  select(obs_id, Actual = AQI, `Random Forest` = pred_rf, `ARIMA` = pred_arima) %>%
  pivot_longer(-obs_id, names_to = "Series", values_to = "AQI_Val")

p4_forecast <- ggplot(plot_window, aes(x = obs_id, y = AQI_Val, color = Series, linetype = Series)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c("Actual" = "#111111", "Random Forest" = "#1B9E77", "ARIMA" = "#D95F02")) +
  scale_linetype_manual(values = c("Actual" = "solid", "Random Forest" = "solid", "ARIMA" = "dashed")) +
  labs(
    title = "Actual vs. Forecasted AQI (Test Window Sample: 200 Observations)",
    subtitle = sprintf("Random Forest RMSE: %.2f (R2: %.2f) | ARIMA RMSE: %.2f (R2: %.2f)",
                       metrics_tbl$RMSE[2], metrics_tbl$R2[2], metrics_tbl$RMSE[1], metrics_tbl$R2[1]),
    x = "Observation Index", y = "AQI", color = "", linetype = ""
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"))

ggsave("outputs/04_actual_vs_forecast_aqi.png", p4_forecast, width = 11, height = 5.5, dpi = 300)

# Graph 5: Random Forest Variable Importance
imp_tbl <- as.data.frame(importance(rf_fit)) %>%
  rownames_to_column("Variable") %>%
  arrange(desc(`%IncMSE`))

p5_imp <- ggplot(imp_tbl, aes(x = reorder(Variable, `%IncMSE`), y = `%IncMSE`)) +
  geom_col(fill = "#2B5C8F", width = 0.65) +
  coord_flip() +
  labs(
    title = "Random Forest Feature Importance",
    subtitle = "Higher %IncMSE indicates stronger predictive contribution to AQI",
    x = "", y = "% Increase in MSE"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/05_random_forest_feature_importance.png", p5_imp, width = 9, height = 5, dpi = 300)

# ------------------------------------------------------------------------------
# STEP 5: HEALTH RISK SCORING & SPATIAL EXPOSURE RISK MAP
# ------------------------------------------------------------------------------
cat("\n=== STEP 5: Computing Health Risk Score & Spatial Risk Map ===\n")

# Health Risk Score (0-100): combines normalized AQI hazard (65%) +
# anthropogenic emission pressure from traffic & industrial activity (35%)
risk_df <- clean_df %>%
  mutate(
    aqi_norm = rescale(AQI, to = c(0, 1)),
    emission_norm = rescale(0.5 * rescale(traffic_density, to = c(0, 1)) +
                              0.5 * rescale(industrial_activity, to = c(0, 1)), to = c(0, 1)),
    health_risk_score = round((0.65 * aqi_norm + 0.35 * emission_norm) * 100, 1),
    risk_level = factor(
      case_when(
        health_risk_score >= 70 ~ "High Risk",
        health_risk_score >= 45 ~ "Moderate Risk",
        TRUE                    ~ "Low Risk"
      ),
      levels = c("Low Risk", "Moderate Risk", "High Risk")
    )
  )

# Summary table by Traffic Density tier
traffic_risk_summary <- risk_df %>%
  group_by(traffic_density) %>%
  summarise(
    records = n(),
    mean_aqi = round(mean(AQI), 1),
    mean_industrial = round(mean(industrial_activity), 1),
    mean_health_risk = round(mean(health_risk_score), 1),
    high_risk_pct = round(mean(risk_level == "High Risk") * 100, 1),
    .groups = "drop"
  )

print(traffic_risk_summary)
write_csv(traffic_risk_summary, "outputs/traffic_health_risk_summary.csv")

# Graph 6: 2D Spatial Health Risk Heatmap across Traffic Density & Industrial Activity
# (Acts as an urban exposure risk surface since lon/lat are not in aqi_dataset.csv)
grid_surface <- expand_grid(
  traffic_seq = seq(min(risk_df$traffic_density), max(risk_df$traffic_density), length.out = 80),
  ind_seq = seq(min(risk_df$industrial_activity), max(risk_df$industrial_activity), length.out = 80)
) %>%
  mutate(
    pred_risk_score = predict(
      lm(health_risk_score ~ poly(traffic_seq, 2) + poly(ind_seq, 2), data = risk_df %>%
           rename(traffic_seq = traffic_density, ind_seq = industrial_activity)),
      newdata = .
    )
  )

p6_heatmap <- ggplot() +
  geom_raster(data = grid_surface, aes(x = traffic_seq, y = ind_seq, fill = pred_risk_score), interpolate = TRUE) +
  geom_contour(data = grid_surface, aes(x = traffic_seq, y = ind_seq, z = pred_risk_score),
               color = "white", alpha = 0.5, bins = 7) +
  scale_fill_viridis_c(option = "inferno", direction = -1, name = "Health Risk\nScore (0-100)") +
  labs(
    title = "Urban Health Risk Mapping Surface (Traffic vs. Industrial Pressure)",
    subtitle = "Interpolated risk surface from cleaned AQI and emission intensity",
    x = "Traffic Density (1 - 10)",
    y = "Industrial Activity Index"
  ) +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold"))

ggsave("outputs/06_health_risk_mapping_surface.png", p6_heatmap, width = 10, height = 6.5, dpi = 300)

cat("\n=== ALL STEPS COMPLETED! Check the 'outputs/' folder for CSVs and PNG graphs. ===\n")