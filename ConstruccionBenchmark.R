# ==============================================================================
# DYNAMIC BENCHMARK RECONSTRUCTION
# ==============================================================================

library(PerformanceAnalytics)
library(quantmod)
library(dplyr)
library(tidyr)
library(zoo)
library(ggplot2)
library(ggthemes)
load(".RData")

# INITIAL DATA LOAD
# ------------------------------------------------------------------------------
message("[INFO] Iniciando reconstrucción desde Tabla de Market Cap...")

datos_cap_hoy <- read.csv("MarketCap_Scraped.csv", stringsAsFactors = FALSE)

tickers_estudio <- colnames(ret_2025)

# OBTAINING PRICES
# ------------------------------------------------------------------------------
precios_full <- do.call(merge, lapply(tickers_estudio, function(tk) {
  Ad(getSymbols(tk, from = "2025-01-01", to = "2026-05-02", auto.assign = FALSE))
}))
colnames(precios_full) <- tickers_estudio
precios_full <- na.locf(precios_full)

# HISTORICAL MARKET CAP INDUCTION
# ------------------------------------------------------------------------------
precio_final <- as.numeric(tail(precios_full, 1))
ratio_precios <- sweep(precios_full, 2, precio_final, FUN = "/")

mc_hoy_vector <- datos_cap_hoy$MarketCap_Billions[match(tickers_estudio, datos_cap_hoy$Ticker)]
mc_historica <- sweep(ratio_precios, 2, mc_hoy_vector, FUN = "*")

# MONTHLY WEIGHTS UPDATE
# ------------------------------------------------------------------------------
ep <- endpoints(mc_historica["2025"], on = "months")
mc_mensual <- mc_historica[ep]

pesos_mensuales <- mc_mensual / rowSums(mc_mensual, na.rm = TRUE)

# DAILY BENCHMARK CONSTRUCTION
# ------------------------------------------------------------------------------
ret_limpios <- na.omit(ret_2025)

vacio <- xts(order.by = index(ret_limpios))
pesos_diarios <- merge(vacio, pesos_mensuales)

pesos_diarios <- na.locf(pesos_diarios)
pesos_diarios <- na.locf(pesos_diarios, fromLast = TRUE)
pesos_diarios <- pesos_diarios[index(ret_limpios), tickers_estudio]

bench_vector <- rowSums(ret_limpios * pesos_diarios, na.rm = TRUE)
port_bench_dinamico <- xts(bench_vector, order.by = index(ret_limpios))
colnames(port_bench_dinamico) <- "Benchmark_Market"

# ==============================================================================
# FINAL MERGE AND COMPARISON
# ==============================================================================
message("[INFO] Uniendo estrategias y Benchmark USA30...")

p1 <- as.xts(port_mpt)[, 1]
p2 <- as.xts(port_sma)[, 1]
p3 <- as.xts(port_ml)[, 1]
p4 <- as.xts(port_fund)[, 1]
p5 <- as.xts(port_bench_dinamico)[, 1]

comparativa <- merge(p1, p2, p3, p4, p5, join = "inner")

colnames(comparativa) <- c("Markowitz", 
                           "BL_Trend", 
                           "BL_MachineLearning", 
                           "BL_Fundamental_Value", 
                           "USA30")

# METRICS AND FINAL PLOT
# ------------------------------------------------------------------------------

# CAPM STATISTICS CALCULATION
stats <- table.CAPM(comparativa[, 1:4], comparativa$USA30, Rf = 0)
print(stats)

# COLOR PALETTE CONFIGURATION
colores_tfm <- c(
  "Markowitz"            = "#95a5a6", 
  "BL_Trend"             = "#f39c12", 
  "BL_MachineLearning"   = "#2c3e50", 
  "BL_Fundamental_Value" = "#27ae60",
  "USA30"                = "#e74c3c" 
)

# DATA PREPARATION
df_cum <- data.frame(Fecha = index(comparativa), cumsum(comparativa)) %>%
  pivot_longer(cols = -Fecha, names_to = "Modelo", values_to = "Retorno")

# FINAL PLOT GENERATION
g_rendimiento <- ggplot(df_cum, aes(x = Fecha, y = Retorno, color = Modelo)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = colores_tfm) +
  theme_fivethirtyeight() +
  labs(
    title = "EVALUACIÓN FINAL: MODELOS BL VS USA30", 
    subtitle = "IA vs Tendencia vs Fundamental vs Markowitz (Rendimiento Acumulado 2025)",
    y = "Retorno Acumulado"
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 9),
    plot.title = element_text(face = "bold", size = 14)
  )

print(g_rendimiento)

message("=== SCRIPT COMPLETADO EXITOSAMENTE: USA30 GENERADO ===")