# ==============================================================================
# MAIN SCRIPT: FINAL BACKTEST
# ==============================================================================

library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)
library(scales)
library(ggthemes)
library(lubridate)
library(TTR)
library(RColorBrewer)
library(tidyr) 

source("funciones_vistas.R")
source("funciones_core.R")
source("Zscore.R")

# DATA
# ------------------------------------------------------------------------------
tickers <- c(
  "AAPL", "MSFT", "NVDA", "AMD", "ADBE", "GOOGL", "META", "NFLX", "DIS",
  "AMZN", "TSLA", "SBUX", "NKE", "WMT", "KO", "COST",
  "JPM", "V", "BAC", "GS",
  "JNJ", "UNH", "PFE", "ABBV",
  "BA", "FDX", "LMT", "XOM", "CVX", "VZ")

if(exists("ranking_final")){
  ranking_final$Ticker[ranking_final$Ticker == "SQ"] <- "XYZ"
}

cat("--- DESCARGANDO DATOS ---\n")
datos_env <- new.env()
getSymbols(tickers, src = "yahoo", from = "2023-01-01", env = datos_env)

precios_list <- lapply(tickers, function(x) Ad(get(x, envir = datos_env)))
precios_all <- do.call(merge, precios_list)
colnames(precios_all) <- tickers
retornos_all <- na.omit(Return.calculate(precios_all, method = "log"))

# BACKTEST CONFIGURATION
# ------------------------------------------------------------------------------
fechas_fin_mes <- unique(as.Date(time(to.monthly(retornos_all["2025-1/2025-12"]))))

historial_pesos_mpt <- list()
historial_pesos_sma <- list()
historial_pesos_ml  <- list()

w_ml_prev <- rep(0, length(tickers))
lambda_ml <- 0.7 
peso_maximo <- 0.3 
risk_aversion <- 2

# WALK-FORWARD LOOP FOR DYNAMIC MODELS
# ------------------------------------------------------------------------------
for(i in 1:length(fechas_fin_mes)) {
  fecha_actual <- fechas_fin_mes[i]
  cat(sprintf("[%d/%d] Rebalanceo Dinámico: %s ... \n", i, length(fechas_fin_mes), fecha_actual))
  
  subset_precios <- precios_all[paste0("/", fecha_actual)]
  subset_retornos <- retornos_all[paste0("/", fecha_actual)]
  
  mu_hist <- colMeans(subset_retornos) * 252
  sigma_hist <- cov(subset_retornos) * 252
  w_eq <- rep(1/length(tickers), length(tickers)) 
  
  w_mpt <- optimizar_cartera(mu_hist, sigma_hist, risk_aversion, max_weight = peso_maximo)
  names(w_mpt) <- tickers
  
  vistas_sma <- generar_vistas_sma(subset_precios, subset_retornos, window = 200)
  mu_bl_sma <- calcular_mu_bl(sigma_hist, w_eq, risk_aversion, vistas_obj = vistas_sma)
  w_sma <- optimizar_cartera(mu_bl_sma, sigma_hist, risk_aversion, max_weight = peso_maximo)
  names(w_sma) <- tickers
  
  capture.output({ vistas_ml <- generar_vistas_ml(subset_precios, subset_retornos, window = 500) })
  mu_bl_ml <- calcular_mu_bl(sigma_hist, w_eq, risk_aversion, vistas_obj = vistas_ml)
  w_ml_raw <- optimizar_cartera(mu_bl_ml, sigma_hist, risk_aversion, max_weight = peso_maximo)
  names(w_ml_raw) <- tickers
  
  w_ml <- if(i > 1) (lambda_ml * w_ml_prev) + ((1 - lambda_ml) * w_ml_raw) else w_ml_raw
  w_ml_prev <- w_ml 
  
  historial_pesos_mpt[[i]] <- xts(t(w_mpt), order.by = fecha_actual)
  historial_pesos_sma[[i]] <- xts(t(w_sma), order.by = fecha_actual)
  historial_pesos_ml[[i]]  <- xts(t(w_ml),  order.by = fecha_actual)
}

# FUNDAMENTAL MODEL
# ------------------------------------------------------------------------------
cat("\n--- CALCULANDO MODELO FUNDAMENTAL (BUY & HOLD) ---\n")
sigma_init <- cov(tail(retornos_all["/2024-12-31"], 500)) * 252
vistas_fund_fixed <- generar_vistas_fundamentales(ranking_final, tickers, n_picks = 10)
mu_bl_fund_fixed  <- calcular_mu_bl(sigma_init, rep(1/length(tickers), length(tickers)), 
                                    risk_aversion, vistas_obj = vistas_fund_fixed)
w_fund_static <- optimizar_cartera(mu_bl_fund_fixed, sigma_init, risk_aversion, max_weight = 0.25)
names(w_fund_static) <- tickers

# AGGREGATION AND RESULTS
# ------------------------------------------------------------------------------
preparar_pesos <- function(lista) {
  df <- do.call(rbind, lista)
  return(df / rowSums(df))
}

w_mpt_all <- preparar_pesos(historial_pesos_mpt)
w_sma_all <- preparar_pesos(historial_pesos_sma)
w_ml_all  <- preparar_pesos(historial_pesos_ml)

ret_2025 <- retornos_all["2025"]

port_mpt  <- Return.portfolio(ret_2025, weights = w_mpt_all)
port_sma  <- Return.portfolio(ret_2025, weights = w_sma_all)
port_ml   <- Return.portfolio(ret_2025, weights = w_ml_all)
port_fund <- Return.portfolio(ret_2025, weights = w_fund_static)

comparativa <- merge(port_mpt, port_sma, port_ml, port_fund)
colnames(comparativa) <- c("Markowitz", "BL_Trend", "BL_MachineLearning", "BL_Fundamental_Value")
comparativa <- na.omit(comparativa)

# COLOR AND DATA CONFIGURATION
# ------------------------------------------------------------------------------
n_activos_total <- length(tickers)
paleta_universal <- colorRampPalette(brewer.pal(12, "Paired"))(n_activos_total)
names(paleta_universal) <- sort(tickers)

# PLOT A: CUMULATIVE RETURN
# ------------------------------------------------------------------------------
df_cum <- data.frame(Fecha = index(comparativa), cumsum(comparativa)) %>%
  pivot_longer(cols = -Fecha, names_to = "Modelo", values_to = "Retorno")

g_rendimiento <- ggplot(df_cum, aes(x = Fecha, y = Retorno, color = Modelo)) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Markowitz" = "#95a5a6", 
                                "BL_Trend" = "#f39c12", 
                                "BL_MachineLearning" = "#2c3e50", 
                                "BL_Fundamental_Value" = "#27ae60")) +
  theme_fivethirtyeight() +
  labs(title = "Comparativa de Rendimiento 2025", 
       subtitle = "IA vs Tendencia vs Fundamental vs Markowitz",
       y = "Retorno Acumulado")
print(g_rendimiento)

# PLOT B: FUNDAMENTAL MODEL
# ------------------------------------------------------------------------------
df_pie_fund <- data.frame(Activo = names(w_fund_static), Peso = as.numeric(w_fund_static)) %>%
  filter(Peso > 0) %>% 
  mutate(Etiqueta = paste0(Activo, "\n", percent(Peso, accuracy = 0.1)))

g_pie <- ggplot(df_pie_fund, aes(x = "", y = Peso, fill = Activo)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = paleta_universal) +
  theme_void() + 
  theme(legend.position = "right", legend.text = element_text(size = 8)) +
  labs(title = "Cartera Fundamental: Composición Inicial",
       subtitle = "Estrategia Buy & Hold (Selección de 10-15 activos)")
print(g_pie)

# PLOT C: WEIGHT EVOLUTION FOR DYNAMIC MODELS
# ------------------------------------------------------------------------------
graficar_barras_pesos <- function(w_matrix, titulo) {
  df_plot <- data.frame(Fecha = index(w_matrix), coredata(w_matrix)) %>%
    pivot_longer(cols = -Fecha, names_to = "Activo", values_to = "Peso") %>%
    filter(Peso > 0)
  
  ggplot(df_plot, aes(x = Fecha, y = Peso, fill = Activo)) +
    geom_col(position = "stack", width = 20, color = "white", linewidth = 0.1) +
    scale_y_continuous(labels = percent, limits = c(0, 1.001), expand = c(0,0)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_fill_manual(values = paleta_universal) +
    theme_fivethirtyeight() +
    theme(legend.position = "right", 
          legend.text = element_text(size = 6),
          panel.grid.major.x = element_blank()) +
    labs(title = titulo, 
         subtitle = "Evolución mensual (Restricción: 10-15 activos, mín. 2%)", 
         y = "Peso Relativo", x = "")
}

print(graficar_barras_pesos(w_ml_all, "Cartera Dinámica: BL Machine Learning"))
print(graficar_barras_pesos(w_sma_all, "Cartera Dinámica: BL Tendencia"))
print(graficar_barras_pesos(w_mpt_all, "Cartera Dinámica: Markowitz"))
