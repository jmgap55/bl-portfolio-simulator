# SCORING SCRIPT (FUNDAMENTAL MODEL)

library(jsonlite)
library(tidyverse)
library(ggthemes)

tickers_usa <- c(
  "AAPL", "MSFT", "NVDA", "AMD", "ADBE", "GOOGL", "META", "NFLX", "DIS",
  "AMZN", "TSLA", "SBUX", "NKE", "WMT", "KO", "COST",
  "JPM", "V", "BAC", "GS",
  "JNJ", "UNH", "PFE", "ABBV",
  "BA", "FDX", "LMT", "XOM", "CVX", "VZ"
)

df_2024_raw<-read.csv("df_2024_raw.csv")

# SCORING ENGINE: UNDERVALUATION + QUALITY APPROACH
# ------------------------------------------------------------------------------
analizar_ranking_undervalued <- function(df) {
  
  df_limpio <- df %>%
    filter(!is.na(FY)) %>%
    mutate(
      EV_EBITDA = ifelse(is.na(EV_EBITDA) | EV_EBITDA < 0, 80, EV_EBITDA),
      ROIC      = ifelse(is.na(ROIC), 0, ROIC),
      Earnings_Yield = ifelse(is.na(Earnings_Yield), 0, Earnings_Yield),
      FCF_Yield      = ifelse(is.na(FCF_Yield), 0, FCF_Yield),
      NetDebt_EBITDA = ifelse(is.na(NetDebt_EBITDA), 10, NetDebt_EBITDA)
    )
  
  df_scored <- df_limpio %>%
    mutate(
      z_ey      = (Earnings_Yield - mean(Earnings_Yield)) / sd(Earnings_Yield),
      z_fcf     = (FCF_Yield - mean(FCF_Yield)) / sd(FCF_Yield),
      z_val     = -(EV_EBITDA - mean(EV_EBITDA)) / sd(EV_EBITDA),
      
      z_qual    = (ROIC - mean(ROIC)) / sd(ROIC),
      
      z_solv    = -(NetDebt_EBITDA - mean(NetDebt_EBITDA)) / sd(NetDebt_EBITDA)
    ) %>%
    mutate(
      Final_Score = (0.25 * z_ey) + (0.15 * z_fcf) + (0.10 * z_val) + 
        (0.30 * z_qual) + (0.20 * z_solv)
    ) %>%
    arrange(desc(Final_Score))
  
  return(df_scored)
}

ranking_final <- analizar_ranking_undervalued(df_2024_raw)

# VISUALIZATION OF RESULTS
# ------------------------------------------------------------------------------

cat("\n--- RANKING FINAL FY2024 (INFRAVALORACIÓN + CALIDAD) ---\n")
print(head(ranking_final %>% select(Ticker, Final_Score, ROIC, Earnings_Yield), 15))

plot_top <- head(ranking_final, 20)

ggplot(plot_top, aes(x = reorder(Ticker, Final_Score), y = Final_Score, fill = Final_Score)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  coord_flip() +
  scale_fill_gradient(low = "#d5f5e3", high = "#27ae60") +
  theme_fivethirtyeight() +
  labs(title = "Top 20 Valores Infravalorados con Alta Calidad (2024)",
       subtitle = "Basado en Earnings Yield, FCF Yield y ROIC (Fiscal Year 2024)",
       y = "Z-Score Combinado (Atractivo)", x = "") +
  theme(legend.position = "none")