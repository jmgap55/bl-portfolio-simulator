library(TTR)
library(randomForest)

# TREND-BASED VIEW GENERATOR
generar_vistas_sma <- function(precios_hist, retornos_hist, window = 200) {
  
  last_price <- as.numeric(tail(precios_hist, 1))
  
  sma <- sapply(precios_hist, function(x) {
    as.numeric(tail(TTR::SMA(x, n = window), 1))
  })
  
  vol <- apply(tail(retornos_hist, window), 2, sd, na.rm=TRUE) * sqrt(252)
  
  n <- ncol(precios_hist)
  Q <- numeric(n)
  Conf <- numeric(n)
  
  for(i in 1:n) {
    if(is.na(sma[i])) { Q[i] <- 0; Conf[i] <- 0; next }
    
    if(last_price[i] > sma[i]) {
      Q[i] <- vol[i] 
    } else {
      Q[i] <- 0      
    }
    
    distancia <- abs(last_price[i] - sma[i]) / last_price[i]
    Conf[i] <- min(max(distancia * 5, 0.10), 0.90)
  }
  
  P <- diag(n)
  return(list(P = P, Q = Q, Confianza = Conf, Nombre="BL_Tendencia"))
}


# MACHINE LEARNING BASED VIEW GENERATOR
generar_vistas_ml <- function(precios_hist, retornos_hist, window = 500) {
  
  n <- ncol(precios_hist)
  Q <- numeric(n)
  Conf <- numeric(n)
  
  cat("\n--- INICIANDO ENTRENAMIENTO ML (Alineando datos...) ---\n")
  
  for(i in 1:n) {
    ticker <- colnames(precios_hist)[i]
    
    px <- as.numeric(coredata(precios_hist[, i]))
    ret <- as.numeric(coredata(retornos_hist[, i]))
    
    len_px <- length(px)
    len_ret <- length(ret)
    
    full_rsi <- RSI(px, n=14)
    full_sma <- SMA(px, n=50)
    full_dist_sma <- (px - full_sma) / full_sma
    
    if(len_px > len_ret) {
      rsi_align <- full_rsi[-1]
      dist_sma_align <- full_dist_sma[-1]
    } else {
      rsi_align <- full_rsi
      dist_sma_align <- full_dist_sma
    }
    
    df_features <- data.frame(
      Ret_Lag1 = c(NA, head(ret, -1)),
      Vol_20   = runSD(ret, n=20),
      RSI      = rsi_align,
      Dist_SMA = dist_sma_align,
      Target   = c(tail(ret, -1), NA)
    )
    
    df_clean <- na.omit(df_features)
    filas <- nrow(df_clean)
    
    if(filas < 200) {
      cat(sprintf("X %s: Insuficientes datos (%d filas). Se asigna 0.\n", ticker, filas))
      Q[i] <- 0; Conf[i] <- 0; next
    }
    
    n_train <- min(window, filas)
    train_data <- tail(df_clean, n_train)
    
    train_set <- head(train_data, -1)
    X_train <- train_set[, -5]
    y_train <- train_set$Target
    
    X_today <- tail(train_data[, -5], 1)
    
    tryCatch({
      rf <- randomForest(x = X_train, y = y_train, ntree = 100)
      
      pred <- predict(rf, X_today)
      Q[i] <- pred * 252 
      
      r_sq <- max(mean(rf$rsq), 0)
      Conf[i] <- min(0.20 + (r_sq * 5), 0.90)
      
      cat(sprintf("V %s: Entrenado OK (R2: %.2f%%) -> Pred Anualizada: %.2f%%\n", 
                  ticker, r_sq*100, Q[i]*100))
      
    }, error = function(e) {
      cat(sprintf("X %s: Error en Random Forest -> %s\n", ticker, e$message))
      Q[i] <<- 0; Conf[i] <<- 0
    })
  }
  
  cat("----------------------------------\n")
  P <- diag(n)
  return(list(P = P, Q = Q, Confianza = Conf, Nombre="BL_MachineLearning"))
}


# FUNDAMENTAL VIEW GENERATOR
generar_vistas_fundamentales <- function(ranking_df, tickers_universo, n_picks = 5) {
  
  df_vistas <- ranking_df %>% 
    filter(Ticker %in% tickers_universo)
  
  top_assets <- head(df_vistas, n_picks)
  bot_assets <- tail(df_vistas, n_picks)
  picks <- rbind(top_assets, bot_assets)
  
  n_total <- nrow(picks)
  n_universo <- length(tickers_universo)
  
  P <- matrix(0, nrow = n_total, ncol = n_universo)
  colnames(P) <- tickers_universo
  rownames(P) <- paste0("View_", picks$Ticker)
  
  for(i in 1:n_total) {
    ticker_act <- picks$Ticker[i]
    if(ticker_act %in% colnames(P)) {
      P[i, ticker_act] <- 1
    }
  }
  
  Q <- picks$Final_Score * 0.04 
  
  confianza <- abs(picks$Final_Score) / max(abs(df_vistas$Final_Score))
  confianza <- pmin(pmax(confianza, 0.2), 0.9)
  
  return(list(P = P, Q = Q, Confianza = confianza, Nombre = "BL_Fundamental_2024"))
}