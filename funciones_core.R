library(quadprog)

# BLACK-LITTERMAN PARAMETERS CALCULATION
calcular_mu_bl <- function(sigma_hist, w_eq, risk_aversion, vistas_obj, tau = 0.05) {
  
  Pi <- risk_aversion * sigma_hist %*% w_eq
  
  P <- vistas_obj$P
  Q <- matrix(vistas_obj$Q, ncol = 1)
  conf <- vistas_obj$Confianza
  
  omega_diag <- diag(P %*% (tau * sigma_hist) %*% t(P))
  conf_safe <- pmax(conf, 0.001) 
  omega_adj <- omega_diag * ((1 - conf_safe) / conf_safe)
  Omega <- diag(omega_adj)
  
  M_inv <- solve(tau * sigma_hist)
  part1 <- solve(M_inv + t(P) %*% solve(Omega) %*% P)
  part2 <- (M_inv %*% Pi + t(P) %*% solve(Omega) %*% Q)
  
  mu_bl <- part1 %*% part2
  return(as.vector(mu_bl))
}


# PORTFOLIO OPTIMIZER (QUADPROG WITH MAXIMUM LIMIT)
optimizar_cartera <- function(mu, sigma, risk_aversion = 2, max_weight = 0.30, ...) { 
  
  activos <- names(mu)
  if (is.null(activos)) activos <- colnames(sigma)
  n_total <- length(activos)
  
  sigma_reg <- 0.9 * sigma + 0.1 * diag(mean(diag(sigma)), n_total)
  Dmat <- risk_aversion * sigma_reg + diag(1e-6, n_total)
  dvec <- mu
  
  Amat <- cbind(rep(1, n_total), diag(n_total), -diag(n_total))
  
  bvec <- c(1, rep(0, n_total), rep(-max_weight, n_total))
  
  opt <- tryCatch({
    solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  }, error = function(e) {
    vol <- sqrt(diag(sigma))
    score <- pmax(mu, 0) / (vol + 0.0001)
    
    w_fall <- rep(0, n_total)
    names(w_fall) <- activos
    top_idx <- order(score, decreasing = TRUE)[1:6]
    
    pesos_simulados <- c(0.30, 0.25, 0.20, 0.15, 0.06, 0.04)
    w_fall[top_idx] <- pesos_simulados
    
    return(list(solution = w_fall))
  })
  
  w_final <- opt$solution
  names(w_final) <- activos
  
  w_final <- zapsmall(w_final)
  w_final[w_final < 0] <- 0
  
  w_final <- w_final / sum(w_final)
  
  return(w_final)
}