# --- Package Load ---

(function(packages){
for (pkg in packages) {
  library(pkg, character.only = TRUE)
  cat("Package loaded:", pkg, "\n")
  }
  }) (c("quantmod", "ggplot2", "xts"))




# --- Return Series ---

get_returns <- (function() {
  symbols <- c("AAPL", "MSFT", "NVDA", "AMZN")
  
  # 1. Download the data and extract the closing prices
  prices_xts <- do.call(
    merge,
    lapply(symbols, function(sym) {
      Cl(getSymbols(sym, from = "2020-01-01", src = "yahoo", auto.assign = FALSE))
    })
  )
  colnames(prices_xts) <- symbols
  
  # 2. Calculate the log returns
  returns_xts <- diff(log(prices_xts))
  colnames(returns_xts) <- paste0(symbols, "_RET")
  
  # Remove the first NA row
  returns_xts <- na.omit(returns_xts)
})()




# --- Grid Search (funciton) ---

(function(data, r) {
  
  l <- length(colnames(data))
  # Expected returns and covariance
  mu    <- colMeans(data)
  Sigma <- cov(data)
  
  # Risk-free rate (adjusted to scale)
  rf = r
  
  # Step size
  grid <- seq(0, 1, by =  0.01)
  
  # Containers for optimal portfolios
  best_sharpe <- -Inf
  best_sharpe_ret <- NA
  best_sharpe_risk <- NA
  best_sharpe_w <- NULL
  min_risk <- Inf
  min_risk_ret <- NA
  min_risk_val <- NA
  min_risk_w <- NULL
  
  
  for (w1 in grid) {
    for (w2 in grid) {
      for (w3 in grid) {
        w4 <- 1 - (w1 + w2 + w3)
        if (w4 >= 0) {
          w <- c(w1, w2, w3, w4)
          
          ret  <- sum(w * mu)
          risk <- sqrt(as.numeric(t(w) %*% Sigma %*% w))
          sharpe <- (ret - rf) / risk
          
          #  Maximum Sharpe portfolio
          if (sharpe > best_sharpe) {
            best_sharpe <- sharpe
            best_sharpe_ret <- ret
            best_sharpe_risk <- risk
            best_sharpe_w <- w
          }
          
          # Minimum risk portfolio
          if (risk < min_risk) {
            min_risk <- risk
            min_risk_ret <- ret
            min_risk_val <- risk
            min_risk_w <- w
          }
        }
      }
    }
  }
  
  # Results
  results_df <- data.frame(
    Type   = c("Max Sharpe", "Min Risk"),
    Sharpe = c(best_sharpe, NA),
    Return = c(best_sharpe_ret, min_risk_ret),
    Risk   = c(best_sharpe_risk, min_risk_val)
  )
  weighted_df <- rbind(
    Max_Sharpe = round(best_sharpe_w, l),
    Min_Risk   = round(min_risk_w, l)
  )
  colnames(weighted_df) <- colnames(data)
  weighted_df <- as.data.frame(weighted_df)
  
  return(list(
    results_df = results_df,
    weighted_df = weighted_df
  ))
  
}) (get_returns, r=0)





# --- Graph ---

local({
# Step size
grid <- seq(0, 1, by = 0.01)
mu    <- colMeans(get_returns)
Sigma <- cov(get_returns)

port_rets <- c()
port_risks <- c()

for (w1 in grid) {
  for (w2 in grid) {
    for (w3 in grid) {
      w4 <- 1 - (w1 + w2 + w3)
      if (w4 >= 0) {
        w <- c(w1, w2, w3, w4)
        ret  <- sum(w * mu)
        risk <- sqrt(t(w) %*% Sigma %*% w)
        port_rets  <- c(port_rets, ret)
        port_risks <- c(port_risks, risk)
      }
    }
  }
}

# Plot: risk-return space
plot(port_risks, port_rets, col=rgb(0,0,1,0.3), pch=16,
     xlab="Risk (σ)", ylab="Beklenen Getiri",
     main="Portfolio Risk-Return Space")
})