# Portfolio Optimization using the Black-Litterman Model

This repository contains the R scripts developed for the Master's Thesis (*Trabajo Fin de Máster - TFM*): **"Optimización de carteras mediante el modelo Black-Litterman: Simulación y análisis comparativo frente a Markowitz"** by Juan María García Aparicio.

The project explores and empirically validates the superiority of the Black-Litterman (BL) portfolio optimization framework over traditional Markowitz Mean-Variance Optimization. By executing a walk-forward backtest simulation over the 2025 financial year for 30 high-capitalization U.S. equities, this research evaluates how different market signals—ranging from technical indicators to machine learning and fundamental analysis—can be effectively integrated into a quantitative portfolio without compromising mathematical coherence and diversification.

## Investment Strategies Evaluated

The simulation evaluates four distinct portfolio strategies simultaneously against a dynamically reconstructed benchmark (`USA30`):

1. **Markowitz Optimization (Baseline)**: A pure historical mean-variance portfolio that serves as a static historical control, demonstrating the classic "error maximization" flaws of unconstrained quantitative optimization.
2. **Trend Black-Litterman**: A reactive strategy using 200-day Simple Moving Averages (SMA) to identify and overweight assets demonstrating strong bullish momentum.
3. **Machine Learning Black-Litterman**: The most advanced model in this repository. It utilizes a 500-day historical data window and Random Forest algorithms to map predictive non-linear patterns, incorporating a smoothing inertia filter to prevent erratic turnover. This strategy proved to be the most efficient in the backtest.
4. **Fundamental Value Black-Litterman**: A static, long-term Buy & Hold approach utilizing a custom Z-Score derived from financial health metrics (undervaluation + quality) to pinpoint objectively underpriced corporate quality.

## Repository Structure

The project is heavily modularized to separate data extraction, view generation, core mathematics, and the backtesting loop. 

- **`Main.R`**: The core execution script. It handles data downloading via Yahoo Finance, defines the walk-forward backtesting loop, aggregates the returns for all four models, and generates the final performance and weight evolution plots.
- **`ConstruccionBenchmark.R`**: Reconstructs the dynamic benchmark (`USA30`) using backward induction. It aligns historical prices with current market capitalization to simulate a market-weighted index for accurate alpha/beta calculation.
- **`Zscore.R`**: The fundamental scoring engine. It processes raw financial data (`df_2024_raw.csv`) to compute a custom Z-Score based on Earnings Yield, FCF Yield, EV/EBITDA, ROIC, and NetDebt/EBITDA, establishing the views for the Fundamental Value portfolio.
- **`funciones_vistas.R`**: Contains the view-generator functions (`P`, `Q`, and `Omega` matrices) for the Black-Litterman model:
  - `generar_vistas_sma()`: Trend-based signals.
  - `generar_vistas_ml()`: Machine Learning (Random Forest) signals.
  - `generar_vistas_fundamentales()`: Fundamental valuation signals.
- **`funciones_core.R`**: The mathematical engine of the project. It includes the logic for the Black-Litterman parameter calculation (`calcular_mu_bl`) and a robust quadratic portfolio optimizer (`optimizar_cartera`) that enforces strict constraints (100% investment, no short-selling, 30% max asset concentration) and includes emergency fallbacks.

## Data Requirements

To run this project, the following datasets are expected in the working directory:
- `df_2024_raw.csv`: Raw corporate financial metrics for the 30 selected equities to compute the fundamental Z-Score.
- `MarketCap_Scraped.csv`: Current market capitalization data used by `ConstruccionBenchmark.R` for the backward induction of the benchmark.
- Historical price data is pulled dynamically using the `quantmod` package via the Yahoo Finance API.

## Key Findings

- The **Machine Learning Black-Litterman** model emerged as the superior strategy, achieving an annualized yield approaching 30% and a highly significant Alpha (15.37%), effectively dodging sectoral crashes through extreme granularity.
- The **Fundamental Value** portfolio acted as a systemic stabilizer, providing an attractive risk-adjusted haven during market panics.
- The **Markowitz** model suffered severely from concentration risks and drawdown damage, proving that historical optimization alone is a dangerous vehicle in transitional market periods.

## Author

**Juan María García Aparicio**  
Master's Degree in Finance (2025-2026)  
Universidad de Murcia - Facultad de Economía y Empresa
