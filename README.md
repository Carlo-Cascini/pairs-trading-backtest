#  Pairs Trading Backtest with State-Space Models

This project implements a **systematic backtesting framework for pairs trading strategies** using 2 **state-space models** based on **partial cointegration (PCI)** techniques. It was developed as part of my Master's thesis in Finance and designed for **mean-reverting pair selection**, **model estimation**, and **performance evaluation** of statistical arbitrage strategies.

##  Core Features

-  ETFs Data fetching and cleaning 
-  **Mean-Reversion parameters estimation** using Partial Cointegration (PCI)
-  **Dynamic pair selection** using estimated parameters
-  **Rolling-window estimation** with customizable training horizon and rolling step
-  Backtest on portfolio of selected pairs
---

##  Repository Structure
pairs-trading-backtest/
├── data/ # Cleaned and raw ETF price data (CSV)
├── results/ # Rolling estimation results (.RData)
├── src/
│ ├── generate_dataset.R # Function to download and clean ETF prices
│ ├── func_partial_ci.R # Functions for PCI estimation and backtest
│ └── stock_list.R # List of ETFs to download
├── analysis/
│ └── pairs_trading_backtest.Rmd # Notebook to run full rolling estimation
└── README.md


