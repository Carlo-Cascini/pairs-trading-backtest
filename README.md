#  Pairs Trading Backtest with State-Space Models

This project implements a **systematic backtesting framework** for pairs trading strategies using **state-space models** based on **Partial Cointegration (PCI)** techniques.It was developed as part of my Master's thesis in Finance, designed for:

- **Mean-reverting pair selection**
- **Model estimation**
- **Performance evaluation** of statistical arbitrage strategies

---

##  Core Features

-  ETFs **data fetching and cleaning**
-  **Mean-reversion parameters estimation** using Partial Cointegration (PCI)
-  **Dynamic pair selection** using estimated PCI parameters
-  **Rolling-window estimation** with customizable training horizon and rolling step
-  **Backtesting** on a portfolio of selected ETF pairs

---

##  Repository Structure


pairs-trading-backtest/

├── data/                     # Cleaned and raw ETF price data (CSV)

├── results/                  # Rolling estimation results (.RData)

├── src/

│   ├── generate_dataset.R    # Download & clean ETF prices

│   ├── func_partial_ci.R     # PCI estimation and rolling backtest

│   └── stock_list.R          # List of ETFs to download

├── analysis/

│   └── pairs_trading_backtest.Rmd  # Notebook to run full backtest

└── README.md
