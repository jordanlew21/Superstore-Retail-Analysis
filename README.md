# Superstore Retail Performance Analysis

An interactive **R Shiny dashboard** analysing retail performance using the Superstore dataset and three external datasets: **CPI, GDP per capita, and weather data**.

## Project Overview

This project investigates how external economic and environmental factors relate to retail performance, alongside internal factors such as discounting and product category.

The analysis uses Superstore data from **2011–2014** and combines it with external datasets to provide additional context for sales and profitability patterns.

## Key Findings

* **Macroeconomic trends:** CPI and GDP provide economic context but do not closely explain short-term sales fluctuations.
* **Discounting:** Moderate discount levels are associated with higher average sales than no or high discounts.
* **Inflation:** Higher CPI periods show greater variability in sales outcomes.
* **Seasonality & weather:** Sales show recurring seasonal patterns, while weather appears to reinforce rather than independently drive these patterns.
* **Profitability:** Profit efficiency varies substantially across product categories, showing that sales volume alone does not represent performance.

## Data Sources

* **Superstore** — Primary retail transaction data
* **FRED** — Consumer Price Index (CPI)
* **World Bank** — GDP per capita
* **Open-Meteo** — Weather data

## Technologies

**R · R Shiny · ggplot2 · dplyr · tidyr**

## Repository Contents

```text
├── README.md
├── app.R
├── data/
│   ├── superstore.xlsx
│   ├── cpi.xlsx
│   ├── gdp.xlsx
│   └── weather.xlsx
└── report/
    └── superstore_retail_analysis.pdf
```

## Report

The full report contains the dashboard, five visualizations and their interpretations, dataset justification, data story, references, and AI acknowledgement.

**[View the Full Report](report/superstore_retail_analysis.pdf)**

## Author

**Jordan Lew**
