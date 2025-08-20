# Classical Risk Process Simulation (R Shiny)

An interactive web application built with R and Shiny for modeling, simulating, and analyzing an insurer's capital surplus over time. This tool provides an analytical platform for understanding the core principles of actuarial ruin theory.

**Live App Link:** [*Link to your deployed shinyapps.io app here*]

![Screenshot of App](*Link to your screenshot here*)

---

## Features

-   **Stochastic Modeling:** Simulates an insurer's surplus using a Monte Carlo approach based on the Classical Risk Process model.
-   **Flexible Distributions:** Allows users to select from multiple statistical distributions for both claim frequency and severity to model various risk scenarios:
    -   **Claim Count Distributions:** Poisson, Binomial, Negative Binomial.
    -   **Claim Size Distributions:** Exponential, Erlang, Gamma, Pareto.
-   **Ruin Probability Analysis:** Estimates the probability of ruin through repeated simulations and presents the results numerically.
-   **Interactive Visualization:** A multi-tab dashboard built with `ggplot2` provides clear, dynamic insights:
    -   A plot of a single surplus trajectory over time.
    -   A sensitivity analysis plot and table showing how ruin probability changes with varying levels of initial capital.

---

## How to Run This App Locally

1.  **Prerequisites:** You need to have R and RStudio installed.
2.  **Clone the repository:**
    ```bash
    git clone [https://github.com/YourUsername/YourRepositoryName.git](https://github.com/YourUsername/YourRepositoryName.git)
    ```
3.  **Install required packages:** Open the `app.R` file in RStudio and run the following command in the console:
    ```r
    install.packages(c("shiny", "ggplot2", "dplyr", "shinythemes"))
    ```
4.  **Run the App:** With `app.R` open in RStudio, click the "Run App" button at the top of the editor.

---

## Technologies Used

-   **Language:** R
-   **Framework:** Shiny
-   **Data Visualization:** ggplot2
-   **Data Manipulation:** dplyr
# History files
.Rhistory
.Rapp.history

# Session Data
.RData

# User-specific files
.Rproj.user/

# Shiny deploy files
rsconnect/
