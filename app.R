#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)

# Surplus simulation function
simulate_surplus <- function(initial_capital, premium_rate, time_horizon, claim_count_dist, claim_size_dist, params) {
    t <- 1:time_horizon
    surplus <- numeric(length(t))
    surplus[1] <- initial_capital
    
    for (i in 2:time_horizon) {
        # Simulate number of claims
        n_claims <- switch(claim_count_dist,
                           "Poisson" = rpois(1, lambda = params$lambda),
                           "Binomial" = rbinom(1, size = params$size, prob = params$prob),
                           "Negative Binomial" = rnbinom(1, size = params$size, mu = params$mu))
        
        # Simulate claim sizes
        claim_sizes <- switch(claim_size_dist,
                              "Exponential" = rexp(n_claims, rate = params$rate),
                              "Erlang" = rgamma(n_claims, shape = params$shape, rate = params$rate),
                              "Gamma" = rgamma(n_claims, shape = params$shape, rate = params$rate),
                              "Pareto" = params$scale / ((runif(n_claims))^(-1 / params$shape)))
        
        total_claims <- sum(claim_sizes)
        surplus[i] <- surplus[i-1] + premium_rate - total_claims
    }
    
    return(data.frame(Time = t, Surplus = surplus))
}

# Ruin probability estimator
estimate_ruin_probability <- function(initial_capital, premium_rate, time_horizon,
                                      claim_count_dist, claim_size_dist, params, n_sim = 500) {
    ruin_count <- 0
    for (i in 1:n_sim) {
        sim <- simulate_surplus(initial_capital, premium_rate, time_horizon, claim_count_dist, claim_size_dist, params)
        if (any(sim$Surplus < 0)) ruin_count <- ruin_count + 1
    }
    return(ruin_count / n_sim)
}

# UI
ui <- fluidPage(
    theme = shinytheme("flatly"),
    titlePanel("Classical Risk Process Simulation"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("initial_capital", "Initial Capital (u)", value = 100, min = 0),
            numericInput("premium_rate", "Premium Rate (c)", value = 20, min = 0),
            numericInput("time_horizon", "Time Horizon", value = 50, min = 1),
            
            selectInput("claim_count_dist", "Claim Count Distribution",
                        choices = c("Poisson", "Binomial", "Negative Binomial")),
            conditionalPanel(
                condition = "input.claim_count_dist == 'Poisson'",
                numericInput("lambda", "Lambda (Poisson)", value = 2, min = 0.1)
            ),
            conditionalPanel(
                condition = "input.claim_count_dist == 'Binomial'",
                numericInput("size_binom", "Size (Binomial)", value = 10, min = 1),
                numericInput("prob_binom", "Prob (Binomial)", value = 0.2, min = 0.01, max = 1)
            ),
            conditionalPanel(
                condition = "input.claim_count_dist == 'Negative Binomial'",
                numericInput("size_nb", "Size (NegBinom)", value = 10, min = 1),
                numericInput("mu_nb", "Mu (NegBinom)", value = 2, min = 0.1)
            ),
            
            selectInput("claim_size_dist", "Claim Size Distribution",
                        choices = c("Exponential", "Erlang", "Gamma", "Pareto")),
            conditionalPanel(
                condition = "input.claim_size_dist == 'Exponential'",
                numericInput("rate_exp", "Rate (Exponential)", value = 0.1, min = 0.01)
            ),
            conditionalPanel(
                condition = "input.claim_size_dist == 'Erlang' || input.claim_size_dist == 'Gamma'",
                numericInput("shape_gamma", "Shape", value = 2, min = 1),
                numericInput("rate_gamma", "Rate", value = 0.1, min = 0.01)
            ),
            conditionalPanel(
                condition = "input.claim_size_dist == 'Pareto'",
                numericInput("shape_pareto", "Shape", value = 3, min = 0.1),
                numericInput("scale_pareto", "Scale", value = 1, min = 0.1)
            )
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Surplus Plot", plotOutput("surplusPlot")),
                tabPanel("Ruin Probability", 
                         verbatimTextOutput("ruinProb"),
                         plotOutput("ruinVsUPlot"),
                         tableOutput("ruinVsUTable"))
            )
        )
    )
)

# Server
server <- function(input, output) {
    
    getParams <- reactive({
        switch(input$claim_count_dist,
               "Poisson" = list(lambda = input$lambda),
               "Binomial" = list(size = input$size_binom, prob = input$prob_binom),
               "Negative Binomial" = list(size = input$size_nb, mu = input$mu_nb)
        ) %>%
            append(
                switch(input$claim_size_dist,
                       "Exponential" = list(rate = input$rate_exp),
                       "Erlang" = list(shape = input$shape_gamma, rate = input$rate_gamma),
                       "Gamma" = list(shape = input$shape_gamma, rate = input$rate_gamma),
                       "Pareto" = list(shape = input$shape_pareto, scale = input$scale_pareto)
                )
            )
    })
    
    output$surplusPlot <- renderPlot({
        sim_data <- simulate_surplus(input$initial_capital, input$premium_rate, input$time_horizon,
                                     input$claim_count_dist, input$claim_size_dist, getParams())
        ggplot(sim_data, aes(x = Time, y = Surplus)) +
            geom_line(linewidth = 1, color = "steelblue") +
            geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
            theme_minimal() +
            labs(title = "Surplus vs Time", y = "Surplus", x = "Time")
    })
    
    output$ruinProb <- renderText({
        prob <- estimate_ruin_probability(input$initial_capital, input$premium_rate, input$time_horizon,
                                          input$claim_count_dist, input$claim_size_dist, getParams(), n_sim = 300)
        paste0("Estimated Probability of Ruin: ", round(prob, 4))
    })
    
    output$ruinVsUPlot <- renderPlot({
        u_vals <- seq(0, 200, by = 20)
        probs <- sapply(u_vals, function(u) {
            estimate_ruin_probability(u, input$premium_rate, input$time_horizon,
                                      input$claim_count_dist, input$claim_size_dist, getParams(), n_sim = 200)
        })
        df <- data.frame(InitialCapital = u_vals, RuinProb = probs)
        
        ggplot(df, aes(x = InitialCapital, y = RuinProb)) +
            geom_line(linewidth = 1, color = "darkred") +
            theme_minimal() +
            labs(title = "Ruin Probability vs Initial Capital", x = "Initial Capital", y = "Ruin Probability")
    })
    
    output$ruinVsUTable <- renderTable({
        u_vals <- seq(0, 200, by = 20)
        probs <- sapply(u_vals, function(u) {
            estimate_ruin_probability(u, input$premium_rate, input$time_horizon,
                                      input$claim_count_dist, input$claim_size_dist, getParams(), n_sim = 200)
        })
        data.frame(Initial_Capital = u_vals, Ruin_Probability = round(probs, 4))
    })
}

shinyApp(ui = ui, server = server)
