# Libraries
library(shiny)
library(DT)
library(shinylive)

# ---------------- UI ----------------
ui <- fluidPage(
  # Status + spinner
  textOutput("status"),
  uiOutput("busy"),
  
  # Spinner CSS
  tags$style(HTML("
    .lds-dual-ring { display:inline-block; width:24px; height:24px; vertical-align:middle; margin-left:8px; }
    .lds-dual-ring:after { content:' '; display:block; width:24px; height:24px; border-radius:50%;
      border:3px solid #999; border-color:#999 transparent #999 transparent; animation:lds-dual-ring 1.2s linear infinite; }
    @keyframes lds-dual-ring { 0%{transform:rotate(0deg);} 100%{transform:rotate(360deg);} }
  ")),
  
  titlePanel("Retirement Simulation"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("mean_input", "Average Gain (%)", value = 7.25, step = 0.25),
      numericInput("stdv_input", "Portfolio Std Dev (%)", value = 13.5, step = 0.25),
      numericInput("t_input", "Time (2-50 Years)", value = 30, min = 2, max = 50, step = 1),
      numericInput("capital_input", "Initial Portfolio Value", value = 5000000, step = 50000),
      numericInput("initial_percent", "Initial Withdrawal Rate (%)", value = 5, step = 0.25),
      numericInput("floor_percent", "Minimum Withdrawal Rate (%)", value = 3, step = 0.25),
      numericInput("spending_inc", "Spending Increase (%)", value = 10, step = 0.5),
      numericInput("spending_dec", "Spending Decrease (%)", value = 20, step = 0.5),
      selectInput("lowPoS_input", "Lower Probability Threshold (%)",
                  choices = c(20, 30, 40, 50, 60, 70), selected = 60),
      selectInput("upPoS_input", "Upper Probability Threshold (%)",
                  choices = c(30, 40, 50, 60, 70, 80), selected = 80),
      numericInput("inflation", "Inflation Rate (%)", value = 2.5, step = 0.25),
      numericInput("trials", "Number of Trials (100-5000)", value = 1000, min = 100, max = 5000, step = 1),
      
      # Cashflow 1
      h4("Additional Cash Flow #1"),
      checkboxInput("cashflow_1", "Receiving Cash Flow?", value = FALSE),
      conditionalPanel(
        condition = "input.cashflow_1 == true",
        sliderInput("rangeinput_1", "Years you receive this income:", min = 0, max = 50, value = c(15, 30))
      ),
      conditionalPanel(
        condition = "input.cashflow_1 == true",
        numericInput("flowamount_1", "Amount received each year", min = 0, value = 25000, step = 1000)
      ),
      conditionalPanel(
        condition = "input.cashflow_1 == true",
        checkboxInput("inflation_1", "Inflation adjusted?", value = FALSE)
      ),
      
      # Cashflow 2
      h4("Additional Cash Flow #2"),
      checkboxInput("cashflow_2", "Receiving Cash Flow?", value = FALSE),
      conditionalPanel(
        condition = "input.cashflow_2 == true",
        sliderInput("rangeinput_2", "Years you receive this income:", min = 0, max = 50, value = c(15, 30))
      ),
      conditionalPanel(
        condition = "input.cashflow_2 == true",
        numericInput("flowamount_2", "Amount received each year", min = 0, value = 25000, step = 1000)
      ),
      conditionalPanel(
        condition = "input.cashflow_2 == true",
        checkboxInput("inflation_2", "Inflation adjusted?", value = FALSE)
      ),
      
      # Cashflow 3
      h4("Additional Cash Flow #3"),
      checkboxInput("cashflow_3", "Receiving Cash Flow?", value = FALSE),
      conditionalPanel(
        condition = "input.cashflow_3 == true",
        sliderInput("rangeinput_3", "Years you receive this income:", min = 0, max = 50, value = c(15, 30))
      ),
      conditionalPanel(
        condition = "input.cashflow_3 == true",
        numericInput("flowamount_3", "Amount received each year", min = 0, value = 25000, step = 1000)
      ),
      conditionalPanel(
        condition = "input.cashflow_3 == true",
        checkboxInput("inflation_3", "Inflation adjusted?", value = FALSE)
      ),
      
      actionButton("run", "Run Simulation")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Summary",
                 h3("Summary Output"),
                 tableOutput("outputs1"),
                 plotOutput("result_plot"),
                 plotOutput("result_plot2"),
                 plotOutput("result_plot3"),
                 plotOutput("result_plot4"),
                 plotOutput("result_plot5"),
                 plotOutput("result_plot6")
        ),
        tabPanel("Portfolio Value Table",
                 h3("Portfolio Value Over Time (All Trials)"),
                 DTOutput("capital_table_dt")
        ),
        tabPanel("Withdrawal Table",
                 h3("Withdrawal Amounts (All Trials)"),
                 DTOutput("withdrawal_table_dt")
        ),
        tabPanel("Portfolio Returns",
                 h3("Portfolio Returns by Year (All Trials)"),
                 DTOutput("portfolio_table_dt")
        ),
        tabPanel("Cash Inflows",
                 h3("Cashflow income by Year (All Trials)"),
                 DTOutput("cashflow_table_dt")
        ),
        tabPanel("Spending At Floor",
                 h3("Years Where Spending Was At the Floor"),
                 DTOutput("floor_table_dt")
        ),
        tabPanel("Spending Below Initial",
                 h3("Years Where Spending Was Below Initial"),
                 DTOutput("initial_table_dt")
        ),
        tabPanel("How to Use the Simulation",
                 h3("How to Use the Monte Carlo Retirement Simulation"),
                 p("This simulation models retirement withdrawal strategies under market uncertainty using a Monte Carlo approach."),
                 h4("Inputs"),
                 tags$ul(
                   tags$li(HTML("<b>Average Gain (%):</b> The average expected annual return of the portfolio.")),
                   tags$li(HTML("<b>Portfolio Std Dev (%):</b> The volatility of the portfolio's returns.")),
                   tags$li(HTML("<b>Time:</b> The number of years for the retirement simulation.")),
                   tags$li(HTML("<b>Initial Withdrawal Rate (%):</b> The withdrawal rate at the start of retirement.<br>- This sets the ceiling spending (inflation-adjusted).")),
                   tags$li(HTML("<b>Minimum Withdrawal Rate (%):</b> The lowest rate you are willing to withdraw (inflation-adjusted floor).")),
                   tags$li(HTML("<b>Probability Thresholds:</b> Used to trigger changes in spending based on portfolio performance.")),
                   tags$li(HTML("<b>Spending Increase/Decrease (%):</b> Adjustments to spending if performance is strong/weak.")),
                   tags$li(HTML("<b>Initial Capital:</b> Starting portfolio.")),
                   tags$li(HTML("<b>Inflation Rate (%):</b> Assumed annual inflation.")),
                   tags$li(HTML("<b>Number of Trials:</b> How many Monte Carlo paths to run.")))
        )
      )
    )
  )
)

# ---------------- SERVER ----------------
server <- function(input, output, session) {
  is_running <- reactiveVal(FALSE)
  
  output$busy <- renderUI({
    if (is_running()) tagList(span("Simulation running..."), div(class = "lds-dual-ring")) else NULL
  })
  output$status <- renderText("")
  
  # ---- Simulation function: uses a snapshot of inputs (vals) ----
  run_simulation <- function(vals) {
    on.exit({
      is_running(FALSE)
      output$status <- renderText("Simulation complete!")
    }, add = TRUE)
    
    # --- all your original logic, using vals$... instead of input$... ---
    t_input <- vals$t_input
    valid_1 <- TRUE
    valid_2 <- TRUE
    valid_3 <- TRUE
    
    if (vals$initial_percent < vals$floor_percent) {
      showModal(modalDialog(
        title = "Input Error",
        "Initial Withdrawal Rate must be >= Floor Withdrawal Rate.",
        easyClose = TRUE
      ))
      valid_1 <- FALSE
    }
    
    if (as.numeric(vals$lowPoS_input) >= as.numeric(vals$upPoS_input)) {
      showModal(modalDialog(
        title = "Input Error",
        "Lower Probability Threshold must be less than the Upper Probability Threshold.",
        easyClose = TRUE
      ))
      valid_2 <- FALSE
    }
    
    check_error <- function(active, range) {
      if (!active) return(TRUE)
      if (is.null(range)) return(FALSE)
      if (active) {
        if (range[2] > t_input) {
          showModal(modalDialog(
            title = "Input Error",
            "Range of Years Must Be Within the simulation years input.",
            easyClose = TRUE
          ))
          return(FALSE)
        }
        return(TRUE)
      }
    }
    
    mean_input <- vals$mean_input
    stdv_input <- vals$stdv_input
    initial_percent <- vals$initial_percent/100
    floor_percent <- vals$floor_percent/100
    lowPoS_input <- vals$lowPoS_input
    upPoS_input <- vals$upPoS_input
    spending.inc_input <- vals$spending_inc/100
    spending.dec_input <- vals$spending_dec/100
    capital_input <- vals$capital_input
    inflation <- vals$inflation/100
    trials <- vals$trials
    
    # cashflows
    valid_3 <- check_error(vals$cashflow_1, vals$rangeinput_1) &&
      check_error(vals$cashflow_2, vals$rangeinput_2) &&
      check_error(vals$cashflow_3, vals$rangeinput_3)
    
    valid <- valid_1 && valid_2 && valid_3
    if (!valid) return()
    
    apply_cashflow <- function(active, range, amount, inflation_adjusted, t_input, inflation_rate) {
      vec <- rep(0, t_input)
      if (active) {
        start <- range[1]
        end <- range[2]
        for (yr in start:end) {
          vec[yr] <- if (inflation_adjusted) amount * (1 + inflation_rate)^(yr - 1) else amount
        }
      }
      vec
    }
    
    cf1 <- apply_cashflow(vals$cashflow_1, vals$rangeinput_1, vals$flowamount_1, vals$inflation_1, t_input, inflation)
    cf2 <- apply_cashflow(vals$cashflow_2, vals$rangeinput_2, vals$flowamount_2, vals$inflation_2, t_input, inflation)
    cf3 <- apply_cashflow(vals$cashflow_3, vals$rangeinput_3, vals$flowamount_3, vals$inflation_3, t_input, inflation)
    total_cashflow <- cf1 + cf2 + cf3
    
    options(scipen = 999)
    
    initial_input <- initial_percent * capital_input
    floor_input <- floor_percent * capital_input
    
    inflation_initial <- c(initial_input)
    inflation_floor <- c(floor_input)
    for (i in 1:(t_input-1)) {
      inflation_initial <- c(inflation_initial, (inflation_initial[i] * (1+inflation)))
      inflation_floor <- c(inflation_floor, (inflation_floor[i] * (1+inflation)))
    }
    
    ranout <- 0
    wealth_storage <- c()
    timefloor <- 0
    timebelowinitial <- 0
    spendingcut <- 0
    calculate <- 0
    timebelowinitial_year <- 0
    timefloor_year <- 0
    
    empty_matrix <- matrix(NA, nrow = t_input, ncol = trials)
    special_matrix <- matrix(0, nrow = t_input, ncol = trials)
    
    capital_table <- as.data.frame(empty_matrix)
    colnames(capital_table) <- paste0("trial ", 1:trials)
    rownames(capital_table) <- paste0("year ", 1:t_input)
    
    withdrawal_table <- as.data.frame(empty_matrix)
    colnames(withdrawal_table) <- paste0("trial ", 1:trials)
    rownames(withdrawal_table) <- paste0("year ", 1:t_input)
    
    lowPoS_table <- as.data.frame(empty_matrix)
    colnames(lowPoS_table) <- paste0("trial ", 1:trials)
    rownames(lowPoS_table) <- paste0("year ", 1:t_input)
    
    upPoS_table <- as.data.frame(empty_matrix)
    colnames(upPoS_table) <- paste0("trial ", 1:trials)
    rownames(upPoS_table) <- paste0("year ", 1:t_input)
    
    Portfolio_table <- as.data.frame(empty_matrix)
    colnames(Portfolio_table) <- paste0("trial ", 1:trials)
    rownames(Portfolio_table) <- paste0("year ", 1:t_input)
    
    timebelowinitial_year_table <- as.data.frame(special_matrix)
    colnames(timebelowinitial_year_table) <- paste0("trial ", 1:trials)
    rownames(timebelowinitial_year_table) <- paste0("year ", 1:t_input)
    
    timefloor_year_table <- as.data.frame(special_matrix)
    colnames(timefloor_year_table) <- paste0("trial ", 1:trials)
    rownames(timefloor_year_table) <- paste0("year ", 1:t_input)
    
    cashflow_table <- as.data.frame(empty_matrix)
    colnames(cashflow_table) <- paste0("trial ", 1:trials)
    rownames(cashflow_table) <- paste0("year ", 1:t_input)
    
    PoS_list <- list(
      pos20 = c(0.0565,0.0570,0.0575,0.0580,0.0584,0.0588,0.0592,0.0596,0.0600,0.0605,0.0610,0.0615,0.0620,0.0625,0.0631,0.0638,0.0645,0.0654,0.0665,0.0677,0.0690,0.0704,0.0718,0.0731,0.0745,0.0759,0.0774,0.0790,0.0810,0.0831,0.0854,0.0880,0.0912,0.0945,0.0980,0.1025,0.1075,0.1130,0.1195,0.1275,0.1375,0.1490,0.1625,0.1815,0.2060,0.2390,0.2890,0.3715,0.5360),
      pos30 = c(0.0500,0.0505,0.0510,0.0515,0.0520,0.0523,0.0526,0.0530,0.0535,0.0540,0.0545,0.0552,0.0558,0.0565,0.0572,0.0580,0.0587,0.0595,0.0605,0.0615,0.0625,0.0635,0.0650,0.0662,0.0675,0.0690,0.0705,0.0725,0.0745,0.0765,0.0790,0.0815,0.0845,0.0875,0.0915,0.0955,0.1000,0.1060,0.1125,0.1200,0.1300,0.1410,0.1550,0.1725,0.1970,0.2300,0.2800,0.3625,0.5250),
      pos40 = c(0.0455,0.0460,0.0464,0.0468,0.0472,0.0476,0.0480,0.0485,0.0490,0.0494,0.0498,0.0503,0.0509,0.0515,0.0522,0.0530,0.0538,0.0545,0.0554,0.0564,0.0575,0.0585,0.0596,0.0608,0.0621,0.0636,0.0652,0.0670,0.0690,0.0710,0.0733,0.0758,0.0788,0.0822,0.0860,0.0900,0.0944,0.1000,0.1065,0.1140,0.1230,0.1340,0.1475,0.1650,0.1890,0.2230,0.2725,0.3550,0.5200),
      pos50 = c(0.0414,0.0418,0.0421,0.0425,0.0429,0.0433,0.0437,0.0441,0.0445,0.0450,0.0455,0.0460,0.0466,0.0473,0.0480,0.0487,0.0495,0.0503,0.0511,0.0519,0.0527,0.0538,0.0550,0.0563,0.0576,0.0590,0.0605,0.0622,0.0644,0.0665,0.0685,0.0710,0.0740,0.0770,0.0805,0.0845,0.0889,0.0945,0.1010,0.1085,0.1170,0.1280,0.1415,0.1590,0.1830,0.2150,0.2645,0.3470,0.5110),
      pos60 = c(0.0375,0.0379,0.0383,0.0386,0.0389,0.0393,0.0397,0.0401,0.0405,0.0410,0.0415,0.0420,0.0425,0.0431,0.0437,0.0444,0.0452,0.0460,0.0468,0.0477,0.0486,0.0496,0.0507,0.0518,0.0531,0.0545,0.0560,0.0576,0.0595,0.0615,0.0637,0.0662,0.0690,0.0720,0.0755,0.0795,0.0840,0.0894,0.0955,0.1025,0.1110,0.1220,0.1355,0.1530,0.1760,0.2080,0.2575,0.3400,0.5030),
      pos70 = c(0.0335,0.0339,0.0343,0.0346,0.0349,0.0352,0.0356,0.0360,0.0364,0.0368,0.0372,0.0377,0.0383,0.0390,0.0397,0.0405,0.0412,0.0419,0.0426,0.0433,0.0441,0.0452,0.0464,0.0477,0.0490,0.0503,0.0516,0.0531,0.0547,0.0567,0.0589,0.0613,0.0640,0.0670,0.0705,0.0740,0.0785,0.0839,0.0895,0.0965,0.1050,0.1160,0.1290,0.1455,0.1685,0.2005,0.2500,0.3310,0.4940),
      pos80 = c(0.0290,0.0293,0.0296,0.0299,0.0302,0.0306,0.0310,0.0315,0.0320,0.0325,0.0330,0.0335,0.0340,0.0345,0.0350,0.0355,0.0361,0.0368,0.0376,0.0385,0.0395,0.0405,0.0415,0.0425,0.0435,0.0448,0.0463,0.0480,0.0497,0.0515,0.0535,0.0555,0.0582,0.0610,0.0645,0.0685,0.0725,0.0775,0.0831,0.0900,0.0980,0.1080,0.1210,0.1375,0.1605,0.1925,0.2400,0.3200,0.4830)
    )
    
    lowPoS <- PoS_list[[paste0("pos", lowPoS_input)]]
    upPoS  <- PoS_list[[paste0("pos", upPoS_input)]]
    lngthLow <- length(lowPoS)
    lngthUp  <- length(upPoS)
    
    for (i in 1:trials) {
      percent_run <- rnorm(t_input, mean = (mean_input/100), sd = (stdv_input/100))
      
      new_capital <- (capital_input - initial_input + total_cashflow[1]) * (1 + percent_run[1])
      cashflow_table[1,i] <- total_cashflow[1]
      capital_table[1, i] <- format(new_capital, big.mark = ",", scientific = FALSE)
      withdrawal_table[1, i] <- format(initial_input, big.mark = ",", scientific = FALSE)
      
      withdrawal <- initial_input
      floor <- floor_input
      calculate <- calculate + 1
      Portfolio_table[1,i] <- paste0((round(100*percent_run[1],2)), "%")
      timebelowinitial_year <- 0
      timefloor_year <- 0
      timebelowinitial_year_table[1,i] <- timebelowinitial_year
      timefloor_year_table[1,i] <- timefloor_year
      
      for (x in 2:t_input) {
        timebelowinitial_year <- 0
        timefloor_year <- 0
        new_capital <- (new_capital + total_cashflow[x])
        cashflow_table[x,i] <- total_cashflow[x]
        
        distribution_rate <- ((withdrawal*(1+inflation))/new_capital)
        
        if (distribution_rate > lowPoS[lngthLow - t_input + x] & withdrawal > inflation_floor[x])  {
          withdrawal <- withdrawal * (1-spending.dec_input) * (1 + inflation)
          spendingcut <- spendingcut + 1
          
          if (withdrawal <= inflation_floor[x]) {
            withdrawal <- inflation_floor[x]
            timefloor <- timefloor + 1
            timefloor_year <- timefloor_year + 1
          }
        } else if (distribution_rate < upPoS[lngthUp - t_input + x]) {
          withdrawal <- withdrawal * (1+spending.inc_input) * (1 + inflation)
          if (withdrawal > inflation_initial[x]) {
            withdrawal <- inflation_initial[x]
          }
        } else if ((withdrawal*(1+inflation)) == inflation_floor[x]) {
          withdrawal <- inflation_floor[x]
          timefloor <- timefloor + 1
          timefloor_year <- timefloor_year + 1
          timefloor_year_table[x,i] <- timefloor_year
        } else if ((withdrawal*(1+inflation)) > inflation_initial[x]) {
          withdrawal <- inflation_initial[x]
        } else {
          withdrawal <- withdrawal * (1+inflation)
        }
        
        new_capital <- (new_capital - withdrawal) * (1 + percent_run[x])
        calculate <- calculate + 1
        
        if (new_capital < 0) {
          ranout <- ranout + 1
          capital_table[x,i] <- 0
          spendingcut <- spendingcut + 1
          timebelowinitial <- timebelowinitial + 1
          withdrawal_table[x, i] <- format(round(withdrawal + (new_capital/(1+ percent_run[x])), 2), big.mark = ",", scientific = FALSE)
          wealth_storage <- c(wealth_storage, 0)
          Portfolio_table[x,i] <- "No Money"
          break
        }
        
        capital_table[x, i] <- format(new_capital, big.mark = ",", scientific = FALSE)
        lowPoS_table[x,i] <- lowPoS[lngthLow - t_input + x]
        upPoS_table[x,i] <- upPoS[lngthUp - t_input + x]
        Portfolio_table[x,i] <- paste0((round(100*percent_run[x],2)), "%")
        withdrawal_table[x, i] <- format(withdrawal, big.mark = ",", scientific = FALSE)
        
        if (withdrawal < inflation_initial[x]) {
          timebelowinitial <- timebelowinitial + 1
          timebelowinitial_year <- timebelowinitial_year + 1
        }
        
        timefloor_year_table[x,i] <- timefloor_year
        timebelowinitial_year_table[x,i] <- timebelowinitial_year
        
        if (x == t_input){
          wealth_storage <- c(wealth_storage, new_capital)
        }
      }
    }
    
    wealth_distribution <- data.frame(
      `Wealth Distribution (Sorted)` = format(round(sort(wealth_storage), 2), big.mark = ",", scientific = FALSE)
    )
    
    sorted_wealth <- sort(wealth_storage)
    percentile_25 <- round(0.25 * trials)
    percentile_50 <- round(0.5 * trials)
    percentile_75 <- round(0.75 * trials)
    
    outputs <- data.frame(
      `Probability of Success` = paste0(100 - (ranout / 10), "%"),
      `Time At Floor Overall` = paste0(round((100 * timefloor) / (trials * t_input), 1), "%"),
      `% of Time Below Initial` = paste0(round((100 * timebelowinitial) / (trials * t_input), 1), "%"),
      `% of Time Spending was Cut` = paste0(round((100 * spendingcut) / (trials * t_input), 1), "%"),
      `Average Ending Portfolio Value` = paste0("$", format(round((mean(wealth_storage))/(1+inflation)^t_input), big.mark = ",", scientific = FALSE)),
      `50th Percentile of Ending Portfolio Value` = paste0("$", format(round((median(sorted_wealth))/(1+inflation)^t_input), big.mark = ",", scientific = FALSE)),
      `25th Percentile of Ending Portfolio Value` = paste0("$", format(round((sorted_wealth[percentile_25])/(1+inflation)^t_input), big.mark = ",", scientific = FALSE)),
      `75th Percentile of Ending Portfolio Value` = paste0("$", format(round((sorted_wealth[percentile_75])/(1+inflation)^t_input), big.mark = ",", scientific = FALSE))
    )
    
    y_mean <- c(capital_input)
    y_median <- c(capital_input)
    y_withdrawal_mean <- c()
    x <- c(0)
    x_withdrawal <- c()
    y_mean_inflation <- c(capital_input)
    y_median_inflation <- c(capital_input)
    y_withdrawal_inflation <- c()
    
    for (z in 1:t_input){
      x <- c(x, z)
      x_withdrawal <- c(x_withdrawal, z)
      y_capital <- c()
      y_withdrawal <- c()
      
      for (i in 1:trials){
        if (is.na(capital_table[z, i])) {
          y_capital <- c(y_capital, 0)
        } else {
          y_capital <- c(y_capital, as.numeric(gsub(",", "", capital_table[z,i])))
        }
        if (!is.na(withdrawal_table[z,i])) {
          y_withdrawal <- c(y_withdrawal, as.numeric(gsub(",", "", withdrawal_table[z,i])))
        }
      }
      
      y_mean <- c(y_mean, mean(y_capital))
      y_median <- c(y_median, median(y_capital))
      y_withdrawal_mean <- c(y_withdrawal_mean, mean(y_withdrawal))
      
      y_mean_inflation <- c(y_mean_inflation, (mean(y_capital))/(1+inflation)^z)
      y_median_inflation <- c(y_median_inflation, (median(y_capital))/(1+inflation)^z)
      y_withdrawal_inflation <- c(y_withdrawal_inflation, (mean(y_withdrawal))/(1+inflation)^(z-1))
    }
    
    # Outputs
    output$outputs1 <- renderTable(outputs)
    
    output$result_plot <- renderPlot({
      plot(x, y_mean, type = "l", col = "blue", lwd = 3,
           xlab = "Year", ylab = "Avg Wealth Each Year", main = "Average Wealth Plot",
           xaxs = "i", yaxs = "i",
           ylim = c(max(0, min(y_mean) - 250000), max(y_mean) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    output$result_plot2 <- renderPlot({
      plot(x, y_median, type = "l", col = "red", lwd = 3,
           xlab = "Year", ylab = "Median Wealth Each Year", main = "Median Wealth Plot",
           xaxs = "i", yaxs = "i",
           ylim = c(max(0, min(y_median) - 250000), max(y_median) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    output$result_plot3 <- renderPlot({
      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(x_withdrawal, y_withdrawal_mean, type = "l", col = "green", lwd = 3,
           xlab = "Year", ylab = "Average Withdrawal Each Year", main = "Average Withdrawal Plot",
           xaxs = "i", yaxs = "i",
           ylim = c(max(0, min(inflation_floor) - 10000), max(inflation_initial) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
      lines(x_withdrawal, inflation_initial, col = "blue", lty = "dashed", lwd = 2)
      lines(x_withdrawal, inflation_floor,   col = "red",  lty = "dashed", lwd = 2)
      legend("topleft",
             legend = c("Avg Withdrawal", "Ceiling Withdrawal", "Floor Withdrawal"),
             col = c("green", "blue", "red"),
             lty = c("solid", "dashed", "dashed"),
             lwd = 2, bty = "o", bg = "white")
    })
    
    output$result_plot4 <- renderPlot({
      plot(x, y_mean_inflation, type = "l", col = "blue", lwd = 3,
           xlab = "Year", ylab = "Avg Wealth (real)", main = "Average Wealth (inflation-adjusted)",
           xaxs = "i", yaxs = "i",
           ylim = c(max(0, min(y_mean_inflation) - 250000), max(y_mean_inflation) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    output$result_plot5 <- renderPlot({
      plot(x, y_median_inflation, type = "l", col = "red", lwd = 3,
           xlab = "Year", ylab = "Median Wealth (real)", main = "Median Wealth (inflation-adjusted)",
           xaxs = "i", yaxs = "i",
           ylim = c(max(0, min(y_median_inflation) - 250000), max(y_median_inflation) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    output$result_plot6 <- renderPlot({
      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(x_withdrawal, y_withdrawal_inflation, type = "l", col = "green", lwd = 3,
           xlab = "Year", ylab = "Average Withdrawal (real)", main = "Average Withdrawal (inflation-adjusted)",
           xaxs = "i", yaxs = "i",
           ylim = c(max(0, min(floor_input) - 10000), max(initial_input) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
      lines(x_withdrawal, rep(initial_input, length(x_withdrawal)), col = "blue", lty = "dashed", lwd = 2)
      lines(x_withdrawal, rep(floor_input,   length(x_withdrawal)), col = "red",  lty = "dashed", lwd = 2)
      legend("topleft",
             legend = c("Avg Withdrawal (real)", "Ceiling (real, Y1)", "Floor (real, Y1)"),
             col = c("green", "blue", "red"),
             lty = c("solid", "dashed", "dashed"),
             lwd = 2, bty = "o", bg = "white")
    })
    
    output$capital_table_dt   <- renderDT({ datatable(capital_table,   options = list(scrollX = TRUE)) })
    output$withdrawal_table_dt<- renderDT({ datatable(withdrawal_table,options = list(scrollX = TRUE)) })
    output$portfolio_table_dt <- renderDT({ datatable(Portfolio_table, options = list(scrollX = TRUE)) })
    output$cashflow_table_dt  <- renderDT({ datatable(cashflow_table,  options = list(scrollX = TRUE)) })
    output$floor_table_dt     <- renderDT({ datatable(timefloor_year_table, options = list(scrollX = TRUE)) })
    output$initial_table_dt   <- renderDT({ datatable(timebelowinitial_year_table, options = list(scrollX = TRUE)) })
  }
  
  # Click handler — snapshot inputs, then run
  observeEvent(input$run, {
    is_running(TRUE)
    output$status <- renderText("Simulation running, please wait...")
    
    vals <- list(
      mean_input = isolate(input$mean_input),
      stdv_input = isolate(input$stdv_input),
      t_input = isolate(input$t_input),
      capital_input = isolate(input$capital_input),
      initial_percent = isolate(input$initial_percent),
      floor_percent = isolate(input$floor_percent),
      spending_inc = isolate(input$spending_inc),
      spending_dec = isolate(input$spending_dec),
      lowPoS_input = isolate(input$lowPoS_input),
      upPoS_input = isolate(input$upPoS_input),
      inflation = isolate(input$inflation),
      trials = isolate(input$trials),
      
      # cashflow snapshot
      cashflow_1 = isolate(input$cashflow_1),
      rangeinput_1 = isolate(input$rangeinput_1),
      flowamount_1 = isolate(input$flowamount_1),
      inflation_1 = isolate(input$inflation_1),
      
      cashflow_2 = isolate(input$cashflow_2),
      rangeinput_2 = isolate(input$rangeinput_2),
      flowamount_2 = isolate(input$flowamount_2),
      inflation_2 = isolate(input$inflation_2),
      
      cashflow_3 = isolate(input$cashflow_3),
      rangeinput_3 = isolate(input$rangeinput_3),
      flowamount_3 = isolate(input$flowamount_3),
      inflation_3 = isolate(input$inflation_3)
    )
    
    # Let UI update first, then run heavy work
    session$onFlushed(function() {
      run_simulation(vals)
    }, once = TRUE)
  })
}

# ---------------- RUN ----------------
shinyApp(ui = ui, server = server)
