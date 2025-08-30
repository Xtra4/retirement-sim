#the libraries required to run this code
library(shiny)
library(DT)

# implements the ui into a variable
ui <- fluidPage(
  #title
  titlePanel("Retirement Simulation"),
  
  #All the inputs
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
      
      #Cashflow 1
      h4("Additional Cash Flow #1"),
      checkboxInput("cashflow_1", "Recieving Cash Flow?", value = FALSE),
      
      conditionalPanel(
        condition = "input.cashflow_1 == true",
        sliderInput("rangeinput_1", "During Which Years Will You Recieve This Income? Select a Range:",
                    min = 0, max = 50,
                    value = c(15, 30))
      ),
      conditionalPanel(
        condition = "input.cashflow_1 == true",
        numericInput("flowamount_1", "Amount Recieved Each Year.", min = 0, value = 25000, step = 1000)
      ),
      conditionalPanel(
        condition = "input.cashflow_1 == true",
        checkboxInput("inflation_1", "Will this be inflation adjusted?", value = F)
      ),
      
      #Cashflow 2
      h4("Additional Cash Flow #2"),
      checkboxInput("cashflow_2", "Recieving Cash Flow?", value = FALSE),
      
      conditionalPanel(
        condition = "input.cashflow_2 == true",
        sliderInput("rangeinput_2", "During Which Years Will You Recieve This Income? Select a Range:",
                    min = 0, max = 50,
                    value = c(15, 30))
      ),
      conditionalPanel(
        condition = "input.cashflow_2 == true",
        numericInput("flowamount_2", "Amount Recieved Each Year.", min = 0, value = 25000, step = 1000)
      ),
      conditionalPanel(
        condition = "input.cashflow_2 == true",
        checkboxInput("inflation_2", "Will this be inflation adjusted?", value = F)
      ),
      
      #Cashflow 3
      h4("Additional Cash Flow #3"),
      checkboxInput("cashflow_3", "Recieving Cash Flow?", value = FALSE),
      
      conditionalPanel(
        condition = "input.cashflow_3 == true",
        sliderInput("rangeinput_3", "During Which Years Will You Recieve This Income? Select a Range:",
                    min = 0, max = 50,
                    value = c(15, 30))
      ),
      conditionalPanel(
        condition = "input.cashflow_3 == true",
        numericInput("flowamount_3", "Amount Recieved Each Year.", min = 0, value = 25000, step = 1000)
      ),
      conditionalPanel(
        condition = "input.cashflow_3 == true",
        checkboxInput("inflation_3", "Will this be inflation adjusted?", value = F)
      ),
      
      actionButton("run", "Run Simulation"),
      textOutput("status"),
      
    ),
    #Creates the titles, gives the spot to display information
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
        
        #Spot for capital table
        tabPanel("Portfolio Value Table",
                 h3("Portfolio Value Over Time (All Trials)"),
                 DTOutput("capital_table_dt"),  # <- show capital table here
                 downloadButton("download_capital", "Download Portfolio Value Table")
        ),
        
        #Spot for withdrawal table
        tabPanel("Withdrawal Table",
                 h3("Withdrawal Amounts (All Trials)"),
                 DTOutput("withdrawal_table_dt"),  # <- show withdrawal table here
                 downloadButton("download_withdrawal", "Download Withdrawal Table")
        ),
        
        #Spot for portfolio returns table
        tabPanel("Portfolio Returns",
                 h3("Portfolio Returns by Year (All Trials)"),
                 DTOutput("portfolio_table_dt"),
                 downloadButton("download_portfolio", "Download Portfolio Returns Table")
        ),
        
        #Spot for cashflow table
        tabPanel("Cash Inflows",
                 h3("Cashflow income by Year (All Trials)"),
                 DTOutput("cashflow_table_dt"),
                 downloadButton("download_cashflow", "Download Cashflow Income Table")
        ),
        
        #Checks
        tabPanel("Spending At Floor",
                 h3("Years Where Spending Were At the Floor"),
                 DTOutput("floor_table_dt"),
                 downloadButton("download_floor", "Download Spending Floor Table")
        ),
        
        
        #Checks
        tabPanel("Spending Below Initial",
                 h3("Years Where Spending Were Below Initial"),
                 DTOutput("initial_table_dt"),
                 downloadButton("download_initial", "Download Spending Initial Table")
        ),
        
        #Information tab
        tabPanel("How to Use the Simulation",
                 h3("How to Use the Monte Carlo Retirement Simulation"),
                 p("This simulation models retirement withdrawal strategies under market uncertainty using a Monte Carlo approach."),
                 
                 h4("Inputs"),
                 tags$ul(
                   tags$li(HTML("<b>Average Gain (%):</b> The average expected annual return of the portfolio.")),
                   tags$li(HTML("<b>Portfolio Std Dev (%):</b> The volatility of the portfolio's returns.")),
                   tags$li(HTML("<b>Time:</b> The number of years for the retirement simulation.")),
                   tags$li(HTML("<b>Initial Withdrawal Rate (%):</b> The withdrawal rate at the start of retirement.<br>
               - This will be the ceiling spending adjusted for inflation (see in withdrawal graph)")),
                   tags$li(HTML("<b>Minimum Withdrawal Rate (%):</b> The lowest rate you are willing to withdraw.<br>
               - This will be the floor spending adjusted for inflation (see in withdrawal graph)")),
                   tags$li(HTML("<b>Probability Thresholds:</b> Used to trigger changes in spending based on portfolio performance.<br>
               - Lower probability of success triggers a spending cut<br>
               - Higher probability of success triggers a spending increase<br>
               - Probability of success is calculated through projected withdrawal divided by portfolio value")),
                   tags$li(HTML("<b>Spending Increase/Decrease (%):</b> Adjustment to spending if portfolio performance is strong/weak.")),
                   tags$li(HTML("<b>Initial Capital:</b> The starting amount of money in your retirement account.")),
                   tags$li(HTML("<b>Inflation Rate (%):</b> Assumed annual inflation.")),
                   tags$li(HTML("<b>Number of Trials:</b> How many simulations to run. More trials leads to better estimates."))
                 ),
                 
                 h4("Outputs"),
                 tags$ul(
                   tags$li(HTML("<b>Probability of Success:</b> % of simulations where you don’t run out of money.")),
                   tags$li(HTML("<b>Time at Floor:</b> Amount of time withdrawal rate was at the floor (out of all year*trials).")),
                   tags$li(HTML("<b>Time Below Initial:</b> Amount of time withdrawal rate was below the initial withdrawal (out of all year*trials).")),
                   tags$li(HTML("<b>Spending Cuts:</b> % of years where spending had to be cut according to probability of success.")),
                   tags$li(HTML("<b>Average Ending Portfolio Value:</b> Average wealth between all your trials at the end. (inflation adjusted)")),
                   tags$li(HTML("<b>Percentile Wealth:</b> 25/50/75th percentile of wealth left at the end of all your trials. (inflation adjusted)")),
                   tags$li(HTML("<b>Plot:</b> Average capital remaining within each year.(Includes 0 for years without money")),
                   tags$li(HTML("<b>Plot:</b> Median capital remaining within each year. (Includes 0 for years without money)")),
                   tags$li(HTML("<b>Plot:</b> Average withdrawal within each year. (Does NOT include 0 for years without money)"))
                 )
                 
        )
      )
    )
  ) 
)  
run_trigger <- reactiveVal(FALSE)

server <- function(input, output) {
  
  #Shows the simulation running text
  observeEvent(input$run, {
    output$status <- renderText({"Simulation running, please wait..."})
    
    invalidateLater(1)
    
    
    later::later(function() {
      run_trigger(TRUE)
    }, delay = 0.1)
  })
  
  
  observe({
    
    #requires to press the button to do the logic
    req(run_trigger())
    req(input$run)
    
    run_trigger(FALSE)
    
    t_input <- input$t_input
    valid_1 <- TRUE
    valid_2 <- TRUE
    valid_3 <- TRUE
    
    #Gives Error if inputs are not correct
    if (input$initial_percent < input$floor_percent) {
      showModal(modalDialog(
        title = "Input Error",
        "Initial Withdrawal Rate must be greater than or equal to the Floor Withdrawal Rate.",
        easyClose = TRUE
      ))
      valid_1 <- FALSE
    }
    
    #POS check
    if (as.numeric(input$lowPoS_input) >= as.numeric(input$upPoS_input)) {
      showModal(modalDialog(
        title = "Input Error",
        "Lower Probability Threshold must be less than the Upper Probability Threshold.",
        easyClose = TRUE
      ))
      valid_2 <- FALSE
    }
    
    #cashflow check
    check_error <- function(active, range) {
      if (!active) {
        return(TRUE)
      }
      
      if (is.null(range)) {
        return(FALSE)
      }
      
      if (active) {
        if (range[2] > t_input) {
          showModal(modalDialog(
            title = "Input Error",
            "Range of Years Must Be Within the simulation years input.",
            easyClose = T
          ))
          return(FALSE)
        }
        return(TRUE)
      }
    }
    
    #converts all the inputs to usable variables
    mean_input <- input$mean_input
    stdv_input <- input$stdv_input
    initial_percent <- input$initial_percent/100
    floor_percent <- input$floor_percent/100
    lowPoS_input <- input$lowPoS_input
    upPoS_input <- input$upPoS_input
    spending.inc_input <- input$spending_inc/100
    spending.dec_input <- input$spending_dec/100
    capital_input <- input$capital_input
    inflation <- input$inflation/100
    trials <- input$trials
    
    
    #checks each specific cash flow
    check_error(input$cashflow_1, input$rangeinput_1)
    check_error(input$cashflow_2, input$rangeinput_2)
    check_error(input$cashflow_3, input$rangeinput_3)
    
    #checks if all cashflow checks were ok
    valid_3 <- check_error(input$cashflow_1, input$rangeinput_1) && 
      check_error(input$cashflow_2, input$rangeinput_2) &&
      check_error(input$cashflow_3, input$rangeinput_3)
    
    #Checks if all checks were ok
    valid <- valid_1 && valid_2 && valid_3
    
    
    
    #Checks if valid is false (from checks earlier) then proceeds if it is true
    if (valid == F){
      return()
    }
    
    if (T == T) {
      showModal(modalDialog(
        title = "Processing",
        "The simulation is processing, please wait until it is completed. Thank you.",
        easyClose = TRUE
      ))
    }
    
    #Combines cashflow into a list
    apply_cashflow <- function(active, range, amount, inflation_adjusted, t_input, inflation_rate) {
      vec <- rep(0, t_input)
      if (active) {
        start <- range[1]
        end <- range[2]
        for (yr in start:end) {
          vec[yr] <- if (inflation_adjusted) {
            amount * (1 + inflation_rate)^(yr - 1)
          } else {
            amount
          }
        }
      }
      return(vec)
    }
    
    #cashflow 1, 2, 3
    cf1 <- apply_cashflow(input$cashflow_1, input$rangeinput_1, input$flowamount_1, input$inflation_1, t_input, inflation)
    cf2 <- apply_cashflow(input$cashflow_2, input$rangeinput_2, input$flowamount_2, input$inflation_2, t_input, inflation)
    cf3 <- apply_cashflow(input$cashflow_3, input$rangeinput_3, input$flowamount_3, input$inflation_3, t_input, inflation)
    
    # Combine all cash flows into one stream
    total_cashflow <- cf1 + cf2 + cf3
    
    
    #removes scientific notation in table
    options(scipen = 999)
    
    #Assigns actual numbers to percentage inputs
    initial_input <- initial_percent * capital_input
    floor_input <- floor_percent * capital_input
    
    #Loops for inflation initial and floor
    inflation_initial = c(initial_input)
    inflation_floor = c(floor_input)
    
    #Pre makes the list for the inflation floor and initial through the years
    #it is t_input - 1 because the list already has the previous value in it (row 24-25)
    for (i in 1:(t_input-1)) {
      inflation_initial <- c(inflation_initial, (inflation_initial[i] * (1+inflation)))
      inflation_floor <- c(inflation_floor, (inflation_floor[i] * (1+inflation)))
    }
    
    
    #Defining Counters
    ranout = 0
    wealth_storage = c()
    timefloor = 0
    timebelowinitial = 0
    spendingcut = 0
    calculate = 0
    timebelowinitial_year = 0
    timefloor_year = 0
    
    # Create matrix filled with NA
    empty_matrix <- matrix(NA, nrow = t_input, ncol = trials)
    special_matrix <- matrix(0, nrow = (t_input), ncol = trials)
    
    # Creates sheet for capital data storage
    capital_table <- as.data.frame(empty_matrix)
    colnames(capital_table) <- paste0("trial", ' ', 1:trials)
    rownames(capital_table) <- paste0("year", ' ', 1:t_input)
    
    # Creates sheet for withdrawal amount data storage
    withdrawal_table <- as.data.frame(empty_matrix)
    colnames(withdrawal_table) <- paste0("trial", ' ', 1:trials)
    rownames(withdrawal_table) <- paste0("year", ' ', 1:t_input)
    
    # Creates sheet for probability of success amount data storage
    lowPoS_table <- as.data.frame(empty_matrix)
    colnames(lowPoS_table) <- paste0("trial", ' ', 1:trials)
    rownames(lowPoS_table) <- paste0("year", ' ', 1:(t_input))
    
    upPoS_table <- as.data.frame(empty_matrix)
    colnames(upPoS_table) <- paste0("trial", ' ', 1:trials)
    rownames(upPoS_table) <- paste0("year", ' ', 1:(t_input))
    
    # Creates sheet for probability of success amount data storage
    Portfolio_table <- as.data.frame(empty_matrix)
    colnames(Portfolio_table) <- paste0("trial", ' ', 1:trials)
    rownames(Portfolio_table) <- paste0("year", ' ', 1:(t_input))
    
    # Creates sheet for probability of success amount data storage
    timebelowinitial_year_table <- as.data.frame(special_matrix)
    colnames(timebelowinitial_year_table) <- paste0("trial", ' ', 1:trials)
    rownames(timebelowinitial_year_table) <- paste0("year", ' ', 1:(t_input))
    
    # Creates sheet for probability of success amount data storage
    timefloor_year_table <- as.data.frame(special_matrix)
    colnames(timefloor_year_table) <- paste0("trial", ' ', 1:trials)
    rownames(timefloor_year_table) <- paste0("year", ' ', 1:(t_input))
    
    # Creates sheet for cashflow return of each year
    cashflow_table <- as.data.frame(empty_matrix)
    colnames(cashflow_table) <- paste0("trial", ' ', 1:trials)
    rownames(cashflow_table) <- paste0("year", ' ', 1:(t_input))
    
    #Checks if lower PoS is lower number than upper PoS, if not, stops and breaks code
    
    #Stores PoS Lists 
    PoS_list <- list(
      pos20 = c(
        0.0565, 0.0570, 0.0575, 0.0580, 0.0584, 0.0588, 0.0592, 0.0596,
        0.0600, 0.0605, 0.0610, 0.0615, 0.0620, 0.0625, 0.0631, 0.0638,
        0.0645, 0.0654, 0.0665, 0.0677, 0.0690, 0.0704, 0.0718, 0.0731,
        0.0745, 0.0759, 0.0774, 0.0790, 0.0810, 0.0831, 0.0854, 0.0880,
        0.0912, 0.0945, 0.0980, 0.1025, 0.1075, 0.1130, 0.1195, 0.1275,
        0.1375, 0.1490, 0.1625, 0.1815, 0.2060, 0.2390, 0.2890, 0.3715,
        0.5360
      ),
      
      pos30 = c(
        0.0500, 0.0505, 0.0510, 0.0515, 0.0520, 0.0523, 0.0526, 0.0530, 0.0535,
        0.0540, 0.0545, 0.0552, 0.0558, 0.0565, 0.0572, 0.0580, 0.0587, 0.0595,
        0.0605, 0.0615, 0.0625, 0.0635, 0.0650, 0.0662, 0.0675, 0.0690, 0.0705,
        0.0725, 0.0745, 0.0765, 0.0790, 0.0815, 0.0845, 0.0875, 0.0915, 0.0955,
        0.1000, 0.1060, 0.1125, 0.1200, 0.1300, 0.1410, 0.1550, 0.1725, 0.1970,
        0.2300, 0.2800, 0.3625, 0.5250
      ),
      
      pos40 = c(
        0.0455, 0.0460, 0.0464, 0.0468, 0.0472, 0.0476, 0.0480, 0.0485, 0.0490,
        0.0494, 0.0498, 0.0503, 0.0509, 0.0515, 0.0522, 0.0530, 0.0538, 0.0545,
        0.0554, 0.0564, 0.0575, 0.0585, 0.0596, 0.0608, 0.0621, 0.0636, 0.0652,
        0.0670, 0.0690, 0.0710, 0.0733, 0.0758, 0.0788, 0.0822, 0.0860, 0.0900,
        0.0944, 0.1000, 0.1065, 0.1140, 0.1230, 0.1340, 0.1475, 0.1650, 0.1890,
        0.2230, 0.2725, 0.3550, 0.5200
      ),
      
      pos50 = c(
        0.0414, 0.0418, 0.0421, 0.0425, 0.0429, 0.0433, 0.0437, 0.0441, 0.0445,
        0.0450, 0.0455, 0.0460, 0.0466, 0.0473, 0.0480, 0.0487, 0.0495, 0.0503,
        0.0511, 0.0519, 0.0527, 0.0538, 0.0550, 0.0563, 0.0576, 0.0590, 0.0605,
        0.0622, 0.0644, 0.0665, 0.0685, 0.0710, 0.0740, 0.0770, 0.0805, 0.0845,
        0.0889, 0.0945, 0.1010, 0.1085, 0.1170, 0.1280, 0.1415, 0.1590, 0.1830,
        0.2150, 0.2645, 0.3470, 0.5110
      ),
      
      pos60 = c(
        0.0375, 0.0379, 0.0383, 0.0386, 0.0389, 0.0393, 0.0397, 0.0401, 0.0405,
        0.0410, 0.0415, 0.0420, 0.0425, 0.0431, 0.0437, 0.0444, 0.0452, 0.0460,
        0.0468, 0.0477, 0.0486, 0.0496, 0.0507, 0.0518, 0.0531, 0.0545, 0.0560,
        0.0576, 0.0595, 0.0615, 0.0637, 0.0662, 0.0690, 0.0720, 0.0755, 0.0795,
        0.0840, 0.0894, 0.0955, 0.1025, 0.1110, 0.1220, 0.1355, 0.1530, 0.1760,
        0.2080, 0.2575, 0.3400, 0.5030
      ),
      
      pos70 = c(
        0.0335, 0.0339, 0.0343, 0.0346, 0.0349, 0.0352, 0.0356, 0.0360, 0.0364, 0.0368,
        0.0372, 0.0377, 0.0383, 0.0390, 0.0397, 0.0405, 0.0412, 0.0419, 0.0426, 0.0433,
        0.0441, 0.0452, 0.0464, 0.0477, 0.0490, 0.0503, 0.0516, 0.0531, 0.0547, 0.0567,
        0.0589, 0.0613, 0.0640, 0.0670, 0.0705, 0.0740, 0.0785, 0.0839, 0.0895, 0.0965,
        0.1050, 0.1160, 0.1290, 0.1455, 0.1685, 0.2005, 0.2500, 0.3310, 0.4940
      ),
      
      pos80 = c(
        0.0290, 0.0293, 0.0296, 0.0299, 0.0302, 0.0306, 0.0310, 0.0315, 0.0320,
        0.0325, 0.0330, 0.0335, 0.0340, 0.0345, 0.0350, 0.0355, 0.0361, 0.0368,
        0.0376, 0.0385, 0.0395, 0.0405, 0.0415, 0.0425, 0.0435, 0.0448, 0.0463,
        0.0480, 0.0497, 0.0515, 0.0535, 0.0555, 0.0582, 0.0610, 0.0645, 0.0685,
        0.0725, 0.0775, 0.0831, 0.0900, 0.0980, 0.1080, 0.1210, 0.1375, 0.1605,
        0.1925, 0.2400, 0.3200, 0.4830
      )
      
    )
    #Stores PoS use depending on inputs
    lowPoS <- PoS_list[[paste0("pos", lowPoS_input)]]
    upPoS <- PoS_list[[paste0("pos", upPoS_input)]]
    
    lngthLow <- length(lowPoS)
    lngthUp <- length(upPoS)
    
    # Loops through each trial (trials from the input)
    for (i in 1:trials) {
      #Gets the percent run for the portfolio
      percent_run <- rnorm((t_input), mean = (mean_input/100), sd = (stdv_input/100))
      
      #Resets the capital
      new_capital <- (capital_input - initial_input + total_cashflow[1]) * (1 + percent_run[1])
      
      #puts in the cashflow
      cashflow_table[1,i] <- total_cashflow[1]
      
      #Puts in the capital 
      capital_table[1, i] <- format(new_capital, big.mark = ",", scientific = FALSE)
      
      #Puts in the initial withdrawal
      withdrawal_table[1, i] <- format(initial_input, big.mark = ",", scientific = FALSE)
      
      #Resets withdrawal
      withdrawal <- initial_input
      
      #Resets floor price
      floor <- floor_input
      
      #Adds a year to the calculator cause this is considered a year
      calculate <- calculate + 1
      
      #Stores portfolio run in the first year
      Portfolio_table[1,i] <- paste0((round(100*percent_run[1],2)), "%")
      
      #Resets it
      timebelowinitial_year = 0
      
      #Resets it
      timefloor_year = 0
      
      timebelowinitial_year_table[1,i] = timebelowinitial_year
      timefloor_year_table[1,i] = timefloor_year
      
      for (x in 2:t_input) {
        #Resets it
        timebelowinitial_year = 0
        
        #Resets it
        timefloor_year = 0
        
        #Adds cashflow before to make accurate withdrawal amount
        new_capital <- (new_capital + total_cashflow[x])
        
        #Adds cashflow to table
        cashflow_table[x,i] <- total_cashflow[x]
        
        #Deal with the withdrawal
        distribution_rate <- ((withdrawal*(1+inflation))/new_capital)
        
        #If dist rate is greater than low PoS and the withdrawal is bigger than the floor
        #Decrease the withdrawal by inputted percentage
        #Adds 1 to the counter of dec. spending
        if (distribution_rate > lowPoS[lngthLow - t_input + x] & withdrawal > inflation_floor[x])  {
          withdrawal <- withdrawal * (1-spending.dec_input) * (1 + inflation)
          spendingcut <- spendingcut + 1
          
          #Check to make sure new value isnt too low either
          #if so, it will add to the time floor
          if (withdrawal <= inflation_floor[x]) {
            withdrawal <- inflation_floor[x]
            timefloor <- timefloor + 1
            timefloor_year <- timefloor_year + 1
          }
          
          #If the dist rate is lower than the upper PoS then
          #increase the spending by the percentage indicated
        } else if (distribution_rate < upPoS[lngthUp - t_input + x]) {
          withdrawal <- withdrawal * (1+spending.inc_input) * (1 + inflation)
          
          if (withdrawal > inflation_initial[x]) {
            withdrawal <- inflation_initial[x]
          }
          #If the withdrawal is below the floor
          #Adds to the time floor
        } else if ((withdrawal*(1+inflation)) == inflation_floor[x]) {
          withdrawal <- inflation_floor[x]
          timefloor <- timefloor + 1
          timefloor_year <- timefloor_year + 1
          timefloor_year_table[x,i] <- timefloor_year
          
          #If none others are true, it will increase the spending by inflation rate
        } else if ((withdrawal*(1+inflation)) > inflation_initial[x]) {
          withdrawal <- inflation_initial[x]  
          
        } else {
          withdrawal <- withdrawal * (1+inflation)
        }
        #Dealing with capital value
        #Technically withdrawal is done and then percent run at the end of the year
        #But this is equivelant
        new_capital <- (new_capital - withdrawal) * (1 + percent_run[x])
        
        
        #Adds one to the calculator to determine actual years
        calculate <- calculate + 1
        
        #Checks if capital reaches below 0 and will 
        #stop trial if done so
        #Add 0 to the wealth table
        #Add 0 to capital distribution
        if (new_capital < 0) {
          ranout <- ranout + 1
          capital_table[x,i] <- 0
          spendingcut <- spendingcut + 1
          timebelowinitial <- timebelowinitial + 1
          withdrawal_table[x, i] <- format(round(withdrawal + (new_capital/(1+ percent_run[x])), 2), big.mark = ",", scientific = FALSE)
          wealth_storage <- c(wealth_storage,0)
          Portfolio_table[x,i] <- "No Money"
          break
        }
        
        #Adds capital to table if money is actually remaining
        capital_table[x, i] <- format(new_capital, big.mark = ",", scientific = FALSE)
        
        #Stores PoS
        lowPoS_table[x,i] <- lowPoS[lngthLow - t_input + x]
        upPoS_table[x,i] <- upPoS[lngthUp - t_input + x]
        
        #Stores Percent run of portfolio
        Portfolio_table[x,i] <- paste0((round(100*percent_run[x],2)), "%")
        
        #If dist rate is greater than low PoS and the withdrawal is bigger than the floor
        #Decrease the withdrawal by inputted percentage
        #Adds 1 to the counter of dec. spending
        
        #Adds withdrawal amount to the data sheet
        withdrawal_table[x, i] <- format(withdrawal, big.mark = ",", scientific = FALSE)
        
        
        #Checks for counters
        if (withdrawal < inflation_initial[x]) {
          timebelowinitial <- timebelowinitial + 1
          timebelowinitial_year <- timebelowinitial_year + 1
        }
        
        timefloor_year_table[x,i] <- timefloor_year
        timebelowinitial_year_table[x,i] <- timebelowinitial_year
        
        
        #Checks for end of specific trial
        if (x == t_input){
          #Store wealth
          wealth_storage <- c(wealth_storage, new_capital)
        } 
      }  
    }
    
    
    #Separate Sheet for Wealth Distribution
    wealth_distribution = data.frame(
      `Wealth Distribution (Sorted)` = format(
        round(sort(wealth_storage), 2),
        big.mark = ",",
        scientific = FALSE
      )
    )
    
    sorted_wealth <- sort(wealth_storage)
    
    # Calculate index for percentiles
    percentile_25 <- round(0.25 * trials)
    percentile_50 <- round(0.5 * trials)
    percentile_75 <- round(0.75 * trials)
    
    # Create data frame for outputs
    outputs = data.frame(
      Probability.of.success = paste0(100 - (ranout / 10), "%"),
      Time.At.Floor = paste0(round((100 * timefloor) / (trials * t_input), 1), "%"),
      Time.below.initial = paste0(round((100 * timebelowinitial) / (trials * t_input), 1), "%"),
      Amount.of.times.spending.cut = paste0(round((100 * spendingcut) / (trials * t_input), 1), "%"),
      Average.of.Wealth = paste0(
        "$", format(round((mean(wealth_storage))/(1+inflation)**t_input), big.mark = ",", scientific = FALSE)
      ),
      Median.of.Wealth = paste0(
        "$", format(round((median(sorted_wealth))/(1+inflation)**t_input), big.mark = ",", scientific = FALSE)
      ),
      Percentile25.of.Wealth = paste0(
        "$", format(round((sorted_wealth[percentile_25])/(1+inflation)**t_input), big.mark = ",", scientific = FALSE)
      ),
      Percentile75.of.Wealth = paste0(
        "$", format(round((sorted_wealth[percentile_75])/(1+inflation)**t_input), big.mark = ",", scientific = FALSE)
      )
    )
    
    
    
    # Set column names
    colnames(outputs) <- c("Probability of Success",
                           "Time At Floor Overall",
                           "% of Time Below Initial",
                           "% of Time Spending was Cut",
                           "Average Ending Portfolio Value",
                           "50th Percentile of Ending Portfolio Value",
                           "25th Percentile of Ending Portfolio Value",
                           "75th Percentile of Ending Portfolio Value")
    
    #Creates variables for graph display
    y_mean <- c(capital_input)
    y_median <- c(capital_input)
    y_withdrawal_mean <- c()
    x <- c(0)
    x_withdrawal <- c()
    y_capital <- c()
    y_withdrawal <- c()
    
    y_mean_inflation <- c(capital_input)
    y_median_inflation <- c(capital_input)
    y_withdrawal_inflation <- c()
    
    #Goes through a loop, grabs all the stuff from a certain year, stores them
    for (z in 1:t_input){
      
      x <- c(x,z)
      x_withdrawal <- c(x_withdrawal,z)
      y_capital <- c()
      y_withdrawal <- c()
      
      for (i in 1:trials){
        if (is.na(capital_table[z, i])) {
          y_capital <- c(y_capital,0) 
        } else {
          y_capital <- c(y_capital, as.numeric(gsub(",", "", capital_table[z,i])))
        }
        if (is.na(withdrawal_table[z,i])){
          #pass
        } else {
          y_withdrawal <- c(y_withdrawal, as.numeric(gsub(",", "", withdrawal_table[z,i])))
        }
        
      }
      #now stores: at this point, y is a full list
      y_mean <- c(y_mean, mean(y_capital))
      y_median <- c(y_median, median(y_capital))
      y_withdrawal_mean <- c(y_withdrawal_mean, mean(y_withdrawal))
      
      y_mean_inflation <- c(y_mean_inflation, (mean(y_capital))/(1+inflation)**z)
      y_median_inflation <- c(y_median_inflation, (median(y_capital))/(1+inflation)**z)
      y_withdrawal_inflation <- c(y_withdrawal_inflation, (mean(y_withdrawal))/(1+inflation)**(z-1))
    }
    
    
    output$outputs1 <- renderTable(outputs)
    
    #Displays plot for average capital
    output$result_plot <- renderPlot({
      plot(x, y_mean, type = "l", col = "blue", lwd = 3,
           xlab = "Year", ylab = "Avg Wealth Each Year", main = "Average Wealth Plot",
           xaxs = "i",  # Removes default horizontal padding
           yaxs = "i",  # Removes default vertical padding
           ylim = c(max(0, min(y_mean) - 250000), max(y_mean) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    #Displays plot for median capital
    output$result_plot2 <- renderPlot({
      plot(x, y_median, type = "l", col = "red", lwd = 3,
           xlab = "Year", ylab = "Median Wealth Each Year", main = "Median Wealth Plot",
           xaxs = "i",  # Removes padding on x-axis
           yaxs = "i",  # Removes padding on y-axis
           ylim = c(max(0, min(y_median) - 250000), max(y_median) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    #Displays plot for average withdrawal
    output$result_plot3 <- renderPlot({
      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(x_withdrawal, y_withdrawal_mean, type = "l", col = "green", lwd = 3,
           xlab = "Year", ylab = "Average Withdrawal Each Year", main = "Average Withdrawal Plot",
           xaxs = "i",
           yaxs = "i",  
           ylim = c(max(0, min(inflation_floor) - 10000), max(inflation_initial) * 1.1))
      
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
      
      # Add inflation-adjusted initial and floor lines
      lines(x_withdrawal, inflation_initial, col = "blue", lty = "dashed", lwd = 2)
      lines(x_withdrawal, inflation_floor, col = "red", lty = "dashed", lwd = 2)
      
      #Creates a legend
      legend("topleft",
             legend = c("Avg Withdrawal", "Ceiling Withdrawal", "Floor Withdrawal"),
             col = c("green", "blue", "red"),
             lty = c("solid", "dashed", "dashed"),
             lwd = 2,
             bty = "o",
             bg = "white") 
    })
    
    #Inflation Adjusted Mean
    output$result_plot4 <- renderPlot({
      plot(x, y_mean_inflation, type = "l", col = "blue", lwd = 3,
           xlab = "Year", ylab = "Avg Wealth Each Year", main = "Average Wealth Plot (inflation-adjusted value)",
           xaxs = "i",  # Removes default horizontal padding
           yaxs = "i",  # Removes default vertical padding
           ylim = c(max(0, min(y_mean_inflation) - 250000), max(y_mean_inflation) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    #Inflation Adjusted Median
    output$result_plot5 <- renderPlot({
      plot(x, y_median_inflation, type = "l", col = "red", lwd = 3,
           xlab = "Year", ylab = "Median Wealth Each Year", main = "Median Wealth Plot (inflation-adjusted value)",
           xaxs = "i",  # Removes padding on x-axis
           yaxs = "i",  # Removes padding on y-axis
           ylim = c(max(0, min(y_median_inflation) - 250000), max(y_median_inflation) * 1.1))
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
    })
    
    
    #Inflation Adjusted Withdrawal
    output$result_plot6 <- renderPlot({
      par(mar = c(5, 4, 4, 2) + 0.1)
      plot(x_withdrawal, y_withdrawal_inflation, type = "l", col = "green", lwd = 3,
           xlab = "Year", ylab = "Average Withdrawal Each Year", main = "Average Withdrawal Plot (inflation-adjusted value)",
           xaxs = "i",
           yaxs = "i",  
           ylim = c(max(0, min(floor_input) - 10000), max(initial_input) * 1.1))
      
      grid(col = "gray50", lty = "dotted", lwd = 1.2)
      
      # Add inflation-adjusted initial and floor lines
      lines(x_withdrawal, rep(initial_input, length(x_withdrawal)), col = "blue", lty = "dashed", lwd = 2)
      lines(x_withdrawal, rep(floor_input, length(x_withdrawal)), col = "red", lty = "dashed", lwd = 2)
      
      #Creates a legend
      legend("topleft",
             legend = c("Avg Withdrawal", "Ceiling Withdrawal", "Floor Withdrawal"),
             col = c("green", "blue", "red"),
             lty = c("solid", "dashed", "dashed"),
             lwd = 2,
             bty = "o",
             bg = "white") 
    })
    
    
    
    #Displays the tables in the other column
    output$capital_table_dt <- renderDT({
      datatable(capital_table, options = list(scrollX = TRUE))
    })
    
    output$withdrawal_table_dt <- renderDT({
      datatable(withdrawal_table, options = list(scrollX = TRUE))
    })
    
    output$download_capital <- downloadHandler(
      filename = function() {"capital_table.csv"},
      content = function(file) {
        write.csv(capital_table, file, row.names = TRUE)
      }
    )
    
    output$download_withdrawal <- downloadHandler(
      filename = function() {"withdrawal_table.csv"},
      content = function(file) {
        write.csv(withdrawal_table, file, row.names = TRUE)
      }
    )
    output$portfolio_table_dt <- renderDT({
      datatable(Portfolio_table, options = list(scrollX = TRUE))
    })
    
    output$download_portfolio <- downloadHandler(
      filename = function() {"portfolio_table.csv"},
      content = function(file) {
        write.csv(Portfolio_table, file, row.names = TRUE)
      }
    )
    
    output$cashflow_table_dt <- renderDT({
      datatable(cashflow_table, options = list(scrollX = TRUE))
    })
    
    output$download_cashflow <- downloadHandler(
      filename = function() {"cash_flow.csv"},
      content = function(file) {
        write.csv(cashflow_table, file, row.names = TRUE)
      }
    )
    
    
    output$floor_table_dt <- renderDT({
      datatable(timefloor_year_table, options = list(scrollX = TRUE))
    })
    
    output$download_floor <- downloadHandler(
      filename = function() {"timefloor_year_table.csv"},
      content = function(file) {
        write.csv(timefloor_year_table, file, row.names = TRUE)
      }
    )
    
    output$initial_table_dt <- renderDT({
      datatable(timebelowinitial_year_table, options = list(scrollX = TRUE))
    })
    
    output$download_initial <- downloadHandler(
      filename = function() {"timebelowinitial_year_table.csv"},
      content = function(file) {
        write.csv(timebelowinitial_year_table, file, row.names = TRUE)
      }
    )
    
    output$status <- renderText({"Simulation complete!"})
    run_trigger(FALSE)
    
  })
}

#Puts it all together (ui variable and server variable)
shinyApp(ui = ui, server = server)
