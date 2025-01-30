# Define required packages
packages <- c(
  "shiny", "shinythemes", "tseries", "strucchange", "zoo", "ggplot2", 
  "lmtest", "car", "WRS2", "sandwich", "extrafont", "DT", "plotly", 
  "shinyBS", "shinyjs", "shinycssloaders", "shinyAce"
)

# Install missing packages
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Apply function to all packages
invisible(lapply(packages, install_if_missing))

r_script_path <- "https://cognovo.net/statistics/mortality-data-destatis/sba-standalone1.R"  

# Set global options for custom error handling
options(shiny.error = function() {
  showNotification("...", type = "error")
})



library(thematic)
thematic::thematic_on(bg = "#033568", fg = "white", accent = "#738FAB", font = "Source Sans Pro")

# Enable thematic theming for all plots
thematic_shiny(font = "auto")

# Import and load fonts (run font_import() only once)
# font_import()  # Uncomment if this is the first time running
loadfonts(device = "pdf")  # Load fonts for PDF output

# Load the dataset
data <- read.csv("https://cognovo.net/statistics/mortality-data-destatis/dataset-tidy1.csv")
data$week <- ifelse(data$week > 52, 52, data$week)
data$date <- as.Date(paste(data$year, data$week, 1, sep = "-"), format = "%Y-%U-%u")

if (!"mortality.rate" %in% colnames(data)) {
  stop("The dataset does not contain a column named 'mortality.rate'. Please check the dataset.")
}

# Define UI for the application
ui <- fluidPage(
  useShinyjs(),  # Activate shinyjs globally
  withMathJax(),  # For mathematical rendering
  theme = shinytheme("flatly"),
  titlePanel(
    div(
      "Structural Breakpoint Analysis",
      style = "color: #033568; font-weight: bold; font-size: 24px; text-align: center;"
    )
  ),
  tags$head(
    tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@300;400;600&display=swap');
    
    body {
      font-family: 'Source Sans Pro', sans-serif;
      cursor: crosshair;
      transition: background-color 0.5s, color 0.5s;
    }
    .inverted {
      filter: invert(1) hue-rotate(180deg);
      background-color: black !important;
      color: white !important;
    }
    .shiny-output-error { color: red; }
    .shiny-output-error:before { content: 'Error: '; }
    
    .js-plotly-plot .plotly .modebar-btn {
      display: none;
    }
    a {
      color: #033568;
      text-decoration: none;
    }
    .btn-primary {
      margin: 5px;
      padding: 5px;
    }
    .shiny-output-error {
	display: none;
}
    .btn-default {
      color: #ffffff;
      background-color: #95a5a6;
      border-color: #95a5a6;
      margin: 5px;
      padding: 5px;
    }
    .btn-secondary {
      margin: 5px;
      padding: 5px;
      background-color: #738FAB;
      border: none;
      color: white;
    }
    #loading-content {
      position: fixed;
      top: 0;
      left: 0;
      width: 100vw;
      height: 100vh;
      background-color: rgba(255, 255, 255, 0.8);
      z-index: 9999;
      display: none;
      text-align: center;
    }
    #loading-content img {
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      max-width: 300px;
    }
  ")),
    tags$script(HTML("
    $(document).ready(function() {
      $('#collapsePanel .panel-collapse').addClass('in'); // Force open
    });
    
    document.getElementById('invert_colors').addEventListener('click', function() {
      document.body.classList.toggle('inverted');
    });
  "))
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "yearRange", 
        "Select the Time Period for Analysis:", 
        min = min(data$year), 
        max = max(data$year), 
        value = c(2019, 2023), 
        sep = "", 
        step = 1
      ),
      actionButton(
        "analyze", 
        label = tagList(icon("chart-line"), "Run Analysis"), 
        style = "background-color: #28a745; color: white; font-size: 18px; padding: 10px 20px; border-radius: 6px;"
      )
      ,
      
      actionButton(
        "showData", 
        label = tagList(icon("table"), "Show Data"), 
        class = "btn btn-default"
      ),
      tags$a(
        id = "dataset-link",
        href = "https://cognovo.net/statistics/mortality-data-destatis/dataset-tidy1.csv",
        target = "_blank",
        class = "btn btn-default",
        tagList(icon("download"), "Download Data")
      ),
   #   tags$a(
    #    id = "download-csv-link",
     #   href = "#",  # Placeholder; adjusted dynamically in server
      #  target = "_blank",
       # class = "btn btn-default",
      #  tagList(icon("file-csv"), "Download Results")
    #  ),
      actionButton(
        "invert_colors", 
        class = "btn btn-default",
        tagList(icon("adjust"), "Invert Color Schema")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        # Tab 1: Time Series Overview
        tabPanel("Time Series",
                 div(
                   id = "loading-content",
                   tags$img(src = "https://cognovo.net/statistics/mortality-data-destatis/workers.gif")
                 ),
                 bsCollapse(
                   id = "collapsePanel",  
                   open = "collapsePanel", 
                   bsCollapsePanel(
                     title = HTML("<i class='fa fa-chevron-down'></i> Full Time Series"), # Title displayed for the collapsible panel
                     style = "primary",  # Styling for the panel: primary, success, danger, etc.
                     
                     # Plotly Output for the Time Series Plot
                     plotlyOutput("initialTimeSeriesPlot", height = "350px")
                   )
                 ),
                 tags$p("'There is no excuse for failing to plot and look.' – John Tukey  (Exploratory Data Analysis, 1977)"),
                 plotlyOutput("breakpointPlotInteractive") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5)
        ),
        
        # Tab 2: Breakpoint Analysis
        tabPanel("Breakpoint Analysis",
                 verbatimTextOutput("breakpointDetails") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5),
                 uiOutput("explanationStats"),
                 verbatimTextOutput("bpResults") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5)  # Raw results
        ),
        
        # Tab 3: Chow Test
        tabPanel("Chow Test",
                 verbatimTextOutput("chowTestResults") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5),
                 uiOutput("explanationChow"),
                 verbatimTextOutput("chowResultsRaw") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5) # Raw results
        ),
        # Tab ANOVA
        tabPanel("ANOVA",
                 verbatimTextOutput("anovaResults") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5),
                 verbatimTextOutput("etaResults") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5)
                 
                 
        ),
        
        
        # Tab 4: CUSUM
        tabPanel("CUSUM",
                 plotOutput("cusumPlot") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5),
                 verbatimTextOutput("cusumStats") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5), # CUSUM formula
                 uiOutput("explanationCUSUM2"), #expl
                 plotOutput("recCusumPlot") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5),
                 plotOutput("scoreCusumPlot") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5),
                 plotOutput("recMosumPlot") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5)
                 
        )
        ,
        
        # Tab 5: Diagnostics
        tabPanel("Diagnostics",
                 #verbatimTextOutput("metricsAIC") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5), # AIC step-wise model comparison
                 verbatimTextOutput("stationarityTests") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5),
                 verbatimTextOutput("residualDiagnostics") %>% withSpinner(type = 6, color = "#738FAB", size = 0.5)
        ),
        
        
    
        tabPanel("R Code",
                 aceEditor(
                   outputId = "codeEditor",
                   mode = "r",           # Set mode to R for syntax highlighting
                   theme = "Minty Night",    # Choose a theme
                   height = "500px",
                   value = ""            # Default content
                 ),
                 uiOutput("analysis_content"), # Include HTML for Bai-Perron
                 verbatimTextOutput("explanaition")
        ),
        
        
        
        # Tab 4: A References
        tabPanel("References",
                 verbatimTextOutput("references")
        ),
###########

# Tab 6: Open Access Information
tabPanel("Open Access",
         tags$h3("About the App"),
         tags$h4("Data Sources"),
         tags$ul(
           tags$li(tags$p("Rockenfeller, R., Günther, M., & Mörl, F. (2023). Reports of deaths are an exaggeration: all-cause and NAA-test-conditional mortality in Germany during the SARS-CoV-2 era. ",
                          tags$a(href = "https://doi.org/10.1098/rsos.221551", "Royal Society Open Science"), ".")),
           tags$li("German Federal Statistical Office (DESTATIS): ", 
                   tags$a(href = "https://www.destatis.de/EN/Home/_node.html", "https://www.destatis.de"))
         ),
         tags$h4("Supplementary Materials"),
         tags$p("For additional documentation and scripts, visit the following resources:"),
         tags$ul(
           tags$li(tags$a(href = "https://cran.r-project.org/package=strucchange", "strucchange R Package")),
           tags$li(tags$a(href = "https://github.com/cognovo/sba-app", "GitHub Repository"))
         ),
         tags$h4("Acknowledgments"),
         tags$p("This appplication was developed by Dr. Christopher B. Germann as part of the European Union Open Science Initiative: https://open-science-cloud.ec.europa.eu/"),
         tags$p("Data and methods are shared under ", tags$b("Creative Commons CC BY 4.0"), ".")
)

###############################
        
        
        
      )
    )
  ),
  
  # Loading script for dynamic loading
  tags$script(HTML("
    document.getElementById('loading-content').style.display = 'none';
    Shiny.addCustomMessageHandler('showLoading', function(message) {
      document.getElementById('loading-content').style.display = message.show ? 'block' : 'none';
    });
  "))
)


# Define server logic for the application
server <- function(input, output, session) {
  #########
  # Load the R script at app start
  script_content <- readLines(r_script_path, warn = FALSE)
  updateAceEditor(session, "codeEditor", value = paste(script_content, collapse = "\n"))
  
  # Refresh code content on button click
  observeEvent(input$refreshCode, {
    script_content <- readLines(r_script_path, warn = FALSE)
    updateAceEditor(session, "codeEditor", value = paste(script_content, collapse = "\n"))
  })
  
  ###################
  output$initialTimeSeriesPlot <- renderPlotly({
    # Define the COVID-19 pandemic period in Germany
    covid_start_date <- as.Date("2020-03-22")  # Start of lockdown in Germany
    covid_end_date <- as.Date("2022-04-01")    # End of restrictions in Germany
    
    gg <- ggplot(data, aes(x = date, y = mortality.rate)) +
      geom_line(size = 0.5) +  # Use default color and style from thematic
      # Add vertical lines for start and end of COVID period
      geom_vline(xintercept = as.numeric(covid_start_date), linetype = "dashed", size = 0.1, color = "#69C6AD", alpha = 0.7) +  
      geom_vline(xintercept = as.numeric(covid_end_date), linetype = "dashed", size = 0.1, color = "#69C6AD", alpha = 0.7) +    
      # Add shaded region for COVID period
      annotate(
        "rect",
        xmin = covid_start_date,
        xmax = covid_end_date,
        ymin = min(data$mortality.rate, na.rm = TRUE),
        ymax = max(data$mortality.rate, na.rm = TRUE),
        fill = "#69C6AD",
        alpha = 0.2
      ) +
      # Fine-grained x-axis by year
      scale_x_date(
        breaks = seq(
          from = min(data$date, na.rm = TRUE),
          to = max(data$date, na.rm = TRUE),
          by = "1 year"  # Breaks set by year
        ),
        date_labels = "%Y",  # Format the x-axis labels to show the year only
        expand = c(0, 0)  # Prevent extra padding on the x-axis
      ) +
      labs(
        title = "Full Time Series: Mortality Rate for Germany with highlighted region of interest", 
        x = "Year", 
        y = "Mortality Rate"
      ) +
      theme_minimal()  # Minimal theme with no additional hardcoding
    
    # Convert ggplot to Plotly and add custom hover text for the shaded period
    p <- ggplotly(gg) %>%
      layout(
        title = list(
          text = "<b>Full Time Series: Mortality Rate (Germany)</b>", 
          x = 0.5
        ),
        shapes = list(
          list(
            type = "rect", 
            x0 = as.numeric(covid_start_date), 
            x1 = as.numeric(covid_end_date), 
            y0 = min(data$mortality.rate, na.rm = TRUE), 
            y1 = max(data$mortality.rate, na.rm = TRUE),
            fillcolor = "#69C6AD",
            opacity = 0.2,
            line = list(width = 0),
            layer = "below",
            # Tooltip Text for the shaded area
            name = "Official Pandemic Period in Germany"
          )
        ),
        hoverlabel = list(
          bgcolor = "white",
          font = list(color = "black")
        )
      )
    
    return(p)
  })
  
  ##########
  observeEvent(input$invert_colors, {
    runjs("
    document.body.classList.toggle('inverted');
  ")
  })
  
  ##############
  observeEvent(input$showData, {
    showModal(
      modalDialog(
        title = "Dataset Viewer",
        size = "l",  # Large modal size
        DT::dataTableOutput("dataTable"),  # Display the data table
        easyClose = TRUE,  # Allows the user to close the modal by clicking outside or pressing Esc
        footer = tags$a(
          id = "modal-dataset-link",
          href = "https://cognovo.net/statistics/mortality-data-destatis/dataset-tidy1.csv",
          target = "_blank",  # Ensures it opens in a new tab
          class = "btn btn-primary",
          "Download Dataset as CSV"
        )
      )
    )
  })
  
  
  ###########

  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("dataset-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  ############
  observe({
    runjs("
    // Set target=_blank for all download links and dataset links
    $('a.download-csv').attr('target', '_blank');
  ")
  })
  
  
  ##################
  ##################
  observeEvent(input$analyze, {
    # Show preloader
    session$sendCustomMessage(type = 'showLoading', message = list(show = TRUE))
    
    req(input$yearRange)
    data_filtered <- subset(data, format(date, "%Y") >= input$yearRange[1] & format(date, "%Y") <= input$yearRange[2])
    data_filtered <- data_filtered[!duplicated(data_filtered$date), ]
    mort_ts <- zoo(data_filtered$mortality.rate, order.by = data_filtered$date)
    
    # Ensure no missing values in the time series
    mort_ts <- na.approx(mort_ts)
    
    # Perform stationarity tests
    adf_test <- adf.test(mort_ts, alternative = "stationary")
    kpss_test <- kpss.test(mort_ts, null = "Level")
    pp_test <- pp.test(mort_ts)
    
    # Render stationarity test results
    output$stationarityTests <- renderPrint({
      cat("### Stationarity Tests for Selected Time Period ###\n\n")

      print(adf_test)#Augmented Dickey-Fuller Test
      print(kpss_test)#KPSS Test for Level Stationarity
      print(pp_test)#Phillips-Perron Unit Root Test
    })
    
    # Breakpoint model
    bp_model <- breakpoints(mort_ts ~ 1)
    breakpoints_info <- breakpoints(bp_model)
    breakpoints_dates <- index(mort_ts)[breakpoints_info$breakpoints]
    
    
    # Calculate BIC for all models
    log_likelihood <- logLik(bp_model)
    bic <- BIC(bp_model)
    
    # Identify the best model based on the lowest BIC
    best_model_index <- which.min(bic)
    best_model_bic <- bic[best_model_index]
    best_breakpoints <- bp_model$breakpoints[[best_model_index]]
    best_model_dates <- index(mort_ts)[best_breakpoints]
    

    
    # Confidence intervals for breakpoints
    ci <- confint(bp_model)
    
    # Calculate widths in terms of indices
    ci_widths <- ci$confint[, 2] - ci$confint[, 1]
    
    # Convert indices (Lower, Upper) to corresponding dates
    ci_dates <- data.frame(
      Start = as.Date(breakpoints_dates),
      LowerDate = as.Date(index(mort_ts)[ci$confint[, 1]]),
      UpperDate = as.Date(index(mort_ts)[ci$confint[, 2]]),
      WidthWeeks = as.numeric(ci$confint[, 2] - ci$confint[, 1]) # Convert width to weeks
    )
    
    # Create final dataframe for display
    ci_df <- data.frame(
      Start = ci_dates$Start,
      Lower = ci_dates$LowerDate,
      Upper = ci_dates$UpperDate,
      WidthWeeks = ci_dates$WidthWeeks
    )
    

    # Chow Test for Each Breakpoint
    chow_results <- sapply(breakpoints_info$breakpoints, function(bp) {
      tryCatch({
        chow_test <- sctest(mort_ts ~ 1, type = "Chow", point = bp)
        p_value <- chow_test$p.value
        return(p_value)
      }, error = function(e) {
        return(NA) # Handle cases where the test cannot be performed
      })
    })
    
    # Residual diagnostics
    residuals <- residuals(bp_model)
    dw_test <- dwtest(mort_ts ~ 1)
    shapiro_test <- shapiro.test(residuals)
    
    # Safely calculate ANOVA and Eta-Squared
    tryCatch({
      segments <- cut(1:length(mort_ts), c(1, breakpoints_info$breakpoints, length(mort_ts)), include.lowest = TRUE)
      anova_result <- summary(aov(mort_ts ~ segments))
      sum_sq_between <- anova_result[[1]]["segments", "Sum Sq"]
      sum_sq_residual <- anova_result[[1]]["Residuals", "Sum Sq"]
      eta_squared <- sum_sq_between / (sum_sq_between + sum_sq_residual)
    }, error = function(e) {
      cat("Error during ANOVA or Eta-Squared calculation:", e$message, "\n")
      sum_sq_between <- NA
      sum_sq_residual <- NA
      eta_squared <- NA
    })
    output$anovaResults <- renderPrint({
      cat("### ANOVA Results ###\n")
      
      if (!is.null(anova_result)) {
        print(anova_result)  # Print the full ANOVA summary object
      } else {
        cat("ANOVA results could not be calculated.\n")
      }
    })
    
    output$etaResults <- renderPrint({
      cat("### Eta-Squared Results ###\n")
      
      if (!is.null(eta_squared)) {
        cat("Eta-Squared (Effect Size):", round(eta_squared, 3), "\n")
      } else {
        cat("Eta-Squared could not be calculated.\n")
      }
    })
    
    
    # Segment means and other statistics
    segment_starts <- c(1, breakpoints_info$breakpoints + 1)
    segment_ends <- c(breakpoints_info$breakpoints, length(mort_ts))
    segment_means <- sapply(1:length(segment_starts), function(i) mean(mort_ts[segment_starts[i]:segment_ends[i]]))
    magnitude_changes <- c(NA, diff(segment_means))
    
    # Create summary table
    summary_df <- data.frame(
      Start = as.Date(index(mort_ts)[segment_starts]),
      End = as.Date(index(mort_ts)[segment_ends]),
      Mean = segment_means,
      Change = magnitude_changes,
      CI_Width = c(NA, ci_widths),
      Chow_p_value = c(NA, chow_results)
    )
    
    output$summaryTable <- renderDataTable({
      datatable(
        summary_df %>%
          mutate(
            Mean = sprintf('<span title="Mean value of mortality rate for the segment">%s</span>', Mean),
            Change = sprintf('<span title="Change in mortality rate between segments">%s</span>', Change),
            CI_Width = sprintf('<span title="Confidence interval width for the segment">%s</span>', CI_Width),
            Chow_p_value = sprintf('<span title="P-value from Chow Test for structural break">%s</span>', Chow_p_value)
          ),
        options = list(
          pageLength = 5,   # Show 5 rows per page
          dom = 'tp',       # Remove search and tabbing
          ordering = FALSE  # Disable column ordering
        ),
        rownames = FALSE, 
        escape = FALSE  # Allow HTML for tooltips
      )
    })
    
    ##########
    # Score-CUSUM Plot: Detecting Abrupt Structural Breaks
    output$scoreCusumPlot <- renderPlot({
      efp_model_score <- efp(mort_ts ~ 1, type = "Score-CUSUM")
      cusum_test <- sctest(efp_model_score)
      plot(
        efp_model_score,
        main = paste0("Score-CUSUM Plot: Detecting Abrupt Structural Breaks\n",
                      "Statistic: ", round(cusum_test$statistic, 3),
                      ", p-value: ", format.pval(cusum_test$p.value, digits = 3)),
        ylab = "Score-CUSUM Process",
        xlab = "Index"
      )
    })
    
    # OLS-Based CUSUM Plot: Identifying Structural Instabilities
    output$cusumPlot <- renderPlot({
      efp_model <- efp(mort_ts ~ 1, type = "OLS-CUSUM")
      cusum_test <- sctest(efp_model)
      plot(
        efp_model,
        main = paste0("OLS-Based CUSUM Plot: Identifying Structural Instabilities\n",
                      "Statistic: ", round(cusum_test$statistic, 3),
                      ", p-value: ", format.pval(cusum_test$p.value, digits = 3)),
        ylab = "OLS-CUSUM Process",
        xlab = "Index"
      )
    })
    
    # Rec-CUSUM Plot: Sudden Parameter Instabilities
    output$recCusumPlot <- renderPlot({
      efp_model_rec <- efp(mort_ts ~ 1, type = "Rec-CUSUM")
      cusum_test <- sctest(efp_model_rec)
      plot(
        efp_model_rec,
        main = paste0("Rec-CUSUM Plot: Sudden Parameter Instabilities\n",
                      "Statistic: ", round(cusum_test$statistic, 3),
                      ", p-value: ", format.pval(cusum_test$p.value, digits = 3)),
        ylab = "Rec-CUSUM Process",
        xlab = "Index"
      )
    })
    
    # Rec-MOSUM Plot: Detecting Gradual Structural Changes
    output$recMosumPlot <- renderPlot({
      efp_model_mosum <- efp(mort_ts ~ 1, type = "Rec-MOSUM")
      cusum_test <- sctest(efp_model_mosum)
      plot(
        efp_model_mosum,
        main = paste0("Rec-MOSUM Plot: Detecting Gradual Structural Changes\n",
                      "Statistic: ", round(cusum_test$statistic, 3),
                      ", p-value: ", format.pval(cusum_test$p.value, digits = 3)),
        ylab = "Rec-MOSUM Process",
        xlab = "Index"
      )
    })
  
    
    
    # Full CUSUM Model Results and Statistics
    output$cusumStats <- renderPrint({
      efp_model <- efp(mort_ts ~ 1, type = "OLS-CUSUM")
      cat("### CUSUM Model Object ###\n")
      print(efp_model)
      
      # Perform statistical test on the CUSUM process
      cat("\n### Test Statistics ###\n")
      cusum_test <- sctest(efp_model)
      print(cusum_test)
    })
    
########
    
    # Add this to count the number of breakpoint configurations
    output$metricsAIC <- renderPrint({
      cat("Model Metrics:\n")
      cat(paste("Log-Likelihood:", log_likelihood, "\n"))
      cat("AIC for Each Model:\n")
      aics <- AIC(bp_model, k = 0:max(length(breakpoints_info$RSS)))  # Calculate AIC for all models
      for (i in seq_along(aics)) {
        cat(paste(" Model with", i - 1, "breakpoints:", aics[i], "\n"))
      }
      cat(paste("AIC of Selected Model:", AIC(bp_model), "\n"))
      cat(paste("BIC:", bic, "\n"))
      cat(paste("Number of Models Compared:", length(aics), "\n"))
    })
    ###########
    # Render all results
    output$bpResults <- renderPrint({
      cat("### Bai-Perron Analysis Results ###\n")
      
      # Breakpoints
      cat("\nBreakpoints Detected:\n")
      print(breakpoints(bp_model))
      
      # Confidence Intervals
      cat("\nConfidence Intervals for Breakpoints:\n")
      print(confint(bp_model))
      
      # Residual Sum of Squares
      cat("\nResidual Sum of Squares (RSS):\n")
      print(bp_model$RSS)
      
      # Log-Likelihood
      cat("\nLog-Likelihood for Models:\n")
      print(logLik(bp_model))
      
      # AIC and BIC
      cat("\nAIC for Models:\n")
      print(AIC(bp_model))
      cat("\nBIC for Models:\n")
      print(BIC(bp_model))
      
      # Full Summary
      cat("\nModel Summary:\n")
      print(summary(bp_model))
    })
    ################
    
    output$residualDiagnostics <- renderPrint({
      cat("Residual Diagnostics:\n")
      cat("Durbin-Watson Test p-value:", dw_test$p.value, "\n")
      cat("Shapiro-Wilk Test p-value (Normality):", shapiro_test$p.value, "\n")
      cat("Breusch-Pagan Test p-value (Homoscedasticity):", bp_test$p.value, "\n")
    })
    
    output$chowTestResults <- renderPrint({
      cat("Chow Test Results for Each Breakpoint:\n")
      print(data.frame(Breakpoint = as.Date(breakpoints_dates), P_Value = chow_results))
    })
    output$breakpointPlotInteractive <- renderPlotly({
      gg <- ggplot(data_filtered, aes(x = date, y = mortality.rate)) +
        geom_line(size = 0.5) +  # Main line with default styling
        geom_vline(
          xintercept = as.numeric(breakpoints_dates), 
          linetype = "dashed", 
          size = 0.9,  # Slightly thicker for emphasis
          alpha = 0.85,
          color = "#69C6AD"  # Use accent color to maintain salience
        ) +
        geom_segment(
          data = summary_df, 
          aes(x = Start, xend = End, y = Mean, yend = Mean), 
          linewidth = 1.0,  # Thicker for visibility
          alpha = 0.5,
          color = "red"  # Use a contrasting color for segment means
        ) +
        labs(
          title = " Bai-Perron structural breakpoint analysis", 
          x = "Date", 
          y = "Mortality Rate"
        ) +
        scale_x_date(
          date_breaks = "3 months",  # Adjust frequency (e.g., monthly, quarterly)
          date_labels = "%b %Y"      # Format as "Month Year"
        ) +
        theme_minimal()  # Use thematic global styling
      
      # Convert ggplot to Plotly and adjust Plotly layout
      ggplotly(gg) %>%
        layout(
          title = list(
            text = "<b>Mortality Rate with Breakpoints (Bai-Perron Algorithm)</b>", 
            x = 0.5
          ),
          xaxis = list(
            title = "Date",
            tickformat = "%b %Y",  # Format as "Month Year" in Plotly
            tickangle = -45        # Rotate for better readability
          ),
          yaxis = list(title = "Mortality Rate")
        )
    })
    
    ########
    output$analysis_content <- renderUI({
      # Include the external HTML file
      includeHTML("https://cognovo.net/statistics/mortality-data-destatis/bai-perron-analysis.html")
    })
    
    ########
    
    verbatimTextOutput("explanation") # Link to the server output
    output$references <- renderPrint({
      # Load ASCII art from a text file
      ascii_art <- tryCatch({
        readLines("https://cognovo.net/statistics/mortality-data-destatis/ascii1.txt")  # Replace with the path to your text file
      }, error = function(e) {
        return("Error loading ASCII art.")
      })
      
      # Print the ASCII art
      cat(paste(ascii_art, collapse = "\n"), "\n")
      
    
    })
    
    
    
    ############
    output$explanationChow <- renderUI({
      tags$div(
        withMathJax(HTML("
      <h4>Explanation of the Chow Test</h4>
      <p>The Chow Test determines whether a significant structural break exists at a specific point in a dataset by comparing regression coefficients before and after the breakpoint.</p>
      <p><strong>Hypotheses:</strong></p>
      <ul>
        <li>\\(H_0\\): Regression coefficients are the same before and after the breakpoint (no structural break).</li>
        <li>\\(H_1\\): Regression coefficients differ before and after the breakpoint (structural break exists).</li>
      </ul>
      <p><strong>Test Statistic:</strong></p>
      <p>The test statistic is calculated as:</p>
      \\[
      F = \\frac{(S_p - (S_1 + S_2)) / k}{(S_1 + S_2) / (n_1 + n_2 - 2k)}
      \\]
      <div>
        <strong>Where:</strong>
        <ul>
          <li>\\(S_p\\): Residual sum of squares for the pooled model (no break).</li>
          <li>\\(S_1\\): Residual sum of squares for the first segment (before the breakpoint).</li>
          <li>\\(S_2\\): Residual sum of squares for the second segment (after the breakpoint).</li>
          <li>\\(k\\): Number of parameters in the regression model.</li>
          <li>\\(n_1, n_2\\): Sample sizes in the first and second segments.</li>
        </ul>
      </div>
      <p>The \\(F\\)-statistic follows an \\(F(k, n_1 + n_2 - 2k)\\) distribution under the null hypothesis.</p>
      <p><strong>Interpretation:</strong> A significant \\(p\\)-value rejects \\(H_0\\), indicating a structural break exists at the tested point.</p>
    "))
      )
    })
    output$explanationStats <- renderUI({
      withMathJax(HTML("
    <h4>Statistical Concepts in Structural Breakpoint Analysis</h4>
    <p>For details, see: Bai, J., & Perron, P. (1998). Estimating and Testing Linear Models with Multiple Structural Changes. <em>Econometrica</em>, 66(1), 47–78. DOI: <a href='https://doi.org/10.2307/2998540' target='_blank'>10.2307/2998540</a></p>
    <p>This section explains the basic statistical concepts and algorithms used in structural breakpoint analysis. The full analysis output is given below.</p>
    <hr>
    
    <ol>
      <li>
        <strong>Residual Sum of Squares (RSS):</strong>
        <p>The Bai-Perron algorithm minimizes RSS to identify structural breakpoints:</p>
        \\[
        \\text{RSS} = \\sum_{j=1}^{m+1} \\sum_{t=T_{j-1}+1}^{T_j} (y_t - X_t \\beta_j)^2
        \\]
        <div>
          <strong>Steps:</strong>
          <ul>
            <li><strong>Partition the Data:</strong> All possible segmentations are evaluated. For \\(m\\) breakpoints, there are \\(\\binom{n}{m}\\) partitions.</li>
            <li><strong>Optimization:</strong> Pseudo-dynamic programming avoids redundant calculations and ensures efficient RSS computation.</li>
          </ul>
        </div>
      </li>
      <hr>
      
      <li>
        <strong>Confidence Intervals for Breakpoints:</strong>
        <p>Breakpoints are estimated with confidence intervals:</p>
        \\[
        CI = [T_j - \\Delta, T_j + \\Delta]
        \\]
        <div>
          <strong>Where:</strong>
          <ul>
            <li>\\(T_j\\): Estimated breakpoint location.</li>
            <li>\\(\\Delta\\): Derived from the distribution of residuals and breakpoints.</li>
          </ul>
        </div>
      </li>
      <hr>
      
      <li>
        <strong>Sequential F-Testing:</strong>
        <p>Tests whether additional breakpoints improve the model fit:</p>
        \\[
        F = \\frac{(RSS_r - RSS_u) / k}{RSS_u / (n - m - k)}
        \\]
        <div>
          <strong>Where:</strong>
          <ul>
            <li>\\(RSS_r\\): RSS for the restricted model (fewer breakpoints).</li>
            <li>\\(RSS_u\\): RSS for the unrestricted model (more breakpoints).</li>
            <li>\\(k\\): Number of parameters.</li>
            <li>\\(m\\): Current number of breakpoints.</li>
            <li>\\(n\\): Number of observations.</li>
          </ul>
        </div>
        <p>If the \\(F\\)-statistic is significant, a breakpoint is added.</p>
      </li>
      <hr>
      
      <li>
        <strong>Model Selection Criteria:</strong>
        <p>The optimal number of breakpoints is selected using criteria like:</p>
        <ul>
          <li><strong>Bayesian Information Criterion (BIC):</strong> \\(\\text{BIC} = -2 \\cdot \\log(L) + k \\cdot \\log(n)\\)</li>
          <li><strong>Akaike Information Criterion (AIC):</strong> \\(\\text{AIC} = 2k - 2 \\cdot \\log(L)\\)</li>
        </ul>
      </li>
      <hr>
      
      <li>
        <strong>Eta-Squared (Effect Size):</strong>
        <p>Measures the proportion of variance explained by breakpoints:</p>
        \\[
        \\eta^2 = \\frac{SS_{\\text{between}}}{SS_{\\text{between}} + SS_{\\text{within}}}
        \\]
        <div>
          <strong>Where:</strong>
          <ul>
            <li>\\(SS_{\\text{between}}\\): Variance between segments.</li>
            <li>\\(SS_{\\text{within}}\\): Variance within segments.</li>
          </ul>
        </div>
      </li>
      <hr>
      
      <li>
        <strong>Pseudo-Dynamic Programming:</strong>
        <p>This approach optimizes the Bai-Perron algorithm:</p>
        <ul>
          <li><strong>Recursive Calculations:</strong> The algorithm avoids evaluating all partitions by dynamically reusing intermediate results.</li>
          <li><strong>Time Complexity:</strong> Reduced from \\(O(n^m)\\) to \\(O(n^2)\\) for \\(m\\) breakpoints.</li>
        </ul>
      </li>
    </ol>
    
    <h4>Algorithm Summary</h4>
    <p>The algorithm steps include:</p>
    <ul>
      <li>Initialize \\(m = 0\\) (no breakpoints) and compute \\(RSS_0\\).</li>
      <li>Iteratively:
        <ol>
          <li>Partition the data using dynamic programming.</li>
          <li>Calculate \\(RSS_m\\) for each partition.</li>
          <li>Perform sequential F-tests to compare models with \\(m\\) and \\(m+1\\) breakpoints.</li>
          <li>Add a breakpoint if the F-test is significant.</li>
        </ol>
      </li>
      <li>Select \\(m\\) based on BIC or AIC.</li>
      <li>Estimate segment-specific coefficients \\(\\beta_j\\) and confidence intervals for \\(T_j\\).</li>
    </ul>
  "))
    })
    
    
    output$explanationCUSUM2 <- renderUI({
  withMathJax(HTML("
    <h4>Statistical Concepts in CUSUM Analysis</h4>
    <p>The CUSUM (Cumulative Sum) method is a statistical technique used to detect changes in the mean of a process or time series over time. It is particularly sensitive to small and gradual shifts.</p>
    
    <ol>
      <li><strong>CUSUM Formula:</strong></li>
      <p>The CUSUM statistic is defined as:</p>
      \\[
      S_t = \\sum_{i=1}^{t} (X_i - \\bar{X})
      \\]
      where:
      <ul>
        <li>\\(S_t\\): The cumulative sum up to time \\(t\\).</li>
        <li>\\(X_i\\): The observed value at time \\(i\\).</li>
        <li>\\(\\bar{X}\\): The mean of the observations.</li>
      </ul>
      <hr>
      
      <li><strong>OLS-Based CUSUM:</strong></li>
      <p>The Ordinary Least Squares (OLS)-based CUSUM test is used to identify structural breaks by examining the stability of regression coefficients over time.</p>
      <p>The test statistic is based on the recursive residuals:</p>
      \\[
      W_t = \\frac{1}{\\sqrt{n}} \\sum_{i=1}^{t} e_i
      \\]
      where:
      <ul>
        <li>\\(W_t\\): CUSUM process at time \\(t\\).</li>
        <li>\\(e_i\\): Recursive residual at time \\(i\\).</li>
        <li>\\(n\\): Total number of observations.</li>
      </ul>
      <hr>
      
      <li><strong>Applications:</strong></li>
      <p>CUSUM analysis is widely used in detecting:</p>
      <ul>
        <li>Parameter instability in regression models.</li>
        <li>Gradual or sudden changes in the mean level of a process.</li>
        <li>Structural breaks in time series data.</li>
      </ul>
      <hr>
      
      <li><strong>Interpretation:</strong></li>
      <p>Significant deviations of \\(S_t\\) or \\(W_t\\) from zero indicate potential structural breaks. Thresholds for significance can be derived from the distribution of the test statistic under the null hypothesis of no structural break.</p>
    </ol>
  "))
})

    
    ##########

    
    
    output$chowResultsRaw <- renderPrint({
      cat("### Chow Test Results ###\n")
      
      if (exists("breakpoints_info")) {
        # Perform Chow Test for each breakpoint
        chow_results <- lapply(breakpoints_info$breakpoints, function(bp) {
          tryCatch({
            chow_test <- sctest(mort_ts ~ 1, type = "Chow", point = bp)
            list(
              Breakpoint = index(mort_ts)[bp],
              Test_Result = chow_test
            )
          }, error = function(e) {
            list(
              Breakpoint = NA,
              Test_Result = paste("Error:", e$message)
            )
          })
        })
        
        # Print raw results
        for (result in chow_results) {
          cat("\n--- Breakpoint at:", result$Breakpoint, "---\n")
          print(result$Test_Result)
        }
      } else {
        cat("No breakpoints detected for Chow Test.\n")
      }
    })
    
    ###################
    output$breakpointDetails <- renderPrint({
      cat("Breakpoints detected at:\n")
      print(as.Date(breakpoints_dates))
      cat("\nConfidence Intervals for Breakpoints:\n")
      print(ci_df)
    })
    
    ###########
    output$dataTable <- DT::renderDataTable({
      datatable(data, options = list(pageLength = 10, scrollX = TRUE))
    })
    ####################
    
    output$downloadSummary <- downloadHandler(
      filename = function() {
        paste("breakpoint_summary", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(summary_df, file, row.names = FALSE)
      }
    )
    
    session$sendCustomMessage(type = 'showLoading', message = list(show = FALSE))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
