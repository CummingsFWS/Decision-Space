library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Extinction Risk Visualization"),
  sidebarLayout(
    sidebarPanel(
      selectInput("tab", "Select View:",
                  choices = c("Overview","1. Framework", "2. Risk Threshold", "3. Time Threshold", "4. Policy Thresholds")),
      
      # Show Risk Threshold slider only when relevant
      conditionalPanel(
        condition = "input.tab == '2. Risk Threshold' || input.tab == '4. Policy Thresholds'",
        sliderInput("risk", "Risk Threshold:", min = 0, max = 100, value = 25, step = 5)
      ),
      
      # Show Time Threshold slider only when relevant
      conditionalPanel(
        condition = "input.tab == '3. Time Threshold' || input.tab == '4. Policy Thresholds'",
        sliderInput("time", "Time Threshold:", min = 0, max = 100, value = 20, step = 5)
      )
    ),
    mainPanel(
      conditionalPanel(
        condition = "input.tab != 'Overview'",
        plotOutput("mainPlot", height = "600px", click = "plot_click")
      ),
      htmlOutput("plotDescription")
    )
  )
)

server <- function(input, output) {
  time <- seq.int(0, 100, 10)
  
  clicked_point <- reactiveVal(NULL)
  
  observeEvent(input$plot_click, {
    if (input$tab == "1. Framework") {
      clicked_point(tibble(x = input$plot_click$x, y = input$plot_click$y))
    }
  })
  
  output$mainPlot <- renderPlot({
    Risk_Threshold <- rep(if (!is.null(input$risk)) input$risk else 25, length(time))
    Time_Threshold <- if (!is.null(input$time)) input$time else 20
    
    if (input$tab == "Overview") {
      
    } else if (input$tab == "1. Framework") {
      cP_Read_Me <- seq.int(0, 100, 10)
      data_Read_Me <- tibble(time, cP_Read_Me)
      
      p<-ggplot(data_Read_Me, aes(time, cP_Read_Me)) +
        labs(x = 'Time', y = 'Cumulative Probability of Extinction') +
        scale_x_continuous(labels = c("Present", "Future"), breaks = c(0, 100), expand = c(0, 0)) +
        scale_y_continuous(breaks = seq.int(0, 100, 10), expand = c(0, 0)) +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = c(0, 1), vjust = 0.2),
              text = element_text(size = 20))
      
      if (!is.null(clicked_point())) {
        pt <- clicked_point()
        p <- p + 
          geom_point(data = pt, aes(x = x, y = y), color = "purple", size = 3) +
          geom_text(data = pt, aes(x = x, y = y, label = paste0("(year ", round(x, 0), ", risk ", round(y, 0), "%)")),
                    vjust = -1, color = "purple", size = 5)
      }
      
      return(p)
      
    } else if (input$tab == "2. Risk Threshold") {
      cP_Warranted <- seq.int(0, 100, 10)
      data_Warranted <- tibble(time, cP_Warranted, Risk_Threshold)
      
      ggplot(data_Warranted, aes(time, Risk_Threshold)) +
        geom_ribbon(aes(ymin = Risk_Threshold, ymax = Risk_Threshold + 100), fill = "red", alpha = 0.25) +
        geom_ribbon(aes(ymin = Risk_Threshold - 100, ymax = Risk_Threshold), fill = "green", alpha = 0.25) +
        geom_line(color = "red") +
        annotate("label", x = 50, y = mean(c(100, mean(Risk_Threshold))), label = "warranted", color = "red", size = 6) +
        annotate("label", x = 50, y = mean(c(0, mean(Risk_Threshold))), label = "not warranted", color = "green", size = 6) +
        labs(x = 'Time', y = 'Cumulative Probability of Extinction') +
        scale_x_continuous(labels = c("Present", "Future"), breaks = c(0, 100), expand = c(0, 0)) +
        scale_y_continuous(breaks = seq.int(0, 100, 10), expand = c(0, 0)) +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = c(0, 1), vjust = 0.2),
              text = element_text(size = 20))
      
    } else if (input$tab == "3. Time Threshold") {
      cP_Time <- seq.int(0, 100, 10)
      data_Time <- tibble(time, cP_Time, Time_Threshold)
      
      ggplot(data_Time) +
        geom_rect(aes(xmin = 0, xmax = Time_Threshold, ymin = 0, ymax = 100), fill = "orange", alpha = 0.075) +
        geom_rect(aes(xmin = Time_Threshold, xmax = 100, ymin = 0, ymax = 100), fill = "khaki1", alpha = 0.075) +
        geom_vline(xintercept = Time_Threshold) +
        annotate("label", x = Time_Threshold / 2, y = 50, label = "endangered", size = 6) +
        annotate("label", x = Time_Threshold + (100 - Time_Threshold) / 2, y = 50, label = "threatened", size = 6) +
        labs(x = 'Time', y = 'Cumulative Probability of Extinction') +
        scale_x_continuous(labels = c("Present", "Future"), breaks = c(0, 100), expand = c(0, 0)) +
        scale_y_continuous(breaks = seq.int(0, 100, 10), expand = c(0, 0)) +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = c(0, 1), vjust = 0.2),
              text = element_text(size = 20))
      
    } else if (input$tab == "4. Policy Thresholds") {
      cP_Time <- seq.int(0, 100, 10)
      data_Policy <- tibble(time, cP_Time, Time_Threshold, Risk_Threshold)
      
      ggplot(data_Policy) +
        geom_rect(aes(xmin = 0, xmax = Time_Threshold, ymin = 0, ymax = 100), fill = "orange", alpha = 0.075) +
        geom_rect(aes(xmin = Time_Threshold, xmax = 100, ymin = 0, ymax = 100), fill = "khaki1", alpha = 0.075) +
        geom_ribbon(aes(ymin = Risk_Threshold, ymax = Risk_Threshold + 100, x = time), fill = "red", alpha = 0.25) +
        geom_ribbon(aes(ymin = Risk_Threshold - 100, ymax = Risk_Threshold, x = time), fill = "green", alpha = 0.25) +
        geom_vline(xintercept = Time_Threshold) +
        geom_hline(aes(yintercept = Risk_Threshold), color = "red") +
        annotate("label", x = Time_Threshold / 2, y = 50, label = "endangered", size = 6) +
        annotate("label", x = Time_Threshold + (100 - Time_Threshold) / 2, y = 50, label = "threatened", size = 6) +
        annotate("label", x = Time_Threshold / 2, y = mean(c(0, mean(Risk_Threshold))), label = "not warranted", color = "green", size = 6) +
        annotate("label", x = Time_Threshold + (100 - Time_Threshold) / 2, y = mean(c(0, mean(Risk_Threshold))),
                 label = "not warranted", color = "green", size = 6) +
        labs(x = 'Time', y = 'Cumulative Probability of Extinction') +
        scale_x_continuous(labels = c("Present", "Future"), breaks = c(0, 100), expand = c(0, 0)) +
        scale_y_continuous(breaks = seq.int(0, 100, 10), expand = c(0, 0)) +
        coord_cartesian(xlim = c(0, 100), ylim = c(0, 100)) +
        theme_bw() +
        theme(axis.text.x = element_text(hjust = c(0, 1), vjust = 0.2),
              text = element_text(size = 20))
    }
  })
  
  output$plotDescription <- renderUI({
    if (input$tab == "Overview") {
      tagList(
        p(strong("Welcome to the Species Risk Visulaization Tool.")),
        p("This application provides visual tools to explore implementation of the Endangered Species Act (ESA). Use the alternate views and adjust the thresholds in the conceptual models to see how different components of these decisions relate to and influence a hypothetical Endangered Species Act (ESA) classification."),
        p("The key components of the visualization that can be adjusted include the information, e.g., species status assessment results and values, e.g., policy thresholds used to make an ESA classification. The visuals display a conceptual framework for how scientific assessments of species status and policy definitions combine, and how their combination provides clarity regarding what reasonable ESA determinations can be made for a given combination."),
        p("Proceed to the '1. Framework ' view for a description of the conceptual framework for these visualizations.")
      )
    } else if (input$tab == "1. Framework"){
      tagList(
        p(strong("Decision Space Overview.")),
        p("This plot shows the basic visualization framework of extinction risk over time. The axes shown here are present in all of the plots that follow. The horizontal axis displays time, from today (Present) to a date many years from now (Future). The vertical axis displays the degree of risk that the species will be extinct."),
        p("Click on the figure to better understand what how an extinction risk data point would be plotted on this figure. This will bring up a data point, and the time and extinction probability (risk) at that point. For example, if you clicked at year 25 and risk 20% that would indicate there is a 20% chance the species will go extinct at some point up to 20 years from now."),
        p("Proceed to the '2. Risk Threshold' view for a discription of risk and it's relevance to an ESA decision.")
      )
    } else if (input$tab == "2. Risk Threshold"){
      tagList(
        p(strong("Risk Tolerance")),
        p("This plot illustrates the risk threshold used to determine if extinction risk is warranted."),
        p(strong("What is Species Risk?")),
        p("Let's start by reviewing what risk is. Risk is defined by a) the consequences (e.g., magnitude, severity, impact) of an event, and b) the probability (e.g., likelihood, odds, chance) of an event. For the ESA risk is defined as a species being 'in danger of extinction'. The consequence here is extinction, and the probability here is 'in danger of'."),
        img(src='ESA and SSA flow chart.png', height = "66%", width = "66%"),
        p("The text of the ESA does not provide a more precise definition of 'in danger of'. Therefore, even if a decision maker were provided with a an exact estimate of the probability of extinction from a species status assessment (SSA, the scientific evaluations of species viability produced by the US Fish and Wildlife Service) they would still need to determine whether that probability is enough to consider the species 'in danger of'. that is, decision makers need to consider the appropriate risk tolerance to set a risk tolerance threshold."),
        p("Adjust the Risk Threshold slider to see how a change in risk tolerance alters the area in the plot where a species would be consider warranted or not warranted for listing. When you feel you have explored the implications of risk tolerance via setting a risk threshold for 'in danger' proceed to the '3. Time threshold' view.")
    )
    } else {
      HTML(switch(input$tab,
                  "3. Time Threshold" = "<p>This plot highlights the time-based threshold used to classify species as endangered or threatened.<p>",
                  "4. Policy Thresholds" = "<p>This plot combines both risk and time thresholds to support policy decisions regarding species status.<p>"
                  #         p("Conservation biology provides us tools like population viability analysis that produce the probability of extinction is the means to assess probability for species risk management. For ESA implementation this probability is assessed via a species status assessment (SSA). Determining this probability depends upon when in the future the probability of extinction is being assessed, i.e., the time of the assessment. Therefore, the relevant dimensions of a figure exploring the relationship between risk and ESA classification decisions are the time (the horizontal or x axis) and the probability (the vertical or y axis) of extinction. (See Tab 2. Species risk curves)
                  # 
                  # With the scientific assessment of available information from an SSA in hand the policy task is then to determine if the assessed risk meets a policy based risk tolerance level. (See Tab 3. Risk Tolerance)")
      ))
    }
  })
  
}

shinyApp(ui = ui, server = server)