#J Tyler Smith
# February 11, 2023
# This Shiny app plots a viral dynamical model, the TIV model,
# using slider inputs for the force of infection, viral production,
# duration of 

library(shiny)
library(deSolve)

tiv_equations <- function(time, variables, parameters){
  with(as.list(c(variables,parameters)), {
    dT <- lambda - beta * T * V - death * T
    dI <- beta * T * V - delta * I
    dV <- n * delta * I - c * V
    return(list(c(dT,dI,dV)))
  })
}


# Define UI for app that draws an TIV trajectory ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("TIV viral dynamical model"),
  
  #Allow the use of MathJax in the UI in all subsequent components
  withMathJax(),
  
  #This bit of script allows for in-line MathJax using a singe $ on each side of the text:
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),
    column(12,
           p("This app exists to provide some intuition for a simplified variant of the T-I-V system of equations inspired by ",
             tags$a(href="https://pubmed.ncbi.nlm.nih.gov/8599114/", "Alan Perelson et al. (1996)"),
             " in the study of HIV."),
           p("In this system of equations, the classes T, I, and V represent uninfected host target cells for the virus, infected host cells, and free virions, respectively. The system of equations is as follows:"),
           p("$$\\dot{T}= \\lambda - \\beta V T - d T$$"),
           p("$$\\dot{I}= \\beta V T - \\delta I$$"),
           p("$$\\dot{V}= n \\delta I - c V$$"),
           p("The idea here is that host cells have an intrinsic birth and death rate, but move to the infected class when encountering a free virion. Infected cells have a death rate equal to or greater than uninfected host cells, and produce a certain number of virions to be released upon death. These virions are cleared at a certain rate."),
           p("This explainer is still under construction")),
  
  #Row based layout: row 1 has the sliders and the main plot
     column(8,
            plotOutput(outputId = "TIVPlot")
     ), 
     column(4,
      # Input: Sliders for the various parameters ----
      sliderInput(inputId = "beta_param",
                  label = "Force of infection (\\beta)(*10^-7):",
                  min = 0.1,
                  max = 100,
                  value = 28),
      sliderInput(inputId = "delta_param",
                  label = "Infected cell death rate (\\delta):",
                  min = 0.1,
                  max = 10,
                  value = 1),
      sliderInput(inputId = "n_param",
                  label = "Burst size (n):",
                  min = 100,
                  max = 5000,
                  value = 1500),
      sliderInput(inputId = "c_param",
                  label = "Viral clearance rate (c):",
                  min = 10,
                  max = 100,
                  value = 23),
      sliderInput(inputId = "duration",
                  label = "Duration of simulation (days)",
                  min = 10,
                  max = 100,
                  value = 30)
    ),
    
    
    
fluidRow(
  column(6,
         plotOutput(outputId = "TotCellPlot")
  ),
  column(6,
         plotOutput(outputId = "PropInfected")
  )))
  


# Define server logic required to draw a trajectory ----
server <- function(input, output) {
  # Reactive() is used throughout to indicate values should be updated with sliders
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs change
  # 2. Its output type is a plot
  tcell_params <- c(1000, 0.01)
  
  parameters_values <- reactive(c(
    lambda = tcell_params[1],
    death = tcell_params[2],
    beta = input$beta_param*10^-7, # force of infection
    delta = input$delta_param,  # death rate of infected cells
    n = input$n_param, # burst size
    c = input$c_param #clearance rate
  ))
  
  
  initial_values <- c(
    T = tcell_params[1]/tcell_params[2],
    I = 0,
    V = 1
  )
  
  time_values <- reactive(seq(
    from = 0,
    to = input$duration,
    by = 0.01
  ))
  
  tiv_values_1 <- reactive(ode(
    y=initial_values,
    times = time_values(),
    func=tiv_equations,
    parms=parameters_values()
  ))
  
  tiv_values_2 <- reactive(as.data.frame(tiv_values_1()))
  
  output$TIVPlot <- renderPlot(
    with(tiv_values_2(),{
      maxT = max(T)
      maxI = max(I)
      maxV = max(V)
      
      maxObs = max(c(maxT,maxI,maxV))
      
      # plotting the time series of susceptibles:
      plot(time,
           log(T),
           type = "l",
           col = "blue",
           xlab = "Time (days)",
           ylab = "Log_10 Population",
           ylim = c(0.1,log(maxObs*1.2)),
           lwd = 2)
      # adding the time series of infecteds:
      lines(time,
            log(I),
            col = "green",
            lwd = 2)
      # adding the time series of recovered:
      lines(time,
            log(V),
            col = "red",
            lwd = 2)
      #adding a legend:
      legend("bottomright", c("Target Cells", "Infected Cells", "Free Virions"),
             col = c("blue", "green", "red"), lty = 1, lwd = 2, bty = "n")
    }), #Closes with command
    res=96
  ) #Closes renderPlot
  output$TotCellPlot <- renderPlot(
    with(tiv_values_2(),{
      TotCells = T+I
      
      maxObs = max(TotCells)
      
      # plotting the time series of total cells:
      plot(time,
           log(TotCells),
           type = "l",
           col = "blue",
           xlab = "Time (days)",
           ylab = "Log_10 Total Cell Count",
           ylim = c(0.1,log(maxObs*1.2)),
           lwd = 2)

    }), #Closes with command
    res=96
  ) #Closes renderPlot
  output$PropInfected <- renderPlot(
    with(tiv_values_2(),{
      PropInfCells <- I/(T+I)
      
      
      # plotting the time series of total cells:
      plot(time,
           PropInfCells,
           type = "l",
           col = "red",
           xlab = "Time (days)",
           ylab = "Proportion of cells infected",
           ylim = c(0,1),
           lwd = 2)
      
    }), #Closes with command
    res=96
  ) #Closes renderPlot

} #Closes shiny server commands
shinyApp(ui = ui, server = server)
