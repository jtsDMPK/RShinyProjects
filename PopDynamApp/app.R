# J Tyler Smith
# Oct 16, 2025
# This Shiny app walks the user through a series of standard population models, including ODE strategies, discrete strategies, and DDEs.

library(shiny)
library(deSolve)

# Section: ODEs
# The differential equations used in this program
# Since these are called within later parts of the program for generating graphs, the
# differential equations used throughout are stored at the top.

# The general structure of the ODE model calls under deSolve is as follows:
# Output <- function(time, vars, params){
#   with(as.list(c(vars,params)),{
#     dVar1 <- [eq 1, with vars and params]
#     dVar2 <- [eq 2, with vars and params]
#     return(list(c(dVar1,dVar2)))
#   })
# }

# They take as inputs the vector of time points, variables for the model equations (vars),
# and the parameters for the model equations (vars), and return a list of values for the
# variables as the Output.




# Simple Continuous Growth model
# This is the set of governing equation for a simple continuous growth model
# with parameters for birth and death.
# Inputs: time      - a vector of timesteps for which to run the model
#         variables - the population size, N
#         parameters- the set of parameters, b (birth rate) and d (death rate)
# Outputs: a list of difference steps for running the model
simple_cont_growth <- function(time,variables,parameters){
  with(as.list(c(variables,parameters)), {
    dN <- b*N - d*N
    return(list(c(N)))
  })
}


# Section: UI
# Here, fluidPage is used to generate the user interface with column/row panels.
# The general structure starts with a title panel, and from there features columns
# with width up to 12. The call looks like:
# column([width],
#   [html elements separated by commas, such as p("...") for paragraphs]
# )
# Since page width is 12, calls for width 8 and 4 or 6 and 6 can fit on the same row.
# 
ui <- fluidPage(
  
  #App title
  titlePanel("Population Dynamics for Single Species"),
  
  #Allow the use of MathJax in the UI in all subsequent components
  withMathJax(),
  
  #This bit of script allows for in-line MathJax using a singe $ on each side of the text:
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),
  
  #Section: Header
  #This is the first row section below the title page, and gives an introduction to the app  
  column(12,
         h1("Introduction"),
         p("In modeling for ecology or other fields like epidemiology that include demographic dynamics, a standard component of the larger modeling strategy is to include a model specifically for the population of the single species under study. In ecology, sometimes single species models are useful objects of their own study; in other cases, they are a building block to a larger model like a predator/prey model. In epidemiology, when studying disease transmission over longer timescales, demographic dynamics become relevant and population components are incorporated intgo the larger modeling strategy."),
         p("At its core, the most basic building blocks of a population model are birth, death, and migration, with more complex terms being variations of these, such as predation being an additional death mechanism. Below are several variations on this theme, when they are used, and some properties of the models under study."),
  ),
  
  #Section: Continuous Growth
  column(12,
         h1("Simple Continuous Growth"),
         p("One of the simplest models one can consider in this field is a continuous growth ordinary differential equation. Suppose we have a population, described by a function over time, $N(t)$, such that it has a constant birth rate $b$ and death rate $d$, both positive. If we choose to model this as a continuous differential equation, we have a rate of change of the population described by"),
         p("$$\\frac{dN(t)}{dt} = bN(t) - dN(t)$$"),
         p("It is worth noting that to be a properly specified differential equation will also have some initial condition at the zero timepoint, or $N(0)=N_{0}$, where $N_{0}$ indicates the fixed initial value."),
         p("It is also standard convention within the context of differential equations to drop the 'function of t' notation, $(t)$, and abbreviate the function $N(t)$ as $N$ when used within a differential expression."),
         p("Now suppose we have a population described by this model, with time in arbitrary units. A plot of the resulting function is below, with the ability to toggle birth and death rates between 0 and 10, and the initial population anywhere from 1 to 100."),
  ),
  
  column(8,
         plotOutput(outputId = "simple_cont_plot")
         ),
  column(4,
         sliderInput(inputId = "simple_cont_birth",
                     label = "Birth rate",
                     min=0,
                     max=10,
                     value=5),
         sliderInput(inputId = "simple_cont_death",
                     label = "Death rate",
                     min=0,
                     max=10,
                     value=4),
         sliderInput(inputId = "simple_cont_initial",
                     label = "Initial population",
                     min=1,
                     max=100,
                     value=25)
         ),
)
  



# Define server logic required to draw a trajectory ----
server <- function(input, output) {
  # Reactive() is used throughout to indicate values should be updated with user input
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs change
  # 2. Its output type is a plot
  
  
  simple_cont_parameters <- reactive(c(
    b = input$simple_cont_birth,
    d = input$simple_cont_death,
  ))
  
  simple_cont_initial_popn <- reactive(c(
    y = input$simple_cont_initial
  ))
  
  simple_cont_time_values <- seq(
    from = 0,
    to = 20,
    by = 0.01
  )
  
  simple_cont_values <- reactive(ode(
    y = simple_cont_initial_popn(),
    times = simple_cont_time_values,
    func = simple_cont_growth,
    parms = simple_cont_parameters()
  ))
  
  simple_cont_values_df <- reactive(as.data.frame(simple_cont_values()))
  
  output$simple_cont_plot <- renderPlot(
    with(simple_cont_values_df(),{
      max_N = max(N)
      plot(time,
           N,
           type = "l",
           col = "blue",
           xlab = "Time",
           ylab = "Population",
           ylim = c(0.1,maxObs*1.2),
           lwd = 2)
    }), #Closes with command
    res=96
  ) #Closes renderPlot

  
} #Closes shiny server commands
shinyApp(ui = ui, server = server)