# J Tyler Smith
# June 8, 2024

# HepaticClearance/app.R

# This Shiny app compares two popular models of hepatic clearance:
# the Well Stirred Model (WSM) and the Parallel Tube Model (PTM).
# The user is presented with a short description of the functional form
# of each model alongside a series of inputs for flow rate, unbound fraction,
# and intrinsic clearance. From this, an estimate of the total hepatic clearance
# estimated by each model is presented in the text; and further, a series of graphs
# showing the effect of intrinsic clearance and unbound fraction (keeping other
# variables fixed) are presented for illustrative purposes, with accompanying
# text in adjacent columns so that users can see how the functional forms of
# the respective models behave under different conditions.

library(shiny)

# First, define the functional forms for the two models, which will be called extensively
# through the app.

# Function for CL_h (total hepatic clearance) using the well-stirred model

# This function returns a numeric value, WSM_hepatic_clearance
# (described in text as CL_H,WSM) by taking as inputs the hepatic blood flow,
# unbound fraction in blood, and the intrinsic hepatic clearance.

WSM_hepatic_clearance <- function(hepatic_blood_flow, unbound_frac_blood, intrinsic_clearance){
  (hepatic_blood_flow*unbound_frac_blood*intrinsic_clearance)/(hepatic_blood_flow + unbound_frac_blood*intrinsic_clearance)
}

# Function for CL_h (total hepatic clearance) using the parallel tube model

# This function returns a numeric value, PTM_hepatic_clearance
# (described in text as CL_H,PTM) by taking as inputs the hepatic blood flow,
# unbound fraction in blood, and the intrinsic hepatic clearance.

PTM_hepatic_clearance <- function(hepatic_blood_flow, unbound_frac_blood, intrinsic_clearance){
  hepatic_blood_flow*(1-exp((-unbound_frac_blood*intrinsic_clearance)/hepatic_blood_flow))
}


# Define UI for the app

ui <- fluidPage(
  #Allow the use of MathJax in the UI in all subsequent components
  withMathJax(),
  
  #This bit of script allows for in-line MathJax using a singe $ on each side of the text:
  tags$div(HTML("<script type='text/x-mathjax-config'>
                MathJax.Hub.Config({
                tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
                });
                </script>
                ")),
  
  # App title ----
  titlePanel("Hepatic Clearance Models"),
  
  # Row based layout: 
  # The first row has the initial description with links, and input for
  # parameters in two columns. Subsequent rows have plots for the models
  # varying a single parameter, with a blurb pointing out properties of
  # the graph in the adjacent column. In those subsequent rows, layouts are
  # alternated (graph left, blurb right; blurb left, plot right) to provide
  # some visual interest as the user scrolls down the page.
  
  # Row 1, column 1:
  # This column has the opening text and description of the models
  # with links. In the text, the WSM and PTM functions are called
  # and the model predicted hepatic clearances are provided in text.
  column(8,
         tags$div("This app provides some visualizations to understand two popular models of hepatic clearance, inspired by Keith Hornberger's pharmacology tweetorial",
                  tags$a(href="http://twitter.com/KRHornberger/status/1459141064187645954", "here.")),
          p("These are the well-stirred model and parallel tube model, and they both serve to provide a link between in vivo and in vitro drug metabolism studies."),
          p("Both models seek to describe the total hepatic clearance of drug in an organism, $CL_H$, by three relevant parameters: the fraction of drug unbound, and thus available, in the blood, $f_{u,b}$, the rate of blood flow to the liver, $Q_H$, and the intrinsic clearance ability of hepatic cells to metabolize drug available to them, $CL_{int}$, as determined from in vitro assays. In both cases, the total hepatic clearance is estimated in ml/min/kg body weight."),
          tags$div("The form of this relation under the well-stirred model, derived and discussed in ",
                  tags$a(href="https://link.springer.com/article/10.1007/BF01059688", "Pang & Rowland, 1977,"), "is as follows:"),
          p("$$CL_{H,WSM} = \\frac{Q_H \\cdot CL_{int} \\cdot f_{ub}}{Q_H + CL_{int} \\cdot f_{ub}}$$"),
          textOutput("WSM_CL_H_text"),
          p("On the other hand, the parallel tube model, discussed in comparison in",
                  tags$a(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC9639621/", "Jusko & Li, 2021,"), "has the following functional form:"),
          p("$$CL_{H,PTM} = Q_H \\cdot (1 - e^\\frac{-CL_{int}\\cdot f_{ub}}{Q_H} )$$"),
          textOutput("PTM_CL_H_text"),
          p("You may also wish to see how more generally how these in vivo estimates vary with $CL_{int}$ and $f_{u,b}$ as curves. Those results are below.")
  ), 
  
  # Row 1, column 2
  # This column has the input values for the models.
  # Flow rate (variable: anim; Q_H in text) has radio buttons to choose between mouse or human values
  # Intrinsic clearance (CL_int) has a numeric input box, allowing inputs between 0.1 and 10000
  # Unbound fraction (f_ub) has a slider allowing values from 0.001 to 1
  
  column(4,
         radioButtons("anim","Flow rate model for $Q_H$:",
                      c("Human - 30 mL/min/kg" = "hum",
                        "Mouse - 90 mL/min/kg" = "mus")),
         numericInput(inputId = "CL_int",
                     label = "Intrinsic Hepatic Clearance, $CL_{int}$, in mL/min/kg",
                     value = 100, min=0.1, max=10000),
         sliderInput(inputId = "f_ub",
                     label = "Fraction of drug unbound in blood, $f_{u,b}$",
                     min = 0.001,
                     max = 1,
                     value = 0.10)
  ),
  
  # Rows 2 and 3 follow the well stirred model (WSM)
  # From here on, the fluidRow() function is called to control output 
  
  # Row 2
  # Column 1 (width 8) plots total hepatic clearance as a function of intrinsic
  # clearance under the well-stirred model, keeping hepatic flow rate and
  # unbound fraction constant, calling the WSM_CL_H_vary_CL_int plot.
  # Column 2 (width 4) describes a short descriptive blurb of this plot.
    
   fluidRow(
     column(8,
            plotOutput(outputId = "WSM_CL_H_vary_CL_int")
     ),
     column(4,
            h4("$CL_{H,WSM}$ as a function of $CL_{int}$"),
            p("Here, $CL_{int}$ has been allowed to vary with the selected value of $f_{u,b}$ fixed. Notice that $CL_{int}$ values on the x-axis are presented on a logarithmic scale given the wide range of intrinsic clearances seen in practice."),
            p("You might notice that the curve seems to top out at $Q_H$. This is the case when hepatic clearance is flow-rate limited. You may notice that varying $f_{u,b}$ above will shift the inflection point. In general, the more available the drug, the sooner the total hepatic clearance becomes flow-rate limited as a function of intrinsic clearance.")
     )
  ), #Close fluidRow
  
 
  # Row 3
  # Column 2 (width 8) plots total hepatic clearance as a function of unbound
  # fraction under the well-stirred model, keeping hepatic flow rate and
  # intrinsic clearance constant, calling the WSM_CL_H_vary_f_ub plot.
  # Column 1 (width 4) provides a short descriptive blurb of this plot.
  
  fluidRow(
    column(4,
           h4("$CL_{H,WSM}$ as a function of $f_{ub}$"),
           p("In this plot, $f_{u,b}$ has been allowed to vary with the selected value of $CL_{int}$ fixed. Because the fraction of drug unbound in the blood can only range from 0 to 1, the plot is shown here as linear in $f_{u,b}$."),
           p("You might notice that the curve seems to top out at the same value as the previous one, again indicating the flow-limited case. It's also worth noting that as $f_{u,b}$ approaches 0, so does hepatic clearance - and this is reasonable, for if no drug is available to the liver, it has nothing to metabolize.")
    ),
    
    column(8,
           plotOutput(outputId = "WSM_CL_H_vary_f_ub")
    )
  ), #Close fluidRow
    
  # Rows 4 and 5 follow the parallel tube model (PTM)
  
  # Row 4
  # Column 1 (width 8) plots total hepatic clearance as a function of intrinsic
  # clearance under the parallel tube model, keeping hepatic flow rate and
  # unbound fraction constant, calling the PTM_CL_H_vary_CL_int plot.
  # Column 2 (width 4) describes a short descriptive blurb of this plot.
  
    fluidRow(
      column(8,
             plotOutput(outputId = "PTM_CL_H_vary_CL_int")
      ),
      column(4,
             h4("$CL_{H,PTM}$ as a function of $CL_{int}$"),
             p("Here, $CL_{int}$ has been allowed to vary with the selected value of $f_{u,b}$ fixed. Notice that $CL_{int}$ values on the x-axis are presented on a logarithmic scale given the wide range of intrinsic clearances seen in practice."),
             p("The curve again hits an asymptote at $Q_H$, but the inflection as it approaches is much more sharp.")
      )
    ), #Close fluidRow
  
  # Row 5
  # Column 2 (width 8) plots total hepatic clearance as a function of unbound
  # fraction under the parallel tube model, keeping hepatic flow rate and
  # intrinsic clearance constant, calling the PTM_CL_H_vary_f_ub plot.
  # Column 1 (width 4) provides a short descriptive blurb of this plot.
  
    fluidRow(
      column(4,
             h4("$CL_{H,PTM}$ as a function of $f_{ub}$"),
             p("In this plot, $f_{u,b}$ has been allowed to vary with the selected value of $CL_{int}$ fixed. Because the fraction of drug unbound in the blood can only range from 0 to 1, the plot is shown here as linear in $f_{u,b}$."),
             p("You might notice that the curve seems to top out at the same value as the previous one, again indicating the flow-limited case. Comparing to the PTM to the WSM here is less straightforward, with the PTM being more linear with respect to $f_{ub}$ for low values of $CL_{int}$, and reaching its asymptote more quickly for higher values of $CL_{int}$.")
      ),
      column(8,
             plotOutput(outputId = "PTM_CL_H_vary_f_ub")
      )
  ) #Close fluidRow
  
) #Closes fluidpage UI call


# Define server logic required to generate the various outputs
server <- function(input, output, session) {
  
  withMathJax() #Allows the use of MathJax in labels and such
  # Reactive() is used throughout to indicate values should be updated with sliders
  # Since this can vary dynamically with user input, the reactive environment
  # is needed to re-execute with those input changes.
  
  # This also turns the "anim" choice into a numeric flow rate, "Q_H",
  # ultimately used by the WSM and PTM functions.
  Q_H <- reactive({
    if (input$anim == "hum"){
    Q_H <- 30
  } else if (input$anim =="mus"){
    Q_H <- 90
  } else {
    Q_H <- 30
  }})

  # This function generates the line of text in Row 1, Column 1
  # that returns the well-stirred model prediction given the user inputs.
  # (It seemed easier to generate the entire line this way
  # than only putting the numerical value in dynamically).
  # It calls the WSM_hepatic_clearance function, and feeds it Q_H(), the
  # () needed as it's a reactive function, input$f_ub, and input$CL_int,
  # and returns the entire output as a line of text.
  
  output$WSM_CL_H_text <- renderText({
    withMathJax()
    paste("Given the values provided, the well-stirred model predicts a total hepatic clearance of $CL_{H,WSM}$ = ",
    signif(WSM_hepatic_clearance(Q_H(),input$f_ub,input$CL_int),digits=4),
    ".")
  })
  
  # This function generates the line of text in Row 1, Column 1
  # that returns the parallel tube model prediction given the user inputs.
  # It calls the PTM_hepatic_clearance function, and feeds it Q_H(), the
  # () needed as it's a reactive function, input$f_ub, and input$CL_int,
  # and it returns the entire output as a line of text.
  
  output$PTM_CL_H_text <- renderText({
    withMathJax()
    paste("Given the values provided, the parallel tube model predicts a total hepatic clearance of $CL_{H,PTM}$ = ",
          signif(PTM_hepatic_clearance(Q_H(),input$f_ub,input$CL_int),digits=4),
          ".")
  })
    
  
  # This generates the plot in Row 2, column 1 for the well-stirred model
  # as a function of intrinsic clearance (keeping fixed unbound fraction and
  # flow rate.)
  # It calls WSM_hepatic_clearance, and feeds it Q_H(), input$f_ub, and a
  # vector x for the intrinsic clearances, with x varying from 0.1 to 10000,
  # taking on 2000 values spaced in between, and uses these 2000 returned values
  # to return the associated plot.
  
  output$WSM_CL_H_vary_CL_int <- renderPlot({
    curve(WSM_hepatic_clearance(Q_H(),input$f_ub,x), from=0.1, to=10000, n=2000, log="x", xlab=expression(CL[int]), ylab=expression(CL[H]), 
          col="blue", lwd=2, main=expression("WS Model for " *CL[H]* " varying " *CL[int]* " but " *f[ub]* " fixed" ))
    
  }, #Closes with command
  res=96
  ) #Closes renderPlot
  
  # This generates the plot in Row 3, column 2 for the well-stirred model
  # as a function of unbound fraction (keeping fixed intrinsic clearance and
  # flow rate.)
  # It calls WSM_hepatic_clearance, and feeds it Q_H(), input$CL_int, and a
  # vector x for the unbound fractions, with x varying from 0.001 to 1,
  # taking on 1000 values spaced in between, and uses these 1000 returned values
  # to return the associated plot.
  
  output$WSM_CL_H_vary_f_ub <- renderPlot({
    curve(WSM_hepatic_clearance(Q_H(),x,input$CL_int), from=0.001, to=1, n=1000, xlab=expression(f[ub]), ylab=expression(CL[H]), 
          col="blue", lwd=2, main=expression("WS Model for " *CL[H]* " varying " *f[ub]* " but " *CL[int]* " fixed" ))
    
  }, #Closes with command
  res=96
  ) #Closes renderPlot
  
  # This generates the plot in Row 4, column 1 for the parallel tube model
  # as a function of intrinsic clearance (keeping fixed unbound fraction and
  # flow rate.)
  # It calls PTM_hepatic_clearance, and feeds it Q_H(), input$f_ub, and a
  # vector x for the intrinsic clearances, with x varying from 0.1 to 10000,
  # taking on 2000 values spaced in between, and uses these 2000 returned values
  # to return the associated plot.
  
  output$PTM_CL_H_vary_CL_int <- renderPlot({
    curve(PTM_hepatic_clearance(Q_H(),input$f_ub,x), from=0.1, to=10000, n=2000, log="x", xlab=expression(CL[int]), ylab=expression(CL[H]), 
          col="blue", lwd=2, main=expression("PT Model for " *CL[H]* " varying " *CL[int]* " but " *f[ub]* " fixed" ))
    
  }, #Closes with command
  res=96
  ) #Closes renderPlot
  
  # This generates the plot in Row 5, column 2 for the parallel tube model
  # as a function of unbound fraction (keeping fixed intrinsic clearance and
  # flow rate.)
  # It calls PTM_hepatic_clearance, and feeds it Q_H(), input$CL_int, and a
  # vector x for the unbound fractions, with x varying from 0.001 to 1,
  # taking on 1000 values spaced in between, and uses these 1000 returned values
  # to return the associated plot.
  
  output$PTM_CL_H_vary_f_ub <- renderPlot({
    curve(PTM_hepatic_clearance(Q_H(),x,input$CL_int), from=0.001, to=1, n=1000, xlab=expression(f[ub]), ylab=expression(CL[H]), 
          col="blue", lwd=2, main=expression("PT Model for " *CL[H]* " varying " *f[ub]* " but " *CL[int]* " fixed" ))
    
  }, #Closes with command
  res=96
  ) #Closes renderPlot
  
  
  
} #Closes shiny server commands
shinyApp(ui = ui, server = server)