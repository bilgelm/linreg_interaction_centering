library(shinydashboard)

# Define UI for app that draws a histogram ----
ui <- dashboardPage(
  
  # App title ----
  dashboardHeader(title="Interaction terms in linear regression and variable centering", titleWidth = 600),
  
  dashboardSidebar(disable=T),
  
  # Main panel for displaying outputs ----
  dashboardBody(
    fluidRow(
      column(width=6,
             box(width=NULL, title="Introduction", solidHeader = T, status = "info",
                 "This interactive example illustrates how to interpret the terms in a linear regression when an interaction is included as an independent variable."
             ),
             box(width=NULL, title="Scatter plot of data", solidHeader = T, status="primary",
                 plotOutput("scatterPlot", height=250)
             ),
             box(width=NULL, title="Linear regression setup", solidHeader = T, status="warning",
                 "We will fit a linear model to the data plotted above.",
                 "It appears that the association between x and y might differ by group.",
                 "To model this, we will include an interaction term between x and group in the model:",
                 uiOutput("lmEq"),
                 "Here, i refers to observation index, g is group, betas are linear regression coefficients to be estimated, and epsilon is i.i.d. 0-mean Gaussian noise with unknown variance."
             ),
             box(width=NULL, title="Visualization of linear regression results", solidHeader = T, status="warning",
                 "The estimated main effects and their 95% confidence intervals are visualized below by group.",
                 plotOutput("lmPlot", height=250)
             )
      ),
      column(width=6,
             box(width=NULL, title="Exploration of independent variable centering", solidHeader = T, status="danger",
                 "Use the slider below to change the value at which the independent variable",span("x",style="font-style:italic"),"is centered prior to fitting the linear regression model.
            Centering refers to setting the origin of ",span("x",style="font-style:italic"),", 
            i.e., centering at 5 means that we subtract 5 from ",span("x",style="font-style:italic")," prior to fitting the model.
            As you move the slider, notice how the linear regression results presented in the table below change (whereas the slopes or confidence intervals visualized in the plot to the left do not change).",
                 sliderInput(inputId = "xcenter",
                             label = "x is centered at:",
                             min = 0.0,
                             max = 10.0,
                             value = 0.0,
                             step = 0.5)
             ),
             box(width=NULL, title="Linear regression results", solidHeader=T, status="warning",
                 htmlOutput("lmSummary")
             ),
             
             box(width=NULL, title="Take-aways", solidHeader = T, status="success",
                 tags$ul(
                   tags$li("Centering does not affect the goodness of fit of the model or the coefficients of the variable being centered (",span("x",style="font-style:italic"),") or the interaction term."),
                   tags$li("Centering affects the coefficient of the variable that the variable being centered (",span("x",style="font-style:italic"),") interacts with in the model.
                            In this particular example, this would be Group. 
                            The coefficient of Group reflects the groupwise difference at the value ",span("x",style="font-style:italic")," is centered at."),
                   tags$li("Centering is important for (the interpretation of) each variable included in interaction terms in a linear regression model."),
                   tags$li("Centering also affects the intercept (i.e., Constant), because this term corresponds to the expected value in the reference group at the value ",span("x",style="font-style:italic")," is centered at.")
                 )
             )
      )
    )
  )
)
  
server <- function(input, output) {
  library(ggplot2)
  library(stargazer)
  
  # generate random data
  set.seed(42)
  
  true_coef <- list(intercept = -4,
                    beta_x = 1,
                    beta_group = 0,
                    beta_interaction = 3,
                    noise_sd = 12)
  
  n <- 50
  group <- rbinom(n, 1, .5)
  x <- runif(n, min=0, max=10)
  y <- true_coef$intercept + 
       true_coef$beta_x * x + 
       true_coef$beta_group * group + 
       true_coef$beta_interaction * x * group + 
       rnorm(n, 0, true_coef$noise_sd)
  data <- data.frame(x=x, y=y, group=group)
  
  output$scatterPlot <- renderPlot({
    ggplot(data, aes(x,y,color=factor(group))) +
      geom_point() + scale_color_discrete(name="Group") +
      theme_bw() + theme(legend.position=c(0.1,0.8), legend.box.background = element_rect(colour = "black",size=1)) +
      xlab("x") + ylab("y")
  })
  
  output$lmEq <- renderUI({
    withMathJax(helpText('$$y_i = \\beta_0 + \\beta_1 x_i + \\beta_2 g_i + \\beta_3 x_i g_i + \\varepsilon_i.$$'))
  })
  
  output$lmPlot <- renderPlot({
    ggplot(data, aes(x,y,color=factor(group))) +
         geom_smooth(method="lm") + scale_color_discrete(name="Group") +
         geom_vline(xintercept = input$xcenter, linetype="dashed") + 
         theme_bw() + theme(legend.position=c(0.1,0.8), legend.box.background = element_rect(colour = "black",size=1)) +
         xlab("x") + ylab("y") + ggtitle("Linear regression")
  })
  
  output$lmSummary <- renderUI({
    lm.fit <- lm(y ~ I(x-input$xcenter)*group, data=data)
    HTML(stargazer(lm.fit, type="html",
                   covariate.labels=c(paste0("x (centered at ",input$xcenter,")"),
                                      "Group (reference = 0)",
                                      "Interaction of x and group"),
                   report="vcsp*", ci=TRUE, 
                   star.cutoffs=c(0.05,0.01,0.001)))
  })
  
}

shinyApp(ui = ui, server = server)