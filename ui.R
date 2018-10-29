shinyUI(fluidPage(

  # title
  titlePanel("Cigarette Smoking by Ethnicity in the U.S."),

  # sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      br(),
      br(),
      br(),
      selectizeInput("var1", "Descriptive statistics: Select Variable", selected='IncomeGroup', 
                       choices = c("Income"="IncomeGroup", "Education"="Education")), 
      br(),
      br(),
      selectizeInput("ethnicity", "Scatter Plot & Table: Select Ethnicity", selected="Non-Hispanic Caucasian", choices = levels(as.factor(myData$Ethnicity))
                     ),      
      checkboxInput("income", h4("Color Code by Income", style = "color:blue;")),
      #Create contitional checkbox
      conditionalPanel(
        condition="input.income ==true",
        checkboxInput("summary", h4("Show summary statistics!", style = "color:green;"))
      ),
      br(),
      helpText(   a("Click Here to Download Data", href="https://www4.stat.ncsu.edu/~post/558/datasets/smokeData.csv", target="_blank"))
    ),

    # show outputs
    mainPanel(
      tabsetPanel(
        tabPanel("Information", 
                 fluidRow(
                   br(),
                   uiOutput("infotitle"),
                   uiOutput("apptitle"),
                   textOutput("tab1"),
                   textOutput("tab2"),
                   textOutput("tab3"),
                   uiOutput("datatitle"),
                   textOutput("datainfo"),
                   helpText(   a("Click Here to Download Data", href="https://www4.stat.ncsu.edu/~post/558/datasets/smokeData.csv", target="_blank")),
                   uiOutput("smoketitle"),
                   textOutput("info1"),
                   helpText(   a("Click Here to get more information on Smoking & Tobacco Use in the U.S.", href="https://www.cdc.gov/tobacco/index.htm", target="_blank")),
                   textOutput("info2")
                 )
                ),
        tabPanel("Descriptive", 
                 fluidRow(
                   br(),
                   # create sub-title
                   uiOutput("densitytitle"),
                   plotOutput("density"),
                   withMathJax(),
                   uiOutput("formula"),
                   downloadButton('downloadDensity', "Download the density plot"),
                   uiOutput("histtitle"),
                   plotOutput("hist"),
                   downloadButton('downloadHistogram', "Download the histogram")                 
                  )
                ),
        tabPanel("Plot", 
                 fluidRow(
                   uiOutput("scatertitle"),
                   uiOutput("smokeinfo"),  
                   uiOutput("ageinfo"),  
                   plotOutput("scatterPlot", click = "plot_click"),
                   downloadButton('downloadScatterplot', "Download the scatter plot"),                  
                   verbatimTextOutput("clickinfo")
                  )
                ),        
        tabPanel("Table", dataTableOutput('table'))
      )
    )
  )
))