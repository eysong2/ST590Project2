library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(stargazer)
library(rpart)
library(haven)
library(knitr)
library(tree)
library(class)
library(dataPreparation)

myData <- read.csv(file="https://www4.stat.ncsu.edu/~post/558/datasets/smokeData.csv")

shinyUI(fluidPage(

  # title
  titlePanel("Cigarette Smoking by Ethnicity in the U.S."),

  # sidebar with options for the data set
  sidebarLayout(
    sidebarPanel(
      br(),
      br(),
      br(),
      selectizeInput("var1", "Descriptive Statistics & Regression Models: Select Variable", selected='Education', 
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
      selectizeInput("cp", "Tree-Base Models: Select Complexity Parameter of the tree (CP)", selected='0', 
                     choices = c("0", "0.0025")),
      br(),
      selectizeInput("pca", "Principal Components Analysis: Select Number of PCs", selected='5', 
                     choices = c("5","6","7")),
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
                   textOutput("tab4"),
                   textOutput("tab5"),
                   textOutput("tab6"),
                   br(),
                   textOutput("package"), 
                   uiOutput("datatitle"),
                   textOutput("datainfo"),
                   uiOutput("vartitle"),
                   textOutput("var2info"),
                   textOutput("var3info"),
                   textOutput("var4info"),
                   textOutput("var4binfo"),
                   textOutput("var4cinfo"),
                   textOutput("var4dinfo"),
                   textOutput("var5info"),
                   textOutput("var6info"),
                   textOutput("var7info"),
                   textOutput("var8info"),
                   helpText(   a("Click Here to Download Data", href="https://www4.stat.ncsu.edu/~post/558/datasets/smokeData.csv", target="_blank")),
                   uiOutput("smoketitle"),
                   textOutput("info1"),
                   helpText(   a("Click Here to get more information on Smoking & Tobacco Use in the U.S.", href="https://www.cdc.gov/tobacco/index.htm", target="_blank")),
                   br(),
                   uiOutput("reftitle"),
                   textOutput("ref1"),
                   textOutput("ref2")
                 )
                ),
        tabPanel("Descriptive", 
                 fluidRow(
                   br(),
                   # create sub-title
                   uiOutput("densitytitle"),
                   plotOutput("density"),
                   downloadButton('densitydata_csv', "Download the data for the density graph"),
                   withMathJax(),
                   uiOutput("formula"),
                   downloadButton('downloadDensity', "Download the density plot"),
                   uiOutput("histtitle"),
                   plotOutput("hist"),
                   downloadButton('histogramdata_csv', "Download the data for the histogram graph"),
                   downloadButton('downloadHistogram', "Download the histogram graph")     
                  )
                ),
        tabPanel("Plot", 
                 fluidRow(
                   uiOutput("scatertitle"),
                   uiOutput("smokeinfo"),  
                   uiOutput("ageinfo"),  
                   plotOutput("scatterPlot", click = "plot_click"),
                   downloadButton('scatterplotdata_csv', "Download the data for the scatter plot"),
                   downloadButton('downloadScatterplot', "Download the scatter plot"),                  
                   verbatimTextOutput("clickinfo")
                  )
                ),        
        tabPanel("Table", dataTableOutput('table')
                 ),
        tabPanel("Regression Models", 
                 fluidRow(
                   helpText(   a("Regression Model Plots and Model Fit Results Summary")),
                   tabPanel("Linear plot", p(), plotOutput("lmPlot")),
                   verbatimTextOutput("linearmodSum")
                 )
                ),
        tabPanel("Tree-Based Models", 
                 fluidRow(
                   helpText(   a("Classification Trees Plots and Model Fit Results Summary")),
                   plotOutput("treePlot"),
                   verbatimTextOutput("treeSum")
                  )
                ),
        tabPanel("Principal Components Analysis", 
                 fluidRow(
                   helpText(   a("Principal Components Analysis (PCA)")),
                   plotOutput("varexPlot"),
                   plotOutput("pcaPlot"),
                   verbatimTextOutput("pcaSum")
                 )
                )
        )
    )
  )
))