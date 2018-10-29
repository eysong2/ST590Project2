library(shiny)
library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)

myData <- read.csv(file="https://www4.stat.ncsu.edu/~post/558/datasets/smokeData.csv")

shinyServer(function(input, output, session) {
  
  #get filtered data
  df1 <- reactive({
    newData <- myData %>% filter(Ethnicity == input$ethnicity)
  })
  
  #create sub-title 
  output$densitytitle <- renderUI({
    h4(paste("Density of Packs Per Day by Ethnicity"))
  })
  
  #draw density plot
  plotInput <- reactive({
    ggplot() + 
      geom_density(aes(x=PacksPerDay, fill = Ethnicity), data=myData, alpha = 0.5) 
  })
  
  output$density <- renderPlot({
    plotInput()
  })

  #create to density plot save fundtion
  output$downloadDensity <- downloadHandler(
    filename = function() {
      paste("density", ".png", sep="")
     },
    content = function(file) {
      png(file)
      plot(plotInput())
      dev.off()
    }
   )

 #add special symbol using withMathJax
  output$formula <- renderUI({
    withMathJax(
      helpText('Density formula 
               $$density=\\frac{mass}{volume}$$'))
    })
  
  #create renderUI for 1st level header with user interfaces 
  output$histtitle <- renderUI({
    h3(paste("Descrtive summary statistics of ", input$var1, "by Ethinicity", sep = ""))
  })
 
  #create histogram by Ethnicity
  histInput <- reactive({
   if (input$var1=='IncomeGroup'){
    ggplot(myData, aes(x = IncomeGroup, fill = Ethnicity)) + geom_bar()
   } else if (input$var1=='Education'){
    ggplot(myData, aes(x = Education, fill = Ethnicity)) + geom_bar()
   } 
  })
  
  output$hist <- renderPlot({
    histInput()
  })
  
  #create to save fundtion
  output$downloadHistogram <- downloadHandler(
    filename = function() {
      paste("histogram", ".png", sep="")
    },
    content = function(file) {
      png(file)
      plot(histInput())
      dev.off()
    }
  )
  
  #create renderUI for sub-title with user interfaces 
  output$scatertitle <- renderUI({
    h3(paste("Scatter Plot of Age & Packs per Day for", input$ethnicity, sep =" "))
  })

  #create scatter plot by Ethnicity
  scatterInput <- reactive({
    #get filtered data
    newData <- df1()
    #create plot
    g <- ggplot(newData, aes(x = Age, y = PacksPerDay))
    
    if(input$income){
      g + geom_point(aes(col = IncomeGroup))
    } else {
      g + geom_point()
    }
  })
  
  output$scatterPlot <- renderPlot({
    scatterInput()
  })
  
  #create to save fundtion
  output$downloadScatterplot <- downloadHandler(
    filename = function() {
      paste("scatterPlot", ".png", sep="")
    },
    content = function(file) {
      png(file)
      plot(scatterInput())
      dev.off()
    }
  )
  
  output$clickinfo <- renderText({
    paste0("Age=", input$plot_click$x, "\nPacks per day=", input$plot_click$y)
  })

  #create renderUI for sub-title
  output$scatertitle <- renderUI({
    h3(paste("Scatter Plot of Age & Packs per Day for", input$ethnicity, sep =" "))
  })
  
  #create renderUI for summary statistics
  output$smokeinfo <- renderUI({
    #get filtered data
    newData <- df1()
    
    #create output of text info
    if ((input$income) & (input$summary)) {
      h5(paste("*The average number of packs of cigarettes smoked per day is", round(mean(newData$PacksPerDay, na.rm = TRUE), 2), ".", sep = " "))
    }  
   })
  
  #create renderUI for summary statistics
  output$ageinfo <- renderUI({
    #get filtered data
    newData <- df1()
    
    #create output of text info
    if ((input$income) & (input$summary)) {
      h5(paste("*The average age for ", input$Ethnicity, "is", round(mean(newData$Age, na.rm = TRUE), 2), ".", sep = " "))
    }  
  })
  
  #create data table by Ethnicity    
  output$table <- renderDataTable(
    newData <- df1()
  )
  
  #create renderUI for sub-title
  output$infotitle <- renderUI({
    h3(paste("This App will help you to understand cigarette smoking among adults by ethnicity in the United States."))
  })
  
  #create renderUI for sub-title 
  output$apptitle <- renderUI({
    h3(paste("1.About the App:"))
  })
  
  #create Tab info text
  output$tab1 <- renderText({
    paste("1. Descriptive Tab: You can check descriptive statistics for selected variables.")
  })
  output$tab2 <- renderText({
    paste("2. Plot Tab: You can check scatterplots for selected variables by ethnicity.")
  })
  output$tab3 <- renderText({
    paste("3. Table Tab: You can check data table by ethnicity and download the data tables.")
  })
  
  #create renderUI for sub-title 
  output$datatitle <- renderUI({
    h3(paste("2. About the Data:"))
  })
  
  #create info text
  output$datainfo <- renderText({
    paste("Data is collected from adults in the US regarding on demographic characteristics and smoking behaviors. Total number of sample size is 443. Data is available at https://www4.stat.ncsu.edu/~post/558/datasets/smokeData.csv and also can download from here.")
  })
  
  #create renderUI for sub-title 
  output$smoketitle <- renderUI({
    h3(paste("3. Smoking Facts:"))
  })
  
  output$info1 <- renderText({
    paste("Tobacco use is the leading cause of preventable death in the United States. Nearly 40 million US adults still smoke cigarettes, and about 4.7 million middle and high school students use at 
          least one tobacco product,including e-cigarettes. Every day, more than 3,800 youth younger than 18 years smoke their first cigarette. Each year, 
          nearly half a million Americans die prematurely of smoking or exposure to secondhand smoke. Another 16 million live with a serious illness caused by smoking. Each year, the United States spends nearly $170 billion on medical care to treat smoking-related disease in adults.")
  })
  
  output$info2 <- renderText({
    paste("Reference: CDC(2018). Smoking & Tobacco Use. Derived from https://www.cdc.gov/tobacco/data_statistics/fact_sheets/index.htm?s_cid=osh-stu-home-spotlight-001 on October 27, 2018.")
  })
})