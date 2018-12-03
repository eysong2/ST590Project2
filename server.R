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

shinyServer(function(input, output, session) {
  
  #get filtered data
  df1 <- reactive({
    newData <- myData %>% filter(Ethnicity == input$ethnicity)
  })
  
# Descriptive statistics ---------------------------------------------------------------------------
  
  #create sub-title 
  output$densitytitle <- renderUI({
    h4(paste("Density of Packs Per Day by Ethnicity"))
  })
  
  #draw density plot
  plotInput <- reactive({
    ggplot() + 
      geom_density(aes(x=PacksPerDay, fill = Ethnicity), data=myData, alpha = 0.5) + xlab("Number of packs smoked per day")
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
  
 #download data for the plot
  densitydata <- reactive({
    densitydata <- myData %>% 
      dplyr::select(SEQN, Ethnicity, PacksPerDay)
  })
  
  # Download Data for this tab
  output$densitydata_csv <- downloadHandler(
    filename = "densitydata.csv",
    content = function(file) {
      write.csv(densitydata(), file)
    },
    contentType = "text/csv"
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
    ggplot(myData, aes(x = IncomeGroup, fill = Ethnicity)) + geom_bar(aes(y = ((..count..)/sum(..count..))*100)) + labs(x = "Income Group", y = "Percent")
     } else if (input$var1=='Education'){
    ggplot(myData, aes(x = Education, fill = Ethnicity)) + geom_bar(aes(y = ((..count..)/sum(..count..))*100)) + labs( y = "Percent") 
   } 
  })
  
  output$hist <- renderPlot({
    histInput()
  })
  
  #create to save function
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
  
  #download data for the histograms
  histogramdata <- reactive({
    histogramdata <- myData %>% 
      dplyr::select(SEQN, Ethnicity, IncomeGroup, Education)
  })
  
  # Download Data for this tab
  output$histogramdata_csv <- downloadHandler(
    filename = "histogramdata.csv",
    content = function(file) {
      write.csv(histogramdata(), file)
    },
    contentType = "text/csv"
  )  
  
# Scatter plot --------------------------------------------------------------------------------------
  
  #create renderUI for sub-title with user interfaces 
  output$scatertitle <- renderUI({
    h3(paste("Scatter Plot of Age & Packs per Day for", input$ethnicity, sep =" "))
  })

  #create scatter plot by Ethnicity
  scatterInput <- reactive({
    #get filtered data
    newData <- df1()
    #create plot
    g <- ggplot(newData, aes(x = Age, y = PacksPerDay)) + ylab("Number of packs smoked per day")
    
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
  
  #download data for the scatter plot
  scatterplotdata <- reactive({
    scatterplotdata <- myData %>% 
      dplyr::select(SEQN, PacksPerDay, Age, Ethnicity)
  })
  
  # Download Data for this tab
  output$scatterplotdata_csv <- downloadHandler(
    filename = "scatterplotdata.csv",
    content = function(file) {
      write.csv(scatterplotdata(), file)
    },
    contentType = "text/csv"
  )    
  
#create data table by Ethnicity--------------------------------------------------------------------------    
  output$table <- renderDataTable(
    newData <- df1()
  )
  
  

 #Supervised learning model 1 - Regression modeling-----------------------------------------------------

  modelData <- subset(myData) %>% select(Ethnicity, Education, IncomeGroup, PacksPerDay)
  
  linearInput <- reactive({
    if (input$var1=='IncomeGroup'){
      ggplot(modelData, aes(x = IncomeGroup, y = PacksPerDay, color = Ethnicity)) + geom_point() + geom_smooth(method = "lm") + ylab("Number of packs smoked per day")
    } else if (input$var1=='Education'){
      ggplot(modelData, aes(x = Education, y = PacksPerDay, color = Ethnicity)) + geom_point() + geom_smooth(method = "lm") + ylab("Number of packs smoked per day")
    } 
  })
  
  # plot model
  output$lmPlot <- renderPlot({
    linearInput()
  })
  
  linearMod <- reactive({
    if (input$var1=='IncomeGroup'){
      lm(PacksPerDay ~ IncomeGroup, data=myData)
     } else if (input$var1=='Education'){
      lm(PacksPerDay ~ Education, data=myData) 
    } 
  })
    
  output$linearmodSum <- renderPrint({
    summary(linearMod())
  })

 
 #Supervised learning model 2 - Tree-Based modeling----------------------------------------------------------------
  
  treeData <- subset(myData) %>% select(Ethnicity, Age, Education, IncomeGroup, PacksPerDay)
  
  # grow tree   
  fitInput <- reactive({
    if (input$cp=='0'){
      rpart(PacksPerDay ~ ., data = treeData, method = "class", control =rpart.control(minsplit =1,minbucket=1, cp=0))
    } else if (input$cp=='0.0025'){
      rpart(PacksPerDay ~ ., data = treeData, method = "class", control =rpart.control(minsplit =1,minbucket=1, cp=0.0025))
    } 
  })
  
  # plot tree 
  output$treePlot <- renderPlot({
    plot(fitInput(), compress=TRUE)
    title(main="Classification Tree ")
  })
   
  output$treeSum <- renderPrint({
    summary(fitInput())
  }) 
  
  
#Unsupervised learning model: PCA--------------------------------------------------------------------------
  
  df <- subset(myData) %>% select(INDHHINC, INDFMPIR, INDFMINC, DMDHHSIZ, Age, Education, PacksPerDay)
  pcaData <- na.omit(df)
  
  #standardize the variables 
  scales <- build_scales(dataSet = pcaData, cols = c("INDHHINC", "INDFMPIR", "INDFMINC", "DMDHHSIZ", "Age", "Education", "PacksPerDay"), verbose = TRUE)
  pcaDataScale <- fastScale(dataSet = pcaData, scales = scales, verbose = TRUE)
  
  PCs <- prcomp(select(pcaDataScale, INDHHINC, INDFMPIR, INDFMINC, DMDHHSIZ, Age, Education, PacksPerDay), center = TRUE, scale = TRUE)
  
  output$pcaSum <- renderPrint({
    summary(PCs)
  })

  #compute standard deviation of each principal component
  std_dev <- PCs$sdev
  
  #compute variance
  pr_var <- std_dev^2
  
  #check variance of components
  pr_var[1:7]
  
  #proportion of variance explained
  prop_varex <- pr_var/sum(pr_var)
  
  # select number of PCs   
  pcaInput <- reactive({
    if (input$pca=='5'){
      prop_varex[1:5]
    } else if (input$pca=='6'){
      prop_varex[1:6]
    } else if (input$pca=='7'){
      prop_varex[1:7]
    }  
    })
  
  output$varexPlot <- renderPlot({
    plot(pcaInput(), xlab = "Principal Component",
         ylab = "Variance",
         type = "b")
  })
  
  bipInput <- reactive({
    if (input$pca=='5'){
      PCs <- prcomp(select(pcaDataScale, INDHHINC, INDFMPIR, INDFMINC, DMDHHSIZ, Age), center = TRUE, scale = TRUE)
    } else if (input$pca=='6'){
      PCs <- prcomp(select(pcaDataScale, INDHHINC, INDFMPIR, INDFMINC, DMDHHSIZ, Age, Education), center = TRUE, scale = TRUE)
    } else if (input$pca=='7'){
      PCs <- prcomp(select(pcaDataScale, INDHHINC, INDFMPIR, INDFMINC, DMDHHSIZ, Age, Education, PacksPerDay), center = TRUE, scale = TRUE)
    }  
  })
  
  output$pcaPlot <- renderPlot({
    biplot(bipInput(), xlabs = rep(".", nrow(pcaDataScale)), cex = 1.2)
  })
  
# Infomation page -------------------------------------------------------------------------------------------------
  
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
  
  output$tab4 <- renderText({
    paste("4. Regression Tab: You can examine the relationship between Number of packs smoked per day with variable of your interest.")
  })
  
  output$tab5 <- renderText({
    paste("5. Tree-BasedModels Tab: You can create a model that predicts the value of a target variable based on several input variables.")
  })
  
  output$tab6 <- renderText({
    paste("6. Principal Components Analysis Tab: You can reduce a large set of variables to a small set that still contains most of the information in the large dataset.")
  })  
   
  output$package <- renderText({
    paste("**Package needed:   library(shiny)
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
  library(dataPreparation)")
  })    
  

  #create renderUI for sub-title 
  output$datatitle <- renderUI({
    h3(paste("2. About the Data:"))
  })
  
  #create info text
  output$datainfo <- renderText({
    paste("This is survey data collected by the US National Center for Health Statistics (NCHS) which has conducted a series of health and nutrition surveys since the early 1960's. Since 1999 approximately
          5,000 individuals of all ages are interviewed in their homes every year and complete the health examination component of the survey. The health examination is conducted in a mobile examination
          centre (MEC). Total number of sample size is 443. Data is available at https://www4.stat.ncsu.edu/~post/558/datasets/smokeData.csv and also can download from here.")
  })
  
  #create renderUI for sub-title 
  output$vartitle <- renderUI({
    h4(paste("Data Dictionary for key variables"))
  })
  
  output$var2info <- renderText({
    paste("Gender - Gender")
  })
  
  output$var3info <- renderText({
    paste("Age - Age")
  })
  
  output$var4info <- renderText({
    paste("IncomeGroup - Income (Less Than 20K vs. More Than 20K)")
  })
  
  output$var4binfo <- renderText({
    paste("INDHHINC - Estimated total household income")
  })
 
  output$var4cinfo <- renderText({
    paste("INDFMPIR - Ratio of family income to poverty")
  }) 

  output$var4dinfo <- renderText({
    paste("INDFMINC - Annual Family Income")
  })   

  output$var5info <- renderText({
    paste("Ethnicity - Ethnicity (MexicanAmerican & Hispanic, Non-Hispanic Black, Non-Hispanic Caucasian)")
  })
  
  output$var6info <- renderText({
    paste("Education - Education (1:8th grade, 2:9-11th grade, 3: High School, 4:Some college, 5: College graduate)")
  })

  output$var7info <- renderText({
    paste("PacksPerDay - Number of packs smoked per day")
  })
  
  output$var8info <- renderText({
    paste("DMDHHSIZ - Number of people in the participant's household")
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
  
  output$reftitle <- renderUI({
    h4(paste("Reference:"))
  })
  
  output$ref1 <- renderText({
    paste("1. CDC(2018). Smoking & Tobacco Use. Derived from https://www.cdc.gov/tobacco/data_statistics/fact_sheets/index.htm?s_cid=osh-stu-home-spotlight-001 on October 27, 2018.")
  })
  output$ref2 <- renderText({
    paste("2. Hlavac, Marek (2018). stargazer: Well-Formatted Regression and Summary Statistics Tables. R package version 5.2.2. https://CRAN.R-project.org/package=stargazer")
  })
})