library(shiny)

########################3

#Importing libraries
#install.packages('shinythemes')
library(shinythemes)
library(shiny)
library(tidyverse)
library(caret)
library(kernlab)
library(imputeTS)
library(rpart.plot)
library(tidyverse)
library(imputeTS)
library(kernlab)
library(caret)
library(readr)
library(tidyverse)
library(imputeTS)
library(kernlab)
library(caret)
library(readr)
library(maps)

ui<-(pageWithSidebar(
  #Headers for the application
  headerPanel("HMO vizualization and Prediction"),
  
  #input for the application
  sidebarPanel
  (
    # Input: select solution and csv file from the local system
    #First the csv is loaded and then the solution is loaded
    fileInput(inputId = "upload", "Upload File ",accept = ".csv"),
    fileInput("upload_Solution", label="solution file for HMO",accept = c(".csv")),
    #We are displaying 7 rows from the top in the application
    numericInput("n", "Number of Rows", value = 7, min = 1, step = 1)
  ),
  
  #The application main screen has two partitions, 1st is visualizations
  #2nd is predictions based on the model built. Here we are first displaying 
  #the visualizations graphs. They include bar plot, box plot and histograms
  mainPanel(
    tabsetPanel(
      id = "uploaded_file",
      tabPanel(
        "Vizualizations", 
        tableOutput("headForDF"),
        plotOutput("graph1",height = "400px"),
        plotOutput("graph2",height = "400px"),
        plotOutput("graph3",height = "400px"),
        plotOutput("graph4",height = "400px"),
        plotOutput("graph5",height = "400px"),
        plotOutput("graph6",height = "400px"),
        plotOutput("graph7",height = "400px")
        
      ),
      
      #These are the predictions from the model we have built.
      tabPanel("Predictions", 
               verbatimTextOutput("TestConfusionMatrix", placeholder = TRUE)
               
               
      )
    )
  )
  
))












#Here server logic is being defined. This is required to plot histogram
server <- function(input, output,session) {
  HMO <- read.csv("https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv", stringsAsFactors = FALSE)
  set.seed(0)
  #Since BMI has  null values, they have to be interpolated using the na_interpolation method
  #Hypertension has binary values and hence na_interpolation cannot be used for this column.
  #The rows with null values for hypertension are removed.
  sum(is.na(HMO))
  HMO <- HMO[!is.na(HMO$hypertension),]
  HMO$bmi <- na.interpolation(HMO$bmi)
  
  
  HMO$smoker = as.factor(HMO$smoker)
  HMO$location_type = as.factor(HMO$location_type)
  HMO$education_level = as.factor(HMO$education_level)
  HMO$yearly_physical = as.factor(HMO$yearly_physical)
  HMO$exercise = as.factor(HMO$exercise)
  HMO$married = as.factor(HMO$married)
  HMO$gender = as.factor(HMO$gender)
  
  quantile(HMO$cost, probs = 0.78)
  HMO$expensive <- ifelse(HMO$cost > quantile(HMO$cost, probs = 0.78),'yes','no')
  HMO$expensive = as.factor(HMO$expensive)
  
  HMO$Age_Group <- ifelse(HMO$age < 25, "Youth", NA)
  HMO$Age_Group <- ifelse(HMO$age >= 25 & HMO$age < 35, "Young Adults", HMO$Age_Group)
  HMO$Age_Group <- ifelse(HMO$age >= 35 & HMO$age < 50, "Middle Aged Adults", HMO$Age_Group)
  HMO$Age_Group <- ifelse(HMO$age >= 50 & HMO$age < 64, "Old Aged Adults", HMO$Age_Group)
  HMO$Age_Group <- ifelse(HMO$age >= 64, "Seniors", HMO$Age_Group)
  HMO$Age_Group = as.factor(HMO$Age_Group)
  
  
  
  trainList <- createDataPartition(y=HMO$expensive,p=.60,list=FALSE)
  trainSet <- HMO[trainList,]
  testSet <- HMO[-trainList,]
  
  SVM_1 <- ksvm(expensive ~ age+bmi+smoker+exercise+hypertension, data=trainSet,C = 8,cross = 3, prob.model = TRUE)
  SVM_1
  
  svmpredict <- predict(SVM_1, newdata=testSet, type = "response")
  
  # Confusion matrix
  CM <- confusionMatrix(svmpredict, testSet$expensive)
  CM
  
  
  save(SVM_1, file = "SVM_1.rda")
  
  
  # Define UI for application that draws a histogram
  # App look and fill
  
  #For the input we need to browse and upload the csv and solution file
  getTestData <- reactive({
    req(input$upload)
    read_csv(input$upload$datapath)
  })
  #For getting the solution data, we need actual values for the prediction
  #The solution csv file is the file used for prediction
  getSolutionData <- reactive({
    req(input$upload_Solution)
    read_csv(input$upload_Solution$datapath)
    #input$file2$datapath
  })
  
  #We are displaying the first 7 lines of the dataframe
  output$headForDF <- renderTable({
    HMOdf <- getTestData()
    head(HMOdf, input$n)
  })
  
  
  #These are the graphs and plots to show the visualizations based on the data set
  
  #Here we are dividing the age in different groups based on their age range. We are then
  #plotting the age groups vs the expensive column
  
  output$graph1 <- renderPlot(
    HMO %>% filter(Age_Group == "Youth" | Age_Group == "Young Adults" | Age_Group == "Middle Aged Adults"  | Age_Group == "Old Aged Adults" | Age_Group == "Seniors")
    %>% ggplot(aes(Age_Group, fill = expensive)) +geom_bar(position = "dodge", alpha = 0.5)  + labs(title="Age Group vs Expensive")
  )
  
  #Here we are plotting the relationship between number of children and cost
  output$graph2 <- renderPlot(
    HMO %>% group_by(children) %>% summarize(age= mean(age), cost = mean(cost)) %>% ggplot(aes(x=children, y= cost, fill=children)) + geom_col()
  )
  
  #Here we are plotting the relationship between BMI and the expensive column in the yellow color
  output$graph3 <- renderPlot(
    ggplot(HMO,aes(x=expensive,y=bmi))+geom_boxplot(fill='yellow')+ggtitle('BMI vs Expense')
    
  )
  
  #Here we are plotting the smoker status of a person and the cost column
  output$graph4 <- renderPlot(
    ggplot(HMO,aes(x=smoker,y=cost))+geom_boxplot(fill='purple')+ggtitle('Smoker vs Cost')
    
  )
  
  #Here we are plotting the relationship between exercise and cost via the box plot
  output$graph5 <- renderPlot(
    ggplot(HMO,aes(x=exercise,y=cost))+geom_boxplot(fill='orange', alpha =0.7)+ggtitle('Exercise vs Cost')
    
  )
  
  
  #Here we are making use of ggplot to draw maps. The output is the map of USA
  #and the states of USA with different cost amount are highlighted
  
  HMO_map <- HMO %>% group_by(location) %>% summarise(mean(cost))
  
  us <- map_data("state")
  us$state_name <- tolower(us$region)
  coord_HMO <- data.frame(loc=tolower(HMO_map$location),avg_cost=HMO_map$`mean(cost)`)
  us_coords <- merge(us,coord_HMO, by.x='state_name',by.y='loc',all.x=TRUE )
  us_coords <- us_coords %>% arrange(order)
  
  
  
  output$graph6 <- renderPlot(
    ggplot(us_coords,aes(map_id= region)) + geom_polygon(color="black",aes(x=long,y=lat,group=group,fill=avg_cost))  +
      expand_limits(x=us_coords$long, y=us_coords$lat)+coord_map("mercator")
    
  )
  
  #Here we are plotting the relationship between hypertension and expensive column in the form of bar plot
  
  output$graph7 <- renderPlot(
    HMO %>% group_by(hypertension) %>% ggplot(aes(hypertension, fill = expensive)) +geom_bar(position = "dodge", alpha = 0.5)  + labs(title= " hypertension vs expensive") 
  )
  
  output$TestConfusionMatrix <- renderPrint({
    #load the data
    dataset <- getTestData()
    dataset_solution <- getSolutionData()
    # #load and use the model on the new data
    # pred_val <- as.factor(predict(our_model,dataset))
    # dataset_solution$expensive = as.factor(dataset_solution$expensive)
    # confusionMatrix(pred_val,dataset_solution$expensive)
    # 
    
    #SVM_1 <- ksvm(expensive ~ age+bmi+smoker+exercise+hypertension, data=trainSet,C = 8,cross = 3, prob.model = TRUE)
    #SVM_1
    
    svmpredict <- predict(SVM_1, newdata=testSet, type = "response")
    
    # Confusion matrix
    confusionMatrix(svmpredict, testSet$expensive)
    
    
    
    
    #svmpredict <- predict(SVM_1, newdata=dataset, type = "response")
    
    #dataset_solution$col5 <- with(
     # dataset_solution, ifelse(dataset_solution$expensive == TRUE, 1, 0))
    
    #confusionMatrix(svmpredict,as.factor(dataset_solution$col5))
    
    
    
  })
  
  output$txt_results <- renderTable({
    HMOdf <- getTestData()
    head(HMOdf, input$n)
  })
}


#loading the model, we will do the prediction using the model and 
#using that to compute the confusion matrix

use_model_to_predict <- function(HMODf, df_solution){
  #load the pre-built model, we named it ‘out_model.rda’)
  #load(file="our_model.rda")
  value_pred <- predict(svm_1,HMOdf)
  confusionMatrix(value_pred,df_solution$expensive)
}



# Run the application 
shinyApp(ui = ui, server = server)
