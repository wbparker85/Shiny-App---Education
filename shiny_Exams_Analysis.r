setwd('~/University of Kansas/DATA 824/Module_11/Homework_Project')

library(plotly)
library(shiny)
library(tidyverse)
library(expss)
library(readxl)
library(DT)
library(shinyWidgets)
library(ggrepel)
library(cluster)
library(factoextra)
library(FactoMineR)

options(ggrepel.max.overlaps = Inf)

#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#import data and convert to factors
df <- read.csv('exams.csv')

df$sex <- factor(df$sex, levels = c('female', 'male'))
df$race_ethnicity <- factor(df$race_ethnicity)
df$parental_education <- factor(df$parental_education, levels = c('some high school',
                                                                  'high school',
                                                                  'some college',
                                                                  'associate\'s degree',
                                                                  'bachelor\'s degree',
                                                                  'master\'s degree'))
df$lunch <- factor(df$lunch)
df$test_prep <- factor(df$test_prep, levels = c('none', 'completed'))
summary(df)
str(df)
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


#begining of shiny app
ui <- fluidPage(
  navbarPage(
    'Student Exam Scores',
    tabPanel('Data Information',
      sidebarPanel(
        checkboxGroupInput('sex', 
                         'Select a sex:', 
                         choices = c('Female' = 'female',
                                     'Male' = 'male')),
      ), #close sidebar panel
  
    #start main panel for data information page
      mainPanel(
        tabsetPanel(
          tabPanel('Table', DT::DTOutput('table')),
          tabPanel('Data Summaries', verbatimTextOutput('summary')),
          br(),
          br(),
          h5(strong('Data Citation:')),
          h5('Chauhan, A. (2022, September). Students Performance in Exams, Retrieved November 21, 2022 from https://www.kaggle.com/datasets/whenamancodes/students-performance-in-exams.')
        ) #close tabsetPanel
      ) #close mainPanel
    
    
    ), #close tabPanel
    
    #exploratory tab
    tabPanel('Exploratory Analysis',
      sidebarPanel(
        selectInput('scores', 'Select a score to analyze:',
                    choices = c('Math Scores' = 'math_score',
                                'Reading Scores' = 'reading_score',
                                'Writing Scores' = 'writing_score')),
        
        radioButtons('attribute', 'Select an attribute:*',
                    choices = c('Sex' = 'sex',
                                'Race/Ethnicity' = 'race_ethnicity',
                                'Parental Education Level' = 'parental_education',
                                'Lunch Status' = 'lunch',
                                'Test Prep Course' = 'test_prep')),
                     #see if you can split into separate lines for attributes
      ),#closes sidebarPanel
    #start main panel for exploratory analysis page
      mainPanel(
        tabsetPanel(
          tabPanel('Box plot', plotOutput('boxplot')),
          tabPanel('Density Plot', plotOutput('density'))
        ) #closes tabsetpanel
      ) #closes main panel 
    ), #closes tabPanel EA
    
    #scatter tab
    tabPanel('Scatter Plots',
      sidebarPanel(
        selectInput('x_scores', 'Select an X variable:',
                    choices = c('Math Scores' = 'math_score',
                                'Reading Scores' = 'reading_score',
                                'Writing Scores' = 'writing_score')),
               
        selectInput('y_scores', 'Select a Y variable:',
                    choices = c('Math Scores' = 'math_score',
                                'Reading Scores' = 'reading_score',
                                'Writing Scores' = 'writing_score'),
                    selected = 'reading_score'),
        
        
      ), #closes sidebarPanel
      
    #start main panel for scatter page
      mainPanel(
        tabsetPanel(
          tabPanel('Scatter plot', 
                   plotOutput('scatterplot'),
                   radioButtons('attribute_scatter', 'Select an attribute:',
                                choices = c('Sex' = 'sex',
                                            'Race/Ethnicity' = 'race_ethnicity',
                                            'Parental Education Level' = 'parental_education',
                                            'Lunch Status' = 'lunch',
                                            'Test Prep Course' = 'test_prep')),
                   plotOutput('scatterplot_attribute'),
                   plotOutput('scatterplot_facet')),
          
          tabPanel('Heatmap', 
                    sliderInput('heatmap_bins', 
                                'Select a number of bins for the heatmap:',
                                min = 1,
                                max = 50,
                                value = 10,
                                width = '100%'),
                   plotOutput('heatmap'))
        ) #closes tabsetpanel
      ) #closes main panel 
    ), #closes tabPanel scatter
    
    
    # unsupervised learning tab
    tabPanel('Unsupervised Learning',
       #closes sidebarPanel
             #start main panel for scatter page
             mainPanel(
               width = '100%',
               tabsetPanel(
                 tabPanel('MCA plot', 
                          checkboxGroupInput('attribute_mca', 'Select attributes for MCA:',
                                             choices = c('Sex' = 'sex',
                                                         'Race/Ethnicity' = 'race_ethnicity',
                                                         'Parental Education Level' = 'parental_education',
                                                         'Lunch Status' = 'lunch',
                                                         'Test Prep Course' = 'test_prep'),
                                             inline = T),
                          plotOutput('mca')),
                 
                 tabPanel('Clustering',
                          sliderInput('clusters', 'Select the number of clusters for analysis:',
                                      min = 1,
                                      max = 10,
                                      value = 3),
                          plotOutput('clustering'))
               ) #closes tabsetpanel
             ) #closes main panel 
    ) #closes tabPanel unsupervised
  ) #close navbarPage
) #close ui

server <- function(input, output, session){
  
  #setup a table  
  output$table <- DT::renderDT({
    validate(
      need(input$sex != '', 'Be sure to select at least one sex.')
    )
    
    filter(df, sex %in% input$sex)
  })
  
  #setup a summary of data
  output$summary <- renderPrint({
    validate(
      need(input$sex != '', 'Be sure to select at least one sex.')
    )
    
    df_new <- filter(df, sex %in% input$sex)
    summary(df_new)
  })
  
  #setup box plots
  output$boxplot <- renderPlot({
    
    plot_data_boxplot <- df %>% 
      select(x_axis = input$attribute, y_axis = input$scores)
    
    ggplot(plot_data_boxplot, aes(x = x_axis, y = y_axis, fill = x_axis)) +
      geom_boxplot() +
      xlab(input$attribute) +
      ylab(input$scores) +
      ggtitle('Box Plot of Selected Scores (grouped by attribute)') +
      theme(legend.position = 'none',
            plot.title = element_text(size = 14, face = 'bold'))
  })
  
  #setup density plots
  output$density <- renderPlot({
    
    plot_data_density <- df %>% 
      select(x_axis = input$scores, attribute_density = input$attribute)
    
    ggplot(plot_data_density, aes(x = x_axis)) +
      geom_density(aes(color = attribute_density), position = 'identity',
                   stat = 'bin', binwidth = 8, alpha = 0.3) +
      xlab(input$scores) +
      ggtitle('Density Plot of Selected Scores') +
      theme(plot.title = element_text(size = 14, face = 'bold'))
  })
  
  #setup scatter plots without
  output$scatterplot <- renderPlot({
    plot_data_scatter <- df %>% 
      select(x_axis = input$x_scores, y_axis = input$y_scores, group = input$attribute_scatter)
    
    ggplot(plot_data_scatter, aes(x = x_axis, y = y_axis)) +
      geom_point() +
      xlab(input$x_scores) +
      ylab(input$y_scores) +
      ggtitle('Scatter Plot of Selected Scores') +
      theme(plot.title = element_text(size = 14, face = 'bold'))
  })
  
  #setup scatter plots with attribute
  output$scatterplot_attribute <- renderPlot({
    plot_data_scatter <- df %>% 
      select(x_axis = input$x_scores, y_axis = input$y_scores, group = input$attribute_scatter)
    
    ggplot(plot_data_scatter, aes(x = x_axis, y = y_axis, color = group)) +
      geom_point() +
      xlab(input$x_scores) +
      ylab(input$y_scores) +
      ggtitle('Scatter Plot of Selected Scores (colored by attribute)') +
      theme(plot.title = element_text(size = 14, face = 'bold'))
  })
  
  #setup scatter plots with attribute facet
  output$scatterplot_facet <- renderPlot({
    plot_data_scatter <- df %>% 
      select(x_axis = input$x_scores, y_axis = input$y_scores, group = input$attribute_scatter)
    
    ggplot(plot_data_scatter, aes(x = x_axis, y = y_axis, color = group)) +
      geom_point() +
      facet_wrap(~group) +
      xlab(input$x_scores) +
      ylab(input$y_scores) +
      ggtitle('Scatter Plot of Selected Scores (faceted by attribute)') +
      theme(plot.title = element_text(size = 14, face = 'bold'))
  })
  
  #setup heatmap
  output$heatmap <- renderPlot({
    plot_data_heat <- df %>% 
      select(x_axis_heat = input$x_scores, y_axis_heat = input$y_scores)
    
    ggplot(plot_data_heat, aes(x=x_axis_heat, y=y_axis_heat)) +
      geom_bin2d(bins = input$heatmap_bins) +
      xlab(input$x_scores) +
      ylab(input$y_scores) +
      scale_fill_continuous(type = "viridis") +
      ggtitle('Scatter Plot of Selected Scores (colored by attribute)') +
      theme(plot.title = element_text(size = 14, face = 'bold'))
  })
  
  #setup MCA
  output$mca <- renderPlot({
    
    validate(
      need(input$attribute_mca > 1, 'Be sure to select at least two attributes.')
    )
    df_exams <- sample_n(df, 200)
    df_exams.active <- df_exams[,c(input$attribute_mca)]
  
    res.mca <- MCA(df_exams.active, graph=FALSE)
    p <- fviz_mca_biplot(res.mca,
                       repel = TRUE)
    print(p)
  })
  
  #setup clusters
  output$clustering <- renderPlot({
    
    df_exams <- sample_n(df, 200)
    mydata <- scale(df_exams[,6:8])
    km.res<-kmeans(mydata,input$clusters,nstart=25)
    cluster_plot <- fviz_cluster(km.res, data=mydata, palette="jco",ggtheme=theme_minimal())
    print(cluster_plot)
  })
} #close server


shinyApp(ui = ui, server = server)

