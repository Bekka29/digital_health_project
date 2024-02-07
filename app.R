#Install packages---------------------------------------

pacman::p_load(
  shiny,         #Building web applications
  bslib,         #UI toolkit
  ggpubr,        #creating and customizing plots
  ggplot2,       #data visualization 
  
  )
# Import data--------------------------------------------
source("clean.R")


# Define UI for OCD application features------------------------
ui <- fluidPage(

    # Application title
    titlePanel( "Obsessive Compulsive Disorder Dashboard"),
    br(),
    p("Obsessive Compulsive disorder (OCD) is a neuropsychiatric disorder or mental health disorder characterized by uncontrollable and recurring thoughts, known as obsessions, and repetitive behaviours, referred to as compulsions. These behaviours are time-consuming and significantly disrupt daily life, as the thoughts are intrusive and induce anxiety. Common obsessions encompass fears of germs or contamination, concerns about causing harm, a desire for perfect order, and thoughts related to sex, religion, or harm. Compulsions manifest as ritualistic or repetitive actions, including excessive cleaning, handwashing, item arranging, repeated checking, counting, and praying. Engaging in these behaviours provides temporary relief from anxiety.[1]"),
    br(),
    p("Affecting approximately 1-2.5% of the population, with subthreshold forms observed in about a third, OCD is twice as prevalent as Schizophrenia and exhibits elevated levels of suicidality. Adult patients with OCD have a higher incidence of completed suicide attempts compared to the general population[2], resulting in a significant societal and healthcare burden. There is a treatment gap due to problems in diagnosing the disorder and the global public health burden of OCD is likely higher than what is known."),
    br(),
    p("The Yale-Brown Obsessive-Compulsive Scale (Y-BOCS) is the clinical “gold” standard for the level of severity of OCD. it measures obsession and compulsion severity separately[3,4]. The higher the score, the more severe the disorder."),



    # Sidebar with a selector for patients characteristics 
    sidebarLayout(
        sidebarPanel(
          # Creating selectors for ethnicity and age category
          selectInput("ethnicity", "Select Ethnicity", levels(clean_ocd_dataset$ethnicity)),
          
          selectInput("age_cat", "Select Age Category", levels(clean_ocd_dataset$age_cat))  
        ),
    
        
        # Show plots of the generated distribution
        mainPanel(
            
            navset_card_underline(
              title = "Visualizations",
              nav_panel("Gender Chart", titlePanel("Gender chart"), plotOutput("genderTable")),
              nav_panel("Age Table", titlePanel("Age Table"), plotOutput("ageTable")),
              nav_panel("Obsession Type", titlePanel("Obsession type"), plotOutput("obstypeTable")),
              nav_panel("Compulsion Type", titlePanel("Compulsion type"), plotOutput("compstypeTable")),
              nav_panel("Y-BOCS Scores", titlePanel("Y-BOCS Boxplot"), plotOutput("box_plot")),
              nav_panel("Obsession Severity", titlePanel("obsession severity"), plotOutput("obsTable")),
              nav_panel("Compulsion Severity", titlePanel("compulsion severity"), plotOutput("compTable"))
              ),
            
            p("The data available here gives an insight into the characteristics and distribution of Obsessive-Compulsive Disorder among adult patients."),
              tags$ul(
                tags$li(tags$b("obs_type"), " - the type of obsession (religious, harm-related, etc)"),
                tags$li(tags$b("comps_type"), " - the type of compulsion (ordering, checking, etc"),
                tags$li(tags$b("y_bocs_obs"), " - the patients score on YBOCS for obsession on a scale of 40"),
                tags$li(tags$b("y_bocs_comps"), " - the patients score on YBOCS for compulsion on a scale of 40"),
                tags$li(tags$b("age_cat"), " - the age categories of the patients (15-19, 20-24,.. 70+,"),
                tags$li(tags$b("obs_cat"), " - the severity of obsession based on the YBOCS score"),
                tags$li(tags$b("comp_cat"), " - the severity of compulsion based on the YBOCS score"),
              )
        ),
    ),
  #Reference section
  tags$b("References"),
    
  tags$ol(
    tags$li("https://doi.org/10.3389/fpsyt.2022.927184"),
    tags$li("https://doi.org/10.4103/psychiatry.IndianJPsychiatry_524_18"),
    tags$li("https://doi.org/10.3389/fpsyt.2010.00018"),
    tags$li("https://doi.org/10.1007/978-3-540-68706-1_1421"),
    
    
  )
)
        

  # Define server logic
server <- function(input, output) {
  
  # Creating a filtered data set
  filtered_data <- reactive({
      clean_ocd_dataset <- clean_ocd_dataset %>% filter(ethnicity == input$ethnicity &
                                                          age_cat == input$age_cat )
    })

  #Creating a bar chart for the filtered dataset by gender
  output$genderTable <- renderPlot({
      ocd_by_gender <- filtered_data() %>%
        group_by(gender) %>%
        tally()
       ggplot(ocd_by_gender, aes(x=gender, y=n )) + geom_col( fill = "darksalmon" , color = "brown") 
  
    })
 
  #Creating a bar chart for the obsession types   
   output$obstypeTable <- renderPlot({
      bardata <- filtered_data()  %>% 
         group_by(obs_type) %>% 
         tally()
      ggplot(bardata, aes(x = obs_type, y = n, fill = obs_type)) +
      geom_bar(stat = "identity", color = "white", position = "stack") +
      labs(title = "Obsession Type", fill = "Stack Color", y = "Count") +
      scale_fill_manual(values = c("lightblue", "darkblue", "purple", "orchid", "cornflowerblue")) 
      
    })
   
   #Creating a bar chart for the compulsion types   
   output$compstypeTable <- renderPlot({
     bardata <- filtered_data()  %>%
       group_by(comps_type) %>%
       tally()
     ggplot(bardata, aes(x = comps_type, y = n, fill = comps_type)) +
       geom_bar(stat = "identity", color = "white", position = "stack") +
       labs(title = "Compulsion Type", fill = "Stack Color", y = "Count") +
       scale_fill_manual(values = c("darkslateblue", "darkorchid", "deeppink", "darkturquoise", "darkcyan"))

    })

   
  #Creating a table that shows the age distribution by age category and gender
  output$ageTable <- renderPlot({
    
     age_table <- filtered_data() %>%
       tabyl(age_cat, gender) %>%
       adorn_totals(where = "both") %>%
       adorn_percentages(denominator = "col") %>%
       adorn_pct_formatting()  %>%
       adorn_ns(position = "front") %>%
       adorn_title(
         row_name = "Age Category",
         col_name = "Gender")

      ggtexttable(age_table)

     })
  
  #Creating a box plot of the Y-BOCS Scores
  output$box_plot <- renderPlot({
    data_long <- filtered_data() %>%
      select(y_bocs_obs, y_bocs_comps) %>%
      gather(key = "Variable", value = "Value")
    ggplot(data_long, aes(x = Variable, y = Value)) + geom_boxplot(fill = "lightblue", color = "blue") +
      labs(title = "Y-BOCS for Obsession and Compulsion", x = "Variable", y = "Y-BOCS Scores") + theme_minimal()
    })
    
  #Creating a bar chart for the obsession categories   
    output$obsTable <- renderPlot({
      obsdata <- filtered_data()  %>% 
        group_by(obs_cat) %>% 
        tally()
      ggplot(obsdata, aes(x=obs_cat, y=n )) + geom_col( fill = "bisque" , color = "burlywood") +
        labs(title = "Obsession severity categories", y = "count")
      
    })
    
  #Creating a bar chart for the compulsion categories   
    output$compTable <- renderPlot({
      compdata <- filtered_data()  %>% 
        group_by(comp_cat) %>% 
        tally()
        palette <- c("lightblue", "steelblue", "dodgerblue", "royalblue")
      ggplot(compdata, aes(x = "", y = n, fill = comp_cat)) +
        geom_bar(stat = "identity", width = 1, color = "white") +
        labs(title = "Compulsion severity categories", fill = "Compulsion Category", y = "Count") +
        theme_minimal() + coord_flip() + scale_fill_manual(values = palette )
     
    })
    

}


# Run the application 
shinyApp(ui = ui, server = server)
