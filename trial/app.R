

pacman::p_load(
  shiny, 
  bslib,
  ggplot2
  )

source("scripts/clean.R")

#glimpse(clean_ocd_dataset)      

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Obsessive Compulsive Disorder Regional Dashboard"),

    # Sidebar with a selector for patients characteristics 
    sidebarLayout(
        sidebarPanel(
          #selector for ethnicity
          selectInput("ethnicity", "Select Ethnicity", levels(clean_ocd_dataset$ethnicity)),
          
          selectInput("age_cat", "Select Age Category", levels(clean_ocd_dataset$age_cat))  
        ),
    
        
    # Show a plot of the generated distribution
        mainPanel(
            
            navset_card_underline(
              title = "Visualizations",
              nav_panel("Gender Chart", titlePanel("Gender chart"), plotOutput("genderTable")),
              nav_panel("Age Table", titlePanel("Age Table"), plotOutput("ageTable")),
              nav_panel("Age Category Chart", titlePanel("Age category"), plotOutput("agecatTable")),
              nav_panel("Y-BOCS Plot", titlePanel("Y-BOCS Boxplot"), plotOutput("box_plot"))
              
              ),
            
            p("The data available here gives an insight into the characteristics and distribution of Obsessive_Compulsive Disorder among adult patients."),
            tags$ul(
              tags$li(tags$b("family_hist"), " - the presence or absence of a family history of OCD"),
              tags$li(tags$b("obs_type"), " - the type of obsession (religious, harm-related, etc)"),
              tags$li(tags$b("comps_type"), " - the type of compulsion (ordering, checking, etc"),
              tags$li(tags$b("y_bocs_obs"), " - the patients score on YBOCS for obsession on a scale of 40"),
              tags$li(tags$b("y_bocs_comps"), " - the patients score on YBOCS for compulsion on a scale of 40"),
              tags$li(tags$b("age_cat"), " - the age categories of the patients (15-19, 20-24,.. 70+,"),
              tags$li(tags$b("depr_diag"), " - the presence or absence of depression as a comorbidity"),
              tags$li(tags$b("anx_diag"), " - the presence or absence of anxiety as a comorbidity"),
              tags$li(tags$b("obs_cat"), " - the severity of obsession based on the YBOCS score"),
              tags$li(tags$b("comp_cat"), " - the severity of compulsion based on the YBOCS score"),
            )
          ),))
        
      
   

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Creating a filtered dataset
  filtered_data <- reactive({
      clean_ocd_dataset <- clean_ocd_dataset %>% filter(ethnicity == input$ethnicity &
                                                          age_cat == input$age_cat )
    })

  #Creating a barchart for the filtered dataset by gender
  output$genderTable <- renderPlot({
     ocd_by_gender <- filtered_data() %>%
        group_by(gender) %>%
        tally()
      ggplot(ocd_by_gender, aes(x=gender, y=n )) + geom_col() 
    })
 
  #Creating a barchart for the age categories   
  output$agecatTable <- renderPlot({
      bardata <- filtered_data()  %>% 
        group_by(age_cat) %>% 
        tally()
      barplot(height = bardata$n, names = bardata$age_cat )
    })
  
  #Creating a table that shows the age distribution by gender
  output$ageTable <- renderPlot({
     #filtered_data <- clean_ocd_dataset %>% filter(ethnicity == input$ethnicity)
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
  
  #Creating a boxplot of the filtered data based on Y-BOCS Scores
  output$box_plot <- renderPlot({
      y_bocs_obs <- clean_ocd_dataset$y_bocs_obs
      y_bocs_comps <- clean_ocd_dataset$y_bocs_comps
        boxplot(y_bocs_obs, y_bocs_comps,
              main = "Y-BOCS for Obsession and Compulsion",
              at = c(1, 4),
              names = c("y_bocs_obs", "y_bocs_comps"),
              las = 2,
              col = c("orange", "red"),
              border = "brown",
              horizontal = TRUE)
  })
  
  

}


# Run the application 
shinyApp(ui = ui, server = server)
