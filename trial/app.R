

pacman::p_load(
  shiny, 
  bslib,
  )

source("scripts/clean.R")

glimpse(ocd_dataset)      

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput("ethnicity", "Ethnicity", levels(ocd_dataset$ethnicity))  
          ),

        # Show a plot of the generated distribution
        mainPanel(
          navset_card_underline(
              title = "Visualizations",
              nav_panel("Plot", titlePanel("gender Table"), plotOutput("genderTable")),
              nav_panel("Summary", titlePanel("age Table"), plotOutput("ageTable")),
              nav_panel("Table", titlePanel("Test2"))
          
              )
           )
        )
      )
   

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    
  
  output$genderTable <- renderPlot({
      filtered_data <- ocd_dataset %>% filter(ethnicity == input$ethnicity)
      ocd_by_gender <- filtered_data %>%
        group_by(gender) %>%
        tally()
      ggplot(ocd_by_gender, aes(x=gender, y=n))+ geom_col()
    })
    
    
  output$ageTable <- renderPlot({
    filtered_data <- ocd_dataset %>% filter(ethnicity == input$ethnicity)
    age_table <- filtered_data %>%
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
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
