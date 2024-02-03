

pacman::p_load(
  shiny, 
  bslib,
  )

source("scripts/clean.R")

glimpse(ocd_dataset)      

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Obsessive Compulsive Disorder Patient Data"),

    # Sidebar with a selector for patients characteristics 
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
          
              ),
            p("The data available here gives an insight into the characteristics and distribution of Obsessive_Compulsive Disorder among adult patients."),
            tags$ul(
              tags$li(tags$b("family_hist"), " - the patient has or does not have a family history of OCD"),
              tags$li(tags$b("obs_type"), " - the type of obsession (religious, harm-related, etc)"),
              tags$li(tags$b("comp_type"), " - the type of compulsion (ordering, checking, etc"),
              tags$li(tags$b("y_bocs_obs"), " - the patients score on YBOCS for obsession on a scale of 40"),
              tags$li(tags$b("y_bocs_obs"), " - the patients score on YBOCS for compulsion on a scale of 40"),
              tags$li(tags$b("age_cat"), " - the age categories of the patients (15-19, 20-24,.. 70+,"),
              tags$li(tags$b("depr_diag"), " - the presence or absence of depression as a comorbidity"),
              tags$li(tags$b("anx_diag"), " - the presence or absence of anxiety as a comorbidity"),
              tags$li(tags$b("obs_cat"), " - the severity of obsession based on the YBOCS score"),
              tags$li(tags$b("comp_cat"), " - the severity of compulsion based on the YBOCS score"),
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
