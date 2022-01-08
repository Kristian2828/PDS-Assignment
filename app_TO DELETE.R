library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(ggcorrplot)
library(correlation)

## Read files
covid = read.csv("covid_data.csv", sep = ';')
eco_data <- read.csv("Economic_Data.csv")

## Change/update the varaible structure
covid$Date = as.Date(covid$Date)
covid$Country = as.factor(covid$Country)
eco_data$Country = as.factor(eco_data$Country)
eco_data$Date = as.Date(eco_data$Date)

ui <- fluidPage(
  theme = shinytheme("cerulean"),
 
  
  navbarPage(
    title = "COVID-19",
    id = "nav",
    
    tabPanel("Introduction", value = "Introduction", tags$img(src = "coronavirus.jpg", height = "300", width = "1875", align="centre"),
      sidebarLayout(
        sidebarPanel(strong("Coronavirus disease (COVID-19)")," is an infectious disease caused by the SARS-CoV-2 virus.

Most people infected with the virus will experience mild to moderate respiratory illness and recover without requiring special treatment. However, some will become seriously ill and require medical attention. Older people and those with underlying medical conditions like cardiovascular disease, diabetes, chronic respiratory disease, or cancer are more likely to develop serious illness. Anyone can get sick with COVID-19 and become seriously ill or die at any age. 

The best way to prevent and slow down transmission is to be well informed about the disease and how the virus spreads. Protect yourself and others from infection by staying at least 1 metre apart from others, wearing a properly fitted mask, and washing your hands or using an alcohol-based rub frequently. Get vaccinated when it's your turn and follow local guidance.

The virus can spread from an infected person's mouth or nose in small liquid particles when they cough, sneeze, speak, sing or breathe. These particles range from larger respiratory droplets to smaller aerosols. It is important to practice respiratory etiquette, for example by coughing into a flexed elbow, and to stay home and self-isolate until you recover if you feel unwell. (WHO, 2021)", align="centre"),
        
        mainPanel(
          tabsetPanel(
            tabPanel(strong("Prevention"), strong("To prevent infection and to slow transmission of COVID-19, do the following:"), br(),br(),
"1. Get vaccinated when a vaccine is available to you.", br(),
"2. Stay at least 1 metre apart from others, even if they don't appear to be sick.", br(),
"3. Wear a properly fitted mask when physical distancing is not possible or when in poorly ventilated settings.", br(),
"4. Choose open, well-ventilated spaces over closed ones. Open a window if indoors.", br(),
"5. Wash your hands regularly with soap and water or clean them with alcohol-based hand rub.", br(),
"6. Cover your mouth and nose when coughing or sneezing.", br(),
"7. If you feel unwell, stay home and self-isolate until you recover."),
            
            tabPanel(strong("Symptoms"),strong(em("COVID-19")), "affects different people in different ways. Most infected people will develop mild to moderate illness and recover without hospitalization.",

strong(h5("Most common symptoms:")),

"1. Fever",br(),"2. Cough",br(),"3. Tiredness",br(),"4. Loss of taste or smell.",br(),"5. Less common symptoms:",br(), "6. Sore throat",br(),"7. Headache", "8. Aches and pains", br(),"9. Diarrhoea",
br(),"9. A rash on skin, or discolouration of fingers or toes", br(), "10. Red or irritated eyes.",
br(),
strong(h5("Serious symptoms:")),

"1. Difficulty breathing or shortness of breath
loss of speech or mobility, or confusion
chest pain.",br(),
"2. Seek immediate medical attention if you have serious symptoms.", br(), "3. Always call before visiting your doctor or health facility.", br(), "4. People with mild symptoms who are otherwise healthy should manage their symptoms at home.", br(),
"5. On average it takes 5-6 days from when someone is infected with the virus for symptoms to show, however it can take up to 14 days."),
            tabPanel(strong("Video"), HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/i0ZabxXmH4Y" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
            
          )
          
        ),
      )
      
    ),
    tabPanel("Cases", value = "Cases",
  
      sidebarLayout(
        sidebarPanel(
          h1("COVID-19 in South East Asia"),
          selectInput(inputId = "dv", label = "Category",
                      choices = c("Total_Cases","New_Cases", "Total_Death","New_Death"),
                      selected = "New_Cases"),
          selectInput(inputId = "Country", "Country(s)",
                      choices = levels(covid$Country),
                      multiple = TRUE,
                      selected = c("Malaysia")),
          dateRangeInput(inputId = "date", "Date range",
                         start = min(covid$Date),
                         end   = max(covid$Date)),
          downloadButton(outputId = "download_data", label = "Download"),
    ),
      mainPanel(
        plotlyOutput(outputId = "plot"), br(),
        strong(em("COVID-19 CASES IN SOUTH EAST ASIA")),
        br(), br(), br(),
        DT::dataTableOutput(outputId = "table")
    )
  )),
  
  tabPanel("Vaccinnation", value = "Vaccination",
),

  tabPanel("Economy", value = "Economy",
           
             sidebarLayout(
               sidebarPanel(
                 h1("ASEAN Economic Impact under Covid-19 Pandemic"),
                 selectInput(inputId = "eco_factor", label = "Category",
                             choices = c("GDP","Unemployment_Rate", "Index_Movement"),
                             selected = "New_Cases"),
                 dateRangeInput(inputId = "range", label = "Select a Timeframe:",
                                start = min(eco_data$Date),
                                end   = max(eco_data$Date)),
                 selectInput(inputId = "Country_eco", "Country(s)",
                             choices = eco_data$Country,
                             multiple = TRUE,
                             selected = c("Malaysia")),
                 downloadButton(outputId = "download_data_eco", label = "Download"),
               ),
               mainPanel(
                 plotlyOutput(outputId = "plot_eco"), br(),
                 strong(em("Economic Impact under Covid-19 Pandemic")),
                 br(), br(), br(),
                 DT::dataTableOutput(outputId = "table_eco")
               )
             )
),
    
     ##Tab Panel - Summary
    tabPanel("Summary", value = "Summary",
             sidebarLayout(
               sidebarPanel(
                 h1("Summary for Variables Statistical Analysis"),
                 selectInput(inputId = "Country_sum", "Country(s)",
                             choices = eco_data$Country,
                             multiple = TRUE,
                             selected = c("Malaysia", "Singapore", "Indonesia", "Philippines", "Thailand", "Vietnam")), 
                 downloadButton(outputId = "download_data_sum", label = "Download"),
               ),
               mainPanel(
                 plotlyOutput(outputId = "plot_sum"), br(),
                 strong(em("Correlation of Variables (per quarter)")),
                 br(), br(), br(),
                 verbatimTextOutput(outputId = "sum_view")
               )
             )),

  tabPanel("About us", value = "About us",
           sidebarLayout(
             sidebarPanel(strong(h4("PREPARED BY"), br(),strong("Kristian Alexander"))),
              mainPanel(""),
))))

server <- function(input, output) {
  filtered_data <- reactive({
    subset(covid,
           Country %in% input$Country &
             Date >= input$date[1] & Date <= input$date[2])})
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="Date", y=input$dv, color="Country")) +
        geom_point(alpha=0.5) + theme(legend.position = "none") +
        ylab("Total Number")
      
      p
    })
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #Economic
  ##Filter Data
  filtered_data_eco <- reactive({
    subset(eco_data,
           Country %in% input$Country_eco &
           Date >= as.Date(input$range[1]) & Date <=as.Date(input$range[2]))})
  
  ##Plot
  output$plot_eco <- renderPlotly({
    ggplotly({
    p <- ggplot(filtered_data_eco(),aes_string(x = "Date", y = input$eco_factor, color = "Country")) + 
      geom_point(alpha = 0.8) + theme(legend.position = "none") + geom_line(alpha=0.2)
    
    p
      })
    })
  
  ##Table
  output$table_eco <- DT::renderDataTable({
    filtered_data_eco()[1:7]
  })
  
  ##Download
  output$download_data_eco <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data_eco()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  #Summary
  ##Filter Data
  filtered_data_sum <- reactive({
    subset(eco_data,
           Country %in% input$Country_sum) 
             })
         
  ##Plot
  output$plot_sum <- renderPlotly({
    ggcorrplot(
    cor(filtered_data_sum()[5:9]), lab = TRUE, digits = 2
    )
  })

  
  ##Table (https://statsandr.com/blog/correlation-coefficient-and-correlation-test-in-r/#between-two-variables)
  output$sum_view <- renderPrint({
    summary(filtered_data_sum()[5:9])
    correlation::correlation(filtered_data_sum()[5:9],
                             include_factors = TRUE, method = "auto"
    )
  })


  ##Download
  output$download_data_sum <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data_sum()
      write.csv(data, file, row.names = FALSE)
    }
  )

  
}

shinyApp(ui = ui, server = server)
