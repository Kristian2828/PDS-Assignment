library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(ggcorrplot)
library(correlation)

## Read files
covid = read.csv("covid_data.csv", sep = ';')
eco_data <- read.csv("Economic_Data.csv")
covid$Date = as.Date(covid$Date)
covid$Country = as.factor(covid$Country)
vaccinedata <- read.csv("VaccineData.csv", header = TRUE, sep = ",")
vaccinedata$Date <- strptime(as.character(vaccinedata$Date),format="%d/%m/%Y")
vaccinedata$Date <- as.POSIXct(vaccinedata$Date)
vaccinedata$Date <- as.Date(vaccinedata$Date)
vaccinedata$Entity <- as.factor(vaccinedata$Entity)




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
        sidebarPanel(h4(strong("Coronavirus disease (COVID-19)")," is an infectious disease caused by the SARS-CoV-2 virus.

Most people infected with the virus will experience mild to moderate respiratory illness and recover without requiring special treatment. However, some will become seriously ill and require medical attention. Older people and those with underlying medical conditions like cardiovascular disease, diabetes, chronic respiratory disease, or cancer are more likely to develop serious illness. Anyone can get sick with COVID-19 and become seriously ill or die at any age. 

The best way to prevent and slow down transmission is to be well informed about the disease and how the virus spreads. Protect yourself and others from infection by staying at least 1 metre apart from others, wearing a properly fitted mask, and washing your hands or using an alcohol-based rub frequently. Get vaccinated when it’s your turn and follow local guidance.

The virus can spread from an infected person’s mouth or nose in small liquid particles when they cough, sneeze, speak, sing or breathe. These particles range from larger respiratory droplets to smaller aerosols. It is important to practice respiratory etiquette, for example by coughing into a flexed elbow, and to stay home and self-isolate until you recover if you feel unwell"), br(), em("Data is retrieved from www.who.com"), align="justified"),
        
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
          downloadButton(outputId = "download_data", label = "Download"),br(),br(), em("Data is retrieved from www.worldometers.info"),
    ),
      mainPanel(
        plotlyOutput(outputId = "plot"), br(),
        strong(em("COVID-19 CASES IN SOUTH EAST ASIA")),
        br(), br(), br(),
        DT::dataTableOutput(outputId = "table")
    )
  )),
  
tabPanel("Vaccinnation", value = "Vaccination",
         sidebarLayout(
           
           sidebarPanel(
             
             h2("COVID-19 Vaccinations in SEA"),
             
            
             selectInput(inputId = "country", label = "Please choose your country.",
                         choices = levels(vaccinedata$Entity)),
            
             selectInput(inputId = "category", label = "Please choose your category.",
                         choices = c("Partially_vaccinated", "Fully_vaccinated")),
             
             
             dateRangeInput(inputId = "date", label = "Please specify your date range.",
                            start = min(vaccinedata$Date),
                            end   = max(vaccinedata$Date)),
             
          
             checkboxInput(inputId = "summarybox",
                           label = strong("Show Summary"),
                           value = FALSE),
             
             verbatimTextOutput("stats"),
             
          
             
             downloadButton(outputId = "download_data_vaccine", label = "Download"),br(),br(), em("Data is retrieved from www.worldometers.info"),
             
             #helpText(em("Data is retrieved from www.worldometers.info")
           ),
           
           mainPanel(
             
             tabsetPanel(type = "tabs",
                         tabPanel(strong("Vaccination Plot"), br(), br(), plotlyOutput(outputId = "Agraph"),
                                  br(),br(),
                                  plotlyOutput(outputId = "Tgraph")),
                         tabPanel("Vaccination Table", strong(em("COVID-19 VACCINATIONS IN SOUTH EAST ASIA")),br(), 
                                  DT::dataTableOutput("table_vaccine")),
                         tabPanel("About Vaccine", strong(h3("The Immune System - the Body's Defense Against Infection")),br(),
                                  "To understand how COVID-19 vaccines work, it helps to first look at how our bodies fight illness. When germs, such as the virus that causes COVID-19, invade our bodies, 
                              they attack and multiply. This invasion, called an infection, is what causes illness. 
                              Our immune system uses several tools to fight infection. Blood contains red cells, 
                              which carry oxygen to tissues and organs, and white or immune cells, 
                              which fight infection.", br(), "Different types of white blood cells fight infection in different ways:",br(),
                                  "1.  Macrophages are white blood cells that swallow up and digest germs and dead or dying cells. The macrophages leave behind parts of the invading germs, called 'antigens'. 
                              The body identifies antigens as dangerous and stimulates antibodies to attack them.", br(),
                                  "2.  B-lymphocytes are defensive white blood cells. They produce antibodies that attack
                              the pieces of the virus left behind by the macrophages.", br(),
                                  "3.  T-lymphocytes are another type of defensive white blood cell. They attack cells in the body that have already been 
                               infected.", br(),
                                  ("The first time a person is infected with the virus that causes COVID-19, it can take several days or weeks for their body to make and use all the germ-fighting tools needed 
                              to get over the infection. After the infection, the person's immune system remembers what it learned about how to protect the body against that disease. The body keeps a few 
                              T-lymphocytes, called 'memory cells', that go into action quickly if the body encounters the same virus again. When the familiar antigens are detected, B-lymphocytes produce 
                              antibodies to attack them. Experts are still learning how long these memory cells protect a person against the virus that causes COVID-19."),br(),
                                  h3("Understanding How COVID-19 Vaccines Work"), br(),
                                  ("Different types of vaccines work in different ways to offer protection. But with all types of vaccines, the body is left with a supply of 'memory' 
                              T-lymphocytes as well as B-lymphocytes that will remember how to fight that virus in the future. It typically takes a few weeks after vaccination for the body to produce 
                              T-lymphocytes and B-lymphocytes. Therefore, it is possible that a person could be infected with the virus that causes COVID-19 just before or just after vaccination and then 
                              get sick because the vaccine did not have enough time to provide protection."),
                                  ("It typically takes a few weeks after vaccination for the body to produce T-lymphocytes and B-lymphocytes. Therefore, it is possible that a person could be infected with the 
                              virus that causes COVID-19 just before or just after vaccination and then get sick because the vaccine did not have enough time to provide protection."),
                                  ("Sometimes after vaccination, the process of building immunity can cause symptoms, such as fever. These symptoms are normal and are signs that the body is building immunity. 
                              Talk to a doctor about taking over-the-counter medicine, such as ibuprofen, acetaminophen, aspirin (only for people age 18 or older), or antihistamines for any pain and discomfort 
                              experienced after getting vaccinated."))
             )
           )
         )
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
             sidebarPanel(strong(h4("PREPARED BY"), br(),strong("1. Kristian Surya Dinata | S2043845", br(), "2. Muhammad Ezlan Bin Zakhir | S2116731", br(), "3. Muhammad Amirul Daniel bin Badrul Hisham | S2115750", br(), "4. Eva Chin Ying Ming | S2115794", br(), "5. Law Shu Meei | S2117388"))),
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

  
  
  
  ###########################################################################################
  
  filtered_data <- reactive({
    subset(covid,
           Country %in% input$Country &
             Date >= input$date[1] & Date <= input$date[2])})
  
  filterdata <- reactive({
    subset(vaccinedata,
           Entity %in% input$country &
             Date >= input$date[1] & Date <= input$date[2])
  })
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(filtered_data(), aes_string(x="Date", y=input$dv, color="Country")) +
        geom_point(alpha=0.5) + theme(legend.position = "none") +
        ylab("Total Number")
      
      p
    })
  })
  
  output$Agraph <- renderPlotly({
    
    ggplotly({
      p <- ggplot(filterdata(), aes_string(x="Date", y= input$category, color="Entity")) +
        geom_point(alpha=0.5) + theme(legend.position = "none") + 
        ggtitle(paste("Daily Vaccination in",input$country, sep = " ")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        ylab("Number of Vaccines2")
      
      p
      
    })
    # }
  })
  
  
  output$Tgraph <- renderPlotly({
    
    if (input$category == "Partially_vaccinated"){
      p <- plot_ly(filterdata(), x=~Date, y=~people_vaccinated_per_hundred, type = "bar") %>%
        layout(xaxis = list(showticklabels = TRUE), title = "Percentage of Partial Vaccination")
      
      output$stats <- renderPrint({
        if (input$summarybox == TRUE) {
          s <- summary(filterdata()$Partially_vaccinated)
          
          s
        }
      })
      
      p
      
    }else if (input$category == "Fully_vaccinated"){
      p <- plot_ly(filterdata(), x=~Date, y=~people_fully_vaccinated_per_hundred, type = "bar") %>%
        layout(xaxis = list(showticklabels = TRUE), title = "Percentage of Full Vaccination")
      
      output$stats <- renderPrint({
        if (input$summarybox == TRUE) {
          s <- summary(filterdata()$Fully_vaccinated)
          
          s
        }
      })
      
      p
    }
  })
  
  output$table <- DT::renderDataTable({
    filtered_data()
  })
  
  output$table_vaccine <- DT::renderDataTable(DT::datatable({
    filterdata()
  }))
  
  
  output$download_data_vaccine <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    
    content = function(file) {
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  
  
}









shinyApp(ui = ui, server = server)
