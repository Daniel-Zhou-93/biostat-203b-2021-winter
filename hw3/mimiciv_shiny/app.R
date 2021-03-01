library("shiny")

mimic4 <- readRDS("icu_cohort.rds")

#TODO: select one or more variables to display (dropdown menu to list of "tags"?)
# (Can delete if feel necessary to do so)

choices <- c("Length of ICU stay",
             "Hospital admissions",
             "Patients")

ui <- fluidPage(
  titlePanel("MIMIC-IV"),

  sidebarLayout(
    sidebarPanel(
      helpText("Overview of the following variables:"),
# dropdown menu with length of ICU stay (histograms), 
# admissions (see hw2 graphs), patients (gender, anchor_age)
      selectInput("stats",
                  label="Choose a variable to analyze",
                  choices=choices,
                  selected=choices[1]),
      #TODO: add other stratification variables (e.g. race)
      checkboxInput("hist","Separate histograms?"),
      sliderInput("bins",
              "Number of bins:",
              min = 1,
              max = 50,
              value = 30)
# TODO: lab measurements? and vitals? Enter a list of items. Let auto complete take care of it?
    ),
    mainPanel(
      textOutput("n_missing"),
      tabsetPanel(type="tabs",
                  tabPanel("Histogram",plotOutput("histplot")),
                  tabPanel("Box Plot",plotOutput("boxplot"))
      )
    )
  )
)


server <- function(input,output){
  #TODO: display histograms with 30 bins, box-whisker (applicable to one or more variables)
#  dataInput <- reactive({
#    getSymbols(input$stats,
#               auto.assign = TRUE)
#  })
  
  # basic render. Deal with individually customized stuff later
  output$n_missing <- renderText({
    if (input$stats == choices[1]) {
      paste("Number missing:",sum(is.na(mimic4$los)))
    } else if (input$stats == choices[2]) {
      paste("Number missing:",sum(is.na(mimic4$admittime)))
    } else if (input$stats == choices[3]) {
      paste("Number missing:",sum(is.na(mimic4$anchor_age)))
    }
  })
  output$histplot <- renderPlot({
    if (input$stats == choices[1]) {
      x <- mimic4$los
    } else if (input$stats == choices[2]) {
      x <- mimic4$admittime
    } else if (input$stats == choices[3]) {
      x <- mimic4$anchor_age
    }

    # generate bins based on input$bins
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray')#, border = 'white')
  })
  
  output$boxplot <- renderPlot({
    if (input$stats == choices[1]) {
      x <- mimic4$los
    } else if (input$stats == choices[2]) {
      x <- mimic4$admittime
    } else if (input$stats == choices[3]) {
      x <- mimic4$anchor_age
    }
    
    # draw the boxplot
    boxplot(x)
  })
}


shinyApp(ui,server) 
