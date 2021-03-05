library("shiny")
library("tidyverse")
library("gtsummary")

mimic4 <- readRDS("icu_cohort.rds")
#mimic4[,"min_admittime"]

# (dropdown menu to list of "tags"?)
# (Can delete if feel necessary to do so)

choices <- c("Length of ICU stay",
             "Hospital admissions",
             "Patients")

itemlist <- colnames(mimic4)
itemlist <- itemlist[!itemlist %in% c("subject_id","hadm_id","stay_id",
                                   "intime","outtime","admittime","dischtime",
                                   "deathtime","anchor_year","dod")]
#NOTE: "dod" has no value
discrete_vars <- c("admission_type","admission_location","discharge_location",
                   "hospital_expire_flag","death30","language","insurance",
                   "ethnicity","first_careunit","last_careunit","gender",
                   "marital_status","admission_type","anchor_year_group")
bin_discrete_vars <- c("hospital_expire_flag","death30","insurance")
mimic4$hospital_expire_flag <- factor(mimic4$hospital_expire_flag)

# demographics, lab measurements, vitals
ui <- fluidPage(
  titlePanel("MIMIC-IV"),

  sidebarLayout(
    sidebarPanel(
      # encode in a drop down list for display purposes
      helpText(paste("Select one of the following variables to analyze:")),
      selectInput("stats","Select the following variable to analyze",
                  itemlist),

      #TODO: if variable is positive, allow for log or inverse functions?
      #textInput("stats", "Measurement","glucose"),
      #actionButton("enter","Enter"),

      #TODO: filter by date range that includes all 
      # where the date range intersects
      dateRangeInput("dates", 
               "Admission Date Range",
               start = "2008-01-01", 
               end = as.character(Sys.Date())),

      radioButtons("discvars","Group by:",c("None",discrete_vars)),
      
      sliderInput("bins",
              "Bins:",
              min = 1,
              max = 50,
              value = 30)
    ),
    mainPanel(
      textOutput("n_missing"),
      tabsetPanel(type="tabs",
                  tabPanel("Histogram",plotOutput("histplot")),
                  tabPanel("Box Plot",plotOutput("boxplot")),
                  tabPanel("Table",tableOutput("summary_stats"))
      )
      # table output with basic stats, stratified if applicable
    )
  )
)


server <- function(input,output){
  # basic render. Deal with individually customized stuff later
  #re <- eventReactive(input$enter,{input$stats})
  output$n_missing <- renderText({
    #re()
    paste("Number missing:",sum(is.na(mimic4[,input$stats])))
  })
  output$histplot <- renderPlot({
    #re()
    var_list <- c(input$stats)
    if (input$discvars != "None") {
      var_list <- c(var_list,input$discvars)
    }
    # if one of the discrete vars is selected, histogram by group
    x <- mimic4 %>%
      select(var_list) %>%
      drop_na()
    if (nrow(x) == 0) {
      print("No values are present in this variable for analysis.")
    } else {
      # generate bins based on input$bins
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      if (input$stats %in% discrete_vars) {
        # stacked barplot
        if (input$discvars == "None") {
          x <- mimic4 %>%
            select(var_list) %>%
            drop_na() %>%
            group_by(!!sym(input$stats)) %>%
            summarize(count = n()) %>%
            ungroup()
          if (nrow(distinct(x)) > 5) {
            ggplot(x,aes_string(y="count",x=input$stats)) +
              geom_bar(position="stack",stat="identity") +
              theme(axis.text.x = element_text(angle=90))
          } else {
            ggplot(x,aes_string(y="count",x=input$stats)) +
              geom_bar(position="stack",stat="identity") +
              theme(axis.text.x = element_text(angle=90))
          }
        } else {
          # figure out how to get cols per grouped value
          x <- mimic4 %>%
            select(var_list) %>%
            drop_na() %>%
            group_by(!!sym(input$discvars)) %>%
            mutate(count=match(input$stats,
                               unique(input$stats))) %>%
            ungroup()
          ggplot(x,aes_string(fill=input$discvars,
                              y="count",x=input$stats)) +
            geom_bar(position="stack",stat="identity") +
            theme(axis.text.x = element_text(angle=90))
        }
      } else {
        if (input$discvars == "None") {
          x <- mimic4 %>%
            select(var_list) %>%
            drop_na()
          ggplot(x,aes_string(x=input$stats)) + 
            geom_histogram(color="darkgrey",
                           bins=input$bins,
                           fill="cyan")
        } else {
          x <- mimic4 %>%
            select(var_list) %>%
            drop_na()
          # stacked histogram
          ggplot(x,aes_string(x=input$stats,fill=input$discvars)) + 
            geom_histogram(color="darkgrey",
                           bins=input$bins)
        }
        #hist(x, breaks = bins, col = 'lightblue')
        #, border = 'white')
      }
    }
  })
  
  output$boxplot <- renderPlot({
    #re()
    # if one of the group vars is selected, boxplot by group
    var_list <- c(input$stats)
    if (input$discvars != "None") {
      var_list <- c(var_list,input$discvars)
    }
    if (input$stats %in% discrete_vars) {
      # draw the boxplot
      if (input$discvars == "None") {
        x <- mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          group_by(!!sym(input$stats)) %>%
          summarize(count = n()) %>%
          ungroup()
        ggplot(data=x,aes_string(x="count",y=input$stats)) + 
          geom_boxplot()
      } else {
        x <- mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          group_by(!!sym(input$discvars),!!sym(input$stats)) %>%
          summarize(count = n()) %>%
          ungroup()
        ggplot(data=x,aes_string(fill=input$discvars,y=input$stats)) + 
          geom_boxplot()
      }
    } else {
      # draw the boxplot
      if (input$discvars == "None") {
        x <- mimic4 %>%
          select(var_list) %>%
          drop_na()
        ggplot(data=x,aes_string(y=input$stats)) + 
          geom_boxplot()
      } else {
        x <- mimic4 %>%
          select(var_list) %>%
          drop_na()
        ggplot(data=x,aes_string(fill=input$discvars,y=input$stats)) + 
          geom_boxplot()
      }
    }
  })
  
  output$summary_stats <- renderTable({
    #re()
    if (input$stats %in% discrete_vars) {
      # add percentile breakdown for each distinct variable
      if (input$discvars == "None") {
        var_list <- c(input$stats)
        if (input$discvars != "None") {
          var_list <- c(var_list,input$discvars)
        }
        n_row <- mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          nrow()
        mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          group_by(!!sym(input$stats)) %>%
          summarize(count = n(),
                    percent = paste(round(n()/n_row*100,digits=2),
                                    "%",sep="")) %>%
          ungroup()
      } else {
        var_list <- c(input$stats)
        if (input$discvars != "None") {
          var_list <- c(var_list,input$discvars)
        }
        n_row <- mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          nrow()
        mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          group_by(!!sym(input$discvars),!!sym(input$stats)) %>%
          summarize(count = n(),
                    percent = paste(round(n()*100/n_row,digits=2),
                                    "%",sep="")) %>%
          ungroup()
      }
    } else {
      if (input$discvars == "None") {
        var_list <- c(input$stats)
        if (input$discvars != "None") {
          var_list <- c(var_list,input$discvars)
        }
        mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          summarize(count = n(),
                    mean = mean(!!sym(input$stats)),
                    median = median(!!sym(input$stats)),
                    IQR = IQR(!!sym(input$stats)),
                    min = min(!!sym(input$stats)),
                    max = max(!!sym(input$stats)))
      } else {
        var_list <- c(input$stats)
        if (input$discvars != "None") {
          var_list <- c(var_list,input$discvars)
        }
        mimic4 %>%
          select(var_list) %>%
          drop_na() %>%
          group_by(!!sym(input$discvars)) %>%
          summarize(count = n(),
                    mean = mean(!!sym(input$stats)),
                    median = median(!!sym(input$stats)),
                    IQR = IQR(!!sym(input$stats)),
                    min = min(!!sym(input$stats)),
                    max = max(!!sym(input$stats))) %>%
          ungroup()
      }
    }
  })
}


shinyApp(ui,server) 
