#install.packages("RSQLite")
#install.packages("shiny")
#install.packages("bslib")
#install.packages("ggplot2")
library("RSQLite")
library("shiny")
library("bslib")
library("ggplot2")

#system('python3 ProcessTrainingLog.py')
connection <- dbConnect(RSQLite::SQLite(), "~/TrainingLog/traininglog.db")
dbListTables(connection)
getLifts <- "SELECT * FROM lifts"
getMeta <- "SELECT * FROM metadata"
liftDf <- dbGetQuery(connection, getLifts)
metaDf <- dbGetQuery(connection, getMeta)
dbDisconnect(connection)

# max weight for all lifts (time series with highlight text box of metadata)
maxWeight <- function(){
  by(liftDf[,c("Set 1","Set 2","Set 3","Set 4")],liftDf$Lift, FUN= function(x) max(x, na.rm=TRUE))
}

# frequency of sets and reps (lift, day, location, date range, order)
allReps <- as.vector(as.matrix(liftDf[,c("Reps Set 1","Reps Set 2","Reps Set 3","Reps Set 4")])) #https://stackoverflow.com/a/20537229
hist(allReps)

# frequency of lifts (day, location, date range, order)
barplot(table(liftDf$Lift))

# frequency of days (lift, location, date range, order)
barplot(table(metaDf$Day))

# relationship between (place-lift) and (order-weight/reps)

freqPlot <- function(id) {
  plotOutput(NS(id, "hist"),height="85vh")
}

freqInput <- function(id) {
  inputs <- list(selectizeInput(NS(id,"selectizePlace"),"Select Places:", choices=unique(metaDf$Place), multiple=TRUE),
                 selectizeInput(NS(id,"selectizeOrder"),"Select Orders:", choices=unique(liftDf$`Order Num`), multiple=TRUE),
                 dateRangeInput(NS(id,"dateRange"),"Date Range:",start=min(liftDf$Date), end=max(liftDf$Date)))
  if (id=="Lift"){
    inputs <- c(inputs,list(selectizeInput(NS(id,"selectizeDay"),"Select Days:", choices=list("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), multiple=TRUE)))
  } else if (id=="Day"){
    inputs <- c(inputs,list(selectizeInput(NS(id,"selectizeLift"),"Select Lifts:", choices=unique(liftDf$Lift), multiple=TRUE)))
  } else {
    inputs <- c(inputs,list(selectizeInput(NS(id,"selectizeDay"),"Select Days:", choices=list("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), multiple=TRUE),
                            selectizeInput(NS(id,"selectizeLift"),"Select Lifts:", choices=unique(liftDf$Lift), multiple=TRUE)))
  }
  inputs
}

freqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data <- reactive(liftDf[liftDf$Lift %in% input$selectizeLift,])
    output$hist <- renderPlot({ggplot(data=data(), aes(x=Lift)) +geom_bar() + theme(text=element_text(size=15),axis.text.x=element_text(size=15))})
  })
}

#https://rstudio.github.io/bslib/reference/navset.html
ui <- fluidPage(theme=bs_theme(version=5,bootswatch="darkly"), 
        titlePanel(h4("Lifting Analysis", align="center")),
        navset_card_pill(height="100%",
          nav_panel("Weighted Time Series",
            layout_sidebar(#border=TRUE,border_color="white",
              sidebar = sidebar(
                selectizeInput("selectizeLift1","Select Lifts:", choices=unique(liftDf$Lift), multiple=TRUE)
              ),
              mainPanel("hello")
            )            
          ),
          nav_panel('Relationships',
            layout_sidebar(#border=TRUE, border_color="white",
              sidebar=sidebar(
                selectizeInput("selectizeLift2","Select Lifts:", choices=unique(liftDf$Lift), multiple=TRUE)
              ),
              mainPanel("hello")
            )
          ),
          nav_panel("Lift Frequency",
            layout_sidebar(
              sidebar=sidebar(freqInput("Lift")),
              mainPanel(freqPlot("Lift"),width="100%")
            )
          ),
          nav_panel("Set Frequency",
            layout_sidebar(
              sidebar=sidebar(freqInput("Set")),
              mainPanel(freqPlot("Set"),width="100%")
            )
          ),
          nav_panel("Rep Frequency",
            layout_sidebar(
              sidebar=sidebar(freqInput("Rep")),
              mainPanel(freqPlot("Rep"),width="100%")
            )
          ),
          nav_panel("Day Frequency",
            layout_sidebar(
              sidebar=sidebar(freqInput("Day")),
              mainPanel(freqPlot("Day"),width="100%")
            )
          )
        )
      )


server <- function(input, output) {
  freqServer("Lift")
  freqServer("Set")
  freqServer("Rep")
  freqServer("Day")
  
}
#runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1280, height = 1280))
shinyApp(ui, server)

