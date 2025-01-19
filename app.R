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
  by(liftDf[,grep("^Set ",names(liftDf),value=TRUE)],liftDf$Lift, FUN= function(x) max(x, na.rm=TRUE))
}

# relationship between (place-lift) and (order-weight/reps)

freqPlot <- function(id) {
  plotOutput(NS(id, "hist"),height="90vh")
}

freqInput <- function(id) {
  inputs <- list(selectizeInput(NS(id,"selectizePlace"),"Select Places:", choices=unique(metaDf$Place), multiple=TRUE),
                 input_switch(NS(id,"switchPlace"), "Select All"),
                 selectizeInput(NS(id,"selectizeOrder"),"Select Exercise Orders:", choices=unique(liftDf$`Order Num`), multiple=TRUE),
                 input_switch(NS(id,"switchOrder"), "Select All"),
                 dateRangeInput(NS(id,"dateRange"),"Date Range:",start=min(liftDf$Date), end=max(liftDf$Date)))
  selectDay <- list(selectizeInput(NS(id,"selectizeDay"),"Select Days:", choices=list("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"), multiple=TRUE),
                    input_switch(NS(id,"switchDay"), "Select All"))
  selectLift <- list(selectizeInput(NS(id,"selectizeLift"),"Select Lifts:", choices=unique(liftDf$Lift), multiple=TRUE),
                     input_switch(NS(id,"switchLift"), "Select All"))
  if (id=="Lift"){
    inputs <- c(selectDay,inputs)
  } else if (id=="Day"){
    inputs <- c(selectLift,inputs)
  } else {
    inputs <- c(selectLift,selectDay,inputs)
  }
  tagList(inputs)
}

freqServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(c(input$switchPlace,input$selectizePlace,input$switchOrder,input$selectizeOrder),{
      if (input$switchPlace){updateSelectizeInput(session,"selectizePlace",selected=unique(metaDf$Place))} 
      if (input$switchOrder){updateSelectizeInput(session,"selectizeOrder",selected=unique(liftDf$`Order Num`))} 
    })
    if (id=="Lift"){
      observeEvent(c(input$switchDay,input$selectizeDay),{
        if (input$switchDay){updateSelectizeInput(session,"selectizeDay",selected=unique(metaDf$Day))} 
      }) 
      data <- reactive({merge(liftDf[liftDf$'Order Num' %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2],],
                              metaDf[metaDf$Day %in% input$selectizeDay & metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")})
      maxCount <- reactive({
        if (length(data()$Lift)){max(table(data()$Lift))} 
        else {100} # magic number (rid of warning)
      }) 
      # https://stackoverflow.com/a/9231857
      output$hist <- renderPlot({ggplot(data=data(),mapping=aes(x=reorder(Lift,Lift,function(x)-length(x)))) + geom_bar() + scale_y_continuous(breaks=seq(0,maxCount()+10,by=10),expand=c(0,0),limits=c(0,maxCount()+10)) + 
                                 xlab("Lift") + ylab("Count") + theme(text=element_text(size=15))})
    } else if (id=="Set"){
      observeEvent(c(input$switchLift,input$selectizeLift,input$switchDay,input$selectizeDay),{
          if (input$switchLift){updateSelectizeInput(session,"selectizeLift",selected=unique(liftDf$Lift))} 
          if (input$switchDay){updateSelectizeInput(session,"selectizeDay",selected=unique(metaDf$Day))} 
      })
      setsData <- reactive({
        data <- merge(liftDf[liftDf$'Order Num' %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2] & liftDf$Lift %in% input$selectizeLift,],
                       metaDf[metaDf$Day %in% input$selectizeDay & metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")
        data[,grep("^Set ",names(data),value=TRUE)] # https://stackoverflow.com/a/26813671
      })
      data <- reactive({
        stackData <- stack(setsData()) 
        stackData[!is.na(stackData$value),]
      })
      maxCount <- reactive({max(colSums(!is.na(setsData())))}) # https://stackoverflow.com/a/46106565
      output$hist <- renderPlot({ggplot(data=data(),mapping=aes(x=ind)) +  geom_bar() + scale_y_continuous(breaks=seq(0,maxCount()+10,by=10),expand=c(0,0),limits=c(0,maxCount()+10)) + xlab("Set") + ylab("Count") +
                                 theme(text=element_text(size=15))})
    } else if (id=="Rep"){
      observeEvent(c(input$switchLift,input$selectizeLift,input$switchDay,input$selectizeDay),{
          if (input$switchLift){updateSelectizeInput(session,"selectizeLift",selected=unique(liftDf$Lift))} 
          if (input$switchDay){updateSelectizeInput(session,"selectizeDay",selected=unique(metaDf$Day))} 
      })
      allReps <- reactive({
        data <- merge(liftDf[liftDf$'Order Num' %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2] & liftDf$Lift %in% input$selectizeLift,],
                       metaDf[metaDf$Day %in% input$selectizeDay & metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")
        as.vector(as.matrix(data[,grep("Reps Set ",names(data),value=TRUE)])) #https://stackoverflow.com/a/20537229
      }) 
      maxRep <- reactive({
        if (length(allReps())){max(allReps(),na.rm=TRUE)} 
        else {20} # magic number (rid of warning) 
      })
      maxCount <- reactive({
        if (length(allReps())){max(table(allReps()))} 
        else {500} # magic number (rid of warning)
      })
      output$hist <- renderPlot({ggplot(mapping=aes(allReps())) + geom_histogram(na.rm=T,binwidth=1) + scale_x_continuous(breaks=seq(1,maxRep(),by=1)) + 
                                 scale_y_continuous(breaks=seq(0,maxCount()+10,by=10),expand=c(0,0),limits = c(0,maxCount()+10)) + xlab("Rep") + ylab("Count") + theme(text=element_text(size=15))})
    } else if (id=="Day"){
      observeEvent(c(input$switchLift,input$selectizeLift),{
        if (input$switchLift){updateSelectizeInput(session,"selectizeLift",selected=unique(liftDf$Lift))} 
      })
      data <- reactive({
        mergedData <- merge(liftDf[liftDf$'Order Num' %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2] & liftDf$Lift %in% input$selectizeLift,],
                            metaDf[metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")
        mergedData[!duplicated(mergedData[,"Location Id"]),] # no duplicate location id (only care about Day) https://stackoverflow.com/a/9945116
      })
      maxCount <- reactive({
        if (length(data()$Day)){max(table(data()$Day))} 
        else {100} # magic number (rid of warning)
      })
      output$hist <- renderPlot({ggplot(data=data(), aes(x=reorder(Day,Day,function(x)-length(x)))) + geom_bar() + scale_y_continuous(breaks=seq(0,maxCount()+5,by=5),expand=c(0,0),limits=c(0,maxCount()+5)) + 
                                 xlab("Rep") + ylab("Count") + theme(text=element_text(size=15))})
    }
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

