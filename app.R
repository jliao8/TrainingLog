install.packages("pacman")
library(pacman)
p_load(RSQLite,shiny,bslib,ggplot2,tidyr,plotly)
library(RSQLite)
library(shiny)
library(bslib)
library(ggplot2)
library(tidyr)
library(plotly)

#system("python3 ProcessTrainingLog.py") # update database
connection <- dbConnect(RSQLite::SQLite(), "~/TrainingLog/traininglog.db")
dbListTables(connection)
getLifts <- "SELECT * FROM lifts"
getMeta <- "SELECT * FROM metadata"
liftDf <- dbGetQuery(connection, getLifts)
metaDf <- dbGetQuery(connection, getMeta)
dbDisconnect(connection)

plotTime <- function(liftName){
  df <- liftDf[liftDf$Lift==liftName,]
  df <- pivot_longer(df, cols=c(grep("Set",names(df),value=TRUE)),names_to=c(".value","Set"),names_pattern="(^[A-Za-z]+)\\s([A-Za-z]+\\s\\d+$)",values_drop_na=TRUE)
  df$Date <- as.Date(df$Date,format="%Y-%m-%d")
  breakInterval <- as.double(difftime(max(df$Date),min(df$Date),units="days"))/5 # https://stackoverflow.com/a/29785779
  dateFormat <- if (breakInterval < 30) {"%Y-%d"} else {"%Y-%b"} # split into months
  if (!breakInterval) {breakInterval <- 1} # one observation
  plot <- ggplot(df, aes(x=Date,y=Weight,color=factor(Reps),shape=factor(`Lift Order`),Reps=Reps,`Lift Order`=`Lift Order`)) + geom_point() + geom_line() +
          labs(x="Date",y="Weight (Lbs)",shape="",color="(Reps, Order)") + scale_x_date(date_breaks=paste0(breakInterval," days"), date_labels=dateFormat) + facet_wrap(~Set,scales="free") + theme(text=element_text(size=15))
  ggplotly(plot,tooltip=c("Date","Weight","Reps","Lift Order"))
}

freqPlot <- function(id) {
  plotOutput(NS(id, "hist"),height="90vh")
}

freqInput <- function(id) {
  inputs <- list(selectizeInput(NS(id,"selectizePlace"),"Select Places:", choices=unique(metaDf$Place), multiple=TRUE),
                 input_switch(NS(id,"switchPlace"), "Select All"),
                 selectizeInput(NS(id,"selectizeOrder"),"Select Lift Orders:", choices=unique(liftDf$`Lift Order`), multiple=TRUE),
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
      if (input$switchOrder){updateSelectizeInput(session,"selectizeOrder",selected=unique(liftDf$`Lift Order`))} 
    })
    if (id=="Lift"){
      observeEvent(c(input$switchDay,input$selectizeDay),{
        if (input$switchDay){updateSelectizeInput(session,"selectizeDay",selected=unique(metaDf$Day))} 
      }) 
      data <- reactive({merge(liftDf[liftDf$"Lift Order" %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2],],
                              metaDf[metaDf$Day %in% input$selectizeDay & metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")})
      maxCount <- reactive({
        if (length(data()$Lift)){max(table(data()$Lift))} 
        else {100} # magic number (rid of warning)
      }) 
      # https://stackoverflow.com/a/9231857
      output$hist <- renderPlot({ggplot(data=data(),mapping=aes(x=reorder(Lift,Lift,function(x)-length(x)))) + geom_bar() + scale_y_continuous(breaks=seq(0,maxCount()+10,by=10),expand=c(0,0),limits=c(0,maxCount()+10)) + 
                                 labs(x="Lift",y="Count") + stat_count(geom="text", color="black", aes(label=after_stat(count)),vjust=-1) + theme(text=element_text(size=15))}) # https://stackoverflow.com/a/24199013
    } else if (id=="Set"){
      observeEvent(c(input$switchLift,input$selectizeLift,input$switchDay,input$selectizeDay),{
          if (input$switchLift){updateSelectizeInput(session,"selectizeLift",selected=unique(liftDf$Lift))} 
          if (input$switchDay){updateSelectizeInput(session,"selectizeDay",selected=unique(metaDf$Day))} 
      })
      setsData <- reactive({
        data <- merge(liftDf[liftDf$"Lift Order" %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2] & liftDf$Lift %in% input$selectizeLift,],
                       metaDf[metaDf$Day %in% input$selectizeDay & metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")
        data <- data[,grep("Weight Set",names(data),value=TRUE)] # https://stackoverflow.com/a/26813671
        colnames(data) <- gsub("(^[A-Za-z]+\\s[A-Za-z]+\\s)","",colnames(data)) # https://stackoverflow.com/a/69472194
        data
      })
      data <- reactive({
        stackData <- stack(setsData()) 
        stackData[!is.na(stackData$value),]
      })
      maxCount <- reactive({max(colSums(!is.na(setsData())))}) # https://stackoverflow.com/a/46106565
      output$hist <- renderPlot({ggplot(data=data(),mapping=aes(x=ind)) +  geom_bar() + scale_y_continuous(breaks=seq(0,maxCount()+10,by=10),expand=c(0,0),limits=c(0,maxCount()+10)) + labs(x="Set",y="Count") +
                                 stat_count(geom="text", color="black", aes(label=after_stat(count)),vjust=-1.5) + theme(text=element_text(size=15))})
    } else if (id=="Rep"){
      observeEvent(c(input$switchLift,input$selectizeLift,input$switchDay,input$selectizeDay),{
          if (input$switchLift){updateSelectizeInput(session,"selectizeLift",selected=unique(liftDf$Lift))} 
          if (input$switchDay){updateSelectizeInput(session,"selectizeDay",selected=unique(metaDf$Day))} 
      })
      allReps <- reactive({
        data <- merge(liftDf[liftDf$"Lift Order" %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2] & liftDf$Lift %in% input$selectizeLift,],
                       metaDf[metaDf$Day %in% input$selectizeDay & metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")
        as.vector(as.matrix(data[,grep("Reps Set",names(data),value=TRUE)])) #https://stackoverflow.com/a/20537229
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
                                 scale_y_continuous(breaks=seq(0,maxCount()+10,by=10),expand=c(0,0),limits = c(0,maxCount()+10)) + labs(x="Rep",y="Count") +
                                 stat_bin(binwidth=1, geom="text", color="black", aes(label=after_stat(count)),vjust=-1,na.rm=TRUE) + theme(text=element_text(size=15))})
    } else if (id=="Day"){
      observeEvent(c(input$switchLift,input$selectizeLift),{
        if (input$switchLift){updateSelectizeInput(session,"selectizeLift",selected=unique(liftDf$Lift))} 
      })
      data <- reactive({
        mergedData <- merge(liftDf[liftDf$"Lift Order" %in% input$selectizeOrder & input$dateRange[1] <= liftDf$Date & liftDf$Date <= input$dateRange[2] & liftDf$Lift %in% input$selectizeLift,],
                            metaDf[metaDf$Place %in% input$selectizePlace,],by.x="Location Id",by.y="Id")
        mergedData[!duplicated(mergedData[,"Location Id"]),] # no duplicate location id (only care about Day) https://stackoverflow.com/a/9945116
      })
      maxCount <- reactive({
        if (length(data()$Day)){max(table(data()$Day))} 
        else {100} # magic number (rid of warning)
      })
      output$hist <- renderPlot({ggplot(data=data(), aes(x=reorder(Day,Day,function(x)-length(x)))) + geom_bar() + scale_y_continuous(breaks=seq(0,maxCount()+5,by=5),expand=c(0,0),limits=c(0,maxCount()+5)) + 
                                 labs(x="Day",y="Count") + stat_count(geom="text", color="black", aes(label=after_stat(count)),vjust=-1) + theme(text=element_text(size=15))})
    }
  })
}

#https://rstudio.github.io/bslib/reference/navset.html
ui <- fluidPage(theme=bs_theme(version=5,bootswatch="darkly"), 
        titlePanel(h4("Lifting Dashboard", align="center")),
        navset_card_pill(height="100%",
          nav_panel("Weight Time Series",
            layout_sidebar(
              sidebar = sidebar(
                selectizeInput("selectizeLiftTime","Select Lifts:", choices=unique(liftDf$Lift), multiple=TRUE),
                input_switch("switchLiftTime", "Select All")
              ),
              mainPanel(width="100%",
                  uiOutput("timePlots")                   
              )
            )            
          ),
          nav_panel("Weight Relationship",
            layout_sidebar(
              sidebar=sidebar(
                selectInput("selectVar","Select Variable:", choices=c("Day","Set","Rep","Order"))
              ),
              mainPanel(width="100%",
                navset_card_tab(height="100%",
                  nav_panel("Boxplot",plotOutput("boxplot",height="90vh")),
                  nav_panel("Test",
                    accordion(
                      accordion_panel("Kruskal-Wallis",verbatimTextOutput("kruskalTest")) 
                    )
                  ) 
                )           
              )
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

server <- function(input, output,session) {
  output$timePlots <- renderUI({ # https://www.r-bloggers.com/2019/09/dynamic-ui-elements-in-shiny/
    req(input$selectizeLiftTime)
    tabs <- lapply(seq_along(input$selectizeLiftTime), function(i) {
      nav_panel(title=input$selectizeLiftTime[i],plotlyOutput(paste("plot",input$selectizeLiftTime[i]),height="90vh"))
    })
    do.call(navset_card_tab,tabs)
  })
  observeEvent(c(input$switchLiftTime,input$selectizeLiftTime),{
    if (input$switchLiftTime){updateSelectizeInput(session,"selectizeLiftTime",selected=unique(liftDf$Lift))}
    for (i in seq_along(input$selectizeLiftTime)) {
      local({ # https://stackoverflow.com/a/16746194
        current_i <- i
        output[[paste("plot",input$selectizeLiftTime[current_i])]] <- renderPlotly({ 
          plotTime(input$selectizeLiftTime[current_i])
        })
      })
    }
  })
  observeEvent(input$selectVar,{
    df <- merge(liftDf,metaDf)
    df <- pivot_longer(df, cols=c(grep("Set",names(df),value=TRUE)),names_to=c(".value","Set"),names_pattern="(^[A-Za-z]+)\\s([A-Za-z]+\\s\\d+$)",values_drop_na=TRUE)
    variable <- input$selectVar
    if (variable == "Rep"){
      plot <- ggplot(df,aes(x=factor(Reps),y=Weight)) + geom_boxplot() + xlab("Rep") + theme(text=element_text(size=15))
      kruskal <- kruskal.test(Weight ~ Reps ,df)
    } else if (variable == "Day"){
      plot <- ggplot(df,aes(x=factor(Day),y=Weight)) + geom_boxplot() + xlab("Day") + theme(text=element_text(size=15))
      kruskal <- kruskal.test(Weight ~ Day ,df)
    } else if (variable == "Set"){
      plot <- ggplot(df,aes(x=factor(Set),y=Weight)) + geom_boxplot() + xlab("Set") + theme(text=element_text(size=15))
      kruskal <- kruskal.test(Weight ~ Set ,df)
    } else if (variable == "Order"){
      plot <- ggplot(df,aes(x=factor(`Lift Order`),y=Weight)) + geom_boxplot() + xlab("Order") + theme(text=element_text(size=15))
      kruskal <- kruskal.test(Weight ~ `Lift Order` ,df)
    }
    output$boxplot <- renderPlot({
      plot
    })
    output$kruskalTest <- renderPrint({
      kruskal
    })
  })
  freqServer("Lift")
  freqServer("Set")
  freqServer("Rep")
  freqServer("Day")
  
}
#runGadget(ui, server, viewer = dialogViewer("Dialog Title", width = 1920, height = 1080))
shinyApp(ui, server)
