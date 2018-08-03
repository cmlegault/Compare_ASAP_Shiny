#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages = c("shiny",       # interactive components
             "shinyBS",     # pop up help boxes
             "ggplot2",     # nice graphics
             "dplyr",       # data handling
             "tidyr")       # data handling

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#-------------------------------------------------------------------------
# Define UI for application that draws a histogram
ui <- navbarPage("Compare ASAP",
   
  tabPanel("ASAP Runs to Compare",
    sidebarLayout(
      sidebarPanel(
        fileInput("myfiles", 
                  label = "ASAP .rdat files",
                  multiple = TRUE,
                  accept=c(".RDAT"))
      ),
      mainPanel(
        tableOutput("filenames")
      )
    )
  ),
  
  tabPanel("Obj Fxn",
    sidebarLayout(
      sidebarPanel(
         selectInput("objfxn",
                     "Objective Function Component",
                      choices = list("total","catch.total","discard.total","index.fit.total","catch.age.comp","discards.age.comp","index.age.comp","sel.param.total","index.sel.param.total","q.year1","q.devs","Fmult.year1.total","Fmult.devs.total","N.year1","Recruit.devs","SR.steepness","SR.scaler","Fmult.Max.penalty","F.penalty"),
                      selected = "total") 
      ),
      mainPanel(
        plotOutput("lkPlot"), 
        tableOutput("lktable")
      )
    )
  ),
  
  tabPanel("Time series",
    sidebarLayout(
      sidebarPanel(
        selectInput("TimeSeries",
                    "Time Series",
                    choices = list("SSB", "Freport", "Recruits"),
                    selected = "SSB"),
        radioButtons("tsoneplot",
                    "Plot",
                    choices = list("One Plot", "Multipanel Plot"),
                    selected = "One Plot",
                    inline = TRUE),
        downloadButton("downloadData", "Download")
      ),
      mainPanel(
        plotOutput("timeseriesPlot"),
        dataTableOutput("timeseriesTable")
      )
    )
  ),
  
  tabPanel("Input Settings",
    sidebarLayout(
      sidebarPanel(
        selectInput("Settings",
                    "Input Settings",
                    choices = list("Phases", "Lambdas"),
                    selected = "Phases"),
        radioButtons("settingsoneplot",
                     "Plot",
                     choices = list("One Plot", "Multipanel Plot"),
                     selected = "One Plot",
                     inline = TRUE)
        ),
      mainPanel(
        plotOutput("settingsPlot"),
        dataTableOutput("settingsTable")
      )
    )
  )
  
) # close navbarPage parens


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   asapnames <- reactive({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     tempnames <-  input$myfiles[,1]
     nfiles <- length(tempnames)
     res <- rep(NA, nfiles)
     for(i in 1:nfiles){
       res[i] <- substr(tempnames[i], 1, nchar(tempnames[i]) - 5)
     }
     res
   })
   
   tsdf <- reactive({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     mydf <- data.frame(Run = character(),
                        Variable = character(),
                        Year = integer(),
                        Value = double() )
     nfiles <- length(asapnames())
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       yrs <- seq(asap$parms$styr, asap$parms$endyr)
       nyrs <- length(yrs)
       ssb <- asap$SSB
       freport <- asap$F.report
       recruits <- asap$N.age[,1]
       thisdf <- data.frame(Run = rep(asapnames()[i], nyrs*3),
                            Variable = c(rep("SSB", nyrs), 
                                         rep("Freport", nyrs),
                                         rep("Recruits", nyrs)),
                            Year = c(yrs, yrs, yrs),
                            Value = c(ssb, freport, recruits))
       mydf <- rbind(mydf, thisdf)
     }
     mydf
   })
   
   settingsdf <- reactive({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     mydf <- data.frame(Run = character(),
                        Variable = character(),
                        Name = character(),
                        Value = double() )
     nfiles <- length(asapnames())
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       phase.names <- names(asap$control.parms$phases)
       nphases <- length(phase.names)
       thisdf <- data.frame(Run = rep(asapnames()[i], nphases),
                            Variable = rep("Phases", nphases),
                            Name = phase.names,
                            Value = as.numeric(asap$control.parms$phases[1:nphases])) %>%
         mutate(Value = replace(Value, Value <= -1, -1))
       lambda.names <- c(names(asap$control.parms$singles[c(1,3,4,6)]),
                         names(asap$control.parms[c(5,6,8,10,12,14,16)]))
       lambda.vals <- c(as.numeric(asap$control.parms$singles[c(1,3,4,6)]),
                        as.numeric(lapply(asap$control.parms[c(5,6,8,10,12,14,16)], mean)))
       nlambdas <- length(lambda.names)
       thisdf2 <- data.frame(Run = rep(asapnames()[i], nlambdas),
                             Variable = rep("Lambdas", nlambdas),
                             Name = lambda.names,
                             Value = lambda.vals)
       mydf <- rbind(mydf, thisdf, thisdf2)
     }
     mydf
   })
   
   output$filenames <- renderTable({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     input$myfiles[,1]
   })
   
   output$lktable <- renderTable({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     Component <- c("total","catch.total","discard.total","index.fit.total","catch.age.comp","discards.age.comp","index.age.comp","sel.param.total","index.sel.param.total","q.year1","q.devs","Fmult.year1.total","Fmult.devs.total","N.year1","Recruit.devs","SR.steepness","SR.scaler","Fmult.Max.penalty","F.penalty")
     nfiles <- length(asapnames())
     lk <- Component
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       lk <- cbind(lk, as.numeric(asap$like))
     }
     colnames(lk) <- c("Component",asapnames())
     lk
   })
   
   output$lkPlot <- renderPlot({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     nfiles <- length(asapnames())
     lkdf <- data.frame(Run = character(),
                        Component = character(),
                        Value = double())
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       lknames <- substr(names(asap$like), 4, 999)
       thislkdf <- data.frame(Run = rep(asapnames()[i], length(lknames)),
                              Component = lknames,
                              Value = as.numeric(asap$like))
       lkdf <- rbind(lkdf, thislkdf)
     }
     ggplot(filter(lkdf, Component == input$objfxn), aes(x=Run, y=Value, color=Run)) +
         geom_point(size=3) +
         theme_bw()
   })
   
   output$timeseriesPlot <- renderPlot({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     ggplot(filter(tsdf(), Variable == input$TimeSeries), aes(x=Year, y=Value, color=Run)) +
       geom_point() +
       geom_line() +
       expand_limits(y = 0) +
       {if (input$tsoneplot == "Multipanel Plot") facet_wrap(~Run)} +
       theme_bw()
   })
   
   output$timeseriesTable <- renderDataTable(filter(tsdf(), Variable == input$TimeSeries))
   
# note: if Run App in RStudio window, the filename will not default correctly (known RStudio bug),
# but if Run App in External browser then filename will show up correctly
   output$downloadData <- downloadHandler(
      filename = function() {
        paste0("ASAP_compare_time_series_data_",Sys.Date(),".csv")
      },
     content = function(file) {
       write.csv(tsdf(), file, row.names = FALSE)
     }
   )

   output$settingsPlot <- renderPlot({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     ggplot(filter(settingsdf(), Variable == input$Settings), aes(x=Value, y=Name, color=Run)) +
       geom_point() +
       {if (input$settingsoneplot == "Multipanel Plot") facet_wrap(~Run)} +
       theme_bw()
   })  
   
   output$settingsTable <- renderDataTable(filter(settingsdf(), Variable == input$Settings))

}

# Run the application 
shinyApp(ui = ui, server = server)

