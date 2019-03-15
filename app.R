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
# Define UI using tabs for different topics
ui <- navbarPage("Compare ASAP",
   
  tabPanel("Select Runs",
    sidebarLayout(
      sidebarPanel(
        fileInput("myfiles", 
                  label = "ASAP .rdat files",
                  multiple = TRUE,
                  accept=c(".RDAT"))
      ),
      mainPanel(
        tableOutput("filenames"),
        tableOutput("dimensions")
      )
    )
  ),
  
  tabPanel("Obj Fxn",
    sidebarLayout(
      sidebarPanel(
         selectInput("objfxn",
                     "Objective Function Component",
                      choices = list("total","catch.total","discard.total","index.fit.total","catch.age.comp","discards.age.comp","index.age.comp","sel.param.total","index.sel.param.total","q.year1","q.devs","Fmult.year1.total","Fmult.devs.total","N.year1","Recruit.devs","SR.steepness","SR.scaler","Fmult.Max.penalty","F.penalty"),
                      selected = "total"),
         downloadButton("downloadObjFxn", "Download")
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
        downloadButton("downloadTimeSeries", "Download")
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
                    choices = list("Phases", "Lambdas", "CVs"),
                    selected = "Phases"),
        radioButtons("settingsoneplot",
                     "Plot",
                     choices = list("One Plot", "Multipanel Plot"),
                     selected = "One Plot",
                     inline = TRUE),
        downloadButton("downloadInputSettings", "Download")
        ),
      mainPanel(
        plotOutput("settingsPlot"),
        dataTableOutput("settingsTable")
      )
    )
  ),
  
  tabPanel("Selectivities",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("Selectivity",
                     "Show Selectivity for",
                     choiceNames = list("Fleet Blocks", "Indices"),
                     choiceValues = list("Fleet Blocks", "Indices"),
                     selected = list("Fleet Blocks", "Indices"),
                     inline = TRUE),
        radioButtons("selectivityoneplot",
                     "Plot",
                     choices = list("One Plot", "Multipanel Plot"),
                     selected = "One Plot",
                     inline = TRUE),
        downloadButton("downloadSelectivities", "Download")
        ),
      mainPanel(
        plotOutput("selectivityPlot"),
        dataTableOutput("selectivityTable")
      )
    )
  ),
  
  tabPanel("Std. Residuals",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("Residuals",
                     "Show Standardized Residuals for",
                     choiceNames = list("Catch by Fleet", "Indices"),
                     choiceValues = list("Catch by Fleet", "Indices"),
                     selected = list("Catch by Fleet", "Indices"),
                     inline = TRUE),
        radioButtons("residualsoneplot",
                     "Plot",
                     choices = list("One Plot", "Multipanel Plot"),
                     selected = "One Plot",
                     inline = TRUE),
        downloadButton("downloadResiduals", "Download")
        ),
      mainPanel(
        plotOutput("residualsPlot"),
        dataTableOutput("residualsTable")
      )
    )
  )
  
) # close navbarPage parens


# Define server logic using reactive data frames
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
   
   lkdf <- reactive({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     mydf <- data.frame(Run = character(),
                        Component = character(),
                        Value = double())
     nfiles <- length(asapnames())
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       lknames <- substr(names(asap$like), 4, 999)
       thisdf <- data.frame(Run = rep(asapnames()[i], length(lknames)),
                            Component = lknames,
                            Value = as.numeric(asap$like))
       mydf <- rbind(mydf, thisdf)
     }
     mydf
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
       # fleet selectivity blocks
       fleet.sel.ini <- asap$sel.input.mats$fleet.sel.ini
       nselblocks <- asap$parms$nselblocks
       nages <- asap$parms$nages
       for (iselb in 1:nselblocks){
         startrow <- (iselb - 1) * (nages + 6) + 1
         if (asap$fleet.sel.option[iselb] == 1){ # by age
           selblock <- fleet.sel.ini[startrow:(startrow + nages - 1), ]
         }
         if (asap$fleet.sel.option[iselb] == 2){ # single logistic
           selblock <- fleet.sel.ini[(startrow + nages):(startrow + nages + 1), ]
         }
         if (asap$fleet.sel.option[iselb] == 3){ # double logisitc
           selblock <- fleet.sel.ini[(startrow + nages + 2):(startrow + nages + 5), ]
         }
         meanselblock <- apply(selblock, 2, mean, na.rm=TRUE)
         if (iselb == 1) selblockres <- meanselblock
         if (iselb > 1) selblockres <- rbind(selblockres, meanselblock)
       }
       if (nselblocks > 1) selblockres <- apply(selblockres, 2, mean, na.rm=TRUE)
       # note cannot include index selectivity block information because rdat file does not distinguish which selectivity option is used for each index
       
       # Phases
       phase.names <- c(names(asap$control.parms$phases), "Fleet.sel.blocks")
       nphases <- length(phase.names)
       thisdf <- data.frame(Run = rep(asapnames()[i], nphases),
                            Variable = rep("Phases", nphases),
                            Name = phase.names,
                            Value = c(as.numeric(unlist(asap$control.parms$phases[1:nphases])),
                                      as.numeric(selblockres[2]))) %>%
         mutate(Value = replace(Value, Value <= -1, -1))
       # Lambdas
       lambda.names <- c(names(asap$control.parms$singles[c(1,3,4,6)]),
                         names(asap$control.parms[c(5,6,8,10,12,14,16)]),
                         "Fleet.sel.blocks")
       lambda.vals <- c(as.numeric(asap$control.parms$singles[c(1,3,4,6)]),
                        as.numeric(lapply(asap$control.parms[c(5,6,8,10,12,14,16)], mean)),
                        selblockres[3])
       nlambdas <- length(lambda.names)
       thisdf2 <- data.frame(Run = rep(asapnames()[i], nlambdas),
                             Variable = rep("Lambdas", nlambdas),
                             Name = lambda.names,
                             Value = lambda.vals)
       # Coefficients of Variation only for parameters with lambda > 0
       nfleets <- asap$parms$nfleets
       cv.names <- paste0("tot.catch.cv.fleet.", 1:nfleets)
       cv.vals <- apply(asap$control.parms$catch.tot.cv, 2, mean, na.rm=TRUE)
       if (sum(asap$control.parms$lambda.Discard.tot) > 0){
         disc.names <- paste0("discard.tot.cv.fleet.", 1:nfleets)
         disc.vals <- apply(asap$control.parms$discard.tot.cv, 2, mean, na.rm=TRUE)
         cv.names <- c(cv.names, disc.names)
         cv.vals <- c(cv.vals, disc.vals)
       }
       if (asap$control.parms$singles$lambda.recruit.devs > 0){
         cv.names <- c(cv.names, "recruit.cv")
         cv.vals <- c(cv.vals, mean(asap$control.parms$recruit.cv, na.rm=TRUE))
       }
       for (isingle in c(1, 4, 6)){
         if (asap$control.parms$singles[isingle] > 0){
           cv.names <- c(cv.names, names(asap$control.parms$singles[isingle + 1]))
           cv.vals <- c(cv.vals, as.numeric(asap$control.parms$singles[isingle + 1]))
         }
       }
       for (ifleet in 1:nfleets){
         if (asap$control.parms$lambda.Fmult.year1[ifleet] > 0){
           cv.names <- c(cv.names, paste0("Fmult.year1.cv.fleet.", ifleet))
           cv.vals <- c(cv.vals, as.numeric(asap$control.parms$Fmult.year1.cv[ifleet]))
         }
         if (asap$control.parms$lambda.Fmult.devs[ifleet] > 0){
           cv.names <- c(cv.names, paste0("Fmult.devs.cv.fleet.", ifleet))
           cv.vals <- c(cv.vals, as.numeric(asap$control.parms$Fmult.devs.cv[ifleet]))
         }
       }
       for (ind in 1:asap$parms$nindices){
         if (asap$control.parms$lambda.q.year1[ind] > 0){
           cv.names <- c(cv.names, paste0("q.year1.cv.index.", ind))
           cv.vals <- c(cv.vals, as.numeric(asap$control.parms$q.year1.cv[ind]))
         }
         if (asap$control.parms$lambda.q.devs[ind] > 0){
           cv.names <- c(cv.names, paste0("q.devs.cv.index.", ind))
           cv.vals <- c(cv.vals, as.numeric(asap$control.parms$q.devs.cv[ind]))
         }
         cv.names <- c(cv.names, names(asap$index.cv[ind]))
         cv.vals <- c(cv.vals, mean(asap$index.cv[[ind]], na.rm=TRUE))
       }
       if (selblockres[3] > 0){
         cv.names <- c(cv.names, "Fleet.sel.blocks")
         cv.vals <- c(cv.vals, selblockres[4])
       }
       ncvs <- length(cv.names)
       thisdf3 <- data.frame(Run = rep(asapnames()[i], ncvs),
                             Variable = rep("CVs", ncvs),
                             Name = cv.names,
                             Value = cv.vals)
       # data frame of input settings
       mydf <- rbind(mydf, thisdf, thisdf2, thisdf3)
     }
     mydf
   })

   selectivitydf <- reactive({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     emptydf <- data.frame(Run = character(),
                           IDcounter = integer(),
                           Variable = character(),
                           Name = character(),
                           Age = integer(),
                           Value = double() )
     mydf <- emptydf
     nfiles <- length(asapnames())
     IDcount = 0
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       nages <- asap$parms$nages
       # fleet block selectivities
       fleet.sel.df <- emptydf
       for (iselb in 1:asap$parms$nselblocks){
         IDcount <- IDcount + 1
         gotit <- FALSE
         for (ifleet in 1:asap$parms$nfleets){
           for (iyear in 1:asap$parms$nyears){
             if (gotit == FALSE){
               if (asap$fleet.sel.blocks[ifleet, iyear] == iselb){
                 gotit <- TRUE
                 fleet.df <- data.frame(Run = rep(asapnames()[i], nages),
                                        IDcounter = rep(IDcount, nages),
                                        Variable = rep("Fleet Blocks", nages),
                                        Name = rep(paste0("selblock",iselb), nages),
                                        Age = 1:nages,
                                        Value = as.numeric(asap$fleet.sel.mats[[ifleet]][iyear,]))
                 fleet.sel.df <- rbind(fleet.sel.df, fleet.df)
               }
             }
           }
         }
       }
       # index selectivities
       index.sel.df <- emptydf
       nindices <- asap$parms$nindices
       for (ind in 1:nindices){
         IDcount <- IDcount + 1
         start.age <- asap$control.parms$index.sel.start.age[ind]
         end.age <- asap$control.parms$index.sel.end.age[ind]
         index.ages <- seq(start.age, end.age)
         nindex.ages <- length(index.ages)
         ind.df <- data.frame(Run = rep(asapnames()[i], nindex.ages),
                              IDcounter = rep(IDcount, nindex.ages),
                              Variable = rep("Indices", nindex.ages),
                              Name = rep(paste0("index",ind), nindex.ages),
                              Age = index.ages,
                              Value = asap$index.sel[ind, start.age:end.age])
         index.sel.df <- rbind(index.sel.df, ind.df)
       }
       # add to total data frame
       mydf <- rbind(mydf, fleet.sel.df, index.sel.df)
     }   
     mydf
   })
   
   residualsdf <- reactive({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     emptydf <- data.frame(Run = character(),
                           IDcounter = integer(),
                           Variable = character(),
                           Name = character(),
                           Year = integer(),
                           Value = double() )
     mydf <- emptydf
     nfiles <- length(asapnames())
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       nyears <- asap$parms$nyears
       nfleets <- asap$parms$nfleets
       years <- seq(asap$parms$styr, asap$parms$endyr)
       # catch resids by fleet
       fleet.df <- data.frame(Run = rep(asapnames()[i], nyears * nfleets),
                              IDcounter = rep(1:nfleets, each = nyears),
                              Variable = rep("Catch by Fleet", nyears * nfleets),
                              Name = rep(paste0("Fleet", 1:nfleets), each = nyears),
                              Year = rep(years, nfleets),
                              Value = as.numeric(t(asap$catch.std.resid)))
       # index resids
       IDcount <- nfleets
       index.resid.df <- emptydf
       for (ind in 1:asap$parms$nindices){
         IDcount <- IDcount + 1
         nyears.ind <- length(asap$index.year[[ind]])
         index.df <- data.frame(Run = rep(asapnames()[i], nyears.ind),
                                IDcounter = rep(IDcount, nyears.ind),
                                Variable = rep("Indices", nyears.ind),
                                Name = rep(paste0("index", ind)),
                                Year = asap$index.year[[ind]],
                                Value = asap$index.std.resid[[ind]])
         index.resid.df <- rbind(index.resid.df, index.df)
       }
       # add to total data frame
       mydf <- rbind(mydf, fleet.df, index.resid.df)
     }
     mydf
   })
   
   output$filenames <- renderTable({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     FileNames <- cbind(1:length(asapnames()), input$myfiles[,1])
     colnames(FileNames) <- c("File", "File Name")
     FileNames
   })
   
   output$dimensions <- renderTable({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     nfiles <- length(asapnames())
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       dmens <- c(i, as.numeric(asap$parms))
       if (i == 1){
         dmensions <- dmens
       }else{
         dmensions <- c(dmensions, dmens)
       }
     }
     dim(dmensions) <- c((1 + length(asap$parms)), nfiles)
     dimnames(dmensions) <- list(c("File", names(asap$parms)), 1:nfiles)
     dmens.table <- t(dmensions) %>%
       data.frame() %>%
       select(-navailindices)
     dmens.table
   }, digits = 0)
   
   output$lktable <- renderTable({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     nfiles <- length(asapnames())
     for (i in 1:nfiles){
       asap <- dget(input$myfiles[[i, "datapath"]])
       if (i == 1){
         lk <-  substr(names(asap$like), 4, 999)
       }
       lk <- cbind(lk, as.numeric(asap$like))
     }
     colnames(lk) <- c("Component",asapnames())
     lk
   })
   
   output$lkPlot <- renderPlot({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     ggplot(filter(lkdf(), Component == input$objfxn), aes(x=Run, y=Value, color=Run)) +
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
   
   output$settingsPlot <- renderPlot({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     ggplot(filter(settingsdf(), Variable == input$Settings), aes(x=Value, y=Name, color=Run)) +
       geom_point() +
       expand_limits(x = 0) +
       {if (input$settingsoneplot == "Multipanel Plot") facet_wrap(~Run)} +
       theme_bw()
   })  
   
   output$settingsTable <- renderDataTable(filter(settingsdf(), Variable == input$Settings))

   output$selectivityPlot <- renderPlot({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     ggplot(filter(selectivitydf(), Variable %in% input$Selectivity), 
            aes(x=Age, y=Value, color=Run, group=IDcounter)) +
       geom_point() +
       geom_line() +
       expand_limits(y = 0) +
       {if (input$selectivityoneplot == "Multipanel Plot") facet_wrap(~Run)} +
       theme_bw()
   })  
   
   output$selectivityTable <- renderDataTable(filter(selectivitydf(), Variable %in% input$Selectivity))
   
   output$residualsPlot <- renderPlot({
     if (is.null(input$myfiles)){
       return(NULL)
     }
     ggplot(filter(residualsdf(), Variable %in% input$Residuals), 
            aes(x=Year, y=Value, color=Run, group=IDcounter)) +
       geom_point() +
       expand_limits(y = 0) +
       {if (input$residualsoneplot == "Multipanel Plot") facet_wrap(~Run)} +
       theme_bw()
   })  
   
   output$residualsTable <- renderDataTable(filter(residualsdf(), Variable %in% input$Residuals))
   
   ## download buttons ##
   # note: if Run App in RStudio window, the filename will not default correctly (known RStudio bug),
   # but if Run App in External browser then filename will show up correctly however it will be
   # downloaded to directory C:\Users\your.name\AppData\Local\Temp
   output$downloadObjFxn <- downloadHandler(
     filename = function() {
       paste0("ASAP_compare_objective_function_data_",Sys.Date(),".csv")
     },
     content = function(file) {
       write.csv(lkdf(), file, row.names = FALSE)
     }
   )
   
   output$downloadTimeSeries <- downloadHandler(
     filename = function() {
       paste0("ASAP_compare_time_series_data_",Sys.Date(),".csv")
     },
     content = function(file) {
       write.csv(tsdf(), file, row.names = FALSE)
     }
   )
   
   output$downloadInputSettings <- downloadHandler(
     filename = function() {
       paste0("ASAP_compare_input_settings_data_",Sys.Date(),".csv")
     },
     content = function(file) {
       write.csv(settingsdf(), file, row.names = FALSE)
     }
   )
   
   output$downloadSelectivities <- downloadHandler(
     filename = function() {
       paste0("ASAP_compare_selectivities_data_",Sys.Date(),".csv")
     },
     content = function(file) {
       write.csv(selectivitydf(), file, row.names = FALSE)
     }
   )
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

