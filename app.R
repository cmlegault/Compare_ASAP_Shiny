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
                    choices = list("Phases", "Lambdas", "CVs"),
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
       expand_limits(x = 0) +
       {if (input$settingsoneplot == "Multipanel Plot") facet_wrap(~Run)} +
       theme_bw()
   })  
   
   output$settingsTable <- renderDataTable(filter(settingsdf(), Variable == input$Settings))

}

# Run the application 
shinyApp(ui = ui, server = server)
