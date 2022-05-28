#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel("New York state taxi demand"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("date:","Date:",min = 1,max = 31,value = 18),
            sliderInput("hour:","Hour:",min = 1,max = 24,value = 10),
            checkboxInput("Model_1","Random forest regression", value = FALSE),
            checkboxInput("Model_2","Extreme gradient boosting regression", value = FALSE),
            checkboxInput("Model_3","Multilayer perceptron", value = FALSE),
            checkboxInput("Model_4","Vector auto regression", value = FALSE),
            checkboxInput("Model_5","ARIMA", value = FALSE),
            checkboxInput("Model_6","ARIMAX", value = FALSE),
            br(),
            br(),
            titlePanel("Location IDs"),
            checkboxInput("Loc_1","40", value = FALSE),
            checkboxInput("Loc_2","50", value = FALSE),
            checkboxInput("Loc_3","60", value = FALSE),
            checkboxInput("Loc_4","70", value = FALSE),
            br(),
            br(),
            titlePanel("Distance Types"),
            checkboxInput("Long_Loc","Long", value = FALSE),
            checkboxInput("Short_Loc","Short", value = FALSE),
            
        ),
        
        
        mainPanel(
            plotOutput("Plot1"),
            h3("Predicted number of pickups from model_1"),
            textOutput("pred1"),
            h3("Predicted number of pickups from model_2"),
            textOutput("pred2"),
            h3("Predicted number of pickups from model_3"),
            textOutput("pred3"),
            h3("Predicted number of pickups from model_4"),
            textOutput("pred4"),
            h3("Predicted number of pickups from model_5"),
            textOutput("pred5"),
            h3("Predicted number of pickups from model_6\n\n\n"),
            textOutput("pred6"),
            br(),
            h3("Peak time from Loc_1"),
            textOutput("predloc1"),
            h3("Peak time from Loc_2"),
            textOutput("predloc2"),
            h3("Peak time from Loc_3"),
            textOutput("predloc3"),
            h3("Peak time from Loc_4\n\n\n"),
            textOutput("predloc4"),
            h3("Off-Peak time from Loc_1"),
            textOutput("predloc5"),
            h3("Off-Peak time from Loc_2"),
            textOutput("predloc6"),
            h3("Off-Peak time from Loc_3"),
            textOutput("predloc7"),
            h3("Off-Peak time from Loc_4\n\n\n"),
            textOutput("predloc8"),
            br(),
            h2("Long Distance Taxi trips"),
            h4("Maximum Long Distance Taxi Trip"),
            textOutput("maxlong1"),
            h4("Average Tip amount"),
            textOutput("avglong1"),
            h4("Popular payment method among Long Distance Taxi Trips"),
            textOutput("paylong1"),
            h4("Top 10 Locations with highest number of Long Distance Taxi Trips"),
            textOutput("loclong1"),
            h4("Passenger count"),
            textOutput("cntlong1"),
            br(),
            h2("Short Distance Taxi trips"),
            h4("Maximum Short Distance Taxi Trip"),
            textOutput("maxshort1"),
            h4("Average Tip amount"),
            textOutput("avgshort1"),
            h4("Popular payment method among Short Distance Taxi Trips"),
            textOutput("payshort1"),
            h4("Top 10 Locations with highest number of Short Distance Taxi Trips"),
            textOutput("locshort1"),
            # h3("Comparing the long distance and short distance trips"),
            # textOutput("diffdist1"),
            h4("Passenger count"),
            textOutput("cntshort1"),
        ),
        
    )
))
