#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgdal)
library(leaflet)
require(dplyr)
require(tigris)

###DATA PREP
#Bring in county boundaries
mapboundaries <- readOGR("./cb_2018_us_county/cb_2018_us_county_500k.shp", layer = "cb_2018_us_county_500k", GDAL1_integer64_policy = TRUE)
#prep county lines by removing all states not in continental US
not48states <- c("02","15","60","64","66","68","69","70","71","72","74","76","78","79","81","84","86","87","89","95")
countylines48 <- mapboundaries[!mapboundaries$STATEFP %in% not48states,]

#bring in county level data
countydata <- read.csv("./data/OpioidCountyData.csv", header = TRUE)
#prep county data by creating state fips codes through regex then removing AK, HI, and USVI
countydata$statefp <- substr(countydata$FIPS, 0, (nchar(countydata$FIPS)-3))
countydata48 <- countydata[!countydata$statefp %in% c(2,15,78),]

#bring in highways 
highways <- readOGR("./tl_2016_us_primaryroads/tl_2016_us_primaryroads.shp", layer = "tl_2016_us_primaryroads", GDAL1_integer64_policy = TRUE)
highway2 <- readOGR("./data/intrstat.shp", layer = "intrstat", GDAL1_integer64_policy = TRUE)
usoutline <- readOGR("./gz_2010_us_outline_20m.json", layer = "gz_2010_us_outline_20m", GDAL1_integer64_policy = TRUE)

#change FIPS and GEOID to integer, b/c GEOID had 0 in front of five states, whild FIPS didnt.
countydata48$FIPS <- as.integer(countydata48$FIPS)
countylines48$FIPS <- as.integer(as.character(countylines48$GEOID))


# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Opioid Data Dashboard"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("year",
                        label = "Year of data being viewed:",
                        min =2013, max = 2017, value = 2015
            ),
            selectInput("variable", label = h3("Variable observed"),
                        choices = list("Opioid Prescription Rate" = "Opioid_Prescribing_Rate2",
                                       "Long acting Opioid Prescription Rate" = "Long_Acting_Opioid_Pres2",
                                       "Median Household Income" = "SDOH_Median_Household_Income"
                        )
            )
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("variableGraph"),
        )
    )
)
)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    testmapyear <-filter(countydata48, year_num==input$year)
    
    datasetInput <- reactive({
        #        countydata48[ countydata48[ , input$WebsiteName ] == input$variable, ]
        mapyear %>% filter(countydata48, year_num==input$year)
    })
    output$variableGraph <- renderPlot({
        x <- mapyear[, input$variable]
        hist(x, breaks = 5, col = "blue")
    })
})


# Run the application 
shinyApp(ui = ui, server = server)
