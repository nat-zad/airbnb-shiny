#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(leaflet)
library(ggplot2)
library(shinyjs)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  titlePanel(div(strong("Airbnb Safe Space"),  style = "font-size:40px;")),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      p(strong("Find an Airbnb Listing"),  style = "font-size:20px;"),
      selectInput('neighbourhood', "Select Neighboord", choices = unique(manhat$neighbourhood), selected = "Upper East Side"),
      selectInput("room_type", "Room Type", choices = unique(manhat$room_type), selected = "Entire  Home/apt"),
      sliderInput("price", "Price Range", min = 0, max = 600, value = c(10,200), sep = ""),
      sliderInput("review", "Number of Reviews", min = 0, max = 400, step = 50,
                  value = c(10, 350)),
      #print text here
      br(),
      br(),
      br(),
      br(),
      p(strong("Local Criminal Activity"), style = "font-size:20px;"), 
      p("The line graph displays the total reported crimes to a selected area."),
      pickerInput("zipcode", "Zipcode:", choices = unique(tally$zipcode), selected = "10021", 
                  options = list('actions-box' = TRUE), multiple = F)
  
      
    ),
   
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map"),
                 plotOutput("stats")),
        tabPanel("Explore Listings", DT::dataTableOutput("listings")),
        tabPanel("Crime Tabular Data", DT::dataTableOutput("table")),
        tabPanel("Analysis", 
                 br(),
                 p(strong("Welcome to Airbnb Safe Space!"), style = "font-size:20px;"),
                 p("Taking a well-deserved vacation can be the best part of the year!"),
                 p("...But finding a safe place to stay can be a challenge."),
                 br(),
                 p("The purpose of this app is to give the user the tools they need to make informed decisions."),
                 p("Not only can you use the interactive map and tabular data, below are findings from the data."),
                 br(),
                 p("So, let's go ahead and take a look at some of the analysis."),
                 br(),
                 p(strong("Considering your room type selection?"), style = "font-size:15px;"),
                 p("The overall averge price of a room in Manhattan is $139.11."),
                 br(),
                 p("The average price of the room types:"),
                 p("Entire Home/Apt - $230.78"),
                 p("Private Room - $106.99"),
                 p("Shared Room - $79.56"),
                 p("The neighborhoods with the highest number of:"),
                 p("Entire home/apt - East Village, Harlem, Hell's Kitchen, Midtown, Upper East & West Side."),
                 p("Private Room - Harlem"),
                 p("Overall, there are far fewer shared and private room to choose from across Manhattan."),
                 plotOutput("avg_roomtype", width ='800px'),
                  plotOutput("rt_listings", width = '800px'),
                 plotOutput('median',  width = '800px'),
                  br(),
                 p(strong("What are the most expensive neighborhoods in Manhattan to stay in?"), style = "font-size:15px;"),
                 p("The top 4 neighborhoods are Tribeca, NoHo, Flatiron District, and SoHo."),
                 p("It is worth noting these four areas are all found in lower westside of Manhattan & close by."),
                 plotOutput("exp_neighborhood", width='800px'),
                 br(),
                 p(strong("What are the most popular neighborhoods?"), style = "font-size:15px;"),
                 p("The most popular neighborhoods to stay in are East Harlem, Two Bridges, Harlem, Lower East Side."),
                 p("It is worth noting these are also among some of the cheapest neighborhoods to stay in. (See graph above.)"),
                 plotOutput("pop_neighborhood", width='800px'),
                 br(),
                 br(),
                 p(strong("Crime Analysis on Neighborhoods seen in the Airbnb Listings"), style = "font-size:18px;"),
                 br(),
                 p("An important part of every vacation is safety. In the Shiny app, the user is able to access year to date crime data based on zipcode. The user can find
                   the zipcode in the selected listing map pop-up or within the 'Explore Listings' tab. Once the zipcode has been selected in the drop-down 
                   menu, the graph on the right-hand side
                   will display the total number of criminal reports filed with the NYPD. You can further investigate this information by also searching for the
                   zipcode in the 'Crime Tabular Data' tab."),
                 br(),
                 p("To get a quick understanding of local crime for booking the Airbnb, below we have crime analysis from this year's crime report."),
                 br(),
                 p(strong("What are the neighboorhoods with the most reported crimes?"), style = "font-size:15px;"),
                 p("The zipcodes with the most reported activity are found in the following neighborhoods: Lower East Side, Harlem, Midtown West, and Washington Heights."),
                 p("Interstingly, if we compare this bar chart to the two neighborhood graphs (above), we can see these are on average the most popular and inexpensive places to stay."),
                 plotOutput("worst_zip", width='800px'),
                 br(),
                 p(strong("What kind of crimes are being reported in these (and other) areas?"), style = "font-size:15px;"),
                 p("The NYPD report has three categories for offenses: Felony, Misdemeanor, and Violation. Felonies represent the largest 
                   group of reported criminal incidents and include grand larcency, burglary, robbery, and rape." ),
                 plotOutput("category", width='800px'),
                 p("While felonies have the total highest amount of reported incidents, the most frequently reported offenses fall within the Violation and Misdemeanor categories."),
                 plotOutput("reports", width = '800px'),
                 br(),
                 p(strong("What areas are have the lowest amount of reported crime?"), style = "font-size:15px;"),
                 p("No NYC area has a perfect record, but these zipcodes These neighborhoods are Roosevelt Island, Battery Park, the Upper West Side, and Fincical District. These areas have under 100 reported incidents so far this year."),
                 plotOutput("low_crime"),
                 br(),
                 p(strong("What month(s) show a spike in criminal activity?"), style = "font-size:15px;"),
                 p("When planning a trip, the user may want to consider what months have overall higher criminal activity. There is an uptick during the warmer months. It may be worth seeing how these
                   numbers compare to last year's when planning ahead."),
                 plotOutput("dates"),
                 br(),
                 p(strong("Conclusions?"), style = "font-size:18px;"),
                 p("Like with any trip, the user has to take into consideration multiple factors. Safer areas with lower criminal activity generally have higher room prices.
                   The most highly rated areas are the cheapest, but also have more reported crime. By using the app and taking this analysis into ocnsideration,
                   the user can make the best decision for their budget and trip expectations."),
                 br(),
                 br(),
                 br(),
                 br(),
                 
                 
                 
              
                 
                 
                
                 
                 )
        
      ))
  )
    ) 
)

