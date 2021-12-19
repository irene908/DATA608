library(shiny)
library(dplyr)
library(tibble)
library(tidyr)
library(lubridate)
library(stringr)
library(plotly)
library(purrr)
#suppressPackageStartupMessages(library(googleVis))

calendar <- read.csv("https://raw.githubusercontent.com/Irene906/DATA608/main/calendar.csv")
listings <- read.csv("https://raw.githubusercontent.com/Irene906/DATA608/main/listings.csv")
reviews <- read.csv("https://raw.githubusercontent.com/Irene906/DATA608/main/reviews.csv")


calendar <- as_tibble(calendar)
listings <- as_tibble(listings)
reviews <- as_tibble(reviews)

#rename the id column to match with calendar dataset
cols_listings <- colnames(listings)
cols_listings[1] <- "listing_id"
colnames(listings) <- cols_listings

df <- calendar %>% inner_join(listings, by = "listing_id") %>% rename(cost = price.x, pay_date = price.y)

df$date <- as.Date(df$date, format = "%Y-%m-%d")

#clean the cost
df$cost <- as.character(df$cost)
df$cost <- str_replace(df$cost, pattern = "^\\$", replacement = "")
df$cost <- str_replace(df$cost, pattern = ",", replacement = "")
df$cost <- as.numeric(df$cost)

#clean the date of payment
df$pay_date <- as.character(df$pay_date)
df$pay_date <- str_replace(df$pay_date, pattern = "^\\$", replacement = "")
df$pay_date <- str_replace(df$pay_date, pattern = ",", replacement = "")
df$pay_date <- as.numeric(df$pay_date)

df$property_type <- as.character(df$property_type)
df$neighbourhood <- as.character(df$neighbourhood)

date_boston <- df$date %>% c() %>% unlist(use.names = FALSE) %>% unique() %>% sort()

df <- df %>% group_by(neighbourhood, property_type) %>% nest() %>% dplyr::mutate( total_listing = map2_dbl(.x = neighbourhood, .y = property_type, .f = ~nrow(filter(listings, neighbourhood == .x & property_type == .y)))) %>% unnest(cols = any_of("data"))

df <- df %>% group_by(neighbourhood,property_type,date) %>% nest() %>% dplyr::mutate( occupied = map_dbl(.x = data, .f = ~nrow(filter(.x, available == "t"))), price_day = map_dbl(.x = data, .f = ~sum(.x$cost[which(.x$available == "t")],na.rm = TRUE))) %>% unnest(cols = any_of("data")) %>% dplyr::mutate(occupied_perc = round(occupied * 100/total_listing,digits = 1)) %>% group_by(neighbourhood,property_type,date,total_listing,occupied,occupied_perc,price_day) %>% nest()

df <- df %>% dplyr::mutate( date_in_month = make_date(year = year(date), month = month(date), day = 1))

df <- df %>% select(neighbourhood,property_type,date,total_listing,occupied,occupied_perc,price_day,date_in_month)

neighborhd <- unique(df$neighbourhood)
propertytype <- unique(df$property_type)

ui <- fluidPage(
  titlePanel("Airbnb homestays in Boston"),
  sidebarLayout(
    sidebarPanel(
      selectInput('neighbor','Neighbourhood',choices = neighborhd,selected = "Hyde Park"),
      dateRangeInput('checkin','Date',start = date_boston[1],end = date_boston[length(date_boston)]),
      selectInput('parameter','earnings/booking numbers',choices = c("earnings","booking numbers"),selected = "earnings"),
      actionButton('makeplot','Display Plot',color = 'primary',style = 'bordered')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('daily',plotlyOutput('DAILY')),
        tabPanel('monthly',plotlyOutput('MONTHLY'))
      )
    )
  )
)

server <- function(input, output, session){
  daily_plot <- reactive({
    df %>% filter(neighbourhood == input$neighbor & date >= input$checkin[1] & date <= input$checkin[2]) %>%
      arrange(property_type,date) %>% group_by(property_type,date) %>%
      summarise(total_listing = sum(total_listing, na.rm = TRUE), occupied = sum(occupied, na.rm = TRUE), price_day = sum(price_day, na.rm = TRUE)) %>%
      dplyr::mutate(occupied_perc = round(occupied * 100/total_listing, digits = 1))
  })
  monthly_plot <- reactive({
    df %>% filter(neighbourhood == input$neighbor & date_in_month >= make_date(year = year(input$checkin[1]), month = month(input$checkin[1]), day = 1) &                            date_in_month <= make_date(year = year(input$checkin[2]), month = month(input$checkin[2]), day = 27)) %>%
      arrange(property_type,date_in_month) %>% group_by(property_type,date_in_month) %>% summarise(total_listing = sum(total_listing, na.rm = TRUE), occupied = sum(occupied, na.rm = TRUE), price_day = sum(price_day, na.rm = TRUE)) %>%
      dplyr::mutate(occupied_perc = round(occupied * 100/total_listing, digits = 1))
  })
  
  display_plot <- eventReactive(input$makeplot,{
    
    if (input$parameter == "earnings"){
      daily_plot() %>%
        ggplot(aes(x = date)) +
        geom_line(aes(y = price_day, color = property_type), size = 1)
    } 
    
    else if (input$parameter == "booking numbers"){
      daily_plot() %>%
        ggplot(aes(x = date)) +
        geom_line(aes(y = occupied_perc, color = property_type), size = 1)
    }
    
  })
  
  make_plot_month <- eventReactive(input$makeplot,{
    if (input$parameter == "earnings"){
      monthly_plot() %>%
        ggplot(aes(x = date_in_month)) +
        geom_line(aes(y = price_day, color = property_type), size = 1)
    } 
    
    else if (input$parameter == "booking numbers"){
      monthly_plot() %>%
        ggplot(aes(x = date_in_month)) +
        geom_line(aes(y = occupied_perc, color = property_type), size = 1)
    }
    
  })
  
  output$DAILY <- renderPlotly({
    display_plot()
  })
  
  output$MONTHLY <- renderPlotly({
    make_plot_month()
  })
  
}

shinyApp(ui = ui, server = server)
