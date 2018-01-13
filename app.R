library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)
library(forcats)
library(rsconnect)
library(leaflet)
library(maps)
library(geojsonio)
library(spdplyr)
library(jsonlite)


# load geospatial file
us_counties <- geojsonio::geojson_read("gz_2010_us_050_00_20m.json",
                                       what = "sp")

# filter for California counties
california <- us_counties %>%
        filter(STATE == '06')


# load employment data table
ca_counties <- read.table("california_counties_monthly_employment_2016.tsv", sep="\t", header = TRUE)

# grab california data
california.data <- california@data

# make column names the same (area_title column)
california.data <- california.data %>%
        mutate(area_title=paste(california.data$NAME, "County, CA"))

# join on area_title
combined_ca <- california.data %>%
        select(CENSUSAREA, area_title) %>%
        right_join(ca_counties) 



# spread rows of interests into just the 58 rows of states ... so each category gets 1 column per month
california@data <- combined_ca %>%
        gather(status, value, labor_force:unemployed_rate) %>% 
        unite(status_and_period, period, status) %>%
        spread(status_and_period, value) %>%
        right_join(california@data) %>%
        select(GEO_ID:LSAD, CENSUSAREA:`2016-12-01_unemployed_rate`)


create.leaflet <- function(category, date){
        ## now lets make the leaflet
        
        status.date <- california@data %>% select(matches(paste(date, category, sep="_")))
        
        status.date <- as.numeric(status.date[,1])
        
        m <- leaflet(california) %>% 
                setView(-120.388, 37.553, zoom = 6) %>%
                addTiles()
        
        m %>% addPolygons()
        
        
        pal <- colorBin("YlOrRd", domain=status.date)
        
        m %>% addPolygons(
                fillColor = ~pal(status.date),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7)
        
        m %>% addPolygons(
                fillColor = ~pal(status.date),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE))
        
        
        labels <- sprintf(
                "<strong>%s</strong><br/>%g value",
                california@data$STATE, status.date
        ) %>% lapply(htmltools::HTML)
        
        m <- m %>% addPolygons(
                fillColor = ~pal(status.date),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7,
                highlight = highlightOptions(
                        weight = 5,
                        color = "#666",
                        dashArray = "",
                        fillOpacity = 0.7,
                        bringToFront = TRUE),
                label = labels,
                labelOptions = labelOptions(
                        style = list("font-weight" = "normal", padding = "3px 8px"),
                        textsize = "15px",
                        direction = "auto"))
        m
        
        m %>% addLegend(pal = pal, values = ~status.date, opacity = 0.7, title = NULL,
                        position = "bottomright")
        
}


month <- unique(as.character(combined_ca$period))
category <- c("labor_force", "employed", "unemployed", "unemployed_rate")


ui <- shinyUI(pageWithSidebar(
        headerPanel("California Counties Employment 2016"),
        sidebarPanel(selectInput(inputId="months",
                                 label="Select Month:",
                                 choices=month),
                     selectInput(inputId="categories",
                                 label="Select Category:",
                                 choices=category),
                     actionButton("plotbutton", "Show Plot")
        ),
        mainPanel(
                leafletOutput("ui_plot")
        )

))


server <- shinyServer(function(input, output){
        output$ui_plot <- renderLeaflet({
                input$plotbutton
                isolate(create.leaflet(input$categories, input$months))
        })
})


shinyApp(ui = ui, server = server)