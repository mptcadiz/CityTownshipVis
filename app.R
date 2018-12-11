# Library -----------------------------------------------------------------

library(shiny)
library(tidyverse)
library(sf)
library(scales)
library(plotly)
library(Hmisc)
library(extrafont)
library(gridExtra)
#library(lettercase)
library(ggiraph)
loadfonts()


# Prepare ggplot themes ---------------------------------------------------

## These change the ggplot themes to more directly match CRPD's style

theme_bar <- theme_bw() +
  theme(text = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 16),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        legend.title = element_blank())

theme_line <- theme_bw() +
  theme(legend.background = element_rect(fill = "transparent", color = "transparent"),
        legend.key = element_rect(fill = "transparent"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey70", size = 0.1),
        axis.ticks = element_blank(),
        axis.text = element_text(face = "bold"),
        panel.border = element_blank(),
        text = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 16),
        plot.caption = element_text(family = "Helvetica Neue,Helvetica,Arial,sans-serif", size = 13, hjust = 0.5, face = "italic"))

theme_sf <- theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 16, family = "Helvetica Neue,Helvetica,Arial,sans-serif"),
        legend.title = element_blank())

# Objects - Visualization of city and townships ---------------------------------------------------------
total.tidy.city <- total.tidy %>%
  filter(area=="City") %>%
  group_by(placeName) %>%
  arrange(year) %>%
  ungroup()
#  mutate(year = as.integer(year),
#         population = as.integer(population)
#         )
#  mutate(bins = cut(population,
#                    breaks = c(0, 9999, 19999, 29999, 39999, 49999, 2000000),
#                    labels = c("1- 9,999", "10,000 - 19,999", "20,000 - 29,999", "30,000 - 39,999", "40,000 - 49,999", "50,000+")),
#         bins = ifelse(is.na(bins), "Not yet incorporated", as.character(bins)))
#need to fix the population of 0 being listed as not yet incorporated

total.tidy.township <- total.tidy %>%
  filter(area=="Township") %>%
  group_by(placeName) %>%
  arrange(year) %>%
  ungroup()

city.list <- total.tidy.city %>%
  select(Id,placeName) %>%
  distinct(Id,placeName) %>%
  select(placeName) %>%
  arrange(placeName)

county.city.list <- total.tidy.city %>%
  select(countyName) %>%
  distinct(countyName)

county.township.list <- total.tidy.city %>%
  select(countyName) %>%
  filter(countyName != "Multiple Counties") %>%
  distinct(countyName)

#cities.in.county.list <- total.tidy.city %>%
#  filter(countyName=="pop.county.city.list")

vis.list <- c("Trend lines","Data points", "Both visualizations") %>% 
  as.list()

# UI - Title Panel --------------------------------------------------------

##Setting up the theme, logo, etc....

ui <- fluidPage(
#for visualizing all cities in a county  
  selectizeInput(inputId = "pop.county.city.list",
              label = "Choose a county",
              choices = county.city.list,
              multiple = FALSE),
  uiOutput("cityControls"),
  
  radioButtons(inputId="pop.city.vis.list",
               label="How would you like to visualize the data?",
               choices = vis.list,
               inline=TRUE),

  ggiraphOutput("popcountycitygraph"),

#for visualizing all townships in a county  
  selectizeInput(inputId = "pop.county.township.list",
              label = "Choose a county",
              choices = county.township.list,
              multiple = FALSE),
  uiOutput("townshipControls"),
  
  radioButtons(inputId="pop.township.vis.list",
               label="How would you like to visualize the data?",
               choices = vis.list,
               inline=TRUE),

  ggiraphOutput("popcountytownshipgraph")
  
#for visualizing individual cities
#  selectInput(inputId = "pop.city.list",
#              label = "Choose a city",
#              choices = city.list,
#              multiple = TRUE),
  
#  radioButtons(inputId="pop.city.vis.list",
#               label="How would you like to visualize the data?",
#               choices = vis.list,
#               inline=TRUE),
  
#  ggiraphOutput("popcitygraph")
)

  # Server - Population: cities and townships from counties -----------------------------------
server <- function(input, output, session) {
  
  selectcity <- function(pop.county.city.list) {
    city.list.filtered <<- total.tidy.city %>%
      filter(countyName==input$pop.county.city.list) %>%
      select(placeName) %>%
      distinct(placeName)
    return(city.list.filtered)
  }
  
  
  output$cityControls <- renderUI({
    cities <- selectcity(input$pop.county.city.list)
    selectizeInput(inputId = "pop.county.city.select",
                label = "Choose a city",
                choices = city.list.filtered,
                multiple = TRUE)
  })
  
  output$popcountycitygraph <- renderggiraph({
    
    pop.county.city.plot <- ggplot(filter(total.tidy.city, placeName %in% input$pop.county.city.select & countyName %in% input$pop.county.city.list), aes(color=placeName, x=as.numeric(year), y=as.numeric(population))) +
      scale_x_continuous(breaks=c(1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2016))+
      #scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Population")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$pop.city.vis.list == "Data points"){
      pop.county.city.plot <- pop.county.city.plot + geom_point_interactive(size=3,aes(tooltip=paste(placeName, year,"\n Pop: ",population)))
    }
    else if (input$pop.city.vis.list == "Trend lines"){
      pop.county.city.plot <- pop.county.city.plot + geom_line()
    }
    else if (input$pop.city.vis.list == "Both visualizations"){
      pop.county.city.plot <- pop.county.city.plot + geom_point_interactive(size=3,aes(tooltip=paste(placeName, year,"\n Pop: ",population))) +
        geom_line()
    }
    ggiraph(code=print(pop.county.city.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
  
  selecttownship <- function(pop.county.township.list) {
    township.list.filtered <<- total.tidy.township %>%
      filter(countyName==input$pop.county.township.list) %>%
      select(placeName) %>%
      distinct(placeName)
    return(township.list.filtered)
  }
  
  
  output$townshipControls <- renderUI({
    townships <- selecttownship(input$pop.county.township.list)
    selectizeInput(inputId = "pop.county.township.select",
                label = "Choose a township",
                choices = township.list.filtered,
                multiple = TRUE)
  })
  
  output$popcountytownshipgraph <- renderggiraph({
    
    pop.county.township.plot <- ggplot(filter(total.tidy.township, placeName %in% input$pop.county.township.select & countyName %in% input$pop.county.township.list), aes(color=placeName, x=as.numeric(year), y=as.numeric(population))) +
      scale_x_continuous(breaks=c(1900,1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2016))+
      #scale_y_continuous(labels=scales::percent)+
      labs(x="Year", y="Population")+
      theme_bar+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    if (input$pop.township.vis.list == "Data points"){
      pop.county.township.plot <- pop.county.township.plot + geom_point_interactive(size=3,aes(tooltip=paste(placeName, year,"\n Pop: ",population)))
    }
    else if (input$pop.township.vis.list == "Trend lines"){
      pop.county.township.plot <- pop.county.township.plot + geom_line()
    }
    else if (input$pop.township.vis.list == "Both visualizations"){
      pop.county.township.plot <- pop.county.township.plot + geom_point_interactive(size=3,aes(tooltip=paste(placeName, year,"\n Pop: ",population))) +
        geom_line()
    }
    ggiraph(code=print(pop.county.township.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
  })
  
#  output$popcitygraph <- renderggiraph({
    
#    pop.city.plot <- ggplot(filter(total.tidy.city, placeName %in% input$pop.city.list), aes(color=placeName, x=year, y=population)) +
      #scale_x_continuous(breaks=c(1910,1920,1930,1940,1950,1960,1970,1980,1990,2000,2010,2016))+
      #scale_y_continuous(labels=scales::percent)+
#      labs(x="Year", y="Population")+
#      theme_bar+
#      theme(axis.text.x = element_text(angle = 45, hjust = 1))

#    if (input$pop.city.vis.list == "Data points"){
#      pop.city.plot <- pop.city.plot + geom_point_interactive(size=3,aes(tooltip=paste(placeName, year,"\n Population of ",population)))
#    }
#    else if (input$pop.city.vis.list == "Trend lines"){
#      pop.city.plot <- pop.city.plot + geom_smooth(se=FALSE)
#    }
#    else if (input$pop.city.vis.list == "Both visualizations"){
#      pop.city.plot <- pop.city.plot + geom_point_interactive(size=3,aes(tooltip=paste(placeName, year,"\n Population of ",population))) +
#        geom_smooth(se=FALSE)
#    }
#    ggiraph(code=print(pop.city.plot), selection_type="none",hover_css = "r:7;",width_svg=10)
#  })
}

# Run the application 
shinyApp(ui = ui, server = server)