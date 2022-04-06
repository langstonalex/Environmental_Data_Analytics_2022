#### Load packages ----
library(shiny)
library(shinythemes)
library(tidyverse)

#### Load data ----
#Loaded the raw data with data from all lakes in the dataset rather than just Peter and Paul.
nutrient_data <- read_csv("./Data/NTL-LTER_Lake_Nutrients_Raw.csv")
nutrient_data$sampledate <- as.Date(nutrient_data$sampledate, format = "%m/%d/%y")
nutrient_data <- nutrient_data %>%
  filter(depth_id > 0) %>%
  select(lakename, sampledate:po4)

#### Define UI ----
#Changed theme to superhero and changed name of title panel
ui <- fluidPage(theme = shinytheme("superhero"),
  titlePanel("Nutrients in Northern Wisconisn Lakes"),
  sidebarLayout(
    sidebarPanel(
      
      # Select nutrient to plot
      selectInput(inputId = "y", 
                  label = "Nutrient",
                  choices = c("tn_ug", "tp_ug", "nh34", "no23", "po4"), 
                  selected = "tp_ug"),
      
      # Select depth
      checkboxGroupInput(inputId = "fill",
                         label = "Depth ID",
                         choices = unique(nutrient_data$depth_id),
                         selected = c(1, 7)),
      
      # Select lake
      #Added all the possible lakes to the checkbox field
      checkboxGroupInput(inputId = "shape",
                         label = "Lake",
                         choices = c("Peter Lake", "Paul Lake", "East Long Lake", "West Long Lake", "Tuesday Lake", "Central Long Lake", "Hummingbird Lake" ,  "Crampton Lake"  ,    "Brown Lake"  ,       "Bergner Lake"   ,   
                                     "Bolger Bog"   ,      "Bog Pot"     ,       "Cranberry Bog"     , "Eds Bog"     ,       "Forest Service Bog",
                                      "Inkpot Lake" ,       "Kickapoo Lake"  ,    "Morris Lake"    ,    "North Gate Bog"  ,   "Plum Lake"  ,       
                                      "Raspberry Lake"  ,   "Reddington Lake"  ,  "Roach Lake"   ,      "Tenderfoot Lake"  ,  "Ward Lake"  ,       
                                     "Tender Bog"),
                         selected = "Peter Lake"),

      # Select date range to be plotted
      sliderInput(inputId = "x",
                  label = "Date",
                  min = as.Date("1991-05-01"),
                  max = as.Date("2016-12-31"),
                  value = c(as.Date("1995-01-01"), as.Date("1999-12-31")))),

    # Output
    mainPanel(
      plotOutput("scatterplot", brush = brushOpts(id = "scatterplot_brush")), 
      tableOutput("mytable")
    )))

#### Define server  ----
server <- function(input, output) {
  
    # Define reactive formatting for filtering within columns
     filtered_nutrient_data <- reactive({
       nutrient_data %>%
         filter(sampledate >= input$x[1] & sampledate <= input$x[2]) %>%
         filter(depth_id %in% input$fill) %>%
         filter(lakename %in% input$shape) 
     })
    
    # Create a ggplot object for the type of plot you have defined in the UI  
     #Reconfigured the plot code to add more shapes to match more lakes
     #Changed from fill to color and changed the scheme to viridis
       output$scatterplot <- renderPlot({
        ggplot(filtered_nutrient_data(), 
               aes_string(x = "sampledate", y = input$y, 
                          color = "depth_id", shape = "lakename")) +
          geom_point(alpha = 0.8, size = 2) +
          theme_classic(base_size = 14) +
          scale_shape_manual(values = c(1:26)) +
          labs(x = "Date", y = expression(Concentration ~ (mu*g / L)), shape = "Lake", fill = "Depth ID") +
          #scale_color_distiller(palette = "YlOrBr", guide = "colorbar", direction = 1)
          scale_color_viridis_c(option = "viridis", begin = 0, end = 0.8, direction = -1)
      })
       
    # Create a table that generates data for each point selected on the graph  
       output$mytable <- renderTable({
         brush_out <- brushedPoints(filtered_nutrient_data(), input$scatterplot_brush)
       })
       
  }


#### Create the Shiny app object ----
shinyApp(ui = ui, server = server)

#### Questions for coding challenge ----
#1. Play with changing the options on the sidebar. 
    # Choose a shinytheme that you like. The default here is "yeti"
    # How do you change the default settings? 
    # How does each type of widget differ in its code and how it references the dataframe?
#2. How is the mainPanel component of the UI structured? 
    # How does the output appear based on this code?
#3. Explore the reactive formatting within the server.
    # Which variables need to have reactive formatting? 
    # How does this relate to selecting rows vs. columns from the original data frame?
#4. Analyze the similarities and differences between ggplot code for a rendered vs. static plot.
    # Why are the aesthetics for x, y, fill, and shape formatted the way they are?
    # Note: the data frame has a "()" after it. This is necessary for reactive formatting.
    # Adjust the aesthetics, playing with different shapes, colors, fills, sizes, transparencies, etc.
#5. Analyze the code used for the renderTable function. 
    # Notice where each bit of code comes from in the UI and server. 
    # Note: renderTable doesn't work well with dates. "sampledate" appears as # of days since 1970.
