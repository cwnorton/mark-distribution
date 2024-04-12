#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(gridExtra)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mark distribution"),

    # Sidebar with a slider input for number of bins 
    sidebarPanel(
      radioButtons("assessment_type", "Assessment type", 
                   choices = c('UG', 'PG')),
      textInput("raw_marks", "Marks")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot", height = "800px")
    )

    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      if (input$assessment_type == 'UG') {
        band_breaks <- c(-Inf, 39, 49, 59, 69, 79, Inf)
        band_labels <- c('Fail','3rd','2:2', '2:1', '1st', '1st')
        bands <- tribble(
          ~start, ~end,  ~classification,
                0,    40, 'Fail',
               40,    50, '3rd',
               50,    60, '2:2',
               60,    70, '2:1',
               70,   100, '1st '
        )
      } else if (input$assessment_type == 'PG') {
        band_breaks <- c(-Inf, 49, 59, 69, 79, Inf) 
        band_labels <- c('Fail','Pass','Merit', 'Distinction', 'Distinction')
        bands <- tribble(
          ~start, ~end,  ~classification,
                0,    50, 'Fail',
               50,    60, 'Pass',
               60,    70, 'Pass (merit)',
               70,    100, 'Pass (distinction)'
        )
      }
    
      marks <- strsplit(input$raw_marks, '[^0-9]', perl=TRUE)[[1]] |>
        as.integer()  |>
        na.omit(marks) %>%
        # Convert into tibble with categorised bands.
        tibble(mark=.) |>
        mutate(band=cut(mark, breaks=band_breaks, labels=band_labels))
              
      barchart <- ggplot(marks, aes(x=band, fill = band)) + geom_bar() 
      low_mark <- min(marks$mark)
      high_mark <- max(marks$mark)
      
      bands <- bands |>
        mutate(start = ifelse(low_mark >= start & low_mark < end, low_mark, start),
               end = ifelse(high_mark >= start & high_mark < end, high_mark, end)) |>
        filter(start >= low_mark, end <= high_mark)
      density_plot <- ggplot() +
        geom_rect(data = bands, aes(xmin = start, xmax = end, ymin = -Inf, 
                                    ymax = Inf, fill = classification), alpha = 0.2) +
        geom_density(data = marks, aes(x=mark)) +
        xlab(sprintf('Marks (Mean = %s, SD = %s)', 
                     mean(marks$mark, na.rm = TRUE) |> ceiling(), 
                     sd(marks$mark, na.rm = TRUE)|> ceiling()) 
             ) +
        xlim(min(bands$start), max(bands$end)) +
        geom_text(
               aes(x = start + (end - start) / 2, y = 0.001, label = classification),
               data = bands,
               size = 5, vjust = 0, hjust = 0, nudge_x = 0, check_overlap = TRUE
             ) +
        theme(legend.position = "none")
      
      box_plot <- ggplot() +
        geom_rect(data = bands, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, 
                                    fill = classification), alpha = 0.2) +
        geom_boxplot(data = marks, aes(x=mark)) +
        xlab(sprintf('Marks (Median = %s, IQR = %s)', 
                     median(marks$mark, na.rm=TRUE) |> ceiling(), 
                     IQR(marks$mark, na.rm=TRUE) |> ceiling()) 
             ) +
        xlim(min(bands$start), max(bands$end)) +
        theme(legend.position = "none")
        
      grid.arrange(barchart, density_plot, box_plot, ncol=1, nrow =3, top = 'Marks')
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
