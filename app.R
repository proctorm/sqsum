# IOC April Challenge

# Find the difference between the square of the sum of the numbers 1 to 100, and
# the sum of the squares of the numbers 1 to 100

# We will generalise and allow the number to sum to to be an input to the
# calculation

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("IOC April Challenge"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "sum_to",
                "Number to sum to:",
                min = 1,
                max = 100,
                value = 10
            ), 
            sliderInput(
                'num_points', 
                'Number of random points to generate: ', 
                min = 10, 
                max = 1e4, 
                value = 1000
            ), 
            radioButtons(
                'big_square_size_calc', 
                'How to calculate sum of series?', 
                c('Analytical', 'Monte Carlo')
            ), 
            conditionalPanel(
                "input.big_square_size_calc === 'Monte Carlo'", 
                uiOutput('square_size_est_ui')
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("square_plot"), 
           verbatimTextOutput('mc_result')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # UI to specify number of samples to take when estimating sum of series
    # using Monte Carlo
    output$square_size_est_ui <- renderUI(
        sliderInput(
            'square_size_mc_samples', 
            'Number of samples to take for estimation: ', 
            min = 10, 
            max = 1000, 
            value = 50
        )
    )
    
    sum_series <- reactive(1:input$sum_to * (1:input$sum_to + 1) / 2)
    final_sum_series <- reactive(
        if (input$big_square_size_calc == 'Analytical') {
            sum_series()[length(sum_series())]
        } else {
            # Monte Carlo method to determine sum of series
            input$sum_to * mean(
                runif(input$square_size_mc_samples) * (input$sum_to - 1) + 1
            )
        }
    )
    
    random_points_df <- reactive({
        tibble(
            x = runif(input$num_points) * final_sum_series(), 
            y = runif(input$num_points) * final_sum_series()
        ) %>% 
            rowwise() %>% 
            mutate(
                # Find which square the x coordinate lies in. Since each
                # square goes up by 1, this coincidentally is also the max y
                # value for the random point to lie inside the square.
                in_squares = y <= which.max(x <= sum_series())
            )
    })

    output$square_plot <- renderPlot({
        
        squares_df <- tibble(
            x2 = sum_series(), 
            x1 = lag(sum_series(), default = 0), 
            y1 = rep(0, input$sum_to), 
            y2 = 1:input$sum_to
        )
        
        big_square_df <- tibble(
            x1 = 0, x2 = final_sum_series(), 
            y1 = 0, y2 = x2
        )
        
        ggplot() + 
            geom_rect(
                aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), 
                squares_df, 
                colour = 'white'
            ) + 
            geom_rect(
                aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), 
                big_square_df, 
                alpha = 0, 
                colour = 'black'
            ) + 
            geom_point(
                aes(x = x, y = y, colour = in_squares), 
                random_points_df(), shape = 'x', size = 3
            ) + 
            coord_fixed() # Makes fixed 1:1 aspect ratio
    })
    
    output$mc_result <- shiny::renderText({
        mc_proportion <- (1 - mean(random_points_df()$in_squares))
        mc_result <- mc_proportion * final_sum_series() ^ 2
        actual_result <- sum(1:input$sum_to) ^ 2 - sum((1:input$sum_to) ^ 2)
        mc_error <- mc_result - actual_result
        mc_error_pct <- 100 * mc_error / actual_result
        c('\n', 
          'Proportion of points outside of squares: ', mc_proportion * 100, '%', 
          '\n',
          'Estimated difference: ', mc_result, 
          '\n', 
          'Difference from true result: ', round(mc_error, 3), 
          '\n', 
          'Error as a percentage: ', round(mc_error_pct, 1), '%', 
          '\n', '\n'
          )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
