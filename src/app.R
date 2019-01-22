

#**********************************************
# Collatz conjecture calculator  
# 2019-01-20
# Nayef 

# references: 
# > https://en.wikipedia.org/wiki/Collatz_conjecture 
# > https://xkcd.com/710/ 

#**********************************************

library(tidyverse)

# function definition: --------
collatz_fn <- function(pos_integer){
    
    count <- 1 
    
    container <- vector()
    container[1] <- pos_integer
    
    while (pos_integer != 1){
        count <- count + 1 
        
        if (pos_integer %% 2 == 0){
            pos_integer <- pos_integer/2
            
        } else {
            pos_integer <- pos_integer*3 + 1
        }
        
        container[count] <- pos_integer
        
    }
    
    return(container)
      
}



# test the function: ---------------------
# collatz_fn(3)

# df <- collatz_fn(34) %>% 
#     as.data.frame() %>% 
#     mutate(x = 1:n()) %>% 
#     set_names(c("result", "x"))  
    
# df %>% 
#     ggplot(aes(x = x, 
#                y = result)) + 
#     
#     geom_line() + 
#     geom_point() + 
#     
#     scale_x_continuous(expand = c(0, 0)) + 
#     scale_y_continuous(expand = c(0, 0)) + 
#     
#     labs(title = paste0("Starting number: ", 
#                         df$result[1], 
#                         "\nNum steps to get to 1: ", 
#                         df$x[nrow(df)], 
#                         " steps")) + 
#     
#     theme_classic(base_size = 12)





#*****************************************
# 1) Define UI for app that draws the Collatz series ----
#*****************************************

ui <- fluidPage(
    
    # > App title ----
    titlePanel("The Collatz Conjecture"),
    
    # > customize sidebar: ----------------
    # >> Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # other layout options: http://shiny.rstudio.com/articles/layout-guide.html 
        # > fluidRow( )
        # > tabSetPanel( ) : to be used within mainPanel below
        # > navlistPanel( )
        # > navbarPage( ) 
        # > navbarMenu()
        
        
        # > Sidebar panel for inputs ----
        sidebarPanel(
            
            # input number to start the series 
            numericInput(inputId = "input_integer", 
                         label = "Input integer", 
                         value = 10, 
                         step = 1), 
            
            # input number to start the x-axis 
            numericInput(inputId = "start_x_axis", 
                         label = "Start x-axis at \n(use Up and Down arrow keys to change)", 
                         value = 1, 
                         step = 5)
            
        ),
        
        # add main panel: 
        mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        # first tab: 
                        tabPanel("Plot",  # name of tab to display  
                                 plotOutput("plot")),  # what to display in the tab
                        
                        # second tab: 
                        tabPanel("Description",
                                 
                                 h3("From Wikipedia:"), 
                                 
                                 tags$div(
                                     tags$p("The Collatz conjecture is a conjecture in mathematics that concerns a sequence defined as follows: start with any positive integer n. Then each term is obtained from the previous term as follows: if the previous term is even, the next term is one half the previous term. If the previous term is odd, the next term is 3 times the previous term plus 1. The conjecture is that no matter what value of n, the sequence will always reach 1."), 
                                     tags$br()
                                 ), 
                                 
                                 h3("Try this: "), 
                                 
                                 tags$div(
                                     tags$p("The algorithm is not complex, so you can put in pretty huge numbers. Try putting in your phone number, then deleting digits one by one to see how the results change. Starting from my phone number, it takes 259 steps to get to 1."), 
                                     tags$br()
                                 ),
                                 
                                 h3("Also see: "), 
                                 
                                 tags$div(
                                     tags$p("https://xkcd.com/710/"), 
                                     tags$br()
                                 ), 
                                 
                                 tags$div(
                                     tags$p("https://github.com/nayefahmad/collatz-conjecture"), 
                                     tags$br()
                                 )
                                 
                        )
            )
        )
        
        
        
    )
    
)






#************************************************************************
# 2) Define server logic required to draw the plot ----
#************************************************************************

server <- function(input, output) {
    
    # Graph of Collatz series: ------
    # This expression that generates a plot is wrapped in a
    # call to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be
    #   automatically re-executed when inputs change
    # 2. Its output type is a plot
    
    output$plot <- renderPlot({
        
        # create dataframe to plot: 
        df <- collatz_fn(input$input_integer) %>% 
            as.data.frame() %>% 
            mutate(x = 1:n()) %>% 
            set_names(c("result", "x")) %>% 
            
            # set starting point for graph: 
            slice(input$start_x_axis:n())
        
        # draw plot: 
        df %>% 
            ggplot(aes(x = x, 
                       y = result)) + 
            
            geom_line() + 
            geom_point() + 
            
            scale_x_continuous(expand = c(0, 0)) + 
            scale_y_continuous(expand = c(0, 0)) + 
            
            labs(title = paste0("Starting number: ", 
                                input$input_integer, 
                                "\nNum steps to get to 1: ", 
                                df$x[nrow(df)], 
                                " steps"), 
                 
                 subtitle = paste0("Showing results from step: ", 
                                   input$start_x_axis)) + 
            
            theme_classic(base_size = 16)
        
        
    }, 
    width = 800, 
    height = 600)
    
}




#*******************************************************************************
# 3) Call to shinyApp: ----------------
#*******************************************************************************

shinyApp(ui = ui, 
         server = server)








