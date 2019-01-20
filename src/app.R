

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
# 1) Define UI for app that draws a histogram ----
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
                         step = 1)
            
        ),
        
        # add main panel: 
        mainPanel(
            tabsetPanel(type = "tabs", 
                        
                        # first tab: 
                        tabPanel("Plot", plotOutput("plot")), 
                        
                        # second tab: 
                        tabPanel("Description",
                                 
                                 h3("The Collatz Conjecture"), 
                                 
                                 tags$div(
                                     tags$p("Let's say you're building a hospital emergency department (ED), and you need to decide how many beds to include in your design. To do so, you have to consider the demand for these beds, which depends on the number of patients that arrive every day (the 'arrival rate'), and the amount of time each patient requires for treatment (the 'service time')."), 
                                     tags$br()
                                 )
                                 
                        ), 
                        tabPanel("References", 
                                 
                                 tags$div(
                                     tags$p("1. Gross et al. Fundamentals of Queueing Theory. 2008, p. 69."), 
                                     tags$p("2. Hall, Randolph. Patient Flow: Reducing delay in healthcare delivery. 2013, p. 365"), 
                                     tags$p("3. Zai et al. 'Queuing Theory to Guide the Implementation of a Heart Failure Inpatient Registry Program'. J Am Med Inform Assoc., 2009"), 
                                     tags$p("4. Wiler et al. 'An emergency department patient flow model based on queueing theory principles'. Acad Emerg Med., 2013")
                                 )
                                 
                                 
                        )
            )
        )
        
        
        
    )
    
)






#************************************************************************
# 2) Define server logic required to draw a histogram ----
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
            set_names(c("result", "x"))  
        
        # draw plot: 
        df %>% 
            ggplot(aes(x = x, 
                       y = result)) + 
            
            geom_line() + 
            geom_point() + 
            
            scale_x_continuous(expand = c(0, 0)) + 
            scale_y_continuous(expand = c(0, 0)) + 
            
            labs(title = paste0("Starting number: ", 
                                df$result[1], 
                                "\nNum steps to get to 1: ", 
                                df$x[nrow(df)], 
                                " steps")) + 
            
            theme_classic(base_size = 12)
        
        
    }, 
    width = 800, 
    height = 600)
    
}




#*******************************************************************************
# 3) Call to shinyApp: ----------------
#*******************************************************************************

shinyApp(ui = ui, 
         server = server)








