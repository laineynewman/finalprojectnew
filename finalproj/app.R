#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(markdown)

state_vector <- unique(data$state)

library(markdown)
ui <- fluidPage(
navbarPage("Navbar!",
           tabPanel("About"
           ),
           tabPanel("National Data",
                    titlePanel("Visualization of Union Density Data Across U.S."),
                    p("National Data")
           ),
           tabPanel("Regional Data",
                    titlePanel("Visualization of Union Density Data Across U.S."),
                    p("National Data"),
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("select_state",
                                        "Select State",
                                        choices = state_vector)
                        ),
                        mainPanel(
                            plotOutput("Plot2")
                        )
                    )
                      ),
           tabPanel("State Data",
                    titlePanel("Visualization of Union Density Data Across U.S."),
                    p("sldsfjsd"),
                    sidebarLayout(
                        sidebarPanel(
                    selectInput("select_state",
                                "Select State",
                                choices = state_vector)
           ),
           mainPanel(
               plotOutput("Plot1")
           )
           )
           )
))


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Plot1 <- renderPlot({
        data %>%
            filter(party == "democrat") %>%
            ungroup(year) %>%
            group_by(state) %>%
            filter(state == input$select_state) %>%
            ggplot(aes(x = year)) +
            geom_line(aes(y = number_members, color = "Union Density")) + 
            geom_line(aes(y = prop_votes * 100, color = "Democratic Vote Share")) +
            labs(y = "Percent") + 
            ggtitle(input$select_state)

    })
    
    output$Plot2 <- renderPlot({
        data %>%
            filter(party == "democrat") %>%
            ungroup(year) %>%
            group_by(state) %>%
            filter(state == input$select_state) %>%
            ggplot(aes(x = year)) +
            geom_line(aes(y = number_members, color = "Union Density")) + 
            geom_line(aes(y = prop_votes * 100, color = "Democratic Vote Share")) +
            labs(y = "Percent") + 
            ggtitle(input$select_state)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
