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
library(shinydashboard)

state_vector <- unique(data$state)

library(markdown)
ui <- fluidPage(
    titlePanel("Labor Union Density in the U.S. 1976-2016"),
navbarPage("",
           tabPanel("About",
                    img(src = "usw.png"),
                    h3("Background about this Data Analysis"),
                    p("For my final project for Gov 1005, I decided to focus on union density 
                      over time in the United States. This is related to my thesis topic, which is
                      described below. In my data analyses on this page, I look at national, regional,
                      and state-level trends regarding union density and Democratic presidential
                      vote share. For the election returns data, I used state-level presidential election returns from the Statistics 
                      of the Congressional Election, published by the U.S. House of Representatives. 
                      For the union density data, I used Barry T. Hirsch, David A. Macpherson, and Wayne G. Vroman's “Estimates of Union Density by State,” 
                      from the Monthly Labor Review (obtained via NPR)."),
                    h3("Why I Chose This Topic"),
                    p("Throughout much of American history, labor unions have played a 
                    significant role in the unity between progressive politics and the 
                    working class. Unions have historically been one of the 
                    recognizable American institutions that, among other 
                    responsibilities and activities, have allowed for social 
                    and political cohesion between members as well as affiliates. 
                    Because the labor union has historically been seen as the 
                    counterweight to big business or corporate interests, a common 
                    conception has been that union members support Democratic candidates, 
                    who more forcefully champion policies that support working class people than Republican 
                    candidates."),
                      p(""),
                    p("However, union household support for Democratic candidates has 
                    wavered over time. In recent years, union household support for 
                    Democratic candidates has decreased and support for Republican candidates 
                    has increased. In 1992, union households supported the Democratic 
                    presidential candidate over the Republican candidate by a margin of 
                    31 percentage points. As of 2016, that gap narrowed to 8 percentage 
                    points. This phenomenon has occurred despite data that have suggested 
                    that labor unions are becoming increasingly diverse."),
                    p(""),
                    p("The research topic that am studying for my senior thesis 
                      is the evolution—or devolution—of social and political 
                      cohesion within industrial labor unions, using the region 
                      of Western Pennsylvania as a representative case study. 
                      I plan to focus on how union members’ social identity has 
                      changed over time as it relates to their membership to the union. My overarching 
                      research question is: How have union members’ social identities, 
                      as they relate to the union, evolved over time, and how has
                      this evolution affected the political choices of union members 
                      and union households? My theory is that, over time, 
                      the extent to which union members’ social identities are tied 
                      to the union has declined. The decrease in social salience 
                      of the union for individual members and households has in 
                      turn caused decreased adherence to union-backed political candidates.")                      
                    ),
           tabPanel("National Data",
                    h3("National Data on Union Density"),
                    p("National Data")
           ),
           tabPanel("Regional Data",
                    h3("Regional Data on Union Density"),
                    p("Regional Data"),
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
                    h3("State-Level Data on Union Density"),
                    p("This interactive feature allows you to visualize union 
                    density trends over time in relation to Democratic vote share 
                    in presidential elections from 1976 to 2016. 
                      In the drop down menu, select your state of choice. 
                      That state wil appear in the graph on the right."),
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
