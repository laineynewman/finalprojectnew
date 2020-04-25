
library(shiny)
library(markdown)
library(shinydashboard)

state_vector <- unique(data$state)
region_vector <- na.omit(unique(data$region))

library(markdown)
ui <- fluidPage(
    titlePanel("Union Density & Democratic Vote Share in the U.S. 1976-2016"),
navbarPage("",
           tabPanel("About",
                    img(src = "usw.png", height = 150, width = 175),
                    img(src = "uaw.png", height = 75, width = 75),
                    img(src = "white2.png", height = 40, width = 40),
                    img(src = "teamsters.png", height = 100, width = 100),
                    img(src = "white2.png", height = 40, width = 40),
                    img(src = "coal.png", height = 100, width = 100),
                    img(src = "white2.png", height = 30, width = 30),
                    img(src = "aflcio.png", height = 100, width = 100),
                    img(src = "white2.png", height = 30, width = 30),
                    img(src = "SEIU_250.png", height = 100, width = 120),
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
                    p("I did not find any statistically significant correlation between
                      union density and Democratic vote share over time at the national level. 
                      Though union density definitely plays a role in Democratic vote share, many,
                      many other things do as well. That being said, I believe it was hard to see
                      any trend at the national level with this data. I still ran several models
                      to test for any relationship and to test whether certain decades
                      indicated more of a relationship than others."),
                    p(""),
                    p("The first plot visualizes the correlation over time between Democratic vote share
                      in presidential elections and the average number of union members per state."),
                    p(""),
                    p("The second plot visualizes the relationship between the number of 
                      members per election year in a state and the proportion of votes 
                      to the Democratic candidate each election year."),
                    p(""),
                    p("The third series of plots breaks down the visualization from the second
                      graph into decades. In this, I was testing to see if there was any
                      particular decade in which there was more of a relationship between
                      these two variables."),
                    mainPanel(
                        plotOutput("Plot5"),
                        p(""),
                        plotOutput("Plot3"),
                        p(""),
                        plotOutput("Plot4")
                    )
           ),
           tabPanel("Regional Data",
                    h3("Regional Data on Union Density"),
                    p("Regional Data"),
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("select_region",
                                        "Select Region",
                                        choices = region_vector)
                        ),
                        mainPanel(
                            plotOutput("Plot2")
                        )
                    )
                      ),
           tabPanel("State Data",
                    h3("State-Level Data on Union Density"),
                    p("This interactive feature allows you to visualize union 
                    density trends over time in individual states
                    in relation to Democratic vote share 
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
            ggtitle(input$select_state) +
            theme_bw()

    })
    
    output$Plot2 <- renderPlot({
        data %>%
            filter(party == "democrat") %>%
            ungroup(year) %>%
            group_by(region) %>%
            filter(region == input$select_region) %>%
            ggplot(aes(x = year)) +
            geom_line(aes(y = regional_mean_members, color = "Union Density")) + 
            geom_line(aes(y = regional_mean_votes * 100, color = "Democratic Vote Share")) +
            labs(y = "Percent") + 
            ggtitle(input$select_region) +
            theme_bw()
        
    })
    
    output$Plot3 <- renderPlot({
        data %>%
            ggplot(aes(x = number_members, y = prop_votes)) + 
            geom_point() + 
            geom_smooth(method = "lm") +
            ggtitle("Visualization of Regression Over Time Between Union Density \nand Democratic Vote Share") +
            theme_bw() +
            labs(x = "Average Percent of State that is Unionized Over Time",
                 y = "Average Proportion of Democratic Vote Share Per State Over Time")
        
    })
    
    output$Plot4 <- renderPlot({
        data %>%
            ggplot(aes(x = number_members, y = prop_votes)) + 
            geom_point() + 
            geom_smooth(method = "lm") + facet_wrap( ~ decade) +
            theme_bw() +
            ggtitle("Regression Broken Down by Decade") +
            labs(y = "Proportion of Democratic Vote Share",
                 x = "Percent of State that is Unionized")
    })
    
    output$Plot5 <- renderPlot({
        data %>%
            ggplot(aes(x = year, y = correlation)) + 
            geom_line() +
            theme_bw() +
            ggtitle("Correlation Between Union Density and Democratic Vote Share Over Time") +
            labs(x = "Year", y = "Correlation Coefficient")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
