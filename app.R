#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(tidyverse)

# Reference: Git Set-Up: https://happygitwithr.com/

# CHAPTER 1: Most Common Variables - Country GDP, Total GDP, Total Pop, Mean Prod per Capita #

data <- read_xlsx("WEOOct2026all.xlsx", "Countries")
data1 <- data %>% filter (INDICATOR.ID %in% c ("NGDPRPPPPC","LP") )

years <- as.character(1980:2025)
df1 <- data1  %>% select( COUNTRY.ID, INDICATOR.ID,  all_of(years))
df1_gdppc <- df1 %>% filter (INDICATOR.ID == "NGDPRPPPPC")
df1_pop <- df1 %>% filter (INDICATOR.ID == "LP") %>% subset()
# 10 countries with the most populations in 2025


# NGDPRPPP is not available to me through WEO, so I calculate it by multiplying NGDPRPPPPC and LP.
NGDPRPPP <- data.frame(matrix(ncol=48, nrow=197))
colnames(NGDPRPPP) <- c("COUNTRY.ID","INDICATOR.ID", years)
NGDPRPPP [,1] <- df1_gdppc[,1]
NGDPRPPP [,2] <- "NGDP"
NGDPRPPP [,3:48] <- mapply('*', df1_gdppc[3:48], df1_pop[3:48]) # Multiplication

TotalGDP <- data.frame(matrix(ncol=48, nrow=1))
colnames(TotalGDP) <- c("COUNTRY.ID","INDICATOR.ID", years)
TotalGDP[1:2] <- c("Global","Total GDP")
TotalGDP[3:48] <- colSums(NGDPRPPP[3:48], na.rm = TRUE)

TotalPop <- data.frame(matrix(ncol=48, nrow=1))
colnames(TotalPop) <- c("COUNTRY.ID","INDICATOR.ID", years)
TotalPop[1:2] <- c("Global","Total Pop")
TotalPop[3:48] <- colSums(df1_pop[3:48], na.rm = TRUE)

MeanPPC <- data.frame(matrix(ncol=48, nrow=3))
colnames(MeanPPC) <- c("COUNTRY.ID","INDICATOR.ID", years)
MeanPPC[1,] <- TotalGDP
MeanPPC[2,] <- TotalPop
MeanPPC[3,1:2] <- c("Global", "Global Mean Prod per Capita")
MeanPPC[3,3:48] <- as.numeric( MeanPPC[1,3:48] / MeanPPC[2,3:48]) # Total GDP / Total Pop


# Some other variables
selected_pop10 <- df1_pop[order(df1_pop[[48]], decreasing = TRUE, na.last = NA)[1:10], ]
unique_countries2 <- unique(data1[, c("COUNTRY.ID", "COUNTRY")])
new_row <- data.frame(COUNTRY.ID = "GM",  COUNTRY = "Global Mean" )  
unique_countries1 <- bind_rows( new_row, unique_countries2)
# View(unique_countries1)

# For Chapter 2
MeanPPCv2 <- MeanPPC
MeanPPCv2[3,1:2] <- c("GM", "NGDPRPPPPC")
combined_gdppc <- bind_rows(df1_gdppc, MeanPPCv2[3,])

# For Chapter 2
NGDPPC_long <- combined_gdppc %>%
  pivot_longer(cols = 3:48, names_to = "year", names_transform = list(year = as.integer), values_to = "gdppc") %>%
  mutate(year = as.numeric(year)) 

# For Chapter 3
RelativeNGDPPC <- data.frame(matrix(ncol=48, nrow=197))
RelativeNGDPPC [1:197,1] <- df1_pop[,1] # Country Names to the first Column
RelativeNGDPPC [1:197,2] <- "Relative GDP per Capita"
RelativeNGDPPC [1:197,3:48] <- as.numeric(mapply('/', df1_gdppc[3:48], MeanPPC[3,3:48]))
colnames(RelativeNGDPPC) <- c("COUNTRY.ID","INDICATOR.ID", years)

# For Chapter 3
RelativeNGDPPC_long <- RelativeNGDPPC %>%
  pivot_longer(
    cols = 3:48,
    names_to = "year",
    names_transform = list(year = as.integer),
    values_to = "rngdppc"
  ) %>%
  mutate(year = as.numeric(year))

# END OF CHAPTER 1 # 



# Define UI with css
ui <- fluidPage(
    theme = shinytheme ("spacelab"),
    tags$head(
      tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "NGDPRPPPPC_Relative_to_GM.css"
      )),

    # Application title
    titlePanel("NGDPRPPPPC Relative to GM"),
    
    #Navbar
    navbarPage ("Tabs:",
      
      tabPanel("Info",
               h2("Disclaimer", style = "text-align: center;"),
               p("This Shinny App is made for learning purposes. It may contain mistakes.", br(), "Data received from IMF Database at 29.10.2026.", br(), "Complete R-Code will be shared at the end of the project. (Estimated: End of 2026.)"),
               h2("ID, Name and Descriptions for Used IMF-Indicators", style = "text-align: center; margin-top: 50px;"),
               h3("NGDPRPPPPC"),
               h4("Gross domestic product (GDP), Constant prices, Per capita, purchasing power parity (PPP) international dollar, ICP benchmark 2021"),
               p("Gross domestic product reflects the total income earned through the production of goods and services in an economic territory during an accounting period. It can be measured in three different ways: using either the expenditure approach, the income approach, or the production approach.  Values that have had the effect of price changes removed, expressed as time series in constant prices (fixed base) or chain-linked volume terms, or as non-time series in previous year's prices Average income or economic output per person adjusted for cost-of-living differences between countries, expressed in international dollars. ICP benchmark 2021"),
               h3("LP", style = "margin-top: 20px;"),
               h4("Population, Persons for countries / Index for country groups"),
               p("The total number of people living in a specific area or country. Persons for countries / Index for country groups"),
               h2("Methods", style = "text-align: center; margin-top: 50px;"),
               p("GM - Global Mean:", br(), "I multiplied NGDPRPPPPC with LP for each year and for each country, called it NGDPRPPP.", br(),
                  "I summed NGDPRPPP for each year, called it TotalGDP.", br(),
                  "I summed LP for each year, called it TotalPop.", br(),
                  "I divided TotalGDP to TotalPop for each year, called it MeanPPC - Mean Production per Capita / later called it GM - Global Mean.", br(),
                  "The empty values are not filled. I see that as a possible cause of error, but I didn't filled them with estimations, because I need to check first which country founded in which year.", br(), 
                  "In the tab 'NGDPRPPPPC and GM', I visualize that GM among countries' GDP per Capita over the years.", br(),
                  "Later I divide each NGDPRPPPPC to GM for each year, creating the NGDPRPPPPC relative to GM.", br(),
                  "In the tab 'NGDPRPPPPC Relative to GM (& Base = 1)', I visualize that GDP per Capita relative to GM, and GM as base = 1.",br(),br(),
                  "The top 10 most populous countries are selected based on the populations in 2025."),
               h2("Link to Raw Data:", style = "text-align: center; margin-top: 50px;"), 
               p("https://data.imf.org/-/media/iData/External-Storage/Documents/0FA3DC49A04D446E9D69DBEB29979E1C/en/WEOOct2026all.xlsx"),
               a("Click to Download" , href = "https://data.imf.org/-/media/iData/External-Storage/Documents/0FA3DC49A04D446E9D69DBEB29979E1C/en/WEOOct2026all.xlsx"),
               h2("Contact", style = "text-align: center; margin-top: 50px;"),
               p("For any questions you can contact me via email at ergun.yagmurkaya@gmail.com"),
               a("Click to send email", href = "mailto:ergun.yagmurkaya@gmail.com?subject=Question%20about%20the%20Shiny%20app%20NGDPRPPPPC_Relative_to_GM&body=Hello,"),
               br(),
               ), # End of tabPanel Info
      
      tabPanel("Table" ,
               h6("This table is for users that want to see the numbers, which are at the right-end of the table, but not want download some or any file. It allows regex, e.g. for AUT and AUS together use in filter: ^(AUS|AUT)$ "),
               DTOutput("mytable", width = "100%", height = "90%") # End of Table
      ),    
      
      tabPanel("NGDPRPPPPC and GM" ,
               sidebarPanel( width = 3,
                             class = "checkbox-scrollable",
                             tags$p("At page-load, the top 10 most populous countries are selected based on the populations in 2025."),
                             tags$h3("Filters:"),
                             checkboxGroupInput(
                               "checkbox_group_1", 
                               "Countries", 
                               choices = setNames(
                                 unique_countries1$COUNTRY.ID,
                                 paste(
                                   unique_countries1$COUNTRY.ID,
                                   unique_countries1$COUNTRY,
                                   sep = " - "
                                 )
                               ),
                               selected = c(selected_pop10$COUNTRY.ID, "GM"),
                             ), # End of checkboxGroupInput 
               ), # End of sidebarPanel 
               
               mainPanel( width = 9,
                          h3("NGDPRPPPPC and GM"), 
                          plotOutput("NGDPPC_long")
               ) # End of mainPanel
               
      ), # End of tabPanel NGDPRPPPPC and GM
      
      tabPanel ("NGDPRPPPPC Relative to GM (& Base = 1)",
                sidebarPanel( width = 3,
                  class = "checkbox-scrollable",
                  tags$p("At page-load, the top 10 most populous countries are selected based on the populations in 2025."),
                  tags$h3("Filters:"),
                  checkboxGroupInput(
                    "checkbox_group_2", 
                    "Countries", 
                    choices = setNames(
                      unique_countries2$COUNTRY.ID,
                      paste(
                        unique_countries2$COUNTRY.ID,
                        unique_countries2$COUNTRY,
                        sep = " - "
                      )
                    ),
                    selected = selected_pop10$COUNTRY.ID,
                  ), # End of checkboxGroupInput 
                ), # End of sidebarPanel 
                
                mainPanel( width = 9,
                  h3("NGDPRPPPPC Relative to GM (& Base = 1)"), 
                  plotOutput("RelativeNGDPPC_long")
                ) # End of mainPanel
                
                ), # End of tabPanel NGDPRPPPPC Relative to GM (& Base = 1)
                
      tabPanel("Graphs with User-Input" , "This is empty")
      
        
    ) # End of navbarPage My First App
) # End of fluidPage

# Define server logic required to draw a histogram
server <- function(input, output) {                                              ### SERVER ###


  #TABLE                                                                          #TABLE

  output$mytable <- 
    renderDT({
    desc_col <- which(names(data) %in% c("INDICATOR.Description","INDICATOR"))
    datatable(data, 
      escape = setdiff(names(data), c("INDICATOR.Description", "INDICATOR")),
      options = list(
        paging = TRUE,
        autoWidth = FALSE,
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        pageLength = 200,
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        search = list(regex = TRUE),
        columnDefs = list(
          list(
            targets = desc_col,
            render = JS(
        "function(data, type, row, meta) {",
        "return type === 'display' && data.length > 90 ?",
        "'<span title=\"' + data + '\">' + data.substr(0, 90) + '...</span>' : data;",
        "}")
        

    ))),
    extensions = 'Buttons',
    selection = 'multiple',
    filter    = "top"
    )    
})

# Reference:  https://clarewest.github.io/blog/post/making-tables-shiny/
# Reference:  https://stackoverflow.com/questions/51261903/r-shiny-collapse-long-rows-in-data-table

  # GRAPHS                                                                        # GRAPHS

  flag_weighted_palette <- readRDS("flag_weighted_palette.rds")
  all_country_colors <- setNames(
    flag_weighted_palette$unified_hex,
    flag_weighted_palette$iso_alpha3
  )
  # View(all_country_colors)
    

 
  ## CHAPTER 2: Visualizing the Countries Together and Global Mean: 
  
  

  

  # Visualizing Together
  output$NGDPPC_long <- renderPlot({ 
    
    selected_data1 <- NGDPPC_long %>%
      dplyr::filter(COUNTRY.ID %in% input$checkbox_group_1)
    # print(head(selected_data1))
    
    # To get the ISO Codes at the right end of the graph.
    # Get the last observation for each country
    last_values <- NGDPPC_long %>%
      group_by(COUNTRY.ID) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::filter(COUNTRY.ID %in% input$checkbox_group_1)
    # View(last_values)
    
    # Visualizing Together
    # View(NGDPPC_long)
    ggplot(selected_data1, aes(x = year, y = gdppc, color = `COUNTRY.ID`, group = `COUNTRY.ID`)) +
      geom_line() + scale_color_manual(values = all_country_colors) + 
      geom_text_repel( data = last_values, aes( x = year, y = gdppc, label = COUNTRY.ID, color = COUNTRY.ID ),
                       inherit.aes = FALSE, direction = "y", hjust = 0, nudge_x = 1, force = 1, force_pull = 5, box.padding = 0.15, point.padding = 0.1,
                       max.overlaps = Inf, max.iter = 10000, max.time = 2, segment.color = NA, show.legend = FALSE ) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))  ) +
      coord_cartesian(clip = "off") +
      labs(x = "Year", y = "GDP per Capita Relative to Global Mean", title = "GDP per Capita relative to Global Mean Prod. per Capita (Base = 1 red dashed)") +
      theme(legend.position = "none")
    # plot.margin = margin(5.5, 35, 5.5, 5.5)
  }, height = 700, res = 96)   

  # END OF CHAPTER 2: Visualizing the Countries Together and Global Mean #
  
  
  ## CHAPTER 3: Visualizing the top Countries Together Relative to Global Mean Base = 1 #
 
   
  output$RelativeNGDPPC_long <- renderPlot({ 
    
    selected_data2 <- RelativeNGDPPC_long %>%
      dplyr::filter(COUNTRY.ID %in% input$checkbox_group_2)
    # print(head(selected_data2))
    
    # To get the ISO Codes at the right end of the graph.
    # Get the last observation for each country
    last_values <- RelativeNGDPPC_long %>%
      group_by(COUNTRY.ID) %>%
      filter(year == max(year, na.rm = TRUE)) %>%
      ungroup() %>%
      dplyr::filter(COUNTRY.ID %in% input$checkbox_group_2)
    # View(last_values)

    # View(RelativeNGDPPC_long)
    # Visualizing Together
    
    ggplot(selected_data2, aes(x = year, y = rngdppc, color = `COUNTRY.ID`, group = `COUNTRY.ID`)) +
      geom_line() + scale_color_manual(values = all_country_colors) + 
      geom_text_repel( data = last_values, aes( x = year, y = rngdppc, label = COUNTRY.ID, color = COUNTRY.ID ),
        inherit.aes = FALSE, direction = "y", hjust = 0, nudge_x = 1, force = 1, force_pull = 5, box.padding = 0.15, point.padding = 0.1,
        max.overlaps = Inf, max.iter = 10000, max.time = 2, segment.color = NA, show.legend = FALSE ) +
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))  ) +
      geom_segment( data = data.frame(
          x = min(selected_data2$year),
          xend = max(selected_data2$year),
          y = 1,
          yend = 1
        ),
        aes(x = x, xend = xend, y = y, yend = yend),
        inherit.aes = FALSE,
        linetype = "dashed",
        color = "red"
      ) +
      coord_cartesian(clip = "off") +
      labs(x = "Year", y = "GDP per Capita Relative to Global Mean", title = "GDP per Capita relative to Global Mean Prod. per Capita (Base = 1 red dashed)") +
      theme(legend.position = "none")
    # plot.margin = margin(5.5, 35, 5.5, 5.5)
  }, height = 700, res = 96)   
  

  # END OF CHAPTER 3: Visualizing the top Countries Together against Global Mean Base = 1 #
  
}

# Run the application 
shinyApp(ui = ui, server = server)


