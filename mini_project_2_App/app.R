library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(plotly)
library(stringr)
library(gridExtra)
library(shiny)
library(tigris)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(sf)
library(rvest)
library(janitor)
library(RColorBrewer)
library(readr)
library(ggthemes) 
library(tidytext)
library(tidyr)
library(purrr)
library(polite)
library(shinybusy)
library(viridis)
library(maps)
library(ggplot2movies)
library(DT)
library(scales)



session <- bow("https://wdi.worldbank.org/table/2.18")
scraped_page <- scrape(session)

tables <- html_elements(scraped_page, "table.indicators-table")

length(tables)

extracted_tables <- lapply(tables, function(table) {
  table %>%
    html_table(fill = TRUE) %>%
    janitor::clean_names()
})

combined_data <- bind_rows(extracted_tables)
head(combined_data)



combined_data1 <- combined_data[, c(14, 1:13, 15:ncol(combined_data))]
combined_data2 <- combined_data1 %>% select(-x)


combined_data2$life_expectancy_at_birth[4:nrow(combined_data2)] <- ifelse(
  is.na(combined_data2$life_expectancy_at_birth[4:nrow(combined_data2)]),
  combined_data2$x2[4:nrow(combined_data2)],
  combined_data2$life_expectancy_at_birth[4:nrow(combined_data2)]
)
combined_data3 <- combined_data2 %>% select(-x2)

combined_data3$life_expectancy_at_birth_2[4:nrow(combined_data3)] <- ifelse(
  is.na(combined_data3$life_expectancy_at_birth_2[4:nrow(combined_data3)]),
  combined_data3$x3[4:nrow(combined_data3)],
  combined_data3$life_expectancy_at_birth_2[4:nrow(combined_data3)]
)
combined_data4 <- combined_data3 %>% select(-x3)

combined_data4$neonatal_mortality_rate[4:nrow(combined_data4)] <- ifelse(
  is.na(combined_data4$neonatal_mortality_rate[4:nrow(combined_data4)]),
  combined_data4$x4[4:nrow(combined_data4)],
  combined_data4$neonatal_mortality_rate[4:nrow(combined_data4)]
)
combined_data5 <- combined_data4 %>% select(-x4)

combined_data5$neonatal_mortality_rate_2[4:nrow(combined_data5)] <- ifelse(
  is.na(combined_data5$neonatal_mortality_rate_2[4:nrow(combined_data5)]),
  combined_data5$x5[4:nrow(combined_data5)],
  combined_data5$neonatal_mortality_rate_2[4:nrow(combined_data5)]
)
combined_data6 <- combined_data5 %>% select(-x5)

combined_data6$infant_mortality_rate[4:nrow(combined_data6)] <- ifelse(
  is.na(combined_data6$infant_mortality_rate[4:nrow(combined_data6)]),
  combined_data6$x6[4:nrow(combined_data6)],
  combined_data6$infant_mortality_rate[4:nrow(combined_data6)]
)
combined_data7 <- combined_data6 %>% select(-x6)

combined_data7$infant_mortality_rate_2[4:nrow(combined_data7)] <- ifelse(
  is.na(combined_data7$infant_mortality_rate_2[4:nrow(combined_data7)]),
  combined_data7$x7[4:nrow(combined_data7)],
  combined_data7$infant_mortality_rate_2[4:nrow(combined_data7)]
)
combined_data8 <- combined_data7 %>% select(-x7)

combined_data8$under_five_mortality_rate[4:nrow(combined_data8)] <- ifelse(
  is.na(combined_data8$under_five_mortality_rate[4:nrow(combined_data8)]),
  combined_data8$x8[4:nrow(combined_data8)],
  combined_data8$under_five_mortality_rate[4:nrow(combined_data8)]
)
combined_data9 <- combined_data8 %>% select(-x8)

combined_data9$under_five_mortality_rate_2[4:nrow(combined_data9)] <- ifelse(
  is.na(combined_data9$under_five_mortality_rate_2[4:nrow(combined_data9)]),
  combined_data9$x9[4:nrow(combined_data9)],
  combined_data9$under_five_mortality_rate_2[4:nrow(combined_data9)]
)
combined_data10 <- combined_data9 %>% select(-x9)

combined_data10$under_five_mortality_rate_3[4:nrow(combined_data10)] <- ifelse(
  is.na(combined_data10$under_five_mortality_rate_3[4:nrow(combined_data10)]),
  combined_data10$x10[4:nrow(combined_data10)],
  combined_data10$under_five_mortality_rate_3[4:nrow(combined_data10)]
)
combined_data11 <- combined_data10 %>% select(-x10)

combined_data11$under_five_mortality_rate_4[4:nrow(combined_data11)] <- ifelse(
  is.na(combined_data11$under_five_mortality_rate_4[4:nrow(combined_data11)]),
  combined_data11$x11[4:nrow(combined_data11)],
  combined_data11$under_five_mortality_rate_4[4:nrow(combined_data11)]
)
combined_data12 <- combined_data11 %>% select(-x11)

combined_data12$adult_mortality_rate[4:nrow(combined_data12)] <- ifelse(
  is.na(combined_data12$adult_mortality_rate[4:nrow(combined_data12)]),
  combined_data12$x12[4:nrow(combined_data12)],
  combined_data12$adult_mortality_rate[4:nrow(combined_data12)]
)
combined_data13 <- combined_data12 %>% select(-x12)

combined_data13$adult_mortality_rate_2[4:nrow(combined_data13)] <- ifelse(
  is.na(combined_data13$adult_mortality_rate_2[4:nrow(combined_data13)]),
  combined_data13$x13[4:nrow(combined_data13)],
  combined_data13$adult_mortality_rate_2[4:nrow(combined_data13)]
)
combined_data14 <- combined_data13 %>% select(-x13)

new_colnames <- mapply(function(col, row1, row2, row3) {
  paste(col, row1, row2, row3, sep = " ")
}, colnames(combined_data14), combined_data14[1, ], combined_data14[2, ], combined_data14[3, ])

colnames(combined_data14) <- new_colnames

final_child_mortality_data <- combined_data14[-c(1:3), ]

rownames(final_child_mortality_data) <- NULL

colnames(final_child_mortality_data)[1] <- "country"

colnames(final_child_mortality_data)
names(final_child_mortality_data)[9]



# Here, we renamed all of our columns in a short and easier to understanf format so that we can use it easliy later:
colnames(final_child_mortality_data) <- c(
  "country", 
  "life_expectancy_1990", 
  "life_expectancy_2021",
  "neonatal_mortality_1990", 
  "neonatal_mortality_2021", 
  "infant_mortality_1990", 
  "infant_mortality_2021",
  "under_five_mortality_1990", 
  "under_five_mortality_2021", 
  "under_five_mortality_male_2021", 
  "under_five_mortality_female_2021", 
  "adult_mortality_male_2015_20", 
  "adult_mortality_female_2015_20"
)

# Converting all the column items into numeric to avoid any problems later due to differnt data types:
numeric_columns <- c("life_expectancy_1990", 
                     "life_expectancy_2021",
                     "neonatal_mortality_1990", 
                     "neonatal_mortality_2021", 
                     "infant_mortality_1990", 
                     "infant_mortality_2021",
                     "under_five_mortality_1990", 
                     "under_five_mortality_2021", 
                     "under_five_mortality_male_2021", 
                     "under_five_mortality_female_2021", 
                     "adult_mortality_male_2015_20", 
                     "adult_mortality_female_2015_20")

# Convert columns to numeric and remove rows with NA values
for (col in numeric_columns) {
  final_child_mortality_data[[col]] <- as.numeric(final_child_mortality_data[[col]])
}

world <- ne_countries(scale = "medium", returnclass = "sf")

merged_data <- inner_join(world, final_child_mortality_data, by = c("name_long" = "country"))



description_text <- "
<h2>Child Mortality Visualization App</h2>
<p>Read this to know more about the app we made so that it will be easy for you to navigate as well as not miss the important features of our app. This interactive Shiny app is designed to help explore and understand child mortality rates across different countries.</p>

<h3>Features of the Shiny App:</h3>
<ol>
    <li><b>Interactive Map</b>
        <p>The 'Map' section of the app is a visualization of the child mortality rates on a world map. User has option to select different variables such as life expectancy at birth, neonatal mortality, infant mortality, under-five mortality, and adult mortality rates. Moreover, you can choose different years, 1990 and 2021 to see the trends for different years.</p>

        <p><b>THINGS YOU CAN DO INSIDE THE MAP SECTION:</b></p>
        <ul>
            <li><b>Variable and Year Selection:</b> There are two action buttons inside this tab, one is to select the year between 1990 and 2021 and other is to choose between the specific variable between the different options . The options in the variables are:
                <ul>
                    <li>Life expectancy at birth </li>
                    <li>Neonatal mortality rate per 1,000 live births. </li>
                    <li>Infant mortality rate per 1,000 live births. </li>
                    <li>Under-five mortality rate per 1,000 live births.    </li>
                    <li> Adult mortality rate per 1,000.    </li>
                </ul>
            </li>
        </ul>
    </li>
    <li><b>Data Table</b>
        <p>This section consists of a searchable area. It also has a sortable table. BY sortable, we mean that it allows you to arrange data from high to low or low to high. The search button on the right side is where we can  search particular country, region or the income level. In this way, our table will provide a detailed information on the selected variables as well as year and different levels present.</p>
    </li>
    <li><b>Color Schemes </b>
        <p>You can change the color scheme of the map. There are different options, which are: viridis, magma, plasma, inferno, and cividis.</p>
    </li>
    <li><b>Update map</b>
        <p>There is a update map button button. This button is to make sure that that the data is being processed  everytime the variable names are changed and different option is wanted by the user. So, the map gets updated everytime any user changes something and wants to see the results by pressing the update map button.</p>
    </li>
</ol>

<h3>TUTORIAL OF USING THE CHILD MORTALITY VISUALIZATION APP TO GET MOST OUT OF IT:</h3>
<ul>
    <li><b>Select a year:</b> Use the given action button given to choose the year between either 1990 or 2021.</li>
    <li><b>Select a Variable:</b> Use the action button to choose a specific variable that you are interested in exploring.</li>
    <li><b>Select a color pattern:</b> Use the action button to choose specific color pattern that you think is cool.</li>
    <li><b>Update the Map:</b> Click the 'Update Map' button. This will act as a refresh button with your change in the settings in the button.</li>
    <li><b>Look at the Data Table:</b> Use the search function and sorting in ascending or descending order features to explore the data in more detail.</li>
</ul>

<h3>Discussion and Conclusion:</h3>
<p>Child mortality is a good topic to look into as it is an indicator of a country's health, development and the economic condition. It reflects the country healthcare system, socioeconomic conditions, and overall well-being of the people in that country. High child mortality rates is an indicator of the bad healthcare services, poor maternal health, lack of parental care, and extreme poverty. Trends in child mortality over years(for our data 1990 and 2021), can reveal the effectiveness ofpolicies that was implemented for the decreasing the child mortality rates in different developing countries. 

The trends in the Child mortality from our graphs also shows the inequalities in different communities that is happening. WE can see the poorest communities and where the need of improvement is the most. In the graphs, we can see that the African coutires and some Asian countries has the highest child mortality due to lack of adequate facilites. So, Addressing child mortality in these type of region is very essential. Through the eyes of the human right, as well as well being of everyone living in this earth, it is very important to look over these kind of plots.

So, by exploring diferent trends and the data table for different variables across different countries, this app is a way for gaining valuable insights about the areas that need more resources so that the child mortality can be decreased by applying different policies.</p>
"

library(shiny)
library(shinydashboard)
library(DT)
library(ggiraph)
library(ggplot2) 
library(dplyr)
library(viridis)
library(scales)


ui <- dashboardPage(
  dashboardHeader(title = "Child Mortality Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "map", icon = icon("globe")),
      menuItem("Table", tabName = "table", icon = icon("table")),
      menuItem("Description", tabName = "description", icon = icon("info-circle"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 12, leafletOutput("mapPlot")),
                pickerInput("variable", "Choose a variable:", 
                            choices = c("life_expectancy", "neonatal_mortality", "infant_mortality", "under_five_mortality"),
                            options = list(`style` = "btn-info"), multiple = FALSE, selected = "neonatal_mortality"),
                pickerInput("year", "Choose a year:", 
                            choices = c("1990", "2021"), 
                            options = list(`style` = "btn-info"), multiple = FALSE, selected = "2021"),
                pickerInput("colorScheme", "Choose Color Scheme:", 
                            choices = c("Spectral", "RdYlBu", "RdYlGn", "PiYG", "PRGn"),
                            options = list(`style` = "btn-success"), multiple = FALSE, selected = "Spectral"),
                actionButton("updateMap", "Update Map")
              )
      ),
      tabItem(tabName = "description",
              HTML(description_text)
      ),
      tabItem(tabName = "table",
              fluidRow(
                box(width = 12,
                    title = "Interactive Data Table",
                    status = "info",
                    solidHeader = TRUE,
                    DTOutput("dataTable")
                )
              )
      )
    ),
    add_busy_gif(
      src = "https://jeroen.github.io/images/banana.gif",
      height = 70, width = 70
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    data = merged_data,
    colorScheme = reactive(paste0(input$colorScheme)),
    variable = reactive(paste0(input$variable, "_", input$year))
  )
  
  updateData <- function() {
    variable_name <- values$variable()
    values$colorScheme <- input$colorScheme
    values$data$selected_var <- values$data[[variable_name]]
    values$data
  }
  
  observeEvent(c(input$updateMap, input$colorScheme), {
    updateData()
    output$mapPlot <- renderLeaflet({
      data <- values$data
      variable_name <- values$variable()
      
      pal <- colorNumeric(brewer.pal(8, values$colorScheme), data$selected_var)
      
      leaflet(data) %>%
        addProviderTiles(providers$Stamen.TonerLite) %>%
        addPolygons(
          fillColor = ~pal(selected_var),
          color = "#000000", weight = 1, opacity = 1,
          fillOpacity = 0.7, smoothFactor = 0.5,
          highlightOptions = highlightOptions(weight = 3, color = "#666666", bringToFront = TRUE),
          popup = ~paste(str_to_title(name_long), ":", round(selected_var, 2))
        ) %>%
        addLegend(
          pal = pal, 
          values = ~selected_var, 
          opacity = 1,
          title = variable_name, 
          position = "bottomright"
        )
    })
  })
  
  output$mapPlot <- renderLeaflet({
    updateData()
    data <- values$data
    variable_name <- values$variable()
    
    pal <- colorNumeric(brewer.pal(8, values$colorScheme), data$selected_var)
    
    # Build and render the map
    leaflet(data) %>%
      addProviderTiles(providers$Stamen.TonerLite) %>%
      addPolygons(
        fillColor = ~pal(selected_var),
        color = "#000000", weight = 1, opacity = 1,
        fillOpacity = 0.7, smoothFactor = 0.5,
        highlightOptions = highlightOptions(weight = 3, color = "#666666", bringToFront = TRUE),
        popup = ~paste(str_to_title(name_long), ":", round(selected_var, 2))
      ) %>%
      addLegend(
        pal = pal, 
        values = ~selected_var, 
        opacity = 1,
        title = variable_name,  
        position = "bottomright"
      )
  })
  
  #data table
  output$dataTable <- renderDT({
    filtered_data <- final_child_mortality_data %>%
      select(country, life_expectancy_1990, life_expectancy_2021, 
             neonatal_mortality_1990, neonatal_mortality_2021,
             infant_mortality_1990, infant_mortality_2021,
             under_five_mortality_1990, under_five_mortality_2021)
    
    datatable(filtered_data, 
              fillContainer = FALSE, 
              options = list(pageLength = 6, scrollY = '400px'))
  })
}

shinyApp( ui, server, options = list(height = 800))

