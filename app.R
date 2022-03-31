library(shiny)
library(tidyverse)
library(ggthemes)
library(maps)
library(ggplot2)
library(dplyr)
library(stringr)
Sys.setlocale("LC_TIME", "English")

covid19 <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")

census_pop_est_2018 <- read_csv("https://www.dropbox.com/s/6txwv3b4ng7pepe/us_census_2018_state_pop_est.csv?dl=1") %>%
  separate(state, into = c("dot","state"), extra = "merge") %>%
  select(-dot) %>%
  mutate(state = str_to_lower(state))

states_map <- map_data("state")

cases_with_2018_pop_est <- covid19  %>%
  mutate(state = str_to_lower(state)) %>%
  left_join(census_pop_est_2018,
            by = c("state" = "state")) %>%
  group_by(state) %>%
  mutate(eachDay=replace_na(lag(cases,n=1),0)) %>%
  mutate(newEach=cases-eachDay) %>%
  mutate(new_cases_per_100000 = (newEach/ est_pop_2018) * 100000) %>%
  mutate(states=str_to_title(state)) %>% 
  mutate(dates=as.Date(date))


ui <- fluidPage(
  sliderInput( inputId = "Date",
               label = "Date Range:",
              min = as.Date(min(cases_with_2018_pop_est$date)),
              max = as.Date(max(cases_with_2018_pop_est$date)),
              value = c(as.Date(min(cases_with_2018_pop_est$date)),
                        as.Date(max(cases_with_2018_pop_est$date)))
  ),
  selectInput(inputId = "States",
              label ="States",
              choices = c("Alabama", "Alaska", "Arizona","American Samoa", "Arkansas",
                          "California", "Colorado", "Connecticut", "Delaware","District Of Columbia",
                          "Florida", "Georgia","Guam", "Hawaii", "Idaho", "Illinois",
                          "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine","Maryland" ,
                          "Massachusetts" ,"Michigan" ,"Minnesota" ,"Mississippi" ,"Missouri" ,"Montana",
                          "Nebraska" ,"Nevada" ,"New Hampshire","New Jersey" ," New Mexico","New York" ,
                          "North Carolina" ,"North Dakota" ,"Northern Mariana Islands" ,"Ohio" ,"Oklahoma" ,
                          "Oregon","Pennsylvania","Puerto Rico","Rhode Island" ,"South Carolina" ,
                          "South Dakota","Tennessee" ,"Texas" ,"Utah" ,"Vermont" ,"Virgin Islands",
                          "Virginia","Washington" ,"West Virginia" ,"Wisconsin" ,"Wyoming" ),
              multiple = TRUE,
              selected = "Alabama"),
  submitButton(text = "Submit"),
  plotOutput(outputId = "covidPlot")
)

server <- function(input, output) {
  output$covidPlot <- renderPlot(
    cases_with_2018_pop_est %>%
      filter(states==input$States) %>%
      ggplot(aes(
        x = date,
        y = new_cases_per_100000,
        color = states
      )) +
      geom_line() +
      scale_x_date(limit=c(as.Date(input$Date))) +
      labs(
        title = "States’ Daily Number of COVID Cases Per 100,000 Residents Over Time",
        x = "",
        y = "",
        color = "states"
      ) +
      theme_bw() +
      theme(legend.position = "top",
            legend.title = element_blank())
  )

}
shinyApp(ui=ui, server=server)


