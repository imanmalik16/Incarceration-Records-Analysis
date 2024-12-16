library(tidyverse)

inmates_data <- read_csv("Currently_Incarcerated_July2024.csv")
inmates <- inmates_data |>
  filter(County == "Travis") |>
  select(Name, Gender, Age, Race, Offense = `TDCJ Offense`, `Sentence Date`, `Projected Release`, Sentence = `Sentence (Years)`) |>
  filter(!str_detect(Sentence, "^[a-zA-Z]")) |>
  mutate(`Sentence Date` = mdy(`Sentence Date`),
         `Projected Release` = mdy(`Projected Release`),
         Sentence = as.numeric(Sentence),
         Estimated_Sentence = round(as.numeric(difftime(`Projected Release`, `Sentence Date`, units = "days"))/365, 1)) |> 
  filter(Sentence <= 100) |>
  mutate(Race = recode(Race, `A` = "Asian American", `B` = "Black", `H` = "Hispanic", `I` = "American Indian or Alaskan Native", `O` = "Other", `U` = "Other", `W` = "White"),
         Gender = recode(Gender, `M` = "Male", `F` = "Female")) |>
  mutate(Murder = str_detect(Offense, pattern = "MURDER") | str_detect(Offense, pattern = "MANSLAUGHTER")) |>
  mutate(Murder = ifelse(Murder == TRUE, "Yes", "No"))

library(shiny)

ui <- fluidPage(
  titlePanel("Incarceration Records for Travis County Offenders"),
  hr(),
  p("This application allows you to view univariate distributions for 5 different variables. You can change the colors of the graphs, change the bins of the histograms, view the descriptive statistics, and filter the distribution range by sentences."),
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", label = h4("Choose Variable:"), 
                  choices = list("Gender", "Race", "Age", "Sentence", "Murder"), 
                  selected = "Gender"),
      hr(),
      radioButtons("color_choice", label = h4("Choose Graph Color:"),
                   choices = c("Blue" = 'cyan3', "Purple" = 'purple', "Pink" = "hotpink"),
                   selected = 'cyan3'),
      hr(),
      
      sliderInput(inputId = "range", 
                  label = ("Filter by range of sentences:"),
                  min = min(inmates$Sentence, na.rm = TRUE), 
                  max = max(inmates$Sentence, na.rm = TRUE), 
                  value = c(min(inmates$Sentence, na.rm = TRUE), max(inmates$Sentence, na.rm = TRUE))),
      
      hr(),
      
      sliderInput(inputId = "bins",
                  label = "Number of bins (histogram only):",
                  min = 1,
                  max = 50,
                  value = 20),
      hr(),
      
      checkboxInput(inputId = "checkbox", 
                    label = "Display descriptive statistics", 
                    value = FALSE),
      hr()),
    
    mainPanel(
      h3("Graph showing Distribution"),
      plotOutput("graph"),
      verbatimTextOutput("stats"),
      img(src = 'https://toldev.s3.amazonaws.com/uploads/talk/image/111/main_video_play_Confronting_Mass_Incarceration.jpg', width = 500, height = 250),
      p("Image source: https://www.talksonlaw.com/talks/confronting-mass-incarceration", )
    )
  )
)

server <- function(input, output) {
  
  output$graph <- renderPlot({
    
    inmates2 <- inmates |> 
      filter(Sentence >= input$range[1] & Sentence <= input$range[2])
    
    if (input$variable == "Gender"){
      barplot(table(inmates2$Gender), main = "Distribution of Gender", xlab = "Gender", col = input$color_choice)
    } else if (input$variable == "Race"){
      barplot(table(inmates2$Race), main = "Distribution of Race", xlab = "Race", col = input$color_choice)
    } else if (input$variable == "Age"){
      hist(inmates2$Age, breaks = input$bins, main = "Distribution of Age", xlab = "Age", col = input$color_choice)
    } else if (input$variable == "Sentence"){
      hist(inmates2$Sentence, breaks = input$bins, main = "Distribution of Sentences", xlab = "Sentence (years)", col = input$color_choice)
    } else if (input$variable == "Murder"){
      barplot(table(inmates2$Murder), main = "Distribution of Murder Cases", xlab = "Murder Case", col = input$color_choice)
    }
  })
  
  output$stats <- renderPrint({
    
    inmates2 <- inmates |> 
      filter(Sentence >= input$range[1] & Sentence <= input$range[2])
    
    if (input$checkbox == TRUE) {
      if (input$variable == "Age" | input$variable == "Sentence") {
        mean_value <- mean(pull(inmates2, input$variable), na.rm = TRUE)
        sd_value <- sd(pull(inmates2, input$variable), na.rm = TRUE)
        cat("Mean:", round(mean_value, 3), "\n")
        cat("Standard Deviation:", round(sd_value, 3), "\n")
      } else {
        prop_table <- prop.table(table(pull(inmates2, input$variable), useNA = "ifany"))
        cat("Proportions:\n")
        print(prop_table)
      }
    }
  })
}

shinyApp(ui = ui, server = server)