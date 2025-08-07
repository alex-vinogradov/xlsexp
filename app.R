library(shiny)
library(haven)
library(readxl)
library(dplyr)

## Ця функція читає файл даних і мета-даних за специфікацією XLSForm
xlsform_read <- function(file_name, data_sheet = 'data', from = 'label::English (en)') {
  data <- read_xlsx(file_name, sheet = data_sheet)
  survey <- read_xlsx(file_name, sheet = "survey")
  choices <- read_xlsx(file_name, sheet = "choices")
  
  for (varname in names(data)) {
    found <- survey$name == varname
    if (any(found)) {
      variable <- survey[found, ]
      varlab <- variable[[from]]
      varlevel <- variable$measure
      
      labs <- c()
      vtype <- unlist(strsplit(variable$type, ' '))
      if (length(vtype) > 1) {
        labs <- subset(choices, list_name == vtype[2])
        labs <- sort(setNames(object = labs$name, nm = labs[[from]]))
      }
      
      if (tolower(vtype[1]) %in% c('select_one', 'select_multiple')) {
        data[[varname]] <- factor(
          data[[varname]], levels = labs,
          labels = names(labs), ordered = tolower(varlevel) == 'ordinal'
        )
      }
      
      if (tolower(vtype[1]) %in% c('integer', 'decimal')) {
        attr(data[[varname]], 'format.spss') <- 'F8.0'
      }
      
      if (tolower(vtype[1]) %in% c('datetime')) {
        data[[varname]] <- as.POSIXct(data[[varname]], format = "%d.%m.%Y %H:%M:%S")
      }
      
      attr(data[[varname]], 'label') <- varlab
    }
  }
  return(data)
}

########################################################################


ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  titlePanel("Експорт даних з формату XLSForm"),
  # helpText(
  #   "Цей застосунок конвертує дані, представлені у модифікованому форматі XLSForm, у формат IBM SPSS Statistics або RDS"
  # ),
  sidebarLayout(
    sidebarPanel(

      fileInput(
        inputId = "file", "Завантажити файл XLSForm",
        accept = c(".xlsx"),
        buttonLabel = "Знайдіть",
        placeholder = "не вказано"
      ),
      
      uiOutput("languageSelect"),
      
      conditionalPanel(
        condition = "input.language !== undefined",
        radioButtons(
          inputId = "format", label = "Формат файлу",
          choices = list("RDS" = 1, "SAV" = 2)
        ),
        downloadButton(
          outputId = "downloadData", label = "Експортувати"
        )
      )
    ),

    mainPanel(
      tableOutput("variables")
    )
  )
)

server <- function(input, output, session) {

  data <- reactive({
    req(input$language %in% languages(), cancelOutput = TRUE)
    xlsform_read(file_name = input$file$datapath, from = input$language)
  })

  languages <- reactive({
    req(input$file)
    x <- read_xlsx(input$file$datapath, sheet = "survey")
    names(select(x, starts_with("label")))
  })

  output$languageSelect <- renderUI({
    selectInput("language", "Оберіть мову міток:", choices = languages())
  })

  output$variables <- renderTable({
    req(data(), languages())
    sapply(X = data(), FUN = attr, "label")
  }, rownames = TRUE, striped = TRUE, colnames = FALSE, bordered = TRUE)

  output$downloadData <- downloadHandler(
    filename = function() {
      if (input$format == 1) {
        paste(tools::file_path_sans_ext(input$file$name), ".rds", sep = "")
      } else {
        paste(tools::file_path_sans_ext(input$file$name), ".sav", sep = "")
      }
    },
    content = function(file) {
      if (input$format == 1) {
        saveRDS(data(), file = file)
      } else {
        write_sav(data = data(), path = file, compress = "zsav")
      }
    }
  )
  
}

shinyApp(ui, server)
