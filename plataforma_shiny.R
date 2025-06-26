

library(shiny)
library(shinyWidgets)
library(plotly)
library(numbers)
library(DT)


ui <- fluidPage(
  titlePanel("EducaShiny: Plataforma de Aplicativos Educacionais"),
  
  # CSS para centralizar os botões na horizontal
  tags$style(HTML("
    .centered-container {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 50vh; # distancia dos modulos do titulo
    }
    .button-row {
      display: flex;
      gap: 40px; /* Espaço entre os botões */
    }
    .dropdown-menu {
      margin-top: 20px;
    }
    .action-button {
      width: 200px;
      height: 100px;
      font-size: 18px;
    }
  ")),
  
  
  # Adicionando os botões em uma linha horizontal
  shiny::div(
    class = "centered-container",
    shiny::div(
      class = "button-row",
      
      # Botão App 1 com subaplicativos
      dropdownButton(
        label = "Matemática",
        icon = icon("chart-pie"),
        circle = FALSE,
        status = "primary",
        width = "200px",
        size = "lg",
        tags$a(href = "https://educashiny.shinyapps.io/fracoes/",
               "Frações", class = "btn btn-link")
      ),
      
      # Botão App 2 com subaplicativos
      dropdownButton(
        label = "Estatística",
        icon = icon("chart-line"),
        circle = FALSE,
        status = "primary",
        width = "200px",
        size = "lg",
        tags$a(href = "https://educashiny.shinyapps.io/EstatisticaBasica/",
               "Gráficos", class = "btn btn-link")#,
        # tags$a(href = "http://localhost:3838/subapp2/", "SubApp 2", class = "btn btn-link")
      ),
      
      # Botão App 3 com subaplicativos
      dropdownButton(
        label = "Física",
        icon = icon("rocket"),
        circle = FALSE,
        status = "primary",
        width = "200px",
        size = "lg",
        tags$a(href = "https://educashiny.shinyapps.io/FisicaLancamentos/",
               "Tipos de Lançamentos", class = "btn btn-link")
      )
  
  )
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
