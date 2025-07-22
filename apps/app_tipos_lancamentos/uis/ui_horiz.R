# Aba do lançamento horizontal
ui_horiz <- fluidPage(

  sidebarLayout(
    mainPanel(
      h1(""),
      tags$div(
        style = "display: flex; align-items: center;", 
        tags$label("Qual o tipo de exercício?", style = "margin-right: 10px; color:#003366; "),
        selectInput("tipoH", NULL, choices = tipos, selected = "")),
      fluidRow(
        column(7, 
               tags$div(
                 style = "display: flex; align-items: center; ", 
                 tags$label("Problema de:", style = "margin-right: 10px; color:#003366; "),
                 selectInput("incognitaH", NULL, choices = variaveisH, selected = ""))
        ),
        column(5, uiOutput("exH"))
      ),
      h1(""),
      fluidRow(
        column(12, uiOutput("enunciadoH"))
      ),
      fluidRow(
        column(12, uiOutput("SegH"))
      ),
      fluidRow(
        style = "display: flex; justify-content: center; align-items: center;",
        
        column(8, uiOutput("parametrosH")),
        column(4, uiOutput("mostrarH"))
      ),
      h1(""),
      fluidRow(
        column(6, uiOutput("lancarH"))
      ),
      h1(""),
      plotlyOutput("movH")
    ),
    sidebarPanel(
      style = "background-color: #D1EEEE; padding: 20px; border-radius: 10px;",
      
      tags$h1(
        strong("Passo a Passo"), class = "titulo-secundario"),
      h1(""),
      uiOutput("b1H"), # botão de equação no arquivo equacoes1.R
      h1(""),
      uiOutput("t_eqH"), # equação detalhada no arquivo equacoes1.R
      h1(""),
      fluidRow(
        column(6, uiOutput("resposta")), # campo para a resposta em resposta.R
        br(),
        column(6, uiOutput("conf1")) # botão de conferir a resposta em resposta.R
      ),
      uiOutput("r1"), # feedback ao clicar no botão de conferencia em resposta.R
      h1(""),
      uiOutput("b2H"), # botão de conferir calculo em calculo1.R
      h1(""),
      uiOutput("t_solH") # solução detalhada do exercicio em calculo1.R
    )
  )
)
