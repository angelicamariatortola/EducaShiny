# Aba do lançamento oblíquo
ui_obli <- sidebarLayout(
  
  mainPanel(width = 8,
    h1(""),
    fluidRow(
      column(12,
             uiOutput("enunciado"))
    ),
    fluidRow(
      column(12,
      uiOutput("parametros"))
    ),
    h1(""),
    fluidRow(
      column(6,
             uiOutput("mostrar")),
      column(6,
             uiOutput("lancar"))
    ),
    h1(""),
    plotlyOutput("mov")
  ),
  
  sidebarPanel(width = 4,
    selectInput("tipo","Qual o tipo de exercício?", tipos),
    fluidRow(
      column(8,
             selectInput("incognita","Problema de:", variaveis)
      ),
      column(4,
             uiOutput("ex")
      )
    ),
    uiOutput("Seg"),
    h1(""),
    uiOutput("b"),
    h1(""),
    uiOutput("tab"),
    h1(""),
    uiOutput("b1"),
    h1(""),
    uiOutput("t_eq"),
    h1(""),
    passwordInput("password", "Acesso do professor:"),
    h1(""),
    uiOutput("b2"),
    h1(""),
    uiOutput("t_sol")
  )
)
