# Aba do teste diagnóstico
ui_diag <- dashboardBody(

  fluidRow(
    column(4,
           box(title = NULL, headerBorder = F, width = 12, collapsible = FALSE,
               actionButton("gerar", "🔄 Gerar Novo Teste"),
               br(), br(),
               actionButton("calcular", "✅ Calcular Pontuação"),
               br(), br(),
               h5("📊 Seu Desempenho"),
               DTOutput("tabela_erros"),  # Tabela mostrando erros e acertos
               br(), br(),
               h4(textOutput("pontuacao"), style = "color: green; font-weight: bold;"),
               h5(textOutput("feedback"), style = "color: blue;")
           )
    ),
    
    column(8,
           box(id = "box1", title = tagList(span("📚 Matemática Básica", 
                                                 style = "font-size: 20px;")),
               solidHeader = TRUE, width = 12,
               uiOutput("perguntas_nivel1")
           ),
           
           box(id = "box2", title = tagList(span("🚗 Conceitos de Movimento", 
                                                 style = "font-size: 20px;")),
               solidHeader = TRUE, width = 12,
               uiOutput("perguntas_nivel2")
           ),
           
           box(id = "box3", title = tagList(span("🎯 Lançamento Horizontal", 
                                                 style = "font-size: 20px;")),
               solidHeader = TRUE, width = 12,
               uiOutput("perguntas_nivel3")
           ),
           
           box(id = "box4", title = tagList(span("📜 Formulário Auxiliar", 
                                                 style = "font-size: 20px;")),
               solidHeader = TRUE, collapsible = FALSE, width = 12,
               uiOutput("formulario")
           )
    )
  )
)
