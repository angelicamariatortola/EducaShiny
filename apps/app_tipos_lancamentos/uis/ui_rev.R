ui_rev <- dashboardBody(
  
  fluidRow(
    column(
      width = 3,
      box(title = HTML("<span style='font-size:24px; color:#003366; text-align: center;
                       font-weight:bold;'>Conteúdos de Revisão</span>"),
          headerBorder = F, width = 12, collapsible = FALSE,
          actionButton("menu_mat", "📚 Matemática Básica", class = "btn-acao"),
          actionButton("menu_mov", "🚗 Conceitos de Movimento", class = "btn-acao"),
          actionButton("menu_lanc", "🎯 Lançamento Horizontal", class = "btn-acao"),
          actionButton("menu_vid", "📺 Vídeos de Revisão", class = "btn-acao")
      )
    ),
    column(
      width = 9,
      uiOutput("conteudo_revisao")  # Conteúdo renderizado dinamicamente
      
    )
  )
)


