
G <- Ex()$grafico
#if (G == 3) {
#  coluna_x <- reactive({select(df3(), 1)
#  })
#} else {
#  coluna_x <- reactive({select(df(), 1)
#  })
#}

#if (G == 3) {
#  coluna_y <- reactive({select(df3(), 2)
#  })
#} else {
#  coluna_y <- reactive({select(df(), 2)
#  })
#}

#if (G == 3) {
#  coluna_xy <- reactive({ 
#    select(df3(), 1:2)
#  })
#} else {
#  coluna_xy <- reactive({ 
#    select(df(), 1:2)
#  })
#}

observeEvent(input$amostra, {
  output$tamanhoamostra <- renderUI({
    tabela_visivel("NULL")
    num_elementos <- length(df()$Y)
    tagList(
      p(withMathJax(helpText(paste0("$$\\text{Tamanho da Amostra} =", num_elementos, "$$"))))
    )
  })
})

#-----------------------------------------------------------------
#tabelas atualização

tabela_visivel <- reactiveVal("NULL")
selected_x <- reactiveVal(NULL)
selected_y <- reactiveVal(NULL)
selected_xy <- reactiveVal(NULL)
selected_freq <- reactiveVal(NULL)

observeEvent({
  input$question
  input$ENEM
  }, {
    #erro aparente está aqui ao trocar para o segundo exercício (ativa o valor sem necessariamente selecionar ele)
  
  req(input$question)
  req(input$ENEM)

  tabela_visivel("NULL")
  selected_x(NULL)
  selected_y(NULL)
  selected_xy(NULL)
  selected_freq(NULL)
  
#TABELA DE FREQUÊNCIAS
observeEvent(input$agrupar_dados, {
  tabela_visivel("freq")
  print("teste-freq")
  selected_x(NULL)
  selected_y(NULL)
  selected_xy(NULL)
  selected_freq(NULL)
  observe({
    selected_freq(input$tabelafreq_rows_selected)
  })
  output$tabelafreq <- renderDT({
    dfa1 <- dfa()
    colnames(dfa1) <- c(Ex()$eixo_y,"Frequência")
    rv$tabela <- datatable(dfa1, selection = "single", options = list(dom = 't'))
    rv$tabela
  })
})

coluna_xyR <- reactive({ 
  select(dfR(), 1:2)
})

#TABELA XY
observeEvent(input$tabela_xy, {
  tabela_visivel("xy")
  print("teste-xy")
  selected_x(NULL)
  selected_y(NULL)
  selected_xy(NULL)
  selected_freq(NULL)
  observe({
    selected_xy(input$tabela_rows_selected)
  })
  
  coluna_xy <- reactive({select(df(), 1:2)})
  
  Rreativo <- reactive({Ex()$R})
  R <- Rreativo()
  
  if (R == "1") {
    coluna_xy <- reactive({select(dfR(), 1:2)})
      coluna_xy1 <- coluna_xy()
      req(Ex()$eixo_x)
      req(Ex()$eixo_y)
      colnames(coluna_xy1) <- c(Ex()$eixo_x, Ex()$eixo_y)
      output$tabela <- renderDT({
        rv$tabela <- datatable(coluna_xy1, selection = "single", options = list(dom = 't'))
        rv$tabela
      })
  } else {
      coluna_xy1 <- coluna_xy()
      req(Ex()$eixo_x)
      req(Ex()$eixo_y)
      colnames(coluna_xy1) <- c(Ex()$eixo_x, Ex()$eixo_y)
      output$tabela <- renderDT({
        rv$tabela <- datatable(coluna_xy1, selection = "single", options = list(dom = 't'))
        rv$tabela
      })
  }
})

#TABELA DO EIXO X
observeEvent(input$tabela_x, {
  tabela_visivel("x")
  print("teste-x")
  selected_x(NULL)
  selected_y(NULL)
  selected_xy(NULL)
  selected_freq(NULL)
  observe({
    selected_x(input$tabela_x_rows_selected)
  })
  
  coluna_x <- reactive({select(df(), 1) })
  
    req(Ex()$eixo_x)
    coluna_x1 <- coluna_x()
    colnames(coluna_x1) <- Ex()$eixo_x
    output$tabela_x <- renderDT({
      rv$tabela <- datatable(coluna_x1, selection = "single", options = list(dom = 't'))
      rv$tabela
    })
})

#TABELA DO EIXO Y
observeEvent(input$tabela_y, {
  tabela_visivel("y")
  print("teste-y")
  selected_x(NULL)
  selected_y(NULL)
  selected_xy(NULL)
  selected_freq(NULL)
  observe({
    selected_y(input$tabela_y_rows_selected)
  })
  
  coluna_y <- reactive({select(df(), 2)})

    Rreativo <- reactive({Ex()$R})
    R <- Rreativo()

if (R == "1") {
  coluna_y <- reactive({select(dfR(), 2)})
    output$tabela_y <- renderDT({
      coluna_y1 <- coluna_y()
      req(Ex()$eixo_y)
      colnames(coluna_y1) <- Ex()$eixo_y
      rv$tabela <- datatable(coluna_y1, selection = "single", options = list(dom = 't'))
      rv$tabela
    })
} else {
  output$tabela_y <- renderDT({
    coluna_y1 <- coluna_y()
    req(Ex()$eixo_y)
    colnames(coluna_y1) <- Ex()$eixo_y
    rv$tabela <- datatable(coluna_y1, selection = "single", options = list(dom = 't'))
    rv$tabela
  })
}
})

}) 

#-----------------------------------------------------------------
#renderização da tabela
observeEvent({
  input$question
  input$ENEM
}, {
output$tabela_ou_freq <- renderUI({
  selected_x(NULL)
  selected_y(NULL)
  selected_xy(NULL)
  selected_freq(NULL)
  if(tabela_visivel() == "xy") {
    dataTableOutput("tabela")
  } else if(tabela_visivel() == "freq") {
    dataTableOutput("tabelafreq")
  } else if(tabela_visivel() == "x") {
    dataTableOutput("tabela_x")
  } else if(tabela_visivel() == "y") {
    dataTableOutput("tabela_y")
  } else if (tabela_visivel() == "NULL") {
    print("teste-null")
    tagList(
      p()
    )  
  }
})
})


#-----------------------------------------------------------------
#dataframe para frequência
dfa <- reactive ({ as.data.frame(table(df()$Y)) })
#dfa2 <- as.data.frame(table(df3()$Y))
#colnames(dfa) <- c("Y","Frequência")

