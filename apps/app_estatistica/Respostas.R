
Rreativo <- reactive({Ex()$R})
R <- Rreativo()

dfR <- reactive({
  
  RRYt <- Ex()$respt1
  RRXt <- Ex()$respt2
  
  if (RRYt == "1") {
    RRY <- reactive(as.numeric(strsplit(Ex()$resp1, ",")[[1]]))
  } else if (RRYt == "2") {
    RRY <- reactive(as.character(strsplit(Ex()$resp1, ",")[[1]]))
  }

  if (RRXt == "1") {
    RRX <- reactive(as.numeric(strsplit(Ex()$resp2, ",")[[1]]))
  } else if (RRXt == "2") {
    RRValX <- reactive(as.character(strsplit(Ex()$resp2, ",")[[1]]))
    RRX <- reactive(factor(RRValX(), levels = RRValX()))
  }
  
  if (R == "1") {
  tX <- Ex()$tx
  
  if (tX == "1") {
    X <- reactive(as.numeric(strsplit(Ex()$X, ",")[[1]]))
  } else if (tX == "2") {
    X <- reactive(as.character(strsplit(Ex()$X, ",")[[1]]))
  }
  
    req(RRY())
    data.frame(X = X(), Y = RRY())
    
  } else if (R == "11" || R == "2" || R == "13" || R == "12" || R == "14") {
    req(RRX())
    req(RRY())
    data.frame(X = RRX(), Y = RRY())
  }
})


# R == "11"
if (R == "12" || R == "14") {
coluna_xy1 <- reactive({select(dfR(), 1:2)})
    
observeEvent(input$resposta, {
  coluna_xyR1 <- coluna_xy1()
  req(Ex()$eixo_x)
  req(Ex()$eixo_y)
  colnames(coluna_xyR1) <- c(Ex()$eixo_x, Ex()$eixo_y)
  output$TabelaR1_1 <- renderDT({
    rv$TabelaR1_1 <- datatable(coluna_xyR1, selection = "single", options = list(dom = 't'))
    rv$TabelaR1_1
  })
})

output$TabelaR1 <- renderUI({
    dataTableOutput("TabelaR1_1")
})

} else if (R == "1") {
observeEvent(input$resposta, {
  coluna_xy <- reactive({select(df(), 1:2)})
  coluna_xy1 <- coluna_xy()
  req(Ex()$eixo_x)
  req(Ex()$eixo_y)
  colnames(coluna_xy1) <- c(Ex()$eixo_x, Ex()$eixo_y)
  output$TabelaR2_1 <- renderDT({
    rv$TabelaR2_1 <- datatable(coluna_xy1, selection = "single", options = list(dom = 't'))
    rv$TabelaR2_1
  })
})

output$TabelaR2 <- renderUI({
  dataTableOutput("TabelaR2_1")
})
} else if (R == 3) {
  observeEvent(input$resposta, {
    output$Resposta0 <- renderUI({
      numerador <- reactive(as.numeric(strsplit(Ex()$resp1, ",")[[1]]))
      denominador <- reactive(as.numeric(strsplit(Ex()$resp2, ",")[[1]]))
      results1 <- lapply(1:length(numerador()), function(i) {
        div(withMathJax(paste0("$$", numerador()[i], " / ", denominador()[i], " = ", numerador()[i] / denominador()[i], "$$")))
      })
      do.call(tagList, results1)
    })
  })
} else if (R == 31) {
  observeEvent(input$resposta, {
  output$Resposta0 <- renderUI({
    numerador <- reactive(as.numeric(strsplit(Ex()$resp1, ",")[[1]]))
    denominador <- reactive(as.numeric(strsplit(Ex()$resp2, ",")[[1]]))
    results2 <- lapply(1:length(numerador()), function(i) {
      result <- numerador()[i] * (denominador()[i] / 100)
      div(withMathJax(paste0("$$", denominador()[i], "\\% \\text{ de } ", numerador()[i], " = ", result, "$$")))
    })
    do.call(tagList, results2)
  })
  })
} else if (R == "4") {
  observeEvent(input$resposta, {
    output$Resposta0 <- renderUI({
      termos <- paste(nums(), collapse = " + ")
      formula <- paste0("$$\\text{Média} = \\frac{", "1 * 10 + 2 * 10 + 3 * 55 + 4 * 25 + 6 * 50 + 7 * 10", "}{", "160", "}$$")
      tagList(
        h4(strong("Média"), style = "text-align: center;"),
        p(withMathJax(helpText(formula))),
        p(withMathJax(helpText("$$\\text{Média} =", "4,156", "$$"))),
        h4(strong("Mediana"), style = "text-align: center;"),
        p("Como temos 160 valores, o valor médio deste é 80, e observando o gráfico, temos:"),
        p(withMathJax(helpText(paste0("$$\\text{Mediana} =", "4", "$$")))),
        h4(strong("Moda"), style = "text-align: center;"),
        withMathJax(paste0("$$\\text{Moda} =","3","$$"))
      )
    })
  })
} else if (R == "41") {
  
  observeEvent(input$resposta, {
    output$Resposta0 <- renderUI({
      nums <- reactive ({ c(df()$Y) })
      sortedNums <- sort(nums())
      termos2 <- paste(sortedNums, collapse = " , ")
      medianValue <- median(nums())
      sequenciamediana <- length(nums())
      if (sequenciamediana %% 2 == 1) {
        medianFormula <- paste0("$$", termos2 , "$$")
      } else {
        medianFormula <- paste0("$$", termos2, "\\\\", "\\frac{", sortedNums[length(nums()) / 2], " + ", sortedNums[length(nums()) / 2 + 1], "}{2}$$")
      }
      tagList(
        h4(strong("Mediana"), style = "text-align: center;"),
        p(withMathJax(helpText(medianFormula))),
        p(withMathJax(helpText(paste0("$$\\text{Mediana} =", medianValue, "$$")))),
      )
    })
  })
} else if (R == "5" || R == "51" || R == "2") {
  observeEvent(input$resposta, {
    output$Resposta0 <- renderUI({
     TextoResposta <- reactive(as.character(strsplit(Ex()$resp1, "~")[[1]]))
      tagList(
        p(TextoResposta(), style = "text-align: justify;")
      )
    })
  })
}

#TIPOS DE GRÁFICOS
#Os gráficos são definidos por G (Gráficos na Aba de Exercícios) e R (Gráficos na Aba de Resposta). Os R são divididos em:
# 1 - Gráfico de Linhas (Com labels em X e Y), com dados originais
# 11 - Gráfico de Linhas (Com labels em X e Y) e com labels numéricos, com dados novos
# 12 - Gráfico de Pontos Ordenados e com Linha no Meio (Mediana), com dados originais
# 13 - Gráfico de Barras (Com labels em X e Y), com dados novos
# 14 - Gráfico com Pontos e duas linhas específicas, com dados novos
# 2 - Gráfico de Linhas que possui uma linha no ponto Y = 0; não possui labels em Y; com dados originais
# 5, 51 e 4 não plotam um gráfico como resposta

#Dados Originais = df() usado para criar o gráfico, sendo o mesmo do original
#Dados Novos = dfR() usado para criar o gráfico, novo dataframe (Usado para exercícios que possuem uma expansão ou mudança do gráfico original)


output$graficoR <- renderPlot({
  y_min <- min(Ex()$y_min)
  if (Ex()$tx == "2") {
    x_min <- min(df()$Xvar)  
  } else if (Ex()$tx == "1") {
    x_min <- min(Ex()$x_min)  
  }
  req(df())
  sR <- input$TabelaR2_1_rows_selected
  sR1 <- input$TabelaR1_1_rows_selected
  if (R == "1") {
ggR <- ggplot(data = df(), aes(x = X, y = Y)) +
  labs(x = Ex()$eixo_x, y = Ex()$eixo_y,
       title = Ex()$titulo)
  } else if (R == "11") {
    ggR <- ggplot(data = dfR(), aes(x = X, y = Y, group = 1)) +
      labs(x = Ex()$eixo_x, y = Ex()$eixo_y,
           title = Ex()$titulo)
  } else if (R == "12") {
    ggR <- ggplot(data = dfR(), aes(x = X, y = Y)) +
      geom_vline(xintercept = Ex()$resp3, linetype="solid", color = "blue", size = 2) +
      theme(axis.text.x = element_blank()) +
      labs(x = Ex()$Reixo_x, y = Ex()$Reixo_y,
           title = Ex()$titulo)
  } else if (R == "14") {
    ggR <- ggplot(data = dfR(), aes(x = X, y = Y)) +
      geom_vline(xintercept = Ex()$resp3, linetype="solid", color = "blue", size = 2) +
      geom_hline(yintercept = Ex()$resp4, linetype="solid", color = "blue", size = 2) +
      labs(x = Ex()$Reixo_x, y = Ex()$Reixo_y,
           title = Ex()$titulo)
    } else if (R == "2") {
      ggR <- ggplot(data = df(), aes(x = X, y = Y), group = 1) +
        geom_hline(yintercept=0, linetype="solid", color = "blue", size = 2, group = 2) +
        theme(axis.text.y = element_blank()) +
        labs(x = Ex()$eixo_x, y = Ex()$eixo_y,
             title = Ex()$titulo)
  } else if (R == "13") {
    ggR <- ggplot(data = dfR(), aes(x = X, y = Y)) +
    geom_col(position = "dodge") +
    labs(title = Ex()$titulo,
         x = Ex()$Reixo_x,
         y = Ex()$Reixo_y)
  }
  
  if (R == 1) {
  ggR = ggR + geom_point(data = df()[sR1, ], color = "yellow", size = 5) + geom_point(data = df()[sR, ], color = "blue", size = 5)
  } else if (R == 11 || R == 12 || R == 14) {
  ggR = ggR + geom_point(data = dfR()[sR1, ], color = "yellow", size = 5) + geom_point(data = dfR()[sR, ], color = "blue", size = 5)
}
  if (R == "2" || R == "11" || R == "1" ) {
    ggR = ggR + geom_line(size = 1.5)
  }
  if (R == "1" || R == "11" || R == "12" || R == "2" || R == "14") {
    ggR = ggR + geom_point(aes(x = X, y = Y), size = 3)
  }
  
  if (R == "2" || R == "11") {
  if (Ex()$tx == "1") {
    ggR = ggR + scale_x_continuous(breaks = Ex()$x_lineR)
  } else if (Ex()$tx == "2") {
    ggR = ggR
  } else {
    ggR = ggR
  }
  } else if (R == "12" || R == "14") {
    if (Ex()$respt1 == "1") {
      ggR = ggR + scale_x_continuous(breaks = dfR()$X)
    } else if (Ex()$respt2 == "2") {
      ggR = ggR
    }
  }
  
  LINER <- Ex()$lineR
  
  x_lineR <- reactive(as.numeric(strsplit(Ex()$x_lineR, ",")[[1]]))
  y_lineR <- reactive(as.numeric(strsplit(Ex()$y_lineR, ",")[[1]]))
  
  if (LINER == 0) {
    ggR = ggR + scale_y_continuous(limits = c(Ex()$y_minR, Ex()$y_maxR)) 
  } else if (LINER == 1) {
    ggR = ggR + scale_y_continuous(limits = c(Ex()$y_minR, Ex()$y_maxR)) + scale_x_continuous(breaks = x_lineR(),limits = c(Ex()$x_minR, Ex()$x_maxR))
  } else if (LINER == 2) {
    ggR = ggR + scale_y_continuous(breaks = y_lineR(), limits = c(Ex()$y_minR, Ex()$y_maxR))
  } else {
    ggR = ggR + scale_y_continuous(breaks = y_lineR(), limits = c(Ex()$y_minR, Ex()$y_maxR)) + scale_x_continuous(breaks = x_lineR(),limits = c(Ex()$x_minR, Ex()$x_maxR))
  }
  
  lab_plotR <- strsplit(Ex()$labels_plotR, ",")[[1]]
  LABR <- Ex()$labelsfactorR
  
  if (LABR == 0) {
    ##
  } else {
    ggR <- ggR + geom_text(aes(label = lab_plotR),vjust = -1, hjust = 0.5,size = 5 ,check_overlap = TRUE)
  }
  
  if (R == "1") {
    ggR = ggR + theme(axis.text=element_text(size=14, face = "bold"))
  } else {
    ggR = ggR + theme(axis.text=element_text(size=14, face = "bold")) + geom_text(aes(x = X, y = Y, label = Y), vjust = -0.8, face = "bold")  
  }
  
  GRID <- Ex()$grid_type
  
  if (GRID == 1) {
    ggR = ggR + theme(panel.grid.major.x = element_line(color = "grey70"), panel.grid.major.y = element_line(color = "grey70"))
  } else if (GRID == 0) {
    ggR = ggR + theme(panel.grid = element_blank())
  } else if (GRID == 11) {
    ggR = ggR + theme(panel.grid.major.x = element_line(color = "grey70",linetype="dashed"), panel.grid.major.y = element_line(color = "grey70",linetype="dashed"))
  } else if (GRID == 2) {
    ggR = ggR + theme(panel.grid.major.y = element_line(color = "grey70"))
  } else if (GRID == 3) {
    ggR = ggR + theme(panel.grid.major.x = element_line(color = "grey70"))
  }
  
  
  
    ggR
  rv$ggR <- ggR
  rv$ggR
  })




