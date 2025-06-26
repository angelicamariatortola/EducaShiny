#----------------------------------------------------------------------
#Medidas de Posição

req(df())
req(dfa())

nums <- reactive ({ c(df()$Y) })
sortedNums <- sort(nums())
termos2 <- paste(sortedNums, collapse = " , ")
media <- reactive ({ mean(nums()) })
termosdespavar <- paste(sprintf("(%.2f - %.2f)^2", nums(), media()), collapse = " + ")
termos_freq_somados <- paste(dfa()$Freq, collapse=" + ")

observeEvent(input$selecao_tabela, {
  if (input$selecao_tabela == "Tabela de Dados") {
    output$tabela_ymed <- renderDT({
      dfmed <- df()
      colnames(dfmed) <- c(Ex()$eixo_y, Ex()$eixo_x)
      datatable(dfmed, selection = "single")
    })
  } else if (input$selecao_tabela == "Tabela de Frequências") {
    output$tabela_ymed <- renderDT({
      dfa1 <- dfa()
      colnames(dfa1) <- c(Ex()$eixo_y, "Frequência")
      datatable(dfa1, selection = "single")
    })
  }
})

#-----------------------------------------------------------------
#média
observeEvent(input$media, {
  output$MediaMedianaModaFor <- renderUI({
    termos <- paste(nums(), collapse = " + ")
    formula <- paste0("$$\\text{Média} = \\frac{", termos, "}{", length(nums()), "}$$")
    tagList(
      h4(strong("Média"), style = "text-align: center;"),
      p("A média aritmética é um conceito fundamental em estatística, representando o valor central de um conjunto de números. É calculada somando todos os números do conjunto e dividindo o total pelo número de valores.", style = "text-align: justify;"),
      p(withMathJax(helpText(formula))),
      p(withMathJax(helpText("$$\\text{Média} =", media(), "$$"))),
      p("Também é possível calcular a média com as frequências, em que a média é calculada multiplicando cada número pela sua frequência e somando os resultados. Depois, divide-se a soma pelo total de frequências.", style = "text-align: justify;"),
      p(withMathJax(helpText(paste0("$$\\text{Média} = \\frac{", 
                                    paste(dfa()$Var1, "\\times", dfa()$Freq, sep="", collapse=" + "), 
                                    "}{", termos_freq_somados, "}$$")))),
      p(withMathJax(helpText(paste0("$$\\text{Média} =", media(), "$$"))))
    )
  })
})

#-----------------------------------------------------------------
#mediana

observeEvent(input$mediana, {
  output$MediaMedianaModaFor <- renderUI({
    medianValue <- median(nums())
    sequenciamediana <- length(nums())
    if (sequenciamediana %% 2 == 1) {
      medianFormula <- paste0("$$", termos2 , "$$")
    } else {
      medianFormula <- paste0("$$", termos2, "\\\\", "\\frac{", sortedNums[length(nums()) / 2], " + ", sortedNums[length(nums()) / 2 + 1], "}{2}$$")
    }
    tagList(
      h4(strong("Mediana"), style = "text-align: center;"),
      p("A mediana é o valor que separa a metade superior da metade inferior de um conjunto de dados. Se o número de observações for ímpar, a mediana é o valor central. Se for par, é a média dos dois valores centrais.", style = "text-align: justify;"),
      p(withMathJax(helpText(medianFormula))),
      p(withMathJax(helpText(paste0("$$\\text{Mediana} =", medianValue, "$$")))),
      p("Analisando pela tabela de frequências, é necessário colocar todos os valores em uma lista crescente e respeitando suas devidas frequências. Então fazemos o processo anterior e encontramos o valor central.", style = "text-align: justify;")
    )
  })
})

#-----------------------------------------------------------------
#moda

observeEvent(input$moda, {
  modaValues <- sort(table(nums()), decreasing = TRUE)
  moda <- names(modaValues[modaValues == modaValues[1]])
  termosModa <- paste(moda, collapse = ", ")
  formulaModa <- paste0("$$\\text{Moda} =", termosModa, "$$")
  
  # Verifique se há uma moda (ou seja, se todos os números têm frequência 1)
  if (length(unique(nums())) == length(nums())) {
    # Não há moda
    output$MediaMedianaModaFor <- renderUI({
      tagList(
        h4(strong("Moda"), style = "text-align: center;"),
        p("A moda é o valor ou valores que aparecem com mais frequência em um conjunto de dados.", style = "text-align: justify;"),
        p("Não existe uma moda nesta sequência.", style = "text-align: justify;"),
        p("Pois todos os números possuem a mesma frequência, como pode ser visto a seguir:", style = "text-align: justify;"),
        tags$ul(
          lapply(1:nrow(dfa()), function(i) {
            tags$li(
              withMathJax(paste0("$$",dfa()$Var1[i], " \\rightarrow ", dfa()$Freq[i],"$$"))
            )
          })
        )
      )
    })
  } else {
    # Há uma ou mais modas
    output$MediaMedianaModaFor <- renderUI({
      tagList(
        h4(strong("Moda"), style = "text-align: center;"),
        p("A moda é o valor ou valores que aparecem com mais frequência em um conjunto de dados.", style = "text-align: justify;"),
        p(withMathJax(helpText(formulaModa))),
        p("Com as frequências fica muito mais fácil! A seguir temos os valores à esquerda e as frequências respectivas à direita.", style = "text-align: justify;"),
        tags$ul(
          lapply(1:nrow(dfa()), function(i) {
            tags$li(
              withMathJax(paste0("$$",dfa()$Var1[i], " \\rightarrow ", dfa()$Freq[i],"$$"))
            )
          })
        ),
        p("O termo que tem a maior frequência é a moda, então:", style = "text-align: justify;"),
        withMathJax(paste0("$$\\text{Moda} =",termosModa,"$$"))
      )
    })
  }
})

#------------------------------------------------------------------------------------------------
#Medidas de Dispersão


#desvio padrão

observeEvent(input$desvpav, {
  output$AmpDesVarFor <- renderUI({
    desvioPadraoValor <- sd(nums())
    formulaGeral <- "$$\\text{Desvio Padrão} = \\sqrt{\\frac{\\sum_{i=1}^{n}(x_i - \\bar{x})^2}{n}}$$"
    formula <- paste0("$$\\sigma = \\sqrt{\\frac{1}{", length(nums()), "}(", termosdespavar, ")}$$")
    tagList(
      h4(strong("Desvio Padrão"), style = "text-align: center;"),
      p("O desvio padrão é uma medida da quantidade de variação ou dispersão de um conjunto de valores.", style = "text-align: justify;"),
      p(withMathJax(helpText(formulaGeral))),
      p(withMathJax(helpText(formula))),
      p(withMathJax(helpText(paste0("$$\\text{Desvio Padrão} =", desvioPadraoValor, "$$"))))
    )
  })
})

#-----------------------------------------------------------------
#amplitude

observeEvent(input$amplitude, {
  output$AmpDesVarFor <- renderUI({
    maxValor <- max(nums())
    minValor <- min(nums())
    amplitudeValor <- maxValor - minValor
    formulaGeral <- "$$\\text{Amplitude} = x_{max} - x_{min}$$"
    formula <- paste0("$$\\text{Amplitude} = ", maxValor, " - ", minValor, " = ", amplitudeValor, "$$")
    tagList(
      h4(strong("Amplitude"), style = "text-align: center;"),
      p("A amplitude é a diferença entre o maior e o menor valor em um conjunto de números.", style = "text-align: justify;"),
      p(withMathJax(helpText(formulaGeral))),
      p(withMathJax(helpText(formula))),
      p(withMathJax(helpText(paste0("$$\\text{Amplitude} =", amplitudeValor, "$$"))))
    )
  })
})


#-----------------------------------------------------------------
#variância

observeEvent(input$var, {
  output$AmpDesVarFor <- renderUI({
    varianciaValor <- var(nums())
    formulaGeral <- "$$\\text{Variância} = \\frac{\\sum_{i=1}^{n}(x_i - \\bar{x})^2}{n}$$"
    formula <- paste0("$$\\sigma^2 = \\frac{1}{", length(nums()), "}(", termosdespavar, ")$$")
    tagList(
      h4(strong("Variância"), style = "text-align: center;"),
      p("A variância é uma medida de dispersão que representa a média das diferenças quadradas em relação à média.", style = "text-align: justify;"),
      p(withMathJax(helpText(formulaGeral))),
      p(withMathJax(helpText(formula))),
      p(withMathJax(helpText(paste0("$$\\text{Variância} =", varianciaValor, "$$"))))
    )
  })
})
