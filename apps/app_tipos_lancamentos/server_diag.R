
# Server da Aba do Teste Diagnóstico
# Gerar questões aleatórias
questoes <- reactive({
  
  input$gerar  # Atualiza ao clicar no botão
  set.seed(Sys.time())  # Garante valores aleatórios a cada acesso
  
  list(
    q11 = sample(80:180, 1),
    q21 = sample(2:20, 1), q22 = sample(2:20, 1),
    q31 = sample(2:20, 1), q32 = sample(2:20, 1), q33 = sample(2:20, 1),
    q41 = sample(2:20, 1),
    q51 = sample(seq(1000,2500, by=100), 1), q52 = sample(2:10, 1),
    q61 = sample(60:180, 1), q62 = sample(1:6, 1),
    q71 = sample(10:100, 1), q72 = sample(4:10, 1),
    q81 = sample(5:20, 1), q82 = sample(50:300, 1), q83 = sample(25:30, 1),
    q91 = sample(2:15, 1), q92 = sample(seq(0.2,1,by = 0.1), 1),
    q101 = sample(seq(0.5,2,by = 0.1), 1), q102 = sample(seq(0.5,2.5,by = 0.1), 1),
    q111 = sample(seq(200,500,by = 20), 1), q112 = sample(5:20, 1),
    q121 = sample(seq(200,500,by = 20), 1), q122 = sample(5:30, 1)
  )
})

# Criar perguntas dinâmicas
output$perguntas_nivel1 <- renderUI({
  
  q <- questoes()
  
  fluidRow(
    column(11, 
           
           # Questão 1
           h5(withMathJax(HTML(sprintf("<strong> 1) (1 ponto) </strong> Um veículo se desloca em linha reta a uma
                                    velocidade constante de \\(%g\\) km/h.
                                    Qual é a sua velocidade em m/s?", q$q11)))),
           numericInput("resp1", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 2
           h5(withMathJax(HTML(sprintf("<strong> 2) (1 ponto) </strong> Considere a equação \\(x = v_0 t\\). 
                                    Se \\(x = %g\\) e \\(t = %g\\), qual será o valor de \\(v_0\\)?", 
                                       q$q21, q$q22)))),
           numericInput("resp2", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 3
           h5(withMathJax(HTML(sprintf("<strong> 3) (2 pontos) </strong> Qual é o valor de x na equação:
                         \\( {\\small{%g x + %g = %g}} \\) ?", q$q31, q$q32, q$q33)))),
           numericInput("resp3", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 4
           h5(withMathJax(HTML(sprintf("<strong> 4) (3 pontos) </strong> Considere a equação \\(t =\\sqrt{\\frac{2h}{g}} \\). 
                           Se \\(t = %g\\) e \\(g = 10\\), qual será o valor de \\(h\\)?", 
                                       q$q41)))),
           numericInput("resp4", label = "Resposta:", value = NULL, width = "50%")
           # helpText("Digite sua resposta acima ⬆️")
    )
  )
})

output$perguntas_nivel2 <- renderUI({
  
  q <- questoes()
  
  fluidRow(
    column(11,
           
           # Questão 5
           h5(withMathJax(HTML(sprintf("<strong> 5) (1 ponto) </strong> Um carro percorre uma distância
             de \\(%g\\) metros em \\(%g\\) minutos.
             Qual é sua velocidade média em metros por segundo?",
                                       q$q51, q$q52)))),
           numericInput("resp5", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 6
           h5(withMathJax(HTML(sprintf("<strong> 6) (2 pontos) </strong> Um veículo trafega a uma velocidade
             constante de \\(%g\\) km/h em uma autoestrada.
             Ao tocar o telefone celular do condutor, imprudentemente ele desvia sua atenção para o aparelho ao
             longo de \\(%g\\) segundos. A distância percorrida pelo veículo durante
             os \\(%g\\) segundos em que se movimentou sem a atenção do condutor, em m,
             foi igual a:", q$q61, q$q62, q$q62)))),
           numericInput("resp6", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 7
           h5(withMathJax(HTML(sprintf("<strong> 7) (2 pontos) </strong> Um veículo freia uniformemente de \\(%g\\) m/s até parar
             em \\(%g\\) segundos. Qual foi a desaceleração?",
                                       q$q71, q$q72)))),
           numericInput("resp7", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 8
           h5(withMathJax(HTML(sprintf("<strong> 8) (3 pontos) </strong> Um ciclista percorre uma estrada reta a uma
             velocidade constante de \\(%g\\) m/s. No instante t=0, ele está a \\(%g\\) metros de um posto de gasolina.
             Simultaneamente, um carro parte do posto de gasolina a uma velocidade
             constante de \\(%g\\) m/s, seguindo na mesma direção e sentido que o ciclista.
             Em que instante o carro alcançará o ciclista?",
                                       q$q81, q$q82, q$q83)))),
           numericInput("resp8", label = "Resposta:", value = NULL, width = "50%")
           
    )
  )
})

output$perguntas_nivel3 <- renderUI({
  q <- questoes()
  
  fluidRow(
    column(12,
           # Questão 9
           h5(withMathJax(HTML(sprintf("<strong> 9) (1 ponto) </strong>
               Uma esteira horizontal despeja minério dentro de um vagão.
               As pedras de minério saem da esteira com velocidade horizontal
               de \\(%g\\) m/s e levam \\(%g\\) s numa trajetória parabólica até o centro do vagão.
               Considerando o peso como força resultante atuando em cada pedra
               e a aceleração da gravidade \\(g=10 m/s^2\\), o módulo do deslocamento
               vertical quando chegam ao vagão é:",
                                       q$q91, q$q92)))),
           numericInput("resp9", label = "Resposta:", value = NULL, width = "50%"),
           # helpText("Digite sua resposta acima ⬆️"),
           
           # Questão 10
           h5(withMathJax(HTML(sprintf("<strong> 10) (2 pontos) </strong>
               Uma pedra é lançada horizontalmente de cima de uma
               mesa com altura de \\(%g\\) m e velocidade inicial de \\(%g\\) m/s.
               Considerando a aceleração da gravidade \\(g=10 m/s^2\\), qual é
               o tempo que a pedra leva para atingir o chão?",
                                       q$q101, q$q102)))),
           numericInput("resp10", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 11
           h5(withMathJax(HTML(sprintf("<strong> 11) (3 pontos) </strong> Uma bola foi lançada horizontalmente do
               alto de um prédio de \\(%g\\) m de altura, com uma velocidade inicial de
               \\(%g\\) m/s.
               Considerando a aceleração da gravidade \\(g=10 m/s^2\\),
               qual é o alcance horizontal da bola?",
                                       ## colocar as equações entre \\( e \\), para ficar 'inline'
                                       q$q111, q$q112)))),
           numericInput("resp11", label = "Resposta:", value = NULL, width = "50%"),
           
           # Questão 12
           h5(withMathJax(HTML(sprintf("<strong> 12) (3 pontos) </strong>
               Considere uma flecha sendo lançada horizontalmente do alto de um edifício
               de \\(%g\\) m de altura, e tendo um alcance horizontal igual a \\(%g\\) m.
               Supondo a aceleração da gravidade \\(g=10 m/s^2\\), qual é a
               velocidade com que a flecha foi lançada, em m/s?",
                                       q$q121, q$q122)))),
           numericInput("resp12", label = "Resposta:", value = NULL, width = "50%")
    )
  )
})

# Formulário Auxiliar - em 2 colunas
output$formulario <- renderUI({
  fluidRow(
    column(6,  # Primeira coluna
           withMathJax(
             h5(HTML("
               <ul>
                 <li>Velocidade Média: \\( v_m = \\frac{\\Delta S}{\\Delta t} \\)</li>
                  <br>
                 <li>Equação do Movimento Uniforme (MU): \\( S = S_0 + v t \\)</li>
                 <br>
                 <li>Equação do Movimento Uniformemente Variado (MUV): \\( v_f = v_0 + a t \\)</li>
               </ul>
               "))
           )
    ),
    column(6,  # Segunda coluna
           withMathJax(
             h5(HTML("
               <ul>
                 Lançamento Horizontal: 
                 <br>
                 <li> \\( x = v_0 t \\) </li>
                 <br>
                 <li>\\( y = \\frac{1}{2} g t^2 \\) </li>
               </ul>
               "))
           )
    )
  )
})

# Calcular pontuação e verificar respostas
observeEvent(input$calcular, {
  
  q <- questoes()
  pontos <- 0
  margem_erro <- 0.1  # Margem de erro de 10%
  
  # Respostas corretas
  # Nivel 1
  correta1 <- round((q$q11 / 3.6), 2)
  correta2 <- round((q$q21/q$q22), 2)
  correta3 <- round(((q$q33-q$q32)/q$q31), 2)
  correta4 <- round(((10*(q$q41^2))/2), 2)
  
  ## Nivel 2
  correta5 <- round((q$q51/(q$q52*60)), 2)
  correta6 <- round(((q$q61/3.6)*q$q62), 2)
  correta7 <- round((-q$q71/q$q72), 2)
  correta8 <- round((q$q82/(q$q83-q$q81)), 2)
  
  ## Nivel 3
  correta9 <- round((10*(q$q92^2)/2), 2)
  correta10 <- round((sqrt(2*q$q101/10)), 2)
  correta11 <- round((q$q112*(sqrt(2*q$q111/10))), 2)
  correta12 <- round((q$q122/(sqrt(2*q$q121/10))), 2)
  
  # Função para verificar se a resposta está correta (com margem de erro)
  resposta_correta <- function(resposta, correta) {
    if (is.null(resposta) || resposta == "" || is.na(as.numeric(resposta))) {
      return(FALSE)  
    }
    resposta <- as.numeric(resposta)
    return(abs(resposta - correta) <= margem_erro * abs(correta))
  }
  
  # Verificar respostas
  ## Nivel 1
  if (resposta_correta(input$resp1, correta1)) {
    pontos <- pontos + 1
  }
  if (resposta_correta(input$resp2, correta2)) {
    pontos <- pontos + 1
  }
  if (resposta_correta(input$resp3, correta3)) {
    pontos <- pontos + 2
  }
  if (resposta_correta(input$resp4, correta4)) {
    pontos <- pontos + 3
  }
  
  ## Nivel 2
  if (resposta_correta(input$resp5, correta5)) {
    pontos <- pontos + 1
  }
  if (resposta_correta(input$resp6, correta6)) {
    pontos <- pontos + 2
  }
  if (resposta_correta(input$resp7, correta7)) {
    pontos <- pontos + 2
  }
  if (resposta_correta(input$resp8, correta8)) {
    pontos <- pontos + 3
  }
  
  ## Nivel 3
  if (resposta_correta(input$resp9, correta9)) {
    pontos <- pontos + 1
  }
  if (resposta_correta(input$resp10, correta10)) {
    pontos <- pontos + 2
  }
  if (resposta_correta(input$resp11, correta11)) {
    pontos <- pontos + 3
  }
  if (resposta_correta(input$resp12, correta12)) {
    pontos <- pontos + 3
  }
  
  
  # Criar tabela com resultados
  dados <- data.frame(
    Questão = c("1)", "2)", "3)", "4)",
                "5)", "6)", "7)", "8)",
                "9)", "10)", "11)", "12)"),
    Sua_Resposta = c(input$resp1, input$resp2, input$resp3, input$resp4,
                     input$resp5, input$resp6, input$resp7, input$resp8,
                     input$resp9, input$resp10, input$resp11, input$resp12),
    Resposta_Correta = c(correta1, correta2, correta3, correta4,
                         correta5, correta6, correta7, correta8,
                         correta9, correta10, correta11, correta12),
    Status = c(
      ifelse(resposta_correta(input$resp1, correta1), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp2, correta2), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp3, correta3), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp4, correta4), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp5, correta5), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp6, correta6), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp7, correta7), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp8, correta8), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp9, correta9), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp10, correta10), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp11, correta11), "✅ Correta", "❌ Errada"),
      ifelse(resposta_correta(input$resp12, correta12), "✅ Correta", "❌ Errada")
    )
  )
  
  colnames(dados) <- c("Questão", "Sua Resposta", "Resposta Correta", "Status")
  
  ## Tabela de Erros
  output$tabela_erros <- renderDT({
    datatable(dados, options = list(
      pageLength = 12,
      dom = 't',
      columnDefs = list(
        list(targets = 1:2, className = "dt-center"),  # Centralizar as colunas
        list(targets = 0, width = '20%'),  # Coluna de Questão
        list(targets = 1, width = '20%'),  # Coluna de Sua Resposta
        list(targets = 2, width = '30%')   # Coluna de Resposta Correta
      )
    ), rownames = FALSE, escape = F)
  })
  
  # Exibir pontuação e feedback
  output$pontuacao <- renderText({ paste("⭐ Pontuação final:", pontos, "/ 24") })
  output$feedback <- renderText({ feedback_text })
  
  # Mensagem de feedback
  if (pontos == 24) {
    feedback_text <- "🥇 Parabéns! Você acertou todas! Pode avançar para desafios mais difíceis."
  } else if (pontos >= 12) {
    feedback_text <- "🥈 Bom desempenho! Revise pequenos erros e tente novamente."
  } else {
    feedback_text <- "🚀 Continue praticando! Estude a teoria e tente novamente."
  }
  
})

observeEvent(input$gerar, {
  # Resetar a tabela de erros, pontuação e feedback
  output$tabela_erros <- renderDT({
    datatable(NULL, options = list(
      pageLength = 12,
      dom = 't',
      columnDefs = list(
        list(targets = 1:2, className = "dt-center"),  # Centralizar as colunas
        list(targets = 0, width = '20%'),  # Coluna de Questão
        list(targets = 1, width = '20%'),  # Coluna de Sua Resposta
        list(targets = 2, width = '30%')   # Coluna de Resposta Correta
      )
    ), rownames = FALSE, escape = F)
  })
  
  output$pontuacao <- NULL
  output$feedback <- NULL
})
