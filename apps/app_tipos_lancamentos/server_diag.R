
# Server da Aba do Teste Diagnóstico

# Função auxiliar para gerar a interface do teste
gerar_ui_teste <- function(conteudo) {
  fluidRow(
    column(4,
           box(title = HTML("<span style='font-size:20px; color:#003366; text-align: center;
               font-weight:bold;'>Menu de Opções:</span>"),
               headerBorder = F, width = 12, collapsible = FALSE,
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
      tagList(conteudo)
    )
  )
}

# Gerando valores aleatórios (compartilhados e específicos) para as questões:
valores_aleatorios <- reactive({
  input$gerar  # Atualiza quando clicar no botão
  set.seed(Sys.time()) # nova semente a cada execução
  
  list(
    comuns = list(
      g = 10,  # aceleração da gravidade fixa
      altura = sample(10:50, 1),
      tempo = sample(2:8, 1),
      v_horizontal = sample(5:25, 1),
      alcance = sample(50:200, 1)
    ),
    horizontal = list(
      v0_horiz = sample(80:180, 1),                 # velocidade inicial horizontal (m/s)
      distancia = sample(seq(1000, 2500, by = 100), 1),  # distância inicial entre ciclista e posto (m)
      v_ciclista = sample(5:15, 1),                 # velocidade do ciclista (m/s)
      d_inicial = sample(20:200, 1),                 # distância inicial do ciclista ao posto (m)
      v_carro = sample(10:30, 1),                    # velocidade do carro (m/s)
      t1 = sample(2:10, 1),                          # tempo qualquer (s)
      t2 = sample(1:6, 1),                           # tempo qualquer (s)
      coef1 = sample(2:20, 1),                       # coeficiente 1 para equações
      coef2 = sample(2:20, 1),                       # coeficiente 2 para equações
      coef3 = sample(2:20, 1)                        # coeficiente 3 para equações
    ),
    obliquo = list(
      v0_obliq = sample(10:30, 1),
      angulo = sample(c(15, 30, 45, 60), 1),
      v0y = sample(5:20, 1),
      coef1 = sample(2:5, 1),
      coef2 = sample(10:30, 1),
      tempo_voo = sample(3:8, 1)
    )
  )
})



# Conteúdo renderizado conforme o botão clicado
output$conteudo_teste <- renderUI({
  req(input$tipo_teste) 
  q <- valores_aleatorios()
  
  if (input$tipo_teste == "horizontal") {
    ## Perguntas para o teste horizontal
    
    perguntas_horizontal <- 
      tagList(
        HTML("<span style='font-size:25px; color:#003366; text-align: center;
                   font-weight:bold;'>🎯 Teste de Lançamento Horizontal</span>"),
        box(id = "box1", title = tagList(span("📚 Matemática Básica", 
                                              style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            
            fluidRow(
              column(12, 
               # Questão 1
               h5(withMathJax(HTML(sprintf("<strong> 1) (1 ponto) </strong> 
               Um veículo se desloca em linha reta a uma
               velocidade constante de \\(%g\\) km/h.
               Qual é a sua velocidade em m/s?", q$horizontal$v0_horiz)))),
               numericInput("resp1", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 2
               h5(withMathJax(HTML(sprintf("<strong> 2) (1 ponto) </strong> 
               Considere a equação \\(x = v_0 t\\). 
               Se \\(x = %g\\) e \\(t = %g\\), qual será o valor de \\(v_0\\)?", 
               q$horizontal$distancia, q$horizontal$t1)))),
               numericInput("resp2", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 3
               h5(withMathJax(HTML(sprintf("<strong> 3) (2 pontos) </strong> 
               Qual é o valor de x na equação: \\( {\\small{%g x + %g = %g}} \\) ?", 
               q$horizontal$coef1, q$horizontal$coef2, q$horizontal$coef3)))),
               numericInput("resp3", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 4
               h5(withMathJax(HTML(sprintf("<strong> 4) (3 pontos) </strong> 
               Considere a equação \\(t =\\sqrt{\\frac{2h}{g}} \\). 
               Se \\(t = %g\\) e \\(g = %g\\), qual será o valor de \\(h\\)?", 
               q$comuns$tempo, q$comuns$g)))),
               numericInput("resp4", label = "Resposta:", value = NULL, width = "50%")
              )
            )
        ),
        
        box(id = "box2", title = tagList(span("🚗 Conceitos de Movimento", 
                                              style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            fluidRow(
              column(12,
                     
               # Questão 5
               h5(withMathJax(HTML(sprintf("<strong> 5) (1 ponto) </strong> 
               Um carro percorre uma distância de \\(%g\\) metros em \\(%g\\) minutos.
               Qual é sua velocidade média em metros por segundo?", 
               q$horizontal$distancia, q$horizontal$t2)))),
               numericInput("resp5", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 6
               h5(withMathJax(HTML(sprintf("<strong> 6) (2 pontos) </strong> 
               Um veículo trafega a uma velocidade constante de \\(%g\\) km/h em uma autoestrada.
               Ao tocar o telefone celular do condutor, imprudentemente ele desvia sua atenção
               para o aparelho ao longo de \\(%g\\) segundos. 
               A distância percorrida pelo veículo durante
               os \\(%g\\) segundos em que se movimentou sem a atenção do condutor, em m,
               foi igual a:", q$horizontal$v0_horiz, q$horizontal$t1, q$horizontal$t1 )))),
               numericInput("resp6", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 7
               h5(withMathJax(HTML(sprintf("<strong> 7) (2 pontos) </strong> 
               Um veículo freia uniformemente de 
               \\(%g\\) m/s até parar em \\(%g\\) segundos. Qual foi a desaceleração?",
               q$horizontal$v0_horiz, q$horizontal$t2)))),
               numericInput("resp7", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 8
               h5(withMathJax(HTML(sprintf("<strong> 8) (3 pontos) </strong> Um ciclista 
               percorre uma estrada reta 
               a uma velocidade constante de \\(%g\\) m/s. No instante t=0, ele está a \\(%g\\) metros 
               de um posto de gasolina.
               Simultaneamente, um carro parte do posto de gasolina a uma velocidade
               constante de \\(%g\\) m/s, seguindo na mesma direção e sentido que o ciclista.
               Em que instante o carro alcançará o ciclista?", 
               q$horizontal$v_ciclista, q$horizontal$d_inicial, q$horizontal$v_carro
               )))),
               numericInput("resp8", label = "Resposta:", value = NULL, width = "50%")
              )
            )
            
        ),
        
        box(id = "box3", title = tagList(span("🎯 Lançamento Horizontal", 
                                              style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            fluidRow(
              column(12,
               # Questão 9
               h5(withMathJax(HTML(sprintf("<strong> 9) (1 ponto) </strong>
               Uma esteira horizontal despeja minério dentro de um vagão.
               As pedras de minério saem da esteira com velocidade horizontal
               de \\(%g\\) m/s e levam \\(%g\\) s numa trajetória parabólica até o centro do vagão.
               Considerando o peso como força resultante atuando em cada pedra
               e a aceleração da gravidade \\(g=%g m/s^2\\), o módulo do deslocamento
               vertical quando chegam ao vagão é:",
               q$horizontal$v0_horiz, q$comuns$tempo, q$comuns$g)))),
               numericInput("resp9", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 10
               h5(withMathJax(HTML(sprintf("<strong> 10) (2 pontos) </strong>
               Uma pedra é lançada horizontalmente de cima de uma
               mesa com altura de \\(%g\\) m e velocidade inicial de \\(%g\\) m/s.
               Considerando a aceleração da gravidade \\(g=%g m/s^2\\), qual é
               o tempo que a pedra leva para atingir o chão?",
               q$comuns$altura, q$horizontal$v0_horiz, q$comuns$g )))),
               numericInput("resp10", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 11
               h5(withMathJax(HTML(sprintf("<strong> 11) (3 pontos) </strong> Uma bola foi 
               lançada horizontalmente 
               do alto de um prédio de \\(%g\\) m de altura, com uma velocidade inicial de
               \\(%g\\) m/s.
               Considerando a aceleração da gravidade \\(g=%g m/s^2\\),
               qual é o alcance horizontal da bola?", 
               q$comuns$altura, q$horizontal$v0_horiz, q$comuns$g)))),
               ## colocar as equações entre \\( e \\), para ficar 'inline'
               numericInput("resp11", label = "Resposta:", value = NULL, width = "50%"),
               
               # Questão 12
               h5(withMathJax(HTML(sprintf("<strong> 12) (3 pontos) </strong>
               Considere uma flecha sendo lançada horizontalmente do alto de um edifício
               de \\(%g\\) m de altura, e tendo um alcance horizontal igual a \\(%g\\) m.
               Supondo a aceleração da gravidade \\(g=%g m/s^2\\), qual é a
               velocidade com que a flecha foi lançada, em m/s?", 
               q$comuns$altura, q$comuns$alcance, q$comuns$g)))),
               numericInput("resp12", label = "Resposta:", value = NULL, width = "50%")
              )
            )
        ),
        
        box(id = "box4", title = tagList(span("📜 Formulário Auxiliar", 
                                              style = "font-size: 20px;")),
            solidHeader = TRUE, collapsible = FALSE, width = 12,
            
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
        )
      )
      gerar_ui_teste(perguntas_horizontal)
      
  } else if (input$tipo_teste == "obliquo") {
    
    ## Perguntas para o teste obliquo
    perguntas_obliquo <- 
      tagList(
        HTML("<span style='font-size:25px; color:#003366; text-align: center;
                   font-weight:bold;'>🚀 Teste de Lançamento Oblíquo</span>"),
        box(id = "box1", title = tagList(span("📚 Matemática Básica", 
                                              style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            fluidRow(
              column(12,
               # Questão 1
               h5(withMathJax(HTML(sprintf("<strong> 1) (1 ponto) </strong>
               Converta a velocidade de \\(%g\\, km/h\\) para \\(m/s\\).", 
               q$obliquo$v0_obliq * 3.6)))),
               numericInput("resp1", "Resposta:", NULL, width = "50%"),
               
               # Questão 2
               h5(withMathJax(HTML(sprintf("<strong> 2) (1 ponto) </strong>
               Resolva a equação \\(%g x + %g = 0\\).", 
               q$obliquo$coef1, q$obliquo$coef2)))),
               numericInput("resp2", "Resposta:", NULL, width = "50%"),
               
               # Questão 3
               h5(withMathJax(HTML(sprintf("<strong> 3) (2 pontos) </strong>
               Se \\(t = %g\\, s\\), 
               calcule a altura \\(h\\) usando \\(h = v t - (1/2) g t^2\\). 
               Considere \\(v = %g\\, m/s\\) e \\(g = %g\\, m/s^2\\).", 
               q$comuns$tempo, q$obliquo$v0y, q$comuns$g)))),
               numericInput("resp3", "Resposta:", NULL, width = "50%"),
               
               # Questão 4
               h5(withMathJax(HTML(sprintf("<strong> 4) (3 pontos) </strong>
               Resolva a equação \\(-%g t^2 + %g t = 0\\), 
               considerando apenas a solução com \\(t \\ne 0\\).",
               q$obliquo$coef1, q$obliquo$coef2)))),
               numericInput("resp4", "Resposta:", NULL, width = "50%")
              )
            )
        ),
        
        box(id = "box2", title = tagList(span("🚗 Conceitos de Movimento", 
                                              style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            fluidRow(
              column(12,
                     
               # Questão 5
               h5(withMathJax(HTML(sprintf("<strong> 5) (1 ponto) </strong>
               Um projétil é lançado com \\(v_0 = %g\\, m/s\\) 
               a um ângulo de \\(%g^\\circ\\). Calcule a componente horizontal \\(v_{0x}\\).", 
               q$obliquo$v0_obliq, q$obliquo$angulo)))),
               numericInput("resp5", "Resposta:", NULL, width = "50%"),
               
               # Questão 6
               h5(withMathJax(HTML(sprintf("<strong> 6) (2 pontos) </strong>
               Um projétil tem componente vertical 
               da velocidade inicial \\(v_{0y} = %g\\, m/s\\). Sabendo que a aceleração da gravidade 
               é \\(g = 10\\, m/s^2\\), qual o tempo que o projétil leva para atingir a altura máxima?", 
               q$obliquo$v0y)))),
               numericInput("resp6", "Resposta:", NULL, width = "50%"),
               
               # Questão 7
               h5(withMathJax(HTML(sprintf("<strong> 7) (2 pontos) </strong>
               Um projétil é lançado 
               com velocidade inicial \\(v_0 = %g\\, m/s\\) a um ângulo \\(\\theta = %g^\\circ\\). 
               Qual é a componente vertical da velocidade inicial \\(v_{0y}\\)?", 
               q$obliquo$v0_obliq, q$obliquo$angulo)))),
               numericInput("resp7", "Resposta:", NULL, width = "50%"),
               
               # Questão 8
               h5(withMathJax(HTML(sprintf("<strong> 8) (3 pontos) </strong>
               Um projétil é lançado com componente 
               horizontal da velocidade \\(v_{0x} = %.2f\\, m/s\\) e atinge o solo com velocidade 
               vertical \\(v_{y} = %.2f\\, m/s\\).
               Calcule a velocidade resultante ao atingir o solo.", 
               q$obliquo$v0_obliq * cos(q$obliquo$angulo * pi / 180),
               q$obliquo$v0y + q$comuns$g * q$obliquo$tempo_voo)))),
               numericInput("resp8", "Resposta:", NULL, width = "50%")
                     
              )
            )
        ),
        
        box(
          id = "box3", 
          title = tagList(span("🚀 Lançamento Oblíquo", style = "font-size: 20px;")),
          solidHeader = TRUE, width = 12,
          fluidRow(
            column(12,
                   
             # Questão 9
             h5(withMathJax(HTML(sprintf(
             "<strong>9) (1 ponto) </strong>
             Um bombeiro precisa lançar um jato de água para apagar um fogo no topo de um prédio 
             de \\(%g\\, m\\) de altura. 
             Ele posiciona a mangueira no solo e dispara a água com velocidade inicial \\(%g\\, m/s\\), 
             a um ângulo de \\(%g^\\circ\\) com a horizontal. 
             Qual será a <strong>altura máxima</strong> atingida pelo jato de água?", 
             q$comuns$altura, q$obliquo$v0_obliq, q$obliquo$angulo
             )))),
             numericInput("resp9", label = "Resposta:", value = NULL, width = "50%"),
             
             # Questão 10
             h5(withMathJax(HTML(sprintf(
             "<strong>10) (2 pontos) </strong>
             Um jogador de futebol cobra uma falta chutando a bola com velocidade 
             inicial de \\(%g\\, m/s\\) 
             a um ângulo de \\(%g^\\circ\\) em relação ao solo. 
             Desprezando a resistência do ar, determine a <strong>distância horizontal</strong>
             que a bola percorre até atingir o solo.", 
             q$obliquo$v0_obliq, q$obliquo$angulo
             )))),
             numericInput("resp10", label = "Resposta:", value = NULL, width = "50%"),
             
             # Questão 11
             h5(withMathJax(HTML(sprintf(
             "<strong>11) (3 pontos) </strong>
             Durante um exercício de treinamento militar, um soldado lança uma granada 
             com velocidade inicial \\(%g\\, m/s\\) 
             a um ângulo de \\(%g^\\circ\\). Considerando \\(g = %g\\, m/s^2\\), 
             calcule o <strong>tempo total de voo</strong> até a granada tocar o solo.", 
             q$obliquo$v0_obliq, q$obliquo$angulo, q$comuns$g
             )))),
             numericInput("resp11", label = "Resposta:", value = NULL, width = "50%"),
             
             # Questão 12
             h5(withMathJax(HTML(sprintf(
             "<strong>12) (3 pontos) </strong>
             Um engenheiro quer projetar uma catapulta para lançar pedras que atinjam 
             uma altura máxima de \\(%g\\, m\\). 
             Se o ângulo de lançamento desejado é \\(%g^\\circ\\), 
             determine a <strong>velocidade inicial mínima</strong> necessária 
             para alcançar essa altura.", 
             q$comuns$altura, q$obliquo$angulo
             )))),
             numericInput("resp12", label = "Resposta:", value = NULL, width = "50%")
            )
          )
        ),
        
        box(
          id = "box4", 
          title = tagList(span("📜 Formulário Auxiliar", style = "font-size: 20px;")),
          solidHeader = TRUE, collapsible = FALSE, width = 12,
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
        <br>
        <li>Altura Máxima: \\( H = \\frac{v_0^2 \\sin^2(\\theta)}{2g} \\)</li>
        <br>
        <li>Tempo Total: \\( t_T = \\frac{2v_0 \\sin(\\theta)}{g} \\)</li>
      </ul>
      ")))),
            column(6,  # Segunda coluna
                   withMathJax(
                     h5(HTML("
      <ul>
        Lançamento Oblíquo:
        <br>
        <li> \\( x = v_0 \\cos(\\theta) t \\)</li>
        <br>
        <li> \\( y = v_0 \\sin(\\theta) t - \\tfrac{1}{2} g t^2 \\)</li>
        <br>
        <li>Alcance Horizontal: \\( s = \\frac{v_0^2 \\sin(2\\theta)}{g} \\)</li>
        <br>
        <li>Velocidade Inicial (a partir da altura): \\( v_0 = \\sqrt{\\frac{2gH}{\\sin^2(\\theta)}} \\)</li>
      </ul>
      "))))
          )
        )
        
      )
    
      gerar_ui_teste(perguntas_obliquo)
  }
})


observeEvent(input$calcular, {
  
  if (input$calcular == 1) {
    # calcula apenas na primeira vez
    showNotification("Pontuação calculada!")
    
    # muda o botão para “Já calculado”
    updateActionButton(session, "calcular", label = "Pontuação já calculada")
    
    # desativa o botão
    shinyjs::disable("calcular")
  }
  
  q <- valores_aleatorios()
  pontos <- 0
  margem_erro <- 0.1  # Margem de erro de 10%
  
  # Função para verificar se a resposta está correta (com margem de erro)
  resposta_correta <- function(resposta, correta) {
    if (is.null(resposta) || resposta == "" || is.na(as.numeric(resposta))) {
      return(FALSE)  
    }
    resposta <- as.numeric(resposta)
    return(abs(resposta - correta) <= margem_erro * abs(correta))
  }
  
  # Definir cálculos e pesos conforme o tipo de teste
  if (input$tipo_teste == "horizontal") {
    respostas <- list(
      round(q$horizontal$v0_horiz / 3.6, 2),                               # Q1
      round(q$horizontal$distancia / q$horizontal$t1, 2),                  # Q2
      round((q$horizontal$coef3 - q$horizontal$coef2) / q$horizontal$coef1, 2), # Q3
      round((q$comuns$g * (q$comuns$tempo)^2) / 2, 2),                     # Q4
      round(q$horizontal$distancia / (q$horizontal$t2 * 60), 2),           # Q5
      round((q$horizontal$v0_horiz / 3.6) * q$horizontal$t1, 2),           # Q6
      round(-q$horizontal$v0_horiz / q$horizontal$t2, 2),                  # Q7
      round(q$horizontal$d_inicial / (q$horizontal$v_carro - q$horizontal$v_ciclista), 2), # Q8
      round((q$comuns$g * (q$comuns$tempo)^2) / 2, 2),                     # Q9
      round(sqrt(2 * q$comuns$altura / q$comuns$g), 2),                    # Q10
      round(q$horizontal$v0_horiz * sqrt(2 * q$comuns$altura / q$comuns$g), 2), # Q11
      round(q$comuns$alcance / sqrt(2 * q$comuns$altura / q$comuns$g), 2)  # Q12
    )
    pesos <- c(1,1,2,3,1,2,2,3,1,2,3,3)
    
  } else if (input$tipo_teste == "obliquo") {
    respostas <- list(
      round(q$obliquo$v0_obliq, 2),                                  # Q1
      round(-q$obliquo$coef2 / q$obliquo$coef1, 2),                        # Q2
      round(q$obliquo$v0y * q$comuns$tempo - 0.5 * q$comuns$g * (q$comuns$tempo)^2, 2), # Q3
      round(q$obliquo$coef2 / q$obliquo$coef1, 2), # Q4
      
      round(q$obliquo$v0_obliq * cos(q$obliquo$angulo * pi / 180), 2),     # Q5
      round(q$obliquo$v0y / q$comuns$g, 2),                                # Q6
      round(q$obliquo$v0_obliq * sin(q$obliquo$angulo * pi / 180), 2),     # Q7
      round(sqrt((q$obliquo$v0_obliq * cos(q$obliquo$angulo * pi / 180))^2 + 
                   (q$obliquo$v0y + q$comuns$g * q$obliquo$tempo_voo)^2), 2), # Q8
      
      round((q$obliquo$v0_obliq^2 * sin(q$obliquo$angulo * pi / 180)^2) / (2*q$comuns$g), 2), # Q9
      round((q$obliquo$v0_obliq^2 * sin(2*q$obliquo$angulo * pi / 180)) / q$comuns$g, 2),     # Q10
      round((2*q$obliquo$v0_obliq * sin(q$obliquo$angulo * pi / 180)) / q$comuns$g, 2),       # Q11
      round(sqrt(2*q$comuns$g*q$comuns$altura)/(sin(q$obliquo$angulo * pi / 180)), 2)      # Q12
    )
    pesos <- c(1,1,2,3,1,2,2,3,1,2,3,3)
  }
  
  # Coletar respostas do usuário dinamicamente
  respostas_usuario <- sapply(1:12, function(i) input[[paste0("resp", i)]])
  
  # Verificar e somar pontos
  status <- Map(function(resp, correta, peso) {
    if (resposta_correta(resp, correta)) {
      pontos <<- pontos + peso
      return("✅ Correta")
    } else {
      return("❌ Errada")
    }
  }, respostas_usuario, respostas, pesos)
  
  # Criar tabela de resultados
  dados <- data.frame(
    Questão = paste0(1:12, ")"),
    # Sua_Resposta = respostas_usuario,
    Resposta_Correta = unlist(respostas),  
    Status = unlist(status),                
    check.names = FALSE
  )
  
  # Renderizar tabela
  output$tabela_erros <- renderDT({
    datatable(
      dados,
      options = list(
        pageLength = 12,
        dom = 't',
        columnDefs = list(
          list(targets = 1:2, className = "dt-center"),  # Centralizar colunas
          list(targets = 0, width = '15%'),             
          list(targets = 1, width = '15%'),
          list(targets = 2, width = '25%')
        )
      ),
      rownames = FALSE,
      class = 'compact'  # Linhas mais compactas
    )
  })
  
  
  # Feedback
  output$pontuacao <- renderText({ paste("⭐ Pontuação final:", pontos, "/ 24") })
  output$feedback <- renderText({
    if (pontos == 24) {
      "🥇 Parabéns! Você acertou todas! Pode avançar para desafios mais difíceis."
    } else if (pontos >= 12) {
      "🥈 Bom desempenho! Revise pequenos erros e tente novamente."
    } else {
      "🚀 Continue praticando! Estude a teoria e tente novamente."
    }
  })
  
})

# Resetar a tabela de erros, pontuação e feedback
observeEvent(list(input$gerar, input$tipo_teste), {
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
