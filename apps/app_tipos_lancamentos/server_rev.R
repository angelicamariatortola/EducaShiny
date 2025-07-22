
  # Variável reativa para armazenar qual botão foi clicado
  menu_revisao <- reactiveVal("matematica")  # inicia com matemática
  
  # Observadores de cada botão
  observeEvent(input$menu_mat,  menu_revisao("matematica"))
  observeEvent(input$menu_mov,  menu_revisao("movimento"))
  observeEvent(input$menu_lanc, menu_revisao("horizontal"))
  observeEvent(input$menu_lanc_obli, menu_revisao("obliquo"))
  observeEvent(input$menu_vid,  menu_revisao("video"))
  
  # Conteúdo renderizado conforme o botão clicado
  output$conteudo_revisao <- renderUI({
    req(menu_revisao())
    
    if (menu_revisao() == "matematica") {
      withMathJax(
        h2("📚 Matemática Básica"),
        tagList(
          box(id = "box1", 
              title = tagList(span("📌 Equação do 1º Grau", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              h4("Forma geral:"),
              tags$p("A equação do 1º grau é escrita como:"),
              tags$p("\\( \\mathbf{ ax + b = 0 } \\)"),
              tags$ul(
                tags$li("\\(a\\) e \\(b\\) são números reais, com \\(a \\neq 0\\)."),
                tags$li("\\(x\\) é a incógnita (valor desconhecido).")
              ),
              h4("Resolução:"),
              tags$p("Para encontrar \\(x\\), isolamos a variável:"),
              tags$p("\\(x = \\mathbf{-\\dfrac{b}{a}}\\)"),
              h4("🔹 Exemplo:"),
              tags$p(withMathJax("Se \\(2x - 6 = 0\\), então \\( \\mathbf{ x = \\dfrac{6}{2} = 3 } \\)"))
          ),
          
          box(id = "box2", 
              title = tagList(span("📌 Equação do 2º Grau", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              h4("Forma geral:"),
              tags$p("A equação do 2º grau tem a forma:"),
              tags$p(withMathJax("\\( \\mathbf{ax^2 + bx + c = 0} \\)")),
              tags$ul(
                tags$li("\\(a\\), \\(b\\) e \\(c\\) são números reais, com \\(a \\neq 0\\)."),
                tags$li("\\(x\\) é a incógnita.")
              ),
              h4("Resolução pela Fórmula de Bhaskara:"),
              tags$p("As raízes são calculadas por:"),
              tags$p(withMathJax("\\( x = \\mathbf{ \\dfrac{-b \\pm \\sqrt{\\Delta}}{2a}} \\)")),
              tags$p(withMathJax("Com \\(\\Delta\\) (delta) dado por: \\(\\mathbf{\\Delta = b^2- 4ac} \\)")),
              h4("🔹 Exemplo:"),
              tags$p("Resolva \\(x^2 - 5x + 6 = 0\\):"),
              tags$ul(
                tags$li("Coeficientes: \\(a = 1\\), \\(b = -5\\) e \\(c = 6\\)."),
                tags$li("\\(\\Delta = (-5)^2-4(1)(6) \\rightarrow \\Delta = \\mathbf{25-24=1}\\)."),
                tags$li("Então, \\(x = \\dfrac{(5 \\pm 1)}{2} \\rightarrow \\mathbf{x_1 = 3}\\), 
                        \\(\\mathbf{x_2 = 2}\\).")
              )
          ),
          
          box(id = "box3", 
              title = tagList(span("📌 Trigonometria Básica", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              h4("Relações fundamentais:"),
              tags$p("Em um triângulo retângulo:"),
              tags$p(withMathJax("\\({sen(\\theta) = \\dfrac{cateto\\ oposto}{hipotenusa}}\\)")),
              tags$p(withMathJax("\\({\\cos(\\theta) = \\dfrac{cateto\\ adjacente}{hipotenusa}}\\)")),
              tags$p(withMathJax("\\({\\tan(\\theta) = \\dfrac{cateto\\ oposto}{cateto\\ adjacente}}\\)"))
          ),
          
          box(id = "box4", 
              title = tagList(span("📌 Sistema de Equações", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              h4("Forma geral:"),
              tags$p("Um sistema com duas equações e duas incógnitas:"),
              tags$p(withMathJax("\\begin{array}{l} a_1x + b_1y = c_1 \\\\ a_2x + b_2y = c_2 \\end{array}")),
              h4("Resolução:"),
              tags$p("Pode ser resolvido por substituição ou adição."),
              h4("🔹 Exemplo:"),
              tags$p("Resolva o sistema:"),
              tags$p(withMathJax("\\begin{array}{l} x + y = 5 \\\\ 2x - y = 1 \\end{array}")),
              tags$p("Solução: \\(x=2, y=3\\).")
          ),
          
          box(id = "box3", 
              title = tagList(span("📌 Mais Informações", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              p("Para mais informações consulte os sites abaixo:"),
              tags$ul(
                tags$li(a("🔗 Equação do 1º Grau – Brasil Escola", 
                          href = "https://brasilescola.uol.com.br/matematica/equacao-do-1-grau.htm", 
                          target = "_blank")),
                tags$li(a("🔗 Equação do 2º Grau – Mundo Educação", 
                          href = "https://mundoeducacao.uol.com.br/matematica/equacao-2-grau.htm", 
                          target = "_blank")),
                tags$li(a("🔗 Trigonometria Básica – Toda Matéria", 
                          href = "https://www.todamateria.com.br/trigonometria/", 
                          target = "_blank")),
                tags$li(a("🔗 Sistemas de Equações – Brasil Escola", 
                          href = "https://brasilescola.uol.com.br/matematica/sistema-duas-equacoes.htm", 
                          target = "_blank"))
              ),
              p("Clique nos links para acessar materiais de apoio sobre os temas.")
          )
          
        )
      )
      
      
    } else 
      if (menu_revisao() == "movimento") {
        tagList(
          h2("🚗 Conceitos de Movimento"),
          
          box(id = "box1", 
              title = tagList(span("🔎️ Cinemática",
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              p("A Cinemática é a área da Física que estuda o movimento dos corpos 
        sem considerar as causas (forças). Ela descreve como os objetos se movem, 
        analisando grandezas como posição, velocidade e aceleração."),
              tags$ul(
                tags$li(strong("Posição:"), "indica onde o corpo está em um determinado instante."),
                tags$li(strong("Velocidade:"), "indica o quão rápido e em qual direção o corpo se desloca."),
                tags$li(strong("Aceleração:"), "variação da velocidade ao longo do tempo.")
              ),
              p("O site abaixo fornece mais detalhes:"),
              actionButton("link2", "🔗 Acessar Site",
                           onclick = "window.open('https://www.todamateria.com.br/cinematica/', '_blank')",
                           class = "btn btn-primary")
          ),
          
          box(id = "box2", 
              title = tagList(span("🚗 ➡️➡️➡️️ Movimento Uniforme (MU)",
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              p("Tipo de Movimento na Cinemática com as características:"),
              tags$ul(
                tags$li("Velocidade", strong("constante.")),
                tags$li("Aceleração", strong("zero.")),
                tags$li("O gráfico de", strong("posição x tempo"), "é uma",
                        strong("reta"), "inclinada.")
              ),
              p(strong("Exemplo:"), "um carro que anda a 60 km/h sem acelerar nem frear."),
              p("O site abaixo fornece mais detalhes:"),
              actionButton("link3", "🔗 Acessar Site",
                           onclick = "window.open('https://www.todamateria.com.br/movimento-uniforme/', '_blank')",
                           class = "btn btn-primary")
          ),
          
          box(id = "box3", 
              title = tagList(span("🚗 ⏩⏩⏩ Movimento Uniformemente Variado (MUV)",
                                   style = "font-size: 20px;")), 
              solidHeader = TRUE, width = 12,
              p("Tipo de Movimento na Cinemática com as características:"),
              tags$ul(
                tags$li("Velocidade", strong("varia"), "ao longo do tempo."),
                tags$li("Aceleração", strong("constante.")),
                tags$li("O gráfico de", strong("posição x tempo"), "é uma", strong("parábola."))
              ),
              p(strong("Exemplo:"), "um carro que acelera ou freia."),
              p("O site abaixo fornece mais detalhes:"),
              actionButton("link4", "🔗 Acessar Site",
                           onclick = "window.open('https://www.todamateria.com.br/movimento-uniformemente-variado/', '_blank')",
                           class = "btn btn-primary")
          ),
          
          box(id = "box4", 
              title = tagList(span("📐 Grandezas Vetoriais e Escalares",
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              p("No lançamento oblíquo é importante diferenciar entre:"),
              tags$ul(
                tags$li(strong("Grandezas escalares:"), "têm apenas valor numérico (ex: tempo, massa, temperatura)."),
                tags$li(strong("Grandezas vetoriais:"), "têm módulo, direção e sentido (ex: velocidade e aceleração).")
              ),
              p("🔹 Essas grandezas serão usadas para decompor o vetor velocidade inicial."),
              actionButton("link5", "🔗 Trigonometria para decomposição vetorial",
                           onclick = "window.open('https://www.todamateria.com.br/vetores/', '_blank')",
                           class = "btn btn-primary")
          )
          
          # box(id = "box3", 
          #     title = tagList(span("📈 Composição de Movimentos",
          #                          style = "font-size: 20px;")),
          #     solidHeader = TRUE, width = 12,
          #     p("No lançamento oblíquo, o movimento pode ser dividido em dois componentes:"),
          #     tags$ul(
          #       tags$li(strong("Movimento horizontal:"), "é uniforme (MU), pois a velocidade na horizontal é constante."),
          #       tags$li(strong("Movimento vertical:"), "é uniformemente variado (MUV), pois a aceleração da gravidade atua.")
          #     ),
          #     p("🔹 A combinação dos dois resulta em uma trajetória parabólica."),
          #     actionButton("link6", "🔗 Lançamento Oblíquo – Mundo Educação",
          #                  onclick = "window.open('https://mundoeducacao.uol.com.br/fisica/lancamento-obliquo.htm', '_blank')",
          #                  class = "btn btn-primary")
          # )
        )
        
      
    } else 
      if (menu_revisao() == "horizontal") {
      tagList(
        h2("🎯 Lançamento Horizontal"),
        box(id = "box1", 
            title = tagList(span( "📌 Conceitos Básicos", 
                                 style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
          
          p("O lançamento horizontal é um tipo de movimento", strong("bidimensional"), 
            "que combina um", strong("Movimento Uniforme (MU)"), "na direção horizontal 
                com um", strong("Movimento Uniformemente Variado (MUV)"), "na direção vertical."),
          tags$ul(
            tags$li("Na direção", strong("horizontal"), "→", strong("Movimento Uniforme 
                        (MU)"), "pois não há aceleração (se ignorarmos o ar). 
                        A velocidade inicial na horizontal permanece constante."),
            tags$li("Na direção", strong("vertical"), "→", 
                    strong("Movimento Uniformemente Variado (MUV)"), 
                    "pois não há aceleração (se ignorarmos o ar). 
                 A velocidade inicial na horizontal permanece constante."),
            
            tags$li("Na direção vertical → Movimento Uniformemente Variado (MUV), 
                        pois o objeto sofre aceleração gravitacional (g = 9,8 m/s²). 
                        Ele cai cada vez mais rápido."),
            tags$li("A trajetória do objeto forma uma curva parabólica.")
          ),
          p(strong("Exemplos:")),
          tags$ul(
            tags$li("Uma bola rolando de uma mesa e caindo."),
            tags$li("Um avião lançando um pacote de suprimentos.")
          )
        ),
        
        box(id = "box2", 
            title = tagList(span("💡 Resumo da comparação:", 
                                  style = "font-size: 20px;")),
          solidHeader = TRUE, width = 12,
          tableOutput("tabela"),
          
          p("Ou seja, o", strong("lançamento horizontal"), "é a combinação de",
            strong("MU na horizontal"), "e", strong("MUV na vertical!"), "🚀")
          
        ),
        
        box(id = "box3", 
          title = tagList(span("📌 Mais Informações", 
                               style = "font-size: 20px;")),
          solidHeader = TRUE, width = 12,
          p("Para mais informações consulte os sites:"),  
          actionButton("link5", "🔗 Site Toda Matéria",
                       onclick = "window.open('https://www.todamateria.com.br/lancamento-horizontal/', '_blank')",
                       class = "btn btn-primary"),
          actionButton("link6", "🔗 Site Mundo Educação",
                       onclick = "window.open('https://mundoeducacao.uol.com.br/fisica/lancamento-horizontal.htm', '_blank')",
                       class = "btn btn-primary")
        )
      )
      
    } else 
      if (menu_revisao() == "obliquo") {
        tagList(
          h2("🚀 Lançamento Oblíquo"),
          
          box(id = "box1", 
              title = tagList(span("📌 Conceitos Básicos", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              
              p("O lançamento oblíquo é um tipo de movimento ", strong("bidimensional"), 
                " que combina um ", strong("Movimento Uniforme (MU)"), 
                " na direção horizontal com um ", strong("Movimento Uniformemente Variado (MUV)"), 
                " na direção vertical."),
              
              tags$ul(
                tags$li("Na direção ", strong("horizontal"), " → ", 
                        strong("Movimento Uniforme (MU)"), 
                        ", pois não há aceleração horizontal (desprezando o ar). A velocidade horizontal permanece constante: ", 
                        withMathJax("$${v_x = v_0 \\cdot \\cos(\\theta)}$$")),
                
                tags$li("Na direção ", strong("vertical"), " → ", 
                        strong("Movimento Uniformemente Variado (MUV)"), 
                        ", pois o objeto está sujeito à aceleração gravitacional. A velocidade vertical varia com o tempo: ", 
                        withMathJax("$${v_y = v_0 \\cdot sen(\\theta) - g \\cdot t}$$")),
                
                tags$li("A trajetória do objeto é uma curva parabólica."),
                tags$li("No ponto mais alto da trajetória (altura máxima), a velocidade vertical é zero: ", 
                        withMathJax("$${v_y = 0,}$$"), "mas a velocidade horizontal se mantém constante.")
              ),
              
              p(strong("Principais Equações:")),
              tags$ul(
                tags$li("Alcance horizontal:", withMathJax("$${s = \\frac{v_0^2 \\cdot sen(2\\theta)}{g}}$$")),
                tags$li("Altura máxima:", withMathJax("$${H = \\frac{v_0^2 \\cdot sen^2(\\theta)}{2g}}$$")),
                tags$li("Tempo total:", withMathJax("$${t_T = \\frac{2 \\cdot v_0 \\cdot sen(\\theta)}{g}}$$"))
              ),
              
              p(strong("Exemplos:")),
              tags$ul(
                tags$li("Um jogador chutando uma bola de futebol para o gol."),
                tags$li("Um foguete de artifício lançado em um ângulo com o solo.")
              )
              
              # # Diagrama ilustrativo
              # img(src = "www/diagrama_lancamento_obliquo.png", 
              #     width = "100%", 
              #     style = "display: block; margin: auto;")
          ),
          
          box(id = "box2", 
              title = tagList(span("💡 Resumo da comparação:", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              
              p("O ", strong("lançamento oblíquo"), " é a combinação de ",
                strong("MU na horizontal"), " e ", strong("MUV na vertical!"), " 🚀")
              
              # p("Veja a decomposição dos movimentos:"),
              # 
              # tableOutput("tabela_obliquo")
          ),
          
          box(id = "box3", 
              title = tagList(span("📌 Mais Informações", 
                                   style = "font-size: 20px;")),
              solidHeader = TRUE, width = 12,
              
              p("Para mais informações consulte os sites:"),  
              actionButton("link5", "🔗 Site Toda Matéria",
                           onclick = "window.open('https://www.todamateria.com.br/lancamento-obliquo/', '_blank')",
                           class = "btn btn-primary"),
              
              actionButton("link6", "🔗 Site Mundo Educação",
                           onclick = "window.open('https://mundoeducacao.uol.com.br/fisica/lancamento-obliquo.htm', '_blank')",
                           class = "btn btn-primary")
          )
        )
        
        
      } else 
      if (menu_revisao() == "video") {
      tagList(
        h2("📺 Vídeos de Revisão"),
        box(id = "box1", 
            title = tagList(span("📌 Equação do 1º e 2º Grau",
                                 style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
          
          p("Video com conceitos básicos de equações do primeiro e segundo grau:"),
          tags$iframe(width="560", height="315",
                      src="https://www.youtube.com/embed/tfm9kUrO5GI",
                      frameborder="0", allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope;
                            picture-in-picture",
                      allowfullscreen=TRUE)
        ),
        box(id = "box2", 
            title = tagList(span("📌 Conceitos de Cinemática",
                                 style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            
          p("Video com conceitos básicos de Cinemática:"),
          tags$iframe(width="560", height="315",
                      src="https://www.youtube.com/embed/AnOKxNhpRPo",
                      frameborder="0", allow="accelerometer; autoplay; clipboard-write;
                          encrypted-media; gyroscope; picture-in-picture",
                      allowfullscreen=TRUE)
        ),
        box(id = "box3", 
            title = tagList(span("📌 Lançamento Horizontal",
                                 style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            
          p("Video com conceitos básicos de Lançamento Horizontal:"),
          tags$iframe(width="560", height="315",
                      src="https://www.youtube.com/embed/JZ7xNle8-X0",
                      frameborder="0", allow="accelerometer; autoplay; clipboard-write;
                                encrypted-media; gyroscope; picture-in-picture",
                      allowfullscreen=TRUE)
        ),
        box(id = "box4", 
            title = tagList(span("📌 Lançamento Oblíquo",
                                 style = "font-size: 20px;")),
            solidHeader = TRUE, width = 12,
            
            p("Video com conceitos básicos de Lançamento Oblíquo:"),
            tags$iframe(width="560", height="315",
                        src="https://www.youtube.com/embed/G1gPxc3T0C8?si=Oefto9Y7diNrfUk8",
                        frameborder="0", allow="accelerometer; autoplay; clipboard-write;
                                encrypted-media; gyroscope; picture-in-picture",
                        allowfullscreen=TRUE)
        )
      )
    }
  })
