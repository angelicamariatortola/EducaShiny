# Se problema de velocidade
bb <- reactive({
  req(!is.null(input$sv) && !is.na(input$sv) && input$sv != 0) # verificando condições para velocidade
  req(!is.null(input$theta) && !is.na(input$theta) && input$theta != 0) # condições angulo
  if(input$sv==""){
    
  } else if(input$sv=="ângulo e altura"){
    req(!is.null(input$alt) && !is.na(input$alt) && input$alt != 0) # condições altura
    actionButton("cal", "🔎 Conferir Cálculo")
  } else if(input$sv=="ângulo e alcance"){
    req(!is.null(input$s) && !is.na(input$s) && input$s != 0) # condições alcance
    actionButton("cal", "🔎 Conferir Cálculo")
  }
})

output$b2 <- renderUI({
  req(input$conferir1Ob)
  if (!is.null(input$incognita) && input$incognita == ""){
  } else if(input$incognita=="velocidade"){ # problema de velocidade
    bb()
  } else { # demais problemas
    req(input$theta != 0)
    req(input$v!= 0)
    actionButton("cal", "🔎 Conferir Cálculo")
  }
})

s <- reactiveValues(solução=NULL)

s1<- reactiveValues(solu=0)

observeEvent(input$cal,{
  s1$solu <- s1$solu+1
})

observeEvent(c(input$nex,input$incognita,input$tipo,input$resp1Ob),{
  s1$solu <- 0
  s$solução<- NULL
})


observeEvent(input$cal,{
  if(s1$solu %% 2 == 0){
    s$solução <- NULL
  }
  else{
    s$solução <- isolate(
      if(input$incognita == "altura máxima") { # se problema de altura máxima
        if(input$altura == 0){
          tagList(
            p(withMathJax(sprintf("$$H = \\frac{%g^2 \\times sen(%g°)^2}{2(%g)}$$",
                                  input$v,
                                  input$theta,
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$H = \\frac{%g \\times %g^2}{%g}$$",
                                  input$v^2,
                                  sin(angulo_rad()),
                                  -gravidade*2
            ))),
            p(withMathJax(sprintf("$$H = \\frac{%g \\times %g}{%g}$$",
                                  input$v^2,
                                  sin(angulo_rad())^2,
                                  -gravidade*2
            ))),
            p(withMathJax(sprintf("$$H = \\frac{%g}{%g}$$",
                                  input$v^2*sin(angulo_rad())^2,
                                  -gravidade*2
            ))),
            p(withMathJax(sprintf("$$H = %g m$$",
                                  altura_maxima() # resposta correta final
            )))
          )
        }
        else{
          tagList(
            p(withMathJax(sprintf("$$H = %g + \\frac{%g^2 \\times sen(%g°)^2}{2(%g)}$$",
                                  input$altura,
                                  input$v,
                                  input$theta,
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$H = %g + \\frac{%g \\times %g^2}{%g}$$",
                                  input$altura,
                                  input$v^2,
                                  sin(angulo_rad()),
                                  -gravidade*2
            ))),
            p(withMathJax(sprintf("$$H = %g + \\frac{%g \\times %g}{%g}$$",
                                  input$altura,
                                  input$v^2,
                                  sin(angulo_rad())^2,
                                  -gravidade*2
            ))),
            p(withMathJax(sprintf("$$H = %g + \\frac{%g}{%g}$$",
                                  input$altura,
                                  input$v^2*sin(angulo_rad())^2,
                                  -gravidade*2
            ))),
            p(withMathJax(sprintf("$$H = %g + %g$$",
                                  input$altura,
                                  (input$v^2*sin(angulo_rad())^2) / (-gravidade*2)
            ))),
            p(withMathJax(sprintf("$$H = %g m$$",
                                  altura_maxima()
            )))
          )
        }
      }   
      else if(input$incognita == "alcance") { # se problema de alcance
        tagList(
          p(withMathJax(sprintf("$$s = \\frac{%g^2 \\times sen(2(%g°))}{%g}$$",
                                input$v,
                                input$theta,
                                -gravidade
          ))),
          p(withMathJax(sprintf("$$s = \\frac{%g \\times sen(%g°)}{%g}$$",
                                input$v^2,
                                input$theta*2,
                                -gravidade
          ))),
          p(withMathJax(sprintf("$$s = \\frac{%g \\times %g}{%g}$$",
                                input$v^2,
                                sin(2*angulo_rad()),
                                -gravidade
          ))),
          p(withMathJax(sprintf("$$s = \\frac{%g}{%g}$$",
                                input$v^2*sin(2*angulo_rad()),
                                -gravidade
          ))),
          p(withMathJax(sprintf("$$s = %g m$$",
                                distancia_maxima() # resposta correta final
          )))
        )}
      else if(input$incognita == "tempo total") { # se problema de tempo total
        if(input$altura==0){
          tagList(
            p(withMathJax(sprintf("$$t_{T} = \\frac{2(%g) \\times sen(%g°)}{%g}$$",
                                  input$v,
                                  input$theta,
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = \\frac{%g \\times %g}{%g}$$",
                                  input$v*2,
                                  sin(angulo_rad()),
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = \\frac{%g}{%g}$$",
                                  input$v*2*sin(angulo_rad()),
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = %g s$$",
                                  Tt() # resposta correta final se altura = 0
            )))
          )
        }
        else{
          tagList(
            p(withMathJax(sprintf("$$t_{T} = \\dfrac{%g \\times sen(%g°) + \\sqrt{2%g \\times %g} }{%g}$$",
                                  input$v,
                                  input$theta,
                                  -gravidade,
                                  altura_maxima(),
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = \\dfrac{%g \\times %g + \\sqrt{%g \\times %g} }{%g}$$",
                                  input$v,
                                  sin(angulo_rad()),
                                  -gravidade*2,
                                  altura_maxima(),
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = \\dfrac{%g + \\sqrt{%g} }{%g}$$",
                                  input$v*sin(angulo_rad()),
                                  -gravidade*2*altura_maxima(),
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = \\dfrac{%g + %g}{%g}$$",
                                  input$v*sin(angulo_rad()),
                                  sqrt(-gravidade*2*altura_maxima()),
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = \\dfrac{%g}{%g}$$",
                                  input$v*sin(angulo_rad()) + sqrt(-gravidade*2*altura_maxima()),
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t_{T} = %g s$$",
                                  Tt() # resposta correta final se altura != 0
            )))
          )
        }
      }
      else if(input$incognita == "velocidade") { # se problema de velocidade
        if (input$sv == "ângulo e altura"){ # com estas variáveis
          if(input$altura==0){
            tagList(
              p(withMathJax(sprintf("$$v_{0} = \\sqrt{\\dfrac{{%g \\times 2\\times %g}}{\\sin(%g)^{2}}}$$",
                                    altura_maxima(),
                                    -gravidade,
                                    input$theta
              ))),
              p(withMathJax(sprintf("$$v_{0} =\\sqrt{\\dfrac{{%g \\times %g}}{(%g)^{2}}}$$",
                                    altura_maxima(),
                                    -gravidade * 2,
                                    sin(angulo_rad())
              ))),
              p(withMathJax(sprintf("$$v_{0} = \\sqrt{\\dfrac{{%g}}{%g}}$$",
                                    altura_maxima() * (-gravidade * 2),
                                    sin(angulo_rad())^2
              ))),
              p(withMathJax(sprintf("$$v_{0} = \\sqrt{%g}$$",
                                    (altura_maxima() * (-gravidade * 2)) / sin(angulo_rad())^2
              ))),
              p(withMathJax(sprintf("$$v_{0} = %g \\dfrac{m}{s}$$",
                                    v() # resposta correta final se altura = 0
              )))
            )
          }
          else{ 
            tagList(
              p(withMathJax(sprintf("$$v_{0} = \\dfrac{\\sqrt{(%g-%g) \\times 2\\times %g}}{\\sin(%g)^{2}}$$",
                                    altura_maxima(),
                                    altura_i(),
                                    -gravidade,
                                    input$theta
              ))),
              p(withMathJax(sprintf("$$v_{0} =\\sqrt{\\dfrac{{%g \\times %g}}{(%g)^{2}}}$$",
                                    altura_maxima()-altura_i(),
                                    -gravidade * 2,
                                    sin(angulo_rad())
              ))),
              p(withMathJax(sprintf("$$v_{0} = \\sqrt{\\dfrac{{%g}}{%g}}$$",
                                    (altura_maxima()-altura_i()) * (-gravidade * 2),
                                    sin(angulo_rad())^2
              ))),
              p(withMathJax(sprintf("$$v_{0} = \\sqrt{%g}$$",
                                    ((altura_maxima()-altura_i()) * (-gravidade * 2)) / sin(angulo_rad())^2
              ))),
              p(withMathJax(sprintf("$$v_{0} = %g \\dfrac{m}{s}$$",
                                    v() # resposta correta final se altura != 0
              )))
            )
          }
        }
        else if(input$sv=="ângulo e alcance"){ # considerando estas outras variáveis
          tagList(
            p(withMathJax(sprintf("$$v_{0}=\\sqrt{\\dfrac{%g \\times %g}{\\sin(2\\times %g°)}}$$",
                                  input$s,
                                  -gravidade,
                                  input$theta
            ))),
            p(withMathJax(sprintf("$$v_{0}=\\sqrt{\\dfrac{%g}{\\sin(%g°)}}$$",
                                  input$s * (-gravidade),
                                  input$theta * 2
            ))),
            p(withMathJax(sprintf("$$v_{0}=\\sqrt{\\dfrac{%g}{%g}}$$",
                                  input$s * (-gravidade),
                                  sin(2 * angulo_rad())
            ))),
            p(withMathJax(sprintf("$$v_{0}=\\sqrt{%g}$$",
                                  (input$s * (-gravidade)) / sin(2 * angulo_rad())
            ))),
            p(withMathJax(sprintf("$$v_{0}= %g \\dfrac{m}{s}$$",
                                  v() # resposta correta final 
            )))
          )
        }
      }
    )
  }
})

output$t_sol <- renderUI({
  s$solução
})