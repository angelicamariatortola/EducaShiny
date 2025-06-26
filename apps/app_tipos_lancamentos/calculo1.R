bb1 <- reactive({
  req(!is.null(input$sH) && !is.na(input$sH) && input$sH != 0)
  req(!is.null(input$vH) && !is.na(input$vH) && input$vH != 0)
  
  if(input$sH==""){
  } else if(input$sH=="velocidade e tempo"){
    req(!is.null(input$tempoH) && !is.na(input$tempoH) && input$tempoH != 0)
    actionButton("calH", "🔎 Conferir Cálculo")
  } else if(input$sH=="velocidade e altura"){
    req(!is.null(input$alturaH) && !is.na(input$alturaH) && input$alturaH != 0)
    actionButton("calH", "🔎 Conferir Cálculo")
  }
})

bb2 <- reactive({
  req(!is.null(input$sH) && !is.na(input$sH) && input$sH != 0)
  req(!is.null(input$vH) && !is.na(input$vH) && input$vH != 0)
  if(input$sH==""){
    
  } else if(input$sH=="velocidade e tempo"){
    req(!is.null(input$tempoH) && !is.na(input$tempoH) && input$tempoH != 0)
    actionButton("calH", "🔎 Conferir Cálculo")
  } else if(input$sH=="velocidade e alcance"){
    req(!is.null(input$alcanceH) && !is.na(input$alcanceH) && input$alcanceH != 0)
    actionButton("calH", "🔎 Conferir Cálculo")
  }
})

output$b2H <- renderUI({
  req(input$conferir1)
  if (!is.null(input$incognitaH) && input$incognitaH == ""){
    
  } else if(input$incognitaH=="alcance"){
    bb1()
  } else if(input$incognitaH=="altura"){
    bb2()
  } else {
    req(input$alcanceH != 0)
    req(input$alturaH!= 0)   
    actionButton("calH", "🔎 Conferir Cálculo")
  }
})

sH <- reactiveValues(soluçãoH=NULL)

s1H <- reactiveValues(soluH=0)

observeEvent(input$calH,{
  s1H$soluH <- s1H$soluH+1
})

observeEvent(c(input$nexH,input$incognitaH,input$tipoH,input$resp1),{
  s1H$soluH <- 0
  sH$soluçãoH<- NULL
})


observeEvent(input$calH,{
  if(s1H$soluH %% 2 == 0){
    sH$soluçãoH <- NULL
  }
  else{
    sH$soluçãoH <- isolate(
      if(input$incognitaH == "altura") {
        if(input$sH == "velocidade e tempo"){
          tagList(
            p(withMathJax(sprintf("$$y = \\frac{%g \\times %g^2 }{2}$$",
                                  -gravidade,
                                  input$tempoH
            ))),
            p(withMathJax(sprintf("$$y = \\frac{%g \\times %g}{2}$$",
                                  -gravidade,
                                  input$tempoH^2
            ))),
            p(withMathJax(sprintf("$$y = \\frac{%g}{2}$$",
                                  input$tempoH^2*(-gravidade)
            ))),
            p(withMathJax(sprintf("$$y = %g m$$",
                                  atH()
            )))
          )
        } 
        else {
          tagList(
            p(withMathJax(sprintf("$$t =\\frac{%g}{%g}$$",
                                  input$alcanceH,
                                  input$vH
            ))),
            p(withMathJax(sprintf("$$t = %g s$$",
                                  tH()
            ))),
            h1(""),
            p(withMathJax(sprintf("$$y = \\frac{%g \\times %g^2 }{2}$$",
                                  -gravidade,
                                  tH()
            ))),
            p(withMathJax(sprintf("$$y = \\frac{%g \\times %g}{2}$$",
                                  -gravidade,
                                  tH()^2
            ))),
            p(withMathJax(sprintf("$$y = \\frac{%g}{2}$$",
                                  tH()^2*(-gravidade)
            ))),
            p(withMathJax(sprintf("$$y = %g m$$",
                                  atH()
            )))
          )}
      }
      else if(input$incognitaH == "alcance") {
        if(input$sH == "velocidade e tempo"){
          tagList(
            p(withMathJax(sprintf("$$x = %g \\times %g$$",
                                  input$vH,
                                  input$tempoH
            ))),
            p(withMathJax(sprintf("$$x = %g m$$",
                                  alH()
            )))
          )
        } 
        else {
          tagList(
            p(withMathJax(sprintf("$$t = \\sqrt{\\dfrac{2 \\times %g}{%g}}$$",
                                  input$alturaH,
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t = \\sqrt{\\dfrac{%g}{%g}}$$",
                                  input$alturaH*2,
                                  -gravidade
            ))),
            p(withMathJax(sprintf("$$t = \\sqrt{%g}$$",
                                  input$alturaH*2 / -gravidade
            ))),
            p(withMathJax(sprintf("$$t = %g s$$",
                                  tH()
            ))),
            h1(""),
            p(withMathJax(sprintf("$$x = %g \\times %g $$",
                                  input$vH,
                                  tH()
            ))),
            p(withMathJax(sprintf("$$x = %g m$$",
                                  alH()
            )))
          )}
      }
      else if(input$incognitaH == "velocidade") {
        tagList(
          p(withMathJax(sprintf("$$t = \\sqrt{\\dfrac{2 \\times %g}{%g}}$$",
                                input$alturaH,
                                -gravidade
          ))),
          p(withMathJax(sprintf("$$t = \\sqrt{\\dfrac{%g}{%g}}$$",
                                input$alturaH*2,
                                -gravidade
          ))),
          p(withMathJax(sprintf("$$t = \\sqrt{%g}$$",
                                input$alturaH*2 / -gravidade
          ))),
          p(withMathJax(sprintf("$$t = %g s$$",
                                sqrt(input$alturaH*2 / -gravidade)
          ))),
          h1(""), 
          p(withMathJax(sprintf("$$v = \\dfrac{%g}{%g}$$",
                                input$alcanceH,
                                sqrt(input$alturaH*2 / -gravidade)
          ))),
          p(withMathJax(sprintf("$$v = %g m/s$$",
                                input$alcanceH / sqrt(input$alturaH*2 / -gravidade)
          )))
        )}
    )
  }
})

output$t_solH <- renderUI({
  sH$soluçãoH
})