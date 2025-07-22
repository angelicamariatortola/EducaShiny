output$b <- renderUI({

  if(input$incognita==""){
    
  }
  else{
    actionButton("eq", "📚 Equação")
  }
})

e<- reactiveValues(equação=NULL)

e1<- reactiveValues(equ=0)

observeEvent(input$eq,{
  e1$equ <- e1$equ+1
})

observeEvent(c(input$nex,input$incognita,input$tipo),{
  e1$equ <- 0
  e$equação<- NULL
})

observeEvent(input$eq,{
  if(e1$equ %% 2 == 0){
    e$equação<- NULL
  }
  else{
    e$equação <- isolate(
    if(input$incognita == "altura máxima") {
      if(input$altura == 0){
        tagList(
          p(withMathJax("\\(H = \\dfrac{v_{0}^2 \\times sen(\\theta)^2}{2g}\\)"), align="center"),
          p("H - altura máxima"),
          p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
          p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento"),
          p("g - gravidade")
        )
      }
      else{
        tagList(
          p(withMathJax("\\(H =h+ \\dfrac{v_{0}^2 \\times sen(\\theta)^2}{2g}\\)"), align="center"),
          p("h - altura inicial"),
          p("H - altura máxima"),
          p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
          p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento"),
          p("g - gravidade")
        )
      }
    }   
    else if(input$incognita == "alcance") {
      tagList(
        p(withMathJax("\\(s = \\dfrac{v_{0}^2 \\times sen(2\\theta)}{g}\\)"), align="center"),
        p("s - alcance"),
        p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
        p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento"),
        p("g - gravidade")
      )}
    else if(input$incognita == "tempo de subida") {
      tagList(
        p(withMathJax("\\(t_{s} = \\dfrac{v_{0} \\times sen(\\theta)}{g}\\)"), align="center"),
        p(withMathJax("\\(t_{s}\\)")," - tempo de subida"),
        p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
        p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento"),
        p("g - gravidade")
      )}
    else if(input$incognita == "tempo total") {
      if(input$altura==0){
        tagList(
          p(withMathJax("\\(t_{T} = \\dfrac{2v_{0} \\times sen(\\theta)}{g}\\)"), align="center"),
          p(withMathJax("\\(t_{T}\\)")," - tempo total"),
          p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
          p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento"),
          p("g - gravidade")
        )
      }
      else{
        tagList(
          p(withMathJax("\\(t_{T} = \\dfrac{v_{0} \\times sen(\\theta) + \\sqrt{2g \\times H} }{g}\\)"), align="center"),
          p(withMathJax("\\(t_{T}\\)")," - tempo total"),
          p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
          p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento"),
          p("g - gravidade"),
          p("H - altura máxima")
        )
      }
    }
    else if(input$incognita == "velocidade") {
      if (input$sv == "ângulo e altura"){
        if(input$altura==0){
          tagList(
            p(withMathJax("\\(v_{0}=\\sqrt{\\dfrac{{H \\times 2g}}{sen(\\theta)^{2}}}\\)"), align="center"),
            p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
            p("H - altura máxima"),
            p("g - gravidade"),
            p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento")
          )
        }
        else{
          tagList(
            p(withMathJax("\\(v_{0}=\\sqrt{\\dfrac{{(H-h) \\times 2g}}{sen(\\theta)^{2}}}\\)"), align="center"),
            p(withMathJax("\\(v_{0}\\)")," - velocidade inicial"),
            p("H - altura máxima"),
            p("h - altura inicial"),
            p("g - gravidade"),
            p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento")
          )
        }
      }
      else if(input$sv=="ângulo e alcance"){
        tagList(
          p(withMathJax("\\(v_{0}=\\sqrt{\\dfrac{s \\times g}{sen(2\\theta)}}\\)"), align="center"),
          p(withMathJax("\\(t_{T}\\)")," - tempo total"),
          p("s - alcance"),
          p(withMathJax("\\(\\theta\\)")," - ângulo do lançamento"),
          p("g - gravidade")
        )
      }
    }
  )}
})



output$t_eq <- renderUI({
  e$equação
})  
