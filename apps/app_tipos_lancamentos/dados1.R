# seleciona o exercício e extrai dados__________________________________________
dadosH <- read_xlsx("lancamento horizontal.xlsx")  

dadosH1 <- reactive({
  req(input$incognitaH)
  if (input$incognitaH == "altura") {
    filter(dadosH, pergunta == "altura")
  } else if (input$incognitaH == "alcance") {
    filter(dadosH, pergunta == "alcance")
  }else if (input$incognitaH == "velocidade") {
    filter(dadosH, pergunta == "velocidade")
  } else {
    dadosH
  }
})

output$SegH <- renderUI({
  if(input$incognitaH=="alcance"){
    tagList(
      tags$div(
        style = "display: flex; align-items: center;", 
        tags$label("Quais variáveis usará no cálculo?", 
        style = "margin-right: 10px; color: #003366;
      "),
        selectInput("sH", NULL, choices = svH0, selected = "")  
      ),
      textOutput("asegH")
    )}
  else if(input$incognitaH=="altura"){
    tagList(
      tags$div(
        style = "display: flex; align-items: center;", 
        tags$label("Quais variáveis usará no cálculo?", 
                   style = "margin-right: 10px; color: #003366;"),
        selectInput("sH", NULL, choices = svH1, selected = "")  
      ),
      textOutput("asegH")
    )}
})

output$asegH<- renderText(fH())

fH <- reactive({
  if(input$tipoH=="fechado"){
    if(input$incognitaH=="alcance"){
      if(input$sH=="velocidade e tempo"){
        if(tempoH()==0){
          validate("O enunciado disponibiliza a velocidade e altura")
        }
      } else if (input$sH=="velocidade e altura"){
        if(alturaH()==0){
          validate("O enunciado disponibiliza a velocidade e tempo")
        }
      }
    } else if(input$incognitaH=="altura"){
      if(input$sH=="velocidade e tempo"){
        if(tempoH()==0){
          validate("O enunciado disponibiliza a velocidade e alcance")
        }
      } else if (input$sH=="velocidade e alcance"){
        if(alcanceH()==0){
          validate("O enunciado disponibiliza a velocidade e tempo")
        }
      }
    }  
  }
})

num_exH <- reactive({
  nrow(dadosH1())
})

output$exH <- renderUI({
  req(input$tipoH=="fechado")
  tags$div(
    style = "display: flex; align-items: center;", 
    tags$label("Exercício:", style = "margin-right: 10px; color: #003366;"),
    selectInput("nexH", NULL, choices = seq_len(num_exH())))
})

enunciadoH <- reactive(dadosH1()[input$nexH,"enunciado"])  
perguntaH <- reactive(dadosH()[input$nexH,"pergunta"])
velocidadeH <- reactive(as.numeric(dadosH1()[input$nexH,"velocidade"]))
alcanceH <- reactive(as.numeric(dadosH1()[input$nexH,"alcance"]))
alturaH <- reactive(as.numeric(dadosH1()[input$nexH,"altura"]))
tempoH <- reactive(as.numeric(dadosH1()[input$nexH,"tempo"]))

output$enunciadoH <- renderUI({
  if(input$tipoH=="aberto"){
    
  }
  else if(input$tipoH=="fechado"){
    p(enunciadoH(),style = "text-align: justify; font-size: 20px;")
  }
})