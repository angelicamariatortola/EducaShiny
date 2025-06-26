# Aba de início
ui_home <- dashboardBody(

  shiny::div(class = "home-title", "Bem-vindo ao Aplicativo de Tipos Lançamentos!"),
  
  shiny::div(class = "home-paragraph",
      HTML("
      <p>Este aplicativo interativo foi desenvolvido para auxiliar na compreensão dos conceitos fundamentais de movimento e dos diferentes tipos de lançamentos em Física.</p>
      <p>Cada aba foi estruturada para proporcionar o melhor aproveitamento dos conteúdos:</p>
      <ul class='custom-list'>
        <li><strong>Teste Diagnóstico</strong>: avalie seus conhecimentos iniciais e identifique os principais pontos que precisam de reforço.</li>
        <li><strong>Revisão</strong>: revise os conceitos essenciais que sustentam o tema de Lançamento Horizontal, facilitando o entendimento dos tópicos mais complexos.</li>
        <li><strong>Lançamento Horizontal</strong>: explore em detalhes esse tipo de movimento, com resolução de exercícios, visualização gráfica e conferência de respostas.</li>
      </ul>
      <p>Aproveite esta oportunidade para praticar, revisar e fortalecer seus conhecimentos de forma dinâmica e envolvente!</p>
    ")
  )
)

