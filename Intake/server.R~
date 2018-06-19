source("helpers.R")

shinyServer(function(input, output) {

    output$graph1 <- renderPlot({

        compare_plays(input$p1,input$p2)
    })
    output$graph2 <- renderPlot({
        compare_stats(input$p1,input$p2)
    })
    output$player <- renderUI({
        players <- playerComps[[input$p1]][["Name"]]
        selectizeInput("p2",
                       label="Choose a comparison player",
                       choices=players,
                       selected=NULL)
    })
})



