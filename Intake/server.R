source("helpers.R")

shinyServer(function(input, output) {

    output$graph1 <- renderPlot({
        print(input$var2)
        tbl <- t(prop.table(table(intake[which(intake[[input$var1]] == input$level),input$var2],intake[which(intake[[input$var1]] == input$level),"result"]),1)*100)
        bp <- barplot(tbl,col=c("#66c2a5","#fc8d62"))
        text(bp, 100-(.5*tbl[2,]), labels=round(tbl[2,],1))
    })
    output$levels <- renderUI({
        levels <- levels(intake[[input$var1]])
        selectizeInput("level",
                       label="Choose a Value",
                       choices=levels,
                       selected=NULL)
    })
    output$vars <- renderUI({
        vars <- names[!names %in% c("result", input$var1)]
        selectizeInput("var2",
                       label="Choose a Variable",
                       choices=vars,
                       selected=NULL)
    })
})



