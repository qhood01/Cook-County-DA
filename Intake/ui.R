source("helpers.R")

bootstrapPage(
    fluidRow(
        column(12, align="center",
               titlePanel("Cook County DA Intake Approval Rates"))
    ),
    fluidRow(
        column(4, align="center",
               selectizeInput("var1",
                              label="Choose a Variable",
                              choices = names[-1],
                              selected = NULL)),
        column(4, align="center",
               uiOutput("levels")),
        column(4, align="center",
               uiOutput("vars"))
    ),
    fluidRow(
        column(12, align="center",
               plotOutput("graph1",width="85%"))
    )
)
