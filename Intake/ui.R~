plays <- readRDS("./data/plays.2017.18.rds")
players <- sort(plays$fullName)

bootstrapPage(
    fluidRow(
        column(12, align="center",
               titlePanel("Player Play Type Comparison"))
    ),
    fluidRow(
        column(12, align="center",
               selectizeInput("p1",
                              label="Choose a player",
                              choices = players,
                              selected = "Kristaps Porzingis")),
        column(12, align="center",
               uiOutput("player"))
    ),
    fluidRow(
        column(12, align="center",
               plotOutput("graph1",width="85%"))
    ),
    fluidRow(
        column(12, align="center",
               plotOutput("graph2",width="85%"))
    )
)
