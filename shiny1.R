library(dplyr)
library(stringr)
rts.j <- full_join(rts, stims)
set.seed(123)
wds <- levels(rts.j$spelling)[sample(1:55865, 500)]
dat <- subset(rts.j, spelling %in% wds)
dat$lett.odd <- match(str_sub(dat$spelling, 1, 1), letters)
dat$part3 <- factor(dat$participant %% 3)

library(shiny)

exercise1 <- tabPanel("Exercise 1",
                     mainPanel(tags$p("The goal of this exercise is to build a model that maximizes",
                                      "the R", tags$sup("2"), ".  You are given a dataset of response times",
                                      "on a lexical decision task, and a number of predictors that might",
                                      "be relevant to the phenomenon.  Try selecting predictors that give",
                                      "the best R", tags$sup("2"), "."),
                               checkboxGroupInput("predictors",
                                                  "Predictors to include",
                                                  c("lexicality",
                                                    "nletters",
                                                    "subtlex.frequency",
                                                    "summed.bigram",
                                                    "OLD20",
                                                    "lett.odd",
                                                    "part3")),
                               tags$p("The R", tags$sup("2"), "for this model is: ",
                                      tags$b(textOutput("r2", inline = T)))))

exercise2 <- tabPanel("Exercise 2",
                      mainPanel(tags$p("Choose the predictor(s) you wish to see in the model:"),
                                checkboxGroupInput("predictors2",
                                                   "Predictors to include",
                                                   c("lexicality",
                                                     "nletters",
                                                     "subtlex.frequency",
                                                     "summed.bigram",
                                                     "OLD20",
                                                     "lett.odd",
                                                     "part3")),
                                tags$pre(textOutput("model.summary"))))

exercise3 <- tabPanel("Exercise 3",
                      mainPanel(tags$p("Choose the predictor(s) you wish to see in the model:"),
                                checkboxGroupInput("predictors3",
                                                   "Predictors to include",
                                                   c("lexicality",
                                                     "nletters",
                                                     "subtlex.frequency",
                                                     "summed.bigram",
                                                     "OLD20",
                                                     "lett.odd",
                                                     "part3",
                                                     "bigram/lexicality interaction" = "summed.bigram*lexicality" )),
                                tags$pre(textOutput("model.summary3"))))

exercise4 <- tabPanel("Exercise 4",
                      mainPanel(checkboxInput("logp", "Take logarithm of response time?"),
                                tags$pre(textOutput("ln.summary"))))

ui <- fluidPage(titlePanel("Mixed effects workshop demos"),
                navlistPanel("Example",
                             exercise1,
                             exercise2,
                             exercise3,
                             exercise4))

server <- function(input, output) {
    output$r2 <- renderText({
        if (length(input$predictors) == 0) {
            "No predictors selected"
        } else {
            summary(lm(as.formula(paste0("rt~",
                                         paste(input$predictors, collapse = "+"))),
                       data = dat))$r.squared
        }
    })

    output$model.summary <- renderPrint({
        if (length(input$predictors2) == 0) {
            print("No predictors selected")
        } else {
            formula <- as.formula(paste0("rt~",
                                         paste(input$predictors2, collapse = "+")))
            exp <- substitute(lm(formula, data = dat),
                              list(formula = formula))
            print(summary(eval(exp)))
        }
    })

    output$model.summary3 <- renderPrint({
        if (length(input$predictors3) == 0) {
            print("No predictors selected")
        } else {
            formula <- as.formula(paste0("rt~",
                                         paste(input$predictors3, collapse = "+")))
            exp <- substitute(lm(formula, data = dat),
                              list(formula = formula))
            print(summary(eval(exp)))
        }
    })

    output$ln.summary <- renderPrint({
        if (input$logp) {
            print(summary(lm(log(rt) ~ nletters + subtlex.frequency + summed.bigram * lexicality + OLD20 + lett.odd + part3, data = dat)))
        } else {
            print(summary(lm(rt ~ nletters * subtlex.frequency + summed.bigram * lexicality + OLD20 + lett.odd + part3, data = dat)))
        }
    })
}

runApp(shinyApp(ui, server))
