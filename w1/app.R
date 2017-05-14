if (!require("pacman")) {install.packages("pacman"); library(pacman)}
pacman::p_load(lme4)

dat$part3 <- factor(dat$part3)
dat$nletters.s <- (dat$nletters - mean(dat$nletters)) / sd(dat$nletters)
dat$subtlex.frequency.s <- (dat$subtlex.frequency - mean(dat$subtlex.frequency)) / sd(dat$subtlex.frequency)
dat$summed.bigram.s <- (dat$summed.bigram - mean(dat$summed.bigram)) / sd(dat$summed.bigram)
dat$OLD20.s <- (dat$OLD20 - mean(dat$OLD20)) / sd(dat$OLD20)

exercise1 <- tabPanel("Exercise 1: maximixing R squared",
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

exercise2 <- tabPanel("Exercise 2: choosing predictors",
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

exercise3 <- tabPanel("Exercise 3: an interaction term",
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

exercise4 <- tabPanel("Exercise 4: transformations",
                      mainPanel(checkboxInput("logp", "Take logarithm of response time?"),
                                tags$pre(textOutput("ln.summary"))))

exercise5 <- tabPanel("Exercise 5: mixed effects",
                      mainPanel(tags$p("Choose the predictor(s) you wish to see in the ",
                                       tags$b("mixed-effect"), "model:"),
                                checkboxGroupInput("predictors4",
                                                   "Predictors to include",
                                                   c("lexicality",
                                                     "nletters" = "nletters.s",
                                                     "subtlex.frequency" = "subtlex.frequency.s",
                                                     "summed.bigram" = "summed.bigram.s",
                                                     "OLD20" = "OLD20.s",
                                                     "bigram/lexicality interaction" = "summed.bigram.s*lexicality" )),
                                tableOutput("predictors.compare"),
                                tags$pre(textOutput("model.summary4"))))

ui <- fluidPage(titlePanel("Mixed effects workshop demos"),
                navlistPanel("Example",
                             exercise1,
                             exercise2,
                             exercise3,
                             exercise4,
                             exercise5))

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
            print(summary(lm(rt ~ nletters + subtlex.frequency + summed.bigram * lexicality + OLD20 + lett.odd + part3, data = dat)))
        }
    })

    output$model.summary4 <- renderPrint({

        if (length(input$predictors4) == 0) {
            print("No predictors selected")
        } else {
            formula <- as.formula(paste0("log(rt)~",
                                         paste(input$predictors4, collapse = "+"),
                                         "+(1|participant)+(1|spelling)"))
            print(summary(eval(exp)))
        }
    })

    ## TODO: this is inefficient because it calcs the lmer twice
    output$predictors.compare <- renderTable({
        if (length(input$predictors4) == 0) {
            NULL
        } else {
            formula <- as.formula(paste0("log(rt)~",
                                         paste(input$predictors4, collapse = "+"),
                                         "+(1|participant)+(1|spelling)"))
            mixed <- lmer(formula, data = dat)
            fixed <- lm(as.formula(paste0("log(rt)~",
                                          paste(input$predictors4, collapse = "+"))),
                        data = dat)

            t <- data.frame(predictor = rownames(summary(mixed)$coefficients),
                            mixed = summary(mixed)$coefficients[,"Estimate"],
                            nonmixed = summary(fixed)$coefficients[,"Estimate"])

            t
        }

    }, digits = 6)
}

shinyApp(ui, server)
