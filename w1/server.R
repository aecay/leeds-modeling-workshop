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
