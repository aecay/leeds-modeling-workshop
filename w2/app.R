if (!require("pacman")) {install.packages("pacman"); library(pacman)}
pacman::p_load(car, corrplot, formatR, glmnet, glmmLasso)

show.code <- function(x) {
    return (tags$pre("> ", textOutput(x)))
}

## TODO: support withCode(plotOutput("foo"))
withCodeOutput <- function(name, outputFn) {
    return (tags$div(
        verbatimTextOutput(paste0(name, ".code")),
        outputFn(paste0(name, ".result"))
    ))
}

# TODO: support withCode(renderPlot({code}))
renderWithCode <- function (name, renderFn, code) {
    ## This doesn't work -- possibly because assigning to output needs to
    ## happen in its lexical scope

    ## eval(substitute(quote(
    ##     `$<-`(outp, cell, )
    ## ), env = list(cell = paste0(".", name, "..code"))))

    ## eval(substitute(quote(
    ##     `$<-`(outp, cell, )
    ## ), env = list(cell = paste0(".", name, "..result"))))

    ##`$<-`(outp, paste0(".", name, "..result"), renderFn(code))

    cd <- tidy_source(text = as.character(deparse(substitute(code))))$text.tidy
    return(list(code = renderText(cd),
                result = renderFn(code)))
}

vif.ui <- tabPanel("Selecting among correlated variables",
                   mainPanel(
                       "It's fairly obvious that the frequency measurements in the data",
                       "(CELEX, BNC, and SUBTLEX) will be correlated.  We can verify this",
                       "using the corrplot package:",
                       withCodeOutput("corrplot", plotOutput),
                       "In order to decide which variables are correlated, we can look at the",
                       "variance inflation factors",
                       withCodeOutput("vif", verbatimTextOutput),
                       "Let's fit a LASSO model to this data:",
                       withCodeOutput("lasso", verbatimTextOutput)
                       ))

ui <- fluidPage(titlePanel("Mixed effect workshop day 2 demos"),
                navlistPanel("Case studies",
                             vif.ui))

server <- function(input, output) {

    cp <- renderWithCode("corrplot", renderPlot,
                         corrplot.mixed(cor(dat[,c("subtlex.frequency","celex.frequency","celex.frequency.lemma","bnc.frequency")]),
                                  upper = "number", lower = "circle"))

    output$corrplot.code <- cp$code
    output$corrplot.result <- cp$result

    vif <- renderWithCode("vif", renderPrint,
                          vif(lm(log(rt) ~
                                 scale(subtlex.frequency) +
                                 scale(celex.frequency)+
                                 scale(celex.frequency.lemma) +
                                 scale(bnc.frequency),
                                 data = dat)))

    output$vif.code <- vif$code
    output$vif.result <- vif$result

    lasso <- renderWithCode("lasso", renderPrint,
                            {
                                cvfit <- cv.glmnet(model.matrix(log(rt) ~
                                                                scale(subtlex.frequency) +
                                                                scale(celex.frequency)+
                                                                scale(celex.frequency.lemma) +
                                                                scale(bnc.frequency) - 1,
                                                                dat),
                                                   log(subset(dat, !is.na(rt))$rt))
                                coef(cvfit, s="lambda.min")
                            })

    output$lasso.code <- lasso$code
    output$lasso.result <- lasso$result
}


shinyApp(ui, server)
