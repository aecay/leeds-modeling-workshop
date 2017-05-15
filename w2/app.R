if (!require("pacman")) {install.packages("pacman"); library(pacman)}
pacman::p_load(car, corrplot, formatR, glmnet, glmmLasso, lme4, devtools)
devtools::install_github("dmbates/RePsychLing")
devtools::install_github("AnalytixWare/ShinySky")
library(RePsychLing)
library(shinysky)

show.code <- function(x) {
    return (tags$pre("> ", textOutput(x)))
}

## XXX: support withCode(plotOutput("foo"))
withCodeOutput <- function(name, outputFn) {
    return (tags$div(
        verbatimTextOutput(paste0(name, ".code")),
        outputFn(paste0(name, ".result"))
    ))
}

glmmLasso.model <- glmmLasso(log(rt) ~
                             scale(subtlex.frequency) +
                             scale(celex.frequency) +
                             scale(celex.frequency.lemma) +
                             scale(bnc.frequency),
                             rnd = list(participant = ~1, spelling = ~1),
                             data = subset(dat, !is.na(rt)),
                             lambda=175,    # Value determined by CV, according to
                                        # glmmLasso soccer example
                             switch.NR=FALSE,
                             final.re=TRUE)

lmer.model <- lmer(log(rt) ~
                   scale(subtlex.frequency) +
                   scale(celex.frequency) +
                   scale(celex.frequency.lemma) +
                   scale(bnc.frequency) +
                   (1|participant) + (1|spelling),
                   data = dat)

lm1 <- lmer(log(rt) ~ scale(nletters) + scale(celex.frequency) + scale(summed.bigram) +
            (1 + scale(nletters) + scale(celex.frequency) + scale(summed.bigram) | participant) +
            (1 | spelling),
            data = dat, REML = FALSE)

lm2 <- lmer(log(rt) ~ scale(nletters) + scale(celex.frequency) + scale(summed.bigram) +
            (1 + scale(nletters) + scale(celex.frequency) + scale(summed.bigram) || participant) +
            (1 | spelling),
            data = dat, REML = FALSE)

lm3 <- lmer(log(rt) ~ scale(nletters) + scale(celex.frequency) + scale(summed.bigram) +
            (1 + scale(nletters) + scale(celex.frequency) || participant) +
            (1 | spelling),
            data = dat, REML = FALSE)

# XXX: support withCode(renderPlot({code}))
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

drop.ui <- tabPanel("Single variable selection",
                    mainPanel(
                        tags$h3("Simple linear model"),
                        "Here are the AIC and LRT test statistics for a linear model containing our",
                        "predictors from last week",
                        verbatimTextOutput("lm.drop"),
                        tags$h3("Mixed effects model"),
                        "Here is the same table for a mixed-effects model",
                        verbatimTextOutput("mlm.drop"),
                        "And finally the direct comparison of these two models",
                        verbatimTextOutput("anova.mlm")
                    ))

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
                       withCodeOutput("lasso", verbatimTextOutput),
                       "For comparison, here is the results of a linear model",
                       withCodeOutput("lm.model", verbatimTextOutput)))

glmmLasso.ui <- tabPanel("Mixed-model LASSO",
                         "Here are the results of a LASSO model fit according to the",
                         "lme4 syntax ",
                         tags$pre("log(rt) ~ scale(subtlex.frequency) + scale(celex.frequency) + scale(celex.frequency.lemma) + scale(bnc.frequency) + (1|participant) + (1|spelling)"),
                         verbatimTextOutput("glmmLasso"),
                         "For comparison, here is the same model fit with lme4 (no LASSO penalty)",
                         verbatimTextOutput("lmerNoLasso"))

pca.ui <- tabPanel("Random effects fitting",
                   tags$h3("Maximal model"),
                   "Here's a maximal model fit to the reaction time data from the British Lexicon Project",
                   verbatimTextOutput("lm1"),
                   "Here's the PCA",
                   verbatimTextOutput("lm1.pca"),
                   tags$h3("ZCP model"),
                   "Next we fit the zero correlation parameter model",
                   verbatimTextOutput("lm2"),
                   "Its PCA",
                   verbatimTextOutput("lm2.pca"),
                   tags$h3("Final model"),
                   "And a model that removes the subject intercept for summed.bigram",
                   verbatimTextOutput("lm3"),
                   "And its PCA",
                   verbatimTextOutput("lm3.pca"),
                   tags$h3("Coefficient comparison"),
                   "Finally, a comparison of the fixed effects from all three models",
                   tableOutput("lm.coefs"))


ui <- fluidPage(busyIndicator(),
                titlePanel("Mixed effect workshop day 2 demos"),
                navlistPanel("Case studies",
                             vif.ui,
                             drop.ui,
                             glmmLasso.ui,
                             pca.ui))

server <- function(input, output, session) {

    cp <- renderWithCode("corrplot", renderPlot,
                         corrplot.mixed(cor(dat[,c("subtlex.frequency","celex.frequency",
                                                   "celex.frequency.lemma","bnc.frequency")]),
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

    lm.model <- renderWithCode("lm.model", renderPrint,
                               summary(lm(log(rt) ~
                                          scale(subtlex.frequency) +
                                          scale(celex.frequency)+
                                          scale(celex.frequency.lemma) +
                                          scale(bnc.frequency),
                                          data = dat)))

    output$lm.model.code <- lm.model$code
    output$lm.model.result <- lm.model$result

    output$glmmLasso <- renderPrint({
        glmmLasso.model
    })

    output$lmerNoLasso <- renderPrint({
        summary(lmer.model)
    })

    output$lm1 <- renderPrint(summary(lm1))
    output$lm2 <- renderPrint(summary(lm2))
    output$lm3 <- renderPrint(summary(lm3))

    output$lm1.pca <- renderPrint(rePCA(lm1)$participant)
    output$lm2.pca <- renderPrint(rePCA(lm2)$participant)
    output$lm3.pca <- renderPrint(rePCA(lm3)$participant)

    output$lm.coefs <- renderTable({
        t <- data.frame(coefficient = names(fixef(lm1)),
                        full = fixef(lm1),
                        zcp = fixef(lm2),
                        final = fixef(lm3))
    },
    digits = 7)

    output$lm.drop <- renderPrint({
        lm.drop <- lm(log(rt) ~
                      scale(nletters) +
                      scale(celex.frequency) +
                      scale(summed.bigram) +
                      scale(OLD20) +
                      lexicality +
                      part3 +
                      lett.odd,
                      data = dat)
        drop1(lm.drop, test = "Chisq")
    })

    drop.mlm.model <- lmer(log(rt) ~
                           scale(nletters) +
                           scale(celex.frequency) +
                           scale(summed.bigram) +
                           scale(OLD20) +
                           lexicality +
                           part3 +
                           lett.odd +
                           (1 | participant) +
                           (1 | spelling),
                           data = dat)

    output$mlm.drop <- renderPrint(drop1(drop.mlm.model, test = "Chisq"))

    drop.mlm.model.reduced <- lmer(log(rt) ~
                                   scale(nletters) +
                                   scale(celex.frequency) +
                                   scale(summed.bigram) +
                                   scale(OLD20) +
                                   lexicality +
                                   lett.odd +
                                   (1 | participant) +
                                   (1 | spelling),
                                   data = dat)

    output$anova.mlm <- renderPrint(anova(drop.mlm.model, drop.mlm.model.reduced))
}


shinyApp(ui, server)
