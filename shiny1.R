library(dplyr)
library(stringr)
rts.j <- full_join(rts, stims)
set.seed(123)
wds <- levels(rts.j$spelling)[sample(1:55865, 500)]
dat <- subset(rts.j, spelling %in% wds)
dat$lett.odd <- match(str_sub(dat$spelling, 1, 1), letters)
dat$part3 <- factor(dat$participant %% 3)
