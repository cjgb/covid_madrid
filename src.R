library(rstan)
library(reshape2)
library(plyr)

library(ggplot2)



dat <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv")
dat <- melt(dat, id.vars = c("cod_ine", "CCAA"))
dat$fecha <- as.Date(dat$variable, format = "X%d.%m.%Y")

fecha_ini <- min(dat$fecha)

madrid <- dat[dat$CCAA == "Madrid",]
madrid$dia <- as.numeric(madrid$fecha - fecha_ini)

madrid <- madrid[, c("dia", "value")]
colnames(madrid) <- c("dia", "defs")

tmp <- data.frame(dia = -30:-1, defs = 0)

madrid <- rbind(tmp, madrid)


fit <- stan(file = "stan.stan",
            data = list(N = nrow(madrid), dia0 = which(madrid$dia == 0), dia = madrid$dia, defs = madrid$defs), 
            iter = 10000, warmup = 2000, 
            chains = 1, thin = 10)

res <- as.data.frame(fit)

contagios <- extract(fit, pars = "contagios")$contagios

contagios <- data.frame(t(contagios))
contagios$fecha <- as.Date(madrid$dia, origin = fecha_ini)

contagios <- melt(contagios, id.vars = "fecha")

casos <- ddply(contagios, .(variable), transform, casos = cumsum(value))


ggplot(casos, aes(x = fecha, y = casos, group = variable)) +
    geom_line(alpha = 0.3) + 
    xlab("fecha") + ylab("casos") +
    ggtitle("Casos de coronavirus en Madrid\n(¡Resultado de un modelo muy crudo y\n casi seguro con errores!)")
