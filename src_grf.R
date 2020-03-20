library(rstan)
library(reshape2)
library(plyr)
library(ggplot2)

pop_madrid <- 6.6e6

# defunciones
defs <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_fallecidos.csv")
defs <- melt(defs, id.vars = c("cod_ine", "CCAA"))
defs$fecha <- as.Date(defs$variable, format = "X%d.%m.%Y")
defs <- defs[, c("CCAA", "fecha", "value")]
colnames(defs) <- c("CCAA", "fecha", "defs")
defs <- defs[defs$CCAA == "Madrid",]

casos <- read.csv("https://raw.githubusercontent.com/datadista/datasets/master/COVID%2019/ccaa_covid19_casos.csv")
casos <- melt(casos, id.vars = c("cod_ine", "CCAA"))
casos$fecha <- as.Date(casos$variable, format = "X%d.%m.%Y")
casos <- casos[, c("CCAA", "fecha", "value")]
colnames(casos) <- c("CCAA", "fecha", "casos")
casos <- casos[casos$CCAA == "Madrid",]

fecha_ini <- min(defs$fecha)

madrid <- merge(defs, casos, all.y = T)
madrid$defs[is.na(madrid$defs)] <- 0

madrid$dia <- madrid$fecha - fecha_ini

tmp <- data.frame(dia = -30:-1, defs = 0, casos = 0)

madrid <- rbind(tmp, madrid[, c("dia", "defs", "casos")])
madrid <- data.frame(dia = madrid$dia[-1], defs = diff(madrid$defs), casos = diff(madrid$casos))


fit <- stan(file = "stan_grf.stan",
            data = list(N    = nrow(madrid), 
                        dia0 = which(madrid$dia == 0), 
                        pop  = pop_madrid,                           
                        dia  = madrid$dia,
                        defs = madrid$defs,
                        casos_obs = madrid$casos), 
            iter = 10000, warmup = 2000, 
            chains = 4, thin = 10, cores = 4,
            include = FALSE, pars = "contagios")

res <- as.data.frame(fit)

# contagios <- extract(fit, pars = "contagios")$contagios
# 
# contagios <- data.frame(t(contagios))
# contagios$fecha <- as.Date(madrid$dia, origin = fecha_ini)

est_casos <- extract(fit, pars = "casos")$casos
est_casos <- data.frame(t(est_casos))
est_casos$fecha <- as.Date(madrid$dia, origin = fecha_ini)
est_casos <- melt(est_casos, id.vars = "fecha")
colnames(est_casos) <- c("fecha", "variable", "casos")

tmp <- casos
tmp$variable <- "a"

ggplot(est_casos, aes(x = fecha, y = casos, group = variable)) +
    geom_line(alpha = 0.01) + 
    geom_line(data = tmp, aes(x = fecha, y = casos), col = "red") + 
    xlab("fecha") + ylab("casos") +
    ggtitle("Casos de coronavirus en Madrid\n(¡Resultado de un modelo muy crudo y\n casi seguro con errores!)")

ggsave("Rplot.png")

tmp <- est_casos[est_casos$fecha == max(est_casos$fecha),]
tmp$casos <- tmp$casos / 1000
ggplot(tmp, aes(x = casos)) + 
    geom_histogram(fill = "steelblue", bins = 50) + 
    xlab("casos (miles)") +
    ylab("número de simulaciones") +
    ggtitle(paste0("casos estimados en ", Sys.Date()))

ggsave("Rplot01.png")

r0 <- extract(fit, pars = "r0")$r0
r0 <- data.frame(t(r0))
r0$fecha <- as.Date(madrid$dia, origin = fecha_ini)
r0 <- melt(r0, id.vars = "fecha")
colnames(r0) <- c("fecha", "variable", "r0")

ggplot(r0, aes(x = fecha, y = r0, group = variable)) +
    geom_line(alpha = 0.01) + 
    xlab("fecha") + ylab("r0") +
    ggtitle("Casos de coronavirus en Madrid\n(¡Resultado de un modelo muy crudo y\n casi seguro con errores!)")


# just a check

# ggplot(tmp, aes(x = log10(casos))) + 
#     geom_histogram(fill = "steelblue") + 
#     xlab("casos (miles)") +
#     ylab("número de simulaciones") +
#     ggtitle(paste0("casos estimados en ", Sys.Date()))

# hist(tmp$casos, breaks = 30, main = "Casos 'hoy'", col = "steelblue", xlab = "casos", freq = FALSE)



tmp <- res[, c("casos_0", "r0", "letalidad")]
plot(tmp)


traceplot(fit, pars = c("casos_0", "letalidad", "sigma_delta_r0"))

# evaluación del ajuste
pairs(fit, pars = c("casos", "contagios", "r0"), include = FALSE)
