#########Chapter2 - Analises Bananaquit playback experiment-------
#########15-10-2018
#########gabrielle.winandy@gmail.com

#### 1. Fazer um LMM com cantos------
####Fazer um LMM (linear mixed model) para avaliar as mudancas no canto de coereba em relacao aos diferentes tipos de playback (elaborate vs simple)

#### 1.1. Ler tabela de dados do canto "Dados.Canto.completo"-----
setwd("C:/Users/Gabrielle/OneDrive/Doutorado/Cap 2_bananaquit experiments/Dados")
dir()
dados.cantos.pbck = read.csv2(file="Dados.cantos.completo.csv", header=TRUE, sep=";", dec=".")
head(dados.cantos.pbck)
summary(dados.cantos.pbck)
str(dados.cantos.pbck)

###1.2. Checking what probability distribution best fits my data-----
require(car)
require(MASS)
dados.cantos.pbck$N.sil.diff.t <- dados.cantos.pbck$N.sil.diff + 1
qqp(dados.cantos.pbck$N.sil.diff.t, "norm")
# lnorm means lognormal
qqp(dados.cantos.pbck$N.sil.diff.t, "lnorm")

# qqp requires estimates of the parameters of the negative binomial, Poisson
# and gamma distributions. You can generate estimates using the fitdistr
# function. Save the output and extract the estimates of each parameter as I
# have shown below.
nbinom <- fitdistr(dados.cantos.pbck$N.sil.diff.t, "weibull")
qqp(dados.cantos.pbck$N.sil.diff.t, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

poisson <- fitdistr(dados.cantos.pbck$N.sil.diff.t, "Poisson")
qqp(dados.cantos.pbck$N.sil.diff.t, "pois", poisson$estimate)

gamma <- fitdistr(dados.cantos.pbck$N.sil.diff.t, "gamma")
qqp(dados.cantos.pbck$N.sil.diff.t, "gamma", shape = gamma$estimate[[1]], rate = gamma$estimate[[2]])

###1.3. Runing the linear mixed model----
install.packages("lmerTest")
install.packages("multcomp")
library(lmerTest)
library(lme4)
library (nlme)
library(readxl)
library(MuMIn)
require(multcomp)

#N?mero de s?labas diferentes
lmm.n.sil.diff <- lmer(N.sil.diff ~ Tratamento + Order + (1 | ID), 
                       data = dados.cantos.pbck, REML = FALSE)

summary(lmm.n.sil.diff)
lmm.n.sil.diff
Anova(lmm.n.sil.diff)

lmm.n.sil.diff.noorder.pois <- glmer(N.sil.diff ~ Tratamento + (1 | ID),
                                     data = dados.cantos.pbck,
                                     family = "poisson",na.action = "na.fail")

lmm.n.sil.diff.notreat <- glmer(N.sil.diff ~ Order + (1 | ID),
                                data = dados.cantos.pbck,
                                family = "poisson", na.action = "na.fail")

lmm.n.sil.diff.nofixed <- glmer(N.sil.diff ~ 1 + (1 | ID),
                                data = dados.cantos.pbck,
                                family = "poisson", na.action = "na.fail")

lmm.n.sil.diff.global <- glmer(N.sil.diff ~ Tratamento + Order + (1 | ID),
                               data = dados.cantos.pbck,
                               family = "poisson",na.action = "na.fail")

d1 <- dredge(lmm.n.sil.diff.global)
model.best <- get.models(d1, subset = 1)[[1]]
r.squaredGLMM(model.best)
out.put=model.sel(dredge(lmm.n.sil.diff.global))
out.put 
r.squaredGLMM(lmm.n.sil.diff.global)
r.squaredGLMM(lmm.n.sil.diff.nofixed)
r.squaredGLMM(lmm.n.sil.diff.noorder.pois)
r.squaredGLMM(lmm.n.sil.diff.notreat)

out.put<-model.sel(lmm.n.sil.diff, lmm.n.sil.diff.notreat, lmm.n.sil.diff.nofixed, lmm.n.sil.diff.noorder)
out.put
anova(lmm.n.sil.diff.noorder, lmm.n.sil.diff, lmm.n.sil.diff.notreat, lmm.n.sil.diff.nofixed)

# Check residual normality
plot(lmm.n.sil.diff.noorder)
lmm.n.sil.diff.noorder.pois <- glmer(N.sil.diff ~ Tratamento + (1 | ID),
                                     data = dados.cantos.pbck,
                                     family = "poisson")
plot(lmm.n.sil.diff.noorder.pois)
summary(lmm.n.sil.diff.noorder.pois)

require(MuMIn)

model.global <- glmer(N.Sil ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck,
                      family = "poisson", na.action = "na.fail")
summary(model.global)

d1 <- dredge(model.global)
model.best <- get.models(d1, subset = 1)[[1]]
r.squaredGLMM(model.best)


#writing table with the results
write.csv(out.put,"Model selection table.csv", row.names = F) 
r.squared.m.noorder<-r.squaredGLMM(lmm.n.sil.diff.noorder)
r.squared.m.noorder
write.csv(r.squared.m.noorder,"R.squared.m.noorder.csv", row.names = F)

#N?mero total de s?labas

model.global <- glmer(N.Sil ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck,
                      family = "poisson", na.action = "na.fail")

summary(model.global)

d1 <- dredge(model.global)
model.best.2 <- get.models(d1, subset = 1)[[1]]
r.squaredGLMM(model.best.2)
summary(model.best.2)
out.put2=model.sel(dredge(model.global))
out.put2 
plot(lmm.n.sil.noorder)

lmm.n.sil.global <- glmer(N.Sil ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck,
                          family = "poisson", na.action = "na.fail")

lmm.n.sil.noorder <- glmer(N.Sil ~ Tratamento + (1 | ID), data = dados.cantos.pbck,
                           family = "poisson", na.action = "na.fail")
summary(model.global)

lmm.n.sil.notreat <- glmer(N.Sil ~ Order + (1 | ID), data = dados.cantos.pbck,
                           family = "poisson", na.action = "na.fail")
summary(model.global)

lmm.n.sil.nofixed <- glmer(N.Sil ~ 1 + (1 | ID), data = dados.cantos.pbck,
                           family = "poisson", na.action = "na.fail")
summary(model.global)
r.squaredGLMM(lmm.n.sil.global)
r.squaredGLMM(lmm.n.sil.noorder)
r.squaredGLMM(lmm.n.sil.nofixed)
r.squaredGLMM(lmm.n.sil.notreat)

out.put<-model.sel(lmm.n.sil, lmm.n.sil.notreat, lmm.n.sil.nofixed, lmm.n.sil.noorder)
out.put

anova(lmm.n.sil.noorder, lmm.n.sil, lmm.n.sil.notreat, lmm.n.sil.nofixed)

#Frequ?ncia m?nima
model.global.3 <- lmer(Low.Freq.Hz ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck,
                       na.action = "na.fail", REML=FALSE)

summary(model.global.3)

d3 <- dredge(model.global.3)
model.best.3 <- get.models(d3, subset = 1)[[1]]
r.squaredGLMM(model.best.3)
summary(model.best.3)
model.best.3
out.put3=model.sel(dredge(model.global.3))
out.put3          

lmm.low.freq <- lmer(Low.Freq.Hz ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck,
                     na.action = "na.fail", REML=FALSE)
plot(lmm.low.freq)
lm.low.freq.stand=rstandard(lmm.low.)

lmm.low.freq.noorder <- lmer(Low.Freq.Hz ~ Tratamento + (1 | ID), data = dados.cantos.pbck, na.action = "na.fail",
                             REML = FALSE)

lmm.low.freq.notreat <- lmer(Low.Freq.Hz ~ Order + (1 | ID), data = dados.cantos.pbck, na.action = "na.fail",
                             REML = FALSE)

lmm.low.freq.nofixed <- lmer(Low.Freq.Hz ~ 1 + (1 | ID), data = dados.cantos.pbck, na.action = "na.fail",
                             REML = FALSE)

r.squaredGLMM(lmm.low.freq)
r.squaredGLMM(lmm.low.freq.nofixed)
r.squaredGLMM(lmm.low.freq.noorder)
r.squaredGLMM(lmm.low.freq.notreat)

out.put<-model.sel(lmm.low.freq, lmm.low.freq.notreat, lmm.low.freq.nofixed, lmm.low.freq.noorder)
out.put
qqplot()

anova(lmm.low.freq, lmm.low.freq.notreat, lmm.low.freq.nofixed, lmm.low.freq.noorder)

#Peak frequency
model.global.4 <- lmer(Peak.Freq.Hz ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck,
                       na.action = "na.fail", REML=FALSE)

summary(model.global.4)

d4 <- dredge(model.global.4)
model.best.4 <- get.models(d4, subset = 1)[[1]]
r.squaredGLMM(model.best.4)
summary(model.best.4)
model.best.4
out.put4=model.sel(dredge(model.global.4))
out.put4  

lmm.peak.freq <- lmer(Peak.Freq.Hz ~ Tratamento + Pbck.1.2 + (1 | ID), data = dados.cantos.pbck,
                      REML = FALSE)

lmm.peak.freq.noorder <- lmer(Peak.Freq.Hz ~ Tratamento + (1 | ID), data = dados.cantos.pbck,
                              REML = FALSE)

lmm.peak.freq.notreat <- lmer(Peak.Freq.Hz ~ Pbck.1.2 + (1 | ID), data = dados.cantos.pbck,
                              REML = FALSE)

lmm.peak.freq.nofixed <- lmer(Peak.Freq.Hz ~ 1 + (1 | ID), data = dados.cantos.pbck,
                              REML = FALSE)


out.put<-model.sel(lmm.peak.freq, lmm.peak.freq.notreat, lmm.peak.freq.nofixed, lmm.peak.freq.noorder)
out.put

anova(lmm.peak.freq, lmm.peak.freq.notreat, lmm.peak.freq.nofixed, lmm.peak.freq.noorder)

#Frequency Bandwidth
model.global.5 <- lmer(Delta.Freq.Hz ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck,
                       na.action = "na.fail", REML=FALSE)

summary(model.global.4)

d5 <- dredge(model.global.5)
model.best.5 <- get.models(d5, subset = 1)[[1]]
r.squaredGLMM(model.best.5)
summary(model.best.5)
model.best.5
out.put5=model.sel(dredge(model.global.5))
out.put5

lmm.delta.freq <- lmer(Delta.Freq.Hz ~ Tratamento + Order + (1 | ID), data = dados.cantos.pbck, na.action = "na.fail",
                       REML = FALSE)

lmm.delta.freq.noorder <- lmer(Delta.Freq.Hz ~ Tratamento + (1 | ID), data = dados.cantos.pbck, na.action = "na.fail",
                               REML = FALSE)

lmm.delta.freq.notreat <- lmer(Delta.Freq.Hz ~ Order + (1 | ID), data = dados.cantos.pbck, na.action = "na.fail",
                               REML = FALSE)

lmm.delta.freq.nofixed <- lmer(Delta.Freq.Hz ~ 1 + (1 | ID), data = dados.cantos.pbck, na.action = "na.fail",
                               REML = FALSE)
r.squaredGLMM(lmm.delta.freq)
r.squaredGLMM(lmm.delta.freq.notreat)
r.squaredGLMM(lmm.delta.freq.noorder)
r.squaredGLMM(lmm.delta.freq.nofixed)

out.put<-model.sel(lmm.delta.freq, lmm.delta.freq.notreat, lmm.delta.freq.nofixed, lmm.delta.freq.noorder)
out.put

anova(lmm.delta.freq, lmm.delta.freq.notreat, lmm.delta.freq.nofixed, lmm.delta.freq.noorder)

#Maximum frequency
model.global.6 <- lmer(High.Freq.Hz ~ Tratamento + Order + (1 | ID), 
                       data = dados.cantos.pbck,
                       na.action = "na.fail", REML=FALSE)

summary(model.global.6)

d6 <- dredge(model.global.6)
model.best.6 <- get.models(d6, subset = 1)[[1]]
r.squaredGLMM(model.best.6)
summary(model.best.6)
model.best.6
out.put6=model.sel(dredge(model.global.6))
out.put6

lmm.high.freq <- lmer(High.Freq.Hz ~ Tratamento + Order + (1 | ID), 
                      data = dados.cantos.pbck,
                      na.action = "na.fail", REML=FALSE)

lmm.high.freq.noorder <- lmer(High.Freq.Hz ~ Tratamento + (1 | ID), 
                              data = dados.cantos.pbck,
                              na.action = "na.fail", REML=FALSE)

lmm.high.freq.notreat <- lmer(High.Freq.Hz ~ Order + (1 | ID), 
                              data = dados.cantos.pbck,
                              na.action = "na.fail", REML=FALSE)

lmm.high.freq.nofixed <- lmer(High.Freq.Hz ~ 1 + (1 | ID), 
                              data = dados.cantos.pbck,
                              na.action = "na.fail", REML=FALSE)

r.squaredGLMM(lmm.high.freq)
r.squaredGLMM(lmm.high.freq.noorder)
r.squaredGLMM(lmm.high.freq.nofixed)
r.squaredGLMM(lmm.high.freq.notreat)

out.put<-model.sel(lmm.high.freq, lmm.high.freq.notreat, lmm.high.freq.nofixed, lmm.high.freq.noorder)
out.put

anova(lmm.high.freq, lmm.high.freq.notreat, lmm.high.freq.nofixed, lmm.high.freq.noorder)


###2.Runing the post hoc test----
require(multcomp)
#N?mero de s?labas diferentes, best model
post.hoc.test.1=glht(lmm.n.sil.diff.noorder, linfct=mcp(Tratamento="Tukey"))
summary(post.hoc.test.1)

#n?mero de s?labas, best model
post.hoc.test.2=glht(model.best.2, linfct=mcp(Tratamento="Tukey"))

summary(post.hoc.test.2)

#frequencia minima, best model
post.hoc.test.3=glht(model.best.3, linfct=mcp(Tratamento="Tukey"))
summary(post.hoc.test.3)

#frequencia de pico, best model
post.hoc.test.4=glht(model.best.4, linfct=mcp(Tratamento="Tukey"))
summary(post.hoc.test)

#frequency bandwidth, best model
post.hoc.test.5=glht(model.best.5, linfct=mcp(Tratamento="Tukey"))
summary(post.hoc.test.5)

#maximum frequency, best model
post.hoc.test.6=glht(model.best.6, linfct=mcp(Tratamento="Tukey"))
summary(post.hoc.test.6)

###3. Runing boxplot to song data-----
# Convert the variable number of syllable types from an interval to a numeric variable
dados.cantos.pbck$N.sil.diff <- as.numeric(dados.cantos.pbck$N.sil.diff)

# Convert the variable playback type from a numeric variable to a factor variable
dados.cantos.pbck$Tratamento <- as.factor(dados.cantos.pbck$Tratamento)

#Convert the variable number of ID from a numeric to a factor variable
dados.cantos.pbck$ID <- as.factor(dados.cantos.pbck$ID)

# Basic box plot
install.packages("ggplot2")
library(ggplot2)

#N?mero de s?labas diferentes
p <- ggplot(dados.cantos.pbck, aes(x=dados.cantos.pbck$Tratamento, y=dados.cantos.pbck$N.sil.diff)) +
  geom_boxplot(fill="gray") +
  theme_classic() +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Number of syllable types per song")
p

#N?mero de s?labas
p <- ggplot(dados.cantos.pbck, aes(x=dados.cantos.pbck$Tratamento, y=dados.cantos.pbck$N.Sil)) +
  geom_boxplot(fill="gray") +
  theme_classic() +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Number of syllables per song")
p

#Freq m?nima
p <- ggplot(dados.cantos.pbck, aes(x=dados.cantos.pbck$Tratamento, y=dados.cantos.pbck$Low.Freq.Hz)) +
  geom_boxplot(fill="gray") +
  theme_classic() +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Low Frequency (Hz)")
p

#Peak frequency
p <- ggplot(dados.cantos.pbck, aes(x=dados.cantos.pbck$Tratamento, y=dados.cantos.pbck$Peak.Freq.Hz)) +
  geom_boxplot(fill="gray") +
  theme_classic() +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Peak Frequency (Hz)")
p

#Maximum frequency
p <- ggplot(dados.cantos.pbck, aes(x=dados.cantos.pbck$Tratamento, y=dados.cantos.pbck$High.Freq.Hz)) +
  geom_boxplot(fill="gray") +
  theme_classic() +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="High Frequency (Hz)")
p

#Frequency bandwidth
p <- ggplot(dados.cantos.pbck, aes(x=dados.cantos.pbck$Tratamento, y=dados.cantos.pbck$Delta.Freq.Hz)) +
  geom_boxplot(fill="gray") +
  theme_classic() +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Frequency Bandwidth (Hz)")
p

#Delta time
p <- ggplot(dados.cantos.pbck, aes(x=dados.cantos.pbck$Tratamento, y=dados.cantos.pbck$Delta.Time.s)) +
  geom_boxplot(fill="gray") +
  theme_classic() +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Song duration (sec)")
p

# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))


#3.2.Running line graphs for song variables----
#Basic line graph with points
setwd("C:/Users/Gabrielle/OneDrive/Doutorado/Cap 2_bananaquit experiments/Dados")
require(ggplot2)
agg.dados.cantos.comp = read.csv2(file="agg.dados.canto.comp.csv", header=TRUE, sep=";", dec=".")
summary(agg.dados.cantos.comp)
str(agg.dados.cantos.comp)

agg.dados.cantos.order1 = read.csv2(file="agg.dados.canto.order1.csv", header=TRUE, sep=";", dec=".")
summary(agg.dados.cantos.order1)
str(agg.dados.cantos.order1)

agg.dados.cantos.order2 = read.csv2(file="agg.dados.canto.order2.csv", header=TRUE, sep=";", dec=".")
summary(agg.dados.cantos.order2)
str(agg.dados.cantos.order2)

# Convert the variable number of syllable types from an interval to a numeric variable
agg.dados.cantos.comp$N.sil.diff_sd <- as.character(agg.dados.cantos.comp$N.sil.diff_sd)
agg.dados.cantos.comp$N.sil.diff_sd <- as.numeric(agg.dados.cantos.comp$N.sil.diff_sd)
agg.dados.cantos.comp$N.sil.diff_mean <- as.character(agg.dados.cantos.comp$N.sil.diff_mean)
agg.dados.cantos.comp$N.sil.diff_mean <- as.numeric(agg.dados.cantos.comp$N.sil.diff_mean)

agg.dados.cantos.order1$N.sil.diff_mean = as.numeric(agg.dados.cantos.order1$N.sil.diff_mean)

agg.dados.cantos.order1$N.sil.diff_sd = as.numeric(agg.dados.cantos.order1$N.sil.diff_sd)

agg.dados.cantos.order2$N.sil.diff_mean = as.numeric(agg.dados.cantos.order2$N.sil.diff_mean)

agg.dados.cantos.order2$N.sil.diff_sd = as.numeric(agg.dados.cantos.order2$N.sil.diff_sd)

# Convert the variable playback type from a numeric variable to a factor variable
agg.dados.cantos.comp$Tratamento <- as.factor(agg.dados.cantos.comp$Tratamento)
agg.dados.cantos.order1$Tratamento = as.factor(agg.dados.cantos.order1$Tratamento)
agg.dados.cantos.order2$Tratamento = as.factor(agg.dados.cantos.order2$Tratamento)

#Convert the variable number of ID from a numeric to a factor variable

agg.dados.cantos.comp$ID <- as.factor(agg.dados.cantos.comp$ID)
agg.dados.cantos.order1$ID <- as.factor(agg.dados.cantos.order1$ID)
agg.dados.cantos.order2$ID <- as.factor(agg.dados.cantos.order2$ID)

require(ggplot2)
#gr?fico em cores:
plot.n.sill= ggplot(data=agg.dados.cantos.comp, 
                    aes(x=Tratamento, 
                        y=N.Sil_mean, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) +
  geom_errorbar(aes(ymin=N.Sil_mean-N.Sil_sd, ymax=N.Sil_mean+N.Sil_sd), width=0.03, position=position_dodge(0.4)) +
  geom_line(size=0.7, position=position_dodge(0.4)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.5, linetype="dashed", color="grey", size=0.75) +
  geom_vline(xintercept=2.5, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Number of syllables per song") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=25),
        axis.text.x=element_text(size=25, color="black"),
        axis.text.y=element_text(size=25, color="black"),
        axis.title=element_text(size=25),
        legend.position='none')

#gr?fico em preto e branco e mais simples:
plot.n.sill= ggplot(data=agg.dados.cantos.comp, 
                    aes(x=Tratamento, 
                        y=N.Sil_mean, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Number of syllables per song") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=25),
        axis.text.x=element_text(size=25, color="black"),
        axis.text.y=element_text(size=25, color="black"),
        axis.title=element_text(size=25),
        legend.position='none')


plot.n.sill


dir()
ggsave("n.sill.tiff", plot.n.sill, dpi=1200, limitsize = FALSE)

ggplot(data=dados.cantos.pbck, aes(x=Tratamento, y=N.sil.diff, group=ID, colour=ID)) +
  geom_line() +
  geom_point() +
  geom_errorbar()
scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=25),
        axis.title=element_text(size=23))

#gr?ficos separados de acordo com a ordem do playback
g1=ggplot(data=agg.dados.cantos.order1, aes(x=Tratamento, y=Low.Freq.Hz_mean, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8, 9, 10,11)) +
  geom_errorbar(aes(ymin=Low.Freq.Hz_mean-Low.Freq.Hz_sd, ymax=Low.Freq.Hz_mean+Low.Freq.Hz_sd), width=0.03, position=position_dodge(0.4), size=1) +
  geom_line(size=1.5, position=position_dodge(0.4)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.5, linetype="dashed", color="grey", size=0.75) +
  geom_vline(xintercept=2.5, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment (order 1)") +
  scale_y_continuous(name="Minimum song Frequency (Hz)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=40),
        axis.text.x=element_text(size=40),
        axis.text.y=element_text(size=30),
        axis.title=element_text(size=40),
        legend.position = "none")
g1

#preto e branco

g1=ggplot(data=agg.dados.cantos.order1, aes(x=Tratamento, y=Low.Freq.Hz_mean, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment (order 1)", labels=c("Before" = "Pre Playback", "Elaborate" = "Elaborate", "Simple" = "Simple")) +
  scale_y_continuous(name="Minimum song Frequency (Hz)", limits=c(2000,4500)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, color="black", size=0.5),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.text.y=element_text(size=12),
        axis.title.y=element_text(size=16),
        legend.position = "none")
g1

ggsave("g1.tiff", g1, width=14.0, height=14.0, dpi=1200, limitsize = FALSE)

level_order= c('Before', 'Simple', 'Elaborate')

g2=ggplot(data=agg.dados.cantos.order2, aes(x=factor(Tratamento, level=level_order), y=Low.Freq.Hz_mean, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment (order 2)", labels=c("Before" = "Pre Playback", "Elaborate" = "Elaborate", "Simple" = "Simple")) +
  scale_y_continuous(name="Minimum song Frequency (Hz)", limits=c(2000,4500)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA, color="black", size=0.5),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        legend.position = "none")
g2


ggsave("low.freq.order2.tiff", g2, width=14.0, height=14.0, dpi=1200, limitsize = FALSE)

g3=ggplot(data=agg.dados.cantos.order1, aes(x=Tratamento, y=Delta.Freq.Hz_mean, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11)) +
  geom_errorbar(aes(ymin=Delta.Freq.Hz_mean-Delta.Freq.Hz_sd, ymax=Delta.Freq.Hz_mean+Delta.Freq.Hz_sd), width=0.03, position=position_dodge(0.4), size=1) +
  geom_line(size=1.5, position=position_dodge(0.4)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.5, linetype="dashed", color="grey", size=0.75) +
  geom_vline(xintercept=2.5, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment (order 1)") +
  scale_y_continuous(name="Frequency Bandwidth (Hz)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=40),
        axis.text.x=element_text(size=40),
        axis.text.y=element_text(size=30),
        axis.title=element_text(size=40),
        legend.position = "none")
g3

#preto e branco
g3=ggplot(data=agg.dados.cantos.order1, aes(x=Tratamento, y=Delta.Freq.Hz_mean, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment (order 1)", labels=c("Before" = "Pre Playback", "Elaborate" = "Elaborate", "Simple" = "Simple")) +
  scale_y_continuous(name="Frequency Bandwidth (Hz)", limits=c(8000,12000)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, color="black", size=0.5),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_text(size=12),
        axis.title=element_text(size=16),
        legend.position = "none")
g3

ggsave("freq.band.order1.tiff", g3, width=14.0, height=14.0, dpi=1200, limitsize = FALSE)

g4=ggplot(data=agg.dados.cantos.order2, aes(x=factor(Tratamento, level=level_order), y=Delta.Freq.Hz_mean, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11)) +
  geom_errorbar(aes(ymin=Delta.Freq.Hz_mean-Delta.Freq.Hz_sd, ymax=Delta.Freq.Hz_mean+Delta.Freq.Hz_sd), width=0.03, position=position_dodge(0.4), size=1) +
  geom_line(size=1.5, position=position_dodge(0.4)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.5, linetype="dashed", color="grey", size=0.75) +
  geom_vline(xintercept=2.5, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment (order 2)") +
  scale_y_continuous(name="Frequency Bandwidth (Hz)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=40),
        axis.text.x=element_text(size=40),
        axis.text.y=element_text(size=30),
        axis.title=element_text(size=40),
        legend.position = "none")
g4

g4=ggplot(data=agg.dados.cantos.order2, aes(x=factor(Tratamento, level=level_order), y=Delta.Freq.Hz_mean, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment (order 2)", labels=c("Before" = "Pre Playback", "Elaborate" = "Elaborate", "Simple" = "Simple")) +
  scale_y_continuous(name="Frequency Bandwidth (Hz)", limits=c(8000,12000)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, color="black", size=0.5),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        line = element_line(colour="black", size=1),
        axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_blank(),
        axis.title.y = element_blank(),
        axis.title=element_text(size=16),
        legend.position = "none")

g4



ggsave("freq.band.order2.tiff", g4, width=14.0, height=14.0, dpi=1200, limitsize = FALSE)

#Several plots in one figure using ggplot-----
install.packages('gridExtra')
require(gridExtra)

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
require(ggpubr)

four.graphs.song = ggarrange(g1, g2, g3, g4,
                             labels = c("I", "II", "III", "IV"), 
                             hjust=c(-19,-10,-6.5,-5.5),
                             vjust=1.9, align="hv", 
                             font.label = list(size=14, face="bold"),
                             ncol = 2, nrow = 2,
                             widths = c(1, 1))
four.graphs.song

ggsave("four.graphs.song.tiff", four.graphs.song, width=10.5, height=10.5, dpi=1200, limitsize = FALSE)

ggsave("four.graphs.tiff", four.graphs, width=14.0, height=14.0, dpi=1200, limitsize = FALSE)


#### 4. Fazer um GLM com dados de resposta comportamental------
####Fazer um GLM (generalized linear model) para avaliar as mudancas no comportamento de coereba em relacao aos diferentes tipos de playback (elaborate vs simple)

#### 4.1. Ler tabela de dados de comportamento "Dados.Canto.completo"-----
setwd("C:/Users/Gabrielle/OneDrive/Doutorado/Cap 2_bananaquit experiments/Dados")
dir()
dados.behav.pbck = read.csv2(file="Dados.Playback.1.csv", header=TRUE, sep=";", dec=".")
dados.behav.pbck$ID <- as.factor(dados.behav.pbck$ID)
dados.behav.pbck$Total.n.flights <- as.character(dados.behav.pbck$Total.n.flights)
dados.behav.pbck$Total.n.flights <- as.numeric(dados.behav.pbck$Total.n.flights)
head(dados.behav.pbck)
summary(dados.behav.pbck)
str(dados.behav.pbck)

dados.behav.pbck$ID <- as.factor(dados.behav.pbck$ID)

dados.song.rate = read.csv2(file="dados.song.rate.csv", header=TRUE, sep=";", dec=".")
head(dados.song.rate)
summary(dados.song.rate)
str(dados.song.rate)
dados.song.rate$ID <- as.factor(dados.song.rate$ID)
dados.song.rate$Tratamento <- as.character(dados.song.rate$Tratamento)
dados.song.rate$Pbck.1.2 <- as.factor(dados.song.rate$Pbck.1.2)

agg.song.rate$Tratamento = as.factor(agg.song.rate$Tratamento)
agg.song.rate$Song.rate_mean = as.factor(agg.song.rate$Song.rate_mean)
agg.song.rate$Song.rate_sd = as.factor(agg.song.rate$Song.rate_sd)

dados.call.rate = read.csv2(file="Dados.call.rate.csv", header=TRUE, sep="\t", dec=".")
head(dados.call.rate)
summary(dados.call.rate)
str(dados.call.rate)
dados.call.rate$ID <- as.factor(dados.call.rate$ID)
dados.call.rate$Pbck.1.2 <- as.factor(dados.call.rate$Pbck.1.2)

####4.2. GLM com dados comportamentais----
#Number of flights----

lmm.total.n.flights.pois <- glmer(Total.n.flights ~ Playback + Ordem + (1 | ID),
                                  data = dados.behav.pbck,
                                  family = "poisson")
plot(lmm.total.n.flights.pois)
summary(lmm.total.n.flights.pois)

require(MuMIn)

model.global <- glmer(Total.n.flights ~ Playback + Ordem + (1 | ID),
                      data = dados.behav.pbck,
                      family = "poisson", na.action = "na.fail")
summary(model.global)

d1 <- dredge(model.global)
model.best <- get.models(d1, subset = 1)[[1]]
r.squaredGLMM(model.best)

summary(model.best)
out.put=model.sel(dredge(model.global))
out.put 
plot(model.best)

#manualmente especificando os modelos
model.global <- glmer(Total.n.flights ~ Playback + Ordem + (1 | ID),
                      data = dados.behav.pbck,
                      family = "poisson", na.action = "na.fail")

model.noorder <- glmer(Total.n.flights ~ Playback + (1 | ID),
                       data = dados.behav.pbck,
                       family = "poisson", na.action = "na.fail")

model.notreat <- glmer(Total.n.flights ~ Ordem + (1 | ID),
                       data = dados.behav.pbck,
                       family = "poisson", na.action = "na.fail")

model.nofixed <- glmer(Total.n.flights ~ (1 | ID),
                       data = dados.behav.pbck,
                       family = "poisson", na.action = "na.fail")

r.squaredGLMM(model.global)
r.squaredGLMM(model.noorder)
r.squaredGLMM(model.notreat)
r.squaredGLMM(model.nofixed)

post.hoc.test.pbck=glht(lmm.total.n.flights, linfct=mcp(Playback="Tukey"))
summary(post.hoc.test.pbck)

#Shortest distance----

require(MuMIn)

model.global <- lmer(Shortest.Dist ~ Playback + Ordem + (1 | ID),
                     data = dados.behav.pbck,
                     na.action = "na.fail", REML="False")
summary(model.global)

d1 <- dredge(model.global)
model.best <- get.models(d1, subset = 1)[[1]]
r.squaredGLMM(model.best)

summary(model.best)
out.put=model.sel(dredge(model.global))
out.put 
plot(model.best)

#manually specifying the models
lmm.short.dist <- lmer(Shortest.Dist ~ Playback + Ordem + (1 | ID),
                       data = dados.behav.pbck,
                       na.action = "na.fail", REML="False")

lmm.short.dist.noorder <- lmer(Shortest.Dist ~ Playback + (1 | ID),
                               data = dados.behav.pbck,
                               na.action = "na.fail", REML="False")

lmm.short.dist.notreat <- lmer(Shortest.Dist ~ Ordem + (1 | ID),
                               data = dados.behav.pbck,
                               na.action = "na.fail", REML="False")

lmm.short.dist.nofixed <- lmer(Shortest.Dist ~ 1 + (1 | ID),
                               data = dados.behav.pbck,
                               na.action = "na.fail", REML="False")

r.squaredGLMM(lmm.short.dist)
r.squaredGLMM(lmm.short.dist.noorder)
r.squaredGLMM(lmm.short.dist.notreat)
r.squaredGLMM(lmm.short.dist.nofixed)

out.put<-model.sel(lmm.short.dist, lmm.short.dist.noorder, lmm.short.dist.notreat, lmm.short.dist.nofixed)
out.put

anova(lmm.short.dist, lmm.short.dist.noorder, lmm.short.dist.notreat, lmm.short.dist.nofixed)

require(multcomp)
post.hoc.test.short.dist=glht(lmm.short.dist.nofixed, linfct=mcp(Playback="Tukey"))
summary(post.hoc.test.short.dist)

#Get out of tree----
lmm.out.tree <- lmer(Getout.tree ~ Playback + Ordem + (1 | ID), data = dados.behav.pbck,
                     REML = FALSE)

lmm.out.tree.noorder <- lmer(Getout.tree ~ Playback + (1 | ID), data = dados.behav.pbck,
                             REML = FALSE)

lmm.out.tree.notreat <- lmer(Getout.tree ~ Ordem + (1 | ID), data = dados.behav.pbck,
                             REML = FALSE)

lmm.out.tree.nofixed <- lmer(Getout.tree ~ 1 + (1 | ID), data = dados.behav.pbck,
                             REML = FALSE)

out.put<-model.sel(lmm.out.tree, lmm.out.tree.noorder, lmm.out.tree.notreat, lmm.out.tree.nofixed)
out.put

anova(lmm.out.tree, lmm.out.tree.noorder, lmm.out.tree.notreat, lmm.out.tree.nofixed)

#Song.rate----
str(dados.song.rate)
model.global <- lmer(Song.rate ~ Tratamento + Pbck.1.2 + (1 | ID),
                     data = dados.song.rate,
                     na.action = "na.fail", REML="False")
summary(model.global)

d1 <- dredge(model.global)
model.best <- get.models(d1, subset = 1)[[1]]
r.squaredGLMM(model.best)

summary(model.best)
out.put=model.sel(dredge(model.global))
out.put 
plot(model.best)

str(dados.song.rate)

lmm.song.rate.global <- lmer(Song.rate ~ Tratamento + Pbck.1.2 + (1 | ID),
                             data = dados.song.rate,
                             na.action = "na.fail", REML="False")

lmm.song.rate.noorder <- lmer(Song.rate ~ Tratamento + (1 | ID),
                              data = dados.song.rate,
                              na.action = "na.fail", REML="False")

lmm.song.rate.notreat <- lmer(Song.rate ~ Pbck.1.2 + (1 | ID),
                              data = dados.song.rate,
                              na.action = "na.fail", REML="False")

lmm.song.rate.nofixed <- lmer(Song.rate ~ 1 + (1 | ID),
                              data = dados.song.rate,
                              na.action = "na.fail", REML="False")

r.squaredGLMM(lmm.song.rate.global)
r.squaredGLMM(lmm.song.rate.noorder)
r.squaredGLMM(lmm.song.rate.notreat)
r.squaredGLMM(lmm.song.rate.nofixed)

out.put<-model.sel(lmm.song.rate, lmm.song.rate.noorder, lmm.song.rate.notreat, lmm.song.rate.nofixed)
out.put

anova(lmm.out.tree, lmm.out.tree.noorder, lmm.out.tree.notreat, lmm.out.tree.nofixed)

#call rate----

model.global <- lmer(Call.rate ~ Tratamento + Pbck.1.2 + (1 | ID),
                     data = dados.call.rate,
                     na.action = "na.fail", REML="False")
summary(model.global)

d1 <- dredge(model.global)
model.best <- get.models(d1, subset = 1)[[1]]
r.squaredGLMM(model.best)

summary(model.best)
out.put=model.sel(dredge(model.global))
out.put 
plot(model.best)

lmm.call.rate.global <- lmer(Call.rate ~ Tratamento + Pbck.1.2 + (1 | ID),
                             data = dados.call.rate,
                             na.action = "na.fail", REML="False")

lmm.call.rate.noorder <- lmer(Call.rate ~ Tratamento + (1 | ID),
                              data = dados.call.rate,
                              na.action = "na.fail", REML="False")

lmm.call.rate.notreat <- lmer(Call.rate ~ Pbck.1.2 + (1 | ID),
                              data = dados.call.rate,
                              na.action = "na.fail", REML="False")

lmm.call.rate.nofixed <- lmer(Call.rate ~ 1 + (1 | ID),
                              data = dados.call.rate,
                              na.action = "na.fail", REML="False")

r.squaredGLMM(lmm.call.rate.global)
r.squaredGLMM(lmm.call.rate.noorder)
r.squaredGLMM(lmm.call.rate.notreat)
r.squaredGLMM(lmm.call.rate.nofixed)

out.put<-model.sel(lmm.call.rate, lmm.call.rate.noorder, lmm.call.rate.notreat, lmm.call.rate.nofixed)
out.put

anova(lmm.out.tree, lmm.out.tree.noorder, lmm.out.tree.notreat, lmm.out.tree.nofixed)

###4.3.Runing the post hoc test----
require(multcomp)
#Song rate, best model
post.hoc.song.rate=glht(lmm.song.rate.noorder, linfct=mcp(Tratamento="Tukey"))
summary(post.hoc.song.rate)

post.hoc.call.rate=glht(lmm.call.rate.noorder, linfct=mcp(Tratamento="Tukey"))
summary(post.hoc.call.rate)

###4.4. Graphs with the behavioural variables----
# Convert the variable shortest distance from a factor to a numeric variable

dados.behav.pbck$Shortest.Dist <- as.numeric(dados.behav.pbck$Shortest.Dist)

# Convert the variable playback (simple or complex) from a numeric variable to a factor
dados.behav.pbck$Playback <- as.factor(dados.behav.pbck$Playback)
dados.behav.pbck$ID <- as.factor(dados.behav.pbck$ID)

require(ggplot2)

g.n.flights= ggplot(data=dados.behav.pbck, 
                    aes(x=Playback, 
                        y=N.flight.aprox.horiz, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) +
  geom_line(size=0.8, position=position_dodge(0.3)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.45, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Number of flights") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position='none')

g.n.flights

g.n.flights=ggplot(data=dados.behav.pbck, aes(x=Playback, y=N.flight.aprox.horiz, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Number of Flights", limits= c(0,7)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, size=0.5, colour = "black"),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position = "none")

ggsave("g.n.flights.tiff", g.n.flights, width=10.5, height=10.5, dpi=1200, limitsize = FALSE)

g.shortest.dist= ggplot(data=dados.behav.pbck, 
                        aes(x=Playback, 
                            y=Shortest.Dist, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) +
  geom_line(size=0.8, position=position_dodge(0.3)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.45, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Closest distance") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position='none')

g.shortest.dist

g.shortest.dist=ggplot(data=dados.behav.pbck, aes(x=Playback, y=Shortest.Dist, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Closest distance", limits=c(0,15)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position = "none")

ggsave("g.shortest.dist.tiff", g.shortest.dist, width=10.5, height=10.5, dpi=1200, limitsize = FALSE)

require(ggplot2)
agg.song.rate = read.csv2(file="agg.song.rate.csv", header=TRUE, sep=";", dec=".")
head(agg.song.rate)
summary(agg.song.rate)
str(agg.song.rate)

agg.song.rate$ID <- as.factor(agg.song.rate$ID)
agg.song.rate$Tratamento <- as.factor(agg.song.rate$Tratamento)

agg.song.rate$Song.rate_mean <- as.character(agg.song.rate$Song.rate_mean)
agg.song.rate$Song.rate_sd <- as.character(agg.song.rate$Song.rate_sd)

agg.song.rate$Song.rate_mean <- as.numeric(agg.song.rate$Song.rate_mean)
agg.song.rate$Song.rate_sd <- as.numeric(agg.song.rate$Song.rate_sd)

agg.song.rate=ggplot(data=agg.song.rate, aes(x=Tratamento, y=Song.rate_mean, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)) +
  geom_errorbar(aes(ymin=Song.rate_mean-Song.rate_sd, ymax=Song.rate_mean+Song.rate_sd), width=0.03, position=position_dodge(0.4), size=1) +
  geom_line(size=0.8, position=position_dodge(0.4)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.5, linetype="dashed", color="grey", size=0.75) +
  geom_vline(xintercept=2.5, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Song rate (songs/sec)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position='none')

agg.song.rate

agg.song.rate=ggplot(data=agg.song.rate, aes(x=Tratamento, y=Song.rate_mean, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Song rate (songs/sec)") +
  theme_minimal() +
  theme(panel.background=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position = "none")

ggsave("song.rate.tiff", agg.song.rate, width=11.5, height= 10.5, dpi=1200, limitsize = FALSE)

agg.call.rate = read.csv2(file="agg.call.rate.csv", header=TRUE, sep=";", dec=".")
head(agg.call.rate)
summary(agg.call.rate)
str(agg.call.rate)

agg.call.rate$ID <- as.factor(agg.call.rate$ID)

g.call.rate=ggplot(data=agg.call.rate, aes(x=Tratamento, y=Call.rate_mean, group=ID, colour=ID, shape=ID)) +
  scale_shape_manual(values=c(0,1,2,3,4,5,6,7,8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)) +
  geom_errorbar(aes(ymin=Call.rate_mean-Call.rate_sd, ymax=Call.rate_mean+Call.rate_sd), width=0.03, position=position_dodge(0.4), size=1) +
  geom_line(size=0.8, position=position_dodge(0.4)) +
  geom_point(size=4, position=position_dodge(0.4)) +
  geom_vline(xintercept=1.5, linetype="dashed", color="grey", size=0.75) +
  geom_vline(xintercept=2.5, linetype="dashed", color="grey", size=0.75) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Call rate (calls/sec)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=16),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position='none')

agg.call.rate

agg.call.rate=ggplot(data=agg.call.rate, aes(x=Tratamento, y=Call.rate_mean, group=ID)) +
  geom_line(size=1) +
  geom_point(size=1.2) +
  scale_x_discrete(name="Treatment") +
  scale_y_continuous(name="Call rate (calls/sec)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black", size=0.5),
        axis.line.x=element_line(colour="black", size=0.5),
        axis.text=element_text(size=16),
        axis.text.x=element_text(size=16, color="black"),
        axis.text.y=element_text(size=14),
        axis.title.y=element_text(size=16),
        axis.title.x=element_text(size=16),
        legend.position = "none")

ggsave("call.rate.tiff", g.call.rate, width=11.5, height= 10.0, dpi=1200, limitsize = FALSE)

#Uma figura s? com os quatro gr?ficos
require(gridExtra)
require(ggpubr)

four.graphs.behav = ggarrange(g.n.flights, g.shortest.dist, agg.song.rate, agg.call.rate,
                              labels = c("I", "II", "III", "IV"), 
                              hjust=c(-15.5,-8,-5.2,-4.5),
                              ncol = 2, nrow = 2,
                              vjust=1.9, align="hv", 
                              font.label = list(size=14, face="bold"),
                              widths = c(1, 1))

four.graphs.behav


ggsave("four.graphs.behav.tiff", four.graphs.behav, width=10.5, height=10.5, dpi=1200, limitsize = FALSE)


###5. Correla??o Trade-off entre n?mero de s?labas diff e song frequency----
require(ggplot2)
agg.dados.cantos.pbck = read.csv2(file="agg.dados.cantos.completo.csv", header=TRUE, sep=";", dec=".")
summary(agg.dados.cantos.pbck)
head(agg.dados.cantos.pbck)

agg.geral.cantos = read.csv2(file="agg.geral.dados.cantos.csv", header=TRUE, sep=",", dec=".")
head(agg.geral.cantos)
str(agg.geral.cantos)

agg.cantos.spont = read.csv2(file="agg.dados.cantos.spontenaeus.csv", header=TRUE, sep=";", dec=".")
head(agg.cantos.spont)
str(agg.cantos.spont)

#Fitting linear models for the relationship between noise and song parameters
# Convert the variable number of syllable types from an interval to a numeric variable
agg.dados.cantos.pbck$N.sil.diff_mean <- as.numeric(agg.dados.cantos.pbck$N.sil.diff_mean)
agg.dados.cantos.pbck$Low.Freq.mean <- as.numeric(agg.dados.cantos.pbck$N.sil.diff_mean)

lm.trade.off=lm(agg.dados.cantos.pbck$N.sil.diff_mean ~ agg.dados.cantos.pbck$Low.Freq.Hz_mean)
summary (lm.trade.off)

lm.trade.off.geral=lm(agg.cantos.spont$N.sil.diff_mean ~ agg.cantos.spont$Low.Freq.Hz_mean)
summary (lm.trade.off.geral)

lm.trade.off=lm(agg.cantos.spont$N.sil.diff_mean ~ agg.cantos.spont$Low.Freq.Hz_mean)
summary (lm.trade.off)

lm.trade.off.geral=lm(agg.cantos.spont$N.sil.diff_mean ~ agg.cantos.spont$Delta.Freq.Hz_mean)
summary (lm.trade.off.geral)

lm.trade.off=lm(agg.dados.cantos.pbck$N.sil.diff_mean ~ agg.dados.cantos.pbck$Delta.Freq.Hz_mean)
summary (lm.trade.off)

retag1=predict(lm(agg.dados.cantos.pbck$N.sil.diff_mean ~ agg.dados.cantos.pbck$Delta.Freq.Hz_mean))
g1=ggplot(agg.dados.cantos.pbck) + aes(y=N.sil.diff_mean, x=Delta.Freq.Hz_mean) +
  geom_point(size=4) +
  scale_x_continuous(name="Frequency bandwidth (Hz)") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=20),
        axis.title=element_text(family ="sans", face="bold", size=20)
  )
g1

#Trade-off evaluation for the 5 individuals with most consistent variation in number of syllables
lm.tradeoff.5consist=lm(agg.dados.cantos.5consist$N.sil.diff_mean ~ agg.dados.cantos.5consist$Low.Freq.Hz_mean)
summary(lm.tradeoff.5consist)

dados.cantos.5consist = read.csv2(file="Dados.cantos.5consistent.csv", header=TRUE, sep=";", dec=".")
summary(dados.cantos.5consist)

dados.cantos.5consist$N.sil.diff <- as.numeric(dados.cantos.5consist$N.sil.diff)
dados.cantos.5consist$Low.Freq.Hz <- as.numeric(dados.cantos.5consist$Low.Freq.Hz)

lm.tradeoff.5consist=lm(dados.cantos.5consist$N.sil.diff ~ dados.cantos.5consist$Low.Freq.Hz)
summary(lm.tradeoff.5consist)

retag1=predict(lm(dados.cantos.5consist$N.sil.diff ~ dados.cantos.5consist$Low.Freq.Hz))
g1=ggplot(dados.cantos.5consist) + aes(y=N.sil.diff, x=Low.Freq.Hz) +
  geom_point(size=1.5) +
  geom_line(aes(y=retag1)) +
  scale_x_continuous(name="Minimum frequency (Hz)") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=12),
        axis.title=element_text(family ="sans", face="bold", size=12)
  )
g1

#Trade-off evaluation for the 5 individuals with the highest variation in number of syllables types----

agg.dados.cantos.highvar = read.csv2(file="Agg.dados.cantos.5highvar.n.sil.csv", header=TRUE, sep=";", dec=",")
summary(agg.dados.cantos.highvar)

# Convert the variable number of syllable types from an interval to a numeric variable
agg.dados.cantos.highvar$N.sil.diff <- as.numeric(agg.dados.cantos.highvar$N.sil.diff)
agg.dados.cantos.highvar$Low.Freq.Hz <- as.numeric(agg.dados.cantos.highvar$Low.Freq.Hz)

lm.tradeoff.highvar=lm(agg.dados.cantos.highvar$N.sil.diff ~ agg.dados.cantos.highvar$Low.Freq.Hz)
summary(lm.tradeoff.highvar)

retag1=predict(lm(agg.dados.cantos.highvar$N.sil.diff ~ agg.dados.cantos.highvar$Low.Freq.Hz))
g1=ggplot(agg.dados.cantos.highvar) + aes(y=N.sil.diff, x=Low.Freq.Hz) +
  geom_point(size=4) +
  geom_line(aes(y=retag1)) +
  scale_x_continuous(name="Minimum frequency (Hz)") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=24),
        axis.title=element_text(family ="sans", face="bold", size=24)
  )
g1

#Trade-off evaluation for the 5 individuals with the highest variation in low frequency----

agg.dados.cantos.highvar.2 = read.csv2(file="Agg.dados.cantos.highvar.low.freq.csv", header=TRUE, sep=";", dec=",")
summary(agg.dados.cantos.highvar.2)

# Convert the variable number of syllable types from an interval to a numeric variable
agg.dados.cantos.highvar.2$N.sil.diff <- as.numeric(agg.dados.cantos.highvar.2$N.sil.diff)
agg.dados.cantos.highvar.2$Low.Freq.Hz <- as.numeric(agg.dados.cantos.highvar.2$Low.Freq.Hz)

lm.tradeoff.highvar.2=lm(agg.dados.cantos.highvar.2$N.sil.diff ~ agg.dados.cantos.highvar.2$Low.Freq.Hz)
summary(lm.tradeoff.highvar.2)
library(ggplot2)
retag1=predict(lm(agg.dados.cantos.highvar.2$N.sil.diff ~ agg.dados.cantos.highvar.2$Low.Freq.Hz))
g1=ggplot(agg.dados.cantos.highvar.2) + aes(y=N.sil.diff, x=Low.Freq.Hz) +
  geom_point(size=4) +
  geom_line(aes(y=retag1)) +
  scale_x_continuous(name="Minimum frequency (Hz)") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=20),
        axis.title=element_text(family ="sans", face="bold", size=20)
  )
g1



#### 6.An?lises das diferen?as dos est?mulos simple vs elaborate----
#### 6.1. Ler tabela de dados do canto "Dados.stimulus.pbck" e aggregar m?dia das vari?veis por indiv?duo-----
setwd("C:/Users/Gabrielle/OneDrive/Doutorado/Cap 2_bananaquit experiments/Dados")
dir()
dados.stimulus.pbck = read.csv2(file="Dados.stimulus.csv", header=TRUE, sep=";", dec=".")
head(dados.stimulus.pbck)
summary(dados.stimulus.pbck)
str(dados.stimulus.pbck)

agg.stimulus = aggregate(dados.stimulus.pbck, by=list(dados.stimulus.pbck$Arquivo, dados.stimulus.pbck$Stimulus), FUN=mean)
head(agg.stimulus)
str(agg.stimulus)

agg.stimulus = subset(agg.stimulus, select = -c(Selection, View, Channel, Arquivo, Stimulus))
head(agg.stimulus)

colnames(agg.stimulus)[1]="ID"
colnames(agg.stimulus)[2]="Stimulus"
head(agg.stimulus)

write.table(agg.stimulus, file="agg.stimulus.csv", sep=";", row.names=FALSE, col.names=TRUE, dec=".")


#### 6.2.Fitting anova to compare song variables between elaborate vs simple songs----
# Convert the variable number of syllable types from an interval to a numeric variable
agg.stimulus$Num.de.S?labas.Diferentes <- as.character(agg.stimulus$N?m.de.S?labas.Diferentes)
agg.stimulus$Num.de.S?labas.Diferentes <- as.numeric(agg.stimulus$N?m.de.S?labas.Diferentes)
agg.stimulus$Num.de.S?labas <- as.character(agg.stimulus$N?m.de.S?labas)
agg.stimulus$Num.de.S?labas <- as.numeric(agg.stimulus$N?m.de.S?labas)
agg.stimulus$Syll.rate <- as.character(agg.stimulus$Syll.rate)
agg.stimulus$Syll.rate <- as.numeric(agg.stimulus$Syll.rate)

#N of s?labas diferetes
lmm.n.syla.diff.pois <- glmer(agg.stimulus$N?m.de.S?labas.Diferentes ~ agg.stimulus$Stimulus,
                              data = agg.stimulus,
                              family = "poisson")
plot(lmm.total.n.flights.pois)
summary(lmm.total.n.flights.pois)

nsil.diff.stimulus=aov(agg.stimulus$N?m.de.S?labas.Diferentes ~ agg.stimulus$Stimulus)
summary.aov (nsil.diff.stimulus)

nsil.stimulus=aov(agg.stimulus$N?m.de.S?labas ~ agg.stimulus$Stimulus)
summary.aov (nsil.stimulus)

duration.stimulus=aov(agg.stimulus$Delta.Time..s. ~ agg.stimulus$Stimulus)
summary.aov (duration.stimulus)

syllrate.stimulus=aov(agg.stimulus$Syll.rate ~ agg.stimulus$Stimulus)
summary.aov (syllrate.stimulus)

peak.freq.stimulus=aov(agg.stimulus$Peak.Freq..Hz. ~ agg.stimulus$Stimulus)
summary.aov (peak.freq.stimulus)

low.freq.stimulus=aov(agg.stimulus$Low.Freq..Hz. ~ agg.stimulus$Stimulus)
summary.aov (low.freq.stimulus)

max.freq.stimulus=aov(agg.stimulus$High.Freq..Hz. ~ agg.stimulus$Stimulus)
summary.aov (max.freq.stimulus)

delta.freq.stimulus=aov(agg.stimulus$Delta.Freq..Hz. ~ agg.stimulus$Stimulus)
summary.aov (delta.freq.stimulus)

str.sil.diff=sd

#### 6.3.Computing boxplots for the results simple vs elaborate----
# Basic box plot
library(ggplot2)
library(ggpubr)
head(agg.stimulus)

#N?mero de s?labas diferentes
stimulus.sil.diff <- ggplot(agg.stimulus, aes(x=agg.stimulus$Stimulus, y=agg.stimulus$N?m.de.S?labas.Diferentes)) +
  geom_boxplot(fill="gray") +
  scale_x_discrete(name="Playback stimulus") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA, color="black", size=0.5),
        axis.text.y=element_text(size=30, color="black"),
        axis.title.y=element_text(size=30, color="black"),
        axis.text.x=element_text(size=30, color="black"),
        axis.title.x = element_blank())

stimulus.sil.diff

ggsave("stimulus.sil.diff.tiff", stimulus.sil.diff, height= 6, width= 6, dpi=1200, limitsize = FALSE)

# Box plot with dot plot
stimulus.sil.diff + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)

#N?mero total de s?labas
stimulus.n.sil <- ggplot(agg.stimulus, aes(x=agg.stimulus$Stimulus, y=agg.stimulus$N?m.de.S?labas)) +
  geom_boxplot(fill="gray") +
  scale_x_discrete(name="Playback stimulus") +
  scale_y_continuous(name="Total number of syllable per song") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text.y=element_text(size=16, color="black"),
        axis.title.y=element_text(size=16, color="black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()
  )

stimulus.n.sil

#Song lenght
stimulus.length <- ggplot(agg.stimulus, aes(x=agg.stimulus$Stimulus, y=agg.stimulus$Delta.Time..s.)) +
  geom_boxplot(fill="gray") +
  scale_x_discrete(name="Playback stimulus") +
  scale_y_continuous(name="Song length") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16))

stimulus.length
stimulus.length + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5)

#Minimum song frequency
stimulus.low.freq <- ggplot(agg.stimulus, aes(x=agg.stimulus$Stimulus, y=agg.stimulus$Low.Freq..Hz.)) +
  geom_boxplot(fill="gray") +
  scale_x_discrete(name="Playback stimulus") +
  scale_y_continuous(name="Minimum song frequency (Hz)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA, color="black", size=0.5),
        axis.text.y=element_text(size=30, color="black"),
        axis.title.y=element_text(size=30, color="black"),
        axis.text.x=element_text(size=30, color="black"),
        axis.title.x = element_blank())

stimulus.low.freq


#Maximum song frequency
stimulus.high.freq <- ggplot(agg.stimulus, aes(x=agg.stimulus$Stimulus, y=agg.stimulus$High.Freq..Hz.)) +
  geom_boxplot(fill="gray") +
  scale_x_discrete(name="Playback stimulus") +
  scale_y_continuous(name="Maximum song frequency (Hz)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA, color="black", size=0.5),
        axis.text.y=element_text(size=30, color="black"),
        axis.title.y=element_text(size=30, color="black"),
        axis.text.x=element_text(size=30, color="black"),
        axis.title.x=element_blank())

stimulus.high.freq

#Frequency bandwidth
stimulus.freq.range <- ggplot(agg.stimulus, aes(x=agg.stimulus$Stimulus, y=agg.stimulus$Delta.Freq..Hz.)) +
  geom_boxplot(fill="gray") +
  scale_x_discrete(name="Playback stimulus") +
  scale_y_continuous(name="Frequency bandwidth (Hz)") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_rect(fill=NA, color="black", size=0.5),
        axis.line=element_line(colour="black", size=0.4),
        axis.line=element_line(colour="black", size=0.4),
        axis.text=element_text(size=35, color="black"),
        axis.title=element_text(size=35, color="black"))

stimulus.freq.range

graphs.stimulus = ggarrange(stimulus.sil.diff, stimulus.low.freq, stimulus.high.freq,
                            labels = c("I", "II", "III"), 
                            hjust=c(-10,-9,-6),
                            ncol = 3, nrow = 1,
                            vjust=1.9, align="h",
                            font.label = list(size=30, face="bold"),
                            widths = c(1, 1))

graphs.stimulus

write.table(agg.stimulus, "agg.stimulus.csv", sep = ";", row.names=FALSE, dec=".")

ggsave("graphs.stimulus.tiff", graphs.stimulus, width = 60, height = 20, unit="cm", dpi=1200, limitsize = FALSE)


