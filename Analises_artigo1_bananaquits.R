###Chapter 1 ----
###Coereba flaveola Correlation data############
###Gabrielle Winandy################
#########01-09-2020#################

library(lme4)
library(lmerTest)
library (nlme)
library(readxl)
library(ggplot2)
require(gridExtra)
library(MuMIn)
require(multcomp)
require(ggpubr)

#2. Preparar tabela de dados para an?lise----
#2.1. Ler tabela de dados do canto "Dados.Canto.completo"-----
dir()
coereba.flaveola.salvador = read.table("dadosbrutos.semrural.txt", header=T, sep=",", dec=".")
summary(coereba.flaveola.salvador)
str(coereba.flaveola.salvador)

#2.2. Gerar log das vari?veis de frequ?ncia e adicionar ? tabela ----
log.min.freq = log10(coereba.flaveola.salvador$Low_Freq_Hz)
log.high.freq = log10(coereba.flaveola.salvador$High_Freq_Hz)
log.delta.freq = log10(coereba.flaveola.salvador$Delta_Freq_Hz)
log.peak.freq = log10(coereba.flaveola.salvador$Peak_Freq_Hz)
song.duration = coereba.flaveola.salvador$End_Time_s - coereba.flaveola.salvador$Begin_Time_s
song.duration

#2.3.Colocar na tabela os logs das variaveis de frequencia
coereba.salvador.clog10 = cbind(coereba.flaveola.salvador, log.min.freq,log.high.freq, log.delta.freq, log.peak.freq, song.duration)

#delta.freq
log.freq.band = coereba.salvador.clog10$log.high.freq - coereba.salvador.clog10$log.min.freq

coereba.salvador.clog10 = cbind(coereba.salvador.clog10, log.freq.band)
summary(coereba.salvador.clog10)

#2.4. Write table .csv in directory----
write.csv(x = coereba.salvador.clog10,
          file = "dadosbrutoselog.semrural.csv", #nome do arquivo que vai ser gerado
          #sep = "\t", # define o caractere que deve ser o separador de colunas
          #dec = ".", #define o caractere que deve ser o spearador de decimal
          row.names = F #ignora nomes/numeros das linhas
)

coereba.salvador.clog10=read.csv("dadosbrutoselog.semrural.csv", sep=",", header=T, dec=".")

#### 2.5. Aggregate data frame, returning means ----
# for numeric variables
agg.coereba.salvador.clog10 <-aggregate(coereba.salvador.clog10, by=list(coereba.salvador.clog10$File),FUN=mean, na.rm=TRUE)
warnings()
print(agg.coereba.salvador.clog10)

write.csv(x = agg.coereba.salvador.clog10,
          file = "agg.coereba.semrural.clog10.csv", #nome do arquivo que vai ser gerado
          #sep = "\t", # define o caractere que deve ser o separador de colunas
          #dec = ".", #define o caractere que deve ser o spearador de decimal
          row.names = F #ignora nomes/numeros das linhas
)
agg.coereba.salvador.clog10 = read.table("agg.coereba.semrural.clog10.csv", header=T, sep=",", dec=".")
summary(agg.coereba.salvador.clog10)


#3. Gr?ficos----
#Gr?ficos com os logs das vari?veis de frequ?ncia
plot(agg.coereba.salvador.clog10$silabas.s ~ agg.coereba.salvador.clog10$log.delta.freq)
#3.1. N?mero de s?labas----
retag1=predict(lm(agg.coereba.salvador.clog10$N_Sil ~ agg.coereba.salvador.clog10$noise))
#gr?fico simples
ggplot(agg.coereba.salvador.clog10, aes(y=N_Sil, x=noise), ylab="Number of syllables per song", xlab="Noise level (dBA)") +
  geom_point() +
  scale_x_continuous(name="Noise level (dBA)") +
  scale_y_continuous(name="Number of syllables per song") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black")
  )
#gr?fico para submiss?o do artigo
g1=ggplot(agg.coereba.salvador.clog10, aes(y=N_Sil, x=noise)) +
  geom_point(size=2) +
  scale_x_continuous(name="Noise level (dBA)") +
  scale_y_continuous(name="Number of syllables") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text=element_text(size=25),
        axis.title=element_text(family="sans", size=25)
  )

g1  

#3.2. N?mero de s?labas diferentes----

#reta preditora
retag2=predict(lm(agg.coereba.salvador.clog10$N_Sil_Dif~agg.coereba.salvador.clog10$noise))

#gr?fico para submiss?o do artigo
g2=ggplot(agg.coereba.salvador.clog10, aes(y=N_Sil_Dif, x=noise)) +
  geom_point(size=2) +
  scale_x_continuous(name="Noise level (dBA)") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=25),
        axis.title=element_text(family= "sans",  size=25)
  )
g2
#Se precisar colocar negrito, adicionar face="bold" ao axis title

g2.1=g2 + ylim(-3,3)

geom_smooth(method="lm", data=agg.coereba.salvador.clog10,aes(ymin = retag3 - 0.5, ymax = retag3 + 0.5), alpha=0.2)

#3.3. Frequ?ncia m?nima----
retag3=predict(lm(agg.coereba.salvador.clog10$log.min.freq~ agg.coereba.salvador.clog10$noise))

g3=ggplot(agg.coereba.salvador.clog10, aes(y=log.min.freq, x=noise)) +
  geom_point(size=2) +
  ylab(bquote('Minimum frequency' ~ (log[10] ~ "Hz"))) +
  xlab(bquote("Noise level (dBA)")) +
  geom_line(size=0.5, aes(y=retag3)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=30),
        axis.title=element_text(family= "sans", size=30)
  )

g3 

#3.4. Frequ?ncia m?xima----
retag4=predict(lm(agg.coereba.salvador.clog10$log.high.freq~ agg.coereba.salvador.clog10$noise))
g4=ggplot(agg.coereba.salvador.clog10, aes(y=log.high.freq, x=noise)) +
  geom_point(size=2) +
  geom_line(size=0.5, aes(y=retag4)) +
  ylab(bquote('Maximum frequency' ~ (log[10] ~ 'Hz'))) +
  xlab(bquote("Noise level (dBA)")) + 
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text=element_text(size=30),
        axis.title=element_text(family = "sans", face= "bold", size=30)
  )
g4

#outra forma de colocar titulo dos eixos 
#  scale_x_continuous(name="Noise level (dBA)") +
#  scale_y_continuous(name="Maximum frequency (log Hz)") +


retag5=predict(lm(agg.coereba.salvador.clog10$log.delta.freq ~ agg.coereba.salvador.clog10$noise))

g5 = ggplot(agg.coereba.salvador.clog10, aes(y=log.delta.freq, x=noise)) + 
  geom_point() +
  labs(y = expression(Frequency bandwidth (log[1*0]Hz)), 
       x = expression(Noise level(dBA))) + 
  geom_line(aes(y=retag5)) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"))

g5

retag6=predict(lm(agg.coereba.salvador.clog10$N_Frases ~ agg.coereba.salvador.clog10$noise))
g6=ggplot(agg.coereba.salvador.clog10, aes(y=N_Frases, x=noise), ylab="Number of phrases", xlab="Noise level (dBA)") +
  geom_point() +
  geom_line(aes(y=retag6)) +
  scale_x_continuous(name="Noise level (dBA)") +
  scale_y_continuous(name="Number of phrases") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black")
  )

g6


#3.5. Colocando todos os 4 gr?ficos em 1 figura s?########
#Several plots in one figure using ggplot
require('gridExtra')
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
require(ggpubr)
x11(20,20)
four.graphs = ggarrange(g4, g1, g3, g2,
                        labels = c("A", "C", "B", "D"), 
                        hjust=c(-8.5,-8.5, -8, -8),
                        ncol = 2, nrow = 2,
                        vjust=1.9, align="hv", 
                        font.label = list(size=20, face="bold"),
                        widths = c(1, 1)
)
1
four.graphs
ggsave("Fig3.tiff", width=15, height=25, four.graphs, dpi=1200)
dir()
getwd()

#3.6. Gr?ficos de Trade-Off----
# Correlation between minimum frequency, number of syllables and syllables types, and syllable rate: trade-off entre audibility and song quality
retag7=predict(lm(coereba.salvador.clog10$N_Sil~coereba.salvador.clog10$log.min.freq))
g7=ggplot(coereba.salvador.clog10, aes(y=N_Sil, x=log.min.freq), ylab="Number of syllables", xlab="Minimum frequency (log Hz)") +
  geom_point(size=2) +
  geom_line(size=0.5, aes(y=retag7)) +
  scale_x_continuous(name="Minimum frequency (log Hz)") +
  scale_y_continuous(name="Number of syllables") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=30),
        axis.title=element_text(family ="sans", face="bold", size=30)
  )
g7

retag8=predict(lm(agg.coereba.salvador.clog10$N_Sil_Dif~ agg.coereba.salvador.clog10$log.min.freq))
g8=ggplot(agg.coereba.salvador.clog10, aes(y=N_Sil_Dif, x=log.min.freq)) +
  geom_point(size=2) +
  geom_line(size=0.5, aes(y=retag8)) +
  scale_x_continuous(name="Minimum frequency (log Hz)") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"),
        axis.text=element_text(size=20),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title=element_text(family ="sans", face="bold", size=20)
  )
g8

retag9=predict(lm(agg.coereba.salvador.clog10$N_Sil_Dif~ agg.coereba.salvador.clog10$log.delta.freq))
g9=ggplot(agg.coereba.salvador.clog10) + aes(y=N_Sil_Dif, x=log.delta.freq) +
  geom_point(size=2) +
  geom_line(size=0.5, aes(y=retag9)) +
  scale_x_continuous(name="Frequency bandwidth (log Hz)") +
  scale_y_continuous(name="Number of syllable types") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"), 
        axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text=element_text(size=20),
        axis.title=element_text(family ="sans", face="bold", size=20)
  )
g9

retag10=predict(lm(agg.coereba.salvador.clog10$silabas.s ~ agg.coereba.salvador.clog10$log.min.freq))
g10=ggplot(agg.coereba.salvador.clog10) + aes(y=silabas.s, x=log.min.freq) +
  geom_point(size=2) +
  geom_line(size=0.5, aes(y=retag10)) +
  scale_x_continuous(name="Minimum frequency (log Hz)") +
  scale_y_continuous(name="Syllable rate") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"), 
        axis.title.y=element_text(size=20),
        axis.ticks.y=element_blank(),
        axis.text.y=element_text(size=20),
        axis.text=element_text(size=20),
        axis.title=element_text(family ="sans", face="bold", size=20)
  )
g10

retag11=predict(lm(agg.coereba.salvador.clog10$silabas.s ~ agg.coereba.salvador.clog10$log.delta.freq))
g11=ggplot(agg.coereba.salvador.clog10) + aes(y=silabas.s, x=log.delta.freq) +
  geom_point(size=2) +
  geom_line(size=0.5, aes(y=retag11)) +
  scale_x_continuous(name="Frequency bandwidth (log Hz)") +
  scale_y_continuous(name="Syllable rate") +
  theme_minimal() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        panel.border=element_rect(fill=NA, colour="black", size=0.5),
        axis.line.y=element_line(colour="black"),
        axis.line.x=element_line(colour="black"), 
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.text=element_text(size=20),
        axis.title=element_text(family ="sans", face="bold", size=20)
  )
g11


x11(20,20)

four.graphs = ggarrange(g8, g9, g10, g11,
                        labels = c("A", "C", "B", "D"), hjust=c(-4.5,-3.5),
                        font.label = list(size=20, face="bold"),
                        align="hv", 
                        widths = c(1, 1),
                        ncol = 2, nrow = 2)
four.graphs


ggsave("Fig4.tiff", four.graphs, width=15, height=15, dpi=1200, limitsize = FALSE)

#4.LMM - Fitting linear mixed models for---- 
#4.1. Relationship between noise and song parameters----
summary(coereba.salvador.clog10)
str(coereba.salvador.clog10)

#4.1.1. Log Minimum frequency (Hz)---- 
lmm.low.freq <- lmer(log.min.freq ~ noise + (1|File), 
                     data = coereba.salvador.clog10)
summary(lmm.low.freq)
lmm.low.freq
r.squaredGLMM(lmm.low.freq)

#4.1.2. Log High Frequency (Hz)----
lmm.high.freq <- lmer(log.high.freq ~ noise + (1|File), 
                      data = coereba.salvador.clog10)
summary(lmm.high.freq)
lmm.high.freq
r.squaredGLMM(lmm.high.freq)

#4.1.3. Log10 Frequency badwidth (Hz)----
lmm.delta.freq <- lmer(log.freq.band ~ noise + (1|File), 
                       data = coereba.salvador.clog10)
summary(lmm.delta.freq)
lmm.delta.freq
r.squaredGLMM(lmm.delta.freq)

#4.1.4. Log10 Frequency badwidth (Hz)----
lmm.peak.freq <- lmer(log.peak.freq ~ noise + (1|File), 
                      data = coereba.salvador.clog10)
summary(lmm.peak.freq)
lmm.peak.freq
r.squaredGLMM(lmm.peak.freq)

#4.1.5. Song duration (s)----
lmm.song.dur <- lmer(Delta_Time_s ~ noise + (1|File),
                     na.action = "na.fail", 
                     data = coereba.salvador.clog10)
summary(lmm.song.dur)
lmm.song.dur
r.squaredGLMM(lmm.song.dur)

#4.1.6. Number of syllables----
lmm.n.sil <- glmer (N_Sil ~ noise + (1|File), 
                    family = "poisson", na.action = "na.fail", 
                    data = coereba.salvador.clog10)

summary(lmm.n.sil)
lmm.n.sil
r.squaredGLMM(lmm.n.sil)

#4.1.7. Number of syllable types----
lmm.n.sil.dif <- glmer (N_Sil_Dif ~ noise + (1|File), 
                        family= "poisson", na.action = "na.fail",
                        data = coereba.salvador.clog10)

summary(lmm.n.sil.dif)
r.squaredGLMM(lmm.n.sil.dif)

#4.1.8. Number of phrases----
lmm.n.phrases <- lmer (N_Frases ~ noise +(1|File),
                       na.action = "na.fail",
                       data = coereba.salvador.clog10)
summary(lmm.n.phrases)
r.squaredGLMM(lmm.n.phrases)

#4.1.8. Syllable rate----
lmm.syl.rate <- lmer (silabas.s ~ noise +(1|File),
                      na.action = "na.fail",
                      data = coereba.salvador.clog10)
summary(lmm.syl.rate)
r.squaredGLMM(lmm.syl.rate)

##########################
lm.noise.low.freq= lm(dados.coereba.media.cscore$Low_Freq_Hz ~ dados.coereba.media.cscore$noise)
summary (lm.noise.low.freq)
lm.noise.high.freq= lm(dados.coereba.media.cscore$High_Freq_Hz ~ dados.coereba.media.cscore$noise)
summary (lm.noise.high.freq)
lm.noise.delta.freq= lm(dados.coereba.media.cscore$Delta_Freq_Hz ~ dados.coereba.media.cscore$noise)
summary (lm.noise.delta.freq)
lm.noise.peak.freq= lm(dados.coereba.media.cscore$Peak_Freq_Hz ~ dados.coereba.media.cscore$noise)
summary (lm.noise.peak.freq)
lm.noise.delta.time= lm(dados.coereba.media.cscore$Delta_Time_s ~ dados.coereba.media.cscore$noise)
summary (lm.noise.delta.time)
lm.noise.PC1= lm(dados.coereba.media.cscore$FAC1_s.divsil.srt ~ dados.coereba.media.cscore$noise)
summary (lm.noise.PC1)
lm.noise.PC2= lm(dados.coereba.media.cscore$FAC2_s.divsil.srt ~ dados.coereba.media.cscore$noise)
summary (lm.noise.PC2)
lm.noise.n.syl.dif=lm(dados.coereba.media.cscore$N_Sil_Dif ~ dados.coereba.media.cscore$noise)
summary (lm.noise.n.syl.dif)
lm.noise.n.syl=lm(dados.coereba.media.cscore$N_Sil ~ dados.coereba.media.cscore$noise)
summary (lm.noise.n.syl)
lm.noise.syl.rate=lm(dados.coereba.media.cscore$silabas.s ~ dados.coereba.media.cscore$noise)
summary (lm.noise.syl.rate)
lm.noise.n.phrases=lm(dados.coereba.media.cscore$N_Frases ~ dados.coereba.media.cscore$noise)
summary (lm.noise.n.phrases)

#4.2. Relationship between low frequency and song syllable diversity----
#4.2.1. Log min freq and number of diff sylables----
lmm.low_feq.n.syl.dif= lmer(N_Sil_Dif ~ log.min.freq + song.duration + (1|File), 
                            data = coereba.salvador.clog10,
                            na.action = "na.fail")
summary (lmm.low_feq.n.syl.dif)
lmm.low_feq.n.syl.dif
r.squaredGLMM(lmm.low_feq.n.syl.dif)

#4.2.2. Log high freq and number of diff sylables ----
lmm.high.freq_n.syl.dif= lmer(N_Sil_Dif ~ log.high.freq + song.duration + (1|File), 
                              data = coereba.salvador.clog10,
                              na.action = "na.fail")
summary (lmm.high.freq_n.syl.dif)
lmm.high.freq_n.syl.dif
r.squaredGLMM(lmm.high.freq_n.syl.dif)

#4.2.3. Log delta freq and number of diff sylables ----
lmm.delta.freq_n.syl.dif= lmer(N_Sil_Dif ~ log.delta.freq + song.duration + (1|File), 
                               data = coereba.salvador.clog10,
                               na.action = "na.fail")
summary (lmm.delta.freq_n.syl.dif)
lmm.delta.freq_n.syl.dif
r.squaredGLMM(lmm.delta.freq_n.syl.dif)

#4.2.4. Log low freq and number of sylables----
lmm.low.freq_n.syl=lmer(N_Sil ~ log.min.freq + song.duration + (1|File),
                        data = coereba.salvador.clog10,
                        na.action = "na.fail")

summary (lmm.low.freq_n.syl)
r.squaredGLMM(lmm.low.freq_n.syl)

#4.2.5. High freq and number of sylables----
lmm.high.freq_n.syl=lmer(N_Sil ~ log.high.freq + song.duration + (1|File),
                         data = coereba.salvador.clog10,
                         na.action = "na.fail")

summary (lmm.high.freq_n.syl)
r.squaredGLMM(lmm.high.freq_n.syl)

#4.2.6. Delta freq and number of syllables ---
lmm.delta.freq_n.syl=lmer(N_Sil ~ log.delta.freq + song.duration + (1|File),
                          data = coereba.salvador.clog10,
                          na.action = "na.fail")
summary (lmm.delta.freq_n.syl)
r.squaredGLMM (lmm.delta.freq_n.syl)

#4.2.5. Log minimum freq e number of syllables ---
lmm.min.freq_n.syl=lmer(N_Sil ~ log.min.freq + song.duration + (1|File),
                        data = coereba.salvador.clog10,
                        na.action = "na.fail")
summary (lmm.delta.freq_n.syl)
r.squaredGLMM (lmm.delta.freq_n.syl)


#4.2.6. Minimum Freq and number of phrases ---
lmm.low.freq_num.phrases=lmer(N_Frases ~log.min.freq + song.duration + (1|File),
                              data = coereba.salvador.clog10,
                              na.action = "na.fail")
summary(lmm.low.freq_num.phrases)
r.squaredGLMM(lmm.low.freq_num.phrases)

#4.2.6. Maximum Freq and number of phrases ---
lmm.high.freq_num.phrases=lmer(N_Frases ~  log.high.freq + song.duration + (1|File),
                               data = coereba.salvador.clog10,
                               na.action = "na.fail")
summary(lmm.high.freq_num.phrases)
r.squaredGLMM(lmm.high.freq_num.phrases)

#4.2.7. Delta Freq and number of phrases ---
lmm.delta.freq_num.phrases=lmer(N_Frases ~  log.delta.freq + song.duration + (1|File),
                                data = coereba.salvador.clog10,
                                na.action = "na.fail")
summary(lmm.delta.freq_num.phrases)
r.squaredGLMM(lmm.delta.freq_num.phrases)

#4.2.7. Low Freq and syllable rate ---
lmm.low.freq_syl.rate=lmer(silabas.s ~ log.min.freq + song.duration + (1|File),
                           data = coereba.salvador.clog10,
                           na.action = "na.fail")

summary(lmm.low.freq_syl.rate)
r.squaredGLMM(lmm.low.freq_syl.rate)

#4.2.8. High Freq and syllable rate ---
lmm.high.freq_syl.rate=lmer(silabas.s ~ log.high.freq + song.duration + (1|File),
                            data = coereba.salvador.clog10,
                            na.action = "na.fail")

summary(lmm.high.freq_syl.rate)
r.squaredGLMM(lmm.high.freq_syl.rate)

#4.2.9. Delta Freq and syllable rate ---
lmm.delta.freq_syl.rate=lmer(silabas.s ~ log.delta.freq + song.duration + (1|File),
                             data = coereba.salvador.clog10,
                             na.action = "na.fail")

summary(lmm.delta.freq_syl.rate)
r.squaredGLMM(lmm.delta.freq_syl.rate)


#4.3. Relationship between song syllable diversity and song duration----
#4.3.1. Song duration and number of diff syllables----
lmm.n.syl.dif_song.dur = lmer(N_Sil_Dif ~ Delta_Time_s + (1|File), 
                              data = coereba.salvador.clog10,
                              na.action = "na.fail")
summary (lmm.n.syl.dif_song.dur)
lmm.n.syl.dif_song.dur
r.squaredGLMM(lmm.n.syl.dif_song.dur)

#4.3.2. Song duration and number of syllables----
lmm.n.syl_song.dur = lmer(N_Sil ~ Delta_Time_s + (1|File), 
                          data = coereba.salvador.clog10,
                          na.action = "na.fail")
summary (lmm.n.syl_song.dur)
lmm.n.syl_song.dur
r.squaredGLMM(lmm.n.syl_song.dur)

#4.3.3. Song duration and syllable rate----
lmm.syl.s_song.dur = lmer(silabas.s ~ Delta_Time_s + (1|File), 
                          data = coereba.salvador.clog10,
                          na.action = "na.fail")
summary (lmm.syl.s_song.dur)
lmm.syl.s_song.dur
r.squaredGLMM(lmm.syl.s_song.dur)

#4.3.4. Song duration and number of phrases----
lmm.n.phrases_song.dur = lmer(N_Frases ~ Delta_Time_s + (1|File), 
                              data = coereba.salvador.clog10,
                              na.action = "na.fail")
summary (lmm.n.phrases_song.dur)
lmm.n.phrases_song.dur
r.squaredGLMM(lmm.n.phrases_song.dur)

#####
head(dados.coereba.media.cscore)
var=var(dados.coereba.media.cscore$Low_Freq_Hz)
summary(dados.coereba.media.cscore$Low_Freq_Hz)
var
sd.error=sd(dados.coereba.media.cscore$Low_Freq_Hz)/sqrt(dados.coereba.media.cscore$Low_Freq_Hz)
sd.error
