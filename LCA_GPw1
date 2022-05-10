## Librerías necesarias

pacman::p_load(poLCA,
               psych,
               foreign,
               gdata, # rename vars 
               stargazer,
               xtable,
               lavaan,
               vcd,
               vcdExtra,
               readstata13,
               skimr,
               ggplot2,
               sjPlot,
               sjmisc,
               dplyr,
               texreg, 
               corrplot, 
               RColorBrewer) 

devtools::install_github("jaredhuling/jcolors")

## Cargar base
load("ELSOC_W01_v3.10_R.RData")
ELSOC<-tbl_df(elsoc_2016)
skim(ELSOC)

## Seleccionar y recodificar variables para ACL
ELSOC = elsoc_2016 %>%
  dplyr::mutate (a.Medico        = ifelse(r01_08 > 1, 1, 2)) %>%
  dplyr::mutate (b.Abogado       = ifelse(r01_06 > 1, 1, 2)) %>%
  dplyr::mutate (c.Profesor_univ = ifelse(r01_13 > 1, 1, 2)) %>%
  dplyr::mutate (d.Gerente       = ifelse(r01_01 > 1, 1, 2)) %>%
  dplyr::mutate (e.Secretario    = ifelse(r01_03 > 1, 1, 2)) %>%
  dplyr::mutate (f.Contador      = ifelse(r01_12 > 1, 1, 2)) %>%
  dplyr::mutate (g.Vendedor_tien = ifelse(r01_05 > 1, 1, 2)) %>%
  dplyr::mutate (h.Parvularia    = ifelse(r01_09 > 1, 1, 2)) %>%
  dplyr::mutate (i.Mecánico_aut  = ifelse(r01_04 > 1, 1, 2)) %>%
  dplyr::mutate (j.Camarero      = ifelse(r01_11 > 1, 1, 2)) %>%
  dplyr::mutate (k.Chofer_taxi   = ifelse(r01_10 > 1, 1, 2)) %>%
  dplyr::mutate (l.Vendedor_amb  = ifelse(r01_02 > 1, 1, 2)) %>%
  dplyr::mutate (m.Aseador       = ifelse(r01_07 > 1, 1, 2)) %>%
  dplyr::select (idencuesta, ola, a.Medico, b.Abogado, c.Profesor_univ, d.Gerente, 
                 e.Secretario, f.Contador, g.Vendedor_tien, h.Parvularia, i.Mecánico_aut, 
                 j.Camarero, k.Chofer_taxi, l.Vendedor_amb, m.Aseador)

summary(ELSOC)

## Definir missing values
ELSOC[ELSOC=="-999"] <- NA
ELSOC[ELSOC=="-888"] <- NA

## Análisis de clases latentes 
skim(ELSOC)
plot_likert(ELSOC[3:15], grid.range = 1.2, expand.grid = FALSE, 
            values = "sum.outside", show.prc.sign = TRUE, 
            geom.colors = c("#CC6666", "#66CC99"))


## Eliminar missings
ELSOC=na.omit(ELSOC)
dim(ELSOC)
names(ELSOC)


## Explorar patrones de respuesta
patterns=table(apply(ELSOC[3:15], 1, paste, collapse=""))

patterns
patterns_m=as.data.frame(patterns)
patterns_m <- patterns_m[order(-patterns_m$Freq),] 
patterns_m

## Estimar modelo
f <- cbind(a.Medico, b.Abogado, c.Profesor_univ, d.Gerente, 
           e.Secretario, f.Contador, g.Vendedor_tien, h.Parvularia, i.Mecánico_aut, 
           j.Camarero, k.Chofer_taxi, l.Vendedor_amb, m.Aseador)~1

#help(poLCA)
lca1 <- poLCA(f,ELSOC, nclass=1, graphs=F)
poLCA.entropy(lca1)
set.seed(1234)

lca2 <- poLCA(f,ELSOC,nclass=2,graphs=T)
poLCA.entropy(lca2)

lca3 <- poLCA(f,ELSOC, nclass=3,graphs=T)
poLCA.entropy(lca3)

lca4 <- poLCA(f,ELSOC, nclass=4,graphs=T)
poLCA.entropy(lca4)

lca5 <- poLCA(f,ELSOC, nclass=5,graphs=T)
poLCA.entropy(lca5)

lca6 <- poLCA(f,ELSOC, nclass=6,graphs=T, maxiter=5000)
poLCA.entropy(lca6)

lca7 <- poLCA(f,ELSOC, nclass=7,graphs=T, maxiter=5000)
poLCA.entropy(lca7)

## Entropía relativa (3 clases)
##Numerator:
nume.E <- -sum(lca6$posterior * log(lca6$posterior))
##Denominator (n*log(K)): ## n is a sample size, and K is a number of class
deno.E <- 2927*log(6)
##Relative Entropy
Entro <- 1-(nume.E/deno.E)
Entro

##  Estadísticos de ajuste (todos los modelos)
AIC.1 <-as.numeric(lca1$aic)
AIC.2 <-as.numeric(lca2$aic)
AIC.3 <-as.numeric(lca3$aic)
AIC.4 <-as.numeric(lca4$aic)
AIC.5 <-as.numeric(lca5$aic)
AIC.6 <-as.numeric(lca6$aic)
AIC.7 <-as.numeric(lca7$aic)


BIC.1 <-as.numeric(lca1$bic)
BIC.2 <-as.numeric(lca2$bic)
BIC.3 <-as.numeric(lca3$bic)
BIC.4 <-as.numeric(lca4$bic)
BIC.5 <-as.numeric(lca5$bic)
BIC.6 <-as.numeric(lca6$bic)
BIC.7 <-as.numeric(lca7$bic)


llik.1 <-as.numeric(lca1$llik)
llik.2 <-as.numeric(lca2$llik)
llik.3 <-as.numeric(lca3$llik)
llik.4 <-as.numeric(lca4$llik)
llik.5 <-as.numeric(lca5$llik)
llik.6 <-as.numeric(lca6$llik)
llik.7 <-as.numeric(lca7$llik)


chisq.1 <- as.numeric(lca1$Chisq)
chisq.2 <- as.numeric(lca2$Chisq)
chisq.3 <- as.numeric(lca3$Chisq)
chisq.4 <- as.numeric(lca4$Chisq)
chisq.5 <- as.numeric(lca5$Chisq)
chisq.6 <- as.numeric(lca6$Chisq)
chisq.7 <- as.numeric(lca7$Chisq)


G.1 <- as.numeric(lca1$Gsq)
G.2 <- as.numeric(lca2$Gsq)
G.3 <- as.numeric(lca3$Gsq)
G.4 <- as.numeric(lca4$Gsq)
G.5 <- as.numeric(lca5$Gsq)
G.6 <- as.numeric(lca6$Gsq)
G.7 <- as.numeric(lca7$Gsq)


HQ.1 <- as.numeric(lca1$HQ)
HQ.2 <- as.numeric(lca2$HQ)
HQ.3 <- as.numeric(lca3$HQ)
HQ.4 <- as.numeric(lca4$HQ)
HQ.5 <- as.numeric(lca5$HQ)
HQ.6 <- as.numeric(lca6$HQ)
HQ.7 <- as.numeric(lca7$HQ)


n.obs1 <- as.numeric(lca1$Nobs)
n.obs2 <- as.numeric(lca2$Nobs)
n.obs3 <- as.numeric(lca3$Nobs)
n.obs4 <- as.numeric(lca4$Nobs)
n.obs5 <- as.numeric(lca5$Nobs)
n.obs6 <- as.numeric(lca6$Nobs)
n.obs7 <- as.numeric(lca7$Nobs)


#Creación de Vectores para TABLA DE COMPARACIÓN
AIC <- c(AIC.1, AIC.2,AIC.3,AIC.4,AIC.5, AIC.6, AIC.7)
BIC <- c(BIC.1, BIC.2,BIC.3,BIC.4,BIC.5, BIC.6, BIC.7)
llik <- c(llik.1, llik.2,llik.3,llik.4,llik.5, llik.6, llik.7)
chi.cuadrado <- c(chisq.1, chisq.2,chisq.3,chisq.4,chisq.5, chisq.6, chisq.7)
G2 <- c(G.1, G.2,G.3,G.4,G.5, G.6, G.7)
N <- c(n.obs1, n.obs2,n.obs3,n.obs4,n.obs5, n.obs6, n.obs7)
Modelos <- c("1 clase", "2 clases", "3 clases", "4 clases", "5 clases", "6 clases", "7 clases")

#CREACIÓN TABLA ESTADÍSTICOS DE AJUSTE MODELOS TODAS LAS VARIABLES
fit.indices <- data.frame(Modelos,AIC,BIC,llik,chi.cuadrado,G2,N)
fit.indices

## Gráficar indicadores de ajuste
#Estadísticos de ajuste MODELOS TODAS LAS VARIABLES
fits= fit.indices %>%
  filter(Modelos!="1 clase" & Modelos!="6 clases")

################ OUTPUT table #################
data(fit.indices)
fit.table <- xtable(fit.indices[1:4,])
digits(fit.table)[c(1,4)] <- 0
print(fit.table,floating=FALSE)

help(gather)
results2<-tidyr::gather(fits, index, value, 2:6)
results2

fit.plot<-ggplot(results2) + 
  geom_point(aes(x=Modelos,y=value),size=3) +
  geom_line(aes(Modelos, value, group = 1)) +
  theme_bw()+
  labs(x = "", y="", title = "") + 
  facet_grid(index ~. ,scales = "free") +
  theme_bw(base_size = 16, base_family = "") +   
  theme(panel.grid.major.x = element_blank() ,
        panel.grid.major.y = element_line(colour="grey", size=0.5),
        legend.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        legend.text=  element_text(size=16),
        axis.line = element_line(colour = "black")) 

fit.plot

## Ordenar las clases
#Extraer valores de inicio (modelo seleccionado)
probs.start<-lca3$probs.start
lca3$probs.start

#Reanalizar con gráficos
lca3<-poLCA(f, ELSOC, nclass=3, probs.start=probs.start, graphs=TRUE, na.rm=TRUE, maxiter=3000)

#Reordnar clases, si es necesario
new.probs.start<-poLCA.reorder(probs.start, c(1,3,2))

#Re-estimar para nuevo orden
lca3<-poLCA(f, ELSOC, nclass=3, probs.start=new.probs.start, graphs=TRUE, na.rm=TRUE)

## Representar los grupos
lcmodel <- reshape2::melt(lca3$probs, level=2)
str(lcmodel)
lcmodel

lcmodel= lcmodel %>%
  dplyr::mutate(clase = ifelse(Var1 == "class 1: ", "cerrada", ifelse(Var1 == "class 2: ", "abierta", ifelse(Var1 == "class 3: ", "apatica", "NA"))))

zp1 <- ggplot(lcmodel,aes(x = L2, y = value, fill = Var2))
zp1 <- zp1 + geom_bar(stat = "identity", position = "stack")
zp1 <- zp1 + facet_grid(clase ~ .) 
zp1 <- zp1 + scale_fill_brewer(type="seq", palette="Greys") + theme_bw()
zp1 <- zp1 + labs(x = "Itemes",y="Classes", fill ="") +  theme(text = element_text(size=20))
zp1 <- zp1 + theme( axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),                    
                    panel.grid.major.y=element_blank())
zp1 <- zp1 + guides(fill = guide_legend(reverse=TRUE))
print(zp1)

## Agregar datos
elsoc_lca3<- cbind(ELSOC, "lclass" = lca3$predclass, prob= lca3$posterior)
head(elsoc_lca3)
dim(elsoc_lca3)

elsoc_lca3[elsoc_lca3=="-999"] <- NA
elsoc_lca3[elsoc_lca3=="-888"] <- NA

elsoc_lca3=na.omit(elsoc_lca3)

