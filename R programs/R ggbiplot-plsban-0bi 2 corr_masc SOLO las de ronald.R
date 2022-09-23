#genera modelo que será consdierado en gráfico biplot
#incluye kmedias #final 
#graba en archivo resultados de modelamiento con 104 individuos con colores
# agreda un dataset adicional (entregado por Ronald Criollo) para predicción. 
# 32
 
library(dplyr) 

library(MultBiplotR)



#Lee datos
datos=read.table("C:/Users/jugar/Documents/R  programas/datos ban 03 2019/datos032019.csv",header = FALSE, dec="." ,  sep = ",")


nfila_d2=dim(datos)[1]
ncolum_d2=dim(datos)[2]


datos_add=read.table("C:/Users/jugar/Documents/R  programas/datos ban 03 2019 adicionales/datos_add_03_2019.csv",,header = FALSE, dec="." ,  sep = ",")

nfila_add=dim(datos_add)[1]
ncolum_add=dim(datos_add)[2]

datos_tot= rbind(datos,datos_add) # unifica grupos para centrar y scalar

datosx = datos_tot[,8:527]

datosx=scale(datosx,center = TRUE,scale=TRUE)

datosy = datos_tot[,3]

#separa datos modelo (train)

Xcal= datosx[1:nfila_d2,]
Ycal = datosy[1:nfila_d2]
sev_train= datos_tot[1:nfila_d2,2]

Xpred = datosx[(nfila_d2+1):(nfila_d2+nfila_add),]
Ypred = datosy[(nfila_d2+1):(nfila_d2+nfila_add)]
sev_test = datos_tot[(nfila_d2+1):(nfila_d2+nfila_add),2]

lambda=0.1	
comp=2
PLSban=PLSR1BinFit(Ycal, Xcal,S=2, penalization = lambda)


df.X=PLSban$ScaledX
df.b=as.data.frame(PLSban$YWeights)
df.u = as.data.frame(PLSban$XScores) #scores
df.v <- as.data.frame(PLSban$XLoadings)#direcciones
df.w= as.data.frame(PLSban$XWeights)

names(df.u) <- c('x1var', 'x2var')
names(df.v) <- c('x1var', 'x2var')

# calcula contribuciones
A=as.matrix(df.u)
B=as.matrix(df.v)
Cont=CalculateContributions(df.X,A,B)
RowCon=Cont$RowContributions
ColCon=Cont$ColContributions
TColCon=apply(ColCon,2,sum)

Fit=as.data.frame(Cont$Fit)
CorXA=Cont$Structure



#K-means clustering #####################

set.seed(20)
clusters <- kmeans(df.u, 3)

# agrega grupo

df.u$grupo <- clusters$cluster

# agrega columna train/test 1= train 2= test

tr_ts=c(rep.int(1,nfila_d2)) #training = 1

df.u$tr_ts <- tr_ts

# agrega respuesta y nivel de severidad
df.u$y <- Ycal

df.u$sev <- sev_train

# tabla grupos

grupo = c(1,2,3)
nam_grp= c('Sanas', 'Enfermas1', 'Enfermas2')
t_grupos= as.data.frame(nam_grp)
t_grupos=cbind(grupo,t_grupos)

#tabla train_test tr_ts
tr_ts = c(1,2)
nam_tr_ts= c('train', 'test')
t_tr_ts= as.data.frame(nam_tr_ts)
t_tr_ts=cbind(tr_ts,t_tr_ts)



## completa df.v
var_name=rownames(df.v) 
#un color para espectro visible y uno para infrarrojo

#vgrupos=c(rep("skyblue2",36),rep("skyblue2",41),rep("skyblue2",18),rep("skyblue2",61),rep("skyblue2",9),rep("skyblue2",31),rep("skyblue2",133), rep("tomato1",191))

#colores del espectro.
vgrupos=c(rep("violet",36),rep("blue",41),rep("dodgerblue",18),rep("green1",61),rep("yellow",9),rep("orange",31),rep("tomato2",133), rep("grey50",191))

vbanda=c(rep(1,12), rep(2,316), rep(3,192))

v= as.matrix(df.v, ncol=2) 
#v2=v%*%t(v)
#v=(df.v/v2)/50 
v=as.data.frame(v)
v=cbind(v,vgrupos)
v=cbind(v,vbanda)

#tabla banda

vbanda = c(1,2,3)
nam_banda= c('Ultravioleta', 'Visible Spectrum','Near Infrared')
t_banda= as.data.frame(nam_banda)
t_banda=cbind(vbanda,t_banda)


################
#predicción

B=df.b
B=as.matrix(B)

	
W=df.w
P1=df.v

Xpred=as.matrix(Xpred)
Ypred = as.matrix(Ypred)


S=2
result=list()
I1=dim(Xpred)[1] 
J=dim(Xpred)[2]   
  
if (is.numeric(Ypred)) {Ypred= as.matrix(Ypred)}
I2=dim(Ypred)[1]
K=dim(Ypred)[2]
     
  
if (!(I1==I2)) stop('The number of rows of both matrices must be the same')
I=I1

Xp = Xpred
T=matrix(0, I, S) # #filas #componentes
C=matrix(0, K, S) # columnas de Y #componentes
#P1=matrix(0, J, S) # Columnas de X #componentes
Q=matrix(0, K, S) #  Columnas de Y #componentes
freq=matrix(1,I,1) # 

Xp=matrix(Xp,nrow=I)#arregado con matrix

X1=Xp #inicializa de X1
i=1
w=matrix(W[,1],nrow=J)

t=X1 %*% w # calcula ts con W calibración
 
T[,1]=t
#p=t(Xp) %*% t/ sum(t^2) #calcula p`
#P[,1]=p
p=P1[,1]
X1=X1-t %*% t(p) #calcula residuo
 
i=2 
t=X1 %*% W[,2] # calcula ts con W calibración
 
T[,2]=t
#p=t(Xp) %*% t/ sum(t^2) #calcula pn
#P[,2]=p
p=P1[,2]
X1=X1-t %*% t(p) #calcula residuo

## completa T
unos=matrix(1,I,1)
T=cbind(unos,T)

T= as.matrix(T)


G = T %*%(B)# +const # regresion funcion enlace + constante(revisar)

Px = (exp(G)/(1 + exp(G)))# p(x)
residuals = Ypred - Px
Prediction = as.numeric(Px > 0.5)# discrimina prob >0.5 = 1

#agrega filas a df.u


####          componentes, grupo, train o test, Y salida,  severidad
## para hojas test (amarillo)
#m_pred = cbind(T[,2:3], rep.int(4,nfila_add), rep.int(2,nfila_add), Ypred, rep.int(4,nfila_add))

## para hojas test (con colores de cada severidad)

m_pred = cbind(T[,2:3], rep.int(4,nfila_add), rep.int(2,nfila_add), Ypred, sev_test)




colnames(m_pred) <- colnames(df.u)

#guarda df.u del modelo y lo elimina
 df.modelo = df.u
 rm(df.u)
#

#df.u = rbind(df.u,m_pred) #esta linea agrega a df.u del modelo (no la usamos)

# copia m_pred de las hojas test como df.u

df.u <- m_pred

###### graba información para gráfico predicción

ProbY_pred =cbind(Px, Prediction, Ypred, datos_add[,2])
colnames(ProbY_pred) = c("Px","Pr","y", "sev")

#GRAFICO ROC

#library(PRROC)

#PRROC_obj <- roc.curve(scores.class0 = Prediction, weights.class0=Ypred,curve=TRUE)
#plot(PRROC_obj)

#plot(1:102,ProbY,col=Yin+3,pch=16, xlab='leaf', ylab='Prob. Y')
#title('Prediction Y (CV)')

write.table((ProbY_pred), file = "source(file = "C:/Users/jugar/Documents/R  programas/ProbY_pred.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)


#################
#Xscores
write.table((df.u), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI/df.u.agr.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)
#XLoadings
write.table((v), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI//v.agr.txt", sep=" ",append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)
#YWeigths; coeficientes beta
write.table((df.b), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI/df.b.agr.txt", sep=" ",append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)
#Contributions
write.table((Fit), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI/Fit.agr.txt", sep=" ",append=FALSE, col.names=TRUE,row.names=FALSE, dec=",", quote= FALSE)

#grupos
write.table((t_grupos), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI/t_grupos.agr.txt", sep=" ", append=FALSE, col.names=TRUE,row.names=FALSE, quote= FALSE)

#train o test
write.table((t_tr_ts), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI/t_tr_ts.agr.txt", sep=" ", append=FALSE, col.names=TRUE,row.names=FALSE, quote= FALSE)

#banda
write.table((t_banda), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI/t_banda.agr.txt", sep=" ",append=FALSE, col.names=TRUE,row.names=FALSE, quote= TRUE)



#df.u=read.table("d:/ugarte/R/programas/ggbiplot/ggbiplotBI/df.u.txt")










remove(df.u)
remove(v)
remove(df.b)
remove(Fit)
remove(t_grupos)
