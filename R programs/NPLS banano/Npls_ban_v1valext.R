library(raster)
library(gtools)
library(dplyr) 


		
############

# lee datos y realiza predicción con 104 tesis



#Lee datos

datosnpls=read.table("d:/ugarte/datos/datos npls/datanplsb.csv", header = FALSE, dec=".", sep = ",", )

datosnpls_add=read.table("d:/ugarte/datos/datos npls/datanpls_add.csv", header = FALSE, dec=".", sep = ",", )


#new test <- na.omit(datosnpls)# elimina filas con na

#danadas = datosnpls %>% select(V1) %>% filter(datosnpls$V4 ==1)

datosb = filter(datosnpls, datosnpls$V4 ==0) #elimina error en máscara #quedan 100

#malasb =c("Experimento-2017-03-15 17-08-05","Experimento-2017-03-29 19-00-05","Experimento-2017-04-05 18-23-13","Experimento-2017-04-05 18-42-17","Experimento-2017-04-10 17-15-22")
malasb =c("Experimento-2017-03-15 17-08-05","Experimento-2017-03-29 19-00-05","Experimento-2017-04-05 18-23-13","Experimento-2017-04-05 18-42-17")


`%notin%` <- Negate(`%in%`)
datos2b <- datosb %>% filter(V1 %notin% malasb)

#datos2b = datosb[-malas,]

totalnpls <- datos2b %>% group_by(V2) %>% summarize(n=n()) 


# número de componentes

ncom=7

Xce=datos2b[,5:2604]
Xce=scale(Xce,center = TRUE,scale=TRUE) # escalado desv. stand y centrado media
#Xe=scale(X,center = FALSE,scale=apply(X,2,sd))# sin centrar, escalado desv estandar
y=datos2b[,3]
yc=y
#yc=y-mean(y)# centrado media
I=dim(Xce)[1]
J=5
K=520
JK=J*K 

Ig=I
Jg=J
Kg=K
Isobran= I-Ig
tgrp=1
tfil= Ig*(tgrp) #total filas en grupos


#filgrp=matrix(seq(1,tfil), nrow=tgrp, byrow=FALSE) #matriz con el las filas sel para cada grupo

Gp= numeric(0) #inicializa matriz con resultados de G
pressp= numeric(0) #inicializa vector con resultados  de error press

MSE=numeric(0) # matriz error MSE para cada componente por cada prueba


# man#procesa grupos calibración y predicción


Ypplot=numeric(0) # mantiene Yp predicción para plot.


#for (gr in 2:2){ # para crear modelo con grupo 2 seleccionado 
	print("lazo 1")
	Xcal=Xce
	Ycal=yc	
      gpred=0

	#procesa grupo de calibración
	Ical=Ig*1
	Xcal=scale(Xcal) # escalado desv. stand y centrado media
	#Ycal=Ycal-mean(Ycal)# centrado media

	#	plsbancal<-m3plslc(Xg=Xcal,Yg=Ycal,ncom=5,Im=Ical,Jm=Jg,Km=Kg )

	Xg=Xcal
	Yg=Ycal
 	Im=Ical
	Jm=Jg
	Km=Kg

	######### funcion m3plslc Calibración

 	#m3plslc<-function(Xg,Yg,ncom,Im,Jm,Km){

	# inicializa variables

	Ecmin=1000 # error mínimo
	nc=0 # número de comp para error mínimo
	Xres = Xg
	Yres = Yg
	T=numeric(0)
	Gcomp=numeric(0) # guarda G
	Gcomp2=numeric(0)# guarda G2
	W=numeric(0)
	B=numeric(0)
	G=c(rep(0,Im)) #respuesta G para cada componente con b
	G2=c(rep(0,Im)) #respuesta G2 para cada componente con B
	EcompT=0
	EcminT=0
	EcompV=numeric(0) # inicia vector con error total por componente
	EcompT2=0
	EcompV2=numeric(0) # inicia vector con error total por componente
	

	# lazo para ncom



	for (i in 1:ncom){
		Z=matrix(crossprod(Xres,Yres),nrow=5)# matriz de 5 filas a partir del vector (producto Xe transpuesta por yc)
	#	a=svd(Z,nu=1,nv=1)
		a=svd(Z)
		wj=a$u[,1]
		wk=a$v[,1]
		w=t(wk %x% wj)
		#ts=Xres %*% (wk %x% wj)
		#plot(1:104,ts,col=Yres+3,pch=19)
		ts=Xres %*% t(w)
		b<-(solve(t(ts)%*%ts)%*%t(ts))%*%Yres
		Yres<-Yres-ts%*%b # calcula residuo de Y
		G<-G+ts%*%b# calcula nuevos valores de G función enlace logit
				
		Xres<-Xres-ts%*%w
		#	Xres<-matrix(unlist(Xres), ncol = Jm*Km , byrow = TRUE)

			T= matrix(cbind(T,ts), nrow=Im, ncol=i)
			W=matrix(rbind(W,w), nrow=i  , ncol= Jg*Kg   )
		
			B=as.vector(c(B,b))	
			Gcomp = matrix(cbind(Gcomp,G), nrow=Im,ncol=i)
			erry= Yg - T%*%B # vector error por cada y
			Ecomp = sum(erry*erry)/Im# eleva al cuadrada cada valor y suma/ n.muestras 

			G2=T%*%B # funci'n enlace calculada con matriz coef. regresión B
			Gcomp2 = matrix(cbind(Gcomp2,G2), nrow=Im,ncol=i)

			erry2=Yres #tomando residuo directamente
			Ecomp2 = sum(erry2*erry2)/Im# eleva al cuadrada cada valor y suma/ n.muestras 

			# suma errores por componente
			EcompT=EcompT+Ecomp
			EcompV=c(EcompV,EcompT)

			# suma errores por componente considerando residuo Y
			EcompT2=EcompT2+Ecomp2
			EcompV2=c(EcompV2,EcompT2)


		 	#consigue el menor error por componente
			if (Ecomp < Ecmin) {
				Ecmin = Ecomp
				nc = i} # fin lazo if

		} # fin lazo para ncomp

		# Crea matriz de errores
		MSE = matrix(rbind(MSE,EcompV),nrow=1, ncol=ncom)
		

	#} # fin función m3plslc




#******



	print("lazo 2 pred")
	#Crea matriz de predicción validación externa
      
tabsev=datosnpls_add[,1:3]
tabsev=mutate(tabsev, color = datosnpls_add[,2]+10 ) #crea columna color
# Reemplazar color verdadero
tabsev <- tabsev %>% mutate(color = replace(color, color == 10, 3))
tabsev <- tabsev %>% mutate(color = replace(color, color == 11, 5))
tabsev <- tabsev %>% mutate(color = replace(color, color == 12, 4))
tabsev <- tabsev %>% mutate(color = replace(color, color == 13, 2))



	Xp=datosnpls_add[,5:2604]
      Yp=datosnpls_add[,3]
	Ip=dim(Xp)[1]

	Ypplot = matrix(cbind(Ypplot,Yp), nrow=Ip,ncol=1)

	Xpred=scale(Xp) # escalado desv. stand y centrado media
	Ypred=Yp-mean(Yp)# centrado media


#	plsbanpr<-m3plslp(Xg=Xpred,Yg=Ypred,ncom=J,Im=Ip,Jm=J,Km=K)

	Xg=Xpred
	Yg=Ypred
	#ncom=1
	Im=Ip
	Jm=5
	Km=520



######### funcion m3plslp predicción

#m3plslp<-function(Xg,Yg,ncom,Im,Jm,Km){

	# inicializa variables

	EPcmin=1000 # error mínimo
	EP=numeric(0)
	npc=0 # número de comp para error mínimo
	Xres = Xg
	Yres = Yg
	GPcomp=numeric(0)
	G=c(rep(0,Im)) #respuesta G para cada componente
	TP=numeric(0)
	WP=numeric(0)
#	BP=numeric(0)


	# pred lazo para ncomp

	for (i in 1:ncom){
		ts=Xres %*% matrix(W[i,], nrow=Jm*Km)# calcula ts con W calibración
		Yres<-Yres-ts%*%B[i] # calcula residuo de Y con el B cal
		G<-G+ts%*%B[i]# calcula nuevos valores de G función enlace logit
		Xres<-Xres -ts%*%W[i,]
	#	Xres<-matrix(unlist(Xres), ncol = Jm*Km , byrow = TRUE)

		
		TP= matrix(cbind(TP,ts), nrow=Im, ncol=i)
		WP=matrix(rbind(WP,W[i,]), nrow=i  , ncol= Jg*Kg   )
	
#		BP=as.vector(c(BP,b))	
		GPcomp = matrix(cbind(GPcomp,G), nrow=Im,ncol=i)
		print(paste('ncom',i))
		erry= Yg - matrix(TP[,i],nrow=Im, ncol=1)%*%B[i] # vector error por cada y
		EPcomp = sum(erry*erry)/Im# eleva al cuadrada cada valor y suma/ n.muestras 

	} # fin lazo para ncomp

	EP=as.vector(c(EP,EPcomp))	# guarda error prediccion por componente

#} # fin función m3plslp



#### fin función


#} #FIN LAZO PRINCIPAL (PRUEBAS)


### error calibración

print(MSE)


Labelcom=character(0)
for(i in 1:ncom){
	Labelcom=c(Labelcom,paste("comp"),i)
}

Labelpru=character(0)
for(i in 1:tgrp){
	Labelpru=c(Labelpru,paste("prueba"),i)
}


E_com=apply(MSE,2,sum)
E_pru=apply(MSE,1,sum)


#ycolor=Yp
ycolor=tabsev$color
Ypg=exp(GPcomp[,ncom])/(1+exp(GPcomp[,ncom]))
titulo=paste("NPLS Ext. Validation")
#jpeg("d:/ugarte/datos/datos npls/ncom1.jpg")
plot(1:Im,Ypg,main=titulo,xlab='leaves', ylab='Y Prob. prediction',pch=21,bg=ycolor)
abline(h=0.5, lty=2, col="black")
legend("bottomleft",legend=c("Healthy","Pre-symp", "Sev 1", "Sev 2"), col=c("green", "turquoise","blue","red"),pch=19)


#dev.off()



#Matriz de confusion
Pred = as.numeric(Ypg > 0.5)# discrimina prob >0.5 = 1


table(Yp,Pred)



## curva ROC y AUC


library(PRROC)

PRROC_obj <- roc.curve(scores.class0 = Pred, weights.class0=Yp,curve=TRUE)
plot(PRROC_obj)


library(cvAUC)

auc_npls <- AUC(Pred, Yp)



#### fin programa




