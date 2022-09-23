#Probar genera datos para predicción
#elimina errores en imágenes sanas y por error en máscara

#Leave-one-out cross-validation (LOOCV) validación cruzada dejando uno afuera

#grabar archivo ggplot2

library(devtools) 
library(MultBiplotR)
library(dplyr) 
# lee datos y realiza validación cruzada


datos=read.table("C:/Users/jugar/Documents/R  programas/datos ban 03 2019/datos032019.csv",header = FALSE, dec="." ,  sep = ",")

Xm = datos[,8:527]
y = datos[,3]


##### validación con toda la matriz

Xin=Xm
Xin=scale(Xin,center = TRUE,scale=TRUE)

nfila=dim(Xin)[1]
ncolum=dim(Xin)[2]

mat_Pxcv1=numeric(0)
mat_predcv1=numeric(0)

Yin=y

#lseq= seq(0.1,0.9, by= 0.1)

ncom = 7

lseq =0.1


for (lambda in lseq){


 MYp = numeric(0)
 Ypr = numeric(0)


 for(m in 1:nfila){# crea matriz de calibración
  		Xcal=numeric(0)
		Ycal=numeric(0)
      	Xpred=numeric(0)
      	Ypred=numeric(0)
      	gpred=0


		Xcal=Xin[-m,]
		Ycal=Yin[-m]
		Xpred=Xin[m,]
		Ypred=Yin[m]

		PLSbanano=PLSR1BinFit(Ycal, Xcal,S=ncom, penalization = lambda)
      	B=PLSbanano$YWeights
		B=matrix(B,ncol=1)
		# calculo de constante revisar xq los valores T son mayores que 1
		#T1=c(1,1,1) #damos valor 1 a cada X 
		#const=1-T1%*%B	
    		
		W=PLSbanano$XWeights
		P1=PLSbanano$XLoadings


  		Xpred=matrix(Xpred,nrow=1)
		S=ncom
  		result=list()
  		I1=dim(Xpred)[1] 
  		J=dim(Xpred)[2]   
  
		if (is.numeric(Ypred)) {Ypred= as.matrix(Ypred)}
  		I2=dim(Ypred)[1]
  		K=dim(Ypred)[2]
     
  
  		if (!(I1==I2)) stop('The number of rows of both matrices must be the same')
  		
		I=I1

    		#Xp=scale(Xpred) #scalado y centrado
             Xp = Xpred
  		T=matrix(0, I, S) # #filas #componentes
  		C=matrix(0, K, S) # columnas de Y #componentes
  		#P1=matrix(0, J, S) # Columnas de X #componentes
  		Q=matrix(0, K, S) #  Columnas de Y #componentes
  		freq=matrix(1,I,1) # 

		Xp=matrix(Xp,nrow=I)#arregado con matrix

  		X1=Xp #inicializa de X1
	
		
		for (i in 1:ncom) {
            w=matrix(W[,i],nrow=J)

		t=X1 %*% w # calcula ts con W calibración
 
    		T[,i]=t
    		#p=t(Xp) %*% t/ sum(t^2) #calcula p`
    		#P[,1]=p
		p=P1[,i]
    		X1=X1-t %*% t(p) #calcula residuo
   		}
   		
		## completa T
		unos=matrix(1,I,1)
		T=cbind(unos,T)


   		G = T %*%(B)# +const # regresion funcion enlace + constante(revisar)

  		Px = (exp(G)/(1 + exp(G)))# p(x)
  		residuals = Ypred - Px
  		Prediction = as.numeric(Px > 0.5)# discrimina prob >0.5 = 1


  		#resultados
       	MYp=c(MYp,Px)
            Ypr=c(Ypr,Prediction)

 }
	

(MC    <- table(Yin,Ypr) )  # 

#aciertos
#aciertos=(MC[1,1]+MC[2,2])/sum(MC)*100
#errores=(MC[1,2]+MC[2,1])/sum(MC)*100
#print("%aciertos")
#print(aciertos)
#print("% errores")
#print(errores)


#### fin programa


 mat_Pxcv1=cbind(mat_Pxcv1,MYp)
 mat_predcv1=cbind(mat_predcv1,Ypr)

}
#colnames(mat_Pxcv1)=paste(rep("l-",4),lseq, sep="")
#colnames(mat_predcv1)=paste(rep("l-",4),lseq, sep="")



ProbY =cbind(mat_Pxcv1, mat_predcv1, y, datos2[,2])
colnames(ProbY) = c("Px","Pr","y", "sev")

plot(1:104,ProbY[,1],col=Yin+3,pch=16, xlab='leaf', ylab='Prob. Y')
#title('Prediction Y (CV)')

write.table((ProbY), file = "C:/Users/jugar/Documents/R  programas/ggbiplotBI/ProbYcorr&masc.txt", sep=" ", append=FALSE, col.names=TRUE, row.names=FALSE, dec=",", quote= FALSE)

