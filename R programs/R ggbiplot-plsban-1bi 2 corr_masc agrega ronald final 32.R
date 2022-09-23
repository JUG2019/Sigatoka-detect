 # con dataset val ext 

#install.packages("tidyverse")
#library(tidyverse)
#library(extrafont)
#loadfonts(device = "win")
#font_import()
library(ggplot2)
#library(ggforce)
#library(ggthemes)
library(dplyr)
library(scales)
library(grid)
#library(devtools)
 
library(showtext)
#font_add("times", regular = "times.ttf", bold = "timesbd.ttf")
#showtext_auto()


df.u=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/df.u.agr.txt",header =TRUE, dec="," , sep = " ")
v=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/v.agr.txt",header = TRUE, dec=",", sep = " ")
df.b=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/df.b.agr.txt",header = TRUE, dec="," , sep = " ")
Fit=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/Fit.agr.txt",header = TRUE, dec="," , sep = " ")
t_grupos=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/t_grupos.agr.txt", header = TRUE, dec="," , sep = " ")
t_banda=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/t_banda.agr.txt", header = TRUE, dec="," , sep = " ")
t_tr_ts=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/t_tr_ts.agr.txt",header = TRUE, dec="," ,  sep = " ")
 
#y=c(rep(0,11),rep(1,91))

y = df.u$y  #


#contributions
Fit1=round(Fit[1,1]*100, digits=2)
Fit2=round(Fit[2,1]*100, digits=2)


#names

#names(df.u) <- c('x1var', 'x2var')
#names(df.v) <- names(df.u)
#names(df.b) <- "Beta"
#rownames(df.b) <- c('const','Comp 1', 'Comp 2')

#names(Fit) <- "Percentage"
#rownames(Fit) <- c('x1var', 'x2var')

choices = 1:2
#circle.prob=0.69
#u.axis.labs <- paste('PLS component ', choices, sep='')
u.axis.labs <- paste('PLS component ', choices, sep=' ')



#K-means clustering #####################

#set.seed(20)
#clusters <- kmeans(df.u, 3)
#df.u$groups <- clusters$cluster

clusters <- df.u$grupo

nsev <- df.u$sev

#Agrega numero de hojas

numhoja= seq(1:length(y)) # se numeran 

 # Base plot
  
#  g <- ggplot(data = df.u, aes(x = x1var, y = x2var, color=factor(clusters), label=numhoja)) + xlab(paste(u.axis.labs[1]," ",Fit1,"%")) + ylab(paste(u.axis.labs[2]," ",Fit2,"%"))  

#  g <- ggplot(data = df.u, aes(x = x1var, y = x2var, color=factor(nsev),label=numhoja )) + xlab(paste(u.axis.labs[1]," ",Fit1,"% explained var.")) + ylab(paste(u.axis.labs[2]," ",Fit2,"% explained var.")) + 

  g <- ggplot(data = df.u, aes(x = x1var, y = x2var, color=factor(nsev),label=numhoja )) + xlab(u.axis.labs[1]) + ylab(u.axis.labs[2]) + 

     labs(colour="Banana\nleaves") + coord_fixed(xlim = c(-60, 40), ylim = c(-30, 30))+
#    scale_colour_manual(label= c( "Healthy", "Presymptomatic", "Severity 1", "Severity 2"),values = c("mediumblue", "green3" , "red","gold" ))
    scale_colour_manual(label= c( "Non-infected", "Pre-symptomatic", "Severity 1", "Severity 2","Test leaves"),values = c("green4","turquoise1","blue4", "red3", "Yellow"))

   g=g + guides(shape=FALSE)

  #g <- g+ ggtitle("HYPERSPECTRAL-BIPLOT")
# windowsFonts(Times=windowsFont("TT Times New Roman"))
# windowsFonts(Arial=windowsFont("Arial Unicode MS"))

  g<-g+theme( title=element_text(size=16,face="bold", family="times"),
         plot.title=element_text(hjust=0.5),
         axis.text=element_text(size=10),
        axis.title=element_text(size=12, hjust=0.5, family="times"),
         legend.text = element_text( size = 13),
	   legend.title =element_text( size = 14))
   
# show varibles 

#var_name=rownames(df.v) 
#vgrupos=c(rep("lightsteelblue2",12),rep("lightsteelblue2",100),rep("skyblue2",100),rep("dodgerblue2",116),rep("lightsalmon1",60),rep("salmon2",60),rep("tomato1",72))
#v=as.matrix(df.v) 
#v2=v%*%t(v)
#v=(df.v/v2)/50 
#v=as.data.frame(v)
#v=cbind(v,vgrupos)
 
g <- g +   geom_abline(intercept=0, slope=v[,2]/v[,1], size=0.25,color=v$vgrupos, alpha=2/3)
   
# ejes
   g= g+geom_hline(yintercept=0, color = "black", size=1,alpha=3/4, linetype="dashed")+
       geom_vline(xintercept = 0, color = "black", size=1,alpha=3/4, linetype="dashed")
# puntos
  g= g+ geom_point(alpha=1, aes(shape=factor(y)), size=3, show.legend = TRUE)#+
#  g= g+ geom_text(aes(label=ifelse(numhoja>0&numhoja<105,as.character(numhoja),'')), size = 5,hjust=-1,vjust=0)
  g= g+ geom_text(aes(label=ifelse(numhoja==13|numhoja==15|numhoja==20,as.character(numhoja),'')), size = 7,hjust=1.2,vjust=0.5)
# Draw linea prob =0.5 
   g <- g + 
      geom_abline(intercept=(-df.b[1,])/df.b[3,], slope=-(df.b[2,]/df.b[3,])
              , color='black', size=2, linetype="dotted") +
       annotate("text", label = "P = 0.5" , x = -15, y = 7  , size = 3.5, colour = "black" )

# dibuja elipses #########################  

#g<- g+ stat_ellipse(data=df.u[,1:2],level = 0.99, size=0.7)

# texto spectro
#     g<-g + annotate("text", x = -50, y = 20, label = "VISIBLESPECTRUM 400-750nm",colour = "black",angle=0,,fontface="bold",  size=4)+
#            annotate("text", x = 7, y = 25, label = "NEARINFRARED 750-2500nm",colour = "black",angle=0,,fontface="bold", size=4)

 print(g)


#install.packages("export")
#library(export)



#setEPS() 


#g 


#dev.off()

ggsave("C:/Users/jugar/Documents/R  programas/resumenes/fighsbiplot32num2.png", width = 12, height = 7, dpi = 600, device ="png")
#ggsave("D:/ugarte/usal/RIP/estudio doctoral/ACADEMICO USAL//tesis/figuras/figura biplot err", dpi = 600, device ="png")

