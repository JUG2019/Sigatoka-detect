
#Leave-one-out cross-validation (LOOCV) validación cruzada dejando uno afuera
# ggplot predicción datos corregidos
# egalley

library(ggplot2)
#windowsFonts(Times=windowsFont("TT Times New Roman"))
#windowsFonts(Arial=windowsFont("Arial Unicode MS"))
#library(showtext)
#font_add("times", regular = "times.ttf", bold = "timesbd.ttf")
#showtext_auto()

ProbY=read.table("C:/Users/jugar/Documents/R  programas/ggbiplotBI/ProbYcorr&masc.txt",header =TRUE, dec= ",",sep = " ")


Yin=ProbY[,3]
sev=ProbY[,4]
mat_Px=as.data.frame(ProbY)
num_filas= dim(ProbY)[1]

ggplot(data = mat_Px) + 
geom_point(mapping = aes(x = c(1:num_filas), y = mat_Px$Px, color= factor(sev)), size=2.5, show.legend = TRUE)+
    xlab("Leaves")+  ylab("Y probability") + 
    labs(colour="Banana leaves") +
    scale_colour_manual( label= c("Non-infected","Presymptomatic", "Severity 1","Severity 2"),values = c("green4","turquoise1", "blue","red"))+ 
    guides(shape=TRUE)+
 #   ggtitle("Y PREDICTION (CV)")+
    theme( title=element_text(size=12,face="bold", family="times"),
         plot.title=element_text(hjust=0.5),
         axis.text=element_text(size=14, family ="times"),
  #        axis.text=element_blank(),
        axis.title=element_text(size=14, hjust=0.5, family="times"),
         legend.text = element_text( size = 14, family="times"),
	   legend.title =element_text( size = 16, family ="times"))	+
#		legend.position = "botton")+

	geom_hline(yintercept=0.5, linetype="dashed", color = "red", size=1)




#setEPS()

ggsave("D:/ugarte/usal/RIP/estudio doctoral/ACADEMICO USAL/resumenes/fig8cvegalley1.png", dpi = 500, device ="png")
  

postscript("plot.eps")
print(p)
dev.off()


setEPS()
postscript("test.eps")
par(family = "times")
plot(rnorm(100), main = "Test Font")
dev.off()



