###########R commands##############

########### General commands#############
R.Version()#Informations on R version.
###########Work directory - organizing work environment###########
getwd()
setwd('/home/youssef/Documents/these/Megaron/details enquete/GIS_Batna1/sphinx results')
rm(list=ls())

###########Installing-calling libraries##############
#install.packages('...','...')
install.packages(c("FactoMineR", "factoextra"))

library (readr)#charge library to read_csv
library(dplyr)
#library(lattice)
#library("FactoMineR")
#library("factoextra")
#library(missMDA)
library(ggplot2)
###########datasets types###########
#variables
#vectors
c()
#matrix
#lists
#tables
#data frames

###########data types###########
#as.character
data<-'1ab'
#as.numeric
data<-12.3
#as.integer()
#as.Date()

###########Creating-assigning data###########
###########Assigning-calling data in data structures########### 
df[,2]
df[2,]
df[c(1:3),]
###deleting data from tables###
df[-1,]#delete row nbre 1
df[,-2]#delete column nbre 2.
df[,-c(1:3)]
ins3<-ins2[,-c(2,4,6:8,11:17,19,21:52)]

#Deleting '0' values

bt2007<-bt7[apply(bt7!=0,1,all),]#delete values using a condition
#condionnal assigning data : ifelse(condition,value if true,value if false)
fq1$endroit<-ifelse(fq1$endroit==3.52,3.78,
                    ifelse(fq1$endroit==7.9,7.84,
                           ifelse(fq1$endroit==8.17,7.80,
                                  ifelse(fq1$endroit==8.31,8.24,
                                         ifelse(fq1$endroit==4.56,4.70,
                                                ifelse(fq1$endroit==6.6,6.73,
                                                       ifelse(fq1$endroit==8.47,8.11,0
                                                       )))))))

#assigning data due to a condition : data that completes that condition
mesumbtw['id'][mesumbtw['id']=='1']<-'1st qrtl'



names(df3)
str(df)
head(df)

###########Reading-Writing data###########
###reading *.csv###
df1<-read.csv('export2.csv',header=T,stringsAsFactors = T,fileEncoding = 'Windows-1256')
###writing csv###
write.csv(ctb3,'vga_chi22.csv')
###producing pdf of plots###
pdf('vgax_sum',title='vgax_sum',h=10,w=7)
par(mfrow=c(5,2))

###read.table and transform it to data frame###
df1 <- read.table(text = as.character(agj_sum$id), sep="_", stringsAsFactors=FALSE)

with(cvgax,plot(Visual.Int,Chi2,
                main=cor(Visual.Int,Chi2)))

dev.off()



###########Manipulating data in data frames###########

###merging-binding vectors-columns-rows in new data frames
#cbind-cbind.data.frame (columns)- rbind-rbind.data.frame(rows)
fq<-cbind.data.frame(as.character(df3$L.endroit),as.character(df3$Identité.spatiale))

smv<-rbind(sint7,sint19,sint3_7,sint3_19,
           sbtw7,sbtw19,sbtw3_7,sbtw3_19,
           sentrpy7,sentrpy19,sentrpy3_7,sentrpy3_19,
           scntv7,scntv19,sclstcf7,sclstcf19,
           scntrl7,scntrl19,scntrllb7,scntrllb19
)

#change column names
colnames(fq)<-c('endroit','identite')
#change row names
rownames(fq)<-c('','')

###aggregating new data using existant from data frames###
ctb2<-aggregate(ctb,by=list(ctb$endroit),FUN = mean)

###merging data frames###
df3<-merge(df1,df2,by='column')

###Subsetting-selecting data through a condition###
h10l33<-subset(h10,n_entity_s>0&n_entrance>0)

###concatenation of coordinates in a new 'newid' variable to allow aggregation of data on spatial basis
join112$newid<-paste(join112$chrightp1,join112$chtopp1,sep="_")

###split concatenated strings using a known separator###
strsplit(agj_sum$id,'_')

###cleaning data###
h8[is.na(h8)]<-0 #converting na data to 0 values.

###deleting Na values from data frames
agj_sum<-na.omit(agj_sum)

#adjust decimals in numeric variables by the command round().
join112$topp1<-mint9+100*round(((join112$top-mint9)/25)/4,0)

###Creating summary table ###
sint7<-with(v7,summary(integhh))

###transposing matrix and data frames : column to rows,rows to columns

smv1<-t(smv)

###set up quantiles##### 
y<-quantile(join13$Unsaf_coun,na.rm = TRUE,probs=0:4/4)

### cut command to divide a range of a data values to intervals and gives them codes### 
q_unsafe <- within(join13, q_unsafe <-as.integer(cut(Unsaf_coun, y, include.lowest=TRUE)))

###melting data : dissoudre les données depuis un tableau large (wide) 
#vers un tableau long en détataillant tous les attributs présents dans 
#le tableau d'origine (large) dans des lignes de tableau différents :
#voir ce lien : https://www.statology.org/melt-in-r/

m.sum_int <- melt(sum_intghh,id.vars='id')

names(m.sum_int)
mesumint<-m.sum_int
mesumint$id<-as.character(mesumint$id)

######Plotting data#########
###boxplot###
with(ax,boxplot(n_entrance~secfeel,
                main="Number of entrances",
                xlab="Year",
                ylab="Number of entrance",
                col="#CD0000",
                border="black"))

###bivariate scatter plot###
with(cvgax,plot(Visual.I_1,Chi2,type='p',
                main=cor(Visual.I_1,Chi2)))

with(gcounttt,plot(log10(Integrat_1),log10(tt_gcount),
                   main=c('R²=',r38),
                   ylab='Space use',
                   xlab='Integration HH R3',
                   col=ifelse(r38>=0.25,'green','black'),
                   abline(lm(log10(tt_gcount)~log10(Integrat_1),data=gcounttt),col='red')))


###Barplots using base ###
mx <- t(as.matrix(min_intghh[-1]))
colnames(mx) <- min_intghh$id
colours = c("Green","red",'gray')
# note the use of ylim to give 30% space for the legend
barplot(mx,main='integration',ylab='Integration', xlab='Quartiles',beside = TRUE, 
        col=colours, ylim=c(0,max(mx)*1.5))
# to add a box around the plot
box()
# add a legend
legend('topright',fill=colours,legend=c('Safe count','Unsafe count', 'Safe-Unsafe count'))




####line/barplot with ggplot#####

int<-ggplot(mesumint,aes(x = id, y = value, colour = variable, group = variable)) + 
  geom_point() + geom_line() +xlab('Security level class')+ylab('Global integration (sum)')+
  theme(legend.position='none',
        axis.text.x = element_text(angle=45,hjust=1,vjust=1,size=7,face=2),axis.text.y = element_text(size=7),
        axis.title.y=element_text(size=9),axis.title.x=element_text(size=9))
int<-int+scale_color_manual(name='',labels = c('Safe count', 'Unsafe count'),
                            values=c("#00CD00", "#EE0000", "#FFA500"))
int



###corplot for PCA and variance analysis###

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)

# Cos2 total des variables sur Dim.1 et Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)

# Colorer en fonction du cos2: qualité de représentation
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Évite le chevauchement de texte
)

####################graphical setup of plots########################
###Base###

par(mfrow=c(4,2),oma=c(0,0,2,0))

#with(gcounttt,plot(log10(Integrat_1),log10(tt_gcount),
#                   main=c('R²=',r38),
#                   ylab='Space use',
#                   xlab='Integration HH R3',
#                   col=ifelse(r38>=0.25,'green','black'),
#                   abline(lm(log10(tt_gcount)~log10(Integrat_1),data=gcounttt),col='red')))


mtext('Correlations between Choice Rn and space use',outer=TRUE,cex=1.5)

#######using grid or patchwork libraries#############
#install.packages("gridExtra")
library(gridExtra)
pdf('scurity_classes_gg3.pdf',onefile = TRUE,width=7,height=10,title = 'Classes security')
grid.arrange(int,int3,cv,btw,btw3,cn,cnlb,cls,fln,ndw,fsi,gsi,nsh,npr, ncol=4, nrow = 5)


dev.off()

#install.packages("patchwork")

library(patchwork)

pdf('scurity_classes_gg56.pdf',onefile = TRUE,width = 7, height = 10,title = 'Classes security')
design<-
  "
ABC#
DE##
FGH#
IJKL
MN##
"
int+int3+cv+btw+btw3+cn+cnlb+cls+fln+ndw+fsi+gsi+nsh+npr+
  plot_layout(design = design)
dev.off()




##########Operations-computing#####################
###arithmetic operations###

join12$topp1<-mint9+100*round(((join12$top-mint9)/25)/4,0)#round to adjust decimals.

####Logarythms####
log()#natural logarythm
dt<-log10(sd_cntrllab)#decimal logarythm
log2()#binary logarythms.

#######Statistical calculations#########
#Coefficient of correlation (by default, pearson coefficient)

r21<-cor(gcount820$Gcount8_20)

###Analysis of variance : Calculating Chisquare###
#Producing contingency table
ctable<-table(df)
ctable
cq<-chisq.test(ctable, simulate.p.value = TRUE)
names(cq)
#Showing data of chisquare computing
cq$statistic
cq$p.value
cq$observed
cq$expected
cq$residuals

### Calculating PCA###

library("FactoMineR")
library("factoextra")

res.pca(join,graph=TRUE)

data(decathlon2)

head(decathlon2)

decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6], 4)



res.pca <- PCA(decathlon2.active, graph = FALSE)


print(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val

fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var

# Coordonnées
head(var$coord)
# Cos2: qualité de répresentation
head(var$cos2)
# Contributions aux composantes principales
head(var$contrib)

# Coordonnées des variables
head(var$coord, 4)

fviz_pca_var(res.pca, col.var = "black")

head(var$cos2, 4)



###calculating MCA###
