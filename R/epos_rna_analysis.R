################# epos_transcriptomics#####################

#### libraries ####
library(readxl)
library(skimr)
library(RColorBrewer)
library(rstatix)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(gplots)



#### upload data #### 
CL <- read_excel("caratteristiche soggetti.xlsx")
FX <- read_excel("mean_fluxes_Rpers_hume.xlsx", sheet ="meanfluxes_full")
notes <- read_excel("mean_fluxes_Rpers_hume.xlsx",  sheet ="notes")




CL$HOMA <-  CL$`glucose mg/dL 22oct`*CL$`Insulin 22oct`/405
LS_code <- as.factor(CL$`Classi OTTOBRE`)
levels(LS_code) <- c( "MASH F3/4","MASL",  "MASL", "MASH F0/1", "MASH F0/1", "MASH F2", "MASH F3/4"
)

LS_code <- factor(LS_code, levels = levels(LS_code)[c(2,3,4,1)])


EGP <- data.frame( EGP_TOT = FX$HMR_9034_r,
                   GNG =FX$HMR_9034_r -FX$fake_GLYC11_tos*11
                   )
EGP$`%GNG` <- EGP$GNG/EGP$EGP_TOT


is.outlier <- function(x, m, sd, n){
  if (is.na(x))
    return(F)
  else
  {if (x> m + n*sd | x< m - n*sd)
    return(T)
    else
      return(F)}
}


find.outlier <- function(vector, m){
  mean_v <- mean(vector, na.rm = T)
  sd_v <- sd(vector, na.rm = T)
  factor_outlier <- apply(as.array(vector), MARGIN = 1 , is.outlier, m = mean_v, sd = sd_v, n = m )
  return(factor_outlier)
}



mask_outliers <-  apply( as.matrix(EGP)  , MARGIN = 2, find.outlier, m= 3)
EGP[mask_outliers == T] <- NA

mask_outliers <-  apply( as.matrix(CL[, c(14:26, 44)])  , MARGIN = 2, find.outlier, m= 3)
CL[, c(14:26, 44)][mask_outliers == T] <- NA

df <- data.frame("HGP" = EGP$EGP_TOT/CL$`LBM HUME`*1000/60, 
                 "HepIR" = EGP$EGP_TOT*CL$`Insulin 22oct`/CL$`LBM HUME`, 
                 "GNG" = (EGP$EGP_TOT/CL$`LBM HUME`)*EGP$`%GNG`*1000/60,
                 "MASLD" = LS_code, 
                 "Fibrosis" = as.factor(CL$Fibrosis_Stage_Kleiner), 
                 "Steatosis" = as.factor(CL$Steatosis_Grade),
                 "AS" = as.factor(CL$FLIP_Activity_Score), 
                 "T2D" = as.factor(CL$T2DM)
)

levels(df$AS) <-c("0/1","0/1", "2", "3/4", "3/4")
levels(df$Steatosis) <-c("0/1","0/1", "2", "3")
levels(df$Fibrosis) <-c("0/1","0/1", "2", "3/4", "3/4")

#### FIG 2####



###### PVALUES ######
variables <- colnames(df)[1:3]
factors <- colnames(df)[4:7]
table <- lapply(variables, function(i){
  lapply(factors, function(ii){
    lapply(c(1, 2), function(t){
      dff <- data.frame(Var = df[[i]][df$T2D == t], 
                        Factor = df[[ii]][df$T2D == t])
      res <- kruskal.test(dff$Var ~ dff$Factor)$p.value
      pp <- pairwise.wilcox.test(dff$Var, dff$Factor, paired = F, p.adjust.method = "none")
      list(Var = i, 
           Factor = ii, 
           T2D = t, 
           all_pval = res, 
           pairwise = pp)
    })
    
  })
  
})

###### A
ggplot(df, aes(y = HGP, x=MASLD , fill = MASLD)) +
  geom_boxplot()+ geom_point(size = 1) + 
  theme_classic()+ scale_fill_brewer(palette = "OrRd") +
  #   facet_wrap(~Variables, switch = "x", scales = "free" , strip.position = "bottom")  + #ylim(20, 80) + 
  theme(text = element_text(size = 10), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) + 
  ylab("HGP ??mol/min LBM") + 
  xlab("")#  +ylim(0.6, 1.43)

###### B
data <- df[, c(1, 5)]

my_sum <- data %>%
  group_by(Fibrosis) %>%
  dplyr::summarise( 
    n=n(),
    median=median(HGP),
    sd=sd(HGP)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


ggplot(my_sum, aes(y = median, x = Fibrosis)) +
  geom_segment( aes(x= Fibrosis, xend=Fibrosis, y=10, yend= median)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +theme_classic()+
  theme(text = element_text(size = 10)) + ylab("HGP ??mol/min LBM")   +ylim(10, 17)
dev.off()

###### C
data <- df[, c(1, 6)]

my_sum <- data %>%
  group_by(Steatosis) %>%
  dplyr::summarise( 
    n=n(),
    median=median(HGP),
    sd=sd(HGP)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


ggplot(my_sum, aes(y = median, x = Steatosis)) +
  geom_segment( aes(x= Steatosis, xend=Steatosis, y=10, yend= median)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +theme_classic()+
  theme(text = element_text(size = 10)) + ylab("HGP ??mol/min LBM")   +ylim(10, 17)
dev.off()

###### D
data <- df[, c(1, 7)]

my_sum <- data %>%
  group_by(AS) %>%
  dplyr::summarise( 
    n=n(),
    median=median(HGP),
    sd=sd(HGP)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))


ggplot(my_sum, aes(y = median, x = AS)) +
  geom_segment( aes(x= AS, xend=AS, y=10, yend= median)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +theme_classic()+
  theme(text = element_text(size = 10)) + ylab("HGP ??mol/min LBM")   +ylim(10, 17)
dev.off()



##
ggplot(df, aes(y = `HOMA-IR`, x=MASLD , fill = MASLD)) +
  geom_boxplot() + geom_point(size = 1) + 
  theme_classic()+ scale_fill_brewer(palette = "OrRd") +
  theme(text = element_text(size = 10), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("HOMA IR") 

ggplot(df, aes(y = `A-IR`, x=MASLD , fill = MASLD)) +
  geom_boxplot() + geom_point(size = 1) + 
  theme_classic()+ scale_fill_brewer(palette = "OrRd") +
  theme(text = element_text(size = 10), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("A-IR") 

ggplot(df, aes(y = `Hep-IR`, x=MASLD , fill = MASLD)) +
  geom_boxplot() + geom_point(size = 1) + 
  theme_classic()+ scale_fill_brewer(palette = "OrRd") +
  theme(text = element_text(size = 10), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank()) +
  xlab("") +
  ylab("Hep-IR") 

#### FIG 3####
new_code <- interaction(LS_code, CL$T2DM)
levels(new_code) <- c("F0-F1 noT2D", "F0-F1 noT2D", 
                      "F2-F4 noT2D", "F2-F4 noT2D", 
                      "F0-F1 T2D", "F0-F1 T2D", 
                      "F2-F4 T2D", "F2-F4 T2D" 
)


selezionefig2 <- c(2:7, 11:17, 26, 27, 37, 40, 42,  49, 65, 81, 88, 90, 106, 139, 160, 10,  87)
selezionefig2 <- selezionefig2[c(25, 13, 12, 10, 9, 8,
                                 11,24, 7,
                                 27, 
                                 22, 23, 14, 3, 5, 6, 18, 15, 26, 21,  19, 16, 20,  17, 4,  2, 28 ,1)]


rxns <- FX[ , selezionefig2]
rxns$MASLD <- new_code


variables <- colnames(rxns)[-dim(rxns)[2]]
table2 <- lapply(variables, function(x){
  median_rxn <- rxns%>%
    dplyr::group_by(MASLD)%>%
    summarise(x = median(.data[[x]], na.rm = T))
  cbind(Var = rep(x, 4), median_rxn)
})
table2 <- do.call(rbind, table2)%>% pivot_wider(names_from = "MASLD", values_from = "x") 

pvalue <- lapply(variables, function(x){
  pp <- pairwise.wilcox.test(rxns[[x]], rxns$MASLD, paired = F, p.adjust.method = "none")$p.value
  c(x, pp[1,1], pp[3,3])}
)
pvalue <- do.call(rbind, pvalue)
pvalue[, -1] <-  apply(pvalue[, -1], 2, function(x) p.adjust(x, method = "fdr"))



pval_tab <- matrix(NA, dim(pvalue)[1], 4)
heatmap <- table2[, -1]

pval_tab[, 2][pvalue[, 2]<0.1 ] <- "*"
pval_tab[, 2][pvalue[, 2]<0.05 ] <- "**"

pval_tab[, 4][pvalue[, 3]<0.1 ] <- "*"
pval_tab[, 4][pvalue[, 3]<0.05 ] <- "**"



heatmap.2(t(heatmap), Colv = NULL, Rowv= NULL, scale = "col",   trace = "none", cexRow = 0.75, cexCol = 0.5,#srtCol = 30 ,
          col =  paletteer::paletteer_c("grDevices::Temps", n= 60), 
          cellnote = t(pval_tab), notecol = "black",  density.info = "none",  key.title = NA,    lwid=c(1.1,3.5), lhei=c(1,4), keysize=2, key.par = list(cex = 0.5), notecex = 0.7, key.xlab = NA
)

