####################### epos flux data analysis #######################

#### libraries ####
library(readxl)
library(skimr)
library(RColorBrewer)
library(rstatix)
library(stringr)
library(ggplot2)
library(dplyr)


#### upload data #### 
EPOS_glycine_GSMM <-read_excel("EPOS-flux tracer 80subjs.xlsx")

EPOS_glycine_GSMM$code_GSMM <- EPOS_glycine_GSMM$code_GSMM %>% as.factor()
levels(EPOS_glycine_GSMM$code_GSMM) <- c("MASL", "MASH F0/F1", "MASH F2", "MASH F3/F4")


#### FIG 1 _ 1 ####
df <- data.frame("HGP" = (EPOS_glycine_GSMM$`EGP umol/min LBM`), 
                 "MASLD" = EPOS_glycine_GSMM$code_GSMM, 
                 "Fibrosis" = as.factor( EPOS_glycine_GSMM$`SCORE FIBROSIS`), 
                 "Steatosis" = as.factor( EPOS_glycine_GSMM$steatosi),
                 "AS" = as.factor(EPOS_glycine_GSMM$`activity score`)
)
levels(df$AS) <-c("0/1","0/1", "2", "3/4", "3/4")
levels(df$Fibrosis) <-c("0/1","0/1", "2", "3/4", "3/4")


###### PVALUES ######
variables <- colnames(df)[-1]

table <- lapply(variables, function(i){
  dff <- data.frame(HGP = df$HGP, 
                    Var = df[[i]])
  res <- kruskal.test(dff$HGP ~ dff$Var)$p.value
  pp <- pairwise.wilcox.test(dff$HGP, dff$Var, paired = F, p.adjust.method = "none")
  list(Var = i, 
       all_pval = res, 
       pairwise = pp
  )
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
data <- df[, c(1, 3)]

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
data <- df[, c(1, 4)]

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
data <- df[, c(1, 5)]

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



#### FIG 1 _ 2 ####
df <- data.frame("Hep-IR" = (EPOS_glycine_GSMM$`EGP umol/min LBM`*EPOS_glycine_GSMM$`INSULIN ok mU/l`), 
                 "A-IR" = EPOS_glycine_GSMM$`Adipo IR ok`, 
                 "HOMA-IR" = EPOS_glycine_GSMM$`HOMA ok`
                 "MASLD" = EPOS_glycine_GSMM$code_GSMM
                 )
)


###### PVALUES ######
variables <- colnames(df)[-4]

table2 <- lapply(variables, function(i){
  dff <- data.frame(MASLD = df$MASLD, 
                    Var = df[[i]])
  res <- kruskal.test(dff$Var ~ dff$MASLD)$p.value
  pp <- pairwise.wilcox.test(dff$Var, dff$MASLD, paired = F, p.adjust.method = "none")
  list(Var = i, 
       all_pval = res, 
       pairwise = pp
  )
})

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


#### FIG 1 _ 3 #### 


df <- data.frame("HGP" = EPOS_glycine_GSMM$`EGPumol/minnew`,  
                 "Lipolysis" = EPOS_glycine_GSMM$`Raglycumol/minnew`,
                 "A-IR" = EPOS_glycine_GSMM$`Adipo IR ok`, 
                 "HOMA-IR" = EPOS_glycine_GSMM$`HOMA ok`, 
                 "MASLD" = EPOS_glycine_GSMM$code_GSMM)

###### PVALUES ######

variables <- colnames(df)[2:4]

table3 <- lapply(variables, function(i){
  dff <- data.frame(HGP = df$HGP, 
                    Var = df[[i]])
  res <- lm(log(Var)~log(HGP), data = dff)%>% summary()
  list(Var = i, 
       summary_lm = res  )
})


ggplot(df, aes(x = log(Lipolysis), y=log(HGP))) +
  geom_point(aes(fill= MASLD), size = 3, color = "black", shape = 21) + 
  stat_smooth(method = "lm", colour = "red") + 
  theme_classic()+ scale_fill_brewer(palette = "OrRd") +
  theme(text = element_text(size = 18), 
        axis.text.x = element_text(angle = 30, hjust = 1)) + 
  ylab("log HGP ??mol/min")+ 
  xlab("log Lipolysis ??mol/min") #+ ylim(3, 6.77 )
dev.off()

