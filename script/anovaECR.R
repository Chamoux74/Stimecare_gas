library(readxl)
library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)

dfgaz <- read_xlsx("C:/Users/maxch/Git/GAS_Exchanges/data/classeur1.xlsx")
colnames(dfgaz) <-
  c("sujet" ,
    "condition" ,
    "instant" ,
    "VO2base" ,
    "VO2" ,
    "eqO2" ,
    "ECR" ,
    "OCR")

dfgaz <- dfgaz[,-c(9, 10)]
dfgaz$ECR <- as.numeric(dfgaz$ECR)

plotCE <- ggboxplot(
  dfgaz ,
  x = "instant",
  y = "ECR",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "Pre" ,
    "Post" ,
    "Post48h"
  ),
  add = "jitter" ,
  ylab = "ECR",
  xlab = "instant_mesure" ,
  title = "ECR_patch_placebo"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd , aes(group = condition) ,
    colour = "grey" ,
    linetype = "dotted" ,
    size = 1 , position = position_dodge2(width = 0.75,
                                          preserve = "single")
  )

plotCE


res.aov1 <- rstatix::anova_test(
  data = dfgaz , dv = ECR , wid = sujet ,
  within = c(condition, instant) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

res.aov1 <- res.aov1 %>% add_xy_position(x = "instant_mesure")
ttesttime$xmin <- c(2 , 2 , 2 , 3 , 3 , 4 , 2 , 2 , 2 , 3 , 3 , 4)
ttesttime$xmax <- c(3 , 4 , 1 , 4 , 1 , 1, 3 , 4 , 1 , 4 , 1 , 1)

plotCE +
  stat_pvalue_manual(
    ttesttime,
    tip.length = 0 ,
    hide.ns = TRUE ,
    label = "p = {p.adj}"  , y.position = , color = "#FC4E07"
  )

ttesttime <- dfgaz %>%
  group_by(condition) %>%
  pairwise_t_test(
    ECR ~ instant, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

ttestrfdinstantmesure <- dfgaz %>%
  pairwise_t_test(
    rfd ~ instant_mesure , paired = TRUE,
    p.adjust.method = "bonferroni"
  )
