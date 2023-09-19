library(readxl)
library(stringr)
library(data.table)
library(ggpubr)
library(rstatix)
library(ggplot2)
library(agricolae)
library(tidyverse)

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

dfgaztest <- dfgaz[!dfgaz$sujet %in% c("SP" , "MD" , "MA") ,  ]
dfgaztest <- dfgaztest[dfgaztest$instant %in% c("Pre" , "Post"), ]

test <- dfgaz[dfgaz$instant %in% c("Pre", "Post"), ]

#séparation des données et enlever les sujets problème manip


test1 <- test[test$condition == "Patch" , ]
test1 <- test1[!test1$sujet %in% c("SP" , "RF") , ]

test2 <- test[test$condition == "Placebo" , ]
test2 <- test2[!test2$sujet %in% c("SP" , "RF") ,]

test6 <- test[!test$sujet %in% c("SP" , "RF") ,]

test6$instant <- factor(test6$instant , levels = c("Pre" , "Post"))
dfgaztest$instant <- factor(dfgaztest$instant , levels = c("Pre" , "Post"))

mycomparaison <-
  list(c("Patch_Pre" , "Patch_Post") ,
       c("Placebo_Pre" , "Placebo_Post") ,
       c("Patch_Pre" , "Placebo_Pre") ,
       c("Patch_Post" , "Placebo_Post"))

#dfgaztest <-
 # mutate(instantcondition = paste0(dfgaztest$condition, "_" , dfgaztest$instant))

plotCE <- ggboxplot(
  test6 , width = 0.6 ,
  x = "instant",
  y = "ECR",
  color = "condition",
  palette = c("#00AFBB" , "#FC4E07"),
  order = c(
    "Pre" ,
    "Post" #,
    #"Post48h"
  ),
  add = "jitter" , size = 0.6 , shape = "condition" ,
  ylab = "ECR",
  xlab = "instant_mesure" ,
  title = "ECR_patch_placebo" , #facet.by = "condition"
) +
  stat_summary(
    geom = "point",
    fun.y = mean , aes(group = condition) ,
    shape = 20 ,
    size = 4 ,
    position = position_dodge2(width = 0.75,
                               preserve = "single")
  ) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold"),
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  )

plotCE

#plot indiv

plotindiv1 <- ggplot(test6, aes( x = instant , y = ECR )) +
  theme_bw() + theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    strip.background = element_rect(color = "black" , fill = "#373737")
    ,
    strip.text = element_text(
      color = "white" ,
      face = "bold" ,
      size = 8
    ) ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  geom_line(
    aes(
      x = instant ,
      group = sujet ,
      color = as.factor(sujet)
    ) ,
    size = 0.7 ,
    position = "identity" ,
    linetype = "dashed"
  ) +
  geom_point(
    aes(x = instant , group = sujet),
    shape = 21,
    colour = "black",
    size = 2,
    position = "identity"
  ) +
  geom_boxplot( outlier.shape = NA, coef = 0 ,
    aes(x = instant , y = ECR) ,
    width = .35,
    fill = "white" , alpha = 0.3
  )  +
  scale_color_manual(
    values = c(
                "purple" ,
                "#0416f5" ,
                "#b00000" ,
                "#19a3e8" ,
                "#fd4c4c" ,
                "#E7B800" ,
                "#5ef11a" ,
                "#c58ede" ,
                "#3e020b" ,
                "#febd02" ,
                "#16161e" ,
                "#24844b" ,
                "#f604fd" ,
                "#439bab" ,
                "#c5c896" ,
                "#6e711d" ,
                "#109c02" #,
                #"#b71385"
    )) +
  labs(color = "sujet") +
  stat_summary(
    geom = "errorbar" ,
    fun.data = mean_sd ,
    colour = "black" ,
    size = 1 ,
    width = 0.2
  ) +
  stat_summary(
    fun = mean,
    shape = 17 ,
    size = 1 ,
    position = "identity",
    color = "#ff0000"
  ) +
  labs(title = "ECR_individual_variation_PRE_POST") +
  facet_wrap(vars(condition) , scales = "free_y")

plotindiv1


shapiro_test(test6$ECR)
identify_outliers(test1 , ECR , variable = NULL)
identify_outliers(test2 , ECR ,  variable = NULL)

qqplotnorm <- ggqqplot(test6, "ECR", ggtheme = theme_bw()) +
  facet_grid(instant ~ condition , labeller = "label_both")

qqplotnorm

res.aov1 <- rstatix::anova_test(
  data = test6 , dv = ECR , wid = sujet ,
  within = c(condition, instant) , effect.size = "ges",
  detailed = TRUE,
)

get_anova_table(res.aov1 , correction = "auto")

one.way <- test6 %>%
  group_by(instant) %>%
  anova_test(dv = ECR, wid = sujet, within = condition) %>%
  get_anova_table() %>%
  adjust_pvalue(method = "bonferroni")
one.way

pwc <- test6 %>%
  group_by(instant) %>%
  pairwise_t_test(
    ECR ~ condition, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
pwc

#analyse sans le post48

ttesttime <- test6 %>%
  group_by(condition) %>%
  pairwise_t_test(
    ECR ~ instant, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime

stat.test <- test1 %>%
  t_test(ECR ~ instant , paired = TRUE) %>%
  add_significance()
stat.test

ttestinstantmesure <- test6 %>%
  group_by(instant) %>%
  pairwise_t_test(
    ECR ~ condition , paired = TRUE,
    p.adjust.method = "bonferroni"
  )

ttestinstantmesure

#ajout pvalue plot

ttestinstantmesure <- ttestinstantmesure %>% add_xy_position(x = "condition")
ttesttime <- ttesttime %>% add_xy_position(x = "instant")

ttesttime$xmin <- c(1.8 , 2.18)
ttesttime$xmax <- c(0.8 , 1.18)

ttestinstantmesure$xmin <- c(1 , 2)
ttestinstantmesure$xmax <- c(1 , 2)

ttesttime1 <- ttesttime[ttesttime$condition == "Patch" , ]
ttesttime2 <- ttesttime[ttesttime$condition == "Placebo" , ]

plotCE +
  stat_pvalue_manual(
  ttestinstantmesure,
  tip.length = 0 ,
  hide.ns = FALSE ,
  label = "p = {p.adj}"  , y.position = c(5.75 , 5.75)
) +
  stat_pvalue_manual(
    ttesttime1,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = 5.4 , color = "#00AFBB"
  ) +
  stat_pvalue_manual(
    ttesttime2,
    tip.length = 0 ,
    hide.ns = FALSE ,
    label = "p = {p.adj}"  , y.position = 7.1  , color = "#FC4E07"
  )

#Analyse sans le PRE

test3 <- dfgaztest[!dfgaztest$instant == "Pre", ]

ttesttime3 <- test3 %>%
  group_by(condition) %>%
  pairwise_t_test(
    ECR ~ instant, paired = TRUE,
    p.adjust.method = "bonferroni"
  )
ttesttime3
