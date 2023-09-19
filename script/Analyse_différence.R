library(tidyr)
library(dplyr)
library(rstatix)
library(ggplot2)
library(ggpubr)

#analyse différence

preplacebo <-
  test2 %>%  filter(instant == "Pre") %>% select(ECR)

prepatch <-
  test1 %>%  filter(instant == "Pre") %>% select(ECR)

postplacebo <-
  test2 %>%  filter(instant == "Post") %>% select(ECR)

postpatch <-
  test1 %>% filter(instant == "Post") %>% select(ECR)

dfprepost <- cbind(preplacebo, prepatch, postplacebo, postpatch)
colnames(dfprepost) <- c("ECR_PRE_PB", "ECR_PRE_P" , "ECR_POST_PB", "ECR_POST_P")

dfprepostpb <- mutate(dfprepost , PRE_POST_PB = (ECR_POST_PB - ECR_PRE_PB)/ECR_PRE_PB*100) %>% select(PRE_POST_PB)
dfprepostp <- mutate(dfprepost , PRE_POST_P = (ECR_POST_P - ECR_PRE_P)/ECR_PRE_P*100) %>% select(PRE_POST_P)

placebo <- "placebo"
patch <- "patch"

dfprepostpb <- cbind(dfprepostpb, placebo)
dfprepostp <- cbind(dfprepostp, patch)

colnames(dfprepostp) <- c("ECR_PRE_POST" , "condition")
colnames(dfprepostpb) <- c("ECR_PRE_POST" , "condition")

dfprepost <- rbind(dfprepostp , dfprepostpb)

ttestpourc <- dfprepost %>% t_test(ECR_PRE_POST ~ condition , paired = T)

#plot

dfprepost %>% ggplot(aes(x = condition , y = ECR_PRE_POST)) +
  geom_boxplot(aes(color = condition)) +
  geom_point(aes(shape = condition , color = condition) , size = 1) +
  ylab("Difference ECR PRE-POST (%)") +
  stat_summary(aes(label = round(..y.., 2)),
               geom = "text",
               fun.y = mean,
               size = 4) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5 , size = 12 , face = "bold") ,
    axis.text = element_text(size = 7) ,
    axis.title = element_text(size = 8 , face = "bold") ,
    legend.position = "right" ,
    legend.title = element_text(size = 8 , face = "bold") ,
    legend.text = element_text(size = 6) ,
    legend.background = element_rect(color = "black" , size = 0.1)
  ) +
  stat_compare_means(method = "t.test" , paired = TRUE)
