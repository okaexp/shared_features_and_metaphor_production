#import libraries
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(stringr)

raw_dat <- read.csv("../data/exp2_sentence_rating_task.csv")

#check age and sex
dat_age <- raw_dat %>% 
  dplyr::select(Age)  %>% 
  summarise(m_age = mean(Age), sd_age = sd(Age), min_age = min(Age), max_age = max(Age))

dat_sex <- raw_dat %>% 
  dplyr::select(Sex) %>% 
  group_by(Sex) %>% 
  summarise(count = n())

#create bar plot
dat_count_eng <- raw_dat %>% 
  dplyr::select(-P_ID, -Sex, -Age, -Participate) %>%
  tidyr::gather(key = Questions, value = Value,
                -ID) %>% 
  tidyr::separate(col = Questions, into = c("Question", "Order", "Condition", "TaskType")) %>%
  tidyr::drop_na() %>%
  dplyr::filter(TaskType == "S") %>%
  dplyr::mutate(test = rep(1,nrow(.))) %>%
  dplyr::mutate(Response = as.factor(Value)) %>%
  dplyr::mutate(Condition = as.factor(Condition)) %>%
  dplyr::group_by(Condition, Response) %>%
  dplyr::summarise(Count = sum(test)) %>%
  dplyr::mutate(Prop = (Count)/1140)

levels(dat_count_eng$Response) <- c("Literal", "Metaphorical", "Otherwise")
dat_count_eng$Response <- factor(dat_count_eng$Response, levels = c("Otherwise", "Literal", "Metaphorical"))
levels(dat_count_eng$Condition) <-  c("One", "Two", "Three")

ggplot()+theme_set(theme_classic(base_size = 16,base_family="Hiragino Mincho Pro W3"))
par(xpd = T, family = "Japan1")
p <- ggplot(dat_count_eng, aes(x = Condition, y = Prop))
p <- p + geom_bar(stat = "identity", aes(fill = Response), colour = "black") +
  scale_fill_manual(values = c("white", "gray", "black"))
p <- p + theme(axis.text.y = element_text(size=16),
               axis.text.x = element_text(size = 16))
p <- p + ylab("Proporotion of responses")
p

#conduct linear mixed effect model
dat_lmer <- raw_dat %>% 
  dplyr::select(-P_ID, -Sex, -Age, -Participate) %>%
  tidyr::gather(key = Questions, value = Value,
                -ID) %>% 
  tidyr::separate(col = Questions, into = c("Question", "Order", "Condition", "TaskType")) %>%
  dplyr::mutate(Value = as.factor(Value)) %>%
  tidyr::drop_na() %>%
  dplyr::filter(TaskType == "S") %>%
  dplyr::mutate(Response = factor(Value, levels = c(1, 2, 3)),
                Condition = as.factor(Condition),
                Question = as.factor(Question),
                ID = as.factor(ID)
                )

CondDum_2m1 <- ifelse(dat_lmer$Condition == "2", "1", "0")
CondDum_3m1 <- ifelse(dat_lmer$Condition == "3", "1", "0")
dat_lmer_ud <- data.frame(dat_lmer, CondDum_2m1, CondDum_3m1)

RespDum_MaO <- ifelse(dat_lmer_ud$Response == "2", "1", "0")
dat_lmer_ud <- data.frame(dat_lmer_ud, RespDum_MaO = as.factor(RespDum_MaO))

res_lmer_MaO_compOrder <- glmer(formula = RespDum_MaO ~ Order + (1|ID) + (1|Question),
                                data = dat_lmer_ud,
                                family = binomial)
summary(res_lmer_MaO_compOrder)

res_lmer_MaO <- glmer(formula = RespDum_MaO ~ Order + CondDum_2m1 + CondDum_3m1 + (1|ID) + (1|Question),
                      data = dat_lmer_ud,
                      family = binomial)
summary(res_lmer_MaO)

CondDum_3m2 <- ifelse(dat_lmer$Condition == "3", "1", "0")
CondDum_1m2 <- ifelse(dat_lmer$Condition == "1", "1", "0")
dat_lmer_ud <- data.frame(dat_lmer_ud, CondDum_3m2, CondDum_1m2)

res_lmer_MaO_2 <- glmer(formula = RespDum_MaO ~ Order + CondDum_3m2 + CondDum_1m2 + (1|ID) + (1|Question),
                      data = dat_lmer_ud,
                      family = binomial)
summary(res_lmer_MaO_2)
