#import libraries
library(tidyr)
library(dplyr)
library(lme4)
library(lmerTest)
library(ggplot2)

raw_dat <- read.csv("../data/exp1_sentence_paraphrasing_task.csv")

#create bar plot (nominal metaphorical)
main_class_dat = dplyr::mutate(raw_dat,
                               Category = case_when(
                                 分類後 %in% c("名詞直喩", "名詞隠喩") ~ "Nominal metaphorical",
                                  分類後 == "字義" ~ "Literal",
                                  分類後 %in% c("その他直喩", "その他隠喩", "それ以外") ~ "Others"
                               )
) %>%
  dplyr::mutate(Category = as.factor(Category),
                Condition = as.factor(Condition))

dat_main_class_count <- main_class_dat %>% 
  dplyr::group_by(Condition, Category) %>%
  dplyr::summarise(Count = n())%>%
  dplyr::mutate(Proportion = (Count)/1140)

dat_main_class_count$Category <- factor(dat_main_class_count$Category,
                                          levels = c("Others", "Literal", "Nominal metaphorical"))
levels(dat_main_class_count$Condition) <- c("One", "Two", "Three")

#quartz(type = "pdf", file = "./p1_bar_graph_noun_English_w10_h10.pdf")
ggplot()+theme_set(theme_classic(base_size = 16,base_family="Hiragino Mincho Pro W3"))
p <- ggplot(dat_main_class_count, aes(x = Condition, y = Proportion))
p <- p + geom_bar(stat = "identity", aes(fill = Category), colour = "black") +
  scale_fill_manual(values = c("white", "gray", "black"))
p <- p + theme(axis.text.y = element_text(size=16),
               #axis.title.x = element_blank(),
               axis.text.x = element_text(size = 16))
p <- p + ylab("Proporotion of responses")
plot(p)
#dev.off()

#create bar plot (metaphorical)
metaphor_class_dat = dplyr::mutate(raw_dat,
                               Category = case_when(
                                 分類後 %in% c("名詞直喩", "名詞隠喩", "その他隠喩", "その他直喩") ~ "Metaphorical",
                                  分類後 == "字義" ~ "Literal",
                                  分類後 %in% c("それ以外") ~ "Otherwise"
                               )
) %>%
  dplyr::mutate(Category = as.factor(Category),
                Condition = as.factor(Condition))

dat_metaphor_class_count <- metaphor_class_dat %>% 
  dplyr::group_by(Condition, Category) %>%
  dplyr::summarise(Count = n())%>%
  dplyr::mutate(Proportion = (Count)/1140)

dat_metaphor_class_count$Category <- factor(dat_metaphor_class_count$Category,
                                        levels = c("Otherwise", "Literal", "Metaphorical"))
levels(dat_metaphor_class_count$Condition) <- c("One", "Two", "Three")

#quartz(type = "pdf", file = "./p1_bar_graph_metaphorical_English.pdf")
ggplot()+theme_set(theme_classic(base_size = 16,base_family="Hiragino Mincho Pro W3"))
p <- ggplot(dat_metaphor_class_count, aes(x = Condition, y = Proportion))
p <- p + geom_bar(stat = "identity", aes(fill = Category), colour = "black") +
  scale_fill_manual(values = c("white", "gray", "black"))
p <- p + theme(axis.text.y = element_text(size=16),
               axis.text.x = element_text(size = 16))
p <- p + ylab("Proporotion of responses")
plot(p)
#dev.off()

#conduct linear mixed effect model (nominal metaphorical)
main_class_dat = dplyr::mutate(raw_dat,
              main_class = case_when(
               分類後 %in% c("名詞直喩", "名詞隠喩") ~ 1,
                分類後 == "字義" ~ 2,
                分類後 %in% c("その他直喩", "その他隠喩", "それ以外") ~ 3
              )
            ) %>%
  dplyr::mutate(main_class = as.factor(main_class),
                Condition = as.factor(Condition))

dat_lmer <- main_class_dat %>% 
  dplyr::mutate(ID = as.factor(ID))

CondDum_2m1 <- ifelse(dat_lmer$Condition == "2", "1", "0")
CondDum_3m1 <- ifelse(dat_lmer$Condition == "3", "1", "0")
dat_lmer_ud <- data.frame(dat_lmer, CondDum_2m1, CondDum_3m1)

RespDum_MaO <- ifelse(dat_lmer_ud$main_class == "1", "1", "0")
dat_lmer_ud <- data.frame(dat_lmer_ud, RespDum_MaO = as.factor(RespDum_MaO))

res_lmer_MaO <- glmer(formula = RespDum_MaO ~ CondDum_2m1 + CondDum_3m1 + (1|ID) + (1|Question),
                      data = dat_lmer_ud,
                      family = binomial)
summary(res_lmer_MaO)

CondDum_3m2 <- ifelse(dat_lmer$Condition == "3", "1", "0")
CondDum_1m2 <- ifelse(dat_lmer$Condition == "1", "1", "0")
dat_lmer_ud <- data.frame(dat_lmer_ud, CondDum_3m2, CondDum_1m2)

res_lmer_MaO_2 <- glmer(formula = RespDum_MaO ~ CondDum_3m2 + CondDum_1m2 + (1|ID) + (1|Question),
                      data = dat_lmer_ud,
                      family = binomial)
summary(res_lmer_MaO_2)


############################################################

#conduct linear mixed effect model (metaphorical)
metaphor_class_dat = dplyr::mutate(raw_dat,
                               metaphor_class = case_when(
                                 分類後 %in% c("名詞直喩", "名詞隠喩", "その他直喩", "その他隠喩") ~ 1,
                                  分類後 == "字義" ~ 2,
                                  分類後 == "それ以外" ~ 3
                               )
) %>%
  dplyr::mutate(metaphor_class = as.factor(metaphor_class),
                Condition = as.factor(Condition))

dat_metaphor_lmer <- metaphor_class_dat %>% 
  dplyr::mutate(ID = as.factor(ID))

CondDum_2m1 <- ifelse(dat_metaphor_lmer$Condition == "2", "1", "0")
CondDum_3m1 <- ifelse(dat_metaphor_lmer$Condition == "3", "1", "0")
dat_metaphor_lmer_ud <- data.frame(dat_metaphor_lmer, CondDum_2m1, CondDum_3m1)


RespDum_MaO <- ifelse(dat_metaphor_lmer_ud$metaphor_class == "1", "1", "0")
dat_metaphor_lmer_ud <- data.frame(dat_metaphor_lmer_ud, RespDum_MaO = as.factor(RespDum_MaO))

res_lmer_metaphor_MaO <- glmer(formula = RespDum_MaO ~ CondDum_2m1 + CondDum_3m1 + (1|ID) + (1|Question),
                      data = dat_metaphor_lmer_ud,
                      family = binomial)
summary(res_lmer_metaphor_MaO)

CondDum_3m2 <- ifelse(dat_metaphor_lmer$Condition == "3", "1", "0")
CondDum_1m2 <- ifelse(dat_metaphor_lmer$Condition == "1", "1", "0")
dat_metaphor_lmer_ud <- data.frame(dat_metaphor_lmer_ud, CondDum_3m2, CondDum_1m2)

res_lmer_metaphor_MaO_2 <- glmer(formula = RespDum_MaO ~ CondDum_3m2 + CondDum_1m2 + (1|ID) + (1|Question),
                        data = dat_metaphor_lmer_ud,
                        family = binomial)
summary(res_lmer_metaphor_MaO_2)
