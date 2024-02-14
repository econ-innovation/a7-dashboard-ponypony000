
#导入包
library(ggplot2)
library(ggpubr)
library(readr)
library(dplyr)
library(ggthemes) 
library(scales) 
library(plotly)
library(gifski)
library(gganimate)


pub_211 <- read.csv("/Users/mawenting/Downloads/pub_211.csv")

#按学校统计各年发文量
pubnum_w <- pub_211 %>% 
  group_by(inst_cn,pubyear) %>% 
  summarise(pubnum_wos = sum(pubnum_wos)) 

# 选出2022年发文最多的十所学校
top10 <- pub_211 %>% 
  filter(pubyear == 2022) %>% 
  group_by(inst_cn) %>% 
  summarise(pubnum_wos = sum(pubnum_wos)) %>% 
  arrange(desc(pubnum_wos)) %>% 
  slice(1:10)

#筛选top10学校的数据并计算各年发文量
top10_pub <- pub_211 %>% 
  semi_join(top10,by = "inst_cn") %>% 
  group_by(inst_cn,pubyear) %>% 
  summarise(pubnum_wos = sum(pubnum_wos),
            totaljif = sum(totaljif))

#1 随时间变化top10学校发文散点图
p1 <- ggplot(top10_pub, aes(x = pubyear, y = pubnum_wos))
p2 <- p1 + geom_point(shape = 1, 
                      size = 2.5,
                      aes(color = inst_cn)) + 
  theme(legend.text = element_text(family='SimSun')) +
  geom_smooth(method = "lm",color = "darkred", 
              formula = y~ poly(x, 2),
              se = FALSE) + 
  scale_x_continuous(limits = c(1996,2023),
                     breaks = seq(1995,2022,5)) +
  scale_y_continuous(breaks = seq(10000,60000,10000), 
                     limits = c(0,65000))+
  guides(colour = guide_legend(title = element_blank()), 
         linetype = guide_legend(title = element_blank()))+
  theme_classic() +
  labs(title = "1996-2022年Top10大学发文量散点图", 
       caption = "注：Top10指2022年发文量前十")
print(p2)

#2 分面
p3 <- p1 + geom_point() + 
  theme(text = element_text(family='SimSun')) +
  geom_smooth(method = 'loess' ,formula = 'y ~ x',size = 0.3) +
  facet_wrap(~ inst_cn)+
  theme_classic() 
print(p3)

##3 图形合并展示
p4 <- ggarrange(p2, p3, labels = c("A", "B"),
                    ncol = 1, nrow = 2) 
print(p4)

#4 交互图
ggplotly(p2,height = 400)

##5 表格
library(gt)
library(DT)
datatable(top10)

print(gt_tbl)