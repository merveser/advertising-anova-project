install.packages("car")
library(tidyverse)
library(car)

data <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/Advertising.csv")
#Bu çalışmada, reklam harcamalarının satışlar üzerindeki etkisi incelenmiştir. Özellikle TV reklam harcamalarının satışlar üzerindeki etkisi analiz edilmiştir.

head(data)
str(data)
summary(data)

# X sütunu gereksiz olduğu için sütunu kaldıralım
data <- data %>% select(-X)

# Eksik veri kontrolü
colSums(is.na(data))

str(data)
#Histogram
ggplot(data, aes(sales)) +
  geom_histogram(binwidth = 2, fill = "#4E79A7", color = "white") +
  labs(
    title = "Sales Distribution",
    x = "Sales",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)
ggsave("outputs/histogram_sales.png", width = 6, height = 4)
#Nokta grafiği saesinde satışlar ve reklam kanallarının arasındaki ilişkiye bakılır. Bu grafiklerde en güçlü ilişki TV en zayıf ilişki gazete olarak gözlenmiştir.
ggplot(data, aes(TV, sales)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(
    title = "TV Advertising vs Sales",
    x = "TV Advertising Budget",
    y = "Sales"
  ) +
  theme_minimal(base_size = 14)

ggplot(data, aes(radio, sales)) +
  geom_point(color = "hotpink", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal(base_size = 14)
ggsave("outputs/scatter_tv.png", width = 6, height = 4)
ggsave("outputs/scatter_radio.png", width = 6, height = 4)
ggplot(data, aes(newspaper, sales)) +
  geom_point(color = "darkgreen", alpha = 0.6) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  theme_minimal(base_size = 14)
ggsave("outputs/scatter_newspaper.png", width = 6, height = 4)
#En güçlü ilişki TV'de olduğu için ANOVA'yı bu ilişki üzerinden kuruyoruz.
data <- data %>%
  mutate(TV_group = case_when(
    TV <= quantile(TV, 0.33) ~ "Low",
    TV <= quantile(TV, 0.66) ~ "Medium",
    TRUE ~ "High"
  ))

data$TV_group <- as.factor(data$TV_group)
#Boxplot grafiğinde yüksek bütçe ayrılan reklamların daha yüksek satış yaptığını görmüş olduk.
ggplot(data, aes(TV_group, sales, fill = TV_group)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Sales by TV Advertising Level",
    x = "TV Group",
    y = "Sales"
  ) +
  theme_minimal(base_size = 14)
ggsave("outputs/boxplot_tv_group.png", width = 6, height = 4)
by(data$sales, data$TV_group, shapiro.test)

anova_model <- aov(sales ~ TV_group, data = data)

shapiro.test(anova_model$residuals)
#H0 reddedilemez
#Artıklar normal dağılıma uygundur
# Normallik varsayımı sağlanmıştır

car::leveneTest(sales ~ TV_group, data = data)
#Varyans homojenliği varsayımı ihlal edilmiştir. Ancak:

#Örneklem büyüklüğü yeterlidir (n=200)
#Gruplar dengelidir

#Bu nedenle klasik ANOVA kısmen güvenilir kabul edilebilir.
#Daha doğru sonuç için Welch ANOVA uygulanmıştır.

oneway.test(sales ~ TV_group, data = data, var.equal = FALSE)
#H0 reddedilir.
#TV reklam harcama seviyelerine göre satış ortalamaları arasında istatistiksel olarak anlamlı fark vardır.

TukeyHSD(anova_model)
#Tukey çoklu karşılaştırma testi sonuçlarına göre, TV reklam harcama seviyeleri arasında tüm grup çiftlerinde istatistiksel olarak anlamlı fark bulunmaktadır. 
#Reklam bütçesi arttıkça satışların da sistematik olarak arttığı gözlemlenmiştir. En yüksek satış ortalamaları yüksek bütçeli (High) grupta elde edilmiştir.


#Elde edilen bulgular, reklam bütçesi ile satışlar arasında pozitif yönlü ve anlamlı bir ilişki olduğunu göstermektedir. 
#Bu durum, pazarlama stratejilerinde TV reklamlarının kritik bir rol oynadığını ortaya koymaktadır.