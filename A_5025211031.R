# Install libraries
install.packages(c("BSDA", "mosaic", "ggpubr", "ggplot2", "multcompView"))

# Nomor 1

# Nomor 1A
sebelum <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
setelah <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)
selisih <- setelah - sebelum

sd(selisih)

# Nomor 1B
t.test(setelah, sebelum, paired = TRUE)$p.value

# Nomor 1C
# p-value lebih kecil daripada tingkat signifikansi sehingga hipotesis nol ditolak dan hipotesis alternatif diterima
# Dengan demikian, ada pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas A


# Nomor 2

# Nomor 2A
# Setuju

# Nomor 2B
library(BSDA)
tsum.test(mean.x=23500, s.x=3900, n.x=100, mu=20000, alternative="greater")

# Nomor 2C
# Karena nilai p-value < tingkat signifikansi maka hipotesis nol ditolak dan hipotesis alternatif diterima sehingga kesimpulan yang diperoleh adalah bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun


# Nomor 3

# Nomor 3A
# H0: mean1 = mean2
# H1: mean1 != mean2

# Nomor 3B
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y=2.79, s.y = 1.32, n.y = 27, alternative="two.sided", var.equal=TRUE)

# Nomor 3C
library(mosaic)
plotDist(dist='t', df=2, col='blue')

# Nomor 3D
qt(p=0.05, df=2, lower.tail=FALSE)

# Nomor 3E
# Nilai p-value > tingkat signifikansi sehingga H0 tidak ditolak atau H0 diterima

# Nomor 3F
# Karena H0 berlaku maka mean1 = mean2

# Nomor 4

# Nomor 4A
# Menyimpan data tabel
data <- read.table(url("https://rstatisticsandresearch.weebly.com/uploads/1/0/2/6/1026585/onewayanova.txt"),h=T)

data$Group <- as.factor(data$Group)
data$Group <- factor(data$Group,labels = c("Kucing Oren", "Kucing Hitam", "Kucing Putih"))

grup1 <- subset(data, Group=="Kucing Oren")
grup2 <- subset(data, Group=="Kucing Hitam")
grup3 <- subset(data, Group=="Kucing Putih")

qqnorm(grup1$Length)
qqline(grup1$Length)

qqnorm(grup2$Length)
qqline(grup2$Length)

qqnorm(grup3$Length)
qqline(grup3$Length)

# Nomor 4B
bartlett.test(Length~Group, data)

# Nomor 4C
model1 <- lm(Length~Group, data)
anova(model1)

# Nomor 4D
# p-value < tingkat signifikansi sehingga H0 ditolak dan H1 diterima
# Dengan demikian, terdapat perbedaan panjang antara ketiga spesies

# Nomor 4E
TukeyHSD(aov(model1))

# Nomor 4F
library(ggpubr)
ggplot(data, aes(x = Group, y = Length)) +
    geom_boxplot(fill="blue", colour="black") +
    scale_x_discrete() + xlab("Spesies") +
    ylab("Panjang")

# Nomor 5

# Nomor 5A
library(ggplot2)
data <- read.csv("GTL.csv")
qplot(x=Temp, y=Light, data=data) +
    facet_grid(.~Glass) +
    geom_point()

# Nomor 5B
data$Glass <- as.factor(data$Glass)
data$Temp_Factor <- as.factor(data$Temp)

anova <- aov(Light ~ Glass*Temp_Factor, data=data)
summary(anova)

# Nomor 5C
library(dplyr)
group_by(data, Glass, Temp) %>% summarise(mean=mean(Light), sd=sd(Light)) %>% arrange(desc(mean))

# Nomor 5D
tukey <- TukeyHSD(anova)
tukey

# Nomor 5E
library(multcompView)
multcompLetters4(anova, tukey)
