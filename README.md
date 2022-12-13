# Modul2_Probstat_5025211031
Nama | NRP 
--- | ---
Aryan Shafa Wardana | 5025211031
## Pendahuluan
Repository ini dibuat untuk menyelesaikan Praktikum Probabilitas dan Statistika Modul 2. Sebelum mulai, diperlukan library-library yang bisa diinstall dengan
```r
install.packages(c("BSDA", "mosaic", "ggpubr", "ggplot2", "multcompView"))
```
## Nomor 1
### A. Mencari standar deviasi dari data selisih
```r
sebelum <- c(78, 75, 67, 77, 70, 72, 78, 74, 77)
setelah <- c(100, 95, 70, 90, 90, 90, 89, 90, 100)
selisih <- setelah - sebelum

sd(selisih)
```
![image](https://user-images.githubusercontent.com/115603634/207385139-73da5e15-5c55-4a7e-978f-f738a1cc1749.png)
Maka standar deviasinya adalah 6.359595

### B. Mencari nilai t (p-value)
```r
t.test(setelah, sebelum, paired = TRUE)$p.value
```
![image](https://user-images.githubusercontent.com/115603634/207385216-9d49165e-b852-4995-8792-af30c03c5e14.png)

Maka nilai t (p-value) adalah 6.003e-05

### C. Mencari keputusan dan kesimpulan
Diketahui bahwa p-value = 6.003e-05 dan α = 0.05 sehingga p-value < α. Oleh karena itu, $H_0$ ditolak sehingga terdapat pengaruh yang signifikan secara statistika dalam hal kadar saturasi oksigen, sebelum dan sesudah melakukan aktivitas 𝐴

## Nomor 2
### A. Apakah Anda setuju dengan klaim tersebut?
Iya, setuju.

### B. Jelaskan maksud dari output yang dihasilkan!
```r
library(BSDA)
tsum.test(mean.x=23500, s.x=3900, n.x=100, mu=20000, alternative="greater")
```
![image](https://user-images.githubusercontent.com/115603634/207385525-de318210-243e-43fc-8404-d59c5e2d8a07.png)

Dari hasil t-test dengan tingkat keyakinan 95%, diperoleh bahwa nilai t = 8.7644 dan p-value = 9.437e-15

### C. Kesimpulan
Diketahui p-value = 9.437e-15 dan α = 0.05 sehingga p-value < α. Oleh karena itu, $H_0$ ditolak sehingga mobil dikemudikan rata-rata kurang dari sama dengan 20.000 kilometer per tahun.

## Nomor 3
### A. $H_0$ dan $H_1$
$H_0$: $μ_1$ = $μ_2$ (rata-rata saham di Bandung sama dengan di Bali)\
$H_1$: $μ_1$ ≠ $μ_2$ (rata-rata saham di Bandung tidak sama dengan di Bali)

### B. Sampel statistik
```r
tsum.test(mean.x=3.64, s.x = 1.67, n.x = 19, mean.y=2.79, s.y = 1.32, n.y = 27, alternative="two.sided", var.equal=TRUE)
```
![image](https://user-images.githubusercontent.com/115603634/207385592-a33c608b-9464-45dd-b895-43901c4ee73f.png)

### C. Uji statistik (df = 2)
```r
library(mosaic)
plotDist(dist='t', df=2, col='blue')
```
![image](https://user-images.githubusercontent.com/115603634/207385700-7e8cf2bb-a549-403c-92c2-ae926984444e.png)

### D. Nilai kritikal
```r
qt(p=0.05, df=2, lower.tail=FALSE)
```
![image](https://user-images.githubusercontent.com/115603634/207385771-efb5738f-aeed-43ac-8c83-4adebb2f2f0e.png)

Maka diperoleh nilai kritikal t adalah 2.919986

### E. Keputusan
Dikarenakan nilai p-value > α maka $H_0$ tidak ditolak 

### F. Kesimpulan
Dikarenakan $H_0$ tidak ditolak maka $μ_1$ = $μ_2$ (rata-rata saham di Bandung sama dengan di Bali)

## Nomor 4
### A. Mengelompokkan masing-masing spesies lalu gambarkan plot kuantil normal untuk setiap kelompok dan lihat apakah ada outlier utama dalam homogenitas varians
```r
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
```
- Grup 1:\
![image](https://user-images.githubusercontent.com/115603634/207385870-1287cc48-802f-4197-b773-03eff9e49aca.png)
- Grup 2:\
![image](https://user-images.githubusercontent.com/115603634/207386082-b811bc02-0610-4e3d-abf2-90cec812bf2e.png)
- Grup 3:\
![image](https://user-images.githubusercontent.com/115603634/207386112-4351e401-a137-42e9-ac60-ed14b1cd3b55.png)

### B. Mencari homogenitas variansi. Berapa nilai p? Apa hipotesis dan kesimpulan yang bisa diambil?
```r
bartlett.test(Length~Group, data)
```
![image](https://user-images.githubusercontent.com/115603634/207386231-0ab0ad79-43f1-4d54-bb06-7b8f9c056388.png)

$H_0$: Variansi dari semua populasi sama \
$H_1$: Terdapat setidaknya satu pasang dengan variansi berbeda\
\
Diketahui p-value = 0.8054 dan α = 0.05 sehingga p-value > α. Oleh karena itu, $H_0$ tidak ditolak sehingga variansi dari semua populasi sama

### C. Membuat model linier dengan Panjang versus Grup dan diberi nama model 1
```r
model1 <- lm(Length~Group, data)
anova(model1)
```
![image](https://user-images.githubusercontent.com/115603634/207386292-eddfd6a3-acf2-4735-a307-8123de7501c1.png)

### D. Dari poin C, berapa nilai p? Apa yang dapat disimpulkan dari $H_0$
Diketahui p-value = 0.0013 dan α = 0.05 sehingga p-value <> α. Oleh karena itu, $H_0$ ditolak sehingga terdapat perbedaan panjang antara ketiga spesies.

### E. Verifikasi jawaban model 1 menggunakan Post-hooc test TukeyHSD, dari nilai p yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain?
```r
TukeyHSD(aov(model1))
```
![image](https://user-images.githubusercontent.com/115603634/207386348-11358050-9369-48af-bbee-57b9d69a8861.png)

Jika perbandingan spesies memiliki nilai p > 0.05, maka secara statistik kedua spesies memiliki panjang yang sama. Sedangkan, jika nilai p < 0.05, maka spesies berbeda.

### F. Visualisasikan data dengan ggplot2
```r
library(ggpubr)
ggplot(data, aes(x = Group, y = Length)) +
    geom_boxplot(fill="blue", colour="black") +
    scale_x_discrete() + xlab("Spesies") +
    ylab("Panjang")
```
![image](https://user-images.githubusercontent.com/115603634/207386845-b1675c8e-af45-4ba0-afa7-d3cec14039ad.png)

## Nomor 5
### A. Buatlah plot sederhana untuk visualisasi data 
```r
library(ggplot2)
data <- read.csv("GTL.csv")
qplot(x=Temp, y=Light, data=data) +
    facet_grid(.~Glass) +
    geom_point()
```
![image](https://user-images.githubusercontent.com/115603634/207386898-fd532b6f-ed27-4ea8-8bde-ca8d875f59ad.png)

### B. Lakukan uji ANOVA dua arah untuk 2 faktor
```r
data$Glass <- as.factor(data$Glass)
data$Temp_Factor <- as.factor(data$Temp)

anova <- aov(Light ~ Glass*Temp_Factor, data=data)
summary(anova)
```
![image](https://user-images.githubusercontent.com/115603634/207386952-456fd72e-38cc-4dc1-a17a-3479299c4fc5.png)

### C. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
```r
library(dplyr)
group_by(data, Glass, Temp) %>% summarise(mean=mean(Light), sd=sd(Light)) %>% arrange(desc(mean))
```
![image](https://user-images.githubusercontent.com/115603634/207387011-f7e1fd32-ed7e-4b01-80b5-d86c17af5495.png)

### D. Lakukan uji Tukey
```r
tukey <- TukeyHSD(anova)
tukey
```
![image](https://user-images.githubusercontent.com/115603634/207387139-9caa99b3-fe32-4b71-97e7-73a6dc3d5477.png)

### E. Gunakan compact letter display untuk menunjukkan perbedaan signifikan antara uji Anova dan uji Tukey
```r
library(multcompView)
multcompLetters4(anova, tukey)
```
![image](https://user-images.githubusercontent.com/115603634/207387186-c8c248a4-242d-4ae8-844d-cc311fba88c4.png)
