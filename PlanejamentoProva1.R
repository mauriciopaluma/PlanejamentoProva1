#Questão 1

# a)
y = c(19.30, 18.00, 18.10, 19.00,
      17.25, 16.75, 15.90, 16.00,  
      15.65, 14.09, 15.95, 16.60,
      18.05, 18.85, 18.04, 17.35,
      17.25, 17.00, 16.45, 16.40
)

racao = factor(rep(1:5, each = 4))

cbind(racao, y)

# Onde 1 represanta a ração A, 2 a B, 3 a C, 4 a D e 5 a E

# d)
bartlett.test(y ~ racao)

qchisq(1 - 0.05, 4) 


# e)
saida_anova <- aov(y ~ racao)
summary(saida_anova)


# f) 
install.packages("ExpDes.pt")
library(ExpDes.pt)

saida_dic = dic(racao, y, quali = TRUE, mcomp = "tukey")

# h) 
boxplot(y ~ racao, pch = 19)
points(racao, y, pch = 19)

plot(saida_dic$residuos/sqrt( 0.5030 ), ylim = c(-3,3))  #QM dos residuos dentro do sqrt
abline(h = 0)
abline(h = 2)
abline(h = -2)


# i) 

medias <- tapply(y, racao, mean)


media_3 <- medias["3"]
media_4 <- medias["4"]


media_medio <- (media_3 + media_4) / 2

n = 4  
peso_total <- n * media_medio

list(media_intermediaria = media_medio, peso_total = peso_total)
