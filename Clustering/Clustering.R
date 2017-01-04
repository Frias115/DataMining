require(graphics)

social_data <- read.csv("social_network.csv")

str(social_data)

summary(social_data)

social_data$age <- ifelse(social_data$age >= 13 & social_data$age < 20, social_data$age, NA )

social_data$female <- ifelse(social_data$gender == "F" & !is.na(social_data$gender), 1, 0)
social_data$no_gender <- ifelse(is.na(social_data$gender), 1, 0)

mean(social_data$age, na.rm = TRUE)

aggregate(data = social_data, age ~ gradyear, mean, na.rm = TRUE)

ave_age <- ave(social_data$age, social_data$gradyear, FUN = function(x) mean(x, na.rm = TRUE))

social_data$age <- ifelse(is.na(social_data$age), ave_age, social_data$age)
summary(social_data$age)

dim(social_data)

social_data$gender <- NULL

social_data_norm<- scale(social_data)

colnames(social_data_norm)

# 1º kmeans
# drunk, drugs, age


tabla1 <- social_data_norm[, c(2,38,39)]

cl1 <- kmeans(tabla1, 4, nstart = 5)

plot(tabla1, col = cl1$cluster)

points(cl1$centers, col = 1:5, pch = 8)

cl1
#Pertenecientes al cluster 1, media age = -0.8, media drunk = -0.12 y media drugs = -0.17
cl1$cluster[128]
head(tabla1[128,])
cl1$cluster[9962]
head(tabla1[9962,])
cl1$cluster[9101]
head(tabla1[9101,])
cl1$cluster[9621]
head(tabla1[9621,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster, aunque los valores de age sean muy dispares los 
#drunk y drugs coinciden

#Pertenecientes al cluster 2, media age = 0.17, media drunk = 6.17 y media drugs = 0.3
cl1$cluster[327]
head(tabla1[327,])
cl1$cluster[977]
head(tabla1[977,])
cl1$cluster[976]
head(tabla1[976,])
cl1$cluster[9951]
head(tabla1[9951,])
#Estos cuatro elementos de testeo no coinciden

#Pertenecientes al cluster 3, media age = 0.8, media drunk = -0.11 y media drugs = -0.17
cl1$cluster[1]
head(tabla1[1,])
cl1$cluster[2]
head(tabla1[2,])
cl1$cluster[3]
head(tabla1[3,])
cl1$cluster[4]
head(tabla1[4,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster

#Pertenecientes al cluster 4, media age = -0.05, media drunk = 0.54 y media drugs = 3.87
cl1$cluster[7216]
head(tabla1[7216,])
cl1$cluster[6827]
head(tabla1[6827,])
cl1$cluster[5916]
head(tabla1[5916,])
cl1$cluster[6046]
head(tabla1[6046,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster, aunque los valores de age sean muy dispares los 
#drunk y drugs coinciden

#El cl1 nos muestras 4 grupos difereciados en:
#age baja, drunk bajo y drugs bajo
#age media, drunk muy alto y drugs medio
#age alta, drunk bajo y drugs bajo
#age media, drunk medio y drugs alto


# 2º kmeans
# sex, hot, god, jesus


tabla2 <- social_data_norm[, c(15,17,24,26)]

cl2 <- kmeans(tabla2, 3, nstart = 5)

plot(tabla2, col = cl2$cluster)

points(cl2$centers, col = 1:5, pch = 8)

cl2
#Pertenecientes al cluster 1, media sex = 0.19, media hot = 2.65, media god = 0.03, media jesus = -0.03
cl2$cluster[651]
head(tabla2[651,])
cl2$cluster[716]
head(tabla2[716,])
cl2$cluster[911]
head(tabla2[911,])
cl2$cluster[982]
head(tabla2[982,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster, aunque los valores de sex y god
#sean muy dispares los de hot y jesus coinciden

#Pertenecientes al cluster 2, media sex = 0.12, media hot = -0.02, media god = 4.3, media jesus = 5.4
cl2$cluster[5981]
head(tabla2[5981,])
cl2$cluster[3251]
head(tabla2[3251,])
cl2$cluster[391]
head(tabla2[391,])
cl2$cluster[4814]
head(tabla2[4814,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster

#Pertenecientes al cluster 3, media sex = -0.02, media hot = -0.2, media god = -0.06, media jesus = -0.06
cl2$cluster[4811]
head(tabla2[4811,])
cl2$cluster[5201]
head(tabla2[5201,])
cl2$cluster[5591]
head(tabla2[5591,])
cl2$cluster[6111]
head(tabla2[6111,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster

#El cl2 nos muestras 3 grupos difereciados en:
#sex medio, hot alto, god bajo y jesus bajo
#sex medio, hot bajo, god alto y jesus muy alto
#sex bajo, hot bajo, god bajo y jesus bajo


# 3º kmeans
# friends, cheerleading, sports, shopping


tabla3 <- social_data_norm[, c(3,10,13,32)]

cl3 <- kmeans(tabla3, 3, nstart = 5)

plot(tabla3, col = cl3$cluster)

points(cl3$centers, col = 1:5, pch = 8)

cl3
#Pertenecientes al cluster 1, media friends = 0.91, media cheerleading = -0.08, media sports = 0.59, media shopping = 1.08
cl3$cluster[5]
head(tabla3[5,])
cl3$cluster[6]
head(tabla3[6,])
cl3$cluster[67]
head(tabla3[67,])
cl3$cluster[132]
head(tabla3[132,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster

#Pertenecientes al cluster 2, media friends = 0.25, media cheerleading = 5.34, media sports = 0.09, media shopping = 0.54
cl3$cluster[5529]
head(tabla3[651,])
cl3$cluster[5266]
head(tabla3[651,])
cl3$cluster[1039]
head(tabla3[651,])
cl3$cluster[1040]
head(tabla3[651,])
#Estos cuatro elementos de testeo no coinciden

#Pertenecientes al cluster 3, media friends = -0.26, media cheerleading = -0.14, media sports = -0.16, media shopping = -0.3
cl3$cluster[2341]
head(tabla3[2341,])
cl3$cluster[2796]
head(tabla3[2796,])
cl3$cluster[3122]
head(tabla3[3122,])
cl3$cluster[3446]
head(tabla3[3446,])
#Estos cuatro elementos de testeo son correctos dentro de los valores del cluster

#El cl3 nos muestras 3 grupos difereciados en:
#friends muy alto, cheerleading bajo, sports alto y shopping muy alto
#friends medio, cheerleading muy alto, sports medio y shopping medio
#friends bajo, cheerleading bajo, sports bajo y shopping bajo
