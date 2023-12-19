# Instalarea și încărcarea pachetelor necesare
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(reshape2)


# Citirea datelor din fișierul CSV
data <- read.csv("C:/Users/sergi/Desktop/UTM/An_III/Analiza datelor/Lazar_Sergiu_AD/Proiect/RealEstate.csv")

# Verificarea structurii datelor
head(data)
str(data)
# Identificarea valorilor lipsă
sum(is.na(data))
# Sumarul statisticilor pentru variabilele numerice
summary(data)

#!!!!!!!!!!!!!!!!!!!!!!!!!!#
# Calculul IQR pentru preț
Q1_price <- quantile(data$price, 0.25)
Q3_price <- quantile(data$price, 0.75)
IQR_price <- Q3_price - Q1_price

# Calculul IQR pentru suprafața locuibilă
Q1_area <- quantile(data$area_living_m2, 0.25)
Q3_area <- quantile(data$area_living_m2, 0.75)
IQR_area <- Q3_area - Q1_area


factor_extensie <- 3 

# Definirea noilor limite pentru outlieri
lower_bound_price_ext <- Q1_price - factor_extensie * IQR_price
upper_bound_price_ext <- Q3_price + factor_extensie * IQR_price
lower_bound_area_ext <- Q1_area - factor_extensie * IQR_area
upper_bound_area_ext <- Q3_area + factor_extensie * IQR_area

# Identificarea outlierilor cu noile limite
outliers <- data$price < lower_bound_price_ext | data$price > upper_bound_price_ext | 
  data$area_living_m2 < lower_bound_area_ext | data$area_living_m2 > upper_bound_area_ext


# Eliminarea outlierilor
cln_data <- data[!outliers, ]

#!!!!!!!!!!!!!!!!!!!!!!!!!!#

## Histogramă pentru preț
ggplot(cln_data, aes(x = price)) +
  geom_histogram(bins = 70, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribuția Prețurilor Proprietăților",
       x = "Preț",
       y = "Frecvență")

## Boxplot pentru numărul de dormitoare
ggplot(cln_data, aes(x = "", y = bedrooms)) +
  geom_boxplot(fill = "orange", color = "black") +
  theme_minimal() +
  labs(title = "Distribuția Numărului de Dormitoare",
       x = "",
       y = "Număr de Dormitoare")

## Boxplot pentru numărul de băi
ggplot(cln_data, aes(x = "", y = bathrooms)) +
  geom_boxplot(fill = "green", color = "black") +
  theme_minimal() +
  labs(title = "Distribuția Numărului de Băi",
       x = "",
       y = "Număr de Băi")

## Scatterplot pentru relația între suprafața locuibilă și preț
ggplot(cln_data, aes(x = area_living_m2, y = price)) +
  geom_point(color = "red") +
  theme_minimal() +
  labs(title = "Relația între Suprafața Locuibilă și Preț",
       x = "Suprafața Locuibilă (m²)",
       y = "Preț")

# Calculul matricei de corelație pentru variabilele numerice
cor_matrix <- cor(cln_data[, sapply(cln_data, is.numeric)])

# Afișarea matricei de corelație
print(cor_matrix)


# Transformarea matricei de corelație pentru vizualizare
melted_cor_matrix <- melt(cor_matrix)

# Transformarea matricei de corelație pentru vizualizare
melted_cor_matrix <- melt(cor_matrix)

# Crearea heat map-ului pentru matricea de corelație cu valorile numerice
ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), vjust = 1) + # Adaugă valorile numerice
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = '', y = '', title = 'Matricea de Corelație')

#SELECTAREA VARIABILELOR
library(MASS)
install.packages('MASS')
# Eliminarea coloanei 'country' folosind subset()
clean_data <- subset(cln_data, select = -country)
# Model inițial cu toate variabilele
model_initial <- lm(price ~ ., data = clean_data)

# Aplicarea metodei stepwise pentru a selecta variabilele
model_stepwise <- stepAIC(model_initial, direction = "both")
summary(model_stepwise)

#DIVIZAREA SETULUI DE DATE
install.packages('caret')
library(caret)

# Setarea unui seed pentru reproducibilitate
set.seed(123)

# Împărțirea setului de date în seturi de antrenare și testare
index <- createDataPartition(clean_data$price, p = 0.8, list = FALSE)
train_data <- clean_data[index, ]
test_data <- clean_data[-index, ]

# Crearea variabilelor dummy pentru 'city' în setul de antrenare
train_data_dummy <- dummyVars("~ .", data = train_data)
train_data_transformed <- data.frame(predict(train_data_dummy, train_data))

# Crearea variabilelor dummy pentru 'city' în setul de testare
test_data_dummy <- dummyVars("~ .", data = test_data, levelsOnly = TRUE, labels = levels(train_data_dummy))
test_data_transformed <- data.frame(predict(test_data_dummy, test_data))

# Asigurarea că ambele seturi de date au aceleași coloane
common_vars <- intersect(names(train_data_transformed), names(test_data_transformed))
train_data_transformed <- train_data_transformed[, common_vars]
test_data_transformed <- test_data_transformed[, common_vars]

# Readaugarea coloanei 'price' în seturile de date transformate
train_data_transformed$price <- train_data$price
test_data_transformed$price <- test_data$price


#CREAREA MODELULUI FINAL DE REGRESIE LINIARA
# Crearea modelului de regresie liniară folosind variabilele selectate
final_model <- lm(price ~ . - area_above_m2 - yr_renovated, data = train_data_transformed)

# Sumarizarea modelului
summary(final_model)



# Predicții pe setul de test
predictions <- predict(final_model, newdata = test_data_transformed)

# Evaluarea performanței modelului
actual_vs_predicted <- data.frame(Actual = test_data_transformed$price, Predicted = predictions)
head(actual_vs_predicted)

ggplot(actual_vs_predicted, aes(x = Actual, y = Predicted)) +
  geom_point(color = 'blue') +
  geom_abline(intercept = 0, slope = 1, color = 'red', linetype = 'dashed') +
  theme_minimal() +
  labs(title = "Compararea Valorilor Reale cu Cele Prezise",
       x = "Valori Reale",
       y = "Valori Prezise")


