# pacotes utilizados
library(tidyverse)

# input de dados

setwd("~/R/who_suicide")

data <- read.csv('who_suicide_statistics.csv')

# checando as colunas iniciais

head(data)

# checando os valores NA

summary(data)

# agrupando por país top 10 e plotando

graf1 <- data %>%
     group_by(country) %>% 
     summarise(total_suicides = sum(suicides_no, na.rm = TRUE)) %>% 
     arrange(desc(total_suicides)) %>% 
     top_n(10)


p1 <- ggplot(data = graf1) +
     geom_bar(
          mapping = aes(x = reorder(country, -total_suicides),
                        y = total_suicides, fill = total_suicides),
                        stat = "identity")


p1 + ggtitle("Nº de suícidios por País") +
     ylab("Número total de suícidios") +
     xlab("Países") +
     theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

# agrupando o número total de suicídios por ano

graf2 <- data %>%
        group_by(year) %>%
        summarise(total_suicides = sum(suicides_no, na.rm = TRUE)) %>% 
        arrange(desc(total_suicides))

p2 <- ggplot(data = graf2) +
        geom_col(mapping = aes(x = year,
                               y = total_suicides,
                               fill = total_suicides))

p2 + ggtitle("Total de suícidios por ano") +
        ylab("Número total de suícidios") +
        xlab("Anos")

# total de suicídios no Brasil

graf3 <- data %>% 
        filter(country == "Brazil") %>% 
        group_by(year) %>% 
        summarise(total_suicides = sum(suicides_no, na.rm = TRUE)) %>% 
        arrange(desc(total_suicides))
        
p3 <- ggplot(data = graf3) +
        geom_col(mapping = aes(x = year,
                               y = total_suicides,
                               fill = total_suicides))

p3 + ggtitle("Número total de Suícidios no Brasil") +
        ylab("Número total de suícidios") +
        xlab("Anos")

# Total de suicídios no Brasil por ano - Sexo

graf4 <- data %>% 
        filter(country == "Brazil") %>% 
        group_by(year, sex) %>% 
        summarise(total_suicides = sum(suicides_no, na.rm = TRUE)) %>% 
        arrange(desc(total_suicides))

p4 <- ggplot(data = graf4) +
        geom_col(mapping = aes(x = year,
                               y = total_suicides,
                               fill = sex))

p4 + ggtitle("Total de suícidios anuais - Sexo") +
        ylab("Número total de suícidios") +
        xlab("Anos")

# Total de suícidios no Brasil por ano - Faixa Etária

graf5 <- data %>% 
        filter(country == "Brazil") %>% 
        group_by(year, age) %>% 
        summarise(total_suicides = sum(suicides_no, na.rm = TRUE)) %>% 
        arrange(desc(total_suicides))

p5 <- ggplot(data = graf5) +
        geom_col(mapping = aes(x = year,
                               y = total_suicides,
                               fill = age))

p5 + ggtitle("Total de suícidios anuais - Sexo") +
        ylab("Número total de suícidios") +
        xlab("Anos")

# Total de suicidios por sexo e faixa etaria

graf6 <-  data %>% 
        filter(country == "Brazil") %>% 
        group_by(sex, age, year) %>% 
        summarise(total_suicides = sum(suicides_no, na.rm = TRUE))

p6 <- ggplot(data = graf6) +
        geom_col(mapping = aes(x = factor(age, levels = c("5-14 years",
                                                          "15-24 years",
                                                          "25-34 years",
                                                          "35-54 years",
                                                          "55-74 years",
                                                          "75+ years")),
                               y = total_suicides,
                               fill = sex))

p6 + ggtitle("Total de suícidios por sexo e faixa etária") +
        ylab("Número total de suícidios") +
        xlab("Faixas Etárias")

# Taxa de suicidios na população

graf7 <- data %>% 
        filter(country == "Brazil") %>% 
        group_by(sex, age, year) %>% 
        summarise(suicidesperpop = sum(suicides_no/population), na.rm = TRUE)

p7 <- ggplot(data = graf7) +
        geom_col(mapping = aes(x = factor(age, levels = c("5-14 years",
                                                          "15-24 years",
                                                          "25-34 years",
                                                          "35-54 years",
                                                          "55-74 years",
                                                          "75+ years")),
                               y = suicidesperpop,
                               fill = sex))

p7 + ggtitle("Taxa de suícidios por sexo e faixa etária") +
        ylab("Número total de suícidios") +
        xlab("Faixas Etárias")
