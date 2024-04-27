library(dplyr)
library(ggplot2)

data <- read.csv('/Users/Viktoriia/Downloads/Parties2019.csv', sep=";")

str(data)

data$Date <- as.Date.character(data$Date, "%d.%m.%Y")
data$AccountType <- as.factor(data$AccountType)
data$SUM <- as.numeric(data$SUM)
data$Reimburse <- as.integer(data$Reimburse)
data$DonorType <- as.factor(data$DonorType)
data$Party <- as.factor(data$Party)
data$BrunchRegion <- as.character(data$BrunchRegion)
data$Quarter <- as.factor(data$Quarter)
data$InsideParty <-as.factor(data$InsideParty)

summary(data)

split_address <- strsplit(data$Address, " ")
data$Region <- sapply(split_address, function(x) 
  paste(x[1], collapse = " "))
data$Region <- gsub("NA", "", data$Region)
data$Region <- gsub(",", "", data$Region)
data$Region <-as.factor(data$Region)
summary(data$Region)

data <- data %>%
  filter(!is.na(SUM), SUM>0, format(Date, "%Y") == "2019", 
         InsideParty != "всередині регіону", 
         InsideParty != "між регіонами", 
         AccountType=="поточний рахунок", 
         Region !="", DonorName != "ЦЕНТРАЛЬНА ВИБОРЧА КОМІСІЯ") 

Mode <- function(x) {
  uniq_x <- unique(x) 
  freq_x <- table(x)  
  max_freq <- max(freq_x) 
  mode_val <- uniq_x[freq_x == max_freq]
  return(mode_val)
}

summary_table <- data %>%
  group_by(Party) %>%
  summarise(
    Count = n(),
    Average = mean(SUM, na.rm = TRUE),
    Median = median(SUM, na.rm = TRUE),
    Mode = Mode(SUM), 
    Maximum = max(SUM, na.rm = TRUE),
    Minimum = min(SUM, na.rm = TRUE)
  )
print(summary_table)

MaxSum <- data %>%
  group_by(Party) %>%
  reframe(MaxSum = max(SUM, na.rm = TRUE)) %>%
  left_join(data, by = "Party") %>%
  filter(SUM == MaxSum) %>%
  arrange(desc(MaxSum)) 

MaxSum

ggplot(summary_table, aes(x=Party, y=Average, fill=Party)) +
  geom_col() +
  labs(title = "Середня сума внеску за партіями", x = "Партія", y = "Середня сума",
       fill="Партія") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::comma)+
  scale_fill_manual(values = c('Всеукраїнське обʼєднання "Батьківщина"' = "#BB0600", 
                               "Європейська солідарність" = "#13769E", "Слуга народу" = "#008B02", 
                               "Опозиційна платформа - За життя" = "#442656", "Голос" = "#FFB947"))+
  theme(plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1"),
        legend.background = element_rect(fill = "#E9F2F1"),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(family = "Arial", face = "bold",color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold",color = "#8256BF"))

sum <- data %>%
  group_by(Party) %>%
  arrange(Party, Date) %>%
  mutate(cumulative_contribution = cumsum(SUM))

ggplot(sum, aes(x = Date, y = cumulative_contribution, group = Party, color = Party)) +
  geom_line() +
  scale_x_date(date_breaks = "1 month", date_labels = "%m") +
  theme_minimal() +
  labs(title = "Наростаючий підсумок внесків за партіями", 
       x = "Дата", 
       y = "Наростаючий підсумок внесків",
       color = "Партія") + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c('Всеукраїнське обʼєднання "Батьківщина"' = "#BB0600", 
                               "Європейська солідарність" = "#13769E", "Слуга народу" = "#008B02", 
                               "Опозиційна платформа - За життя" = "#442656", "Голос" = "#FFB947")) +
  theme(plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1", color = NA),
        legend.background = element_rect(fill = "#E9F2F1", color = NA), 
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(family = "Arial", face = "bold", color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold", color = "#8256BF"))

ggplot(data, aes(x = SUM, fill = Party)) +
  geom_histogram(bins = 100, position = "identity", alpha = 1) +
  labs(title = "Розподіл внесків за сумами для кожної партії",
       x = "Сума внеску",
       y = "Кількість внесків",
       fill = "Партія")+
  scale_fill_manual(values = c('Всеукраїнське обʼєднання "Батьківщина"' = "#BB0600", 
                               "Європейська солідарність" = "#13769E", "Слуга народу" = "#008B02", 
                               "Опозиційна платформа - За життя" = "#442656", "Голос" = "#FFB947"))+
  theme(plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1"),
        legend.background = element_rect(fill = "#E9F2F1"),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(family = "Arial", face = "bold",color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold",color = "#8256BF"))+
  scale_x_continuous(labels = scales::comma)

q1 <- quantile(data$SUM, 0.25)
q3 <- quantile(data$SUM, 0.75)
iqr <- q3 - q1
threshold <- 1.5 * iqr 

large_SUM_df <- data %>%
  filter(SUM > threshold)

filtered_data <- data %>%
  filter(SUM <= threshold)

ggplot(filtered_data, aes(x = Date, y = SUM, color = Party)) +
  geom_point(size = 1) +
  theme_minimal() +
  labs(title = "Суми внесків за датами",
       x = "Дата",
       y = "Сума внеску",
       color="Партія")+
  scale_color_manual(values = c('Всеукраїнське обʼєднання "Батьківщина"' = "#BB0600", 
                                "Європейська солідарність" = "#13769E", "Слуга народу" = "#008B02", 
                                "Опозиційна платформа - За життя" = "#442656", "Голос" = "#FFB947"))+
  theme(plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1", color = NA),
        legend.background = element_rect(fill = "#E9F2F1", color = NA),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(family = "Arial", face = "bold",color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold",color = "#8256BF"))

ggplot(large_SUM_df, aes(x = Date, y = SUM, color = Party)) +
  geom_point(size = 1) +
  theme_minimal() +
  labs(title = "Суми великих внесків за датами",
       x = "Дата",
       y = "Сума внеску",
       color="Партія")+
  scale_color_manual(values = c('Всеукраїнське обʼєднання "Батьківщина"' = "#BB0600", 
                                "Європейська солідарність" = "#13769E", "Слуга народу" = "#008B02", 
                                "Опозиційна платформа - За життя" = "#442656", "Голос" = "#FFB947"))+
  theme(plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1", color = NA),
        legend.background = element_rect(fill = "#E9F2F1", color = NA),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(family = "Arial", face = "bold",color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold",color = "#8256BF"))+
  scale_y_continuous(labels = scales::comma)

ggplot(filtered_data, aes(x = Region, y = SUM, color = Party)) +
  geom_point(size = 1, position = position_jitter(width = 0.2, height = 0)) +
  theme_minimal() +
  labs(title = "Розподіл внесків за областями для кожної партії",
       x = "Область",
       y = "Сума внеску",
       color = "Партія") +
  scale_color_manual(values = c('Всеукраїнське обʼєднання "Батьківщина"' = "#BB0600", 
                                "Європейська солідарність" = "#13769E", "Слуга народу" = "#008B02", 
                                "Опозиційна платформа - За життя" = "#442656", "Голос" = "#FFB947"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1", color = NA),
        legend.background = element_rect(fill = "#E9F2F1", color = NA),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(family = "Arial", face = "bold",color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold",color = "#8256BF"))


ggplot(large_SUM_df, aes(x = Region, y = SUM, color = Party)) +
  geom_point(size = 1, position = position_jitter(width = 0.2, height = 0)) +
  theme_minimal() +
  labs(title = "Розподіл великих внесків за областями для кожної партії",
       x = "Область",
       y = "Сума внеску",
       color = "Партія") +
  scale_color_manual(values = c('Всеукраїнське обʼєднання "Батьківщина"' = "#BB0600", 
                                "Європейська солідарність" = "#13769E", "Слуга народу" = "#008B02", 
                                "Опозиційна платформа - За життя" = "#442656", "Голос" = "#FFB947"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1", color = NA),
        legend.background = element_rect(fill = "#E9F2F1", color = NA),
        panel.grid = element_line(color = "grey"),
        plot.title = element_text(family = "Arial", face = "bold",color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold",color = "#8256BF"))+
  scale_y_continuous(labels = scales::comma)

ggplot(data, aes(x = SUM,  color = Party)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Розподіл суми внесків", x = "Сума внеску", y = "Частота (логарифмічна шкала)")

support_by_region <- data %>%
  group_by(Party, Region) %>%
  summarize(Total_SUM = sum(SUM))
ggplot(support_by_region, aes(x = Region, y = Total_SUM, fill = Party)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Підтримка партій за регіонами", x = "Регіон", y = "Сумарний внесок")


data$Month <- as.factor(format(data$Date, "%m"))

model_party <- lm(SUM ~ Party + Month, data = data)
summary(model_party)

model_party <- lm(SUM ~ Party + DonorType + BrunchRegion + Region + Month, data = data)
summary(model_party)

split_BrunchRegion <- strsplit(data$BrunchRegion, " ")
data$BrunchRegion <- sapply(split_BrunchRegion, function(x) paste(x[1], collapse = " "))
data$BrunchRegion <- as.character(data$BrunchRegion)
data$Region <- as.character(data$Region)
data_mismatced <- data%>% filter(BrunchRegion !="Україна", DonorType =="фізична особа", DonationType=="грошовий внесок")
mismatched_donations <- filter(data_mismatced, Region != BrunchRegion)

mismatched_summary <- mismatched_donations %>%
  group_by(Party) %>%
  summarize(Total_SUM = sum(SUM), Count = n())

mismatched_summary

heatmap_data_all <- data %>%
  group_by(Party, Region, BrunchRegion) %>%
  summarize(Count = n()) %>%
  ungroup()

ggplot(heatmap_data_all, aes(x = Region, y = BrunchRegion, fill = Count)) +
  geom_tile(color = "white", size = 0.1) +
  scale_fill_gradient(low = "#73D9BC", high = "#4B2273") +
  facet_wrap(~ Party) + 
  labs(title = "Мапа регіонів донорів та осередків за партіями",
       x = "Область/місто донора",
       y = "Область/місто осередку",
       fill = "Кількість") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(angle = 0),
        plot.background = element_rect(fill = "#E9F2F1"),
        panel.background = element_rect(fill = "#E9F2F1", color = NA),
        legend.background = element_rect(fill = "#E9F2F1", color = NA),
        plot.title = element_text(family = "Arial", face = "bold",color = "#8256BF"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.title = element_text(family = "Arial", face = "bold",color = "#8256BF"))

