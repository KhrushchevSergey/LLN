################################################################################
set.seed(123)  # Устанавливаем начальное значение для воспроизводимости

# Функция для подбрасывания монеты
flip_coin <- function(n) {
  # Подбрасывание монеты
  coin_flips <- sample(c("орел", "решка"), n, replace = TRUE)
  
  # Создаем таблицу с относительной частотой
  freq_table <- table(coin_flips) / n * 100  # Относительная частота в процентах
  
  # Создаем вектор с нулями для значений "орел" и "решка"
  result_freq <- c(орел = 0, решка = 0)  # Инициализация с именами
  
  # Заполняем вектор частот
  result_freq[names(freq_table)] <- freq_table
  
  return(result_freq)
}

# Подбрасывание монеты с разным количеством бросков
results_list <- lapply(c(10, 100, 1000, 10000), flip_coin)

# Графики и таблицы
par(mfrow = c(2, 2))
# Имена для графиков
names_list <- c("10 бросках", "100 бросках", "1000 бросках", "10000 бросках")

for (i in 1:4) {
  barplot(results_list[[i]], main = paste("Относительная частота при", names_list[i]), 
          ylim = c(0, 100), col = c("skyblue", "salmon"),
          names.arg = c("Орел", "Решка"), ylab = "Частота (%)")
}

# Создаем таблицу результатов
all_results <- data.frame(Исход = character(0),
                          Количество_бросков = integer(0),
                          Количество_выпадений = integer(0),
                          Процент = numeric(0))

# Объединяем результаты в одну таблицу
for (i in 1:4) {
  n <- c(10, 100, 1000, 10000)[i]
  result <- data.frame(Исход = names(results_list[[i]]),
                       Количество_бросков = n,
                       Количество_выпадений = results_list[[i]]/100*n,
                       Процент = round(results_list[[i]], 1))
  print(result)
  all_results <- rbind(all_results, result)
}

print(all_results)
################################################################################
# Функция для бросания игрального кубика
roll_dice <- function(n) {
  dice_rolls <- sample(1:6, n, replace = TRUE)
  freq_table <- table(dice_rolls) / n * 100  # Относительная частота в процентах
  
  # Создаем вектор с нулями для значений от 1 до 6
  result_freq <- rep(0, 6)
  names(result_freq) <- 1:6
  
  # Заполняем вектор частот
  result_freq[names(freq_table)] <- freq_table
  
  return(result_freq)
}

set.seed(123)  # Устанавливаем начальное значение для воспроизводимости
# Подбрасывание кубика с разным количеством бросков
n_list <- c(10, 100, 1000, 10000)
results_dice_list <- lapply(n_list, roll_dice)

# Графики и таблицы
par(mfrow = c(2, 2))  # Установим 2x2 графика
for (i in 1:length(results_dice_list)) {
  barplot(results_dice_list[[i]], 
          main = paste("Относительная частота при", names_list[i]), 
          ylim = c(0, 35), 
          col = rainbow(6), 
          names.arg = 1:6, 
          ylab = "Частота (%)")
}


# Создаем таблицу результатов
all_results <- data.frame(Исход = character(0),
                          Количество_бросков = integer(0),
                          Количество_выпадений = integer(0),
                          Процент = numeric(0))

# Объединяем результаты в таблицу
for (i in 1:4) {
  n <- c(10, 100, 1000, 10000)[i]
  result <- data.frame(Исход = names(results_dice_list[[i]]),
                       Количество_бросков = n,
                       Количество_выпадений = results_dice_list[[i]] / 100 * n,
                       Процент = round(results_dice_list[[i]], 1))
  all_results <- rbind(all_results, result)
}

print(all_results)
################################################################################
# Функция для бросания двух кубиков
roll_two_dice <- function(n) {
  # Бросаем два кубика
  dice_rolls_one <- sample(1:6, n, replace = TRUE)
  dice_rolls_two <- sample(1:6, n, replace = TRUE)
  dice_sum <- dice_rolls_one + dice_rolls_two
  
  # Создаем таблицу с относительной частотой
  freq_table <- table(dice_sum) / n * 100  # Относительная частота в процентах
  
  # Создаем вектор с нулями для сумм от 2 до 12
  result_freq <- rep(0, 11)  # Суммы от 2 до 12: 11 возможных значений
  names(result_freq) <- 2:12  # Именуем вектор от 2 до 12
  
  # Заполняем вектор частот
  result_freq[names(freq_table)] <- freq_table
  
  return(result_freq)
}


set.seed(123)  # Устанавливаем начальное значение для воспроизводимости
# Подбрасывание двух кубиков с разным количеством бросков
n_list <- c(10, 100, 1000, 10000)
results_two_dice_list <- lapply(n_list, roll_two_dice)

results_two_dice_list

# Графики и таблицы
par(mfrow = c(2, 2))  # Установим 2x2 графика
for (i in 1:length(results_two_dice_list)) {
  # Получаем все названия от 2 до 12
  bar_data <- as.numeric(results_two_dice_list[[i]])
  bar_names <- 2:12
  
  # Если в результате есть отсутствующие значения, их нужно заполнить нулями
  freq <- rep(0, length(bar_names))
  names(freq) <- bar_names
  freq[names(results_two_dice_list[[i]])] <- bar_data
  
  barplot(freq,
          main = paste("Относительная частота сумм при", names_list[i]),
          ylim = c(0, 30), 
          col = rainbow(length(bar_names)), 
          names.arg = bar_names, 
          ylab = "Частота (%)")
}

# Создаем таблицу результатов
all_results <- data.frame(Исход = character(0),
                          Количество_бросков = integer(0),
                          Количество_выпадений = integer(0),
                          Процент = numeric(0))

# Объединяем результаты в одну таблицу
for (i in 1:4) {
  n <- c(10, 100, 1000, 10000)[i]
  result <- data.frame(Исход = 2:12,
                       Количество_бросков = n,
                       Количество_выпадений = results_two_dice_list[[i]]/100*n,
                       Процент = round(results_two_dice_list[[i]], 1))
  all_results <- rbind(all_results, result)
}

print(all_results)
################################################################################
