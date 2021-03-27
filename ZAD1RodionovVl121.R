# Родионов Владимир ПАЭ-121, вариант 12- для региона 58 рассчитайте урожайность пшеницы 
#в 2002 году, взяв для рассчета средние суммы
# активных температур за предыдущие 5 лет
#с метеостанций в радиусе не более 140 км
# проверяем рабочую дирректорию
rm(list=ls())
getwd()

#Устанавливаем пакеты
library(tidyverse)
library(rnoaa)
library(lubridate)

# скачиваем список метеостанций
station_data = ghcnd_stations() 
write.csv(station_data,"station_data.csv")
station_data = read.csv("station_data.csv")
station_data

#После получения списка всех станций, выберем из него список станций ближайших к Пензе,
#создав таблицу с именем региона и координатами его столицы
Penza = data.frame(id  = "Penza",latitude= 53.2007, longitude = 45.0046)

# выбираем метеостанции в фиксированном радиусе от Пензы в заданный временной период
Penza_around=meteo_nearby_stations(lat_lon_df = Penza, station_data = station_data, radius = 140,
                                   var = c("TAVG"), year_min = 1997, year_max = 2001)

#получаем индентификатор метеостанции Пензы
Penza_id=Penza_around[["Penza"]][["id"]][1]
summary(Penza_id)

#чтобы получить таблицу всех метеостанций вокруг 
#Пензы нужно выбрать целиком первый объект из списка
Penza_table=Penza_around[[1]]
summary(Penza_table)


#нужно убедится, что этот список включает нужные по условию задачи метеостанции
Penza_stations= Penza_table
str(Penza_stations)

#Таким образом, мы сформировали список необходимых станций, посмотрим, что он содержит
Penza_stations$Penza.id

# Создаем цикл, в который будут скачиваются необходимые данные с метеостанций 
# Промежуточный объект, куда скачиваются данные с кокретной метеостанции
all_i = data.frame()

# Объект куда скачиваются все данные со всех метеостанций
all_Penza_meteodata = data.frame()

# Цикл для всех метеостанций
for(i in 1:6)
{
  Penza_id =  Penza_around[["Penza"]] [["id"]] [ i]
  data = meteo_tidy_ghcnd(stationid = Penza_id,
                          var = "TAVG",
                          date_min = "1997-01-01",
                          date_max = "2001-12-31")
  all_Penza_meteodata =  bind_rows(all_Penza_meteodata, data)
}


# Записываем полученные данных в файл, из которого их можно считывать и просматривать
write.csv(all_Penza_meteodata, "all_Penza_meteodata.csv")

#Cчитываем данные из файла all_penza_meteodata.csv
all_Penza_meteodata = read.csv("all_Penza_meteodata.csv")

#посмотрим на данные
str(all_Penza_meteodata)

#  Добавим год, месяц,день. Это надо сделать чтобы получить результаты за конкретный период времени
all_Penza_meteodata = mutate(all_Penza_meteodata, year = year(date), month = month(date), day = day(date))
#проверим результат
str(all_Penza_meteodata)

# Выведем данные за 1997 - 2001 годы
years_Penza_meteodata = filter(all_Penza_meteodata, year %in% c( 1997:2001))

#  Проверим результат
str(years_Penza_meteodata)
summary(years_Penza_meteodata)

#Средняя (по годам и метеостанциям) сумма активных температур за месяц
# Приведение средней суммы температур в подходящую форму, при помощи деления на 10
years_Penza_meteodata[,"tavg"] = years_Penza_meteodata$tavg/10

# Превратим в нули все NA и где 5<tavg>30
years_Penza_meteodata[is.na(years_Penza_meteodata$tavg),"tavg"] = 0
years_Penza_meteodata[years_Penza_meteodata$tavg<5, "tavg"] = 0

#проверяем, что температура получилась или 0, или больше 5 градусов
summary(years_Penza_meteodata)

#Группируем по метеостанциям, годам и месяцам
alldays = group_by(years_Penza_meteodata, id, year, month)

#Вычислим сумма температуру по этим группам  
sumT_alldays_Penza = summarize(alldays, tsum = sum(tavg))
summary(sumT_alldays_Penza) 

#Группировка данных по месяцам  
groups_Penza_months = group_by(sumT_alldays_Penza, month); groups_Penza_months

#Расчет среднего по месяцам для всех метеостанций и всех лет
sumT_months=summarize(groups_Penza_months,St=mean(tsum))
sumT_months

# Расчет урожайности
#Ввод констант для расчета урожайности
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)

#Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
y = 1.0
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "сумма частей основной и побочной продукции"
Lj = 2.2
#Коэффициент "стандартная влажность культуры"
Ej = 25 
#Рассчет Fi по месяцам
sumT_months = mutate(sumT_months, Fi = afi + bfi * y * St)

#Рассчет Yi
sumT_months = mutate(sumT_months, Yi = ((Fi * di) * Kf) / (Qj * Lj * (100 - Ej)))
#Расчет урожая, как сумму по месяцам
Yield = sum(sumT_months$Yi)
Yield

