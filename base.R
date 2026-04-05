# Tabla QPC ##################################################################################
# Crear key larga (pais_anio inicio_anio fin_programa_review_QPC)
# Crear key corta (pais_anio_programa_review)
# Eliminar todas las filas que no digan descripcion en la columna Sub
# Eliminar todas las filas que no tengan en la col Status M, NM, W, WA
# Crear columna R con el valor del review
# Para igual valor en todas las restantes columnas, quedarse solo con las filas que Tengan el menor valor en la columna R

#library(readxl)
#QPC_11_ <- read_excel("C:/Users/HP/Desktop/Tesis UDESA/QPC (11).xlsx") #Tabla original de imf.org/mona

# Leanto base
library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/QPC.xlsx"
QPC_11 <- read.xlsx(url)

QPC_11_ <- QPC_11

str(QPC_11_)
colnames(QPC_11_)

# Creo keys

library(dplyr)

QPC_11_ <- QPC_11_ %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

QPC_11_ <- QPC_11_ %>%
  mutate(Key_larga = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, Criteria.Order, sep = "_"))

QPC_11_  <- QPC_11_  %>%
  mutate(Key_min = paste(Country.Code , Arrangement.Number, sep = "_"))

QPC_11_  <- QPC_11_  %>%
  relocate(Key_min, .before = 3)


# Elimino etiquetas inecesarias

df_filtrado <- QPC_11_ %>%
  filter(
    grepl("Description", Sub, ignore.case = TRUE),
    grepl("NM|M|W|WA|WM|Mod", Status, ignore.case = TRUE)  # "|" means OR in regex
  )

df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Sub")]


# Creo variable R (Review number)

library(stringr)

QPC <- df_filtrado %>%
  mutate(R = as.numeric(str_extract(Review.Type, "\\d+"))) %>%
  relocate(R, .after = 11)

str(QPC)

QPC_final <- QPC %>%
  group_by(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Criteria.Order, Status, Test.Date, Program.Amount) %>%
  filter(R == min(R)) %>%
  ungroup()

# Elimino variables innecesarias

QPC_final <- QPC_final[, -which(names(QPC_final) == "is.IT.Test.Date?")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Grouping")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Comments")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Program.Type")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Board.Document.Number")]
QPC_final <- QPC_final[, -which(names(QPC_final) == "Dqpc.Code")]

# Ordeno variables en el set

QPC_final <- QPC_final %>%
  relocate(Key_corta, .before = 1)

QPC_final <- QPC_final %>%
  relocate(Key_larga, .before = 1)

# Variable Review Sequence (Last)

unique(QPC_final$Review.Sequence)

QPC_final <- QPC_final %>%
  mutate(Review.Sequence = trimws(Review.Sequence))

QPC_final <- QPC_final %>%
  mutate(Review.Sequence = ifelse(Review.Sequence == "L", 1, 0))

# Elimino NA

QPC_final <- QPC_final %>%
  mutate(Actual.Amount = ifelse(is.na(Actual.Amount), Adjusted.Amount, Actual.Amount))


# Formato fecha

QPC_final$Test.Date <- format(as.Date(QPC_final$Test.Date, format = "%m/%d/%Y"), "%d/%m/%Y")

colnames(QPC_final)


# Elimina errores de columna Units

QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("SDR (millions)",
                   "SDR millions",
                   "SDR (in million)",
                   "SDR (in millions)",
                   "SRD (in million)",
                   "SRD (in millions)") ~ "SDR_millions",
      TRUE ~ Units
    )
  )


QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("US$(billions)",
                   "USD(billions)",
                   "US$ (billions)",
                   "US$(bmllions)",
                   "US$") ~ "USD_billions",
      TRUE ~ Units
    )
  )

QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("US$(millions)",
                   "US$(mllions)",
                   "USD(millions)",
                   "US$(Millions)",
                   "US$ (millions)",
                   "US$(million)",
                   "None",
                   " US$(millions)",
                   " US$(millions) ",
                   "US$(millions) ") ~ "USD_millions",
      TRUE ~ Units
    )
  )

QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("NC (millions)",
                   "NCU(millions)",
                   "NCU (millions)",
                   "NCU (millions)",
                   "Euros(millions)",
                   "Euro(millions)",
                   "KM(millions)",
                   "KM(millions)",
                   "Millions of national currency",
                   "Rufiyaa (millions)",
                   "CGF (millions)",
                   "LCU(millions)",
                   "Euros (millions)",
                   "KM (millions)",
                   "NC(millions)",
                   "Rufiyaa(millions)",
                   "Euors(millions)",
                   "NUC(millions)",
                   "LCU(Millions)",
                   "NC$(millions)",
                   "rufiyaa(millions)",
                   "LCU(million)",
                   "million rupees",
                   "euros(millions)") ~ "NC_millions",
      TRUE ~ Units
    )
  )


QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("NC (billions)",
                   "NC(billions)",
                   "NC$ (billions)",
                   "NCU(billions)",
                   "Euro (billions)",
                   "CFAF (billions)",
                   "rufiyaa(millions)",
                   "Rwandan franc(billions)",
                   "Tsh billions",
                   "NCU (billions)",
                   "NCU$ (billions)",
                   "Euros (billions)",
                   "Euro(billions)",
                   "CFAF(billions)",
                   "Euros(billions)",
                   "AMD(billions)",
                   "LCU(billions)",
                   "Dinars (billions)",
                   "NC (billions)",
                   " NCU (billions) ",
                   " NCU(billions)  ",
                   " NC (billions) ",
                   "NCU (billions) ",
                   "NCU(billions) ",
                   " NC(billions) ",
                   " NC (billions)",
                   " NCU(billions) ",
                   " Euros(billions) ") ~ "NC_billions",
      TRUE ~ Units
    )
  )


QPC_final <- QPC_final %>%
  mutate(
    Units = case_when(
      Units %in% c("percent", "Percent") ~ "Percent",
      TRUE ~ Units
    )
  )

unique(QPC_final$Units)


QPC_final <- QPC_final %>%
  mutate(Units_USD_bill = if_else(grepl("USD_billions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_USD_mill = if_else(grepl("USD_millions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_NC_bill = if_else(grepl("NC_billions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_NC_mill = if_else(grepl("NC_millions", Units, ignore.case = TRUE), 1, 0))

QPC_final <- QPC_final %>%
  mutate(Units_SDR_mill = if_else(grepl("SDR_millions", Units, ignore.case = TRUE), 1, 0))

colnames(QPC_final)

QPC_final$Program.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Program.Amount) / 1000, QPC_final$Program.Amount)
QPC_final$Revised.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Revised.Amount) / 1000, QPC_final$Revised.Amount)
QPC_final$Adjusted.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Adjusted.Amount) / 1000, QPC_final$Adjusted.Amount)
QPC_final$Actual.Amount <- ifelse(QPC_final$Units == "USD_millions", as.numeric(QPC_final$Actual.Amount) / 1000, QPC_final$Actual.Amount)

QPC_final$Program.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Program.Amount) / 1000, QPC_final$Program.Amount)
QPC_final$Revised.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Revised.Amount) / 1000, QPC_final$Revised.Amount)
QPC_final$Adjusted.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Adjusted.Amount) / 1000, QPC_final$Adjusted.Amount)
QPC_final$Actual.Amount <- ifelse(QPC_final$Units == "NC_millions", as.numeric(QPC_final$Actual.Amount) / 1000, QPC_final$Actual.Amount)

QPC_final$Program.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Program.Amount) / 1000, QPC_final$Program.Amount)
QPC_final$Revised.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Revised.Amount) / 1000, QPC_final$Revised.Amount)
QPC_final$Adjusted.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Adjusted.Amount) / 1000, QPC_final$Adjusted.Amount)
QPC_final$Actual.Amount <- ifelse(QPC_final$Units == "SDR_millions", as.numeric(QPC_final$Actual.Amount) / 1000, QPC_final$Actual.Amount)


QPC_final <- QPC_final %>%
  filter(!is.na(Program.Amount) & Program.Amount != "")  

colnames(QPC_final)

# Creo dummies para tipo de programa

library(fastDummies)

QPC_final <- QPC_final %>%
  arrange(Arrangement.Type) %>%
  dummy_cols(select_columns = "Arrangement.Type")

# Elimino columnas inecesarias

QPC_final <- QPC_final %>% select(-Country.Name)
QPC_final <- QPC_final %>% select(-Main)
QPC_final <- QPC_final %>% select(-Main.Criteria.Sequence)
QPC_final <- QPC_final %>% select(-Sub.Option)
QPC_final <- QPC_final %>% select(-Approval.Year)
#QPC_final <- QPC_final %>% select(-Approval.Date)
QPC_final <- QPC_final %>% select(-Initial.End.Year)
QPC_final <- QPC_final %>% select(-Initial.End.Date)
QPC_final <- QPC_final %>% select(-Review.Sequence)
#QPC_final <- QPC_final %>% select(-Arrangement.Type)

# Ordeno key

QPC_final <- QPC_final %>%
  relocate(Key_min, .after = 2)

# Elimina filas duplicadas

QPC_final  <- QPC_final %>% distinct()

# Pasar a excel

library(writexl)
write_xlsx(QPC_final, "C:/Users/HP/Desktop/Tesis UDESA/QPC_final.xlsx")


# Tabla Purchases ##################################################################################
# Crear key (corta)
# Columna Review Sequence quedarme solo con los L
# Columna Original Basis sacar el numero que es la revision R del purchase
# Quedarme con el monto (original y actual)  del purchase para cada revision (ACTUAL BASIS)  y su fecha (original y actual).


# Levanto tabla purchases
#Purchases_8 <- read_excel("C:/Users/HP/Desktop/Tesis UDESA/Purchases.xlsx")


library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Purchases.xlsx"
Purchases_ <- read.xlsx(url)

Purchases <-  Purchases_

# Creo variacion purchase

Purchases <- Purchases %>%
  mutate(Var_Purchase = ((Actual.Amount - Original.Scheduled.Amount) / Original.Scheduled.Amount) * 100)

# Creo key

colnames(Purchases)

Purchases <- Purchases %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

Purchases <- Purchases %>%
  relocate(Key_corta, .before = 1)

# Creo R

Purch <- Purchases %>%
  mutate(R = as.numeric(str_extract(Review.Type, "\\d+"))) %>%
  relocate(R, .after = 15)

Purch <- Purch %>%
  mutate(Rbis = as.numeric(str_extract(Original.Basis, "\\d+"))) %>%
  relocate(Rbis, .after = 16)

Purch <- Purch %>%
  mutate(Rbis = replace(Rbis, is.na(Rbis), 0))

df_filtrado <- Purch %>%
  filter(R == Rbis)

colnames(df_filtrado)

# Elimino columnas

df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Country.Name")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Country.Code")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Approval.Year")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Approval.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Initial.End.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Initial.End.Year")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Duration.Of.Annual.Arrangement.From")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Duration.Of.Annual.Arrangement.To")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Set.Aside")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Augmentation")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Comments")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Sort")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Arrangement.Number")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Arrangement.Type")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Board.Action.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Original.Scheduled.PC.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Scheduled.PC.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.End.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Program.Type")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Scheduled.Amount")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Scheduled.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Revised.Basis")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Rbis")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Review.Type")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Review.Sequence")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Amount")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Date")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Actual.Basis")]
colnames(df_filtrado)
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "Original.Basis")]
df_filtrado <- df_filtrado[, -which(names(df_filtrado) == "R")]

df_filtrado  <- df_filtrado %>% distinct()

# Pasar a excel
library(writexl)
write_xlsx(df_filtrado, "C:/Users/HP/Desktop/Tesis UDESA/Purch_final.xlsx")

##################################################################################

# Left Join
anti_join(QPC_final, df_filtrado, by = "Key_corta") %>% count()
glimpse(QPC_final$Key_corta)
glimpse(df_filtrado$Key_corta)

sample(unique(QPC_final$Key_corta), 10)
sample(unique(df_filtrado$Key_corta), 10)

library(stringr)

tabla_combined <- QPC_final %>%
  mutate(Key_corta = str_trim(Key_corta)) %>%
  left_join(
    df_filtrado %>% mutate(Key_corta = str_trim(Key_corta)),
    by = "Key_corta"
  )

tabla_combined  <- tabla_combined %>% distinct()

colnames(tabla_combined)

# Elimno NAs

mean(is.na(tabla_combined$Original.Scheduled.Amount)) * 100
mean(is.na(tabla_combined$Original.Scheduled.Date)) * 100

tabla_combined$Original.Scheduled.Amount <- ifelse(is.na(tabla_combined$Original.Scheduled.Amount), 0, tabla_combined$Original.Scheduled.Amount)
tabla_combined$Original.Scheduled.Date <- ifelse(is.na(tabla_combined$Original.Scheduled.Amount), tabla_combined$Test.Date, tabla_combined$Original.Scheduled.Amount)

# Cambio nombre a columans

tabla_combined <- tabla_combined %>%
  rename(
    #Arrangement_type = Arrangement.Type,
    QPC_Original_Amount = Program.Amount,
    QPC_Test_Date = Test.Date,
    QPC_Units = Units,
    #QPC_Code = Dqpc.Code,
    QPC_Criteria_Order = Criteria.Order,
    Purchase_Original_Amount = Original.Scheduled.Amount,
    Purchase_Original_Date = Original.Scheduled.Date)

colnames(tabla_combined)


# Crear label

tabla_combined <- tabla_combined %>%
  mutate(
    Status = ifelse(Status == "M", 0, 1)
  )

tabla_combined <- tabla_combined %>%
  relocate(Status, .before = 1)

# Elimino duplicados

tabla_combined  <- tabla_combined %>% distinct()

# Pasar a excel
library(writexl)
write_xlsx(tabla_combined, "C:/Users/HP/Desktop/Tesis UDESA/QPC_Purch_final.xlsx")


# Tabla description ##################################################################################
# Crear la key (corta)
# Quedarse con columnas Total Access y Precautionary

library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Description.xlsx"
Description_13_ <- read.xlsx(url)

Description_13 <- Description_13_

colnames(Description_13)

# Key

Description_13 <- Description_13 %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

# variable fiscal year

library(fastDummies)

Description_13 <- Description_13 %>%
  arrange(Fycy) %>%
  dummy_cols(select_columns = "Fycy")

# variable purchase_revised

library(fastDummies)

Description_13 <- Description_13 %>%
  arrange(Purchaseschedule) %>%
  dummy_cols(select_columns = "Purchaseschedule")

# variable delay_by

library(fastDummies)

Description_13 <- Description_13 %>%
  arrange(Delayedby) %>%
  dummy_cols(select_columns = "Delayedby")


# Me quedo con las 2 variables importantes

Desc <- Description_13 %>%
  select(Key_corta, Totalaccess, Precautionary,
         Fycy_CY, Fycy_FY, 
         Purchaseschedule_Original, Purchaseschedule_Revised, Purchaseschedule_NA, 
         `Delayedby_More than 6 months`, `Delayedby_Three  to Six months`,
         `Delayedby_Up to  3 months`, Delayedby_NA)

# Cambio nombre a columans

Desc <- Desc %>%
  rename(
    Delay6 = `Delayedby_More than 6 months`,
    Delay3_6 = `Delayedby_Three  to Six months`,
    Delay3 = `Delayedby_Up to  3 months`)



# Variable precautionary

Desc <- Desc %>%
  mutate(`Precautionary` = ifelse(is.na(`Precautionary`) | trimws(`Precautionary`) == "", "N", `Precautionary`))

unique(Desc$Precautionary)

Desc <- Desc %>%
  mutate(`Precautionary` = ifelse(is.na(`Precautionary`) | trimws(`Precautionary`) == "Y", 1, 0))

# Joint

tabla <- tabla_combined %>%
  left_join(
    Desc %>%
      distinct(Key_corta, .keep_all = TRUE),# Elimina duplicados
    by = "Key_corta"
  )

# Elimino filas duplicadas

tabla_comb <- tabla %>% distinct()

# Elimino NA

mean(is.na(tabla_comb$Precautionary)) * 100
mean(is.na(tabla_comb$Totalaccess)) * 100

colnames(tabla_comb)

# Elimino columnas

tabla_comb <- tabla_comb[, -which(names(tabla_comb) == "Revised.Amount")]
tabla_comb <- tabla_comb[, -which(names(tabla_comb) == "Adjusted.Amount")]

# Pasar a excel
library(writexl)
write_xlsx(tabla_comb, "C:/Users/HP/Desktop/Tesis UDESA/QPC_Purch_Desc_final.xlsx")

# Tabla Mecon ##################################################################################
# Crear key (corta)
# Quedarse solo con las filas R0 (que muestra las variables macro de los tres anios previous al programa, y muestra los valores esperados de esas variables para los proximos 5 anios T a T=4)
# Sacar las tasas de cambio de cada variable macro de T respecto de T-1, de T=1 respecto de T, etc.
# Eliminar todas las columnas que no son la key, ni el Mneumonic, ni las variables macro
# Eliminar las filas que tienen porcentaje y blanks en la columna Indicator Currency
# Pivoteas. Filas ahora son las keys y columnas son cada una de las variables macro para cada momento del tiempo
# Eliminar variables que Tengan mas de 5% de missing values por columna. Asignar la mediana a esos missing values.
# Reexpresar variables macro para evitar nominalidad. Las variables en USD divider por BXG (exportaciones de Bienes) y multiplicar por 100. Las variables en moneda local, dividir por NGDP y multiplicar por 100. Los indices de precios dividir por ENDA (tipo de cambio). Los tipos de cambio dividir por indices de precios (PCPI).

library(openxlsx)
url <- "https://www.imf.org/external/np/pdr/mona/ArrangementsData/Mecon.xlsx"
Mecon_10_ <- read.xlsx(url)

Mecon_10 <- Mecon_10_

colnames(Mecon_10)

# Creo variables var escenario base y pre-programa

Mecon_10 <- Mecon_10 %>%
  mutate(var_t4_t = ((`T+4` - `T`) / `T`) * 100)

Mecon_10 <- Mecon_10 %>%
  mutate(var_t_t_3 = ((`T` - `T-3`) / `T-3`) * 100)


# Keys

df_filt <- Mecon_10

df_filt  <- df_filt  %>%
  mutate(Key_corta = paste(Country.Code , Approval.Year, Initial.End.Year, Arrangement.Number, Review.Type, sep = "_"))

df_filt  <- df_filt  %>%
  mutate(Key_min = paste(Country.Code , Arrangement.Number, sep = "_"))

df_filt <- df_filt %>%
  relocate(Key_min, .before = 1)

# Elimino filas dup

df_filt  <- df_filt %>% distinct()

colnames(df_filt)

# Seleccionao columnas

macro_df <- df_filt %>%
  select(Key_corta, Mneumonic, "T-3", "T-2", "T-1", "T", "T+1","T+2","T+3","T+4",
         "var_t4_t"  ,
         "var_t_t_3" )

sapply(macro_df, class)


# pivoteamos

library(tidyr)

wide_data <- macro_df %>%
  pivot_wider(names_from = 'Mneumonic', values_from = c("T-3", "T-2", "T-1", "T", "T+1","T+2","T+3","T+4",
                                                        "var_t4_t"  , 
                                                        "var_t_t_3" ), values_fn = mean )

sapply(wide_data, class)

# si mas del 10 porciento de una columna son NULL, entonces eliminar la columna

df_clean <- wide_data %>%
  select(where(~mean(is.na(.x) | sapply(.x, is.null)) <= 0.1))

sapply(df_clean, class)

colnames(df_clean)


# Reemplazar NA por la media

df_clean <- df_clean %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

df_clean <- df_clean %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.) | . == "", median(., na.rm = TRUE), .)))

any(is.na(df_clean))



##################################################################################

# Join

test_join <- head(tabla_comb) %>%
  left_join(head(df_clean), by = "Key_corta")

# Obtener las keys únicas de ambas tablas
keys_tabla_comb <- unique(tabla_comb$Key_corta)
keys_df_clean <- unique(df_clean$Key_corta)

# Encontrar intersección (keys comunes)
keys_comunes <- intersect(keys_tabla_comb, keys_df_clean)


tabla <- tabla_comb %>%
  mutate(Key_corta = str_trim(Key_corta) %>% toupper()) %>%
  left_join(
    df_clean %>%
      mutate(Key_corta = str_trim(Key_corta) %>% toupper()) %>%
      distinct(Key_corta, .keep_all = TRUE),
    by = "Key_corta"
  )

colnames(tabla)

# Elimino filas

tabla <- tabla %>% distinct()

colnames(tabla)

# Pasar a excel

library(writexl)
write_xlsx(tabla , "C:/Users/HP/Desktop/Tesis UDESA/QPC_Purch_Desc_Macro.xlsx")

##################################################################################
# Eliminar filas si variables macro son blanck

colnames(tabla)

tabla1 <- tabla %>%
  filter(!is.na(`T-3_BXG`), `T-3_BXG` != "")

tabla2 <- tabla1 %>%
  filter(!is.na(`T-3_NGDP`), `T-3_NGDP` != "")

tabla3 <- tabla2 %>%
  filter(!is.na(`QPC_Original_Amount`), `QPC_Original_Amount` != "")

colnames(tabla3)

tabla5 <- tabla3 %>%
  select(where(~mean(is.na(.x) | sapply(.x, is.null)) <= 0.1))

tabla6 <- tabla5 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.) | . == "", median(., na.rm = TRUE), .)))

colnames(tabla6)


# Variable date span

tabla6$Purchase_date_span <- tabla6$Purchase_Original_Date - tabla6$Approval.Date
tabla6$Purchase_date_span <- as.numeric(tabla6$Purchase_date_span)

tabla6 <- tabla6 %>% select(-Purchase_Original_Date)


# Eliminar nominalidad

tabla6$`T-3_PCPI_real` <- tabla6$`T-3_PCPI` / tabla6$`T-3_ENDA`
tabla6$`T-3_NGDP_usd` <- tabla6$`T-3_NGDP` / tabla6$`T-3_ENDA`
tabla6$`T-3_PCPIE_real` <- tabla6$`T-3_PCPIE` / tabla6$`T-3_ENDA`

tabla6$`T-2_PCPI_real` <- tabla6$`T-2_PCPI` / tabla6$`T-2_ENDA`
tabla6$`T-2_NGDP_usd` <- tabla6$`T-2_NGDP` / tabla6$`T-2_ENDA`
tabla6$`T-2_PCPIE_real` <- tabla6$`T-2_PCPIE` / tabla6$`T-2_ENDA`

tabla6$`T-1_PCPI_real` <- tabla6$`T-1_PCPI` / tabla6$`T-1_ENDA`
tabla6$`T-1_NGDP_usd` <- tabla6$`T-1_NGDP` / tabla6$`T-1_ENDA`
tabla6$`T-1_PCPIE_real` <- tabla6$`T-1_PCPIE` / tabla6$`T-1_ENDA`

tabla6$`T_PCPI_real` <- tabla6$`T_PCPI` / tabla6$`T_ENDA`
tabla6$`T_NGDP_usd` <- tabla6$`T_NGDP` / tabla6$`T_ENDA`
tabla6$`T_PCPIE_real` <- tabla6$`T_PCPIE` / tabla6$`T_ENDA`

tabla6$`T+1_PCPI_real` <- tabla6$`T+1_PCPI` / tabla6$`T+1_ENDA`
tabla6$`T+1_NGDP_usd` <- tabla6$`T+1_NGDP` / tabla6$`T+1_ENDA`
tabla6$`T+1_PCPIE_real` <- tabla6$`T+1_PCPIE` / tabla6$`T+1_ENDA`

tabla6$`T+2_PCPI_real` <- tabla6$`T+2_PCPI` / tabla6$`T+2_ENDA`
tabla6$`T+2_NGDP_usd` <- tabla6$`T+2_NGDP` / tabla6$`T+2_ENDA`
tabla6$`T+2_PCPIE_real` <- tabla6$`T+2_PCPIE` / tabla6$`T+2_ENDA`

tabla6$`T+3_PCPI_real` <- tabla6$`T+3_PCPI` / tabla6$`T+3_ENDA`
tabla6$`T+3_NGDP_usd` <- tabla6$`T+3_NGDP` / tabla6$`T+3_ENDA`
tabla6$`T+3_PCPIE_real` <- tabla6$`T+3_PCPIE` / tabla6$`T+3_ENDA`

tabla6$`T+4_PCPI_real` <- tabla6$`T+4_PCPI` / tabla6$`T+4_ENDA`
tabla6$`T+4_NGDP_usd` <- tabla6$`T+4_NGDP` / tabla6$`T+4_ENDA`
tabla6$`T+4_PCPIE_real` <- tabla6$`T+4_PCPIE` / tabla6$`T+4_ENDA`

colnames(tabla6)


# crear nuevas variables como ratios

tabla6$QPC_Units_ <- substr(tabla6$QPC_Units, 1, 3)
unique(tabla6$QPC_Units_)
colnames(tabla6)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "USD", as.numeric(tabla6$QPC_Original_Amount) / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)
tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "NC_", as.numeric(tabla6$QPC_Original_Amount) / tabla6$`T-1_ENDA`, tabla6$QPC_Original_Amount)
tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "SDR", as.numeric(tabla6$QPC_Original_Amount) / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)

glimpse(tabla6)
tabla6$QPC_Original_Amount_real <- as.numeric(tabla6$QPC_Original_Amount_real)
tabla6$QPC_Original_Amount <- as.numeric(tabla6$QPC_Original_Amount)

# variables purchase ENDA

colnames(tabla6)


tabla6$`Purchase_Original_Amount_ENDA_1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-1_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_1`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_1`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA_2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-2_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_2`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_2`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA_3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T-3_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_3`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_3`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA_t` <- tabla6$`Purchase_Original_Amount` / tabla6$`T_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA_t`[is.finite(tabla6$`Purchase_Original_Amount_ENDA_t`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA_t`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA_t`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+1` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+1_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+1`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+1`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+1`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+1`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+2` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+2_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+2`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+2`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+2`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+2`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+3` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+3_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+3`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+3`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+3`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+3`)] <- mediana_valor

tabla6$`Purchase_Original_Amount_ENDA+4` <- tabla6$`Purchase_Original_Amount` / tabla6$`T+4_ENDA`
mediana_valor <- median(tabla6$`Purchase_Original_Amount_ENDA+4`[is.finite(tabla6$`Purchase_Original_Amount_ENDA+4`)], na.rm = TRUE)
tabla6$`Purchase_Original_Amount_ENDA+4`[is.infinite(tabla6$`Purchase_Original_Amount_ENDA+4`)] <- mediana_valor

# variables QPC ENDA

tabla6$`QPC_Original_Amount_ENDA_1` <- tabla6$`QPC_Original_Amount` / tabla6$`T-1_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_1`[is.finite(tabla6$`QPC_Original_Amount_ENDA_1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_1`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA_2` <- tabla6$`QPC_Original_Amount` / tabla6$`T-2_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_2`[is.finite(tabla6$`QPC_Original_Amount_ENDA_2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_2`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA_3` <- tabla6$`QPC_Original_Amount` / tabla6$`T-3_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_3`[is.finite(tabla6$`QPC_Original_Amount_ENDA_3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_3`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA_t` <- tabla6$`QPC_Original_Amount` / tabla6$`T_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA_t`[is.finite(tabla6$`QPC_Original_Amount_ENDA_t`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA_t`[is.infinite(tabla6$`QPC_Original_Amount_ENDA_t`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+1` <- tabla6$`QPC_Original_Amount` / tabla6$`T+1_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+1`[is.finite(tabla6$`QPC_Original_Amount_ENDA+1`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+1`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+1`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+2` <- tabla6$`QPC_Original_Amount` / tabla6$`T+2_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+2`[is.finite(tabla6$`QPC_Original_Amount_ENDA+2`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+2`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+2`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+3` <- tabla6$`QPC_Original_Amount` / tabla6$`T+3_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+3`[is.finite(tabla6$`QPC_Original_Amount_ENDA+3`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+3`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+3`)] <- mediana_valor

tabla6$`QPC_Original_Amount_ENDA+4` <- tabla6$`QPC_Original_Amount` / tabla6$`T+4_ENDA`
mediana_valor <- median(tabla6$`QPC_Original_Amount_ENDA+4`[is.finite(tabla6$`QPC_Original_Amount_ENDA+4`)], na.rm = TRUE)
tabla6$`QPC_Original_Amount_ENDA+4`[is.infinite(tabla6$`QPC_Original_Amount_ENDA+4`)] <- mediana_valor


# Total Access

tabla6$`Totalaccess_bxg` <- tabla6$`Totalaccess` / tabla6$`T-1_BXG`
mediana_valor <- median(tabla6$`Totalaccess_bxg`[is.finite(tabla6$`Totalaccess_bxg`)], na.rm = TRUE)
tabla6$`Totalaccess_bxg`[is.infinite(tabla6$`Totalaccess_bxg`)] <- mediana_valor


# Formato fecha

library(lubridate)

tabla6$QPC_Test_Date_ <- dmy(tabla6$QPC_Test_Date)
tabla6$Approval.Date_  <- as.Date(tabla6$Approval.Date, origin = "1899-12-30")

# Variable QPC Span

tabla6$QPC_span <- tabla6$QPC_Test_Date_ - tabla6$Approval.Date_
tabla6$QPC_date_span <- as.numeric(tabla6$QPC_span)

tabla6 <- tabla6[, -which(names(tabla6) == "QPC_Test_Date_")]
tabla6 <- tabla6[, -which(names(tabla6) == "Approval.Date_")]
tabla6 <- tabla6[, -which(names(tabla6) == "QPC_span")]


#si review type len es mayor a 3 poner un 1 si no poner cero

tabla6$Joint_revision <- as.integer(nchar(tabla6$Review.Type) > 3)

# Eliminar filas duplicadas
library(dplyr)
tabla6 <- tabla6 %>% distinct()

# Buscamos NAs

colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

# Imputamos la mediana

library(dplyr)

tabla6 <- tabla6 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


# Limpiamos

library(dplyr)
library(purrr)

# Función mejorada para reemplazar valores problemáticos
limpiar_columna <- function(x) {
  # Manejar columnas numéricas
  if(is.numeric(x)) {
    x[x == "" | is.infinite(x)] <- NA
    med <- median(x, na.rm = TRUE)
    x[is.na(x)] <- med
    return(x)
  }
  
  # Manejar columnas de caracteres/factores
  if(is.character(x) | is.factor(x)) {
    x[x == ""] <- NA
    moda <- names(which.max(table(x)))
    x[is.na(x)] <- moda
    return(x)
  }
  
  # Manejar columnas lógicas
  if(is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x)
  }
  
  return(x)
}

# Aplicar a todo el dataframe
tabla6 <- tabla6 %>%
  mutate(across(everything(), limpiar_columna))

# Verificación final
colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

tabla6 <- tabla6 %>% distinct()

colnames(tabla6)

###########################################################################################
# Eliminar columnas redundantes

colnames(tabla6)

#tabla6 <- tabla6 %>% select(-Country.Name)
#tabla6 <- tabla6 %>% select(-Arrangement_type)
tabla6 <- tabla6 %>% select(-Approval.Date)
#tabla6 <- tabla6 %>% select(-Approval.Year)
#tabla6 <- tabla6 %>% select(-Initial.End.Date)
#tabla6 <- tabla6 %>% select(-Initial.End.Year)
tabla6 <- tabla6 %>% select(-Review.Type)
#tabla6 <- tabla6 %>% select(-Review.Sequence)
#tabla6 <- tabla6 %>% select(-Main)
tabla6 <- tabla6 %>% select(-QPC_Units)
#tabla6 <- tabla6 %>% select(-Purchase_Units)
#tabla6 <- tabla6 %>% select(-Main.Criteria.Sequence)
#tabla6 <- tabla6 %>% select(-Sub.Option)
#tabla6 <- tabla6 %>% select(-Program_span)


tabla6  <- tabla6 %>% distinct()

# Pasar a dias

library(lubridate)
tabla6$QPC_Test_Date <- dmy(tabla6$QPC_Test_Date)

# Numerico 

glimpse(tabla6)
colnames(tabla6)

tabla6$Actual.Amount <- as.numeric(tabla6$Actual.Amount)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "USD", tabla6$QPC_Original_Amount / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)
tabla6$QPC_Actual_Amount_real <- ifelse(tabla6$QPC_Units_ == "USD", tabla6$Actual.Amount / tabla6$`T-1_BXG`, tabla6$Actual.Amount)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "NC_", tabla6$QPC_Original_Amount / tabla6$`T-1_NGDP`, tabla6$QPC_Original_Amount)
tabla6$QPC_Actual_Amount_real <- ifelse(tabla6$QPC_Units_ == "NC_", tabla6$Actual.Amount / tabla6$`T-1_NGDP`, tabla6$Actual.Amount)

tabla6$QPC_Original_Amount_real <- ifelse(tabla6$QPC_Units_ == "SDR", tabla6$QPC_Original_Amount / tabla6$`T-1_BXG`, tabla6$QPC_Original_Amount)
tabla6$QPC_Actual_Amount_real <- ifelse(tabla6$QPC_Units_ == "SDR", tabla6$Actual.Amount / tabla6$`T-1_BXG`, tabla6$Actual.Amount)


# Buscamos NAs

colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

# Imputamos la mediana

library(dplyr)

tabla6 <- tabla6 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))


###

library(dplyr)
library(purrr)

# Función mejorada para reemplazar valores problemáticos
limpiar_columna <- function(x) {
  # Manejar columnas numéricas
  if(is.numeric(x)) {
    x[x == "" | is.infinite(x)] <- NA
    med <- median(x, na.rm = TRUE)
    x[is.na(x)] <- med
    return(x)
  }
  
  # Manejar columnas de caracteres/factores
  if(is.character(x) | is.factor(x)) {
    x[x == ""] <- NA
    moda <- names(which.max(table(x)))
    x[is.na(x)] <- moda
    return(x)
  }
  
  # Manejar columnas lógicas
  if(is.logical(x)) {
    x[is.na(x)] <- FALSE
    return(x)
  }
  
  return(x)
}

# Aplicar a todo el dataframe
tabla6 <- tabla6 %>%
  mutate(across(everything(), limpiar_columna))

# Verificación final
colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

tabla6 <- tabla6 %>% distinct()

#############################################

tabla6 <- tabla6 %>% select(-QPC_Units_)
tabla6 <- tabla6 %>% select(-Actual.Amount)

glimpse(tabla6)
colnames(tabla6)

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

tabla6 <- tabla6[, -c(39:296)]

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

# Elimino key larga

datos_filtrados <- tabla6 %>%
  filter(Key_larga == "213_2018_2021_770_R1_16")

tabla6 <- tabla6 %>% select(-Key_larga)

# Solo variables fiscales y externas

tabla7 <- tabla6

tabla6 <- tabla6 %>%
  filter(QPC_Criteria_Order %in% c(32, 135))


###########################################################################################

# base <- tabla6
# 
# library(ggplot2)
# library(dplyr)
# library(lubridate)
# 
# # 1. Filtrar por rango de años (2003-2024)
# base <- base %>%
#   mutate(Year = year(QPC_Test_Date)) %>%
#   filter(Year >= 2003 & Year <= 2024)
# 
# # Primero asegurémonos de que los datos estén en el formato correcto
# datos_anuales <- base %>%
#   count(Year, Status) %>%  # Crear conteos si no existen
#   mutate(
#     Year = factor(Year),
#     Status = factor(Status)
#   )
# 
# # Crear etiquetas en la posición correcta (acumuladas)
# etiquetas <- datos_anuales %>%
#   group_by(Year) %>%
#   arrange(Year, desc(Status)) %>%
#   mutate(
#     posicion_cumulativa = cumsum(n),
#     posicion_etiqueta = posicion_cumulativa - (n / 2)
#   )
# 
# # Obtener número único de estados para la paleta de colores
# num_estados <- nlevels(datos_anuales$Status)
# 
# ggplot(datos_anuales, aes(x = Year, y = n, fill = Status)) +
#   geom_col(position = position_stack(), color = "black", width = 0.7) +
#   geom_text(
#     data = etiquetas,
#     aes(y = posicion_etiqueta, label = ifelse(n > 0, n, "")),
#     size = 3.5,
#     family = "Times New Roman",
#     color = "white",
#     fontface = "bold"
#   ) +
#   scale_fill_manual(
#     values = colorRampPalette(c("#3A5F7F", "#A83232"))(num_estados)
#   ) +
#   labs(x = "Año", y = "Número de QPCs", fill = "Status") +
#   theme_minimal(base_family = "Times New Roman", base_size = 16) +
#   theme(
#     axis.text.x = element_text(
#       angle = 90,
#       vjust = 0.5,
#       hjust = 1,
#       size = 11
#     ),
#     axis.line = element_line(color = "black"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.background = element_blank(),
#     plot.background = element_blank(),
#     plot.margin = unit(c(10, 10, 20, 10), "points"),
#     legend.position = "top"
#   ) +
#   scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
# 
# tabla6 <- tabla6 %>%
#   relocate(QPC_Test_Date, .before = 3)
# 
# colnames(tabla6)


# Simplificacion nombres

names(tabla6) <- gsub("-", "_", names(tabla6))
names(tabla6) <- gsub("\\+", "X", names(tabla6))

# Variable sesgo 

tabla6$QPC_Amount_span_perc <- (tabla6$QPC_Actual_Amount_real - tabla6$QPC_Original_Amount_real) / tabla6$QPC_Original_Amount_real

tabla6$Sesgo <- ifelse(tabla6$QPC_Amount_span_perc > 0, 1, -1)

tabla6 <- tabla6 %>% select(-QPC_Actual_Amount_real)
tabla6 <- tabla6 %>% select(-QPC_Amount_span_perc)

colnames(tabla6)

# Aplicar a todo el dataframe
tabla6 <- tabla6 %>%
  mutate(across(everything(), limpiar_columna))

# Verificación final
colSums(is.na(tabla6))

sapply(tabla6, function(x) sum(is.infinite(x)))

tabla6 <- tabla6 %>% distinct()
###########################################################################################

# Pasar a excel la table
write_xlsx(tabla6, "C:/Users/HP/Desktop/Tesis UDESA/2026 03 29/base3.xlsx")

