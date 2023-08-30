library(RSQLite)

############# Problem 1 : Create tables #############################

# Establish database connection
  # Ce code crée une database SQLite nommée "Project_SQL_R.sqlite"
  # et établit la connexion à cette database.
conn <- dbConnect(SQLite(),"Project_SQL_R.sqlite")

# CROP_DATA
  # ce code crée la table CROP_DATA
df1 <- dbExecute(conn, 
                 "CREATE TABLE CROP_DATA (
                                      CD_ID INTEGER NOT NULL,
                                      YEAR DATE NOT NULL,
                                      CROP_TYPE VARCHAR(20) NOT NULL,
                                      GEO VARCHAR(20) NOT NULL, 
                                      SEEDED_AREA INTEGER NOT NULL,
                                      HARVESTED_AREA INTEGER NOT NULL,
                                      PRODUCTION INTEGER NOT NULL,
                                      AVG_YIELD INTEGER NOT NULL,
                                      PRIMARY KEY (CD_ID)
                                      )", 
                 errors=FALSE
)

if (df1 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}

# FARM_PRICES
  # Ce code crée la table FARM_PRICES
df2 <- dbExecute(conn, 
                 "CREATE TABLE FARM_PRICES (
                                      CD_ID INTEGER NOT NULL,
                                      DATE DATE NOT NULL,
                                      CROP_TYPE VARCHAR(20) NOT NULL,
                                      GEO VARCHAR(20) NOT NULL, 
                                      PRICE_PRERMT FLOAT(6),
                                      PRIMARY KEY (CD_ID)
                                      )", 
                 errors=FALSE
)

if (df2 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
}

# DAILY_FX
  # ce code crée la table DAILY_FX
df3 <- dbExecute(conn, "CREATE TABLE DAILY_FX (
                                DFX_ID INTEGER NOT NULL,
                                DATE DATE NOT NULL, 
                                FXUSDCAD FLOAT(6),
                                PRIMARY KEY (DFX_ID)
                                )",
                 errors=FALSE
)

if (df3 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
} 

# MONTHLY_FX
  # Ce code crée la table MONTHLY_FX
df4 <- dbExecute(conn, "CREATE TABLE MONTHLY_FX (
                                DFX_ID INTEGER NOT NULL,
                                DATE DATE NOT NULL, 
                                FXUSDCAD FLOAT(6),
                                PRIMARY KEY (DFX_ID)
                                )",
                 errors=FALSE
)

if (df4 == -1){
  cat ("An error has occurred.\n")
  msg <- odbcGetErrMsg(conn)
  print (msg)
} else {
  cat ("Table was created successfully.\n")
} 

############ Problem 2: Read Datasets and Load Tables ##############
  # Ici il s'agit de lire les ensembles de données dans R
  # et de charger les dataframes correspondantes dans les tables

# Lire les fichiers de données en tant que dataframes
crop_df <- read.csv(
  "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Annual_Crop_Data.csv", 
  colClasses = c(YEAR="character")
)
head(crop_df, 3)

farm_prices_df <- read.csv(
  "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Monthly_Farm_Prices.csv",
  colClasses = c(DATE="character")
)
head(farm_prices_df, 3)

daily_fx_df <- read.csv(
  "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Daily_FX.csv",
  colClasses = c(DATE="character")
)
head(daily_fx_df, 3)

monthly_fx_df <- read.csv(
  "https://cf-courses-data.s3.us.cloud-object-storage.appdomain.cloud/IBM-RP0203EN-SkillsNetwork/labs/Final%20Project/Monthly_FX.csv",
  colClasses = c(DATE="character")
)
head(monthly_fx_df, 3)

# Chargement des données dans les tables
dbWriteTable(conn, "CROP_DATA", crop_df, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "FARM_PRICES", farm_prices_df, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "DAILY_FX", daily_fx_df, overwrite=TRUE, header = TRUE)
dbWriteTable(conn, "MONTHLY_FX", monthly_fx_df, overwrite=TRUE, header = TRUE)

# Liste des tables présentes dans la database
dbListTables(conn)


###### Problem 3: How many records are in the farm prices dataset?######
dbGetQuery(conn, 'SELECT COUNT(CD_ID) FROM FARM_PRICES')

#### problem 4: Which geographies are included in the farm prices dataset?####
dbGetQuery(conn, 'SELECT DISTINCT(GEO) FROM FARM_PRICES')

### problem 5: How many hectares of Rye were harvested in Canada in 1968?####

# crop_df %>%
#   filter(CROP_TYPE == "Rye", GEO=="Canada", format(as.Date(YEAR), "%Y") == "1968") %>%
#   summarize(n_hectares = sum(HARVESTED_AREA))

query5 <- "
SELECT SUM(HARVESTED_AREA)
FROM CROP_DATA
WHERE 
  (CROP_TYPE = 'Rye') AND
  (GEO = 'Canada') AND
  YEAR LIKE '1968%'
"
dbGetQuery(conn, query5)

### Problem 6: Query and display the first 6 rows of the farm prices table for Rye.###
query6 <- "
SELECT * FROM FARM_PRICES
WHERE CROP_TYPE = 'Rye'
LIMIT 6
"
dbGetQuery(conn, query6)

### Problem 7: Which provinces grew Barley? ####
query7 <- "
SELECT DISTINCT(GEO) FROM FARM_PRICES
WHERE CROP_TYPE = 'Barley'
"
dbGetQuery(conn, query7)

### Problem 8: Find the first and last dates for the farm prices data.###
query8 <- "
SELECT MIN(DATE), MAX(DATE)
FROM FARM_PRICES
"
dbGetQuery(conn, query8)

### Problem 9: Which crops have ever reached a farm price greater than or equal to $350 per metric tonne?###
query9 <- "
SELECT DISTINCT(CROP_TYPE)
FROM FARM_PRICES
WHERE PRICE_PRERMT >= 350
"
dbGetQuery(conn, query9)

### Problem 10: Rank the crop types harvested in Saskatchewan in the year 2000 by their average yield. Which crop performed best?###
query10 <- "
SELECT * FROM CROP_DATA
WHERE 
  (GEO = 'Saskatchewan') AND
  (YEAR LIKE '2000%')
ORDER BY AVG_YIELD DESC
"
dbGetQuery(conn, query10)

######################## problem 11 ###############################
query11 <- "
SELECT YEAR, CROP_TYPE, GEO, AVG_YIELD
FROM CROP_DATA
WHERE YEAR > '2000-01-01'
ORDER BY AVG_YIELD DESC
LIMIT 2
"
dbGetQuery(conn, query11)

##################### problem 12 ###########################
query12 <- "
SELECT SUM(HARVESTED_AREA) FROM CROP_DATA
WHERE 
  YEAR = (SELECT max(YEAR) FROM CROP_DATA) AND 
  CROP_TYPE = 'Wheat'
"
dbGetQuery(conn, query12)


################ problem 13 ########################
query13 <- "
SELECT 
  o.DATE,
  m.FXUSDCAD,
  o.CROP_TYPE,
  o.GEO,
  o.PRICE_PRERMT
FROM FARM_PRICES o
INNER JOIN MONTHLY_FX m ON o.DATE = m.DATE
WHERE o.CROP_TYPE = 'Canola' AND o.GEO = 'Saskatchewan'
ORDER BY o.DATE DESC
LIMIT 6
"
dbGetQuery(conn, query13)


# Déconnecter la base
dbDisconnect(conn)
