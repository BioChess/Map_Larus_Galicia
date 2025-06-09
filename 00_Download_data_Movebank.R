library(pacman)
pacman::p_load("dplyr", "lubridate", "move", "leaflet", "htmlwidgets", "jsonlite")

# Retrieve credentials from environment variables
user <- Sys.getenv("MOVEBANK_USER")
password <- Sys.getenv("MOVEBANK_PASS")


# Debugging: Print if credentials are set
print(paste("User:", user))  # It should not be empty
print(paste("Password length:", nchar(password)))  # Should be non-zero

# Log in
loginStored <- movebankLogin(username = user, password = password)

# Debugging: Check login object
print(loginStored)

# Storing the study name
st <- "AMPLIAMAR (Larus, Galicia)"
#print(paste("Study name:", st))  # Debugging

# Verify if study exists
studies <- getMovebankStudies(login = loginStored)
#print("Available studies:")
#print(studies)

# Check if the study exists
if (!(st %in% studies)) {
  stop("Error: Study not found in Movebank.")
}

# Storing the names of the individuals
Animal_Data <- getMovebankAnimals(study = st, login = loginStored)

# Debugging: Check retrieved data
#print(Animal_Data)

# If no data is retrieved, stop the script
if (nrow(Animal_Data) == 0) {
  stop("Error: No animal data retrieved. Check study name or login credentials.")
}

Animal_Names <- as.character(unique(Animal_Data$animalName))

# Debugging: Print extracted animal names
print(paste("Animal Names:", paste(Animal_Names, collapse = ", ")))

# Get GPS data
gps <- getMovebankData(
  study = st, 
  login = loginStored, 
  removeDuplicatedTimestamps = TRUE,
  animalName = Animal_Names
)

# Debugging: Check GPS data
print(head(gps@data))

gps.df <- data.frame(
  birdID = gsub("^X", "", gps@trackId),
  tagID = gps@data$tag_local_identifier,
  datetimeGMT = gps@data$timestamp,
  latitude = gps@data$location_lat,
  longitude = gps@data$location_long
)

if (nrow(gps.df) == 0) {
  stop("No se obtuvieron datos GPS. No se guardará ningún archivo.")
}

gps.df$sex <- gps@idData$sex[match(gps.df$birdID, gps@idData$ring_id)]

gps.df$sex <- case_when(gps.df$sex == 'f' ~ 'Female', 
                        gps.df$sex == 'm' ~ 'Male',
                        T ~ 'Unknown')

head(gps.df)

# Crear un nombre de archivo con timestamp o número incremental
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
csv_file <- paste0("gps_data_", timestamp, ".csv")
# json_file <- paste0("gps_data_", timestamp, ".json")

# Guardar en CSV
write.csv(gps.df, csv_file, row.names = FALSE)


