## code to prepare `diabetic_data` dataset goes here

# read the data and prepare as raw_data
diabetic_data = readRDS("data-raw//diabetic_data.rds")
# usethis::use_data_raw("diabetic_data")

# based on the definition of READMITTED
str(diabetic_data)
dim(diabetic_data)
diabetic_data <- subset(diabetic_data,
                      select = -c(encounter_id, patient_nbr))
                  # delete the id information
diabetic_data <- diabetic_data[diabetic_data$readmitted != '>30',]
dim(diabetic_data)

diabetic_data[diabetic_data$readmitted == '<30', "readmitted"] <- "YES"
summary(diabetic_data$readmitted |> as.factor())

usethis::use_data(diabetic_data, overwrite = TRUE)
