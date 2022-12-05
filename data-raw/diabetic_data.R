## code to prepare `diabetic_data` dataset goes here

# read the data and prepare as raw_data
diabetic_data <- readRDS("data-raw//diabetic_data.rds")
str(diabetic_data)
dim(diabetic_data)
diabetic_data <- subset(diabetic_data,
                      select = -c(encounter_id, patient_nbr))
diabetic_data <- diabetic_data[diabetic_data$readmitted != ">30", ]
dim(diabetic_data)

diabetic_data[diabetic_data$readmitted == "<30", "readmitted"] <- "YES"

usethis::use_data(diabetic_data, overwrite = TRUE)
