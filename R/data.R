#' @title Diabetes patients readmission with 130-US hospitals
#'
#' @description This data has been prepared to analyze factors related to
#' readmission as well as other outcomes pertaining to patients with diabetes.
#'
#' @format
#' a dataset with 66221 obs. and 48 variables:
#'
#' \describe{
#' \item{readmitted}{Days to inpatient readmission. Values: "YES" if the patient
#' was readmitted in less than 30 days and "No" for no record of readmission.}
#'
#' \item{race}{Values: Caucasian, Asian, African American, Hispanic, and other}
#' \item{gender}{Values: male, female, and unknown/invalid}
#' \item{age}{Grouped in 10-year intervals: `[0, 10)`, `[10, 20)`, ...,
#' `[90, 100)`}
#' \item{weight}{Weight in pounds}
#' \item{admission_type_id}{Integer identifier corresponding to 9 distinct
#' values, for example, emergency, urgent, elective, newborn, and not available}
#' \item{discharge_disposition_id}{Integer identifier corresponding to 29
#' distinct values, for example, discharged to home, expired, and not available}
#' \item{admission_source_id}{Integer identifier corresponding to 21 distinct
#' values, for example, physician referral, emergency room, and transfer from a
#' hospital}
#' \item{time_in_hospital}{Integer number of days between admission and
#' discharge}
#' \item{payer_code}{Integer identifier corresponding to 23 distinct values,
#' for example, Blue Cross, Blue Shield, Medicare, and self-pay}
#' \item{medical_specialty}{Integer identifier of a specialty of the admitting
#' physician, corresponding to 84 distinct values, for example, cardiology,
#' internal medicine, family or general practice, and surgeon}
#' \item{num_lab_procedures}{Number of lab tests performed during the encounter}
#' \item{num_procedures}{Number of procedures (other than lab tests) performed
#' during the encounter}
#' \item{num_medications}{Number of distinct generic names administered during
#' the encounter}
#' \item{number_outpatient}{Number of outpatient visits of the patient in the
#' year preceding the encounter}
#' \item{number_emergency}{ Number of emergency visits of the patient in the
#' year preceding the encounter}
#' \item{number_inpatient}{Number of inpatient visits of the patient in the year
#' preceding the encounter}
#' \item{diag_1}{The primary diagnosis (coded as first three digits of ICD9);
#' 848 distinct values}
#' \item{diag_2}{Secondary diagnosis (coded as first three digits of ICD9);
#' 923 distinct values}
#' \item{diag_3}{Additional secondary diagnosis (coded as first three digits of
#' ICD9); 954 distinct values}
#' \item{number_diagnoses}{Number of diagnoses entered to the system}
#' \item{max_glu_serum}{Indicates the range of the result or if the test was not
#' taken. Values: ???>200,??? ???>300,??? ???normal,??? and ???none??? if not measured}
#' \item{A1Cresult}{Indicates the range of the result or if the test was not
#' taken. Values: ???>8??? if the result was greater than 8%, ???>7??? if the result was
#' greater than 7% but less than 8%, ???normal??? if the result was less than 7%,
#' and ???none??? if not measured.}
#' \item{metformin}{Down or Steady or Up, or No denotes no treatment}
#' \item{repaglinide}{Down or Steady or Up, or No denotes no treatment}
#' \item{nateglinide}{Down or Steady or Up, or No denotes no treatment}
#' \item{chlorpropamide}{Down or Steady or Up, or No denotes no treatment}
#' \item{glimepiride}{Down or Steady or Up, or No denotes no treatment}
#' \item{acetohexamide}{Down or Steady or Up, or No denotes no treatment}
#' \item{glipizide}{Down or Steady or Up, or No denotes no treatment}
#' \item{glyburide}{Down or Steady or Up, or No denotes no treatment}
#' \item{tolbutamide}{Down or Steady or Up, or No denotes no treatment}
#' \item{pioglitazone}{Down or Steady or Up, or No denotes no treatment}
#' \item{rosiglitazone}{Down or Steady or Up, or No denotes no treatment}
#' \item{acarbose}{Down or Steady or Up, or No denotes no treatment}
#' \item{miglitol}{Down or Steady or Up, or No denotes no treatment}
#' \item{troglitazone}{Down or Steady or Up, or No denotes no treatment}
#' \item{tolazamide}{Down or Steady or Up, or No denotes no treatment}
#' \item{examide}{Down or Steady or Up, or No denotes no treatment}
#' \item{citoglipton}{Down or Steady or Up, or No denotes no treatment}
#' \item{citoglipton}{Down or Steady or Up, or No denotes no treatment}
#' \item{insulin}{Down or Steady or Up, or No denotes no treatment}
#' \item{glyburide.metformin}{Down or Steady or Up, or No denotes no treatment}
#' \item{glipizide.metformin}{Down or Steady or Up, or No denotes no treatment}
#' \item{glimepiride.pioglitazone}{Down or Steady or Up, or No denotes
#' no treatment}
#' \item{metformin.rosiglitazone}{Down or Steady or Up, or No denotes no
#' treatment}
#' \item{metformin.pioglitazone}{Down or Steady or Up, or No denotes no
#'treatment}
#' \item{change}{Indicates if there was a change in diabetic medications
#' (either dosage or generic name). Values: ???change??? and ???no change???}
#' \item{diabetesMed}{Indicates if there was any diabetic medication prescribed.
#' Values: ???yes??? and ???no???}
#' }
#'
#'
#' @source Beata Strack, Jonathan P. DeShazo, Chris Gennings, Juan L. Olmo,
#' Sebastian Ventura, Krzysztof J. Cios, and John N. Clore, ???Impact of HbA1c
#' Measurement on Hospital Readmission Rates: Analysis of 70,000 Clinical
#' Database Patient Records,??? BioMed Research International, vol. 2014,
#' Article ID 781670, 11 pages, 2014.
#' @name diabetic_data
#' @docType data
#' @keywords data
NULL




#' @title ID mapping information for Diabetic Readmission dataset
#'
#' @description the map information for dataset `diabetic_data`
#'
#' @format
#' a dataset with 63 obs. and 3 variables:
#' \describe{
#' \item{column}{column name;}
#' \item{value}{value in original dataset, the map_id;}
#' \item{description}{meaning of map_id;}
#' }
#' @source Beata Strack, Jonathan P. DeShazo, Chris Gennings, Juan L. Olmo,
#' Sebastian Ventura, Krzysztof J. Cios, and John N. Clore, ???Impact of HbA1c
#' Measurement on Hospital Readmission Rates: Analysis of 70,000 Clinical
#' Database Patient Records,??? BioMed Research International, vol. 2014, Article
#' ID 781670, 11 pages, 2014.
#' @name ids_map
#' @docType data
#' @keywords data
NULL



#' @title ICD-9 codes mapping information
#'
#' @description the map information for ICD-9 coding and primary category
#'
#' @format
#' a dataset with 17 obs. and 3 variables:
#' \describe{
#' \item{description}{primary category;}
#' \item{code_start}{the smallest code for this category;}
#' \item{code_end}{the largetest code for this category;}
#' }
#' @source \url{https://simba.isr.umich.edu/restricted/docs/Mortality/icd_09_
#' codes.pdf}
#' @name icd9_map
#' @docType data
#' @keywords data
NULL
