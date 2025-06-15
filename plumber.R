library(plumber)
library(e1071)

# Load the trained SVM model
load("svm_model.RData")

#* Predict disease group based on patient data
#* @post /predict
#* @param AgeOnset:number Age at onset (e.g., 30)
#* @param MRIHeadAbnormality:string MRI Head Abnormality (Normal or Abnormal)
#* @param MRIOpticNerve:string MRI Optic Nerve Abnormality (Normal or Abnormal)
#* @param Gender:string Gender (M or F)
#* @param ONSide:string ON Side (Unilateral or Bilateral)
#* @param RAPD:string Relative Afferent Pupillary Defect (Yes or No)
#* @param OpticDiscEdemaDoctor:string Optic Disc Edema (Yes or No)
#* @param SteroidResponse:string Steroid Treatment Response (Good or Poor)
#* @param OtherAutoimmuneDisorder:string Other Autoimmune Disorder Present (Yes or No)
#* @serializer json
function(
    AgeOnset,
    MRIHeadAbnormality,
    MRIOpticNerve,
    Gender,
    ONSide,
    RAPD,
    OpticDiscEdemaDoctor,
    SteroidResponse,
    OtherAutoimmuneDisorder
) {
  # Validate categorical inputs
  valid_vals <- list(
    MRIHeadAbnormality = c("Normal", "Abnormal"),
    MRIOpticNerve = c("Normal", "Abnormal"),
    Gender = c("M", "F"),
    ONSide = c("Unilateral", "Bilateral"),
    RAPD = c("Yes", "No"),
    OpticDiscEdemaDoctor = c("Yes", "No"),
    SteroidResponse = c("Good", "Poor"),
    OtherAutoimmuneDisorder = c("Yes", "No")
  )
  
  input_vals <- list(
    MRIHeadAbnormality, MRIOpticNerve, Gender, ONSide,
    RAPD, OpticDiscEdemaDoctor, SteroidResponse, OtherAutoimmuneDisorder
  )
  
  for (i in seq_along(valid_vals)) {
    name <- names(valid_vals)[i]
    if (!(input_vals[[i]] %in% valid_vals[[i]])) {
      return(list(
        error = paste0("Invalid value for ", name, 
                       ". Allowed: ", paste(valid_vals[[i]], collapse = ", "))
      ))
    }
  }
  
  # Build input dataframe
  new_data <- data.frame(
    AgeOnset = as.numeric(AgeOnset),
    MRIHeadAbnormality = factor(MRIHeadAbnormality, levels = c("Normal", "Abnormal")),
    MRIOpticNerve = factor(MRIOpticNerve, levels = c("Normal", "Abnormal")),
    Gender = factor(Gender, levels = c("M", "F")),
    ONSide = factor(ONSide, levels = c("Unilateral", "Bilateral")),
    RAPD = factor(RAPD, levels = c("Yes", "No")),
    OpticDiscEdemaDoctor = factor(OpticDiscEdemaDoctor, levels = c("Yes", "No")),
    SteroidResponse = factor(SteroidResponse, levels = c("Good", "Poor")),
    OtherAutoimmuneDisorder = factor(OtherAutoimmuneDisorder, levels = c("Yes", "No"))
  )
  
  # Predict
  prediction <- predict(svm_model, new_data, probability = TRUE)
  probs <- attr(predict(svm_model, new_data, probability = TRUE), "probabilities")
  
  # Format output
  predicted_class <- as.character(prediction)
  prob_list <- as.list(round(probs[1, c("MOGAD", "MS", "NMOSD")], 3))
  
  # Return JSON response
  list(
    Prediction = predicted_class,
    Probabilities = prob_list
  )
}
