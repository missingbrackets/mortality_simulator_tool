library(openxlsx)

export_simulation_workbook <- function(results, fit_comparison, path) {
  wb <- createWorkbook()

  addWorksheet(wb, "assumptions")
  writeData(
    wb, "assumptions",
    data.frame(
      parameter = names(results$assumptions),
      value = as.character(unlist(results$assumptions)),
      stringsAsFactors = FALSE
    )
  )

  addWorksheet(wb, "summary")
  writeData(wb, "summary", results$summary_table)

  addWorksheet(wb, "production_schedule")
  writeData(wb, "production_schedule", results$production_schedule)

  addWorksheet(wb, "severity_input")
  writeData(wb, "severity_input", results$severity_input)

  addWorksheet(wb, "severity_fit_check")
  writeData(wb, "severity_fit_check", fit_comparison)

  addWorksheet(wb, "simulation_summary")
  writeData(wb, "simulation_summary", results$simulation_summary)

  addWorksheet(wb, "event_detail")
  writeData(wb, "event_detail", results$event_detail)

  if (!is.null(results$claims_history) && nrow(results$claims_history) > 0) {
    addWorksheet(wb, "claims_history")
    writeData(wb, "claims_history", results$claims_history)
  }

  saveWorkbook(wb, path, overwrite = TRUE)
}
