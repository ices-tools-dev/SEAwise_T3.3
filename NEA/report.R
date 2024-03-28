dir.create("NEA/report", showWarnings = FALSE)


# Rmarkdown reports ------------------------------------------------

library(rmarkdown)

rm.list <- ls(all.names=TRUE)
rm(list=rm.list)
# dev.off()

rmarkdown::render(
  input="NEA/report_01.Rmd",
  output_file = "report/report_01.docx"
)
