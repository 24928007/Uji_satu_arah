# Fungsi Uji ANOVA Satu Arah
uji_satu_arah <- function(data, response, group) {
  # Validasi input
  if (!response %in% colnames(data)) {
    stop("Kolom respons tidak ditemukan dalam data.")
  }
  if (!group %in% colnames(data)) {
    stop("Kolom kelompok (group) tidak ditemukan dalam data.")
  }

  # Membentuk formula model
  formula <- as.formula(paste(response, "~", group))

  # Uji ANOVA satu arah
  model <- aov(formula, data = data)

  # Ringkasan hasil
  return(summary(model))
}
#' Uji ANOVA Satu Arah
#'
#' Fungsi ini melakukan analisis varians (ANOVA) satu arah pada data yang diberikan.
#'
#' @param data Data frame yang digunakan untuk analisis.
#' @param response Nama kolom respons (variabel dependen).
#' @param group Nama kolom kelompok (group) untuk analisis ANOVA.
#' @return Ringkasan hasil ANOVA.
#' @examples
#' @export
# Contoh penggunaan:
# data <- data.frame(
#   nilai = c(15, 20, 25, 30, 35, 40, 45, 50, 55),
#   grup = rep(c("A", "B", "C"), each = 3)
# )
# uji_satu_arah(data, "nilai", "grup")
