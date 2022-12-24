#' Korelason Hesaplıyoruz
#'tidy fonksiyonu ile korelasyon hesaplıyoruz
#'bu paket çok basit iki sürekli değişken arasındaki ilişkiyi belirlemeye yarar.
#' @param data yüklemeniz gereken veri seti
#' @param var1 birinci değişken
#' @param var2 ikinci değişken
#'
#' @return Bir tibble tablosu ile korelasyon sonuçlarını verir
#' @export
#'
#' @examples
#' korhesap(data = faithful, var1 = eruptions, var2 = waiting)
#'@importFrom rlang .data

korhesap<- function(data, var1, var2){

  # compute correlation ----
  stats::cor.test(
    x = data %>% dplyr::pull({{var1}}),
    y = data %>% dplyr::pull({{var2}})
  ) %>%
    # tidy up results ----
  broom::tidy() %>%
    # retain and rename relevant bits ----
  dplyr::select(
    correlation = .data$estimate,
    pval = .data$p.value
  )
}
