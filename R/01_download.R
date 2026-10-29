# Загрузка данных TSE по участкам (seções eleitorais), выборы 2026.
# Источник: https://dadosabertos.tse.jus.br/dataset/resultados-2026

base_url <- "https://cdn.tse.jus.br/estatistica/sead/odsele"

files <- c(
  # голоса за кандидатов в президенты по участкам (вся страна + зарубежье)
  votacao_secao        = file.path(base_url, "votacao_secao/votacao_secao_2026_BR.zip"),
  # явка, пустые и недействительные бюллетени по участкам
  detalhe_votacao_secao = file.path(base_url, "detalhe_votacao_secao/detalhe_votacao_secao_2026.zip")
)

csv_br <- c(
  votacao_secao         = "votacao_secao_2026_BR.csv",
  detalhe_votacao_secao = "detalhe_votacao_secao_2026_BR.csv"
)

raw_dir <- "data/raw"
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

options(timeout = 3600)

# TSE перезаписывает файлы по тем же адресам (после 2-го тура в них оба тура),
# поэтому архив скачивается заново, если его размер на сервере изменился.
tamanho_remoto <- function(url) {
  h <- curlGetHeaders(url)
  as.numeric(sub(".*:\\s*", "", grep("^content-length:", h, ignore.case = TRUE, value = TRUE)))
}

for (name in names(files)) {
  zip_path <- file.path(raw_dir, paste0(name, ".zip"))
  out_dir  <- file.path(raw_dir, name)

  if (!file.exists(zip_path) || file.size(zip_path) != tamanho_remoto(files[[name]])) {
    message("Скачиваю ", files[[name]])
    download.file(files[[name]], zip_path, mode = "wb")
    unlink(out_dir, recursive = TRUE)
  }

  # распаковываем только президентский файл: в архиве detalhe ещё ~1.8 ГБ по штатам и должностям
  if (!dir.exists(out_dir)) {
    unzip(zip_path, files = csv_br[[name]], exdir = out_dir)
  }

  message(name, ": ", paste(list.files(out_dir), collapse = ", "))
}
