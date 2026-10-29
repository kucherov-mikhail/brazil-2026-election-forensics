# Сборка таблицы "участок -> явка + голоса кандидатов" для каждого тура
# выборов президента (CD_CARGO == 1). Результат: data/processed/secoes_t<тур>.rds

library(data.table)

key_cols <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")

# --- явка по участкам --------------------------------------------------------
# detalhe_..._BR.csv содержит только президентские выборы (SG_UE == "BR"),
# в отличие от detalhe_..._BRASIL.csv, где все должности.
det_all <- fread(
  "data/raw/detalhe_votacao_secao/detalhe_votacao_secao_2026_BR.csv",
  encoding = "Latin-1",
  select = c("NR_TURNO", "CD_CARGO", key_cols, "NM_MUNICIPIO", "NR_LOCAL_VOTACAO",
             "QT_APTOS", "QT_COMPARECIMENTO", "QT_ABSTENCOES",
             "QT_VOTOS_NOMINAIS", "QT_VOTOS_BRANCOS", "QT_VOTOS_NULOS",
             "DS_ORIGEM_VOTO", "ST_SECAO_INSTALADA", "ST_SECAO_ANULADA", "CD_MODELO_URNA")
)[CD_CARGO == 1]

# --- голоса кандидатов по участкам -------------------------------------------
vot_all <- fread(
  "data/raw/votacao_secao/votacao_secao_2026_BR.csv",
  encoding = "Latin-1",
  select = c("NR_TURNO", "CD_CARGO", key_cols, "NR_VOTAVEL", "NM_VOTAVEL", "QT_VOTOS")
)[CD_CARGO == 1]

regioes <- c(
  AC = "Norte", AM = "Norte", AP = "Norte", PA = "Norte", RO = "Norte", RR = "Norte", TO = "Norte",
  AL = "Nordeste", BA = "Nordeste", CE = "Nordeste", MA = "Nordeste", PB = "Nordeste",
  PE = "Nordeste", PI = "Nordeste", RN = "Nordeste", SE = "Nordeste",
  DF = "Centro-Oeste", GO = "Centro-Oeste", MS = "Centro-Oeste", MT = "Centro-Oeste",
  ES = "Sudeste", MG = "Sudeste", RJ = "Sudeste", SP = "Sudeste",
  PR = "Sul", RS = "Sul", SC = "Sul",
  ZZ = "Exterior"
)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

for (turno in sort(unique(det_all$NR_TURNO))) {
  det <- det_all[NR_TURNO == turno]
  vot <- vot_all[NR_TURNO == turno]

  candidatos <- vot[, .(votos = sum(QT_VOTOS)), by = .(NR_VOTAVEL, NM_VOTAVEL)][order(-votos)]
  candidatos[, NM_VOTAVEL := enc2utf8(NM_VOTAVEL)]

  # широкий формат: v_<номер кандидата>, 95 = branco, 96 = nulo
  vot_wide <- dcast(vot, SG_UF + CD_MUNICIPIO + NR_ZONA + NR_SECAO ~ paste0("v_", NR_VOTAVEL),
                    value.var = "QT_VOTOS", fun.aggregate = sum, fill = 0L)

  secoes <- merge(det[, !c("NR_TURNO", "CD_CARGO")], vot_wide, by = key_cols, all.x = TRUE)
  v_cols <- grep("^v_", names(secoes), value = TRUE)
  setnafill(secoes, fill = 0L, cols = v_cols)

  # --- контроль согласованности двух файлов ----------------------------------
  # сумма голосов из votacao_secao (включая brancos/nulos) должна равняться явке
  secoes[, soma_votos := rowSums(.SD), .SDcols = v_cols]
  message("Тур ", turno, ": участков, где сумма голосов != явке: ",
          secoes[soma_votos != QT_COMPARECIMENTO, .N])
  stopifnot(secoes[, sum(v_95)] == secoes[, sum(QT_VOTOS_BRANCOS)],
            secoes[, sum(v_96)] == secoes[, sum(QT_VOTOS_NULOS)])

  # --- производные показатели ------------------------------------------------
  secoes[, regiao := regioes[SG_UF]]
  secoes[, exterior := SG_UF == "ZZ"]
  secoes[, instalada := ST_SECAO_INSTALADA == "Sim" & ST_SECAO_ANULADA == "Não"]
  secoes[, validos := QT_VOTOS_NOMINAIS]  # действительные = за кандидатов, без brancos/nulos
  secoes[, turnout := QT_COMPARECIMENTO / QT_APTOS]

  saveRDS(secoes, sprintf("data/processed/secoes_t%d.rds", turno))
  fwrite(candidatos, sprintf("data/processed/candidatos_t%d.csv", turno))

  message("Тур ", turno, ": сохранено участков: ", nrow(secoes),
          " (установлено: ", secoes[instalada == TRUE, .N],
          ", за рубежом: ", secoes[exterior == TRUE, .N], ")")
}

rm(det_all, vot_all, det, vot, vot_wide)
