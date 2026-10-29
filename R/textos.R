# Подписи графиков и таблиц на трёх языках. Язык задаётся переменной LANG
# ("ru", "en", "pt") до вызова source() скриптов 03 и 04.

if (!exists("LANG")) LANG <- "ru"
stopifnot(LANG %in% c("ru", "en", "pt"))

TEXTOS <- list(
  # общие
  x_turnout    = c(ru = "Явка на участке", en = "Polling-station turnout", pt = "Comparecimento na seção"),
  y_votos      = c(ru = "Голосов", en = "Votes", pt = "Votos"),
  secoes       = c(ru = "Участков", en = "Polling stations", pt = "Seções"),
  sub_bin_1pp  = c(ru = "Число участков в корзине шириной 1 п.п.",
                   en = "Number of polling stations per 1 pp bin",
                   pt = "Número de seções por faixa de 1 p.p."),
  pp_sufixo    = c(ru = " п.п.", en = " pp", pt = " p.p."),

  # 03_shpilkin.R
  fonte_turno  = c(ru = "Источник: TSE, votacao_secao / detalhe_votacao_secao 2026, %d-й тур",
                   en = "Source: TSE, votacao_secao / detalhe_votacao_secao 2026, round %d",
                   pt = "Fonte: TSE, votacao_secao / detalhe_votacao_secao 2026, %dº turno"),
  outros_t1    = c(ru = "Остальные кандидаты", en = "Other candidates", pt = "Demais candidatos"),
  outros_t2    = c(ru = "Пустые и недействительные", en = "Blank and null", pt = "Brancos e nulos"),
  p1_titulo    = c(ru = "Голоса за кандидатов в зависимости от явки на участке",
                   en = "Candidate votes by polling-station turnout",
                   pt = "Votos dos candidatos por comparecimento na seção"),
  p1_sub       = c(ru = "Сумма голосов на участках с явкой в корзине шириной 0.5 п.п.",
                   en = "Total votes at polling stations grouped into 0.5 pp turnout bins",
                   pt = "Soma dos votos nas seções agrupadas em faixas de comparecimento de 0,5 p.p."),
  p2_titulo    = c(ru = "Явка и доля кандидата по участкам",
                   en = "Turnout and candidate vote share by polling station",
                   pt = "Comparecimento e votação do candidato por seção"),
  p2_sub       = c(ru = "Доля от действительных голосов; цвет — число участков в ячейке (лог. шкала)",
                   en = "Share of valid votes; colour shows the number of polling stations per cell (log scale)",
                   pt = "Percentual dos votos válidos; a cor mostra o número de seções na célula (escala log)"),
  y_share      = c(ru = "Доля кандидата", en = "Candidate share", pt = "Percentual do candidato"),
  p3_titulo    = c(ru = "Голоса за кандидатов в зависимости от явки, по регионам",
                   en = "Candidate votes by turnout, by region",
                   pt = "Votos dos candidatos por comparecimento, por região"),
  p3_sub       = c(ru = "Корзины явки шириной 1 п.п.; шкала Y своя в каждой панели",
                   en = "1 pp turnout bins; each panel has its own Y scale",
                   pt = "Faixas de comparecimento de 1 p.p.; escala Y própria em cada painel"),
  p4_titulo    = c(ru = "Распределение явки по участкам, по штатам",
                   en = "Polling-station turnout distribution by state",
                   pt = "Distribuição do comparecimento por seção, por UF"),
  medida_turn  = c(ru = "Явка", en = "Turnout", pt = "Comparecimento"),
  medida_share = c(ru = "Доля %s", en = "%s share", pt = "Percentual de %s"),
  p5_titulo    = c(ru = "Тонкие гистограммы: есть ли пики на круглых процентах?",
                   en = "Fine-grained histograms: are there spikes at round percentages?",
                   pt = "Histogramas finos: há picos em percentuais redondos?"),
  p5_sub       = c(ru = "Число участков в корзине 0.1 п.п. Пики на простых дробях (50%, 66.7%, 75%) частично арифметические — см. тест ниже",
                   en = "Number of polling stations per 0.1 pp bin. Spikes at simple fractions (50%, 66.7%, 75%) are partly arithmetic",
                   pt = "Número de seções por faixa de 0,1 p.p. Picos em frações simples (50%, 66,7%, 75%) são em parte aritméticos"),
  crit_inteiro = c(ru = "целый %", en = "integer %", pt = "% inteiro"),
  crit_5       = c(ru = "кратно 5%", en = "multiple of 5%", pt = "múltiplo de 5%"),

  # 04_comparacao.R
  fonte_comp   = c(ru = "Источник: TSE, votacao_secao / detalhe_votacao_secao 2026, 1-й и 2-й туры",
                   en = "Source: TSE, votacao_secao / detalhe_votacao_secao 2026, rounds 1 and 2",
                   pt = "Fonte: TSE, votacao_secao / detalhe_votacao_secao 2026, 1º e 2º turnos"),
  g1_titulo    = c(ru = "Изменение явки между 1-м и 2-м турами на тех же участках",
                   en = "Change in turnout between rounds 1 and 2 at the same polling stations",
                   pt = "Variação do comparecimento entre o 1º e o 2º turno nas mesmas seções"),
  g1_x         = c(ru = "Явка во 2-м туре минус явка в 1-м",
                   en = "Round 2 turnout minus round 1 turnout",
                   pt = "Comparecimento no 2º turno menos no 1º"),
  g2_titulo    = c(ru = "Прирост явки и прирост голосов кандидата на участке",
                   en = "Change in turnout vs change in candidate votes by polling station",
                   pt = "Variação do comparecimento e dos votos do candidato por seção"),
  g2_sub       = c(ru = "Обе величины в долях от списка избирателей. Красная пунктирная линия y = x — так выглядел бы вброс в пользу кандидата; чёрная — регрессия",
                   en = "Both as a share of registered voters. The dashed red line y = x is what ballot stuffing for the candidate would look like; the black line is the regression fit",
                   pt = "Ambos em proporção do eleitorado apto. A linha vermelha tracejada y = x mostra como seria o enchimento de urnas a favor do candidato; a preta é a regressão"),
  g2_x         = c(ru = "Изменение явки", en = "Change in turnout", pt = "Variação do comparecimento"),
  g2_y         = c(ru = "Изменение голосов кандидата", en = "Change in candidate votes",
                   pt = "Variação dos votos do candidato")
)

tx <- function(chave, ...) {
  s <- TEXTOS[[chave]][[LANG]]
  if (...length()) sprintf(s, ...) else s
}

# регионы в данных TSE называются по-португальски; в русской версии так и остаются
REGIOES_EN <- c("Centro-Oeste" = "Central-West", "Nordeste" = "Northeast", "Norte" = "North",
                "Sudeste" = "Southeast", "Sul" = "South")
rotulo_regiao <- function(x) if (LANG == "en") unname(REGIOES_EN[x]) else x
