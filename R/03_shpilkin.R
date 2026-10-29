# Анализ одного тура президентских выборов по методу Шпилькина.
# Тур задаётся переменной TURNO до вызова source() (по умолчанию 1):
#   TURNO <- 2; source("R/03_shpilkin.R")
# Язык подписей — переменная LANG ("ru" по умолчанию, "en", "pt"), см. R/textos.R.
# Результаты пишутся в output/<язык>/t<тур>/.
#
# 1. Распределение голосов кандидатов по явке (классический график Шпилькина).
# 2. Плотность "явка x доля кандидата" по участкам.
# 3. То же, что (1), по регионам: в Бразилии сильная география голосования,
#    смешение регионов само по себе может дать "двугорбость".
# 4. Распределение явки по штатам.
# 5. Тонкие гистограммы (шаг 0.1 п.п.) — поиск пиков на круглых процентах.
# 6. Монте-Карло-тест на избыток целых процентов (Kobak, Shpilkin, Pshenichnikov 2016):
#    наблюдаемое число участков с целым процентом сравнивается с биномиальной
#    симуляцией, сохраняющей размер каждого участка — это учитывает арифметические
#    пики (например, 75% чаще встречается на участках, где число избирателей кратно 4).

library(data.table)
library(ggplot2)

MIN_APTOS <- 50   # маленькие участки дают шум и арифметические пики
N_SIM     <- 200  # число симуляций в тесте целых процентов
if (!exists("TURNO")) TURNO <- 1
source("R/textos.R")
set.seed(2026)

secoes <- readRDS(sprintf("data/processed/secoes_t%d.rds", TURNO))
d <- secoes[instalada & !exterior & QT_APTOS >= MIN_APTOS & validos > 0]
message("Тур ", TURNO, ". Участков в анализе: ", nrow(d), " из ", nrow(secoes))

# третья линия на графиках: в 1-м туре — прочие кандидаты, во 2-м их нет,
# и вместо них показываются пустые и недействительные бюллетени
if (TURNO == 1) {
  d[, `:=`(flavio = v_22, lula = v_13, outros = validos - v_22 - v_13)]
  rotulo_outros <- tx("outros_t1")
} else {
  d[, `:=`(flavio = v_22, lula = v_13, outros = v_95 + v_96)]
  rotulo_outros <- tx("outros_t2")
}

cand_labels <- c(flavio = "Flávio Bolsonaro (22)", lula = "Lula (13)", outros = rotulo_outros)
cand_cores  <- setNames(c("#2a78d6", "#eb6834", "#1baf7a"), cand_labels)

theme_shp <- theme_minimal(base_size = 11) +
  theme(
    plot.background    = element_rect(fill = "#fcfcfb", colour = NA),
    panel.grid.minor   = element_blank(),
    panel.grid.major   = element_line(colour = "#e1e0d9", linewidth = 0.3),
    text               = element_text(colour = "#0b0b0b"),
    axis.text          = element_text(colour = "#898781"),
    plot.subtitle      = element_text(colour = "#52514e"),
    plot.caption       = element_text(colour = "#898781", hjust = 0),
    strip.text         = element_text(colour = "#52514e", face = "bold"),
    legend.position    = "top",
    legend.justification = "left",
    legend.title       = element_blank()
  )
theme_set(theme_shp)

fonte <- tx("fonte_turno", TURNO)
pct   <- scales::label_percent(accuracy = 1)

out_dir <- file.path("output", LANG, paste0("t", TURNO))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
salvar <- function(p, nome, w = 10, h = 6) {
  ggsave(file.path(out_dir, nome), p, width = w, height = h, dpi = 150, bg = "#fcfcfb")
}

# голоса кандидатов, агрегированные по корзинам явки.
# Простые дроби (75%, 80%, 13/16 = 81.25%) встречаются чаще соседних значений, поэтому
# корзины центрированы на круглых значениях, а участок, попавший ровно на границу,
# делится поровну между двумя корзинами — иначе на графике появляются ложные зубцы.
votos_por_turnout <- function(dt, by = NULL, bin = 0.005) {
  x <- dt[, c("turnout", by, names(cand_labels)), with = FALSE]
  x[, pos := turnout / bin]
  x[, borda := abs(pos - floor(pos) - 0.5) < 1e-9]
  x <- rbind(
    x[borda == FALSE][, `:=`(bin_t = round(pos) * bin, w = 1)],
    x[borda == TRUE][, `:=`(bin_t = floor(pos) * bin, w = 0.5)],
    x[borda == TRUE][, `:=`(bin_t = ceiling(pos) * bin, w = 0.5)]
  )
  long <- melt(x, id.vars = c("bin_t", "w", by), measure.vars = names(cand_labels),
               variable.name = "cand", value.name = "votos")
  long <- long[, .(votos = sum(votos * w)), by = c("bin_t", by, "cand")]
  long[, cand := factor(cand_labels[as.character(cand)], levels = cand_labels)]
  long
}

# --- 1. классический график Шпилькина ----------------------------------------
v1 <- votos_por_turnout(d)
# подписи кривых Flávio и Lula ставятся там, где кривая сильнее всего выше соперника
# (среди корзин с заметным числом голосов), третья линия — над своим пиком
lider <- dcast(v1[cand != cand_labels[["outros"]]], bin_t ~ cand, value.var = "votos")
setnames(lider, c("bin_t", "a", "b"))
rotulo_para <- function(rotulo, proprio, outro) {
  pico <- lider$bin_t[which.max(proprio)]
  ok   <- proprio >= 0.3 * max(proprio)
  x    <- lider$bin_t[ok][which.max((proprio - outro)[ok])]
  lado <- sign(round((x - pico) / 0.005))
  data.table(cand = factor(rotulo, levels = cand_labels), bin_t = x + 0.006 * lado,
             votos = proprio[lider$bin_t == x], hjust = c(1, 0.5, 0)[lado + 2], vjust = -0.6)
}
rotulos <- rbind(
  rotulo_para(cand_labels[["flavio"]], lider$a, lider$b),
  rotulo_para(cand_labels[["lula"]],   lider$b, lider$a),
  v1[cand == cand_labels[["outros"]], .SD[which.max(votos)], by = cand][, `:=`(hjust = 0.5, vjust = -0.6)]
)

p1 <- ggplot(v1, aes(bin_t, votos, colour = cand)) +
  geom_line(linewidth = 0.7) +
  geom_text(data = rotulos, aes(label = cand, hjust = hjust, vjust = vjust),
            size = 3.3, colour = "#52514e", show.legend = FALSE) +
  scale_colour_manual(values = cand_cores) +
  scale_x_continuous(labels = pct, limits = c(0.4, 1)) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(title = tx("p1_titulo"), subtitle = tx("p1_sub"),
       x = tx("x_turnout"), y = tx("y_votos"), caption = fonte)
salvar(p1, "01_shpilkin_votos_por_turnout.png")

# --- 2. явка x доля кандидата ------------------------------------------------
share <- melt(d[, .(turnout, flavio = flavio / validos, lula = lula / validos)],
              id.vars = "turnout", variable.name = "cand", value.name = "share")
share[, cand := factor(cand_labels[as.character(cand)], levels = cand_labels[1:2])]

p2 <- ggplot(share, aes(turnout, share)) +
  geom_bin2d(binwidth = c(0.005, 0.01)) +
  scale_fill_gradient(low = "#cde2fb", high = "#0d366b", trans = "log10",
                      name = tx("secoes"), labels = scales::label_number()) +
  facet_wrap(~cand) +
  scale_x_continuous(labels = pct, limits = c(0.4, 1)) +
  scale_y_continuous(labels = pct) +
  labs(title = tx("p2_titulo"), subtitle = tx("p2_sub"),
       x = tx("x_turnout"), y = tx("y_share"), caption = fonte) +
  theme(legend.position = "right", legend.title = element_text(colour = "#52514e"))
salvar(p2, "02_turnout_vs_share.png", w = 12, h = 6)

# --- 3. Шпилькин по регионам -------------------------------------------------
v3 <- votos_por_turnout(d, by = "regiao", bin = 0.01)

p3 <- ggplot(v3, aes(bin_t, votos, colour = cand)) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~regiao, scales = "free_y", labeller = as_labeller(rotulo_regiao)) +
  scale_colour_manual(values = cand_cores) +
  scale_x_continuous(labels = pct, limits = c(0.4, 1)) +
  scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
  labs(title = tx("p3_titulo"), subtitle = tx("p3_sub"),
       x = tx("x_turnout"), y = tx("y_votos"), caption = fonte)
salvar(p3, "03_shpilkin_por_regiao.png", w = 12, h = 7)

# --- 4. распределение явки по штатам -----------------------------------------
p4 <- ggplot(d, aes(turnout)) +
  geom_histogram(binwidth = 0.01, center = 0, fill = "#2a78d6", colour = "#fcfcfb", linewidth = 0.1) +
  facet_wrap(~SG_UF, scales = "free_y", ncol = 7) +
  scale_x_continuous(labels = pct, limits = c(0.4, 1), breaks = c(0.5, 0.75, 1)) +
  labs(title = tx("p4_titulo"), subtitle = tx("sub_bin_1pp"),
       x = tx("x_turnout"), y = tx("secoes"), caption = fonte) +
  theme(axis.text.y = element_blank())
salvar(p4, "04_turnout_por_uf.png", w = 14, h = 8)

# --- 5. тонкие гистограммы: пики на круглых процентах ------------------------
# во 2-м туре доля Flávio = 1 - доля Lula, поэтому она не даёт новой информации
fino <- rbind(
  d[, .(medida = tx("medida_turn"), valor = turnout)],
  if (TURNO == 1) d[, .(medida = tx("medida_share", "Flávio Bolsonaro"), valor = flavio / validos)],
  d[, .(medida = tx("medida_share", "Lula"), valor = lula / validos)]
)
fino[, medida := factor(medida, levels = unique(medida))]
# округление до 0.1 п.п.: каждая корзина центрирована на x.x%
fino[, bin := round(valor * 1000) / 1000]
fino <- fino[, .N, by = .(medida, bin)]

p5 <- ggplot(fino[bin >= 0.2 & bin <= 0.95], aes(bin, N)) +
  geom_col(fill = "#2a78d6", width = 0.0008) +
  facet_wrap(~medida, ncol = 1, scales = "free_y") +
  scale_x_continuous(labels = pct, breaks = seq(0.2, 0.95, 0.05)) +
  labs(title = tx("p5_titulo"), subtitle = tx("p5_sub"),
       x = NULL, y = tx("secoes"), caption = fonte)
salvar(p5, "05_histogramas_finas.png", w = 14, h = 9)

# --- 6. тест на избыток целых процентов --------------------------------------
# "круглый" участок: 100*k/n отстоит от ближайшего кратного step не более чем на 0.05 п.п.
eh_redondo <- function(k, n, step) {
  p <- 100 * k / n
  abs(p - step * round(p / step)) < 0.05
}

teste_inteiros <- function(k, n, medida) {
  p <- k / n
  rbindlist(lapply(c(1, 5), function(step) {
    obs <- sum(eh_redondo(k, n, step))
    sim <- vapply(seq_len(N_SIM), function(i) sum(eh_redondo(rbinom(length(n), n, p), n, step)), numeric(1))
    data.table(
      medida, criterio = if (step == 1) tx("crit_inteiro") else tx("crit_5"),
      observado = obs, esperado = round(mean(sim)), sd_sim = round(sd(sim), 1),
      excesso_pct = round(100 * (obs / mean(sim) - 1), 1),
      z = round((obs - mean(sim)) / sd(sim), 2),
      p_valor = (sum(sim >= obs) + 1) / (N_SIM + 1)
    )
  }))
}

teste <- rbind(
  d[, teste_inteiros(QT_COMPARECIMENTO, QT_APTOS, tx("medida_turn"))],
  if (TURNO == 1) d[, teste_inteiros(flavio, validos, tx("medida_share", "Flávio Bolsonaro"))],
  d[, teste_inteiros(lula, validos, tx("medida_share", "Lula"))]
)
fwrite(teste, file.path(out_dir, "06_teste_inteiros.csv"))
print(teste)

# --- сводка по штатам --------------------------------------------------------
resumo_uf <- d[, .(
  secoes        = .N,
  turnout       = round(sum(QT_COMPARECIMENTO) / sum(QT_APTOS), 4),
  turnout_sd    = round(sd(turnout), 4),
  share_flavio  = round(sum(flavio) / sum(validos), 4),
  share_lula    = round(sum(lula) / sum(validos), 4),
  # связь явки и доли кандидата на участках внутри штата
  cor_turnout_flavio = round(cor(turnout, flavio / validos), 3),
  cor_turnout_lula   = round(cor(turnout, lula / validos), 3)
), by = .(regiao, SG_UF)][order(regiao, SG_UF)]
fwrite(resumo_uf, file.path(out_dir, "07_resumo_uf.csv"))
print(resumo_uf)
