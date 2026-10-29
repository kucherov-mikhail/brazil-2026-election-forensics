# Сравнение 1-го и 2-го туров на одних и тех же участках.
#
# Если во 2-м туре явку "добавляли" в пользу одного кандидата, то на участках,
# где явка выросла сильнее, прирост его голосов (в долях от списка избирателей)
# рос бы почти один к одному с приростом явки: точки ложились бы вдоль y = x,
# а наклон регрессии был бы близок к 1. При естественном росте явки новые
# избиратели делятся между кандидатами, и наклоны заметно меньше 1.

library(data.table)
library(ggplot2)

MIN_APTOS <- 50

# штаты со 2-м туром выборов губернатора (detalhe_votacao_secao_2026_BRASIL.csv,
# DS_CARGO == "Governador", NR_TURNO == 2): там больше поводов прийти на 2-й тур
UF_2T_GOVERNADOR <- c("AC", "AM", "DF", "ES", "RN", "TO")

key_cols <- c("SG_UF", "CD_MUNICIPIO", "NR_ZONA", "NR_SECAO")
cols <- c(key_cols, "regiao", "exterior", "instalada", "QT_APTOS", "QT_COMPARECIMENTO",
          "turnout", "validos", "v_13", "v_22")

t1 <- readRDS("data/processed/secoes_t1.rds")[, ..cols]
t2 <- readRDS("data/processed/secoes_t2.rds")[, ..cols]
p  <- merge(t1, t2, by = c(key_cols, "regiao", "exterior"), suffixes = c("_1", "_2"))
message("Участков в обоих турах: ", nrow(p),
        "; с изменившимся числом избирателей: ", p[QT_APTOS_1 != QT_APTOS_2, .N])

p <- p[instalada_1 & instalada_2 & !exterior & pmin(QT_APTOS_1, QT_APTOS_2) >= MIN_APTOS &
         validos_1 > 0 & validos_2 > 0]
message("Участков в анализе: ", nrow(p))

p[, `:=`(
  d_turnout = turnout_2 - turnout_1,
  # изменение голосов кандидата в долях от списка избирателей — в тех же единицах, что явка
  d_lula    = v_13_2 / QT_APTOS_2 - v_13_1 / QT_APTOS_1,
  d_flavio  = v_22_2 / QT_APTOS_2 - v_22_1 / QT_APTOS_1,
  gov_2t    = SG_UF %in% UF_2T_GOVERNADOR
)]

theme_set(
  theme_minimal(base_size = 11) +
    theme(
      plot.background      = element_rect(fill = "#fcfcfb", colour = NA),
      panel.grid.minor     = element_blank(),
      panel.grid.major     = element_line(colour = "#e1e0d9", linewidth = 0.3),
      text                 = element_text(colour = "#0b0b0b"),
      axis.text            = element_text(colour = "#898781"),
      plot.subtitle        = element_text(colour = "#52514e"),
      plot.caption         = element_text(colour = "#898781", hjust = 0),
      strip.text           = element_text(colour = "#52514e", face = "bold"),
      legend.position      = "top",
      legend.justification = "left"
    )
)

fonte  <- "Источник: TSE, votacao_secao / detalhe_votacao_secao 2026, 1-й и 2-й туры"
pp     <- scales::label_number(accuracy = 1, scale = 100, suffix = " п.п.", style_positive = "plus")
out_dir <- "output/comparacao"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
salvar <- function(g, nome, w = 10, h = 6) {
  ggsave(file.path(out_dir, nome), g, width = w, height = h, dpi = 150, bg = "#fcfcfb")
}

# --- 1. изменение явки по регионам ------------------------------------------
# на участке в ~320 избирателей один человек — это ~0.3 п.п., поэтому корзины уже 1 п.п.
# дают ложную "гребёнку" (в соседние корзины попадает разное число возможных значений)
g1 <- ggplot(p, aes(d_turnout)) +
  geom_histogram(binwidth = 0.01, center = 0, fill = "#2a78d6", colour = "#fcfcfb", linewidth = 0.1) +
  geom_vline(xintercept = 0, colour = "#52514e", linewidth = 0.3) +
  facet_wrap(~regiao, scales = "free_y") +
  scale_x_continuous(labels = pp, limits = c(-0.25, 0.25)) +
  labs(title = "Изменение явки между 1-м и 2-м турами на тех же участках",
       subtitle = "Число участков в корзине шириной 1 п.п.",
       x = "Явка во 2-м туре минус явка в 1-м", y = "Участков", caption = fonte)
salvar(g1, "01_delta_turnout_por_regiao.png", w = 12, h = 7)

# --- 2. прирост явки x прирост голосов кандидата -----------------------------
dd <- melt(p[, .(d_turnout, `Lula (13)` = d_lula, `Flávio Bolsonaro (22)` = d_flavio)],
           id.vars = "d_turnout", variable.name = "cand", value.name = "d_cand")
dd[, cand := factor(cand, levels = c("Flávio Bolsonaro (22)", "Lula (13)"))]

g2 <- ggplot(dd, aes(d_turnout, d_cand)) +
  geom_bin2d(binwidth = c(0.005, 0.005)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "#e34948", linewidth = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "#0b0b0b", linewidth = 0.5, se = FALSE) +
  scale_fill_gradient(low = "#cde2fb", high = "#0d366b", trans = "log10",
                      name = "Участков", labels = scales::label_number()) +
  facet_wrap(~cand) +
  coord_cartesian(xlim = c(-0.2, 0.2), ylim = c(-0.2, 0.4)) +
  scale_x_continuous(labels = pp) +
  scale_y_continuous(labels = pp) +
  labs(title = "Прирост явки и прирост голосов кандидата на участке",
       subtitle = "Обе величины в долях от списка избирателей. Красная пунктирная линия y = x — так выглядел бы вброс в пользу кандидата; чёрная — регрессия",
       x = "Изменение явки", y = "Изменение голосов кандидата", caption = fonte) +
  theme(legend.position = "right", legend.title = element_text(colour = "#52514e"))
salvar(g2, "02_delta_turnout_vs_delta_votos.png", w = 12, h = 6)

# --- 3. сводка по штатам -----------------------------------------------------
inclinacao <- function(y, x) cov(x, y) / var(x)

resumo <- function(x) x[, .(
  secoes            = .N,
  turnout_1         = round(sum(QT_COMPARECIMENTO_1) / sum(QT_APTOS_1), 4),
  turnout_2         = round(sum(QT_COMPARECIMENTO_2) / sum(QT_APTOS_2), 4),
  d_turnout_sd      = round(sd(d_turnout), 4),
  # доля участков, где явка упала более чем на 10 п.п.
  queda_10pp        = round(mean(d_turnout < -0.10), 4),
  share_lula_2      = round(sum(v_13_2) / sum(validos_2), 4),
  # наклон регрессии прироста голосов кандидата на прирост явки (вброс: около 1)
  slope_lula        = round(inclinacao(d_lula, d_turnout), 3),
  slope_flavio      = round(inclinacao(d_flavio, d_turnout), 3)
)]

resumo_uf <- p[, resumo(.SD), by = .(regiao, SG_UF, gov_2t)][order(regiao, SG_UF)]
resumo_br <- cbind(regiao = "Brasil", SG_UF = "BR", gov_2t = NA, resumo(p))
resumo_uf <- rbind(resumo_uf, resumo_br)
fwrite(resumo_uf, file.path(out_dir, "03_resumo_uf.csv"))
print(resumo_uf)
