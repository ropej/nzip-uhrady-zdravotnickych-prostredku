# ---------------------------------------------------------------------------
# Pomocné statistické funkce pro analýzu úhrad zdravotnických prostředků
# Načítá se v hlavním dokumentu přes source("nzip-funkce.R")
# ---------------------------------------------------------------------------

# --- Logaritmický průměr (váha v LMDI dekompozici) -------------------------
# Pro a == b vrací a (limitní hodnota), jinak (a - b) / (log a - log b).
log_mean <- function(a, b) {
  ifelse(a == b, a, (a - b) / (log(a) - log(b)))
}

# --- LMDI dekompozice dvoufaktorového součinu V = Q * P --------------------
# Rozkládá změnu hodnoty V mezi dvěma obdobími na příspěvek objemu (Q)
# a příspěvek ceny (P). Příspěvky jsou aditivní: d_total = d_volume + d_price.
#   V0, V1 – hodnota (úhrada) na začátku a na konci období
#   Q0, Q1 – objem (počet balení)
#   P0, P1 – cena na jednotku (úhrada / balení)
lmdi_2f <- function(V0, V1, Q0, Q1, P0, P1) {
  L <- log_mean(V1, V0)
  list(
    d_total  = V1 - V0,
    d_volume = L * log(Q1 / Q0),
    d_price  = L * log(P1 / P0)
  )
}

# --- Giniho koeficient ------------------------------------------------------
# Míra nerovnoměrnosti rozdělení (0 = dokonalá rovnost, 1 = maximální
# koncentrace). Vstupem je vektor nezáporných hodnot.
gini <- function(x) {
  x <- sort(x[!is.na(x)])
  n <- length(x)
  if (n == 0 || sum(x) == 0) return(NA_real_)
  i <- seq_len(n)
  (2 * sum(i * x)) / (n * sum(x)) - (n + 1) / n
}

# --- Bootstrapový interval spolehlivosti pro Giniho koeficient -------------
# Neparametrický percentilový interval z R bootstrapových výběrů.
gini_bootstrap_ci <- function(x, R = 1000, conf = 0.95, seed = 42) {
  x <- x[!is.na(x)]
  if (length(x) < 2) return(c(lwr = NA_real_, upr = NA_real_))
  set.seed(seed)
  boot <- replicate(R, gini(sample(x, replace = TRUE)))
  alpha <- (1 - conf) / 2
  out <- stats::quantile(boot, c(alpha, 1 - alpha), na.rm = TRUE)
  c(lwr = unname(out[1]), upr = unname(out[2]))
}

# --- Herfindahlův–Hirschmanův index (HHI) ----------------------------------
# Míra koncentrace trhu. Vstupem jsou podíly nebo absolutní hodnoty
# (ty se na podíly přepočítají). Vrací HHI na škále 0–1 a efektivní
# počet subjektů (1 / HHI).
#   x             – vektor hodnot nebo podílů
#   already_shares – TRUE, pokud x už jsou podíly (součet 1)
hhi <- function(x, already_shares = FALSE) {
  x <- x[!is.na(x)]
  shares <- if (already_shares) x else x / sum(x)
  h <- sum(shares^2)
  c(hhi = h, eff_n = 1 / h)
}

# --- Cramérovo V + chí-kvadrát test nezávislosti ---------------------------
# Měří sílu asociace dvou kategoriálních proměnných (0 = nezávislost,
# 1 = perfektní asociace). Vrací též výsledek chí-kvadrát testu.
# Vstupem mohou být dva vektory, nebo přímo kontingenční tabulka (x = tab).
#
# Ověření podmínek dobré aproximace (Cochranovo pravidlo):
#   - žádná očekávaná četnost není < 1
#   - nejvýše 20 % buněk má očekávanou četnost < 5
# approx_ok = TRUE, pokud jsou obě podmínky splněny.
cramer_v <- function(x, y = NULL,
                     breaks = c(0.1, 0.3, 0.5),
                     labels = c("zanedbatelný efekt", "slabý efekt",
                                "střední závislost", "silná závislost")) {
  tab <- if (is.null(y)) as.table(x) else table(x, y)
  chi <- suppressWarnings(stats::chisq.test(tab))
  n   <- sum(tab)
  phi2 <- as.numeric(chi$statistic) / n
  k <- min(nrow(tab), ncol(tab))
  v <- sqrt(phi2 / (k - 1))

  # Ověření podmínek dobré aproximace
  exp        <- chi$expected
  podil_ge5  <- mean(exp >= 5)          # podíl buněk s očekávanou četností >= 5
  exp_min    <- min(exp)                # nejmenší očekávaná četnost
  approx_ok  <- (podil_ge5 >= 0.8) && (exp_min >= 1)

  # Long tabulka: frekvence, podíl x vůči y (%), podíl y v x (%), pořadí
  col_pct    <- prop.table(tab, margin = 2) * 100          # sloupcová % (x vůči y)
  row_pct    <- prop.table(tab, margin = 1) * 100          # řádková %  (y v x)
  # Pořadí y podle frekvence v rámci skupiny x (1 = nejčetnější)
  rank_y_v_x <- t(apply(tab, 1, function(r) rank(-r, ties.method = "min")))

  tab_long <- data.frame(
    x                 = rep(rownames(tab), times = ncol(tab)),
    y                 = rep(colnames(tab), each  = nrow(tab)),
    frekvence         = as.vector(tab),
    podil_x_against_y = round(as.vector(col_pct), 1),
    podil_y_in_x      = round(as.vector(row_pct), 1),
    order             = as.vector(rank_y_v_x),
    stringsAsFactors  = FALSE
  )

  list(
    cramer_v   = v,
    label      = cramer_v_label(v, breaks = breaks, labels = labels),
    chisq      = as.numeric(chi$statistic),
    df         = as.numeric(chi$parameter),
    p_value    = chi$p.value,
    approx_ok  = approx_ok,
    table      = tab_long
  )
}

# --- Slovní hodnocení velikosti efektu (Cramérovo V) -----------------------
# Vrací zaokrouhlenou hodnotu a slovní popis síly asociace.
# Hranice (breaks) i popisky (labels) lze upravit argumentem;
# délka labels musí být o 1 větší než délka breaks.
cramer_v_label <- function(v,
                           breaks = c(0.1, 0.3, 0.5),
                           labels = c("zanedbatelný efekt", "slabý efekt",
                                      "střední závislost", "silná závislost")) {
  stopifnot(length(labels) == length(breaks) + 1)
  sila <- labels[findInterval(v, breaks) + 1]
  paste(format(round(v, 3), nsmall = 3, decimal.mark = ","), sila)
}

# --- Velikost účinku epsilon² pro Kruskalův–Wallisův test ------------------
# ε² = H / (n − 1); rozsah 0–1.
epsilon_squared <- function(H, n) {
  unname(H / (n - 1))
}

# --- Rank-biseriální korelace pro Mannův–Whitneyho test --------------------
# Velikost účinku pro porovnání dvou skupin (rozsah −1 až 1).
#   x – numerický vektor
#   g – dvoúrovňový faktor (skupina)
rank_biserial <- function(x, g) {
  g  <- as.factor(g)
  lv <- levels(g)
  x1 <- x[g == lv[1]]
  x2 <- x[g == lv[2]]
  w  <- suppressWarnings(stats::wilcox.test(x1, x2)$statistic)
  n1 <- length(x1)
  n2 <- length(x2)
  unname(1 - 2 * w / (n1 * n2))
}

# --- Formátování p-hodnoty pro výpis v textu -------------------------------
format_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) "< 0,001" else format(round(p, 3), nsmall = 3, decimal.mark = ",")
}


# --- ggplotly s výchozím config  -------------------------------------------
pl <- function(p, ...) {
  ggplotly(p, ...) |>
    config(
      displayModeBar         = TRUE,
      displaylogo            = FALSE,
      modeBarButtonsToRemove = c("pan2d", "select2d", "lasso2d",
                                 "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines")
    )
}


# --- Funkce na odstranění diakritiky ---------------------------------------
remove_accents <- function(s) {
  
  # 1 character substitutions
  old1 <- "áéěíóúůčďňřšťž"
  new1 <- "aeeiouucdnrstz"
  s1 <- chartr(old1, new1, s)
  
  # 2 character substitutions 
  old2 <- c("ß")
  new2 <- c("ss")
  s2 <- s1
  
  # finalize the function
  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
  
  s2
}
