# Analýza úhrad zdravotnických prostředků v ČR

Interaktivní report analyzující objem a strukturu úhrad zdravotnických prostředků hrazených z veřejného zdravotního pojištění v České republice (2020–2024).

**Online verze:** https://ropej.github.io/nzip-uhrady-zdravotnickych-prostredku/

## Zdroje dat

- **Národní registr hrazených zdravotních služeb (NRHZS)** – ÚZIS ČR
  - Zdravotnické prostředky podle kódu, měsíce a pojišťovny (NR-04-30)
  - Zdravotnické prostředky podle kódu, měsíce a IČZ (NR-04-31)
  - Číselník IČZ a pojišťoven (NR-04-64)

## Obsah analýzy

- Dlouhodobý vývoj a sezónnost úhrad
- Regionální koncentrace úhrad
- Struktura podle typu péče, odborností a skupin prostředků
- Koncentrace úhrad na úrovni poskytovatelů (Lorenzova křivka, Giniho koeficient)
- Statistické ověření: LMDI dekompozice (objem vs. cena), HHI, Cramérovo V, Kruskal–Wallis, Mann–Kendall

## Soubory

- `nzip-zdravotnicke-prostredky.qmd` – zdrojový Quarto dokument
- `nzip-funkce.R` – pomocné statistické funkce
- `pzt_ciselnik.rds` – číselník skupin zdravotnických prostředků
- `docs/index.html` – renderovaný report (GitHub Pages)

## Autor

Romana Pejcalová
