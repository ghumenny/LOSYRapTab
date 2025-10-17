#' @title Generowanie parametrów dla wskaźnika S7
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'    dla wskaźnika S7. Parametry te posłużą do późniejszego generowania rozkładów.
#'    Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'    (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'    Zmienna `kryterium` jest dobierana dynamicznie: dla typów szkół
#'    "Liceum ogólnokształcące", "Liceum dla dorosłych" i "Szkoła specjalna przysposabiająca do pracy"
#'    przyjmuje wartość "sexf", a dla pozostałych typów szkół przyjmuje "sexf" i "nazwa_zaw".
#'    **Agregacja jest wykonywana dla województw (`typ="woj"`) lub dla branż (`typ="bran"`).**
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "S7"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**, `typ_szk2`,
#'    `rok_abs`, `edycja`, `kryterium`, `rok`, `miesiac` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select bind_rows setdiff mutate filter rowwise ungroup rename
#' @importFrom tidyr expand_grid unnest
#' @importFrom purrr map
#' @importFrom rlang sym
#' @export
generuj_ramke_s7_parametry <- function(glowna_edycja, typ = "woj") {
  roki_s7 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_s7_ograniczone_szkoly <- expand_grid(
    wskaznik = "S7",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = TYPY_SZKOL_OGOL,
    rok_abs = roki_s7,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO_s7,
    wynik = list(NULL)
  )

  ramka_s7_pozostale_szkoly <- expand_grid(
    wskaznik = "S7",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
    rok_abs = roki_s7,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA_s7,
    wynik = list(NULL)
  )

  ramka_s7 <- bind_rows(ramka_s7_ograniczone_szkoly, ramka_s7_pozostale_szkoly) %>%
    rowwise() %>% # Iterujemy po każdym wierszu
    mutate(
      rok_data = list(
        if (kryterium %in% c("sexf", "nazwa_zaw", "teryt_pow_szk")) {
          # Tworzymy ramkę danych rok-miesiąc dla każdego wiersza
          tibble(
            rok = seq(rok_abs, edycja - 1),
            miesiac = 12
          )
        } else {
          tibble(rok = NA_integer_, miesiac = NA_integer_)
        }
      )
    ) %>%
    ungroup() %>% # Wychodzimy z trybu rowwise
    unnest(cols = rok_data, keep_empty = TRUE) %>% # Rozwijamy kolumnę listową rok_data
    mutate(wynik = list(NULL)) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium, rok, miesiac, wynik)


  return(ramka_s7)
}

#' @title Generowanie parametrów dla wskaźnika D1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'    dla wskaźnika D1. Parametry te posłużą do późniejszego generowania rozkładów.
#'    Wskaźnik D1 jest obliczany tylko dla typów szkół **niebędących**
#'    "Liceum ogólnokształcące", "Liceum dla dorosłych" i "Szkoła specjalna przysposabiająca do pracy".
#'    Rok akademicki (`rok_abs`) jest obliczany na podstawie głównej edycji
#'    (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'    Dla tych typów szkół zmienna `kryterium` zawsze przyjmuje wartości "sexf" i "nazwa_zaw".
#'    **Agregacja jest wykonywana dla województw (`typ="woj"`) lub dla branż (`typ="bran"`).**
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "D1"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**, `typ_szk2`,
#'    `rok_abs`, `edycja`, `kryterium`, `rok`, `miesiac` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select setdiff
#' @importFrom tidyr expand_grid
#' @importFrom rlang sym
#' @export
generuj_ramke_d1_parametry <- function(glowna_edycja, typ = "woj") {
  roki_d1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)
  typy_szk_d1 <- setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_d1 <- expand_grid(
    wskaznik = "D1",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = typy_szk_d1,
    rok_abs = roki_d1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    rok = NA_integer_, # Bezpośrednie NA_integer_
    miesiac = NA_integer_, # Bezpośrednie NA_integer_
    wynik = list(NULL)
  ) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium, rok, miesiac, wynik)

  return(ramka_d1)
}

#' @title Generowanie parametrów dla wskaźnika D2
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'    dla wskaźnika D2. Parametry te posłużą do późniejszego generowania rozkładów.
#'    Wskaźnik D2 jest obliczany tylko dla typów szkół z listy:
#'    "Liceum ogólnokształcące", "Liceum dla dorosłych", "Technikum", "Branżowa szkoła II stopnia".
#'    Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'    (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'    Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#'    **Agregacja jest wykonywana dla województw (`typ="woj"`) lub dla branż (`typ="bran"`).**
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "D2"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**, `typ_szk2`,
#'    `rok_abs`, `edycja`, `kryterium` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select intersect bind_rows
#' @importFrom tidyr expand_grid
#' @importFrom rlang sym
#' @export
generuj_ramke_d2_parametry <- function(glowna_edycja, typ = "woj") {
  roki_d2 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_d2_ograniczone_szkoly <- expand_grid(
    wskaznik = "D2",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = intersect(TYPY_SZKOL_D2, TYPY_SZKOL_OGOL),
    rok_abs = roki_d2,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    wynik = list(NULL),
    rok = NA_integer_,
    miesiac = NA_integer_
  )

  ramka_d2_pozostale_szkoly <- expand_grid(
    wskaznik = "D2",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = intersect(TYPY_SZKOL_D2, setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL)),
    rok_abs = roki_d2,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL),
    rok = NA_integer_,
    miesiac = NA_integer_
  )

  ramka_d2 <- bind_rows(ramka_d2_ograniczone_szkoly, ramka_d2_pozostale_szkoly) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium, rok, miesiac, wynik)

  return(ramka_d2)
}

#' @title Generowanie parametrów dla wskaźnika K1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'    dla wskaźnika K1. Wskaźnik K1 posiada dwie kategorie analizy
#'    ("kategoria_k1_1" i "kategoria_k1_2"), z których każda ma własny zestaw
#'    wartości do wykorzystania w analizie (przechowywany w kolumnie `parametr_k1_wartosci`).
#'    Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'    (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'    Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'    Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#'    **Agregacja jest wykonywana dla województw (`typ="woj"`) lub dla branż (`typ="bran"`).**
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "K1"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**, `typ_szk2`,
#'    `rok_abs`, `edycja`, `kryterium`, `typ_k1_analizy` (identyfikator kategorii analizy K1),
#'    `parametr_k1_wartosci` (lista wartości specyficznych dla kategorii K1), `rok`, `miesiac`
#'    oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select mutate bind_rows setdiff intersect case_when
#' @importFrom tidyr expand_grid unnest
#' @importFrom rlang sym
#' @export
generuj_ramke_k1_parametry <- function(glowna_edycja, typ = "woj") {
  roki_k1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_k1_kat1_og <- expand_grid(
    wskaznik = "K1",
    typ_k1_analizy = "kategoria_k1_1",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = intersect(TYPY_SZKOL_OGOL, TYPY_SZKOL_K1m),
    rok_abs = roki_k1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    wynik = list(NULL)
  )

  ramka_k1_kat1_za <- expand_grid(
    wskaznik = "K1",
    typ_k1_analizy = "kategoria_k1_1",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = setdiff(TYPY_SZKOL_K1m, TYPY_SZKOL_OGOL),
    rok_abs = roki_k1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL)
  )

  ramka_k1_kat2 <- expand_grid(
    wskaznik = "K1",
    typ_k1_analizy = "kategoria_k1_2",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = TYPY_SZKOL_K1b,
    rok_abs = roki_k1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL)
  )

  ramka_k1 <- bind_rows(ramka_k1_kat1_og, ramka_k1_kat1_za, ramka_k1_kat2) %>%
    rowwise() %>% # Iterujemy po każdym wierszu
    mutate(
      rok_data = list(
        if (kryterium %in% c("sexf", "nazwa_zaw", "teryt_pow_szk")) {
          # Tworzymy ramkę danych rok-miesiąc dla każdego wiersza
          tibble(
            rok = seq(rok_abs, edycja - 1),
            miesiac = 12
          )
        } else {
          tibble(rok = NA_integer_, miesiac = NA_integer_)
        }
      )
    ) %>%
    ungroup() %>% # Wychodzimy z trybu rowwise
    unnest(cols = rok_data, keep_empty = TRUE) %>% # Rozwijamy kolumnę listową rok_data
    mutate(wynik = list(NULL))  %>%
    mutate(parametr_K1 = case_when(
      typ_k1_analizy == "kategoria_k1_1" ~ list(c("nauka_studiaf", "nauka_spolicf", "nauka_kkzf")),
      typ_k1_analizy == "kategoria_k1_2" ~ list(c("nauka_bs2stf", "nauka_loddf", "nauka_kkzf")),
      .default = list(NA_character_) # Zapewnienie, że domyślna wartość jest też listą z wektorem
    )) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium,
           rok, miesiac, typ_k1_analizy, parametr_K1, wynik)
  return(ramka_k1)

}

#' @title Generowanie parametrów dla wskaźnika K2
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'    dla wskaźnika K2. Wskaźnik K2 posiada dwie kategorie analizy: "dziedziny"
#'    i "dyscypliny" (przechowywane w kolumnie `parametr_K2`).
#'    Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'    (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'    Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'    Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#'    **Agregacja jest wykonywana dla województw (`typ="woj"`) lub dla branż (`typ="bran"`).**
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "K2"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**, `typ_szk2`,
#'    `rok_abs`, `edycja`, `kryterium`, `parametr_K2`, `rok`, `miesiac` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select bind_rows setdiff
#' @importFrom tidyr expand_grid
#' @importFrom rlang sym
#' @export
generuj_ramke_k2_parametry <- function(glowna_edycja, typ = "woj") {
  roki_k2 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)
  typy_analizy_k2 <- c("dziedziny", "dyscypliny")

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramki_k2 <- list()

  for (typ_analizy in typy_analizy_k2) {
    ramka_k2_typ_ograniczone <- expand_grid(
      wskaznik = "K2",
      parametr_K2 = typ_analizy,
      !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
      typ_szk2 = TYPY_SZKOL_K2o,
      rok_abs = roki_k2,
      edycja = glowna_edycja,
      kryterium = KRYTERIUM_SEXY_TYLKO,
      wynik = list(NULL),
      rok = NA_integer_,
      miesiac = NA_integer_
    )

    ramka_k2_typ_pozostale <- expand_grid(
      wskaznik = "K2",
      parametr_K2 = typ_analizy,
      !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
      typ_szk2 = TYPY_SZKOL_K2z,
      rok_abs = roki_k2,
      edycja = glowna_edycja,
      kryterium = KRYTERIUM_SEXY_I_NAZWA,
      wynik = list(NULL),
      rok = NA_integer_,
      miesiac = NA_integer_
    )

    ramki_k2[[paste0(typ_analizy, "_ograniczone")]] <- ramka_k2_typ_ograniczone
    ramki_k2[[paste0(typ_analizy, "_pozostale")]] <- ramka_k2_typ_pozostale
  }

  return(bind_rows(ramki_k2) %>%
           select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium,
                  rok, miesiac, parametr_K2, wynik))
}

#' @title Generowanie parametrów dla wskaźnika W1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'    dla wskaźnika W1. Parametry te posłużą do późniejszego generowania rozkładów.
#'    Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'    (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'    Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'    Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#'    **Agregacja jest wykonywana dla województw (`typ="woj"`) lub dla branż (`typ="bran"`).**
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "W1"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**, `typ_szk2`,
#'    `rok_abs`, `edycja`, `kryterium`, `rok`, `miesiac` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select bind_rows setdiff mutate filter rowwise ungroup rename
#' @importFrom tidyr expand_grid unnest
#' @importFrom purrr map map2_dbl
#' @importFrom rlang sym
#' @export
generuj_ramke_w1_parametry <- function(glowna_edycja, typ = "woj") {
  roki_w1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_w1_ograniczone_szkoly <- expand_grid(
    wskaznik = "W1",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = TYPY_SZKOL_OGOL,
    rok_abs = roki_w1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO
  ) %>%
    mutate(wynik = list(NULL))

  ramka_w1_pozostale_szkoly <- expand_grid(
    wskaznik = "W1",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
    rok_abs = roki_w1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA
  ) %>%
    mutate(wynik = list(NULL))

  ramka_w1 <- bind_rows(ramka_w1_ograniczone_szkoly, ramka_w1_pozostale_szkoly) %>%
    rowwise() %>%
    mutate(
      rok_vals = list(seq(rok_abs, edycja)),
      miesiac_vals = list(
        purrr::map2_dbl(rok_vals, rok_abs, ~{
          if (.x == .y) {
            12
          } else if (.y < .x && .x < edycja) {
            NA_real_
          } else if (.y < .x && .x == edycja) {
            3
          } else {
            NA_real_
          }
        })
      )
    ) %>%
    unnest(cols = c(rok_vals, miesiac_vals), keep_empty = TRUE) %>%
    rename(rok = rok_vals, miesiac = miesiac_vals) %>%
    ungroup() %>%
    mutate(wynik = list(NULL)) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium, rok, miesiac, wynik)

  return(ramka_w1)
}

#' @title Generowanie parametrów dla wskaźnika B1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'    dla wskaźnika B1. Parametry te posłużą do późniejszego generowania rozkładów.
#'    Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'    (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'    Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'    Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#'    **Agregacja jest wykonywana dla województw (`typ="woj"`) lub dla branż (`typ="bran"`).**
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "B1"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**, `typ_szk2`,
#'    `rok_abs`, `edycja`, `kryterium`, `rok`, `miesiac` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select bind_rows setdiff mutate filter rowwise ungroup rename
#' @importFrom tidyr expand_grid unnest
#' @importFrom purrr map
#' @importFrom rlang sym
#' @export
generuj_ramke_b1_parametry <- function(glowna_edycja, typ = "woj") {
  roki_b1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_b1_ograniczone_szkoly <- expand_grid(
    wskaznik = "B1",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = TYPY_SZKOL_OGOL,
    rok_abs = roki_b1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO_b1,
    wynik = list(NULL)
  )

  ramka_b1_pozostale_szkoly <- expand_grid(
    wskaznik = "B1",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
    rok_abs = roki_b1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA_b1,
    wynik = list(NULL)
  )

  ramka_b1 <- bind_rows(ramka_b1_ograniczone_szkoly, ramka_b1_pozostale_szkoly) %>%
    rowwise() %>% # Iterujemy po każdym wierszu
    mutate(
      rok_data = list(
        if (kryterium %in% c("sexf", "nazwa_zaw", "teryt_pow_szk")) {
          # Tworzymy ramkę danych rok-miesiąc dla każdego wiersza
          tibble(
            rok = seq(rok_abs, edycja - 1),
            miesiac = 12
          )
        } else {
          tibble(rok = NA_integer_, miesiac = NA_integer_)
        }
      )
    ) %>%
    ungroup() %>% # Wychodzimy z trybu rowwise
    unnest(cols = rok_data, keep_empty = TRUE) %>% # Rozwijamy kolumnę listową rok_data
    mutate(wynik = list(NULL)) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium,
           rok, miesiac, wynik)


  return(ramka_b1)
}

#' @title Generowanie parametrów ogólnych dla wskaźników z typ_szk2 = NULL i kryterium = sexf
#' @description Funkcja tworzy ramkę danych dla wskaźników, gdzie typ_szk2 jest
#' traktowany jako NULL (agregacja na poziomie województwa/branży), a kryterium to
#' zawsze "sexf". Jest to użyteczne dla analiz ogólnych, niezależnych
#' od specyficznego typu szkoły.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (nazwa wskaźnika, np. "S7"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**,
#'    `typ_szk2` (NULL), `rok_abs`, `edycja`, `kryterium` (z wartością "sexf"), `rok`, `miesiac`
#'    oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select
#' @importFrom tidyr expand_grid
#' @importFrom rlang sym
#' @export
generuj_ramke_null_typszk2_sexf_parametry <- function(glowna_edycja, typ = "woj") {
  roki_abs <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_null_sexf <- expand_grid(
    wskaznik = "typ_szk2",
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = NA_character_, # Tutaj typ_szk2 jest NULL
    rok_abs = roki_abs,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    rok = NA_integer_,
    miesiac = NA_integer_,
    wynik = list(NULL)
  ) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium,
           rok, miesiac, wynik)

  return(ramka_null_sexf)
}

#' @title Generowanie parametrów ogólnych dla wskaźników z typ_szk2 = NULL i kryterium = nazwa_zaw dla wybranej grupy szkół
#' @description Funkcja tworzy ramkę danych dla ogólnego wskaźnika obejmującego
#' specyficzną grupę szkół, gdzie typ_szk2 jest traktowany jako NULL
#' (agregacja na poziomie województwa/branży dla tej grupy), a kryterium to
#' "nazwa_zaw". Jest to użyteczne dla analiz ogólnych w kontekście zawodów
#' dla określonej podgrupy szkół.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której generowane są parametry. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'    `wskaznik` (z wartością "meta_zaw"), **`WOJ_NAZWA` lub `branza` (zależnie od `typ`)**,
#'    `typ_szk2` (NULL), `rok_abs`, `edycja`, `kryterium` (z wartością "nazwa_zaw"), `rok`, `miesiac`
#'    oraz `wynik` (inicjowany jako `NULL`). Kolumna `parametr_grupa_szkol` będzie zawierać
#'    listę typów szkół objętych tą agregacją.
#' @importFrom dplyr select mutate
#' @importFrom tidyr expand_grid
#' @importFrom rlang sym
#' @export
generuj_ramke_null_typszk2_nazwazaw_parametry <- function(glowna_edycja, typ = "woj") {
  roki_abs <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Ustalenie dynamicznej nazwy kolumny i wektora wartości
  grupa <- ustaw_grupe_parametrów(typ)
  nazwa_kolumny <- grupa$nazwa
  wartosci_kolumny <- grupa$wartosci

  ramka_null_nazwazaw <- expand_grid(
    wskaznik = "meta_zaw", # Nazwa wskaźnika dla tej specyficznej agregacji
    !!rlang::sym(nazwa_kolumny) := wartosci_kolumny, # Dynamiczna kolumna
    typ_szk2 = NA_character_, # Tutaj typ_szk2 jest NULL
    rok_abs = roki_abs,
    edycja = glowna_edycja,
    kryterium = "nazwa_zaw", # Zawsze nazwa_zaw dla tej tabeli
    wynik = list(NULL),
    rok = NA_integer_,
    miesiac = NA_integer_
  ) %>%
    # Dodajemy kolumnę, która przechowuje listę typów szkół, które są agregowane
    mutate(parametr_grupa_szkol = list(TYPY_SZKOL_ZAW)) %>%
    select(wskaznik, !!rlang::sym(nazwa_kolumny), typ_szk2, rok_abs, edycja, kryterium,
           rok, miesiac, parametr_grupa_szkol, wynik)

  return(ramka_null_nazwazaw)
}

#' @title Generowanie parametrów ogólnych dla wskaźników z typ_szk2 = NULL
#' i kryterium = teryt_woj dla wybranej grupy szkół
#' @description Funkcja tworzy ramkę danych dla ogólnego wskaźnika obejmującego
#' wszystkie szkoły, gdzie typ_szk2 jest traktowany jako NULL
#' (agregacja na poziomie województwa), a kryterium to
#' "teryt_woj". Jest to użyteczne dla analiz migracji.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "meta_zaw"), `WOJ_NAZWA`, `typ_szk2` (NULL), `rok_abs`,
#'   `edycja`, `kryterium` (z wartością "teryt_woj"), `rok`, `miesaic`
#'    oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select mutate rowwise ungroup
#' @importFrom tidyr expand_grid unnest
#' @importFrom purrr map
#' @export
generuj_ramke_migracje <- function(glowna_edycja) {
  roki_abs <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  ramka_null_nazwazaw <- expand_grid(
    wskaznik = "migracje", # Nazwa wskaźnika dla tej specyficznej agregacji
    WOJ_NAZWA = WOJ_NAZWY[WOJ_NAZWY != "Polska"],
    typ_szk2 = NA_character_,
    rok_abs = roki_abs,
    edycja = glowna_edycja,
    kryterium = "teryt_woj",
    wynik = list(NULL)
      ) %>%
    rowwise() %>%
    mutate(rok_data = list(
      tibble(
      rok = seq(rok_abs, edycja - 1),
           miesiac = 12))) %>%
    ungroup() %>%
    unnest(cols = rok_data, keep_empty = TRUE) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium,
           rok, miesiac, wynik)

  return(ramka_null_nazwazaw)
}

#' @title Łączenie ramek danych parametrów dla wszystkich wskaźników
#' @description Funkcja przyjmuje główną edycję projektu i generuje osobne ramki danych
#'    dla każdego wskaźnika (S7, D1, D2, K1, K2, W1, B1 oraz dla tabel meta)
#'    za pomocą dedykowanych funkcji.
#'    Następnie łączy te ramki w jedną spójną ramkę, która może służyć jako szablon
#'    do przechowywania parametrów i wyników rozkładów.
#'    **Generowanie danych jest zależne od parametru `typ`:** dla `typ="woj"`
#'    generowane są wszystkie ramki, włącznie z migracją, a kolumna grupująca to
#'    `WOJ_NAZWA`. Dla `typ="bran"` generowane są parametry dla branż, a ramka migracji
#'    (`ramka_migracje`) jest pomijana.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'    dla której mają być generowane dane. Np. 2025.
#' @param typ Ciąg znaków określający poziom agregacji: **"woj"** (województwa, domyślnie)
#'    lub **"bran"** (branże).
#' @return Jedna duża ramka danych typu `tibble` zawierająca unikalne kombinacje
#'    parametrów dla wszystkich wskaźników i typów agregacji. Kolumna grupująca
#'    będzie nosiła nazwę **`WOJ_NAZWA` lub `branza`** w zależności od parametru `typ`.
#' @importFrom dplyr bind_rows
#' @export
polacz_ramki_wskaznikow <- function(glowna_edycja, typ = "woj") {
  # Walidacja parametru typ
  if (!(typ %in% c("woj", "bran"))) {
    stop("Parametr 'typ' musi przyjmować jedną z wartości: 'woj' lub 'bran'.")
  }

  ramka_s7 <- generuj_ramke_s7_parametry(glowna_edycja, typ)
  ramka_d1 <- generuj_ramke_d1_parametry(glowna_edycja, typ)
  ramka_d2 <- generuj_ramke_d2_parametry(glowna_edycja, typ)
  ramka_k1 <- generuj_ramke_k1_parametry(glowna_edycja, typ)
  ramka_k2 <- generuj_ramke_k2_parametry(glowna_edycja, typ)
  ramka_w1 <- generuj_ramke_w1_parametry(glowna_edycja, typ)
  ramka_b1 <- generuj_ramke_b1_parametry(glowna_edycja, typ)
  ramka_typszk_N <- generuj_ramke_null_typszk2_sexf_parametry(glowna_edycja, typ)
  ramka_typszk_zaw <- generuj_ramke_null_typszk2_nazwazaw_parametry(glowna_edycja, typ)

  list_ramek <- list(
    ramka_s7,
    ramka_d1,
    ramka_d2,
    ramka_k1,
    ramka_k2,
    ramka_w1,
    ramka_b1,
    ramka_typszk_N,
    ramka_typszk_zaw
  )


  if (typ == "woj") {
    ramka_migracje <- generuj_ramke_migracje(glowna_edycja)
    list_ramek <- append(list_ramek, list(ramka_migracje))
  }

  finalna_ramka <- bind_rows(list_ramek)

  return(finalna_ramka)
}