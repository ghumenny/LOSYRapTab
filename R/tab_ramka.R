
#' @title Generowanie parametrów dla wskaźnika S7
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'   dla wskaźnika S7. Parametry te posłużą do późniejszego generowania rozkładów.
#'   Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'   (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'   Zmienna `kryterium` jest dobierana dynamicznie: dla typów szkół
#'   "Liceum ogólnokształcące", "Liceum dla dorosłych" i "Szkoła specjalna przysposabiająca do pracy"
#'   przyjmuje wartość "sexf", a dla pozostałych typów szkół przyjmuje "sexf" i "nazwa_zaw".
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "S7"), `WOJ_NAZWA`, `typ_szk2`, `rok_abs`,
#'   `edycja`, `kryterium` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select mutate bind_rows
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_s7_parametry <- function(glowna_edycja) {
  roki_s7 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  ramka_s7_ograniczone_szkoly <- expand_grid(
    wskaznik = "S7",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = TYPY_SZKOL_OGOL,
    rok_abs = roki_s7,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    wynik = list(NULL)
  )

  ramka_s7_pozostale_szkoly <- expand_grid(
    wskaznik = "S7",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
    rok_abs = roki_s7,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL)
  )

  ramka_s7 <- bind_rows(ramka_s7_ograniczone_szkoly, ramka_s7_pozostale_szkoly) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, wynik)

  return(ramka_s7)
}

#' @title Generowanie parametrów dla wskaźnika D1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'   dla wskaźnika D1. Parametry te posłużą do późniejszego generowania rozkładów.
#'   Wskaźnik D1 jest obliczany tylko dla typów szkół **niebędących**
#'   "Liceum ogólnokształcące", "Liceum dla dorosłych" i "Szkoła specjalna przysposabiająca do pracy".
#'   Rok akademicki (`rok_abs`) jest obliczany na podstawie głównej edycji
#'   (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'   Dla tych typów szkół zmienna `kryterium` zawsze przyjmuje wartości "sexf" i "nazwa_zaw".
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "D1"), `WOJ_NAZWA`, `typ_szk2`, `rok_abs`,
#'   `edycja`, `kryterium` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select setdiff
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_d1_parametry <- function(glowna_edycja) {
  roki_d1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  typy_szk_d1 <- setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL)

  ramka_d1 <- expand_grid(
    wskaznik = "D1",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = typy_szk_d1,
    rok_abs = roki_d1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL)
  ) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, wynik)

  return(ramka_d1)
}

#' @title Generowanie parametrów dla wskaźnika D2
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'   dla wskaźnika D2. Parametry te posłużą do późniejszego generowania rozkładów.
#'   Wskaźnik D2 jest obliczany tylko dla typów szkół z listy:
#'   "Liceum ogólnokształcące", "Liceum dla dorosłych", "Technikum", "Branżowa szkoła II stopnia".
#'   Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'   (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'   Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "D2"), `WOJ_NAZWA`, `typ_szk2`, `rok_abs`,
#'   `edycja`, `kryterium` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select intersect bind_rows
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_d2_parametry <- function(glowna_edycja) {
  roki_d2 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  ramka_d2_ograniczone_szkoly <- expand_grid(
    wskaznik = "D2",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = intersect(TYPY_SZKOL_D2, TYPY_SZKOL_OGOL),
    rok_abs = roki_d2,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    wynik = list(NULL)
  )

  ramka_d2_pozostale_szkoly <- expand_grid(
    wskaznik = "D2",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = intersect(TYPY_SZKOL_D2, setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL)),
    rok_abs = roki_d2,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL)
  )

  ramka_d2 <- bind_rows(ramka_d2_ograniczone_szkoly, ramka_d2_pozostale_szkoly) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, wynik)

  return(ramka_d2)
}

#' @title Generowanie parametrów dla wskaźnika K1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'   dla wskaźnika K1. Wskaźnik K1 posiada dwie kategorie analizy
#'   ("kategoria_k1_1" i "kategoria_k1_2"), z których każda ma własny zestaw
#'   wartości do wykorzystania w analizie (przechowywany w kolumnie `parametr_k1_wartosci`).
#'   Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'   (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'   Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'   Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "K1"), `WOJ_NAZWA`, `typ_szk2`, `rok_abs`,
#'   `edycja`, `kryterium`, `typ_k1_analizy` (identyfikator kategorii analizy K1),
#'   `parametr_k1_wartosci` (lista wartości specyficznych dla kategorii K1)
#'   oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select mutate bind_rows setdiff
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_k1_parametry <- function(glowna_edycja) {
  roki_k1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  kryteria_k1_zestawy <- list(
    "kategoria_k1_1" = c("nauka_studiaf", "nauka_spolicf", "nauka_kkzf"),
    "kategoria_k1_2" = c("nauka_bs2st", "nauka_lodd", "nauka_kkz")
  )

  ramki_k1 <- list()
  for (nazwa_kategorii in names(kryteria_k1_zestawy)) {
    wartosci_kryterium_specyficzne <- kryteria_k1_zestawy[[nazwa_kategorii]]

    ramka_k1_kat_ograniczone <- expand_grid(
      wskaznik = "K1",
      typ_k1_analizy = nazwa_kategorii,
      WOJ_NAZWA = WOJ_NAZWY,
      typ_szk2 = TYPY_SZKOL_OGOL,
      rok_abs = roki_k1,
      edycja = glowna_edycja,
      kryterium = KRYTERIUM_SEXY_TYLKO,
      wynik = list(NULL)
    ) %>%
      mutate(parametr_k1_wartosci = list(wartosci_kryterium_specyficzne))

    ramka_k1_kat_pozostale <- expand_grid(
      wskaznik = "K1",
      typ_k1_analizy = nazwa_kategorii,
      WOJ_NAZWA = WOJ_NAZWY,
      typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
      rok_abs = roki_k1,
      edycja = glowna_edycja,
      kryterium = KRYTERIUM_SEXY_I_NAZWA,
      wynik = list(NULL)
    ) %>%
      mutate(parametr_k1_wartosci = list(wartosci_kryterium_specyficzne))

    ramki_k1[[paste0(nazwa_kategorii, "_ograniczone")]] <- ramka_k1_kat_ograniczone
    ramki_k1[[paste0(nazwa_kategorii, "_pozostale")]] <- ramka_k1_kat_pozostale
  }

  return(bind_rows(ramki_k1) %>%
           select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, typ_k1_analizy, parametr_k1_wartosci, wynik))
}

#' @title Generowanie parametrów dla wskaźnika K2
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'   dla wskaźnika K2. Wskaźnik K2 posiada dwie kategorie analizy: "dziedziny"
#'   i "dyscypliny" (przechowywane w kolumnie `typ_k2_analizy`).
#'   Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'   (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'   Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'   Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "K2"), `WOJ_NAZWA`, `typ_szk2`, `rok_abs`,
#'   `edycja`, `kryterium`, `typ_k2_analizy` (identyfikator kategorii analizy K2)
#'   oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select bind_rows setdiff
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_k2_parametry <- function(glowna_edycja) {
  roki_k2 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)
  typy_analizy_k2 <- c("dziedziny", "dyscypliny")

  ramki_k2 <- list()

  for (typ_analizy in typy_analizy_k2) {
    ramka_k2_typ_ograniczone <- expand_grid(
      wskaznik = "K2",
      typ_k2_analizy = typ_analizy,
      WOJ_NAZWA = WOJ_NAZWY,
      typ_szk2 = TYPY_SZKOL_OGOL,
      rok_abs = roki_k2,
      edycja = glowna_edycja,
      kryterium = KRYTERIUM_SEXY_TYLKO,
      wynik = list(NULL)
    )

    ramka_k2_typ_pozostale <- expand_grid(
      wskaznik = "K2",
      typ_k2_analizy = typ_analizy,
      WOJ_NAZWA = WOJ_NAZWY,
      typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
      rok_abs = roki_k2,
      edycja = glowna_edycja,
      kryterium = KRYTERIUM_SEXY_I_NAZWA,
      wynik = list(NULL)
    )

    ramki_k2[[paste0(typ_analizy, "_ograniczone")]] <- ramka_k2_typ_ograniczone
    ramki_k2[[paste0(typ_analizy, "_pozostale")]] <- ramka_k2_typ_pozostale
  }

  return(bind_rows(ramki_k2) %>%
           select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, typ_k2_analizy, wynik))
}

#' @title Generowanie parametrów dla wskaźnika W1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'   dla wskaźnika W1. Parametry te posłużą do późniejszego generowania rozkładów.
#'   Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'   (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'   Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'   Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "W1"), `WOJ_NAZWA`, `typ_szk2`, `rok_abs`,
#'   `edycja`, `kryterium` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select bind_rows setdiff
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_w1_parametry <- function(glowna_edycja) {
  roki_w1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  ramka_w1_ograniczone_szkoly <- expand_grid(
    wskaznik = "W1",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = TYPY_SZKOL_OGOL,
    rok_abs = roki_w1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    wynik = list(NULL)
  )

  ramka_w1_pozostale_szkoly <- expand_grid(
    wskaznik = "W1",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
    rok_abs = roki_w1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL)
  )

  ramka_w1 <- bind_rows(ramka_w1_ograniczone_szkoly, ramka_w1_pozostale_szkoly) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, wynik)

  return(ramka_w1)
}

#' @title Generowanie parametrów dla wskaźnika B1
#' @description Funkcja tworzy ramkę danych zawierającą unikalne kombinacje parametrów
#'   dla wskaźnika B1. Parametry te posłużą do późniejszego generowania rozkładów.
#'   Rok ukończenia szkoły (`rok_abs`) jest obliczany na podstawie głównej edycji
#'   (parametr `glowna_edycja`), przyjmując wartości `edycja - 1`, `edycja - 2` i `edycja - 5`.
#'   Typy szkół obejmują wszystkie dostępne typy z listy `TYPY_SZKOL`.
#'   Zmienna `kryterium` jest dobierana dynamicznie w zależności od typu szkoły.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "B1"), `WOJ_NAZWA`, `typ_szk2`, `rok_abs`,
#'   `edycja`, `kryterium` oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select bind_rows setdiff
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_b1_parametry <- function(glowna_edycja) {
  roki_b1 <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  ramka_b1_ograniczone_szkoly <- expand_grid(
    wskaznik = "B1",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = TYPY_SZKOL_OGOL,
    rok_abs = roki_b1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    wynik = list(NULL)
  )

  ramka_b1_pozostale_szkoly <- expand_grid(
    wskaznik = "B1",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = setdiff(TYPY_SZKOL, TYPY_SZKOL_OGOL),
    rok_abs = roki_b1,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_I_NAZWA,
    wynik = list(NULL)
  )

  ramka_b1 <- bind_rows(ramka_b1_ograniczone_szkoly, ramka_b1_pozostale_szkoly) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, wynik)

  return(ramka_b1)
}

#' @title Generowanie parametrów ogólnych dla wskaźników z typ_szk2 = NULL i
#' kryterium = sexf
#' @description Funkcja tworzy ramkę danych dla wskaźników, gdzie typ_szk2 jest
#' traktowany jako NULL (agregacja na poziomie województwa), a kryterium to
#' zawsze "sexf". Jest to użyteczne dla analiz ogólnych, niezależnych
#' od specyficznego typu szkoły.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (nazwa wskaźnika, np. "S7"), `WOJ_NAZWA`, `typ_szk2` (NULL), `rok_abs`,
#'   `edycja`, `kryterium` (z wartością "sexf") oraz `wynik` (inicjowany jako `NULL`).
#' @importFrom dplyr select
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_null_typszk2_sexf_parametry <- function(glowna_edycja) {
  roki_abs <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  # Wskaźniki, dla których chcemy agregacji na poziomie ogólnym (NULL typ_szk2)
  # i kryterium 'sexf'

  ramka_null_sexf <- expand_grid(
    wskaznik = "typ_szk2",
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = NA_character_, # Tutaj typ_szk2 jest NULL
    rok_abs = roki_abs,
    edycja = glowna_edycja,
    kryterium = KRYTERIUM_SEXY_TYLKO,
    wynik = list(NULL)
  ) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, wynik)

  return(ramka_null_sexf)
}

#' @title Generowanie parametrów ogólnych dla wskaźników z typ_szk2 = NULL
#' i kryterium = nazwa_zaw dla wybranej grupy szkół
#' @description Funkcja tworzy ramkę danych dla ogólnego wskaźnika obejmującego
#' specyficzną grupę szkół, gdzie typ_szk2 jest traktowany jako NULL
#' (agregacja na poziomie województwa dla tej grupy), a kryterium to
#' "nazwa_zaw". Jest to użyteczne dla analiz ogólnych w kontekście zawodów
#' dla określonej podgrupy szkół.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której generowane są parametry. Np. 2025.
#' @return Ramka danych typu `tibble` zawierająca kolumny:
#'   `wskaznik` (z wartością "meta_zaw"), `WOJ_NAZWA`, `typ_szk2` (NULL), `rok_abs`,
#'   `edycja`, `kryterium` (z wartością "nazwa_zaw") oraz `wynik` (inicjowany jako `NULL`).
#'   Kolumna `parametr_grupa_szkol` będzie zawierać listę typów szkół objętych tą agregacją.
#' @importFrom dplyr select mutate
#' @importFrom tidyr expand_grid
#' @export
generuj_ramke_null_typszk2_nazwazaw_parametry <- function(glowna_edycja) {
  roki_abs <- c(glowna_edycja - 1, glowna_edycja - 2, glowna_edycja - 5)

  ramka_null_nazwazaw <- expand_grid(
    wskaznik = "meta_zaw", # Nazwa wskaźnika dla tej specyficznej agregacji
    WOJ_NAZWA = WOJ_NAZWY,
    typ_szk2 = NA_character_, # Tutaj typ_szk2 jest NULL
    rok_abs = roki_abs,
    edycja = glowna_edycja,
    kryterium = "nazwa_zaw", # Zawsze nazwa_zaw dla tej tabeli
    wynik = list(NULL)
  ) %>%
    # Dodajemy kolumnę, która przechowuje listę typów szkół, które są agregowane
    mutate(parametr_grupa_szkol = list(TYPY_SZKOL_ZAW)) %>%
    select(wskaznik, WOJ_NAZWA, typ_szk2, rok_abs, edycja, kryterium, parametr_grupa_szkol, wynik)

  return(ramka_null_nazwazaw)
}


#' @title Łączenie ramek danych parametrów dla wszystkich wskaźników
#' @description Funkcja przyjmuje główną edycję projektu i generuje osobne ramki danych
#'   dla każdego wskaźnika (S7, D1, D2, K1, K2, W1, B1 oraz dla tabel meta)
#'   za pomocą dedykowanych funkcji.
#'   Następnie łączy te ramki w jedną spójną ramkę, która może służyć jako szablon
#'   do przechowywania parametrów i wyników rozkładów. Kolumny specyficzne dla
#'   wskaźników K1 i K2 (tj. `typ_k1_analizy`, `parametr_k1_wartosci`,
#'   `typ_k2_analizy`, `parametr_grupa_szkol`) #'   będą zawierały wartości `NA`
#'    dla pozostałych wskaźników, zapewniając spójność struktury.
#' @param glowna_edycja Liczba całkowita reprezentująca główną edycję (rok),
#'   dla której mają być generowane dane. Np. 2025.
#' @return Jedna duża ramka danych typu `tibble` zawierająca unikalne kombinacje
#'   parametrów dla wszystkich wskaźników i typów agregacji. Ramka będzie zawierać kolumny:
#'   `wskaznik`, `WOJ_NAZWA`, `typ_szk2`, `rok_abs`, `edycja`, `kryterium`,
#'   `typ_k1_analizy` (dla K1, `NA` dla innych), `parametr_k1_wartosci` (dla K1, `NA` dla innych),
#'   `typ_k2_analizy` (dla K2, `NA` dla innych), `parametr_grupa_szkol` (dla nowej agregacji, `NA` dla innych),
#'   oraz `wynik` (kolumna listowa z `NULL`).
#' @importFrom dplyr bind_rows
#' @export
polacz_ramki_wskaznikow <- function(glowna_edycja) {
  ramka_s7 <- generuj_ramke_s7_parametry(glowna_edycja)
  ramka_d1 <- generuj_ramke_d1_parametry(glowna_edycja)
  ramka_d2 <- generuj_ramke_d2_parametry(glowna_edycja)
  ramka_k1 <- generuj_ramke_k1_parametry(glowna_edycja)
  ramka_k2 <- generuj_ramke_k2_parametry(glowna_edycja)
  ramka_w1 <- generuj_ramke_w1_parametry(glowna_edycja)
  ramka_b1 <- generuj_ramke_b1_parametry(glowna_edycja)
  ramka_typszk_N <- generuj_ramke_null_typszk2_sexf_parametry(glowna_edycja)
  ramka_typszk_zaw <- generuj_ramke_null_typszk2_nazwazaw_parametry(glowna_edycja)

  finalna_ramka <- bind_rows(
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

  return(finalna_ramka)
}
