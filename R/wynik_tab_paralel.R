#' @title Wypełnia kolumnę 'wynik' tabeli parametrów wygenerowanymi tabelami krzyżowymi
#' @description Funkcja iteruje przez wiersze tabeli parametrów i dla każdej kombinacji
#'   parametru (wskaźnik, kryterium, filtry, itd.) filtruje odpowiednie dane źródłowe
#'   i wywołuje właściwą funkcję generującą tabelę krzyżową. Wynik tabeli krzyżowej
#'   jest zapisywany w kolumnie 'wynik' jako element listy.
#' @param param_df Ramka danych z parametrami wskaźników (np. wygenerowana przez `polacz_ramki_wskaznikow`).
#'   Powinna zawierać kolumny: `wskaznik`, `WOJ_NAZWA`, `typ_szk2`, `rok_abs`, `edycja`, `kryterium`,
#'   `rok`, `miesiac`, `typ_k1_analizy`, `parametr_K1`, `parametr_K2`, `parametr_grupa_szkol`.
#' @param p2_1 Dane źródłowe dla wskaźnika K2.
#' @param p3 Dane źródłowe dla wskaźników S7, K1, W1, B1.
#' @param p4 Dane źródłowe dla wskaźników D1, D2, typ_szk2, meta_zaw.
#' @return Zmodyfikowana ramka danych `param_df` z wypełnioną kolumną `wynik`,
#'   gdzie każda komórka zawiera tabelę krzyżową lub informację o błędzie/braku danych.
#' @importFrom dplyr %>% filter pull row_number mutate select bind_rows group_by group_split
#' @importFrom purrr map map_dfr
#' @importFrom rlang .data sym
#' @importFrom furrr future_map
#' @importFrom progressr progressor
#' @export
wypelnij_wyniki_wskaznikow_rownolegle <- function(param_df, p2_1, p3, p4) {

  # Mapowanie wskaźników do nazw zbiorów danych źródłowych
  wskaznik_to_data_map <- list(
    "S7" = "p3", "K1" = "p3", "W1" = "p3", "B1" = "p3",
    "D1" = "p4", "D2" = "p4", "typ_szk2" = "p4",
    "K2" = "p2_1", "meta_zaw" = "p4", "migracje" = "p3"
  )

  # Dynamiczne funkcje (na podstawie stringa) - małe, można eksportować
  funkcje_generujace_global <- list(
    tab_wier = tab_wier, tab_kolu = tab_kolu, tab_msc = tab_msc,
    tab_wier_K1 = tab_wier_K1, tab_kolu_K1 = tab_kolu_K1, tab_wier_K2 = tab_wier_K2,
    tab_wyna = tab_wyna, tab_sz_sex = tab_sz_sex, tab_sz_zaw = tab_sz_zaw,
    tab_freq = tab_freq
  )

  # Dodajemy tymczasowe ID wiersza, aby zachować oryginalną kolejność
  # i kolumnę wskazującą na używany zbiór danych źródłowych.
  param_df_indexed <- param_df %>%
    mutate(
      row_id = row_number(),
      # TUTAJ NASTĘPUJE ZMIANA: Używamy pluck() z wartością domyślną NA_character_
      # lub map_chr() z .default = NA_character_
      data_source_name = purrr::map_chr(wskaznik, ~purrr::pluck(wskaznik_to_data_map, .x, .default = NA_character_))
      # Alternatywnie: data_source_name = purrr::map_chr(wskaznik, ~wskaznik_to_data_map[[.x]] %||% NA_character_)
    )

  # Sprawdzenie, czy są jakieś wiersze, dla których nie znaleziono mapowania
  if (any(is.na(param_df_indexed$data_source_name))) {
    warning("Znaleziono wskaźniki w `param_df`, dla których nie ma mapowania w `wskaznik_to_data_map`. Te wiersze zostaną pominięte lub zwrócą błąd.")
  }

  # Grupowanie param_df najpierw po nazwie zbioru danych, a potem po innych filtrach
  # To jest kluczowe dla optymalizacji pamięci
  grouped_tasks <- param_df_indexed %>%
    group_by(data_source_name, WOJ_NAZWA, typ_szk2, rok_abs) %>%
    group_split()

  # Inicjalizacja paska postępu dla liczby grup do przetworzenia
  # p <- progressor(steps = length(grouped_tasks))

  # Funkcja pomocnicza do przetwarzania JEDNEJ GRUPY wierszy param_df
  # Worker otrzyma tylko ten zbiór danych, którego potrzebuje.
  process_group <- function(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref) {
    # browser()
    # p() # Zgłaszanie postępu

    # Pobieramy nazwę zbioru danych dla tej grupy (powinna być jedna)
    data_source_name_for_group <- group_df_chunk$data_source_name[1]

    # Wybieramy odpowiedni duży zbiór danych źródłowych
    dane_do_filtrowania_group <- switch(
      data_source_name_for_group,
      "p2_1" = all_p2_1,
      "p3" = all_p3,
      "p4" = all_p4,
      NULL
    )

    if (is.null(dane_do_filtrowania_group)) {
      # Jeśli dane źródłowe nie są zdefiniowane, zwracamy od razu wyniki dla tej grupy
      return(group_df_chunk %>% mutate(wynik = list(data.frame(Uwaga = "Brak zdefiniowanych danych źródłowych dla tej grupy."))))
  }

    # Wyciągamy wartości głównych filtrów z pierwszego wiersza grupy
    group_wskaznik <- group_df_chunk$wskaznik[1] # Dla switch wewnątrz
    group_WOJ_NAZWA <- group_df_chunk$WOJ_NAZWA[1]
    group_typ_szk2 <- group_df_chunk$typ_szk2[1]
    group_rok_abs <- group_df_chunk$rok_abs[1]

    # Zastosowanie głównych filtrów (WOJ_NAZWA, typ_szk2, rok_abs) do dużych danych
    # Odbywa się to raz na grupę, w obrębie worker'a, na jego kopii *jednego* dużego zbioru
    filtered_data_for_group <- dane_do_filtrowania_group

    if (!is.na(group_WOJ_NAZWA) && group_WOJ_NAZWA != "Polska") {
      filtered_data_for_group <- filtered_data_for_group %>% filter(.data$WOJ_NAZWA == group_WOJ_NAZWA)
    }

    if (!is.na(group_typ_szk2)) {
      if (group_typ_szk2 == "Branżowa szkoła I stopnia") {
        filtered_data_for_group <- filtered_data_for_group %>%
          filter(.data$typ_szk2 %in% c("Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia"))
      } else {
        filtered_data_for_group <- filtered_data_for_group %>% filter(.data$typ_szk2 == group_typ_szk2)
      }
    } else if (group_wskaznik == "Grupa_Szkol_Nazwa_Zaw" && !is.null(group_df_chunk$parametr_grupa_szkol[1])) {
      # Należy sprawdzić, czy 'parametr_grupa_szkol' jest listą list, jeśli tak, użyj unlist.
      # W przykładzie jest unlist, ale upewnij się, że zawsze jest to oczekiwany format.
      param_szkol_val <- if (is.list(group_df_chunk$parametr_grupa_szkol[1])) {
        unlist(group_df_chunk$parametr_grupa_szkol[1])
      } else {
        group_df_chunk$parametr_grupa_szkol[1]
      }
      dane_filtrowane <- filtered_data_for_group %>% filter(.data$typ_szk2 %in% param_szkol_val)
    }

    if (!is.na(group_rok_abs)) {
      filtered_data_for_group <- filtered_data_for_group %>% filter(.data$rok_abs == group_rok_abs)
    }

    # Iterujemy przez pojedyncze wiersze w obrębie tej odfiltrowanej grupy param_df

    results_for_group <- group_df_chunk %>%
      rowwise() %>% # Rowwise tutaj jest na małej ramce danych
      mutate(
        wynik = {
          current_wskaznik <- .data$wskaznik
          current_kryterium <- .data$kryterium
          current_rok <- .data$rok
          current_miesiac <- .data$miesiac
          current_edycja <- .data$edycja
          current_parametr_K1 <- .data$parametr_K1
          current_parametr_K2 <- .data$parametr_K2
          current_parametr_grupa_szkol <- .data$parametr_grupa_szkol
          current_row_id_in_group <- .data$row_id # Dodajemy row_id do komunikatu o błędzie

          # Dodatkowe filtry (rok, miesiac) na już wstępnie odfiltrowanych danych
          dane_dla_pojedynczego_wiersza <- filtered_data_for_group

          # Specjalna logika dla K2, która nie używa 'miesiac', ale 'okres_kont'
          if (current_wskaznik == "K2") {
            dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
              filter(.data$okres_kont == ((group_rok_abs * 12) + 12))
          }
          # Krok 1: Zastosuj filtr ROKU, jeśli jest dostępny
          if (!is.na(current_rok)) {
            # Standardowe filtrowanie po roku dla wszystkich innych wskaźników
              dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
                filter(.data$rok == current_rok)
          }

          # Krok 2: Zastosuj filtr MIESIĄCA na już przefiltrowanych danych (jeśli miesiąc jest dostępny)
          if (!is.na(current_miesiac)) {
            # Specjalna logika dla W1 (filtrowanie po 3 miesiącach)
            if (current_wskaznik == "W1") {
              mies_do_filtrowania <- unique(c(current_miesiac, current_miesiac - 1, current_miesiac - 2))
              mies_do_filtrowania <- mies_do_filtrowania[mies_do_filtrowania > 0]

              dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
                filter(.data$miesiac %in% mies_do_filtrowania)

            } else if (current_wskaznik != "K2") {
              # Standardowe filtrowanie po miesiącu dla wszystkich innych wskaźników
              # (z wyjątkiem K2, które nie używa tego filtra)
              dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
                filter(.data$miesiac == current_miesiac)
            }
          }

          # if (current_wskaznik %in% c("S7", "K1", "W1", "B1", "migracje") && !is.na(current_rok)) {
          #   if (is.na(current_miesiac)) {
          #     dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>% filter(.data$rok == current_rok)
          #   } else if (current_wskaznik == "K2") {
          #     dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
          #       filter(.data$rok == current_rok, .data$okres_kont == ((group_rok_abs * 12) + 12))
          #   } else {
          #   if (current_wskaznik == "W1") {
          #     mies_do_filtrowania <- unique(c(current_miesiac, current_miesiac - 1, current_miesiac - 2))
          #     mies_do_filtrowania <- mies_do_filtrowania[mies_do_filtrowania > 0]
          #     dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>% filter(.data$rok == current_rok, .data$miesiac %in% mies_do_filtrowania)
          #   } else {
          #     dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>% filter(.data$rok == current_rok, .data$miesiac == current_miesiac)
          #   }
          #   }
          # }


          # --- SPRAWDZENIE DANYCH WEJŚCIOWYCH ---
          if (nrow(dane_dla_pojedynczego_wiersza) == 0) {
            list(data.frame(Uwaga = paste0("Brak danych źródłowych po filtrowaniu dla wskaźnika: ",
                                                  current_wskaznik, ", kryterium: ", current_kryterium,
                                                  ", ID wiersza: ", current_row_id_in_group)))
          } else {

          # --- Wywołanie funkcji generującej tabelę krzyżową ---
          func_name <- switch(
            current_wskaznik,
            "S7" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw" || current_kryterium == "teryt_pow_szk") "tab_kolu"
              else if (current_kryterium == "mscrok") "tab_msc"
              else NULL
            },
            "D1" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu"
              else NULL
            },
            "D2" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu"
              else NULL
            },
            "K1" = {
              if (current_kryterium == "sexf") "tab_wier_K1"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu_K1"
              else NULL
            },
            "K2" = "tab_wier_K2",
            "W1" = "tab_wyna",
            "B1" = {
              if (current_kryterium == "sexf") "tab_wier"
              else if (current_kryterium == "nazwa_zaw") "tab_kolu"
              else if (current_kryterium == "mscrok") "tab_msc"
              else NULL
            },
            "typ_szk2" = "tab_sz_sex",
            "meta_zaw" = "tab_sz_zaw",
            "migracje" = "tab_freq",
            NULL
          )

          if (is.null(func_name)) {
            list(data.frame(Uwaga = paste0("Brak zdefiniowanej funkcji dla wskaźnika: ",
                                                  current_wskaznik, ", kryterium: ", current_kryterium,
                                                  ", ID wiersza: ", current_row_id_in_group)))
            } else {

          args <- list(dat = dane_dla_pojedynczego_wiersza)

          if (func_name %in% c("tab_wier", "tab_kolu", "tab_msc",
                               "tab_wyna", "tab_freq")) {
            if (func_name == "tab_msc") {
              args$wskaznik <- rlang::sym(current_wskaznik)
              args$kryterium <- rlang::sym(current_kryterium)
              args$edycja <- current_edycja
            } else if (func_name %in% c("tab_wyna", "tab_freq")) {
              args$kryterium <- rlang::sym(current_kryterium)
            } else { # tab_wier, tab_kolu
              args$wskaznik <- rlang::sym(current_wskaznik)
              args$kryterium <- rlang::sym(current_kryterium)
            }
          } else if (func_name %in% c("tab_wier_K1", "tab_kolu_K1")) {
            args$wskazniki <- unlist(current_parametr_K1)
            args$kryterium <- rlang::sym(current_kryterium)
          } else if (func_name == "tab_wier_K2") {
            args$wskaznik <- rlang::sym(current_parametr_K2)
            args$kryterium <- rlang::sym(current_kryterium)
          } else if (func_name %in% c("tab_sz_sex", "tab_sz_zaw")) {
            # Te funkcje przyjmują tylko 'dat'
          }

          tryCatch({
            result <- do.call(funcs_gen_ref[[func_name]], args)
            if (!inherits(result, "data.frame")) { # Upewnij się, że wynik jest ramką danych
              list(data.frame(Uwaga = paste0("Funkcja '", func_name, "' nie zwróciła ramki danych dla ID wiersza: ",
                                             current_row_id_in_group)))
            } else {
            list(result) # Zawsze zwracaj listę z jednym elementem (ramką danych)
            }
          }, error = function(e) {
            list(data.frame(Uwaga = paste0("Błąd podczas generowania tabeli krzyżowej (", func_name, ") dla wiersza ID: ",
                                           current_row_id_in_group, ", wskaźnik: ", current_wskaznik,
                                           ", kryterium: ", current_kryterium, ", błąd: ", e$message)))
          })
            }
          }
        }
      ) %>%
      ungroup()
    # DODAJĘ TE LINIE DLA DEBUGOWANIA:
    # message("DEBUG: Wynik grupy przed zwróceniem:")
    # print(head(results_for_group))
    # print(str(results_for_group$wynik)) # KLUCZOWE! Sprawdź strukturę kolumny 'wynik'
    # browser() # <-- Włącz to, jeśli chcesz zatrzymać się tutaj dla KAŻDEJ grupy

    return(results_for_group)
}

  # Przeprowadzamy obliczenia równolegle na GRUPACH
  # .options = furrr_options(globals = ...) jest kluczowe!
  # P2_1, p3, p4 są przekazywane jako argumenty do future_map
  final_param_df_results <- furrr::future_map_dfr(
    .x = grouped_tasks,
    .f = function(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref) {
      process_group(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref)
    },
    all_p2_1 = p2_1, # przekazujemy p2_1 jako argument
    all_p3 = p3,     # przekazujemy p3 jako argument
    all_p4 = p4,     # przekazujemy p4 jako argument
    funcs_gen_ref = funkcje_generujace_global, # przekazujemy funkcje
    .options = furrr_options(
      globals = c("process_group", "wskaznik_to_data_map", # Dodajemy wskaznik_to_data_map do globals, jeśli jest używane w proces_group
                  # Pakiety, które mogą być potrzebne w workerach
                  "dplyr", "purrr", "rlang", "future", "furrr", "progressr",
                  # Twoje funkcje pomocnicze
                  "tab_wier", "tab_kolu", "tab_msc", "tab_wier_K1", "tab_kolu_K1",
                  "tab_wier_K2", "tab_wyna", "tab_sz_sex", "tab_sz_zaw", "tab_freq"
      ),
      packages = c("dplyr", "purrr", "rlang", "future", "furrr", "progressr")
    )
  ) %>%
    # Sortujemy wyniki z powrotem do oryginalnej kolejności
    arrange(.data$row_id) %>%
    select(-.data$row_id, -.data$data_source_name) # Usuwamy pomocnicze kolumny

  return(final_param_df_results)
}





#' @title Wypełnia kolumnę 'wynik' tabeli parametrów wygenerowanymi tabelami krzyżowymi
#' @description Funkcja iteruje przez wiersze tabeli parametrów i dla każdej kombinacji
#'   parametru (wskaźnik, kryterium, filtry, itd.) filtruje odpowiednie dane źródłowe
#'   i wywołuje właściwą funkcję generującą tabelę krzyżową. Wynik tabeli krzyżowej
#'   jest zapisywany w kolumnie 'wynik' jako element listy.
#' @param param_df Ramka danych z parametrami wskaźników (np. wygenerowana przez `polacz_ramki_wskaznikow`).
#'   Powinna zawierać kolumny: `wskaznik`, `branza`, `typ_szk2`, `rok_abs`, `edycja`, `kryterium`,
#'   `rok`, `miesiac`, `typ_k1_analizy`, `parametr_K1`, `parametr_K2`, `parametr_grupa_szkol`.
#' @param p2_1 Dane źródłowe dla wskaźnika K2.
#' @param p3 Dane źródłowe dla wskaźników S7, K1, W1, B1.
#' @param p4 Dane źródłowe dla wskaźników D1, D2, typ_szk2, meta_zaw.
#' @return Zmodyfikowana ramka danych `param_df` z wypełnioną kolumną `wynik`,
#'   gdzie każda komórka zawiera tabelę krzyżową lub informację o błędzie/braku danych.
#' @importFrom dplyr %>% filter pull row_number mutate select bind_rows group_by group_split
#' @importFrom purrr map map_dfr
#' @importFrom rlang .data sym
#' @importFrom furrr future_map
#' @importFrom progressr progressor
#' @export
wypelnij_wyniki_branzowe_rownolegle <- function(param_df, p2_1, p3, p4) {

  # Mapowanie wskaźników do nazw zbiorów danych źródłowych
  wskaznik_to_data_map <- list(
    "S7" = "p3", "K1" = "p3", "W1" = "p3", "B1" = "p3",
    "D1" = "p4", "D2" = "p4", "typ_szk2" = "p4",
    "K2" = "p2_1", "meta_zaw" = "p4"
  )

  # Dynamiczne funkcje (na podstawie stringa) - małe, można eksportować
  funkcje_generujace_global <- list(
    tab_wier = tab_wier, tab_kolu = tab_kolu, tab_msc = tab_msc,
    tab_wier_K1 = tab_wier_K1, tab_kolu_K1 = tab_kolu_K1, tab_wier_K2 = tab_wier_K2,
    tab_wyna = tab_wyna, tab_sz_sex = tab_sz_sex, tab_sz_zaw = tab_sz_zaw
  )

  # Dodajemy tymczasowe ID wiersza, aby zachować oryginalną kolejność
  # i kolumnę wskazującą na używany zbiór danych źródłowych.
  param_df_indexed <- param_df %>%
    mutate(
      row_id = row_number(),
      # TUTAJ NASTĘPUJE ZMIANA: Używamy pluck() z wartością domyślną NA_character_
      # lub map_chr() z .default = NA_character_
      data_source_name = purrr::map_chr(wskaznik, ~purrr::pluck(wskaznik_to_data_map, .x, .default = NA_character_))
      # Alternatywnie: data_source_name = purrr::map_chr(wskaznik, ~wskaznik_to_data_map[[.x]] %||% NA_character_)
    )

  # Sprawdzenie, czy są jakieś wiersze, dla których nie znaleziono mapowania
  if (any(is.na(param_df_indexed$data_source_name))) {
    warning("Znaleziono wskaźniki w `param_df`, dla których nie ma mapowania w `wskaznik_to_data_map`. Te wiersze zostaną pominięte lub zwrócą błąd.")
  }

  # Grupowanie param_df najpierw po nazwie zbioru danych, a potem po innych filtrach
  # To jest kluczowe dla optymalizacji pamięci
  grouped_tasks <- param_df_indexed %>%
    group_by(data_source_name, branza, typ_szk2, rok_abs) %>%
    group_split()

  # Inicjalizacja paska postępu dla liczby grup do przetworzenia
  # p <- progressor(steps = length(grouped_tasks))

  # Funkcja pomocnicza do przetwarzania JEDNEJ GRUPY wierszy param_df
  # Worker otrzyma tylko ten zbiór danych, którego potrzebuje.
  process_group <- function(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref) {
    # browser()
    # p() # Zgłaszanie postępu

    # Pobieramy nazwę zbioru danych dla tej grupy (powinna być jedna)
    data_source_name_for_group <- group_df_chunk$data_source_name[1]

    # Wybieramy odpowiedni duży zbiór danych źródłowych
    dane_do_filtrowania_group <- switch(
      data_source_name_for_group,
      "p2_1" = all_p2_1,
      "p3" = all_p3,
      "p4" = all_p4,
      NULL
    )

    if (is.null(dane_do_filtrowania_group)) {
      # Jeśli dane źródłowe nie są zdefiniowane, zwracamy od razu wyniki dla tej grupy
      return(group_df_chunk %>% mutate(wynik = list(data.frame(Uwaga = "Brak zdefiniowanych danych źródłowych dla tej grupy."))))
    }

    # Wyciągamy wartości głównych filtrów z pierwszego wiersza grupy
    group_wskaznik <- group_df_chunk$wskaznik[1] # Dla switch wewnątrz
    group_branza <- group_df_chunk$branza[1]
    group_typ_szk2 <- group_df_chunk$typ_szk2[1]
    group_rok_abs <- group_df_chunk$rok_abs[1]

    # Zastosowanie głównych filtrów (branza, typ_szk2, rok_abs) do dużych danych
    # Odbywa się to raz na grupę, w obrębie worker'a, na jego kopii *jednego* dużego zbioru
    filtered_data_for_group <- dane_do_filtrowania_group

    if (!is.na(group_branza)) {
      filtered_data_for_group <- filtered_data_for_group %>% filter(.data$branza == group_branza)
    }

    if (!is.na(group_typ_szk2)) {
      if (group_typ_szk2 == "Branżowa szkoła I stopnia") {
        filtered_data_for_group <- filtered_data_for_group %>%
          filter(.data$typ_szk2 %in% c("Młodociani w Branżowej szkole I stopnia", "Niemłodociani w Branżowej szkole I stopnia"))
      } else {
        filtered_data_for_group <- filtered_data_for_group %>% filter(.data$typ_szk2 == group_typ_szk2)
      }
    } else if (group_wskaznik == "Grupa_Szkol_Nazwa_Zaw" && !is.null(group_df_chunk$parametr_grupa_szkol[1])) {
      # Należy sprawdzić, czy 'parametr_grupa_szkol' jest listą list, jeśli tak, użyj unlist.
      # W przykładzie jest unlist, ale upewnij się, że zawsze jest to oczekiwany format.
      param_szkol_val <- if (is.list(group_df_chunk$parametr_grupa_szkol[1])) {
        unlist(group_df_chunk$parametr_grupa_szkol[1])
      } else {
        group_df_chunk$parametr_grupa_szkol[1]
      }
      dane_filtrowane <- filtered_data_for_group %>% filter(.data$typ_szk2 %in% param_szkol_val)
    }

    if (!is.na(group_rok_abs)) {
      filtered_data_for_group <- filtered_data_for_group %>% filter(.data$rok_abs == group_rok_abs)
    }

    # Iterujemy przez pojedyncze wiersze w obrębie tej odfiltrowanej grupy param_df

    results_for_group <- group_df_chunk %>%
      rowwise() %>% # Rowwise tutaj jest na małej ramce danych
      mutate(
        wynik = {
          current_wskaznik <- .data$wskaznik
          current_kryterium <- .data$kryterium
          current_rok <- .data$rok
          current_miesiac <- .data$miesiac
          current_edycja <- .data$edycja
          current_parametr_K1 <- .data$parametr_K1
          current_parametr_K2 <- .data$parametr_K2
          current_parametr_grupa_szkol <- .data$parametr_grupa_szkol
          current_row_id_in_group <- .data$row_id # Dodajemy row_id do komunikatu o błędzie

          # Dodatkowe filtry (rok, miesiac) na już wstępnie odfiltrowanych danych
          dane_dla_pojedynczego_wiersza <- filtered_data_for_group

          # Specjalna logika dla K2, która nie używa 'miesiac', ale 'okres_kont'
          if (current_wskaznik == "K2") {
            dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
              filter(.data$okres_kont == ((group_rok_abs * 12) + 12))
          }
          # Krok 1: Zastosuj filtr ROKU, jeśli jest dostępny
          if (!is.na(current_rok)) {
            # Standardowe filtrowanie po roku dla wszystkich innych wskaźników
            dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
              filter(.data$rok == current_rok)
          }

          # Krok 2: Zastosuj filtr MIESIĄCA na już przefiltrowanych danych (jeśli miesiąc jest dostępny)
          if (!is.na(current_miesiac)) {
            # Specjalna logika dla W1 (filtrowanie po 3 miesiącach)
            if (current_wskaznik == "W1") {
              mies_do_filtrowania <- unique(c(current_miesiac, current_miesiac - 1, current_miesiac - 2))
              mies_do_filtrowania <- mies_do_filtrowania[mies_do_filtrowania > 0]

              dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
                filter(.data$miesiac %in% mies_do_filtrowania)

            } else if (current_wskaznik != "K2") {
              # Standardowe filtrowanie po miesiącu dla wszystkich innych wskaźników
              # (z wyjątkiem K2, które nie używa tego filtra)
              dane_dla_pojedynczego_wiersza <- dane_dla_pojedynczego_wiersza %>%
                filter(.data$miesiac == current_miesiac)
            }
          }



          # --- SPRAWDZENIE DANYCH WEJŚCIOWYCH ---
          if (nrow(dane_dla_pojedynczego_wiersza) == 0) {
            list(data.frame(Uwaga = paste0("Brak danych źródłowych po filtrowaniu dla wskaźnika: ",
                                           current_wskaznik, ", kryterium: ", current_kryterium,
                                           ", ID wiersza: ", current_row_id_in_group)))
          } else {

            # --- Wywołanie funkcji generującej tabelę krzyżową ---
            func_name <- switch(
              current_wskaznik,
              "S7" = {
                if (current_kryterium == "sexf") "tab_wier"
                else if (current_kryterium == "nazwa_zaw" || current_kryterium == "teryt_pow_szk") "tab_kolu"
                else if (current_kryterium == "mscrok") "tab_msc"
                else NULL
              },
              "D1" = {
                if (current_kryterium == "sexf") "tab_wier"
                else if (current_kryterium == "nazwa_zaw") "tab_kolu"
                else NULL
              },
              "D2" = {
                if (current_kryterium == "sexf") "tab_wier"
                else if (current_kryterium == "nazwa_zaw") "tab_kolu"
                else NULL
              },
              "K1" = {
                if (current_kryterium == "sexf") "tab_wier_K1"
                else if (current_kryterium == "nazwa_zaw") "tab_kolu_K1"
                else NULL
              },
              "K2" = "tab_wier_K2",
              "W1" = "tab_wyna",
              "B1" = {
                if (current_kryterium == "sexf") "tab_wier"
                else if (current_kryterium == "nazwa_zaw") "tab_kolu"
                else if (current_kryterium == "mscrok") "tab_msc"
                else NULL
              },
              "typ_szk2" = "tab_sz_sex",
              "meta_zaw" = "tab_sz_zaw",
              NULL
            )

            if (is.null(func_name)) {
              list(data.frame(Uwaga = paste0("Brak zdefiniowanej funkcji dla wskaźnika: ",
                                             current_wskaznik, ", kryterium: ", current_kryterium,
                                             ", ID wiersza: ", current_row_id_in_group)))
            } else {

              args <- list(dat = dane_dla_pojedynczego_wiersza)

              if (func_name %in% c("tab_wier", "tab_kolu", "tab_msc",
                                   "tab_wyna")) {
                if (func_name == "tab_msc") {
                  args$wskaznik <- rlang::sym(current_wskaznik)
                  args$kryterium <- rlang::sym(current_kryterium)
                  args$edycja <- current_edycja
                } else if (func_name %in% c("tab_wyna")) {
                  args$kryterium <- rlang::sym(current_kryterium)
                } else { # tab_wier, tab_kolu
                  args$wskaznik <- rlang::sym(current_wskaznik)
                  args$kryterium <- rlang::sym(current_kryterium)
                }
              } else if (func_name %in% c("tab_wier_K1", "tab_kolu_K1")) {
                args$wskazniki <- unlist(current_parametr_K1)
                args$kryterium <- rlang::sym(current_kryterium)
              } else if (func_name == "tab_wier_K2") {
                args$wskaznik <- rlang::sym(current_parametr_K2)
                args$kryterium <- rlang::sym(current_kryterium)
              } else if (func_name %in% c("tab_sz_sex", "tab_sz_zaw")) {
                # Te funkcje przyjmują tylko 'dat'
              }

              tryCatch({
                result <- do.call(funcs_gen_ref[[func_name]], args)
                if (!inherits(result, "data.frame")) { # Upewnij się, że wynik jest ramką danych
                  list(data.frame(Uwaga = paste0("Funkcja '", func_name, "' nie zwróciła ramki danych dla ID wiersza: ",
                                                 current_row_id_in_group)))
                } else {
                  list(result) # Zawsze zwracaj listę z jednym elementem (ramką danych)
                }
              }, error = function(e) {
                list(data.frame(Uwaga = paste0("Błąd podczas generowania tabeli krzyżowej (", func_name, ") dla wiersza ID: ",
                                               current_row_id_in_group, ", wskaźnik: ", current_wskaznik,
                                               ", kryterium: ", current_kryterium, ", błąd: ", e$message)))
              })
            }
          }
        }
      ) %>%
      ungroup()
    # DODAJĘ TE LINIE DLA DEBUGOWANIA:
    # message("DEBUG: Wynik grupy przed zwróceniem:")
    # print(head(results_for_group))
    # print(str(results_for_group$wynik)) # KLUCZOWE! Sprawdź strukturę kolumny 'wynik'
    # browser() # <-- Włącz to, jeśli chcesz zatrzymać się tutaj dla KAŻDEJ grupy

    return(results_for_group)
  }

  # Przeprowadzamy obliczenia równolegle na GRUPACH
  # .options = furrr_options(globals = ...) jest kluczowe!
  # P2_1, p3, p4 są przekazywane jako argumenty do future_map
  final_param_df_results <- furrr::future_map_dfr(
    .x = grouped_tasks,
    .f = function(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref) {
      process_group(group_df_chunk, all_p2_1, all_p3, all_p4, funcs_gen_ref)
    },
    all_p2_1 = p2_1, # przekazujemy p2_1 jako argument
    all_p3 = p3,     # przekazujemy p3 jako argument
    all_p4 = p4,     # przekazujemy p4 jako argument
    funcs_gen_ref = funkcje_generujace_global, # przekazujemy funkcje
    .options = furrr_options(
      globals = c("process_group", "wskaznik_to_data_map", # Dodajemy wskaznik_to_data_map do globals, jeśli jest używane w proces_group
                  # Pakiety, które mogą być potrzebne w workerach
                  "dplyr", "purrr", "rlang", "future", "furrr", "progressr",
                  # Twoje funkcje pomocnicze
                  "tab_wier", "tab_kolu", "tab_msc", "tab_wier_K1", "tab_kolu_K1",
                  "tab_wier_K2", "tab_wyna", "tab_sz_sex", "tab_sz_zaw", "tab_freq"
      ),
      packages = c("dplyr", "purrr", "rlang", "future", "furrr", "progressr")
    )
  ) %>%
    # Sortujemy wyniki z powrotem do oryginalnej kolejności
    arrange(.data$row_id) %>%
    select(-.data$row_id, -.data$data_source_name) # Usuwamy pomocnicze kolumny

  return(final_param_df_results)
}