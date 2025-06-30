#' @title funkcja tworzaca tabele krzyzowa, w ktorej wskaznik jest w wierszach
#' @import daneIBE
#' @importFrom dplyr  %>% n_distinct
#' @param dat dane na podstawie ktorych ma powstac tabela
#' @param wskaznik wskaznik, z którego ma powstać tabela
#' @param kryterium zmienna przez która ma być skrosowany wskaznik
#' @export
tab_wier <- function(dat, wskaznik, kryterium) {
  if (n_distinct(dat$id_abs) >= 10) {
    dat_sex = dat %>%
      daneIBE::tab2({{wskaznik}},  {{kryterium}}, "k") %>%
      as.data.frame()
  } else {
    dat_sex = data.frame(Uwaga = "Nie można pokazać wyników - zbyt mała liczba obserwacji (n<10)")
  }
  return(dat_sex)
}
#' @title funkcja tworzaca tabele krzyzowa, w ktorej wskaznik jest w kolumnach
#' @import daneIBE
#' @importFrom dplyr  %>% n_distinct distinct
#' @param dat dane na podstawie ktorych ma powstac tabela
#' @param wskaznik wskaznik, z którego ma powstać tabela
#' @param kryterium zmienna przez która ma być skrosowany wskaznik
#' @export
tab_kolu <- function(dat, wskaznik, kryterium) {
  if (n_distinct(dat$id_abs) >= 10) {
    dat_zaw = dat %>%
      daneIBE::tab2({{kryterium}}, {{wskaznik}}, "w") %>%
      arrange(desc(n_SUMA)) %>% as.data.frame()
  } else {
    dat_zaw = data.frame(Uwaga = "Nie można pokazać wyników - zbyt mała liczba obserwacji (n<10)")
  }
  return(dat_zaw)
}
#' @title funkcja tworzaca tabele krzyzowa, w ktorej wskaznik jest w wierszach a
#' w kolumnach jest zmienna czasu "mscrok"
#' @import daneIBE
#' @importFrom dplyr  %>% n_distinct if_else
#' @param dat dane na podstawie ktorych ma powstac tabela
#' @param wskaznik wskaznik, z którego ma powstać tabela
#' @param kryterium zmienna przez która ma być skrosowany wskaznik
#' @param edycja parametr przekazujący dla której edycji monitoringu ma być
#' tworzona tabela; druga zmienna, ktora umożliwia utworzenie tabeli "rok_abs"
#' w przekazanych danych moze posiadac tylko jedną wartosc - musi byc
#' odfiltrowana wczesniej
#' @export
tab_msc <- function(dat, wskaznik, kryterium, edycja) {
  if (n_distinct(dat$id_abs) >= 10) {
    rok_a <- dat %>% select(rok_abs) %>% distinct()

    if (edycja - rok_a == 1) {
      kol_1 <- c(as.character(substitute(wskaznik)),paste0("pct_7.",edycja-1),
                 paste0("pct_8.",edycja-1),paste0("pct_9.",edycja-1),
                 paste0("pct_10.",edycja-1),paste0("pct_11.",edycja-1),
                 paste0("pct_12.",edycja-1),paste0("pct_1.",edycja),
                 paste0("pct_2.",edycja),paste0("pct_3.",edycja))
    } else if (edycja - rok_a == 2) {
      kol_1 <- c(as.character(substitute(wskaznik)),
                 paste0("pct_7.",edycja-2),paste0("pct_8.",edycja-2),
                 paste0("pct_9.",edycja-2),paste0("pct_10.",edycja-2),
                 paste0("pct_11.",edycja-2),paste0("pct_12.",edycja-2),
                 paste0("pct_1.",edycja-1),paste0("pct_2.",edycja-1),
                 paste0("pct_3.",edycja-1),paste0("pct_4.",edycja-1),
                 paste0("pct_5.",edycja-1),paste0("pct_6.",edycja-1),
                 paste0("pct_7.",edycja-1),paste0("pct_8.",edycja-1),
                 paste0("pct_9.",edycja-1),paste0("pct_10.",edycja-1),
                 paste0("pct_11.",edycja-1),paste0("pct_12.",edycja-1),
                 paste0("pct_1.",edycja),paste0("pct_2.",edycja),
                 paste0("pct_3.",edycja))
    } else if (edycja - rok_a == 5) {
      kol_1 <- c(as.character(substitute(wskaznik)),
                 paste0("pct_9.",edycja-5),paste0("pct_12.",edycja-5),
                 paste0("pct_3.",edycja-4),paste0("pct_6.",edycja-4),
                 paste0("pct_9.",edycja-4),paste0("pct_12.",edycja-4),
                 paste0("pct_3.",edycja-3),paste0("pct_6.",edycja-3),
                 paste0("pct_9.",edycja-3),paste0("pct_12.",edycja-3),
                 paste0("pct_3.",edycja-2),paste0("pct_6.",edycja-2),
                 paste0("pct_9.",edycja-2),paste0("pct_12.",edycja-2),
                 paste0("pct_1.",edycja-1),paste0("pct_3.",edycja-1),
                 paste0("pct_9.",edycja-1),paste0("pct_12.",edycja-1),
                 paste0("pct_3.",edycja))
    }

    dat_msc = dat %>%
      daneIBE::tab2({{wskaznik}},  {{kryterium}}, "k", liczby = FALSE,
                    usunSuma = TRUE, usunOgolem = TRUE) %>%
      as.data.frame()
    dat_msc = dat_msc %>% select(all_of(kol_1))
  } else {
    dat_msc = data.frame(Uwaga = "Nie można pokazać wyników - zbyt mała liczba obserwacji (n<10)")
  }
  return(dat_msc)
}

#' @title Tabela z kontynuowaniem nauki w różnych formach, ktore musza byc
#' przekazane w liscie wskaznikow, ulozonych w wierszach w podziale na
#' kryterium w kolumnach (pierwotnie przygotowane pod plec)
#' @description Zastosowanie funkcji  pozwala uzyskac tabele,
#' w ktorej przechowywana jest informacja o liczbie i procencie kobiet, mezczyzn
#' i ogolem wszystkich absolwentow kontynuujacych naukę na studiach, w szkole
#' policealnej lub na KKZ
#' @import daneIBE
#' @importFrom purrr map
#' @importFrom stringr str_detect str_replace
#' @importFrom dplyr  %>% n_distinct if_else rename_with starts_with bind_rows
#'   mutate_at
#' @param dat dane na podstawie ktorych ma powstac tabela - dane te są
#' przekazywane do funkcji tab_wier
#' @param wskazniki lista wskaznikow, z których ma powstać tabela wynikowa ze
#' sposobami kontynuowania nauki - kazdy wskaznik jest osobno przekazywany do
#' funkcji tab_wier
#' @param kryterium zmienna przez która ma być skrosowany wskaznik - zmienna ta
#' jest przekazywana do funkcji tab_wier
#' @export
tab_wier_K1 <- function(dat, wskazniki, kryterium) {
  wsk <- dat %>%
    slice(0) %>%
    select({{wskazniki}}) %>%
    names()
  dat_wier_K1 <- vector(mode = "list", length = length(wsk))
  names(dat_wier_K1) <- wsk
  for (k in wsk) {
    wskaznik <- as.name(k)
    dat_wier_K1[[k]] <- tab_wier(dat, {{wskaznik}}, {{kryterium}})
  }
  dat_wier_K1 <- map(dat_wier_K1, ~ rename_with(.x, ~ "Kontynuacja nauki",
                                              starts_with("nauka"))) %>%
    bind_rows() %>% filter(str_detect(`Kontynuacja nauki`,'Kontynuacja')) %>%
    mutate_at("Kontynuacja nauki", str_replace, "Kontynuacja nauki ", "")
  if (nrow(dat_wier_K1) == 0) {
    dat_wier_K1 = data.frame(Uwaga = "Nie można pokazać wyników - zbyt mała liczba obserwacji (n<10)")
  }
  return(dat_wier_K1)
}

#' @title Tabela z kontynuowaniem nauki w różnych formach, ktore musza byc
#' przekazane w liscie wskaznikow, ulozonych w kolumnach w podziale na
#' kryterium w wierszach (pierwotnie przygotowane pod zawody)
#' @description Zastosowanie funkcji  pozwala uzyskac tabele,
#' w ktorej przechowywana jest informacja o liczbie i procencie kobiet, mezczyzn
#' i ogolem wszystkich absolwentow kontynuujacych naukę na studiach, w szkole
#' policealnej lub na KKZ
#' @import daneIBE
#' @importFrom purrr map
#' @importFrom stringr str_detect str_replace
#' @importFrom dplyr  %>% n_distinct if_else rename_with slice bind_cols
#'   mutate_at select select_if matches arrange
#' @param dat dane na podstawie ktorych ma powstac tabela - dane te są
#' przekazywane do funkcji tab_wier
#' @param wskazniki lista wskaznikow, z których ma powstać tabela wynikowa ze
#' sposobami kontynuowania nauki - kazdy wskaznik jest osobno przekazywany do
#' funkcji tab_wier
#' @param kryterium zmienna przez która ma być skrosowany wskaznik - zmienna ta
#' jest przekazywana do funkcji tab_wier
#' @export
tab_kolu_K1 <- function(dat, wskazniki, kryterium) {
  wsk <- dat %>%
    slice(0) %>%
    select({{wskazniki}}) %>%
    names()
  dat_kolu_K1 <- vector(mode = "list", length = length(wsk))
  names(dat_kolu_K1) <- wsk
  for (k in wsk) {
    wskaznik <- as.name(k)
    dat_kolu_K1[[k]] <- tab_kolu(dat, {{wskaznik}}, {{kryterium}}) %>%
      arrange(desc(n_SUMA)) %>% as.data.frame() %>% slice(2:11)
  }
  dat_kolu_K1 <- dat_kolu_K1 %>%
    bind_cols() %>%
    select("nazwa_zaw...1", "n_SUMA...4",
           matches("Kontynuacja")) %>%
    select_if(~ !is.numeric(.) || sum(.) != 0)
  if (ncol(dat_kolu_K1) <= 2) {
    dat_kolu_K1 = data.frame(Uwaga = "Nie można pokazać wyników - zbyt mała liczba obserwacji (n<10)")
  }
  return(dat_kolu_K1)
}

#' @title Tabela z kontynuowaniem nauki w różnych dziedzinach lub dyscyplinach,
#' ulozonych w kolumnach w podziale na kryterium
#' (z zalozenia plec lub nazwa_zaw) w wierszach
#' @description Zastosowanie funkcji  pozwala uzyskac tabele,
#' w ktorej przechowywana jest informacja o liczbie i procencie ze względu
#' na płeć lub wyuczony zawód oraz i ogółem wszystkich absolwentóww
#' kontynuujacych naukę na studiach w różnych dziedzinach/dyscyplinach
#' @import daneIBE
#' @importFrom rlang as_name
#' @importFrom stringr str_detect str_replace
#' @importFrom dplyr  %>% n_distinct if_else rename_with slice bind_cols
#'   mutate_at select select_if matches arrange
#' @param dat dane bedące odfiltrowanym w funkcjach nadrzednych fragmentem
#' tabeli p2_1 przygotowanej na potrzeby raportow z tabeli p2
#' @param wskaznik wskaznik może przybierać tylko dwie wartości dziedziny lub
#' dyscypliny
#' @param kryterium zmienna przez która ma być skrosowany wskaznik -
#' może przybierać dwie wartości plec lub nazwa_zaw
#' @export
tab_wier_K2 <- function(dat, wskaznik, kryterium) {
  if (n_distinct(dat$id_abs) >= 10) {
    wsk_q   <- enquo(wskaznik) %>% as_name()
    kry_q   <- enquo(kryterium) %>% as_name()
    if (wsk_q == "dziedziny") {
      selected_cols <- grep("^(nauk |sztuki$)", names(dat), value = TRUE)
    } else if (wsk_q == "dyscypliny"){
      selected_cols <- grep(
        "^(?!(?:nauk |sztuki$ |plec$))[[:lower:],ąćęłńóśźż ]+$",
        names(dat), value = TRUE, perl  = TRUE)
    }
    df <- dat %>% tab({{kryterium}}) %>%
      as.data.frame()  %>%
      mutate(wartość = if_else(wartość == "ŁĄCZNIE", "OGÓŁEM", wartość))
    colnames(df) <- c(kry_q, "liczba", "procent")
    for (m in selected_cols) {
      type <- ensym(m)
      temp <- dat %>% tab2({{kryterium}}, !!type, sumowanie = "w") %>%
        as.data.frame()
      if (ncol(temp)>5) {
        temp <- temp %>% select({{kryterium}}, n_1, pct_1)
        colnames(temp) <- c(kry_q, paste0("liczba_",type),
                            paste0("procent_",type))
        df <- df %>% left_join(temp, by = kry_q)}
    }
    tab_wier_K2 <- df
  } else {
    tab_wier_K2 = data.frame(Uwaga = "Nie można pokazać wyników - zbyt mała liczba obserwacji (n<10)")
  }
  return(tab_wier_K2)
}

#' @title funkcja tworzaca tabele ze statystykami wynagrodzen w poidziale na
#' kryterium (płeć lub zawód)
#' @import daneIBE
#' @importFrom dplyr  %>% n_distinct
#' @param dat dane na podstawie ktorych ma powstac tabela bedące podzbiorem
#' tabeli p3wyodrębnionym ze względu na typ szkoły, (ewentualnie) województwo,
#' rok absolwenta oraz okres za który mają być uśrednione zarobki
#' @param kryterium zmienna przez która ma być skrosowany wskaznik domyslnie
#' płeć lub nazwa zawodu
#' @export
tab_wyna <- function(dat, kryterium) {
  if (n_distinct(dat$id_abs) >= 10) {
    dat_wyna = dat %>%
      group_by(id_abs, {{kryterium}}) %>%
      summarize(wynagrodzenie = mean(wynagrodzenie, na.rm = TRUE)) %>%
      group_by({{kryterium}}) %>%
      summarize(n=n(),
                sre=mean(wynagrodzenie, na.rm = TRUE),
                q5=quantile(wynagrodzenie, probs=0.05, na.rm = TRUE),
                q25=quantile(wynagrodzenie, probs=0.25, na.rm = TRUE),
                med=median(wynagrodzenie, na.rm = TRUE),
                q75=quantile(wynagrodzenie, probs=0.75, na.rm = TRUE),
                q95=quantile(wynagrodzenie, probs=0.95, na.rm = TRUE)) %>%
      arrange(desc(n)) %>% filter(n>10)
  } else {
    dat_wyna = data.frame(Uwaga = "Nie można pokazać wyników - zbyt mała liczba obserwacji (n<10)")
  }
  return(dat_wyna)
}


