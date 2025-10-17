# Funkcja pomocnicza do ustawiania nazw kolumn i wartości
ustaw_grupe_parametrów <- function(typ) {
  if (!(typ %in% c("woj", "bran"))) {
    stop("Parametr 'typ' musi przyjmować jedną z wartości: 'woj' lub 'bran'.")
  }

  if (typ == "woj") {
    nazwa_kolumny <- "WOJ_NAZWA"
    wartosci_kolumny <- WOJ_NAZWY # Zakładamy, że WOJ_NAZWY jest zdefiniowane
  } else { # typ == "bran"
    nazwa_kolumny <- "branza"
    wartosci_kolumny <- branza # Zakładamy, że branza jest zdefiniowana
  }

  return(list(nazwa = nazwa_kolumny, wartosci = wartosci_kolumny))
}