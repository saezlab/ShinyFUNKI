org <- c("PIG", "YEAST", "HUMAN", "MOUSE", "TRIHA", "THEAS", "SIVAM", "AERPX")

test_taxname2species <- function() {
  res <- taxname2species("PIG")
  checkIdentical(res, "Sus scrofa")
}

test_taxname2species_vec <- function() {
  res <- taxname2species(org)
  checkIdentical(
      res,
      c("Sus scrofa","Saccharomyces cerevisiae","Homo sapiens",
        "Mus musculus","Trichoderma harzianum",
        "Thermanaerovibrio acidaminovorans",
        "Simian immunodeficiency virus","Aeropyrum pernix")
  )
}

test_taxname2taxid <- function() {
  res <- taxname2taxid("PIG")
  checkTrue(res == "9823")
}

test_taxname2taxid_vec <- function() {
  res <- taxname2taxid(org)
  checkIdentical(
      res,
      as.integer(c(9823, 559292, 9606, 10090, 5544, 525903, 36378, 56636))
  )
}

test_taxname2domain <- function() {
  res <- taxname2domain("PIG")
  checkTrue(res == "E")
}

test_taxname2domain_vec <- function() {
  res <- taxname2domain(c(org, "MYCGI"))
  checkIdentical(
      res,
      factor(
          c("E","E","E","E","E","B","V","A","B"),
          levels = c("A", "B", "E", "V", "X")
      )
  )
}
