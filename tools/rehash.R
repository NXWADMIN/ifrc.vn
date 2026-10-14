# Replaces the seed's PBKDF2 password hashes with libsodium (scrypt) hashes, which R verifies quickly.
# Run from the shiny folder after export-seed.js:  Rscript tools/rehash.R
library(DBI)
seed <- "data/fms-seed.db"
tmp <- tempfile(fileext = ".db")
file.copy(seed, tmp)
con <- dbConnect(RSQLite::SQLite(), tmp)
logins <- jsonlite::fromJSON("tools/logins.json")
logins <- rbind(logins[, c("email", "password")],
                data.frame(email = c("thanh.nguyen@example.com", "kevin.tran@example.com"), password = "welcome1"))
for (i in seq_len(nrow(logins))) {
  n <- dbExecute(con, "UPDATE fms_users SET password_hash = ? WHERE lower(email) = ?",
                 params = list(sodium::password_store(logins$password[i]), tolower(logins$email[i])))
  cat(sprintf("%-32s %s\n", logins$email[i], if (n) "rehashed" else "not found"))
}
dbExecute(con, "VACUUM")
dbDisconnect(con)
file.copy(tmp, seed, overwrite = TRUE)
