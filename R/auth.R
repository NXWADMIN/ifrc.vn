# Passwords are stored as libsodium scrypt hashes ("$7$..."). Hashes made by the Node version
# (pbkdf2_sha256$iterations$salt$hash) still verify, and are upgraded on the next sign-in.

hash_password <- function(password) sodium::password_store(enc2utf8(password))

verify_password <- function(password, stored) {
  if (is.na(stored) || !nzchar(password)) return(FALSE)
  if (startsWith(stored, "$7$")) return(sodium::password_verify(stored, enc2utf8(password)))
  parts <- strsplit(stored, "$", fixed = TRUE)[[1]]
  if (length(parts) != 4 || parts[1] != "pbkdf2_sha256") return(FALSE)
  got <- pbkdf2_sha256(password, sodium::hex2bin(parts[3]), as.integer(parts[2]))
  expected <- sodium::hex2bin(parts[4])
  length(got) == length(expected) && sum(as.integer(xor(got, expected))) == 0
}

# PBKDF2-HMAC-SHA256, one 32-byte block, as used by the Node version.
pbkdf2_sha256 <- function(password, salt, iterations) {
  key <- charToRaw(enc2utf8(password))
  u <- as.raw(openssl::sha256(c(salt, as.raw(c(0, 0, 0, 1))), key = key))
  out <- u
  for (i in seq_len(iterations - 1L)) {
    u <- as.raw(openssl::sha256(u, key = key))
    out <- xor(out, u)
  }
  out
}
