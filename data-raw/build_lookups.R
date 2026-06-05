## data-raw/build_lookups.R
##
## Refresh R/sysdata.rda with the curated BMF lookup tables from S3.
##
## Source:  s3://nccsdata/lookups/bmf/latest/  (published by ../nccs-data-bmf/)
## Output:  R/sysdata.rda containing a single list `.nccs_lookups` with one
##          element per bundled table plus a `.metadata` element holding the
##          manifest's vintage, generated_at, and per-file sha256.
##
## Run from the package root:
##   Rscript data-raw/build_lookups.R
##
## Requirements: jsonlite, usethis (dev only, not package deps); sha256sum on PATH.

## ---- Configuration -------------------------------------------------------

S3_HTTPS_BASE <- "https://nccsdata.s3.amazonaws.com/lookups/bmf/latest"

## Curated subset — see CLAUDE.md § "Bundled lookups". Confirm with the user
## before adding any of the larger or pipeline-internal tables.
CURATED <- c(
  "subsection_classification_code",
  "nteev2_subsector",
  "foundation_code",
  "affiliation_code"
)

SYSDATA_PATH <- "R/sysdata.rda"

## ---- Helpers -------------------------------------------------------------

sha256_file <- function(path) {
  out <- tryCatch(
    system2("sha256sum", shQuote(path), stdout = TRUE, stderr = TRUE),
    error = function(e) NULL
  )
  if (is.null(out) || !length(out)) {
    stop("sha256sum not available on PATH; install coreutils or run in WSL.")
  }
  sub(" .*$", "", out[[1]])
}

download_text <- function(url, destfile) {
  utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
  invisible(destfile)
}

load_prior_lookups <- function(path) {
  if (!file.exists(path)) return(NULL)
  env <- new.env(parent = emptyenv())
  load(path, envir = env)
  if (!exists(".nccs_lookups", envir = env)) return(NULL)
  env$.nccs_lookups
}

print_diff <- function(prior, current) {
  cat("\n--- Diff vs previously bundled lookups ---\n")
  if (is.null(prior)) {
    cat("(no prior R/sysdata.rda — this is the first build)\n")
    return(invisible())
  }
  prior_meta <- prior$.metadata
  cur_meta   <- current$.metadata
  cat(sprintf("vintage:      %s -> %s\n",
              prior_meta$vintage %||% "?", cur_meta$vintage))
  cat(sprintf("generated_at: %s -> %s\n",
              prior_meta$generated_at %||% "?", cur_meta$generated_at))

  all_names <- union(setdiff(names(prior),   ".metadata"),
                     setdiff(names(current), ".metadata"))
  for (nm in sort(all_names)) {
    p <- prior[[nm]]; c_ <- current[[nm]]
    if (is.null(p)) {
      cat(sprintf("  + %s (new, %d rows)\n", nm, nrow(c_))); next
    }
    if (is.null(c_)) {
      cat(sprintf("  - %s (removed)\n", nm)); next
    }
    delta <- nrow(c_) - nrow(p)
    code_col <- names(c_)[[1L]]
    prior_codes <- as.character(p[[code_col]])
    cur_codes   <- as.character(c_[[code_col]])
    added   <- setdiff(cur_codes,   prior_codes)
    removed <- setdiff(prior_codes, cur_codes)
    cat(sprintf("  %s: %d -> %d rows (%+d); +%d / -%d codes\n",
                nm, nrow(p), nrow(c_), delta, length(added), length(removed)))
    if (length(added))   cat(sprintf("      added:   %s\n", paste(added,   collapse = ", ")))
    if (length(removed)) cat(sprintf("      removed: %s\n", paste(removed, collapse = ", ")))
  }
}

`%||%` <- function(a, b) if (is.null(a)) b else a

## ---- Main ----------------------------------------------------------------

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("jsonlite is required for data-raw/build_lookups.R. install.packages('jsonlite').")
}
if (!requireNamespace("usethis", quietly = TRUE)) {
  stop("usethis is required for data-raw/build_lookups.R. install.packages('usethis').")
}

tmp <- tempfile("nccsdata_lookups_"); dir.create(tmp)
on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

cat("Fetching MANIFEST.json from", S3_HTTPS_BASE, "\n")
manifest_path <- file.path(tmp, "MANIFEST.json")
download_text(file.path(S3_HTTPS_BASE, "MANIFEST.json"), manifest_path)
manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)

cat("Vintage:     ", manifest$vintage, "\n")
cat("Generated at:", manifest$built_at, "\n")

manifest_tables <- sub("\\.csv$", "", names(manifest$files))
missing_from_manifest <- setdiff(CURATED, manifest_tables)
if (length(missing_from_manifest)) {
  stop("Curated tables not in manifest: ",
       paste(missing_from_manifest, collapse = ", "))
}

lookups <- list()
sha_metadata <- list()

for (nm in CURATED) {
  entry <- manifest$files[[paste0(nm, ".csv")]]
  expected_sha <- entry$sha256
  url <- file.path(S3_HTTPS_BASE, entry$file)
  dest <- file.path(tmp, entry$file)

  cat(sprintf("Downloading %s ...\n", entry$file))
  download_text(url, dest)

  actual_sha <- sha256_file(dest)
  if (!identical(actual_sha, expected_sha)) {
    stop(sprintf("sha256 mismatch for %s\n  expected: %s\n  actual:   %s",
                 entry$file, expected_sha, actual_sha))
  }

  df <- utils::read.csv(dest, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(df) != entry$row_count) {
    stop(sprintf("Row-count mismatch for %s: manifest=%d, file=%d",
                 nm, entry$row_count, nrow(df)))
  }
  lookups[[nm]] <- dplyr::as_tibble(df)
  sha_metadata[[nm]] <- expected_sha
}

.nccs_lookups <- c(
  lookups,
  list(.metadata = list(
    vintage      = manifest$vintage,
    generated_at = manifest$built_at,
    source       = S3_HTTPS_BASE,
    sha256       = sha_metadata,
    built_at     = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  ))
)

prior <- load_prior_lookups(SYSDATA_PATH)
print_diff(prior, .nccs_lookups)

cat("\nWriting", SYSDATA_PATH, "\n")
usethis::use_data(.nccs_lookups, internal = TRUE, overwrite = TRUE)

cat("Done.\n")
