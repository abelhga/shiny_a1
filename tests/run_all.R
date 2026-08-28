# Run every check in this folder.
#
#   Rscript tests/run_all.R      (from the repository root)
#
# These exercise the pure logic: tokenising, graph building, suggestion
# harvesting, series cleaning and anomaly scoring. They make no network calls
# and need no API keys, so they are safe to run anywhere.

files <- sort(list.files("tests", pattern = "^test_.*[.]R$", full.names = TRUE))

for (file in files) {
  cat("\n==", basename(file), "==\n")
  source(file, local = new.env())
}

cat("\nAll checks passed.\n")
