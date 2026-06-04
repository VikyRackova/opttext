files <- c(
  "opttext.dll",
  "libicudt77.dll",
  "libicuin77.dll",
  "libicuuc77.dll",
  "libgcc_s_seh-1.dll",
  "libstdc++-6.dll",
  "libwinpthread-1.dll"
)

dest <- file.path(Sys.getenv("R_PACKAGE_DIR"), "libs", Sys.getenv("R_ARCH"))
dir.create(dest, recursive = TRUE, showWarnings = FALSE)

for (f in files) {
  from <- f
  to <- file.path(dest, f)

  if (file.exists(from)) {
    file.copy(from, to, overwrite = TRUE)
  }
}
