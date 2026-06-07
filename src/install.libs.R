pkg_dir <- Sys.getenv("R_PACKAGE_DIR")
dest <- file.path(pkg_dir, "libs", "x64")

dir.create(dest, recursive = TRUE, showWarnings = FALSE)

if (file.exists("opttext.dll")) {
  file.copy("opttext.dll", file.path(dest, "opttext.dll"), overwrite = TRUE)
}

# ICU DLLs from Rtools
extra_dlls <- c(
  "C:/rtools45/mingw64/bin/libicuuc77.dll",
  "C:/rtools45/mingw64/bin/libicuin77.dll",
  "C:/rtools45/mingw64/bin/libicudt77.dll"
)
extra_dlls <- extra_dlls[file.exists(extra_dlls)]
if (length(extra_dlls) > 0) {
  file.copy(extra_dlls, dest, overwrite = TRUE)
}

# RcppParallel DLLs (libs/x64)
rcppparallel_dlls <- Sys.glob(
  file.path(system.file(package = "RcppParallel"), "libs", "x64", "*.dll")
)
if (length(rcppparallel_dlls) > 0) {
  file.copy(rcppparallel_dlls, dest, overwrite = TRUE)
}

# tbb.dll (lives in lib/x64, not libs/x64)
tbb_dlls <- Sys.glob(
  file.path(system.file(package = "RcppParallel"), "lib", "x64", "*.dll")
)
if (length(tbb_dlls) > 0) {
  file.copy(tbb_dlls, dest, overwrite = TRUE)
}

message("Files in installed libs/x64:")
message(paste(list.files(dest), collapse = ", "))
