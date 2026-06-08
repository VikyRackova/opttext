# Linux: copy package .so and TBB .so into package libs
if (.Platform$OS.type == "unix" && !grepl("darwin", R.version$os)) {
  pkg_dir <- Sys.getenv("R_PACKAGE_DIR")
  dest <- file.path(pkg_dir, "libs")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)

  if (file.exists("opttext.so")) {
    file.copy("opttext.so", file.path(dest, "opttext.so"), overwrite = TRUE)
  } else {
    stop("opttext.so was not found in src during install.libs.R")
  }

  tbb_so <- Sys.glob(
    file.path(system.file(package = "RcppParallel"), "lib", "*.so*")
  )

  if (length(tbb_so) > 0) {
    file.copy(tbb_so, dest, overwrite = TRUE)
    message("Copied TBB libs: ", paste(basename(tbb_so), collapse = ", "))
  } else {
    message("No TBB .so found — relying on rpath")
  }

  message("Files in installed libs:")
  message(paste(list.files(dest), collapse = ", "))

} else if (.Platform$OS.type == "windows") {
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
}
