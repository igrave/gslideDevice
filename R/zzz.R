.onLoad <- function(libname, pkgname) {

  # .auth is created in R/drive_auth.R
  # this is to insure we get an instance of gargle's AuthState using the
  # current, locally installed version of gargle
  utils::assignInMyNamespace(
    ".auth",
    gargle::init_AuthState(
      package = "gslideDevice",
      auth_active = TRUE,
      gargle::gargle_oauth_client_from_json(
        path = system.file("api.json", package = "gslideDevice")
      )
    )
  )
}
