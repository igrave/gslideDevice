## This file is the interface between gslideDevice and the
## auth functionality in gargle.

# Initialization happens in .onLoad
.auth <- NULL

## The roxygen comments for these functions are mostly generated from data
## in this list and template text maintained in gargle.
gargle_lookup_table <- list(
  PACKAGE     = "gslideDevice",
  YOUR_STUFF  = "your presentations",
  PRODUCT     = "Google Slides",
  API         = "Slides API",
  PREFIX      = "gsd"
)

#' Authorize gslideDevice
#'
#' @eval gargle:::PREFIX_auth_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_details(gargle_lookup_table)
#' @eval gargle:::PREFIX_auth_params()
#' @param scopes A character vector of scopes to request.
#'   Pick from those listed at <https://developers.google.com/identity/protocols/oauth2/scopes>.
#'
#' @family auth functions
#' @export
#'
#' @examples
#' \dontrun{
#' ## load/refresh existing credentials, if available
#' ## otherwise, go to browser for authentication and authorization
#' gsd_auth()
#'
#' ## force use of a token associated with a specific email
#' gsd_auth(email = "jenny@example.com")
#'
#' ## force a menu where you can choose from existing tokens or
#' ## choose to get a new one
#' gsd_auth(email = NA)
#'
#' ## use a 'read only' scope, so it's impossible to change data
#' gsd_auth(
#'   scopes = "https://www.googleapis.com/auth/devstorage.read_only"
#' )
#'
#' ## use a service account token
#' gsd_auth(path = "foofy-83ee9e7c9c48.json")
#' }
gsd_auth <- function(email = gargle::gargle_oauth_email(),
                    path = NULL,
                    scopes = c(
                      "https://www.googleapis.com/auth/drive.file",
                      "https://www.googleapis.com/auth/presentations"
                      ),
                    cache = gargle::gargle_oauth_cache(),
                    use_oob = gargle::gargle_oob_default(),
                    token = NULL) {
  cred <- gargle::token_fetch(
    scopes = scopes,
    app = gsd_oauth_app() ,
    email = email,
    path = path,
    package = "gslidesDevice",
    cache = cache,
    use_oob = use_oob,
    token = token
  )
  if (!inherits(cred, "Token2.0")) {
    stop(
      "Can't get Google credentials.\n",
      "Are you running gslidesDevice in a non-interactive session? Consider:\n",
      "  * Call `gsd_auth()` directly with all necessary specifics.\n",
      call. = FALSE
    )
  }
  .auth$set_cred(cred)
  .auth$set_auth_active(TRUE)

  invisible()
}

#' Suspend authorization
#'
#' @eval gargle:::PREFIX_deauth_description_with_api_key(gargle_lookup_table)
#'
#' @family auth functions
#' @export
#' @examples
#' \dontrun{
#' gsd_deauth()
#' gsd_user()
#' }
gsd_deauth <- function() {
  .auth$set_auth_active(FALSE)
  .auth$clear_cred()
  invisible()
}

#' Produce configured token
#'
#' @eval gargle:::PREFIX_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_token_return()
#'
#' @family low-level API functions
#' @export
#' @examples
#' \dontrun{
#' gsd_token()
#' }
gsd_token <- function() {
  if (!gsd_has_token()) {
    gsd_auth()
  }
  httr::config(token = .auth$cred)
}

#' Is there a token on hand?
#'
#' @eval gargle:::PREFIX_has_token_description(gargle_lookup_table)
#' @eval gargle:::PREFIX_has_token_return()
#'
#' @family low-level API functions
#' @export
#'
#' @examples
#' gsd_has_token()
gsd_has_token <- function() {
  inherits(.auth$cred, "Token2.0")
}

#' Edit and view auth configuration
#'
#' @eval gargle:::PREFIX_auth_configure_description(gargle_lookup_table, .has_api_key = FALSE)
#' @eval gargle:::PREFIX_auth_configure_params(.has_api_key = FALSE)
#' @eval gargle:::PREFIX_auth_configure_return(gargle_lookup_table, .has_api_key = FALSE)
#'
#' @family auth functions
#' @export
#' @examples
#' # see the current user-configured OAuth app (probaby `NULL`)
#' gsd_oauth_app()
#'
#' if (require(httr)) {
#'
#'   # store current state, so we can restore
#'   original_app <- gsd_oauth_app()
#'
#'   # bring your own app via client id (aka key) and secret
#'   google_app <- httr::oauth_app(
#'     "my-awesome-google-api-wrapping-package",
#'     key = "123456789.apps.googleusercontent.com",
#'     secret = "abcdefghijklmnopqrstuvwxyz"
#'   )
#'   gsd_auth_configure(app = google_app)
#'
#'   # confirm current app
#'   gsd_oauth_app()
#'
#'   # restore original state
#'   gsd_auth_configure(app = original_app)
#'   gsd_oauth_app()
#' }
#'
#' \dontrun{
#' # bring your own app via JSON downloaded from GCP Console
#' gsd_auth_configure(
#'   path = "/path/to/the/JSON/you/downloaded/from/gcp/console.json"
#' )
#' }
#'
gsd_auth_configure <- function(app, path) {
  if (!xor(missing(app), missing(path))) {
    stop("Must supply exactly one of `app` and `path`", call. = FALSE)
  }
  if (!missing(path)) {
    stopifnot(rlang::is_string(path))
    app <- gargle::gargle_oauth_client_from_json(path)
  }
  stopifnot(is.null(app) || inherits(app, "oauth_app"))

  .auth$set_app(app)
}

#' @export
#' @rdname gsd_auth_configure
gsd_oauth_app <- function() .auth$app

#' Get info on current user
#'
#' @eval gargle:::PREFIX_user_description()
#' @eval gargle:::PREFIX_user_seealso()
#' @eval gargle:::PREFIX_user_return()
#'
#' @export
#' @examples
#' \dontrun{
#' gsd_user()
#' }
gsd_user <- function() {
  if (gsd_has_token()) {
    gargle::token_email(gsd_token())
  } else {
    NULL
  }
}
