PRINFO <- function (x, pos = -1L, envir = as.environment(pos), inherits = TRUE,
    evaluated = FALSE)
.External2(.C_PRINFO, if (evaluated) x else substitute(x), envir, inherits)


mkPROMISE <- function (expr, envir = parent.frame(1))
.External2(.C_mkPROMISE, expr, envir)


mkEVPROMISE <- function (expr, value)
.External2(.C_mkEVPROMISE, expr, value)


unlockEnvironment <- function (env, bindings = FALSE)
.External2(.C_unlockEnvironment, env, bindings)
