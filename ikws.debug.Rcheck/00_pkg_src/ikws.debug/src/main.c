#define R_NO_REMAP
#include <R.h>
#include <Rinternals.h>


#if defined(ENABLE_NLS)
#include <libintl.h>
#define _(String) dgettext ("R", String)
#else
#define _(String) (String)
#endif


extern Rboolean R_Visible;


extern SEXP ddfind(int i, SEXP rho);


int ddVal(SEXP symbol)
{
    const char *buf;
    char *endp;
    int rval;

    buf = R_CHAR(PRINTNAME(symbol));
    if (!strncmp(buf, "..", 2) && strlen(buf) > 2) {
        buf += 2;
        rval = (int) strtol(buf, &endp, 10);
        return ((*endp != '\0') ? 0 : rval);
    }
    return 0;
}


SEXP ddfindVar(SEXP symbol, SEXP rho)
{
    int i = ddVal(symbol);
    return ddfind(i, rho);
}


#define wrong_nargs_to_External(nargs, name, expected_nargs)   \
    (((nargs) == 1) ? "%d argument passed to .External(%s) which requires %s" :\
                      "%d arguments passed to .External(%s) which requires %s"),\
                     (nargs), (name), (expected_nargs)


SEXP R_getS4DataSlot(SEXP obj, SEXPTYPE type)
{
    static SEXP _DataSymbol  = NULL,
                _xDataSymbol = NULL;
    if (_DataSymbol == NULL) {
        _DataSymbol  = Rf_install(".Data");
        _xDataSymbol = Rf_install(".xData");
    }


    SEXP value = Rf_getAttrib(obj, _DataSymbol);
    if (value == R_NilValue)
        value = Rf_getAttrib(obj, _xDataSymbol);
    if (value != R_NilValue &&
        (type == ANYSXP || type == TYPEOF(value)))
    {
        return value;
    }
    else return R_NilValue;
}


#define simple_as_environment(arg) (IS_S4_OBJECT(arg) && (TYPEOF(arg) == S4SXP) ? R_getS4DataSlot(arg, ENVSXP) : R_NilValue)


#define _get_sym(elsecode)                                     \
    sym = CAR(args);                                           \
    if (TYPEOF(sym) == SYMSXP);                                \
    else if (Rf_isValidStringF(sym)) {                         \
        if (XLENGTH(sym) > 1)                                  \
            Rf_errorcall(call, _("first argument has length > 1"));\
        sym = Rf_installTrChar(STRING_ELT(sym, 0));            \
    }                                                          \
    else elsecode





SEXP PRINFO(SEXP e)
{
    if (TYPEOF(e) != PROMSXP)
        Rf_error("in PRINFO: argument is not a promise");


    /*
     * PRCODE
     * PRENV
     * PREXPR
     * PRSEEN
     * PRVALUE
     */


#define n 4
#define allocate_value_and_names(len)                          \
        value = Rf_allocVector(VECSXP, len);                   \
        Rf_protect(value);                                     \
        names = Rf_allocVector(STRSXP, len);                   \
        Rf_setAttrib(value, R_NamesSymbol, names)


    SEXP value, names;
    if (PRVALUE(e) == R_UnboundValue) {
        allocate_value_and_names(n);
    }
    else {
        allocate_value_and_names(n + 1);
        SET_VECTOR_ELT(value, n, PRVALUE(e));
        SET_STRING_ELT(names, n, Rf_mkChar("PRVALUE"));
    }


#undef n
#undef allocate_value_and_names


    SET_VECTOR_ELT(value, 0,                  PRCODE(e) );
    SET_VECTOR_ELT(value, 1,                  PRENV (e) );
    SET_VECTOR_ELT(value, 2,                  PREXPR(e) );
    SET_VECTOR_ELT(value, 3, Rf_ScalarInteger(PRSEEN(e)));


    SET_STRING_ELT(names, 0, Rf_mkChar("PRCODE"));
    SET_STRING_ELT(names, 1, Rf_mkChar("PRENV" ));
    SET_STRING_ELT(names, 2, Rf_mkChar("PREXPR"));
    SET_STRING_ELT(names, 3, Rf_mkChar("PRSEEN"));


    Rf_unprotect(1);
    return value;
}


SEXP do_PRINFO(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);


    int nargs = Rf_length(args);


    SEXP sym, env = rho;
    int inherits = TRUE;


    switch (nargs) {
    case 3:
        inherits = Rf_asLogical(CADDR(args));
        if (inherits == NA_LOGICAL)
            Rf_errorcall(call, _("invalid '%s' argument"), "inherits");
    case 2:
        env = CADR(args);
        if (!Rf_isEnvironment(env) &&
            !Rf_isEnvironment(env = simple_as_environment(env)))
        {
            Rf_errorcall(call, _("invalid '%s' argument"), "envir");
        }
    case 1:
        _get_sym({
            if (TYPEOF(sym) == PROMSXP)
                return PRINFO(sym);
            Rf_errorcall(call, _("invalid '%s' argument"), "x");
        })
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(nargs, ".C_PRINFO", "1, 2, or 3"));
        return R_NilValue;
    }


    if (sym == R_MissingArg)
        Rf_error(_("argument \"%s\" is missing, with no default"), "x");


    SEXP e;
    if (DDVAL(sym))
        e = ddfindVar(sym, env);
    else
        e = (inherits ? Rf_findVar(sym, env) : Rf_findVarInFrame(env, sym));
    if (e == R_UnboundValue)
        Rf_error(_("object '%s' not found"), R_CHAR(PRINTNAME(sym)));
    if (TYPEOF(e) != PROMSXP)
        Rf_error("'%s' is not a promise", R_CHAR(PRINTNAME(sym)));


    return PRINFO(e);
}





SEXP (*makePROMISE)(SEXP expr, SEXP env);
SEXP (*makeEVPROMISE)(SEXP expr, SEXP value);


SEXP do_mkPROMISE(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    SEXP expr = CAR(args); args = CDR(args);
    SEXP env  = CAR(args); args = CDR(args);
    if (!Rf_isEnvironment(env)) Rf_error(_("not an environment"));


    return makePROMISE(expr, env);
}


SEXP do_mkEVPROMISE(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);
    return makeEVPROMISE(CAR(args), CADR(args));
}





#define FRAME_LOCK_MASK (1<<14)
#define UNLOCK_FRAME(e) SET_ENVFLAGS(e, ENVFLAGS(e) & (~ FRAME_LOCK_MASK))


void unLockEnvironment(SEXP env, Rboolean bindings)
{
    if (IS_S4_OBJECT(env) && (TYPEOF(env) == S4SXP))
        env = R_getS4DataSlot(env, ANYSXP); /* better be an ENVSXP */

    if (TYPEOF(env) != ENVSXP)
        Rf_error(_("not an environment"));
    if (bindings) {
        Rf_protect(env);
        SEXP names = R_lsInternal3(env, /* all */ TRUE, /* sorted */ FALSE);
        Rf_protect(names);
        for (int i = 0, n = LENGTH(names); i < n; i++)
            R_unLockBinding(Rf_installTrChar(STRING_ELT(names, i)), env);
        Rf_unprotect(2);
    }
    UNLOCK_FRAME(env);
}


SEXP do_unlockEnvironment(SEXP call, SEXP op, SEXP args, SEXP rho)
{
    args = CDR(args);


    SEXP frame;
    Rboolean bindings = FALSE;
    switch (Rf_length(args)) {
    case 2:
        bindings = Rf_asLogical(CADR(args));
    case 1:
        frame = CAR(args);
        break;
    default:
        Rf_errorcall(call, wrong_nargs_to_External(Rf_length(args), ".C_unlockEnvironment", "1 or 2"));
        return R_NilValue;
    }


    unLockEnvironment(frame, bindings);
    R_Visible = FALSE;
    return R_NilValue;
}





#include <R_ext/Rdynload.h>    /* need R_ExternalMethodDef */
#include <R_ext/Visibility.h>  /* need attribute_visible */


static const R_ExternalMethodDef externalRoutines[] = {
{"PRINFO"           , (DL_FUNC) &do_PRINFO           , -1},
{"mkPROMISE"        , (DL_FUNC) &do_mkPROMISE        ,  2},
{"mkEVPROMISE"      , (DL_FUNC) &do_mkEVPROMISE      ,  2},
{"unlockEnvironment", (DL_FUNC) &do_unlockEnvironment, -1},
{NULL, NULL, 0}
};


attribute_visible
void R_init_ikws_debug(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, NULL, externalRoutines);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);


    makePROMISE   = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("this.path", "makePROMISE"  );
    makeEVPROMISE = (SEXP(*)(SEXP,SEXP)) R_GetCCallable("this.path", "makeEVPROMISE");
}
