#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void model_deterministic_initmod_desolve(void *);
extern void model_deterministic_output_dde(void *);
extern void model_deterministic_rhs_dde(void *);
extern void model_deterministic_rhs_desolve(void *);

/* .Call calls */
extern SEXP model_deterministic_contents(SEXP);
extern SEXP model_deterministic_create(SEXP);
extern SEXP model_deterministic_initial_conditions(SEXP, SEXP);
extern SEXP model_deterministic_metadata(SEXP);
extern SEXP model_deterministic_rhs_r(SEXP, SEXP, SEXP);
extern SEXP model_deterministic_set_initial(SEXP, SEXP, SEXP, SEXP);
extern SEXP model_deterministic_set_user(SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
    {"model_deterministic_initmod_desolve", (DL_FUNC) &model_deterministic_initmod_desolve, 1},
    {"model_deterministic_output_dde",      (DL_FUNC) &model_deterministic_output_dde,      1},
    {"model_deterministic_rhs_dde",         (DL_FUNC) &model_deterministic_rhs_dde,         1},
    {"model_deterministic_rhs_desolve",     (DL_FUNC) &model_deterministic_rhs_desolve,     1},
    {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
    {"model_deterministic_contents",           (DL_FUNC) &model_deterministic_contents,           1},
    {"model_deterministic_create",             (DL_FUNC) &model_deterministic_create,             1},
    {"model_deterministic_initial_conditions", (DL_FUNC) &model_deterministic_initial_conditions, 2},
    {"model_deterministic_metadata",           (DL_FUNC) &model_deterministic_metadata,           1},
    {"model_deterministic_rhs_r",              (DL_FUNC) &model_deterministic_rhs_r,              3},
    {"model_deterministic_set_initial",        (DL_FUNC) &model_deterministic_set_initial,        4},
    {"model_deterministic_set_user",           (DL_FUNC) &model_deterministic_set_user,           2},
    {NULL, NULL, 0}
};

void R_init_metapopdeterministic(DllInfo *dll)
{
    R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
