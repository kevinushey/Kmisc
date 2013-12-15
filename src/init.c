#include <R.h>
#include <Rinternals.h>

#include <R_ext/Rdynload.h>

SEXP any_na( SEXP x );
SEXP charlist_transpose_to_df( SEXP x, SEXP names );
SEXP copy(SEXP x_);
SEXP df2list(SEXP x_, SEXP inplace);
SEXP factor_to_char( SEXP X_, SEXP inplace_ );
SEXP in_interval( SEXP x, SEXP lo, SEXP hi, 
        SEXP include_lower, SEXP include_upper );
SEXP list2df(SEXP x_, SEXP inplace);
SEXP mat2df(SEXP x);
SEXP melt_dataframe( SEXP x, SEXP id_ind_, SEXP val_ind_, SEXP variable_name, SEXP value_name );
SEXP melt_matrix( SEXP x );
SEXP simp( SEXP x, SEXP y );
SEXP str_rev( SEXP x );
SEXP str_slice(SEXP x, SEXP n);
SEXP transpose_list(SEXP x_);
SEXP unmelt(SEXP data, SEXP uniq_id, SEXP other_ind_, SEXP id_ind_, SEXP value_ind_);
SEXP Kmisc_char_to_factor(SEXP x_SEXP, SEXP inplaceSEXP);
SEXP Kmisc_counts(SEXP xSEXP);
SEXP Kmisc_extract_rows_from_file_to_file(SEXP input_file_nameSEXP, SEXP output_file_nameSEXP, SEXP delimSEXP, SEXP items_to_keepSEXP, SEXP column_to_checkSEXP);
SEXP Kmisc_extract_rows_from_file(SEXP input_file_nameSEXP, SEXP delimSEXP, SEXP items_to_keepSEXP, SEXP column_to_checkSEXP);
SEXP Kmisc_fast_factor(SEXP xSEXP, SEXP levelsSEXP);
SEXP Kmisc_matches(SEXP xSEXP);
SEXP Kmisc_read(SEXP pathSEXP, SEXP linesSEXP);
SEXP Kmisc_split_file(SEXP pathSEXP, SEXP dirSEXP, SEXP basenameSEXP, SEXP path_sepSEXP, SEXP sepSEXP, SEXP prependSEXP, SEXP file_extSEXP, SEXP columnSEXP, SEXP skipSEXP, SEXP verboseSEXP);
SEXP Kmisc_split_runs_numeric(SEXP XSEXP);
SEXP Kmisc_split_runs_character(SEXP XSEXP);
SEXP Kmisc_split_runs_one(SEXP xSEXP);
SEXP Kmisc_stack_list_df(SEXP XSEXP, SEXP classesSEXP, SEXP num_elemSEXP, SEXP make_row_namesSEXP, SEXP nameSEXP, SEXP keep_list_indexSEXP, SEXP index_nameSEXP);
SEXP Kmisc_str_collapse_list(SEXP xSEXP);
SEXP Kmisc_str_collapse_str(SEXP xSEXP);

R_CallMethodDef callMethods[]  = {
  {"Cany_na", (DL_FUNC) &any_na, 1},
  {"Ccharlist_transpose_to_df", (DL_FUNC) &charlist_transpose_to_df, 2},
  {"Ccopy", (DL_FUNC) &copy, 1},
  {"Cdf2list", (DL_FUNC) &df2list, 2},
  {"Cfactor_to_char", (DL_FUNC) &factor_to_char, 2},
  {"Cin_interval", (DL_FUNC) &in_interval, 5},
  {"Clist2df", (DL_FUNC) &list2df, 2},
  {"Cmat2df", (DL_FUNC) &mat2df, 1},
  {"Cmelt_dataframe", (DL_FUNC) &melt_dataframe, 5},
  {"Cmelt_matrix", (DL_FUNC) &melt_matrix, 1},
  {"Csimp", (DL_FUNC) &simp, 2},
  {"Cstr_rev", (DL_FUNC) &str_rev, 1},
  {"Cstr_slice", (DL_FUNC) &str_slice, 2},
  {"Ctranspose_list", (DL_FUNC) &transpose_list, 1},
  {"Cunmelt", (DL_FUNC) &unmelt, 5},
  {"CKmisc_char_to_factor", (DL_FUNC) &Kmisc_char_to_factor, 2},
  {"CKmisc_counts", (DL_FUNC) &Kmisc_counts, 1},
  {"CKmisc_extract_rows_from_file_to_file", (DL_FUNC) &Kmisc_extract_rows_from_file_to_file, 5},
  {"CKmisc_extract_rows_from_file", (DL_FUNC) &Kmisc_extract_rows_from_file, 4},
  {"CKmisc_fast_factor", (DL_FUNC) &Kmisc_fast_factor, 2},
  {"CKmisc_matches", (DL_FUNC) &Kmisc_matches, 1},
  {"CKmisc_read", (DL_FUNC) &Kmisc_read, 2},
  {"CKmisc_split_file", (DL_FUNC) &Kmisc_split_file, 10},
  {"CKmisc_split_runs_numeric", (DL_FUNC) &Kmisc_split_runs_numeric, 1},
  {"CKmisc_split_runs_character", (DL_FUNC) &Kmisc_split_runs_character, 1},
  {"CKmisc_split_runs_one", (DL_FUNC) &Kmisc_split_runs_one, 1},
  {"CKmisc_stack_list_df", (DL_FUNC) &Kmisc_stack_list_df, 7},
  {"CKmisc_str_collapse_list", (DL_FUNC) &Kmisc_str_collapse_list, 1},
  {"CKmisc_str_collapse_str", (DL_FUNC) &Kmisc_str_collapse_str, 1},
  {NULL, NULL, 0}
};

void R_init_Kmisc(DllInfo *info) {
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
}

