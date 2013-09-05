#include <Rcpp.h>
using namespace Rcpp;

template <int RTYPE, class container>
inline
Vector<RTYPE> stack( List& X, int index ) {
	std::vector<container> out;
	int x_size = X.size();
	for( int i=0; i < x_size; ++i ) {
		List tmp = as<List>(X[i]);
		std::vector<container> tmp2 = tmp[index];
		int tmp2_size = tmp2.size();
		for( int j=0; j < tmp2_size; ++j ) {
			out.push_back( tmp2[j] );
		}
	}
	return wrap(out);
}

// [[Rcpp::export]]
List stack_list_df( List& X,
		std::vector< std::string > classes,
		int num_elem,
		bool make_row_names,
		std::string name,
		bool keep_list_index,
		std::string index_name ) {

	List out(num_elem);

	// loop through the columns to generate the stacked DF
	List tmp = as<List>(X[0]);

	for( int i=0; i < num_elem; ++i ) {

		switch( TYPEOF(tmp[i]) ) {
		case STRSXP:
			out[i] = stack<STRSXP, std::string>(X, i);
			break;
		case REALSXP:
			out[i] = stack<REALSXP, double>(X, i);
			break;
		case INTSXP:
			out[i] = stack<INTSXP, int>(X, i);
			break;
		case LGLSXP:
			out[i] = stack<LGLSXP, int>(X, i);
			break;
		case RAWSXP:
			out[i] = stack<RAWSXP, unsigned char>(X, i);
			break;
		}

	}

	// add the list indices as a vector
	if( keep_list_index ) {
		std::vector<int> list_indices;
		int counter = 1;
		for( int i=0; i < X.size(); ++i ) {
			List tmp = as<List>( X[i] );
			for( int j=0; j < ::Rf_length( tmp[0] ); ++j ) {
				list_indices.push_back(counter);
			}
			++counter;
		}
		out["index"] = wrap( list_indices );
	}

	// get row names to assign to vector of df
	if( make_row_names ) {
		std::vector< std::string > row_names;
		for( int i=0; i < X.size(); ++i ) {
			std::vector< std::string > rownames = as<std::vector< std::string > >( as<List>( X[i] ).attr("row.names") );
			int rownames_size = rownames.size();
      for( int j=0; j < rownames_size; ++j ) {
				row_names.push_back( rownames[j] );
			}
		}

		out["which"] = wrap( row_names );
	}

	std::vector<std::string> col_names = as<List>(X[0]).attr("names");

	if( keep_list_index ) {
		col_names.push_back( index_name );
	}

	if( make_row_names ) {
		col_names.push_back( name );
	}

	out.attr("names") = col_names;
	out.attr("class") = "data.frame";
	return out;

}
