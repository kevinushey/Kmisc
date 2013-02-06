#include <Rcpp.h>
using namespace Rcpp;

inline
CharacterVector stack_char( List& X, int index ) {
  
  std::vector< std::string > out;
  for( unsigned int i=0; i < X.size(); i++ ) {
    List tmp = as<List>( X[i] );
    std::vector< std::string > tmp2 = tmp[index];
    for( unsigned int j=0; j < tmp2.size(); j++ ) {
      out.push_back( tmp2[j] );
    }
  }
  
  return wrap(out);
  
}

inline
NumericVector stack_numeric( List& X, int index ) {
  
  std::vector<double> out;
  for( unsigned int i=0; i < X.size(); i++ ) {
    List tmp = as<List>( X[i] );
    std::vector<double> tmp2 = tmp[index];
    for( unsigned int j=0; j < tmp2.size(); j++ ) {
      out.push_back( tmp2[j] );
    }
  }
  
  return wrap(out);
  
}

inline
IntegerVector stack_int( List& X, int index ) {
  
  std::vector<int> out;
  for( unsigned int i=0; i < X.size(); i++ ) {
    List tmp = as<List>( X[i] );
    std::vector<int> tmp2 = tmp[index];
    for( unsigned int j=0; j < tmp2.size(); j++ ) {
      out.push_back( tmp2[j] );
    }
  }
  
  return wrap(out);
  
}

inline
LogicalVector stack_bool( List& X, int index ) {
  
  std::vector< bool > out;
  for( unsigned int i=0; i < X.size(); i++ ) {
    List tmp = as<List>( X[i] );
    std::vector< bool > tmp2 = tmp[index];
    for( unsigned int j=0; j < tmp2.size(); j++ ) {
      out.push_back( tmp2[j] );
    }
  }
  
  return wrap(out);
  
}

inline
RawVector stack_raw( List& X, int index ) {
  
  std::vector< unsigned char > out;
  for( unsigned int i=0; i < X.size(); i++ ) {
    List tmp = as<List>( X[i] );
    std::vector< unsigned char > tmp2 = tmp[index];
    for( unsigned int j=0; j < tmp2.size(); j++ ) {
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
  for( unsigned int i=0; i < num_elem; i++ ) {
    
    if( classes[i] == "character" ) {
      CharacterVector tmp = stack_char(X, i);
      out[i] = tmp;
    } else if( classes[i] == "numeric" ) {
      NumericVector tmp = stack_numeric(X, i);
      out[i] = tmp;
    } else if( classes[i] == "integer" || classes[i] == "factor" ) {
      IntegerVector tmp = stack_int(X, i);
      out[i] = tmp;
    } else if( classes[i] == "logical" ) {
      LogicalVector tmp = stack_bool(X, i);
      out[i] = tmp;
    } else if( classes[i] == "raw" ) {
      RawVector tmp = stack_raw(X, i);
      out[i] = tmp;
    } else {
      stop("one of the vectors in your list is of incompatible type");
    }

  }
  
  // add the list indices as a vector
  if( keep_list_index ) {
    std::vector<int> list_indices;
    int counter = 1;
    for( unsigned int i=0; i < X.size(); i++ ) {
      List tmp = as<List>( X[i] );
      for( unsigned int j=0; j < ::Rf_length( tmp[0] ); j++ ) {
        list_indices.push_back(counter);
      }
      counter++;
    }
    out["index"] = wrap( list_indices );
  }
  
  // get row names to assign to vector of df
  if( make_row_names ) {
    std::vector< std::string > row_names;
    for( unsigned int i=0; i < X.size(); i++ ) {
      std::vector< std::string > rownames = as<std::vector< std::string > >( as<List>( X[i] ).attr("row.names") );
      for( unsigned int j=0; j < rownames.size(); j++ ) {
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
