#include <Rcpp.h>
#include <iostream>
#include <fstream>

using namespace Rcpp;

inline
std::vector<std::string> split(std::string str, const char delim) {
    
    std::vector<std::string> v;
    std::string tmp;

    for(std::string::const_iterator i = str.begin(); i != str.end(); ++i) {
        if(*i != delim && i != str.end()) {
            tmp += *i; 
        } else {
            v.push_back(tmp);
            tmp = ""; 
        }   
    }   

    return v;
}

inline bool in( std::string& elem, std::map< std::string, std::ofstream*>& x ) {
	if( x.find(elem) == x.end() ) {
		return false;
	} else {
		return true;
	}
}

// [[Rcpp::export]]
void split_file( std::string path,
		std::string dir,
		std::string basename,
		std::string path_sep,
		std::string sep,
		std::string file_ext,
		int column,
    bool chatty) {

	// space for a line, and a line post-split
	std::string line;
	std::vector< std::string > line_split;
	std::map< std::string, std::ofstream* > files;
  const char delim = *sep.c_str();

	// input file connections
	std::ifstream conn;
	conn.open( path.c_str() );

  int counter = 0;
  
	if( conn.is_open() ) {

		while( conn.good() ) {

			// read in a line of input
			std::getline( conn, line );

			// make sure that the line is not empty
			if( line == "" ) {
				continue;
			}

			// split it to a vector
			line_split = split(line, delim);
      
			// check the value of the 'column'th item
			std::string col_item = line_split[column-1];

			// if it has not yet been found, open a new file connection to
			if( !in( col_item, files ) ) {
				Rcout << "Opening new file for column entry: " << col_item << std::endl;
				std::string file_path =  dir + path_sep + basename + "_" + col_item + file_ext;
				files[col_item] = new std::ofstream( file_path.c_str() );
			}

			// write the line to the appropriate ofstream
			*files[col_item] << line << std::endl;
      
      // write out the counter?
      counter++;
      if( chatty & counter % 100000 == 0 ) {
        Rcout << "line: " << counter << std::endl;
      }

		}

	}

	// close the other file connections
	for( std::map< std::string, std::ofstream*>::iterator itr = files.begin(); itr != files.end(); itr++ ) {
		itr->second->close();
	}

}
