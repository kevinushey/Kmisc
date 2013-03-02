#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string.h>

using namespace Rcpp;

inline std::string get_item( std::string& line, const char* delim, int column ) {

	char* line_cast = const_cast<char*>( line.c_str() );
	const char* pch = strtok(line_cast, delim);
	int counter = 0;
	while( TRUE ) {
		if( counter == column-1 ) {
			return( std::string(pch) );
		}
		pch = strtok(NULL, delim);
		counter++;
	}
	return( "get_line is broken" );
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
    int skip,
		bool chatty) {

	// space for a line, and a file map
	std::string line;
	std::map< std::string, std::ofstream* > files;
	const char* delim = sep.c_str();

	// input file connections
	std::ifstream conn;
	conn.open( path.c_str() );

	int counter = 0;

	if( conn.is_open() ) {

		while( conn.good() ) {
      
      // skip lines
      if( skip > 0 ) {
        for( int i=0; i < skip; i++ ) {
          std::getline( conn, line );
        }
      }

			// read in a line of input
			std::getline( conn, line );

			// make sure that the line is not empty
			if( line == "" ) {
				continue;
			}

			// check the value of the 'column'th item
			// we copy the string so that strtok doesn't mangle it
			std::string str_copy;
			str_copy = line.c_str();
			std::string col_item = get_item(str_copy, delim, column);

			// if it has not yet been found, open a new file connection
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
		files.erase(itr);
	}

}
