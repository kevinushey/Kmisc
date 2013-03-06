#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <string.h>

using namespace Rcpp;

inline std::string get_item( std::string& line, const char* delim, int column ) {

	char* line_cast = const_cast<char*>( line.c_str() );
	const char* pch = strtok(line_cast, delim);
	int counter = 0;
	while( pch != NULL ) {
		if( counter == column-1 ) {
			return( std::string(pch) );
		}
		pch = strtok(NULL, delim);
		counter++;
	}
	stop( "get_line is broken" );
	return( "get_line is broken" );
}

inline bool in( std::string& elem, std::map<std::string, std::ofstream*>& x ) {
	if( x.find(elem) == x.end() ) {
		return false;
	} else {
		return true;
	}
}

// [[Rcpp::export]]
void split_file(
		std::string path,
		std::string dir,
		std::string basename,
		std::string path_sep,
		std::string sep,
		std::string file_ext,
		int column,
		int skip,
		bool verbose) {

	// space for a line, and a file map
	std::string line;
	std::map< std::string, std::ofstream* > files;
	std::map< std::string, std::ostreambuf_iterator<char>* > file_itrs;
	const char* delim = sep.c_str();

	// input file connections
	std::ifstream conn;
	conn.open( path.c_str(), std::ios_base::binary );

	int counter = 0;

	if( conn.is_open() ) {

		// skip lines
		if( skip > 0 ) {
			for( int i=0; i < skip; i++ ) {
				std::getline( conn, line );
			}
		}

		while( std::getline( conn, line ) ) {

			// make sure that the line is not empty
			// if( line == "" ) {
			// 	continue;
			// }

			// check the value of the 'column'th item
			// we copy the string so that strtok doesn't mangle it
			std::string str_copy;
			str_copy = line.c_str();
			std::string col_item = get_item(str_copy, delim, column);

			// if a column entry has not yet been found, open a new file connection
			if( !in( col_item, files ) ) {
				if( verbose ) {
					Rcout << "Opening new file for column entry: " << col_item << std::endl;
				}
				std::string file_path =  dir + path_sep + basename + "_" + col_item + file_ext;
				files[col_item] = new std::ofstream( file_path.c_str() );
				file_itrs[col_item] = new std::ostreambuf_iterator<char>( *files[col_item] );
			}

			// write the line to the appropriate ofstream
			copy( line.begin(), line.end(), *file_itrs[col_item] );
			*file_itrs[col_item] = '\n';
			// *files[col_item] << line << std::endl;

			// write out the counter?
			if( verbose && (counter % 100000 == 0) ) {
				Rcout << "line: " << counter << std::endl;
			}

			counter++;

		}

	}

	// close the other file connections
	typedef std::map<std::string, std::ofstream*>::iterator MItr;
	for( MItr it = files.begin(); it != files.end(); it++ ) {
		it->second->close();
		delete it->second;
	}
	files.clear();

	typedef std::map<std::string, std::ostreambuf_iterator<char>*>::iterator NItr;
	for( NItr it = file_itrs.begin(); it != file_itrs.end(); it++ ) {
		delete it->second;
	}
	file_itrs.clear();

}
