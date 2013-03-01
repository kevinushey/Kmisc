#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <boost/algorithm/string.hpp>

using namespace Rcpp;

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
		int column ) {

	// space for a line, and a line post-split
	std::string line;
	std::vector< std::string > line_split;
	std::map< std::string, std::ofstream* > files;

	// input file connections
	std::ifstream conn;
	conn.open( path.c_str() );

	if( conn.is_open() ) {

		while( conn.good() ) {

			// read in a line of input
			std::getline( conn, line );

			// make sure that the line is not empty
			if( line == "" ) {
				continue;
			}

			// split it to a vector
			boost::split( line_split, line, boost::is_any_of(sep) );

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

		}

	}

	// close the other file connections
	for( std::map< std::string, std::ofstream*>::iterator itr = files.begin(); itr != files.end(); itr++ ) {
		itr->second->close();
	}

}
