#include <iostream>
#include <fstream>
#include <Rcpp.h>
using namespace Rcpp;

inline std::string get_item( std::string& line, std::string& delim, int column ) {

	char* line_cast = const_cast<char*>( line.c_str() );
	const char* pch = strtok(line_cast, delim.c_str());
	int counter = 0;
	while( true ) {
		if( counter == column-1 ) {
			return( std::string(pch) );
		}
		pch = strtok(NULL, delim.c_str());
		counter++;
	}
	stop( "get_line is broken" );
	return( "get_line is broken" );
}

inline bool in( std::string& item, std::vector< std::string >& list ) {
	if( std::find( list.begin(), list.end(), item ) != list.end() ) {
		return true;
	} else {
		return false;
	}
}

// [[Rcpp::export]]
void extract_rows_from_file(
		std::string input_file_name,
		std::string output_file_name,
		std::string delim,
		std::vector< std::string > items_to_keep,
		int column_to_check ) {

	std::string line;
	std::string line_copy;
	std::string item_to_check;
	std::string end_line = "\n";

	std::ifstream conn;
	conn.open( input_file_name.c_str() );

	std::ofstream out_conn( output_file_name.c_str() );
	std::ostreambuf_iterator<char> out_itr( out_conn );

	if( !out_conn.is_open() ) {
		stop("Couldn't open the output file!");
	}

	if( conn.is_open() ) {
		// Rcout << "Successfully opened file." << std::endl;
		while( std::getline(conn, line) ) {

			// copy the string
			line_copy = line.c_str();
			item_to_check = get_item( line_copy, delim, column_to_check );
			// Rcout << "The item we're checking is: " << item_to_check << std::endl;
			if( in( item_to_check, items_to_keep ) ) {
				// Rcout << "Copying line" << std::endl;
				std::copy( line.begin(), line.end(), out_itr );
				std::copy( end_line.begin(), end_line.end(), out_itr );
			}
		}
	} else {
		stop("Couldn't open File!\nInput file path: " + input_file_name);
	}

	conn.close();
	out_conn.close();

}
