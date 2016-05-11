# Huffman Compression

## Compression Ratio

A basic implementation of Huffman compression using a priority queue. Of the 5 sample text files, compression ratio averages ~78%.

## Usage

To zip: 	./zip.native [name of file]

To unzip:	./unzip.native [name of original file] [name of file to unzip] [name of unzipped file]

## TODOs

There are some problems with zipping images; right now this program works with text files. Compression ratio
can be improved by adding, for example, run-length encoding. I also don't like usage - I'd prefer to save the huffman tree
during execution of ./zip.native instead of having to recreate it, as explained above.
