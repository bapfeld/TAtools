import os
import glob
from bs4 import BeautifulSoup

print "This code takes a folder of text submissions downloaded from Canvas and combines all output into a single file.  This is a rudimentary function and will produce a duplicated title line based on the html output from Canvas."
print "Please enter the path to the folder of submission. \n Note: you must enter an absolute path (e.g. /Users/yourusername/Desktop/submissions on a mac)."
path = str(raw_input("Path:"))
print "Output file will be placed in working directory.  Please choose a name for the output file."
output_file = str(raw_input("Output filename:"))
output_file += ".txt"

for infile in glob.glob(os.path.join(path, "*.html")):
	markup = (infile)
	soup = BeautifulSoup(open(markup, "r").read(), "html.parser")
	with open(output_file, "a") as myfile:
		text = soup.get_text()
		myfile.write(text.encode('ascii', 'ignore'))
		myfile.close()