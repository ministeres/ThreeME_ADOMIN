import csv
import sys

if len(sys.argv) > 1:
    infile = sys.argv[1]
    outfile = sys.argv[2]
else:
    infile = 'all.csv'
    outfile = 'all.csv'

with open(infile) as f:
    reader = csv.reader(f)
    cols = []
    for row in reader:
        cols.append(row)

with open(outfile, 'wb') as f:
    writer = csv.writer(f)
    for i in xrange(len(max(cols, key=len))):
        writer.writerow([(c[i] if i<len(c) else '') for c in cols])
