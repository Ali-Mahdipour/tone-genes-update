# Script to download the latest (as of 23 April 2020) available shapshot of the World Phonotactics Database (wphon) 
# from the Internet Archive/WayBackMachine (currently dated 8 June 2019) 
# and convert it into a CSV file for downstream processing.
#
# Inspired from phonotactics.py available at https://gist.github.com/xflr6/800401204fe15a6d1b9289149725b790

import csv
import io
import json
import os
import sys

if sys.version_info.major == 2:
    from urllib import urlretrieve
else:
    from urllib.request import urlretrieve

# Please download this file manually!
# Now saved as the XZ archive wphon-latest.js.xz
URL = 'https://web.archive.org/web/20141022165943/http://phonotactics.anu.edu.au/qtp/wphon-latest.js'

ENCODING = 'utf-8'

JS = URL.rpartition('/')[2]

CSV = '%s.csv' % os.path.splitext(JS)[0]

with io.open(JS, encoding=ENCODING) as f:
    js = json.load(f)

cols, rows = [c[1] for c in js[0]], js[1:]

if not os.path.exists(CSV):
    if sys.version_info.major == 2:
        with io.open(CSV, 'wb') as f:
            writer = csv.writer(f)
            writer.writerow([c.encode(ENCODING) for c in cols])
            for r in rows:
                r = [v.encode(ENCODING) if isinstance(v, unicode) else v for v in r]
                writer.writerow(r)
    else:
        with io.open(CSV, 'w', encoding=ENCODING, newline='') as f:
            writer = csv.writer(f)
            writer.writerow(cols)
            writer.writerows(rows)


# Finally, manually XZ'ed as wphon-latest.csv.xz to save space (inside is a comma-separated double-quoted CSV file....
