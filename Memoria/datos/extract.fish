#!/bin/fish

set TOTAL (wc -l ReplayIDs_pvt.csv)
set i 1

for ReplayID in (cat ReplayIDs_pvt.csv)
  echo Extracting $i / $TOTAL
  mysql -u root -e "use sc_pvt; call extractReplay($ReplayID)"
  set i (math "$i+1")
end
