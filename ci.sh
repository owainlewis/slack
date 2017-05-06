echo 'Formating source code'

find src -name \*.hs -exec hindent {} \;
find test -name \*.hs -exec hindent {} \;
