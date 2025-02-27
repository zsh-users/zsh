#autoload

_arguments -S \
  '(-h --help)'{-h,--help}'[show a help message and exit]' \
  '--sort-keys[sort the output of dictionaries alphabetically by key]' \
  '--no-ensure-ascii[disable escaping of non-ASCII characters]' \
  '--json-lines[parse input using the JSON Lines format]' \
  '--indent=[separate items with newlines and use this number of spaces for indentation]:spaces:' \
  '--tab[separate items with newlines and use tabs for indentation]' \
  '--no-indent[separate items with spaces rather than newlines]' \
  '--compact[suppress all whitespace separation]' \
  '1:infile:_files' \
  '2:outfile:_files'
