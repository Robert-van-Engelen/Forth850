#!/bin/sh
#
# ./words.sh

awk '/^; [^ ]+\t/,/^$/ { print }' forth850.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;( |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/### \1\n_\2_/' \
      | sed -E 's/^;//'
echo
echo '## Additional words included with the full version'
echo
awk '/^;\+ [^ ]+\t/,/^$/ { print }' forth850.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\+ |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/### \1\n_\2_/' \
      | sed -E 's/^;//'
echo
echo '## Floating point math words included with the full version'
echo
echo 'Floating point values are doubles on the stack.  Double words, such as 2DUP,'
echo 'can be used to manipulate floats.  Floats can be stored in 2CONSTANT, 2VARIABLE'
echo 'and 2VALUE assignments with TO (but not with +TO.)'
echo
echo 'Beware that HEX prevents inputting floats and garbles the output of floats.'
echo
awk '/^;= [^ ]+\t/,/^$/ { print }' forth850.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(= |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/### \1\n_\2_/' \
      | sed -E 's/^;//'
