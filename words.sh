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
