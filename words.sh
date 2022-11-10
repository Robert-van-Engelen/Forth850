#!/bin/sh
#
# ./words.sh < forth850.asm

echo '## Forth words'
echo
awk '/^; [^ \t]+\t/,/^$/ { print }' forth850.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;( |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/### \1\n_\2_/' \
      | sed -E 's/^;//' \
      | sed -E 's/</\\</g' \
      | sed -E 's/\*/\\*/g'
echo
echo '## Additional words included with the full version'
echo
awk '/^;\+ [^ \t]+\t/,/^$/ { print }' forth850.asm \
      | sed -E 's/^;    /    /' \
      | sed -E 's/^;(\+ |		)//' \
      | sed -E 's/^([^	]+)[	]+(.*)/### \1\n_\2_/' \
      | sed -E 's/^;//' \
      | sed -E 's/</\\</g' \
      | sed -E 's/\*/\\*/g'
