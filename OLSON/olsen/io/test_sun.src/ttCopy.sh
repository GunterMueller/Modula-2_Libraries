#!/bin/sh
set -e
echo tCopy: Conformant
tCopy -source /usr/dict/words -dest x.$$
cmp -s /usr/dict/words x.$$
tCopy -source /usr/dict/words -dest x.$$ -overwrite
cmp -s /usr/dict/words x.$$
tCopy -source /dev/null -dest x.$$ -overwrite
cmp -s /dev/null x.$$
set +e
echo tCopy: Deviant
tCopy -source /usr/dict/words -dest x.$$
tCopy -source /usr/dict/words -dest /NotRoot
set -e
echo
echo tCopyObject: Conformant
tCopyObject < /usr/dict/words > x.$$
cmp -s /usr/dict/words x.$$
tCopyObject < /dev/null > x.$$
cmp -s /dev/null x.$$
tee y.$$ << EOF | tCopyObject > x.$$


hello
EOF
cmp -s y.$$ x.$$
rm -f x.$$ y.$$
