#!/bin/sh
echo Deleting all files not related to Sun
rm -r conf/lg2
rm -r conf/lg3
rm -r conf/pc
rm fio/bin/*.lg2 
rm fio/bin/*.lg3 
rm fio/test.bin/*.lg2
rm fio/test.bin/*.lg3
rm fio/test_lwp.bin/*.lg2
rm fio/test_lwp.bin/*.lg3
rm fio/lwp.bin/*.lg2
rm fio/lwp.bin/*.lg3 
rm -r gen/lg2.src
rm -r gen/pc.src
rm -r gen/pc_lwp.src
rm -r gen/test_pc.src
rm -r gen/test_pc_lwp.src
rm gen/test.bin/*.lg2
rm gen/test.bin/*.lg3
rm gen/bin/*.lg2
rm gen/bin/*.lg3
rm gen/test_lwp.bin/*.lg2
rm gen/test_lwp.bin/*.lg3
rm gen/lwp.bin/*.lg2
rm gen/lwp.bin/*.lg3
rm tools/bin/*.lg2
rm tools/bin/*.lg3
rm demo/lwp.bin/*.lg2
rm demo/lwp.bin/*.lg3
rm -r io/pc.src
rm -r io/test_pc.src
rm io/bin/*.lg2
rm io/bin/*.lg3
rm io/test_lwp.bin/*.lg2
rm io/test_lwp.bin/*.lg3
rm io/test.bin/*.lg2
rm io/test.bin/*.lg3
rm io/lwp.bin/*.lg2
rm io/lwp.bin/*.lg3
