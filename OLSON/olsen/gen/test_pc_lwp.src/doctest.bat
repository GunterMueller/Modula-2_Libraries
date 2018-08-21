echo off
break=on
rem ******************************************************* 
rem this is a testhandler for TaskMonitors, TaskMessges and 
rem TaskDebug written by
rem Rob Nagler and Franco Monti , 30.6.87
rem ******************************************************* 
pause are you ready for the test ?
echo ****************************************************** 
echo :::::::::: simple monitor enter, exit test:::::::::::::::::::::::
echo :::::::::: runs four times ::::::::::::::::::::::::::::::::::::::
tmon9 /i=4 >>err.doc
echo :::::::::: destroy a monitor with living task inside ::::::::::::
tmon8 >>err.doc
echo ::::::::::: exit monitor without an enter  :::::::::::::::::::::
tmon2 >>err.doc
echo ::::::::::::: selfmodification of priority of a task :::::::::::
echo ::::::::::::: inside monitor, runs twice             :::::::::::
tmon4 >>err.doc
echo ::::::::::: consumer, producer mechanism (mutex) ::::::::::::::
echo ::::::::::: is running three times ! ::::::::::::::::::::::::::
tmon3 /i=2 /t=2000 >>err.doc
tmon3 /i=3 /t=1000 >>err.doc
tmon3 /i=4 /t=300 >>err.doc 
echo :::::::::::: timed monitor enter for consumer, producer :::::::: 
echo :::::::::::: runs 5 times, with different testinputs :::::::::::
tmon5 /d=10000 /r=0 >>err.doc
tmon5 /d=300 /r=400 /c=2 >>err.doc
tmon5 /d=10000 /r=300 /c=2 >>err.doc
tmon5 /d=4000 /r=20 >>err.doc 
tmon5 /d=8000 /r=300 /c=2 >>err.doc
echo ****************** MESSAGES TESTS *********************
echo ::::::::::: simple message primitives test :::::::::::::::::::::
echo ::::::::::: runs three times               :::::::::::::::::::::
tmess2 /i=3 >>err.doc
echo ::::::::: timed receive test with several time situations ::::::
echo ::::::::: runs three times                                ::::::
echo .....................Should pass
tmess3 /i=3 /s=80 /t=10000 >>err.doc
echo .....................Should fail with "too slow"
tmess3 /s=10000 /t=200 >>err.doc
echo ................May pass, but time is a little tight.
tmess3 /s=300 /t=550 >>err.doc
echo ::: Permutation:test: several tasks with all reply possibilities::
tmess5 >>err.doc
echo :::::::::: Queing:test: all senders wait for a receive :::::::::
tmess6 >>err.doc
echo ********************* TaskDebug Tests ********************* 
echo ::::::::::::::: tests all termination modes ::::::::::::::::::::
tdebug1 /m=1 >>err.doc
tdebug1 /m=2 >>err.doc 
tdebug1 /m=3 >>err.doc
echo :::::::::::tests all user interrupt modes 
tdebug2 /m=1 >>err.doc
tdebug2 /m=2 >>err.doc
tdebug2 /m=3 >>err.doc
echo **************** Additional tests concerning : *****************
echo ********* Tasks, TaskInfo, TaskTime, Interrupts ****************
echo :::::::Simple hello program:::::::::
t >>err.doc
echo :::::::Stack overflow default stack (DEVIANT):::::::::
tstack >>err.doc
echo :::::::PingPong 10 times with Ready:::::::::
pingpong/n=10 >>err.doc
echo :::::::PingPong 10 times with SetPriority:::::::::
pingpong/n=10/s >>err.doc
echo :::::::Interrupt a task 5 times:::::::::
tint1/n=5 >>err.doc
echo :::::::Nested interrupt a task 5 times:::::::::
tint2/n=5 >>err.doc
echo :::::::Nested chained interrupt a task 5 times:::::::::
tint2/n=5/chain >>err.doc
echo :::::::Task Information:::::::::
ttaskinf >>err.doc
echo :::::::Interrupted task A with a Ready of B:::::::::
inttasks >>err.doc
echo :::::::Interrupted task A with a SetPriority of B:::::::::
inttasks/s >>err.doc
echo :::::::Some Sleeping (few seconds) with a wakeup:::::::::
ttasktim 2000 0/status 10000/ready 4000 5000 /status
echo :::::::Object test (DEVIANT):::::::::
ttasktim /ready >>err.doc
echo :::::::Simple PingPong Message Passing:::::::::
tmess1/n=5 >>err.doc
echo :::::::Simple PingPong Message Passing with Big Messages:::::::::
tmess1/n=5/big >>err.doc 
echo :::::::Simple PingPong Monitor with Nested Entries:::::::::
tmon1/n=4 >>err.doc
echo ................................................................ 
echo The monitor test is now over. For further
echo test details or options please consult    
echo the implementers or have a look at the
echo specifications at the beginning of every module
echo ................................................................ 
