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
tmon9 /i=4
echo :::::::::: destroy a monitor with living task inside ::::::::::::
tmon8
echo ::::::::::: exit monitor without an enter  :::::::::::::::::::::
tmon2
echo ::::::::::::: selfmodification of priority of a task :::::::::::
echo ::::::::::::: inside monitor, runs twice             :::::::::::
tmon4
echo ::::::::::: consumer, producer mechanism (mutex) ::::::::::::::
echo ::::::::::: is running three times ! ::::::::::::::::::::::::::
tmon3 /i=2 /t=2000
tmon3 /i=3 /t=1000
tmon3 /i=4 /t=300
echo :::::::::::: timed monitor enter for consumer, producer :::::::: 
echo :::::::::::: runs 5 times, with different testinputs :::::::::::
tmon5 /d=10000 /r=0
tmon5 /d=300 /r=400 /c=2
tmon5 /d=10000 /r=300 /c=2
tmon5 /d=4000 /r=20 
tmon5 /d=8000 /r=300 /c=2
echo ****************** MESSAGES TESTS *********************
echo ::::::::::: simple message primitives test :::::::::::::::::::::
echo ::::::::::: runs three times               :::::::::::::::::::::
tmess2 /i=3
echo ::::::::: timed receive test with several time situations ::::::
echo ::::::::: runs three times                                ::::::
echo .....................Should pass
tmess3 /i=3 /s=80 /t=10000
echo .....................Should fail with "too slow"
tmess3 /s=10000 /t=200
echo ................May pass, but time is a little tight.
tmess3 /s=300 /t=550
echo ::: Permutation:test: several tasks with all reply possibilities::
tmess5
echo :::::::::: Queing:test: all senders wait for a receive :::::::::
tmess6
echo ********************* TaskDebug Tests ********************* 
echo ::::::::::::::: tests all termination modes ::::::::::::::::::::
tdebug1 /m=1
tdebug1 /m=2
tdebug1 /m=3
echo :::::::::::tests all user interrupt modes 
tdebug2 /m=1
tdebug2 /m=2
tdebug2 /m=3
echo **************** Additional tests concerning : *****************
echo ********* Tasks, TaskInfo, TaskTime, Interrupts ****************
echo :::::::Simple hello program:::::::::
t
echo :::::::Stack overflow default stack (DEVIANT):::::::::
tstack
echo :::::::PingPong 10 times with Ready:::::::::
pingpong/n=10
echo :::::::PingPong 10 times with SetPriority:::::::::
pingpong/n=10/s
echo :::::::Interrupt a task 5 times:::::::::
tint1/n=5
echo :::::::Nested interrupt a task 5 times:::::::::
tint2/n=5
echo :::::::Nested chained interrupt a task 5 times:::::::::
tint2/n=5/chain
echo :::::::Task Information:::::::::
ttaskinf
echo :::::::Interrupted task A with a Ready of B:::::::::
inttasks
echo :::::::Interrupted task A with a SetPriority of B:::::::::
inttasks/s
echo :::::::Some Sleeping (few seconds) with a wakeup:::::::::
ttasktim 2000 0/status 10000/wakeup 4000 5000 /status
echo :::::::Object test (DEVIANT):::::::::
ttasktim /wakeup
echo :::::::Simple PingPong Message Passing:::::::::
tmess1/n=5
echo :::::::Simple PingPong Message Passing with Big Messages:::::::::
tmess1/n=5/big
echo :::::::Simple PingPong Monitor with Nested Entries:::::::::
tmon1/n=4
echo ................................................................ 
echo The monitor test is now over. For further
echo test details or options please consult    
echo the implementers or have a look at the
echo specifications at the beginning of every module
echo ................................................................ 
