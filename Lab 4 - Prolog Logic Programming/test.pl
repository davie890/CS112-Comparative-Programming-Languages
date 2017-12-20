test(StartName, StartName) :-
   format('Error: Cannot Fly To Same Location %s \n', [StartName]),
   !, fail.

newLineTest(x) :-
   write('Error: Input Does Not Exist.\n'), !, fail.