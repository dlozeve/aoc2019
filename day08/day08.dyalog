 p←100 6 25⍴⍎¨⊃⊃⎕nget'input.txt'1
 x←,(⊃⍋+/+/0=p)⌷p
 (+/1=x)×+/2=x ⍝ part 1
 ' ⌺'[{1⌷⍋⍵⍳0 1}¨⊂[1]p] ⍝ part 2
