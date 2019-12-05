 ⎕io←0
 a b←245182 790572
 p←⊃(,(∘.,))/6/⊂⍳10
 x←{(1↓⍵)-¯1↓⍵}¨p
 n←10⊥¨((∨/¨0=x)∧∧/¨0≤x)/p
 ≢((n>a)∧n<b)/n ⍝ part 1
 n2←10⊥¨(({∨/2=⍵}¨(⊢∘≢⌸)¨p)∧∧/¨0≤x)/p
 ≢((n2>a)∧n2<b)/n2 ⍝ part 2

 ⍝ better version (inspired from
 ⍝ https://github.com/jayfoad/aoc2019apl/blob/master/p4.dyalog)
 p←⍉10⊥⍣¯1⊢a+⍳1+b-a
 +/({∧/2≤/⍵}∧{∨/2=/⍵})p ⍝ part 1
 +/({∧/2≤/⍵}∧{∨/¨2=(⊢∘≢⌸)¨↓⍵})p ⍝ part 2