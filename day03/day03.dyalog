 ⎕io←0
 p←','(≠⊆⊢)¨⊃⎕nget'input.txt'1
 segment←{x y←⍺ ⋄ 'R'=⊃⍵:{x y+⍵}¨1↓,⍳(1+⍎1↓⍵),1 ⋄ 'L'=⊃⍵:{x y+⍵}¨1↓,-⍳(1+⍎1↓⍵),1 ⋄ 'D'=⊃⍵:{x y+⍵}¨1↓,-⍳1,1+⍎1↓⍵ ⋄ 'U'=⊃⍵:{x y+⍵}¨1↓,⍳1,1+⍎1↓⍵}
 path←{x←⊂0 0 ⋄ 1↓x⊣{x,←(⊃⌽x) segment ⍵}¨⍵}
 ⌊/+/↑(path ⊃0⌷p) ∩ path ⊃1⌷p ⍝ part 1
 c←(path ⊃0⌷p)∩path ⊃1⌷p
 2+⌊/((path ⊃0⌷p)⍳c)+(path ⊃1⌷p)⍳c ⍝ part 2
