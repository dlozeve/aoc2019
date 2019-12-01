p←⍎¨⊃⎕nget'input.txt'1
f←{0⌈⌊2-⍨⍵÷3}
+/f p ⍝ part 1
+/,↑{(f⍣⍵)p}¨⍳10 ⍝ part 2
