p←⍎¨⊃⎕nget'input.txt'1
-+/2-⌊p÷3
+/⊃+/{({0⌈2-⍨⌊⍵÷3}⍣⍵)p}¨⍳10
