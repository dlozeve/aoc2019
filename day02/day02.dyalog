 ⎕io←0
 p←⍎¨','(≠⊆⊢)⊃⊃⎕nget'input.txt'1
 f←{i←0 ⊣ s[1 2]←⍵ ⊣ s←p ⋄ ⊃s⊣{i+←4 ⊣ a x y z←s[i+⍳4] ⋄ s[z]←(a-1)⌷s[x](+,×)s[y]}⍣{99=i⌷s}0}
 f 12 2 ⍝ part 1
 nv←1+,⍳99 99 ⋄ +/100 1×⊃nv[(19690720=f¨nv)⍳1] ⍝ part 2
