
# octlimit-haskell

This program finds valid face-turning octahedra or Icosamates. For more information see the pdf [here](https://milojacquet.com/twisty/fto.pdf).

To install, clone the respository and run `stack init`. Usage is
```
stack run (octa|icosa) <n>
```
where `n` ≥ 3 is the order being searched, and use `octa` to search for face-turning octahedra and `icosa` to search for Icosamates.