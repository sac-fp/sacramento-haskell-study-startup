# Extra Resources for Week N (title)
*(Haskell Book, Chapters NNN)*

## Epigraph sources

- Chapter N
  - [placeholder](url)

## Miscellaneous

```haskell
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q []     = q : []
scanl f q (x:xs) = scanl f (f q x) xs


fibs = 1 : scanl (+) 1 fibs
                  f  q
       x   ___xs___________

fibs = 1 : 1 : scanl (+) (1+1) fibs
                      f  __q__
           x : __xs________________

fibs = 1 : 1 : 2 : scanl (+) (2+1) fibs
                          f  __q__
               x   __xs________________

fibs = 1 : 1 : 2 : 5 : scanl (+) (2+1) fibs
                              f  __q__
                   x   __xs________________

fibs = 1 : 1 : 2 : 5 : 8 : scanl (+) (2+1) fibs
                                  f  __q__
                       x   __xs________________
```

- [placeholder](url)
