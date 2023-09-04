# Birb

| bird | term                      | birb        | bird             |
|:-----|:--------------------------|:------------|------------------|
| 🪶   | c `[[[2 0 1]]]`           | cardinal    | feather          |
| 🐦   | i `[0]`                   | idiot       | bird             |
| 🕊️   | d `[[[[3 2 (1 0)]]]]`     | dove        | dove             |
| 🐤   | t `[[0 (1 1 0)]]`         | turing      | baby chick       |
| 🐥   | q’’’ `[[[0 (1 2)]]]`      | quacky      | front baby chick |
| 🐣   | Ω `[0 0] [0 0]`           | omega       | hatching chick   |
| 🦉   | o `[[0 (1 0)]]`           | owl         | owl              |
| 🐔   | m `[0 0]`                 | mockingbird | chicken          |
| 🦆   | k `[[1]]`                 | kestrel     | duck             |
| 🦤   | t `[[0 1]]`               | thrush      | dodo             |
| 🦩   | y `[[1 (0 0)] [1 (0 0)]]` | sage        | flamingo         |
| 🦢   | s `[[[2 0 (1 0)]]]`       | starling    | swan             |
| 🪽   | φ `[[[[3 (2 0) (1 0)]]]]` | phoenix     | wing             |
| 🦃   | w `[[1 0 0]]`             | warbler     | turkey           |
| 🐓   | f `[[[0 1 2]]]`           | finch       | rooster          |
| 🦚   | q `[[[1 (2 0)]]]`         | queer       | peacock          |
| 🦜   | b `[[[2 (1 0)]]]`         | bluebird    | parrot           |
| 🦅   | e `[[[[[4 3 (2 1 0)]]]]]` | eagle       | eagle            |
| 🐧   | ι `[0 s k]`               | iota        | penguin          |

The mappings and their paired reduction are *not* biologically accurate!
Unfortunately the Unicode team does not have enough bird emojis. Create
a PR to improve biological accuracy.

# Syntax

- `[birb]+`: Birb (left-associative)
- everything else: Comment

# Examples

## Biology

- 🐔🐔 $\rightsquigarrow$ 🐣
- 🪶🦜 $\rightsquigarrow$ 🦚
- 🦢🐦 $\rightsquigarrow$ 🦉
- 🦢🦆🦆 $\rightsquigarrow$ 🐦
- 🦢🐦🐦 $\rightsquigarrow$ 🐔
- 🦜🐤 $\rightsquigarrow$ 🐥
- 🦜🦜 $\rightsquigarrow$ 🕊️

## Arithmetic

## Busy ~~beavers~~ birbs

# Turing-completeness

The language is probably Turing complete. If the language supported
left- *and* right-associativity, 🐧 would be enough to prove its
completeness.
