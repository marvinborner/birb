# Birb

Birb is an *advanced* programming language that only consists of bird
emojis 🐣. Each emoji gets substituted by a [*combinator
bird*](https://www.angelfire.com/tx4/cus/combinator/birds.html) of pure
lambda calculus.

## Birbs

Unfortunately, the Unicode standard does not yet have many
(single-character) birds. These are the ones currently mapped/supported:

| emoji | animal         | combinator   | term                               |
|:-----:|----------------|--------------|------------------------------------|
|  🦉   | owl            | owl          | $\lambda ab.b(ab)$                 |
|  🦅   | eagle          | eagle        | $\lambda abcde.ab(cde)$            |
|  🪽   | wing           | phoenix      | $\lambda abcd.a(bd)(cd)$           |
|  🕊️   | dove           | dove         | $\lambda abcd.ab(cd)$              |
|  🦜   | parrot         | mockingbird  | $\lambda a.aa$                     |
|  🦆   | duck           | quacky       | $\lambda abc.c(ba)$                |
|  🐤   | touring chick  | turing       | $\lambda ab.b(aab)$                |
|  🐥   | kool chick     | kestrel      | $\lambda ab.a$                     |
|  🐣   | hatching chick | quirky       | $\lambda abc.c(ab)$                |
|  🐦   | simple bird    | identity     | $\lambda a.a$                      |
|  🦚   | peacock        | queer        | $\lambda abc.b(ac)$                |
|  🦤   | dodo           | sage         | $\lambda ab.a(bb)\lambda ab.a(bb)$ |
|  🐧   | penguin        | blackbird    | $\lambda abc.a(bc)$                |
|  🦢   | swan           | substitution | $\lambda abc.ac(bc)$               |
|  🦩   | flamingo       | cardinal     | $\lambda abc.acb$                  |

Lonely/unmatched birbs: 🐔🦃🐓

# Syntax

- `[birb]+`: Birb
- everything else: Comment

Syntax errors are impossible as long as you use at least one birb.

# Semantics

Birbs stagger as they walk: they are reduced in alternating associative
order, starting with left associativity at birb index
$\lfloor\frac{\texttt{len}}{2}\rfloor$:

    🐦🐦 -> (🐦🐦)
    🐦🐦🐦 -> ((🐦🐦)🐦)
    🐦🐦🐦🐦 -> (🐦((🐦🐦)🐦))
    🐦🐦🐦🐦🐦 -> ((🐦((🐦🐦)🐦))🐦)
    🐦🐦🐦🐦🐦🐦 -> (🐦((🐦((🐦🐦)🐦))🐦))
    🐦🐦🐦🐦🐦🐦🐦 -> ((🐦((🐦((🐦🐦)🐦))🐦))🐦)
    ...

# Examples

You can find more examples (with comments) in the `samples/` directory.

## Relationships

- 🪽🐦 $\rightsquigarrow$ 🦢
- 🦢🐦 $\rightsquigarrow$ 🦉
- 🦉🐦 $\rightsquigarrow$ 🦜
- 🕊️🐦 $\rightsquigarrow$ 🐧
- 🐧🐧 $\rightsquigarrow$ 🕊️
- 🦩🐧 $\rightsquigarrow$ 🦚
- 🦩🦚 $\rightsquigarrow$ 🐧
- 🦩🦆 $\rightsquigarrow$ 🐣

One can only imagine what happens if two parrots talk to each other:
🦜🦜 $\rightsquigarrow$ 💥. The same happens with 🐤🐤; they just can’t
stop waddling!

## Arithmetic

For this example I use the Church numerals. Zero would then be encoded
as 🐥🐦. The successor function can be written as 🦢🐧:

- 🐦🐧🐦🦢🐧🐥🐦 $\rightsquigarrow\lambda\lambda(10)$ – (Church numeral
  1)  
- 🐦🐧🐦🐧🕊️🦢🐧🦢🐧🐥🐦 $\rightsquigarrow\lambda(1(10))$ – (Church
  numeral 2)

Similarly, one can very obviously translate the Church addition function
to 🪽🐧. Now, to calculate $1+2$ based on their increments from zero:

- 🐦🐦🕊️🐧🕊️🐧🐦🐧🕊️🐧🕊️🪽🐧🦢🐧🦢🐧🐥🐦🦢🐧🐥🐦
  $\rightsquigarrow\lambda(1(1(10)))$ – (Church numeral 3)

Also: 🐧 is $a\cdot b$, 🦜 is $n^n$ and 🦚🐦 $a^b$.

Note that there exist many alternative ways to do arithmetic. Try
writing the functions above with other birbs!

## Containers

You can create a pair $\langle X,Y\rangle$ using `🦩🦩🦩YX`.

Typically, one would now construct a list using repeated application of
pairs (Boehm-Berarducci/Church encoding). However, due to the reversed
application and alternating associativity, the Mogensen-Scott encoding
is more suitable:

List $\langle X_1,X_2,\dots,X_n\rangle$: `[🦩]ⁿ🦩X2X1...XN`.

## Busy ~~beavers~~ birbs

Contestants:

- *The touring eagle*: `[🐦]ⁿ[🦅🐤]ⁿ` ($n=3$: 9 birbs, ~20M BLC bits)
- better? PR!

# Usage

Install [Haskell’s
stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/).
Then,

- `stack run -- file.birb` or `stack run <(echo 🐧🐧)`
- `stack install` so you can enjoy `birb` from anywhere

If the output cannot be translated to birbs, the raw lambda calculus
term (with De Bruijn indices) is printed. For the examples above I
sometimes manually converted the term back to birbs.

# Turing-completeness

Birb is Turing complete.

It turns out that even its sub-language $\Sigma=\{🦢🐥\}$ (SK) is Turing
complete, since the semantics allow an initial construction of 🐦 using
`((🦢 🐥) 🐥)`. By doing that, birb is equivalent to the
[Jot](https://esolangs.org/wiki/Jot) variant of Iota calculus.

------------------------------------------------------------------------

The idea of the language was originally proposed in 2021 by @SCKelement
on [Esolang](https://esolangs.org/wiki/Birb).
