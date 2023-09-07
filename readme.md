# Birb

Birb is an *advanced* programming language that only consists of bird
emojis 🐣. Each emoji gets substituted by a [*combinator
bird*](https://www.angelfire.com/tx4/cus/combinator/birds.html) of pure
lambda calculus.

## Birbs

Unfortunately, the Unicode standard does not yet have many
(single-character) birds. These are the ones currently mapped/supported:

| emoji | animal         | combinator   | bruijn              | term                               |
|:-----:|----------------|--------------|---------------------|------------------------------------|
|  🦉   | owl            | owl          | `[[0(10)]]`         | $\lambda ab.b(ab)$                 |
|  🦅   | eagle          | eagle        | `[[[[[43(210)]]]]]` | $\lambda abcde.ab(cde)$            |
|  🪽   | wing           | phoenix      | `[[[[3(20)(10)]]]]` | $\lambda abcd.a(bd)(cd)$           |
|  🕊️   | dove           | dove         | `[[[[32(10)]]]]`    | $\lambda abcd.ab(cd)$              |
|  🦜   | parrot         | mockingbird  | `[00]`              | $\lambda a.aa$                     |
|  🦆   | duck           | quacky       | `[[[0(12)]]]`       | $\lambda abc.c(ba)$                |
|  🐤   | touring chick  | turing       | `[[0(110)]]`        | $\lambda ab.b(aab)$                |
|  🐥   | kool chick     | kestrel      | `[[1]]`             | $\lambda ab.a$                     |
|  🐣   | hatching chick | quirky       | `[[[0(21)]]]`       | $\lambda abc.c(ab)$                |
|  🐦   | simple bird    | identity     | `[0]`               | $\lambda a.a$                      |
|  🦚   | peacock        | queer        | `[[[1(20)]]]`       | $\lambda abc.b(ac)$                |
|  🦤   | dodo           | sage         | `[[0(11)][0(11)]]`  | $\lambda ab.b(aa)\lambda ab.b(aa)$ |
|  🐧   | penguin        | blackbird    | `[[[2(10)]]]`       | $\lambda abc.a(bc)$                |
|  🦢   | swan           | substitution | `[[[20(10)]]]`      | $\lambda abc.ac(bc)$               |
|  🦩   | flamingo       | cardinal     | `[[[201]]]`         | $\lambda abc.acb$                  |

Lonely/unmatched birbs: 🐔🦃🐓

# Syntax

- `[birb]+`: Birb
- everything else: Comment

# Semantics

Birbs stagger as they walk: they are reduced in alternating associative
order, starting with birb index $\lfloor\frac{\texttt{len}}{2}\rfloor$:

    🐦🐦 -> (🐦🐦)
    🐦🐦🐦 -> ((🐦🐦)🐦)
    🐦🐦🐦🐦 -> (🐦((🐦🐦)🐦))
    🐦🐦🐦🐦🐦 -> ((🐦((🐦🐦)🐦))🐦)
    🐦🐦🐦🐦🐦🐦 -> (🐦((🐦((🐦🐦)🐦))🐦))
    🐦🐦🐦🐦🐦🐦🐦 -> ((🐦((🐦((🐦🐦)🐦))🐦))🐦)

# Examples

You can find more examples in the `samples/` directory.

## Relationships

- 🦉🐦 $\rightsquigarrow$ 🦜
- 🦢🐦 $\rightsquigarrow$ 🦉
- 🐧🐧 $\rightsquigarrow$ 🕊️
- 🦩🐧 $\rightsquigarrow$ 🦚
- 🦩🦚 $\rightsquigarrow$ 🐧

One can only imagine what happens if two parrots repeat each other: 🦜🦜
$\rightsquigarrow$ 💥

## Arithmetic

For this example I use the Church numerals. Zero would then be encoded
as 🐥🐦. The successor function can be written as 🦢🐧:

- 🐦🐧🐦🦢🐧🐥🐦 $\rightsquigarrow$ λλ(10) – (Church numeral 1)
- 🐦🐧🐦🐧🕊️🦢🐧🦢🐧🐥🐦 $\rightsquigarrow$ λλ(1(10)) – (Church numeral
  2)  

Similarly, one can very obviously translate the Church addition function
to 🐧🦢🐥🦢🕊️. Now, to calculate $1+2$ based on their increments from
zero:

- 🦢🐧🦢🐧🐥🐦

Also: 🐧 is $a\cdot b$, 🦜 is $n^n$ and 🦚🐦 $a^b$.

Note that there are many alternative ways to do arithmetic. Try writing
the functions above with other birbs!

## Busy ~~beavers~~ birbs

- The touring eagle: 🐦🐦🐦🦅🐤🦅🐤🦅🐤 (~20M BLC bits)
- more? PR!

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

The language is probably Turing complete. If the language supported
left- *and* right-associativity, 🐧 would be enough to prove its
completeness.

------------------------------------------------------------------------

The idea of the language was originally proposed in 2021 by an
unknown(?) author on [Esolang](https://esolangs.org/wiki/Birb).
