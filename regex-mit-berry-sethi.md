---
patat:
  slideLevel: 2
  images:
    backend: auto
  eval:
    figlet:
      command: figlet -c -t
      fragment: false
      replace: true
    figlet-term:
      command: figlet -f term -c -t
      fragment: false
      replace: true
  pandocExtensions:
    - patat_extensions
    - tex_math_dollars
    - emoji

title: Reguläre Ausdrücke mithilfe des Berry-Sethi-Verfahrens
author: alexander.lehmann@tngtech.com
...

```figlet
RegExp mit Berry-Sethi
```
```figlet-term
Dr. Alexander Lehmann
<alexander.lehmann@tngtech.com>
4. Dezember 2020
```

# Einführung

## Reguläre Ausdrücke

- Wozu dienen reguläre Ausdrücke?
    - Suchen & Ersetzen von Text (Pattern Matching)
    - Zerlegen von Text in einzelne Bausteine (Groups)
    - Lexer, Parser (insb. Compilerbau, aber auch ganz allgemein)

>- Relativ einfache Implementierung als Endlicher Automat
   &nbsp;
>- Standards: Perl Compatible Regular Expressions (PCRE), POSIX basic & extended
>     - ... Micro$oft, JetBrains, ...
>- Was können sie bzw. was gibt es für Einschränkungen?
>     - Kleene-Stern `a*`, Konkatenation `ab` und Alternative `(a|b)` können alle&nbsp;
>     - Weiteres:
>         - Zeichenklassen `[:alnum:]`, `[:digit:]`
>         - Lookahead `q(?!u)`, `q(?=u)` / Lookbehind `q(?<!u)`, `q(?<=u)`
>         - Named Groups `(?P<name>foo)`
>         - Backwards References `\0`, `\1`, ...
>         - etc.

<!-- Lookahead and lookbehind, collectively called “lookaround”, are zero-length assertions just like
the start and end of line, and start and end of word anchors explained earlier in this tutorial. The
difference is that lookaround actually matches characters, but then gives up the match, returning
only the result: match or no match. That is why they are called “assertions”. They do not consume
characters in the string, but only assert whether a match is possible or not. Lookaround allows you
to create regular expressions that are impossible to create without them, or that would get very
longwinded without them. -->

## Beispiel Emailadressen (RFC5322)

```
(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|
"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09
\x0b\x0c\x0e-\x7f])*")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+
[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9]
[0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?
[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a
\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])
```

## Reguläre Ausdrücke

- Gehen zurück auf ~Regular Sets~ (Kleene)
- Bestehen aus Zeichen σ ∈ Σ (Alphabet) und Metazeichen für Operationen
    > - ∀ σ ∈ Σ => "σ" ist ein regulärer Ausdruck ~ ℒ(σ) = {"σ"}&nbsp;
    >   e.g. "a", "b", ...
    >   &nbsp;
    > - regex(P) ∧ regex(Q) => regex(P|Q) ~ ℒ(P|Q) = ℒ(P) ∪ ℒ(Q)
    >   e.g. ℒ("a|b") = { "a", "b" }
    >   &nbsp;
    > - regex(P) ∧ regex(Q) => regex(P∘Q) ~ ℒ(P∘Q) = { pq | p ∈ ℒ(P) ∧ q ∈ ℒ(Q) }&nbsp;
    >   e.g. ℒ("a∘b") = { "ab" }
    >   &nbsp;
    > - regex(P) => regex(P\*) ~ Kleensche Hülle L(P*)
    >   e.g. ℒ("a\*") = { ∅, "a", "aa", "aaa", ... }
- Natürlich auch mit mehr als ~einem~ Zeichen ;-)
    - "(a|b)*c"
    - Präzedenz: \*, ∘, |
    - ∘ wird üblicherweise weggelassen

## Formale Sprachen und Grammatiken

- Formale Sprachen
    - Teilmenge der Kleeneschen Hülle Σ* über einem Alphabet
    - Eher abstrakt und nicht unmittelbar auf natürliche Sprachen anwendbar
    - Bsp.: Wort vs. Satz

>- Formale Grammatiken G = (N,T,P,S)
>     - Nichtterminale N
>     - Terminale T (analog zu Σ)
>     - Produktionsregeln P: (N ∪ T)+ → (N ∪ T)*&nbsp;
>       `fooBbaz → foobarbaz`
>     - Startsymbol S ∈ N

> - Klassisches Beispiel für einfache arithmetische Ausdrücke

```
E -> E + T | E - T | T
T -> T * F | T / F | F
F -> ( E ) | number | variable
```

<!--
- Eine formale Sprache L über einem Alphabet Σ ist eine Teilmenge der Kleeneschen Hülle des Alphabets Σ*
- Eine formale Sprache über einem Alphabet Σ ist also eine bestimmte Teilmenge der Kleeneschen Hülle ihres Alphabets
- Formale Sprachen können leer, endlich oder unendlich sein.
-->

## Chomsky-Hierarchie

- Typ 0 = Rekursiv aufzählbare Grammatiken (Turing-Maschine)&nbsp;
  `α → β`
  &nbsp;
- Typ 1 = Kontextsensitive Grammatik (Turing-Maschine, linear beschränkt)&nbsp;
  `α X γ → α β γ`
  &nbsp;
- Typ 2 = Kontextfreie Grammatik (Kellerautomat)&nbsp;
  `X → α`
  &nbsp;
- Typ 3 = Reguläre Grammatik (NFA, DFA)&nbsp;
  `X → x | x Y`
  &nbsp;

# Endliche Automaten

## Deterministic Finite Automaton

- (Q, Σ , δ, q0, F) mit
    - Zustandsmenge Q
    - Alphabet Σ
    - Transitionen δ : Q x Σ → Q
    - Startzustand q0 ∈ Q
    - Endzustände F ⊆ Q
- Akzeptiert Input-Strings aus einer regulären Sprache

```
                   0
              ┌───────────┐
              ▼           │
┌────┐  0   ┌────┐  1   ┌────┐  1   ╔════╗
│ q0 │ ───▶ │ q1 │ ───▶ │ q2 │ ───▶ ║ q3 ║
└────┘      └────┘      └────┘      ╚════╝
```

> - Zustandssequenz ist klar durch die Eingabe definiert

## Non-Deterministic Finite Automaton

- (Q, Σ , δ, q0, F) mit
    - Zustandsmenge Q
    - Alphabet Σ
    - Transitionen δ : Q x Σ → P(Q)
    - Startzustand q0 ∈  Q
    - Endzustände F ⊆ Q

```
                0
              ┌───┐
              ▼   │
┌────┐  0   ┌───────┐  1   ╔════╗
│ q0 │ ───▶ │  q1   │ ───▶ ║ q2 ║
└────┘      └───────┘      ╚════╝
              ▲ 1 │
              └───┘
```

> - Akzeptiert Input-Strings, wenn es mindestens einen Pfad von q0 zu einem Endzustand gibt
> - ε-NFA erlaubt Zustandsübergänge, ohne dabei ein Zeichen zu lesen
> - In DFA transformierbar (Potentmengenkonstruktion, DFA-Minimierung)

<!-- Zu jedem ε-NFA existiert ein äquivalenter NFA ohne ε-Übergänge -->

# Berry-Sethi

## Idee und Running Example

- Für jeden regulären Ausdruck e kann in linearer Zeit ein ε-NFA erzeugt werden, der L(e)
  akzeptiert
  &nbsp;
- Regulärer Ausdruck als Baum
    - Innere Knoten als Operatoren
    - Blätter als Terminale
    - Beispiel: `(a|b)*a(a|b)b?`

## Idee und Running Example

![](aux/graph1.jpg)

## Idee und Running Example

![](aux/graph2.jpg)

## Idee und Running Example

![](aux/graph3.jpg)

## Idee und Running Example

![](aux/graph4.jpg)

## Idee und Running Example

![](aux/graph5.jpg)

## Idee und Running Example

- Regulärer Ausdruck als Baum
    - Innere Knoten als Operatoren
    - Blätter als Terminale&nbsp;
    - Beispiel: `(a|b)*a(a|b)b?`
- Blätter nummerieren, damit sich gleiche Terminale unterscheiden lassen

## Idee und Running Example

![](aux/graph6.jpg)

## Berry-Sethi Konstruktion

- Idee:
     - Token um den Baum wandern lassen
     - Nur an den Blättern wird gelesen, sonst \epsilon-Übergänge&nbsp;

## Idee und Running Example

![](aux/graph7.jpg)

## Berry-Sethi Konstruktion

- Idee:
    - Token um den Baum wandern lassen
    - Nur an den Blättern wird gelesen, sonst \epsilon-Übergänge&nbsp;
    - \epsilon-Übergänge beseitigen

- Aus dem \epsilon-NFA wird ein NFA (Q, Σ, δ, q0, F) mit
    - Alphabet Σ ~ Terminale
      &nbsp;
    - Zustandsmenge Q&nbsp;
        `Q = { *e } ∪ { *i | i ist ein Blatt }`
        &nbsp;
    - Startzustand `q0 = *e`
      &nbsp;
    - Endzustände F
        `last(e)` falls `empty(e) = false`, sonst `{ *e } ∪ last(e)`
      &nbsp;
    - Transitionen δ
      ```
        { (*e, a, i*) | i ∈ first(e) }
      ∪ { (i*, a, j*) | j ∈ next(i)  }
      ```
      &nbsp;

# Tischlein deck dich

## Wie kommt man jetzt vom Regulären Ausdruck zum Baum?

- Baum als Sum-Type

```fsharp
  type BsTree = Leaf of Lf
              | Concat of BsTree * BsTree
              | Or of BsTree * BsTree
              | Asterisk of BsTree
              | QuestionMark of BsTree

  type Lf     = Lf of int * char
              | Root
```

>- Beispiel (a|b)*

```fsharp
  Asterisk(
      Or(
          Leaf( Lf(1, 'a') ),
          Leaf( Lf(2, 'b') )))
```

## Wie kommt man jetzt vom Regulären Ausdruck zum Baum?

- Grammatik
  ```
      E -> ME | M
      M -> T* | T? | T
      T -> σ | (E|E)
  ```
> - Recursive Descent Parser / Parser Combinators

```fsharp
    let pSigma = asciiLetter <|> digit
                 |>> fun sigma -> Leaf (..., sigma)

    let pEorE  = pchar '(' >>. pExpr .>> pchar '|' .>>. pExpr .>> pchar ')'
                 |>> Or

    let pTerm  = pSigma <|> pEorE

    ...
```

## Wie kommt man jetzt vom Regulären Ausdruck zum Baum?

```fsharp
> type UserState = int

> let initialState = 1 in
  runParserOnString pExpr initialState "berry-sethi" "(a|b)*a(a|b)b?"

val it : ParserResult<BsTree<char>,UserState> =
  Success:
    Concat(
       Asterisk(
         Or(
           Leaf (Lf (1, 'a')),
           Leaf (Lf (2, 'b')))),
       Concat(
          Leaf (Lf (3, 'a')),
          Concat(
             Or(
               Leaf (Lf (4, 'a')),
               Leaf (Lf (5, 'b'))),
             QuestionMark(
               Leaf (Lf (6, 'b'))))))
```

<!-- let pSigma: Parser<BsTree<char>, int> = asciiLetter <|> digit .>>. userState ... -->

## Und wie vom Baum zum NFA?

- Kann ein Knoten leer sein? (Post-Order Traversal)

```fsharp
let rec empty node = match node with
    | Leaf _ -> false
    | Concat (l, r) -> empty l && empty r
    | Or (l, r) -> empty l || empty r
    | Asterisk _ -> true
    | QuestionMark _ -> true
```

```
             ┌───┐             ┌───┐        ┌───┐   ┌───┐
             │ ● │             │ | │        │ * │   │ ? │
             └───┘             └───┘        └───┘   └───┘
            /     \           /     \         |       |
           /       \         /       \        |       |
┌───┐   ┌───┐     ┌───┐   ┌───┐     ┌───┐   ┌───┐   ┌───┐
│ a │   │ a │     │ b │   │ a │     │ b │   │ a │   │ a │
└───┘   └───┘     └───┘   └───┘     └───┘   └───┘   └───┘
```

## Und wie vom Baum zum NFA?

- Die Menge erster Blätter (Post-Order Traversal)

```fsharp
let rec first node = match node with
    | Leaf lf -> Set.singleton lf
    | Concat (l, r) -> if empty l then Set.union (first l) (first r) else first l
    | Or (l, r) -> Set.union (first l) (first r)
    | Asterisk n -> first n
    | QuestionMark n -> first n
```

```
             ┌───┐             ┌───┐        ┌───┐   ┌───┐
             │ ● │             │ | │        │ * │   │ ? │
             └───┘             └───┘        └───┘   └───┘
            /     \           /     \         |       |
           /       \         /       \        |       |
┌───┐   ┌───┐     ┌───┐   ┌───┐     ┌───┐   ┌───┐   ┌───┐
│ a │   │ a │     │ b │   │ a │     │ b │   │ a │   │ a │
└───┘   └───┘     └───┘   └───┘     └───┘   └───┘   └───┘
```

## Und wie vom Baum zum NFA?

- Die Menge letzter Blätter (Post-Order Traversal)

```fsharp
let rec last node = match node with
    | Leaf lf -> Set.singleton lf
    | Concat (l, r) -> if empty r then Set.union (last l) (last r) else last r
    | Or (l, r) -> Set.union (last l) (last r)
    | Asterisk n -> last n
    | QuestionMark n -> last n
```

```
             ┌───┐             ┌───┐        ┌───┐   ┌───┐
             │ ● │             │ | │        │ * │   │ ? │
             └───┘             └───┘        └───┘   └───┘
            /     \           /     \         |       |
           /       \         /       \        |       |
┌───┐   ┌───┐     ┌───┐   ┌───┐     ┌───┐   ┌───┐   ┌───┐
│ a │   │ a │     │ b │   │ a │     │ b │   │ a │   │ a │
└───┘   └───┘     └───┘   └───┘     └───┘   └───┘   └───┘
```

## Und wie vom Baum zum NFA?

- Die Menge nächster Blätter (Pre-Order Traversal)

```fsharp
let next root node =

    let rec go root' acc =
        match root' with
        | _ when root' = node -> (acc, true)
        | Leaf _ -> (acc, false)
        | Concat (l, r) ->
            match go r acc with
            | (_, true) as result -> result
            | _ -> go l (Set.union (first r) (if empty r then acc else Set.empty))
        | Or (l, r) ->
            match go l acc with
            | (_, true) as result -> result
            | _ -> go r acc
        | Asterisk n -> go n (Set.union (first n) acc)
        | QuestionMark n -> go n acc

    go root Set.empty |> fun (result, _) -> result
```

## Und wie vom Baum zum NFA?

- NFA (Q, Σ , δ, q0, F)

    - Startzustand: `let q0 = Root`
      &nbsp;
    - Endzustände:
      ```fsharp
      let F = Set.union (last root)
                        (if empty root then Set.singleton Root else Set.empty)
      ```

## Und wie vom Baum zum NFA?

- NFA (Q, Σ , δ, q0, F)

    - Transitionen:
      ```
        { (*e, a, i*) | i ∈ first(e) }   // fromRoot
      ∪ { (i*, a, j*) | j ∈ next(i)  }   // fromLeaves
      ```
      ```fsharp
      // val first : BsTree -> Set<Lf>

      let fromRoot: seq<Lf<'a> * 'a * Lf<'a>> =
          first root
          |> Seq.map (fun (Lf (_, input) as target) -> (Root, input, target))
      ```

## Und wie vom Baum zum NFA?

- NFA (Q, Σ , δ, q0, F)

    - Transitionen:
      ```
        { (*e, a, i*) | i ∈ first(e) }   // fromRoot
      ∪ { (i*, a, j*) | j ∈ next(i)  }   // fromLeaves
      ```
      ```fsharp
      // val leaves : Set<BsTree>
      // val next   : BsTree -> BsTree -> Set<Lf>

      let fromLeaves: seq<Lf<'a> * 'a * Lf<'a>> =
          leaves
          |> Seq.map (fun (Leaf (Lf _ as lf) as source) -> (lf, next root source))
          |> Seq.collect (fun (source, targets) ->
               targets |> Seq.map (fun (Lf (_, s) as target) -> (source, s, target)))
      ```

## Wie akzeptiert der NFA?

```fsharp
type NFA<'State>(Delta: Map<'State * 'T, Set<'State>>,
                 q0:    'State,
                 F:     Set<'State>) =

    let Delta = Delta
    let F     = F
    let q0    = q0

    member x.Accept input =
        let rec go q = function
            | h :: t ->
                match Delta.TryFind(q, h) with
                | Some targets -> targets
                                  |> Seq.map (fun q' -> go q' t)
                                  |> Seq.fold (||) false
                | _ -> false
            | [] when F.Contains q -> true
            | _ -> false

        go q0 input
```

## Wie akzeptiert der NFA?

- Alles in einen Topf geworfen:

```fsharp
> let regex = "(a|b)*a(a|b)b?"

> let parseResult = parseRegex regex

> let nfa =
      match parseResult with
      | Success (tree, _, _) -> tree |> BsTree.toNFA
      | Failure (msg, _, _) -> failwith msg

> "aab" |> Seq.toList |> nfa.Accept

val it : bool = true    // ¯\_(ツ)_/¯

```

# Vielen Dank für Eure Aufmerksamkeit! \#

```
                                   |                         _...._
                                \  _  /                    .::o:::::.
                                 (\o/)                    .:::'''':o:.
                             ---  / \  ---                :o:_    _:::
                                  >*<                     `:}_>()<_{:'
                                 >0<@<                 @    `'//\\'`    @
                                >>>@<<*              @ #     //  \\     # @
                               >@>*<0<<<           __#_#____/'____'\____#_#__
                              >*>>@<<<@<<         [__________________________]
                             >@>>0<<<*<<@<         |=_- .-/\ /\ /\ /\--. =_-|
                            >*>>0<<@<<<@<<<        |-_= | \ \\ \\ \\ \ |-_=-|
                           >@>>*<<@<>*<<0<*<       |_=-=| / // // // / |_=-_|
             \*/          >0>>*<<@<>0><<*<@<<      |=_- |`-'`-'`-'`-'  |=_=-|
         ___\\U//___     >*>>@><0<<*>>@><*<0<<     | =_-| o          o |_==_|
         |\\ | | \\|    >@>>0<*<<0>>@<<0<<<*<@<    |=_- | !     (    ! |=-_=|
         | \\| | _(UU)_ >((*))_>0><*<0><@<<<0<*<  _|-,-=| !    ).    ! |-_-=|_
         |\ \| || / //||.*.*.*.|>>@<<*<<@>><0<<@</=-((=_| ! __(:')__ ! |=_==_-\
         |\\_|_|&&_// ||*.*.*.*|_\\db//__     (\_/)-=))-|/^\=^=^^=^=/^\| _=-_-_\
         """"|'.'.'.|~~|.*.*.*|     ____|_   =('.')=//   ,------------.
         jgs |'.'.'.|   ^^^^^^|____|>>>>>>|  ( ~~~ )/   (((((((())))))))

                                    https://asciiart.website/index.php?art=holiday/christmas/trees
```

<!-- vim:set ts=4 sw=4 expandtab tw=120 -->
