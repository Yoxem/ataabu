<TeXmacs|2.1.1>

<style|<tuple|generic|taiwanese>>

<\body>
  <tfrac|c:Type<rsub|a>\<in\> \<Gamma\> |\<Gamma\>
  \<vdash\>c:Type<rsub|a>><space|11em>12

  <frac| \<Gamma\> \<vdash\> c : Type<rsub|a><space|1em>\<Gamma\> , x :
  Type<rsub|a> \<vdash\> \<Delta\> |let x = c : Type<rsub|a> in \<Gamma\>
  ><space|3em>int x = c

  \;

  <frac| y : Y<space|1em>x<rsub|1> : X<rsub|1><space|1em>x<rsub|2> : X<rsub|2
  ><text-dots> x<rsub|n> : X<rsub|n>|lambda (x<rsub|1>,
  x<rsub|2>,<text-dots>,x<rsub|n>){y} : (X<rsub|1>,
  X<rsub|2>,<text-dots>,X<rsub|n>)\<rightarrow\>Y ><space|4em>(-\<gtr\> (x y)
  z) foo = lamda(x : X<rsub|> , y : Y){z}

  \;

  <frac| lambda (x<rsub|1>, x<rsub|2>,<text-dots>,x<rsub|n>){y} : (X<rsub|1>,
  X<rsub|2>,<text-dots>,X<rsub|n>)\<rightarrow\>Y<space|1em>c<rsub|1> :
  X<rsub|1><space|1em>c<rsub|2> : X<rsub|2 ><text-dots> c<rsub|n> :
  X<rsub|n>|lambda (x<rsub|1>, x<rsub|2>,<text-dots>,x<rsub|n>){y}(c<rsub|1>,
  c<rsub|2>,<text-dots>,c<rsub|n>) : Y>

  int z = (lambda(x : int, y: int){12;})(2, 3);
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>