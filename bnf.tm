<TeXmacs|2.1.1>

<style|<tuple|generic|taiwanese>>

<\body>
  Body = SentAndCmt* + Return

  SentAndCmt = Sentence \| Comment

  Comment = \P#\Q CommentCont \P<inactive|<hybrid|>>n\Q

  Sentence = TypeDefinition \| NonTypeDefSent \P;\Q

  NonTypeDefSent = VarDef \| MetaExpression

  VarDef = Type VarName \P=\Q (NonLambda \| VarDefLambda)

  Pattern = \P(\P TypeName VarName+ \P)\Q \| TypeName

  NonLambda = Expression \| IfClause \| Matching

  MetaExpression = NonLambda \| Lambda

  TypeDefinition = Record \ \| Union

  Record = TypeName \P(\Q TypeName AttrName \P)\Q

  Union = UnionUnit (\P\|\Q UnionUnit )+

  UnionUnit = TypeName \| Record\ 

  TypeName = Iden

  Lambda = \Plmd\Q \P(\P TypeVarPair ( \P,\Q TypeVarPair )* \P)\Q LambdaBody

  TypeVarPair = TypeName VarName

  LambdaBody = NonTypeDefSent* Return

  Return = \Preturn\Q + (Expression \| \Pnone\Q)

  IfClause = \Pif\Q Expression \P{\Q NonTypeDefSent* \P}\Q
  \Pelse\Q<space|1em>\Q{\Q NonTypeDefSent* \P}\Q

  Matching = \Pmatch\Q VarName \P{\Q \Pas\Q Pattern \P=\<gtr\>\Q
  NonTypeDefSent+ \P}\Q

  VarName = Iden

  <em|Iden = [_0-9A-Za-z][_0-9A-Za-z]*>

  CommentCont = <inactive|<hybrid|>>s+

  Expression = +=*/. ** // % f(x)

  Expression = Base ( \P**\Q Base)*

  Base = Modulee ( (\P%\Q\|\P//\Q) Modulee)*

  Modulee = Factor (( \P*\Q \| \P/\Q ) Factor)+

  Factor = Term((\P+\Q\|\Q-\Q) Term)*

  Term = CallItem( \Q(\Q CallItem \P)\Q )?

  CallItem = \P(\Q Expression \P)\Q \| Iden \| Number \| Bool \| String

  Number = Int \| Flo

  Int = [+-]?<inactive|<hybrid|d>>+

  Flo = [+-]?<inactive|<hybrid|d>>+.<inactive|<hybrid|d>>+

  Bool = True \| False

  String = <inactive|<hybrid|">>StrCont<inactive|<hybrid|">>

  StrCont = (\<#975E\><inactive|<hybrid|>>\|2\<#500B\><inactive|<hybrid|>>)

  \;
</body>

<\initial>
  <\collection>
    <associate|page-medium|paper>
  </collection>
</initial>