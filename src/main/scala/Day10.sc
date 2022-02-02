import scala.annotation.tailrec
import scala.collection.mutable

val rawInput="""(<[<[((<[<([<<[]()>{{}()}>({<>[]}<[][]>)])<<[{{}[]}[{}<>]]([()<>][()()])>>>]((<{{([]{})<[]<>>}(<[
               |<[{({[[<{[{{{<<><>><{}>}<([]()){()[]}>}({(()()){{}<>}})}{([<()()>[<><>]><<{}[]>({}<>)>)}]{{[((<>()){<>[]
               |[[{[[(<((<[<<{{}()}<()[]>>(<<>{}>{{}()})>]<<[[{}()](()<>)][<<>{}><[]()>]>{((<>()){{}()))}>>[{[<[<>{}
               |{{((({({(<[({<<>()>([]<>)}({()[]}(<>{}))}[({{}{}}([]()))<({}[])[[]()]>]](<{([]<>)(<><>)}{[[]]
               |[[({[{(((<(<{({}[])}((()<>)([]<>))>({{[]<>}{{}[]}}{[<>[]][()()]})){(<{{}[]}{{}[]}><([][])[<>{}]>)}><<{{{()}[
               |(<{([{<[([(<({{}{}}{<>()})[<{}<>>{()()}]><((<>}(()<>))>){<[[()[]](()[])]>[{<{}[]>(<>())}]}](<[({(
               |<{{{<({[{<<[<({}()){[][]}>[<{}{}>[{}<>]]][[(()<>)([][])]]>><{<[(<><>)]>{(<()[]><[]{}>)<<<><>>(<>[]
               |({{{{<<[{([(({{}{}})<[[]<>]>)])}]>>}[(<[<<<{[{{}()}]<[{}{}]>}>>{(({(<>())<()()>}(<[][]>{()()})))}){[<<([()
               |<(([{(<{{{{([({}{})(<>[])][[()[]][<>{}]])<[<()[]>]}}{<<{{}<>}><{<>}([][])>>{<{{}{}}<{}<>>><((){})
               |<(<<[([<[<({{{<>[]}<()[]>}}[([()<>]([]()))(({}<>){[]{}})])>]{({[([<>()])]})}]{[[<(<{()<>}[{}<>]>{[()[]][<><>]
               |{[[[((<(<[<(<[()()]>)<<<{}<>><<>[]>>(<(){}>)>>](<([<(){}><[]()>](([])))[<{{}{}}{<><>}>{<<>>{<>
               |[<{([{<[<<<[({[][]}<{}[]>){(()<>)<{}<>>}]><{{{[]<>}<()<>>}[(()[])[{}<>]]}(({<>[]}<{}()>)(<{}[]>([]{}))
               |{<[<[<<[<[{[{(()<>)}(<<>{}>[<>()])](([<>[]](<>[]))[[[]<>]([]<>)])}]><{{<{{[]<>}{{}[]]}<[<><>
               |[((<<{<[{<{(<[[]{}]>)}<<({()[]}[[]])<(()<>)[(){}]>>(<[{}<>]{<>[]}>)>>{[<<{()<>}]<[[]<>]({}{})>>[<[(
               |<<<<{[<(<[[<<{()[]}({}<>)>{{[]<>}{[]()}}>](([{()}]([(){}][<>[]])))]{[(<(()[])[<>[]]>{([]())}){(
               |({<[({([{[(<[{<>{}}(<>[])][<()()><()<>>]>)]}({<<<{[]{}}[<><>]>>><{((<><>))<<<>[]>{<>[]}}}([(()<>)<<>{
               |<(<({{[<[{<[[{()[]}]((<>()))]<[[()[]](<><>)][[{}{}]<()>]>>}]>]{<(({<((())[<>{}])[<[]<>>{<>{}}]>}<<{[<>[]]
               |{[([{((<({<{[{[][]}<[]{}>]{{<>{}}<{}[]>}}>[<{(<>[]){[]{}}}<{[][]}{{}}>>]}{((<[[]()]<[]>>)[[<()[]>
               |(<{(({[(<{{(({<>[]}[{}])[[()()][[]()]])}[{(<()()>{[]<>})(<(){}><[]()})}[(([]{})<<><>>)<(<>())<<>
               |<(<{<({<((<{(({}{})><(<>[])>}>[<{<<>{}>([]{})}{[<>{}]([]())}>([[()[]]<{}<>>]({[][]}<()[]>))]){<[(<{}<>
               |([{({([({([[[{[]{}}({}<>)]]((<{}()><<>{}>){({}[])<{}<>>})]>}{[({{(()<>){<>[]}}[<()[]>[()<>]]}<<<<
               |{(<<[{(<<<(<<{[]<>}<[]()>><(<>)(()[])>><{(()<>){{}<>}}>)({{(<>())[<>[]]}}<[[{}{}](()())]<{{}()}(
               |<<(<<{[{([{([[<>()][[]()]](<()<>>[[][]])))<(<<[]<>>>)[({<>[]})]>][<<<(<><>)(<>)>[([]{})]>>])[[{(<(<>()){[]
               |<<<{[(<(<[(([{{}}]((<><>)[{}<>])))<({{<>{}}[{}<>]})>]{<[<(<><>)[{}{}]>[[{}[]]({}{})]]{<[[]<>
               |<{(({{{[[[<([{{}[]}]({(){}}((){})))>]]<(({[{<>()}({}{})]})(((<(){}>[(){}])[{[]{}}(<><>)]))
               |<[[{((<{([(<<{()<>}(()[])><<[][]>>>{(<{}[]>({}<>))})<<<<()()>{[]()}>(<<>()>)>>])}>))}[<<{[<{{{{<[]<>>
               |({(<{<(([[({{{[]{}}[()<>])}{(((){})<()[]>)})[<([[][]][[]()]){{<>()}[[][]]}>(<(<>)<<>[]>>{[<>[]]{[](
               |((({(<<({<[[[{<>}[<>()]]<<{}<>>({}[])>]]<({<{}{}>[()[]]}{<{}<>>{<>()}}){[(()())<{}[]>][([]{})[[]<>]]}>>[{[<((
               |{[[<[([{{<[<[[<>()][{}[]]]{<<>()>{[]()}}>]{[<[<><>]<<>{}>><{<><>}<<>{}>>)[{<{}<>><<>[]>}{{<><>}}]}>{
               |<(<{(({([<[([([][])(()<>)]{[<><>](()<>)})((([]{})[[]()]))]([({[]()}{<>{}}){{{}})])>]{<<({<<>[]
               |<<{([{<[{{(({{{}()}{[][]}}<{[]()}{<><>}>){{<()[])<<><>>}[<{}[]><{}[]>]}){<<{[]()}<<><>>>{<{}{}>({}(
               |<[[<<(<({<([((()<>){()[]})[([][])<{}[]>]]({<<>()>((){})}([[]()]({}<>})))>})[<<{({([]())<<>{}>}{{()
               |<{(<[[<(<<({(<[][]><<>{}>)[<{}{}>[()<>]]}(({()[]}<{}[]>)))>[<[{(<>{}){()<>]}<[{}[]][<><>]>]>]>[<[({<
               |(<(([{{[<(<[<{()}{[][]}>]>{(((<>())[{}{}])<[[]]<(){}>))})>](<[(<(<[][]><<>()>){<{}[]>([]{})}>{(([]))<([]<>)[(
               |{((<{(({({<[{<{}<>><<>[]>}((<>{})[[][]]]]{([()[]](()()))([[]<>]{{}()})}>})(<[<<<[]{}><()[]>><<{}<>><<
               |{<{(<[<{[[{{{[{}()]}({()()})}((([]<>){()<>})(<<><>>))}<[{{()()}{[]()}}]>]<[((<[]()>((){}))([()()][<>
               |([({((([<({[[[[]()](()<>)][({}())<<><>>]]}<{<[{}<>][{}{}]>(([]<>){[]()})}[<<[]()>[{}()]>]>)<([([()
               |<<{({<<[{[<{<(<><>)([])>({{}[]}{[][]})}<([()()])(([]{}))>>([(<[][]>{{}<>})[[<>{}]<()<>>}]{[{{}[]}({}<>)]<[[]<
               |([<{[[[([[<[(((){})[{}()]){{[]<>}(()<>}}][({(){}}{<>[]})]>]])({<[{<(()<>)>({[]{}}<{}()>)}(([()()]<<><>>
               |([({{[<([[{<[{()[]}[<>]]({<>()})><{({}[])(()<>)}{(<>{}){{}<>}}>}]])>]<([([[{[[<>{}]{[]{}}]{<[][]]{{}[]}
               |[[{(({[{{[[[<(()[])[[]<>]>]{<{()[]}{()()}>{(<>[])({}[])}}](<((<><>)[<><>])([<><>])))][[[(<(
               |[{(<<{(((([[<<{}<>>({}())><<<>[]>[{}{}]>][<(<>[])<{}>>[<[][]>{{}[]}]]]<<(<[][]>{[]()})({{}()}<{}<>>)><([<
               |{<[[<{{<[[(<{{()()}<[]<>>}><<{<>}<{}{}>><<[][]>>>)<{{[()<>]{{}{}}}<({}[])[(){}]>}>]]>}<<(({{(((){}
               |((<([[<{{<(<([<>()]{(){}}]({[]()}<()()>)>(([{}[]]((){}))))<[{[<>{}]({}<>)}]>>}[[<({(<>())(<><>)}){([<><>][[][
               |<[([<(({<(([<({}<>)[()]><<[][]><<>()>>]<{([]{})}<{<>{}}{[]()}>>)){[[{[<>{}][[][]]}]<[[<>()][{}[]]]>][<[[()
               |((({({[[[[{{<(<><>)((){})>}}]{{[[[<>[]][<>[]]]{{()<>}[[][]]}]{{{{}{}}{<>[]}}([{}][<><>])}}({(
               |<<<{[<(<({[{[({}[]){()()}]}]}{[(({{}()}[[]<>])[([]<>)(<>)])]{{({()<>}{<><>})<<{}()>>}<[<[]{}>]{([]<
               |(({[(<<<<{((((<>{}){[]<>})[{[]<>}])(([[][]]<()<>>)<[{}<>]<[]{}>>)){(({()[]}<[][]>)[[<>[]]{()<>}])([[()[]]]<
               |<([<<[<<[{[[<<[][]>>{((){})[<>[]]}]((<{}{}>[[]<>]){<{}()>(()<>]})]({[(<>{})(<><>)](<(){}>[
               |((<{[{[[{<<((<<>{}>{<>()}){{[]()}((){})})((([]<>)<<>[]>)<(()){<>[]}>}><[<(<>[])<{}()>>]{<<
               |[(<[{([{<{([[(()){(){}>]{{{}<>}<[]()>}][(<()<>>[(){}])<[()](()<>)>])}({([[[]{}]({}())]({()<>}{
               |[<[{[<[{[[<[{([]){{}{}}}]>][[<<(<>{})<<>{}]><{{}()}>>(([{}<>]({}))[(<>{})<[][]>])]]]}]<[(({
               |(([{{<<{<[[(((<>())<[]{}>)([()()}(()())))[((<>()))<[{}()]({}[])>]](<{{<>}{{}<>}}>)]>[{<{{[
               |<<[[{<([<<<<{({}<>]([]())}({{}()}<()<>>)>[<<()()>{()()}>({()<>}[{}()])]>{<(<[][]>)>[<[{}{}]>((<>()
               |(<{[<[({{[[([<[]{}>[[]()]]{{{}[]}(<><>)})]<[([<>{}]<{}[]>)[<{}<>>{[]()}]]<(<<><>>(()()))<[<><>}{{}{}}>>>](
               |[<((<{{[(<(((<[]()>([][]))(<()<>>{{}[]}))((<{}{}><{}[]>)({{}[]}[[]{}])))<{(<[][]>)<{<>()}>}{{[()<>]}{{<>(
               |{((([{{((<<<<[()()][(){}]>{[[][]](()<>)}>>{{{{<><>}[[]<>]}[{<><>}({}()]]}([[<>]{(){}}][[(){}](()<>)])}>[([([[
               |[(({<<{([{(<[<()[]><[]()>]((<>[])[<>()])>([<(){}><<>()>]{[<>()][(){}]}))}[<[{[{}()]{()()}}(<<>>{[]})]{[<<>()>
               |<[{[<<({<<<[((<>()){()()})[(<>[])([]{})]]><[<([][])([]<>)>(<[]{}>(()()))][{[()[]]{[]<>}}[(<>{})<<>
               |((<[<[(({[{[[<<>[]]{[]()}]({()<>})]{[<<>()><{}()>]}}(<(([]{}){(){}})>{<{{}()}(()())>})]}([(<{[[][]
               |{(((((<({[<[{<<><>>{<>[]}}(<(){}><[]()>)]<{<{}()>(<>())}[[[]<>]{{}<>}]>>{[{<()()>[[]()]}[<{}>]]}]{
               |(({([{<([<{{<<[]{}>[[]{}]>({{}{}}({}<>))}<[{<>{}}{()<>}][<<>{}>]>}>{({(<(){}>[{}()])[{()<>}[()<>]]}<[{[
               |[[{{[<<[<((<{{{}()}<[]>}{[{}<>]<()[]>}>({[[]{}]}[([]())<[]{}>])){(<<()()>>{[{}[]]<{}{}>})})>]>>]<([<[<
               |{({{{{[[<{<[{[{}<>]}([[]{}][{}<>])][((()()){[]()}){[<>()](<>())}]>}>[{<[{{<>{}}({}())}[<<>{}>((
               |({[((<{{<<<({[{}{}][(){}]})((<[]><()()>)<{<>()}[[]<>]>)>(({<{}[]>([][])}(({}{})<<>()>))<{<[
               |[<<[<{[[{{(<{[[]<>)({}[])}>)}}(({{((<>[])<<><>>)}{[[[]<>]]{{[][]}<{}{}>}}}){[[((<><>)[{}<>])
               |(({<(<([({<(<<()<>>([]<>)>[([]()><()[]>])<[<()[]>[()()]]{[[][]]{{}[]}}>>{{<<[][]><[]{}>>{[{}[]]<<>>}}{{<[]{
               |([{{({({{{<([({}[])[()<>]][[[]{}]([]())])>([[[()()][<>()]]([{}[]]{{}()})]<{{[]<>}({}[])}{<()[
               |(<{(({(<{{[<[[()<>]{<>()}][<(){}><()<>>]>{<<<>[]>[<><>]><({}<>>>}](<<{<>[]}[[]<>]>[[[]<>](()[])]>{{{[]
               |[[[<[([([(({{<<><>>([][])}})[([[{}()]<<><>>])<({{}<>})<{{}}({}{})>>])])<([((<[[]<>]{(){}}>{[[]<>]<[]<>>})(
               |(({(<({[[<{<([{}<>]{<>()})([()()](()<>))>(((()())[{}()])({()[]}{{}<>}))}>([({<(){}>([]<>)}{[<>()]<<>>}
               |({{[<<{<<[<<{[()()]<<>[]>}({[]}<<>{}>)>[<<{}{}>[<>[]]><({}<>)<[]()>>]>({(<()[]>(<>[]))}[([()()][{}]){{()()}{
               |(<({{([{<[{(([()]<<><>>))}<<<[()][{}{}]><([][])(()())>>[[<<>[]><[]>]{[<>{}]<[][]>}]>](<(<<[]>({}())
               |(<{<([[[<<((([<>{}]{()[]})([(){}]{{}<>}))[[[{}<>](()[])][[<>[]](<>())]])>>([{([{<><>}<{}[]>]
               |<{[{[{<{{({<<({}[])><{()()]<[]<>>>><[({}[])[()()]]<[{}<>]{<>{}}>>}((({()[]}{[]<>}){[[]()]([]<>)})))
               |[(([[[[<<<[<{{<><>}{[]()}}[({}[])<(){}>]>(([[][]]([]{}))<{{}}(()<>)>)]({<<[]{}>{[]<>}>{[()<>}{{}}}}{{[(){}]}
               |{(([<{<(<{<{{<()><[]{}>}}({{{}<>}}<{<>[]}<{}<>>>)>[<([{}[]]<()<>>)(<{}{}>{()[]})><<{<>[]}(()[])>>]}(
               |(<((<{[[{({[([()[]]<()<>>)[<<>{}>({}[])]][<[<>[]]><{(){}}>]}[<(({})({}()))(<[]{}>(<><>))>[[([]{})[{}[
               |{({([[[[{<([<{{}<>}[[][]]><<<>[]><<>{}>>]{{<{}<>><[][]>}({[]{}}<{}[]>)})>{{{{[[][]]}}}}}[<[[{[{}[]][()()]
               |[{(<([{{{<{<({{}[]}<()<>>){<<>())[{}[]]}>{{{[][]}}[[(){}]{<>()}]}}([(<<>()><[]()>)]{({[]<>}
               |[<[([{<((({<{{<>{}}}<<(){}><()()>>><({()}[[]{}])(([]())[<>[]])>}))){<[{{{(()[])({}())}{{()()}{{}<>}}}[(<{}
               |<([({({{(({{({<>}(<><>))}{([{}[]]{{}<>})}}[{({[]<>}([]<>))[<[]>([]{})]}({[[]{}]({}())}{<{}<>
               |[{(({(({<[[{<[(){}](<>[])>}[[[<>()]][([][])<<>()>]]](((<<><>>[(){}])([[][]]<{}{}>))[<(<>()){[]{}}>[[()[
               |<<{{<[(([{[{[[<><>][[]()]]<(<>[])[[]<>]>}]<([<()[]>((){})][<{}>(<>())])>}][{(<([()()][<><>])(({
               |({(<({([(<[<{[{}()]({}{})}<<{}{}>(()<>)>>]>{([<[<><>]<(){}>><({}[])((){})>]{{<[]()>[{}[]]}(({}{})<
               |<[{<{((<{((<[([]<>)]<{{}()}{()[]}>>(((())){<[]{}>[{}[]]}))<[<<{}[]>((){})>[<()()>{()()}]]<[[(
               |{{{((<(([{({(({}{})({}{}))}[<{()<>}[{}()]><([]()){[][]}}])[([[()[]][[]{}]])<{({}())[<>{}]}{(<>[])}
               |<({[{[[{(({<(<<>>[(){}])({[]{}}<[]()>)>}<{[([]())<<>{})]{(()[])<[]>}}(<[{}{}]>(((){}){{}{}}))>)
               |<{[[({[{[{[([[<>{}]((){})][[[][]]{()<>}])<[<{}()>(<>{})]<<<><>>[<>()]>>][[[<[]<>>{{}<>}]<{[]{}}<[]{}>>]
               |(({(<((({({{<<{}()>(()<>)><<<>{}>[{}{}]>}(((()<>)(()()))(<{}<>>))}[<[[[]<>]]({[][]}[<>{}])>]){[(({{}{}}){([][
               |[{{<({<<{(<({<{}<>>[[]<>]}<[{}[]](()()]>)>([{{<>[]}}{([][])([][])}]))({(<{[]{}}{()<>}>([<>()]{[]{}}))
               |[<({[(<{{{[((<[]<>>(<>[]))([[]<>]<()<>>))(<((){})>)]}}}>{{<([<[<()[]>{{}<>}](([]())[[]<>))>]{(({[][]}<<>(
               |([(({<([{({<(([]<>)(()[])){<()()>{<>()}}><([[][]]<()<>>)[[[][]]<()<>>]>}[{[{()[]}(<>[])]([<>[]]<<>()>)}])
               |{{{([<[([{(<[{[]()}<[]{}>]>)}])<<<<({(<>())<{}()>})[{({}[]}{<>[]}}<<[]<>><{}[]>>]><(<[()<>""".stripMargin.split("\\n").toList

//  val rawInput  = """[({(<(())[]>[[{[]{<()<>>
//                  |[(()[<>])]({[<{<<[]>>(
//                  |{([(<{}[<>[]}>{[]{[(<()>
//                  |(((({<>}<{<{<>}{[]{[]{}
//                  |[[<[([]))<([[{}[[()]]]
//                  |[{[{({}]{}}([{[{{{}}([]
//                  |{<[[]]>}<{[{[{[]{()[[[]
//                  |[<(<(<(<{}))><([]([]()
//                  |<{([([[(<>()){}]>(<<{{
//                  |<{([{{}}[<[[[<>{}]]]>[]]""".stripMargin.split("\\n").toList

val input = rawInput.map(_.trim.toList)

def simulate(chunk: List[Char]):Seq[Char] = {
  val acceptablePairs:Map[Char,Char] = Map(( '(', ')'),('[',']'),('{','}'),('<','>'))

  @tailrec
  def simulate2(in: List[Char], stack:mutable.Stack[Char], incompletes:Seq[Char]):Seq[Char]={
    in match {
      case head +: tail =>
        if(!acceptablePairs.keySet.contains(head)){ // closing brace due to !
          if(acceptablePairs(stack.top) == head){ //matching pair
            stack.pop()
            simulate2(tail,stack,incompletes)
          }else{
            incompletes :+ head // incomplete
          }
        }else {
          stack.push(head)
          simulate2(tail,stack,incompletes)
        }
      case _ => Nil
    }
  }

  simulate2(chunk,mutable.Stack(),Nil)
}

val results =  input.map(simulate) // returns a seq with Nil values being lines that are complete
val syntaxErrorScore:Map[Char,Int] = Map(( ')',3),(']',57),('}',1197),('>',25137))
val res = results.filter(_.nonEmpty).foldLeft(0)((x,y) => x + syntaxErrorScore(y.head))
println(res)





//Part Two **************************************************************
def getIncompleteChunks(in: List[Seq[Char]]):List[List[Char]]= {
  val indices = for {i<-in.indices if(in(i) == Nil)} yield i
  indices.map(input).toList
}

val partTwoInput = getIncompleteChunks(results)

def simulatePartTwo(chunk: List[Char]):Seq[Char] = {
  val acceptablePairs:Map[Char,Char] = Map(( '(', ')'),('[',']'),('{','}'),('<','>'))

  @tailrec
  def simulatePartTwo2(in: List[Char], stack:mutable.Stack[Char], closingChars:Seq[Char]):Seq[Char]={
    in match {
      case head +: tail =>
        if(!acceptablePairs.keySet.contains(head)){ // closing brace due to !
          if(acceptablePairs(stack.top) == head){ //matching pair
            stack.pop()
            simulatePartTwo2(tail,stack,closingChars)
          }else{
            val correctClosingBrace = acceptablePairs(stack.top)
            stack.pop()
            val sameChunk = head+:tail
            val newClosingChars = closingChars :+ correctClosingBrace // incomplete
            simulatePartTwo2(sameChunk,stack,newClosingChars)
          }
        }else {
          stack.push(head)
          simulatePartTwo2(tail,stack,closingChars)
        }
      case _ => {
        val res = if(stack.nonEmpty){
          stack.popAll().reverse.map{char => acceptablePairs(char) }
        }else Nil
        closingChars ++ res
      }
    }
  }

  simulatePartTwo2(chunk,mutable.Stack(),Nil)
}

val completingStrings = partTwoInput.map(simulatePartTwo)

@tailrec
def calculateScore(completingStrings :Seq[Char], totalScore:Long ): Long = {
  val completingStringValues:Map[Char,Int] = Map(( ')',1),(']',2),('}',3),('>',4))

  completingStrings match {
    case head +: tail =>
      val newTotalScore = (totalScore * 5) +(completingStringValues(head))
      calculateScore(tail,newTotalScore)
    case _ => totalScore
  }
}

val median = (completingStrings.length / 2)
val sortedRes = completingStrings.map(calculateScore(_,0)).sorted
val winner = sortedRes(median)
println(winner)