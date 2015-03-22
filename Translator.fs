namespace While2Arr

module Translator =

    module While = While.AST
    module Arr = Arr.AST

    let rec private xlateAexp (exp : While.Aexp) : Arr.Aexp =
        match exp with
        | While.Int(n) -> Arr.Aexp.Int(n)
        | While.Var(x) -> Arr.Aexp.Arr(x, Arr.Int(0))
        | While.Add(a1, a2) -> Arr.Aexp.Add(xlateAexp a1, xlateAexp a2)
        | While.Mul(a1, a2) -> Arr.Aexp.Mul(xlateAexp a1, xlateAexp a2)
        | While.Sub(a1, a2) -> Arr.Aexp.Sub(xlateAexp a1, xlateAexp a2)


    /// Translates a While program into an equivalent Arr program.
    ///
    /// A variable x in the While program is mapped to an array with
    /// one element in Arr.  So, x := 3 becomes x[0] := 3, and x := y
    /// becomes x[0] := y[0].
    ///
    /// The Arr program may contain assignments to temporary arrays.
    /// In the Arr program's output, any array whose name starts with
    /// "temp" should be ignored.
    let rec While2Arr (stm : While.Stm) : Arr.Stm =
        match stm with
        | While.Assign(variable, valueExp) -> Arr.Assign(variable, Arr.Int(0), xlateAexp valueExp)
        | While.Skip -> Arr.Assign("tempskip", Arr.Int(0), Arr.Int(0))
        | While.Seq(s1, s2) -> Arr.Seq(While2Arr s1, While2Arr s2)
        | While.IfElse(bexp, s1, s2) ->
            match bexp with
            | While.True -> (While2Arr (While.IfElse(While.Lte(While.Int(0), While.Int(0)), s1, s2)))
            | While.False -> While2Arr (While.IfElse(While.Lte(While.Int(1), While.Int(0)), s1, s2))
            | While.And(bexp1, bexp2) ->
                While2Arr (While.IfElse(bexp1, While.IfElse(bexp2, s1, s2), s2))
            | While.Lte(a1, a2) ->
                Arr.Stm.Seq(
                    Arr.Stm.For("tempbool", Arr.Int(0), xlateAexp a1, xlateAexp a2, 
                        Arr.Seq(While2Arr s1, Arr.Assign("tempbool", Arr.Int(0), xlateAexp a2))),
                    Arr.Stm.For("tempbool", Arr.Int(0), xlateAexp (While.Add(a2, While.Int(1))), xlateAexp a1,
                        Arr.Seq(While2Arr s2, Arr.Assign("tempbool", Arr.Int(0), xlateAexp a1)))
                )
            | While.Eq(a1, a2) -> While2Arr (While.IfElse(While.And(While.Lte(a1, a2), While.Lte(a2, a1)), s1, s2))
            | While.Not(bexp2) ->
                match bexp2 with
                | While.True -> While2Arr (While.IfElse(While.False, s1, s2))
                | While.False -> While2Arr (While.IfElse(While.True, s1, s2))
                | _ -> failwith "unhandled"
        | While.While(bexp, body) ->
            match bexp with
            | While.True -> While2Arr (While.While(While.Lte(While.Int(0), While.Int(1)), While.Seq(body, While.Assign("tempbool", While.Int(0))))) /// oh my god, it's so ugly
            | While.False -> While2Arr (While.While(While.Lte(While.Int(1), While.Int(0)), body))
            | While.And(bexp1, bexp2) ->
                let negateBexp (bexp3 : While.Bexp) : While.Aexp =
                    match bexp3 with
                    | While.True -> While.Int(1) // change the infinite construct to be 1
                    | While.Eq(a1, a2) -> While.Add(a1, While.Int(1)) // add 1 to the Aexp to make them no longer equal
                    | While.Lte(a1, a2) -> While.Add(a2, While.Int(1)) // add 1 here to prevent a weird case from happening
                    | While.And(a1, a2) -> failwith "What the hell happened here?" // can't have an and as the root of a For
                    | While.False -> While.Int(1) // This should not affect anything, as the only case it handles is False. 
                While2Arr (While.While(bexp1, While.IfElse(bexp2, body, While.Assign("tempbool", (negateBexp bexp1)))))
            | While.Lte(a1, a2) ->
                Arr.Stm.For("tempbool", Arr.Int(0), xlateAexp a1, xlateAexp a2, While2Arr body)
            | While.Eq(a1, a2) ->
                While2Arr (While.While(While.And(While.Lte(a1, a2), While.Lte(a2, a1)), body))
            | While.Not(bexp1) -> failwith "unhandled"
