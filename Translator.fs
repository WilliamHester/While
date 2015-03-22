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

    let rec private xlateBexp (exp : While.Bexp) : Arr.Stm = 
        /// true is 1, false is 0.
        match exp with
        | While.True -> Arr.Assign("tempbool", Arr.Int(0), Arr.Int(1))
        | While.False -> Arr.Assign("tempbool", Arr.Int(0), Arr.Int(0))
        | While.Lte(a1, a2) ->
            let a1, a2 = xlateAexp a1, xlateAexp a2
            Arr.Seq(
                Arr.Assign("tempbool", Arr.Int(0), Arr.Int(0)),
                Arr.For("tempbool", Arr.Int(2), a1, a2, 
                    Arr.Seq(
                        Arr.Assign("tempbool", Arr.Int(0), Arr.Int(1)),
                        Arr.Assign("tempbool", Arr.Int(2), a2)
                    )
                )
            )
        | While.Eq(a1, a2) ->
            let a1, a2 = xlateAexp a1, xlateAexp a2
            Arr.Seq(
                Arr.Seq(
                    Arr.Seq(
                        Arr.Assign("tempbool", Arr.Int(0), Arr.Int(0)),
                        Arr.Assign("tempbool", Arr.Int(1), Arr.Int(0))
                    ),
                    Arr.Seq(
                        Arr.For("tempbool", Arr.Int(2), a1, a2, 
                            Arr.Seq(
                                Arr.Assign("tempbool", Arr.Int(0), Arr.Int(1)),
                                Arr.Assign("tempbool", Arr.Int(2), a2)
                            )
                        ),
                        Arr.For("tempbool", Arr.Int(2), a2, a1, 
                            Arr.Seq(
                                Arr.Assign("tempbool", Arr.Int(1), Arr.Int(1)),
                                Arr.Assign("tempbool", Arr.Int(2), a1)
                            )
                        )
                    )
                ),
                Arr.Seq(
                    Arr.Assign("tempbool", Arr.Int(0), Arr.Mul(Arr.Arr("tempbool", Arr.Int(0)), Arr.Arr("tempbool", Arr.Int(1)))),
                    Arr.Assign("tempbool", Arr.Int(1), Arr.Int(0))
                )
            )
        | While.Not(b1) -> 
            Arr.Seq(
                xlateBexp b1,
                Arr.Assign("tempbool", Arr.Int(0), Arr.Sub(Arr.Int(1), Arr.Arr("tempbool", Arr.Int(0))))
            )
        | While.And(b1, b2) ->
            Arr.Seq(
                Arr.Seq(
                    xlateBexp b1,
                    Arr.Assign("tempbool", Arr.Int(10), Arr.Arr("tempbool", Arr.Int(0)))
                ),
                Arr.Seq(
                    xlateBexp b2,
                    Arr.Assign("tempbool", Arr.Int(0), Arr.Mul(Arr.Arr("tempbool", Arr.Int(0)), Arr.Arr("tempbool", Arr.Int(10))))
                )
            )
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
            Arr.Seq(
                xlateBexp bexp,
                Arr.Seq(
                    Arr.For("tempif", Arr.Int(0), Arr.Int(1), Arr.Arr("tempbool", Arr.Int(0)),
                        While2Arr s1
                    ),
                    Arr.For("tempif", Arr.Int(0), Arr.Int(1), Arr.Sub(Arr.Int(1), Arr.Arr("tempbool", Arr.Int(0))),
                        While2Arr s2
                    )
                )
            )
        | While.While(bexp, body) ->
            Arr.Seq(
                Arr.Seq(
                    Arr.Seq(
                        xlateBexp bexp,
                        Arr.Seq(
                            Arr.Assign("tempcounter", Arr.Int(-1), Arr.Add(Arr.Arr("tempcounter", Arr.Int(-1)), Arr.Int(1))),
                            Arr.Assign("tempcounter", Arr.Arr("tempcounter", Arr.Int(-1)), Arr.Int(0))
                        )
                    ),
                    (*Do the loop*)
                    Arr.For("tempcounter", Arr.Int(0), Arr.Arr("tempcounter", Arr.Arr("tempcounter", Arr.Int(-1))), Arr.Sub(Arr.Arr("tempbool", Arr.Int(0)), Arr.Int(1)),
                        Arr.Seq(
                            While2Arr body,
                            Arr.Seq(
                                xlateBexp bexp,
                                Arr.Assign("tempcounter", Arr.Arr("tempcounter", Arr.Int(-1)), Arr.Sub(Arr.Arr("tempcounter", Arr.Arr("tempcounter", Arr.Int(-1))), Arr.Int(1)))
                            )
                        )
                    )
                ),
                Arr.Seq(
                    Arr.Assign("tempcounter", Arr.Arr("tempcounter", Arr.Int(-1)), Arr.Int(0)),
                    Arr.Assign("tempcounter", Arr.Int(-1), Arr.Add(Arr.Arr("tempcounter", Arr.Int(-1)), Arr.Int(-1)))
                )
            )