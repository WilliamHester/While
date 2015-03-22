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

    let rec private xlateBexp (exp : While.Bexp) : Arr.Aexp =
        (*Values less than or equal to zero correspond to truth*)
        match exp with
        | While.True -> Arr.Int(0)
        | While.False -> Arr.Int(1)
        | While.Eq(a1, a2) -> Arr.Mul(Arr.Sub(xlateAexp a2, xlateAexp a1), Arr.Sub(xlateAexp a2, xlateAexp a1))
        | While.Lte(a1, a2) -> Arr.Sub(xlateAexp a1, xlateAexp a2)
        | While.Not(b1) -> Arr.Add(Arr.Int(1), Arr.Mul(Arr.Int(-1), xlateBexp b1))
        | While.And(b1, b2) -> 
            let a1, a2 = xlateBexp b1, xlateBexp b2
            Arr.Mul(Arr.Int(-1), Arr.Add(Arr.Int(1), Arr.Mul(2, Arr.Mul(Arr.Sub(a2, a1), Arr.Sub(a1, a2)))))

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
        | _ -> failwith "not implemented"