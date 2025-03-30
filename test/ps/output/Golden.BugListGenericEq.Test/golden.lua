local M = {}
M.Data_HeytingAlgebra_foreign = {
  boolConj = function(b1)
    return function(b2)
      return b1 and b2
    end
  end,
  boolDisj = function(b1)
    return function(b2)
      return b1 or b2
    end
  end,
  boolNot = function(b)
    return not b
  end
}
M.Type_Proxy_Proxy = {["$ctor"] = "Type.Proxy∷Proxy.Proxy"}
M.Data_HeytingAlgebra_heytingAlgebraBoolean = {
  ff = false,
  tt = true,
  implies = function(a)
    return M.Data_HeytingAlgebra_heytingAlgebraBoolean.disj(M.Data_HeytingAlgebra_heytingAlgebraBoolean._not_(a))
  end,
  conj = M.Data_HeytingAlgebra_foreign.boolConj,
  disj = M.Data_HeytingAlgebra_foreign.boolDisj,
  _not_ = M.Data_HeytingAlgebra_foreign.boolNot
}
M.Data_Eq_eqRecord = function(dict)
  return dict.eqRecord
end
M.Data_Eq_eq = function(dict)
  return dict.eq
end
M.Data_Eq_eqRowCons = function(dictEqRecord)
  return function()
    return function(dictIsSymbol)
      return function(dictEq)
        return {
          eqRecord = function()
            return function(ra)
              return function(rb)
                local key = dictIsSymbol.reflectSymbol(M.Type_Proxy_Proxy)
                local get = (function(l)
                  return function(r)
                    return r[l]
                  end
                end)(key)
                return M.Data_HeytingAlgebra_heytingAlgebraBoolean.conj(M.Data_Eq_eq(dictEq)(get(ra))(get(rb)))(
                  M.Data_Eq_eqRecord(dictEqRecord)(M.Type_Proxy_Proxy)(ra)(rb)
                )
              end
            end
          end
        }
      end
    end
  end
end
M.Data_Eq_Generic_genericEqPrime = function(dict)
  return dict.genericEqPrime
end
M.Data_Eq_Generic_genericEqConstructor = function(dictGenericEq)
  return {genericEqPrime = M.Data_Eq_Generic_genericEqPrime(dictGenericEq)}
end
M.Golden_BugListGenericEq_Test_Nil = {
  ["$ctor"] = "Golden.BugListGenericEq.Test∷List.Nil"
}
M.Golden_BugListGenericEq_Test_Cons = function(value0)
  return {
    ["$ctor"] = "Golden.BugListGenericEq.Test∷List.Cons",
    value0 = value0
  }
end
M.Golden_BugListGenericEq_Test_genericList = {
  to = function(x)
    if "Data.Generic.Rep∷Sum.Inl" == x["$ctor"] then
      return M.Golden_BugListGenericEq_Test_Nil
    else
      if "Data.Generic.Rep∷Sum.Inr" == x["$ctor"] then
        return M.Golden_BugListGenericEq_Test_Cons(x.value0)
      else
        return error("No patterns matched")
      end
    end
  end,
  from = function(x)
    if "Golden.BugListGenericEq.Test∷List.Nil" == x["$ctor"] then
      return (function(value0)
        return {["$ctor"] = "Data.Generic.Rep∷Sum.Inl", value0 = value0}
      end)({["$ctor"] = "Data.Generic.Rep∷NoArguments.NoArguments"})
    else
      if "Golden.BugListGenericEq.Test∷List.Cons" == x["$ctor"] then
        return (function(value0)
          return {["$ctor"] = "Data.Generic.Rep∷Sum.Inr", value0 = value0}
        end)(x.value0)
      else
        return error("No patterns matched")
      end
    end
  end
}
M.Golden_BugListGenericEq_Test_eqList = function(dictEq)
  return {
    eq = (function()
      return function(dictGenericEq)
        local from = M.Golden_BugListGenericEq_Test_genericList.from
        return function(x)
          return function(y)
            return M.Data_Eq_Generic_genericEqPrime(dictGenericEq)(from(x))(from(y))
          end
        end
      end
    end)()(
      {
        genericEqPrime = function(v)
          return function(v1)
            if "Data.Generic.Rep∷Sum.Inl" == v["$ctor"] then
              if "Data.Generic.Rep∷Sum.Inl" == v1["$ctor"] then
                return M.Data_Eq_Generic_genericEqPrime(
                  M.Data_Eq_Generic_genericEqConstructor(
                    {
                      genericEqPrime = function()
                        return function()
                          return true
                        end
                      end
                    }
                  )
                )(v.value0)(v1.value0)
              else
                return false
              end
            else
              if "Data.Generic.Rep∷Sum.Inr" == v["$ctor"] then
                if "Data.Generic.Rep∷Sum.Inr" == v1["$ctor"] then
                  return M.Data_Eq_Generic_genericEqPrime(
                    M.Data_Eq_Generic_genericEqConstructor(
                      {
                        genericEqPrime = M.Data_Eq_eq(
                          {
                            eq = M.Data_Eq_eqRecord(
                              M.Data_Eq_eqRowCons(
                                M.Data_Eq_eqRowCons(
                                  {
                                    eqRecord = function()
                                      return function()
                                        return function()
                                          return true
                                        end
                                      end
                                    end
                                  }
                                )()(
                                  {
                                    reflectSymbol = function()
                                      return "tail"
                                    end
                                  }
                                )(M.Golden_BugListGenericEq_Test_eqList(dictEq))
                              )()(
                                {
                                  reflectSymbol = function()
                                    return "head"
                                  end
                                }
                              )(dictEq)
                            )(M.Type_Proxy_Proxy)
                          }
                        )
                      }
                    )
                  )(v.value0)(v1.value0)
                else
                  return false
                end
              else
                return false
              end
            end
          end
        end
      }
    )
  }
end
return {
  Nil = M.Golden_BugListGenericEq_Test_Nil,
  Cons = M.Golden_BugListGenericEq_Test_Cons,
  cons = function(head)
    return function(tail)
      return M.Golden_BugListGenericEq_Test_Cons({head = head, tail = tail})
    end
  end,
  genericList = M.Golden_BugListGenericEq_Test_genericList,
  eqList = M.Golden_BugListGenericEq_Test_eqList
}
