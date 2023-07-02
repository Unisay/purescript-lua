local Prim_I_undefined = nil
local _S___runtime_lazy = function(name)
  return function(init)
    local state = 0
    local val = nil
    return function()
      if state == 2 then
        return val
      else
        if state == 1 then
          return error(name .. " was needed before it finished initializing")
        else
          state = 1
          val = init()
          state = 2
          return val
        end
      end
    end
  end
end
local Data_Symbol_I_foreign = (function()
  return {unsafeCoerce = function(a) return a end}
end)()
local Data_Symbol_I_unsafeCoerce = Data_Symbol_I_foreign.unsafeCoerce
local Data_Unit_I_foreign = (function() return {unit = nil} end)()
local Data_Unit_I_unit = Data_Unit_I_foreign.unit
local Record_Unsafe_I_foreign = (function()
  return {
    unsafeHas = function(l) return function(r) return r[l] ~= nil end end,
    unsafeGet = function(l) return function(r) return r[l] end end,
    unsafeSet = function(l)
      return function(value)
        return function(r)
          local copy = {}
          for key, val in pairs(r) do copy[key] = val end
          copy[l] = value
          return copy
        end
      end
    end,
    unsafeDelete = function(l)
      return function(r)
        local copy = {}
        for key, val in pairs(r) do if key ~= l then copy[key] = val end end
        return copy
      end
    end
  }
end)()
local Record_Unsafe_I_unsafeHas = Record_Unsafe_I_foreign.unsafeHas
local Record_Unsafe_I_unsafeGet = Record_Unsafe_I_foreign.unsafeGet
local Record_Unsafe_I_unsafeSet = Record_Unsafe_I_foreign.unsafeSet
local Record_Unsafe_I_unsafeDelete = Record_Unsafe_I_foreign.unsafeDelete
local Data_HeytingAlgebra_I_foreign = (function()
  return {
    boolConj = function(b1) return function(b2) return b1 and b2 end end,
    boolDisj = function(b1) return function(b2) return b1 or b2 end end,
    boolNot = function(b) return not b end
  }
end)()
local Data_HeytingAlgebra_I_boolConj = Data_HeytingAlgebra_I_foreign.boolConj
local Data_HeytingAlgebra_I_boolDisj = Data_HeytingAlgebra_I_foreign.boolDisj
local Data_HeytingAlgebra_I_boolNot = Data_HeytingAlgebra_I_foreign.boolNot
local Data_Eq_I_foreign = (function()
  local refEq = function(r1) return function(r2) return r1 == r2 end end
  return {
    refEq = refEq,
    eqBooleanImpl = refEq,
    eqIntImpl = refEq,
    eqNumberImpl = refEq,
    eqCharImpl = refEq,
    eqStringImpl = refEq,
    eqArrayImpl = function(f)
      return function(xs)
        return function(ys)
          local l = #xs
          if l ~= #ys then return false end
          for i = 1, l do if not f(xs[i])(ys[i]) then return false end end
          return true
        end
      end
    end
  }
end)()
local Data_Eq_I_eqBooleanImpl = Data_Eq_I_foreign.eqBooleanImpl
local Data_Eq_I_eqIntImpl = Data_Eq_I_foreign.eqIntImpl
local Data_Eq_I_eqNumberImpl = Data_Eq_I_foreign.eqNumberImpl
local Data_Eq_I_eqCharImpl = Data_Eq_I_foreign.eqCharImpl
local Data_Eq_I_eqStringImpl = Data_Eq_I_foreign.eqStringImpl
local Data_Eq_I_eqArrayImpl = Data_Eq_I_foreign.eqArrayImpl
local Data_Semigroup_I_foreign = (function()
  return {
    concatString = function(s1) return function(s2) return s1 .. s2 end end,
    concatArray = function(xs)
      return function(ys)
        if #xs == 0 then return ys end
        if #ys == 0 then return xs end
        return table.concat(xs, ys)
      end
    end
  }
end)()
local Data_Semigroup_I_concatString = Data_Semigroup_I_foreign.concatString
local Data_Semigroup_I_concatArray = Data_Semigroup_I_foreign.concatArray
local Data_Show_I_foreign = (function()
  return {
    showIntImpl = function(n) return tostring(n) end,
    showNumberImpl = function(n) return tostring(n) end,
    showCharImpl = function(n)
      local code = n:byte()
      if code < 0x20 or code == 0x7F then
        if n == "\x07" then return "'\\a'" end
        if n == "\b" then return "'\\b'" end
        if n == "\f" then return "'\\f'" end
        if n == "\n" then return "'\\n'" end
        if n == "\r" then return "'\\r'" end
        if n == "\t" then return "'\\t'" end
        if n == "\v" then return "'\\v'" end
        return "'\\" .. code:toString(10) .. "'"
      end
      if n == "'" or n == "\\" then return "'\\" .. n .. "'" end
      return "'" .. n .. "'"
    end,

    --[[
          export const showStringImpl = function (s) {
          var l = s.length;
          return "\"" + s.replace(
              /[\0-\x1F\x7F"\\]/g, // eslint-disable-line no-control-regex
              function (c, i) {
              switch (c) {
                  case "\"":
                  case "\\":
                  return "\\" + c;
                  case "\x07": return "\\a";
                  case "\b": return "\\b";
                  case "\f": return "\\f";
                  case "\n": return "\\n";
                  case "\r": return "\\r";
                  case "\t": return "\\t";
                  case "\v": return "\\v";
              }
              var k = i + 1;
              var empty = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
              return "\\" + c.charCodeAt(0).toString(10) + empty;
              }
          ) + "\"";
          };
      ]]

    showStringImpl = function(s)
      return s -- TODO
    end,
    showArrayImpl = function(f)
      return function(xs)
        local l = #xs
        local ss = {}
        for i = 1, l do ss[i] = f(xs[i - 1]) end
        return "[" .. table.concat(ss, ",") .. "]"
      end
    end

  }
end)()
local Data_Show_I_showIntImpl = Data_Show_I_foreign.showIntImpl
local Data_Show_I_showNumberImpl = Data_Show_I_foreign.showNumberImpl
local Data_Show_I_showCharImpl = Data_Show_I_foreign.showCharImpl
local Data_Show_I_showStringImpl = Data_Show_I_foreign.showStringImpl
local Data_Show_I_showArrayImpl = Data_Show_I_foreign.showArrayImpl
local Data_Semiring_I_foreign = (function()
  return {
    intAdd = function(x) return function(y) return x + y | 0 end end,
    intMul = function(x) return function(y) return x * y | 0 end end,
    numAdd = function(x) return function(y) return x + y end end,
    numMul = function(x) return function(y) return x * y end end
  }
end)()
local Data_Semiring_I_intAdd = Data_Semiring_I_foreign.intAdd
local Data_Semiring_I_intMul = Data_Semiring_I_foreign.intMul
local Data_Semiring_I_numAdd = Data_Semiring_I_foreign.numAdd
local Data_Semiring_I_numMul = Data_Semiring_I_foreign.numMul
local Data_Ring_I_foreign = (function()
  return {
    intSub = function(x) return function(y) return x - y | 0 end end,
    numSub = function(x) return function(y) return x - y end end
  }
end)()
local Data_Ring_I_intSub = Data_Ring_I_foreign.intSub
local Data_Ring_I_numSub = Data_Ring_I_foreign.numSub
local Data_Ord_I_foreign = (function()
  local unsafeCoerceImpl = function(lt)
    return function(eq)
      return function(gt)
        return function(x)
          return function(y)
            if x < y then
              return lt
            elseif x == y then
              return eq
            else
              return gt
            end
          end
        end
      end
    end
  end

  return {
    ordBooleanImpl = unsafeCoerceImpl,
    ordIntImpl = unsafeCoerceImpl,
    ordNumberImpl = unsafeCoerceImpl,
    ordStringImpl = unsafeCoerceImpl,
    ordCharImpl = unsafeCoerceImpl,
    ordArrayImpl = function(f)
      return function(xs)
        return function(ys)
          local i = 1
          local xlen = #xs
          local ylen = #ys
          while i <= xlen and i <= ylen do
            local x = xs[i]
            local y = ys[i]
            local o = f(x)(y)
            if o ~= 0 then return o end
            i = i + 1
          end
          if xlen == ylen then
            return 0
          elseif xlen > ylen then
            return -1
          else
            return 1
          end
        end
      end
    end
  }
end)()
local Data_Ord_I_ordBooleanImpl = Data_Ord_I_foreign.ordBooleanImpl
local Data_Ord_I_ordIntImpl = Data_Ord_I_foreign.ordIntImpl
local Data_Ord_I_ordNumberImpl = Data_Ord_I_foreign.ordNumberImpl
local Data_Ord_I_ordStringImpl = Data_Ord_I_foreign.ordStringImpl
local Data_Ord_I_ordCharImpl = Data_Ord_I_foreign.ordCharImpl
local Data_Ord_I_ordArrayImpl = Data_Ord_I_foreign.ordArrayImpl
local Data_Functor_I_foreign = (function()
  return {
    arrayMap = function(f)
      return function(arr)
        local l = #arr
        local result = {}
        for i = 1, l do result[i] = f(arr[i]) end
        return result
      end
    end
  }
end)()
local Data_Functor_I_arrayMap = Data_Functor_I_foreign.arrayMap
local Control_Apply_I_foreign = (function()
  return {
    arrayApply = function(fs)
      return function(xs)
        local n, l, m, result = 1, #fs, #xs, {}
        for i = 1, l do
          for j = 1, m do
            result[n] = fs[i](xs[j])
            n = n + 1
          end
        end
        return result
      end
    end
  }
end)()
local Control_Apply_I_arrayApply = Control_Apply_I_foreign.arrayApply
local Control_Bind_I_foreign = (function()
  return {
    arrayBind = function(arr)
      return function(f)
        local n = 1
        local result = {}
        for i = 1, #arr do
          for _, v in ipairs(f(arr[i])) do
            result[n] = v
            n = n + 1
          end
        end
        return result
      end
    end
  }
end)()
local Control_Bind_I_arrayBind = Control_Bind_I_foreign.arrayBind
local Data_Bounded_I_foreign = (function()
  return {
    topInt = math.maxinteger,
    bottomInt = math.mininteger,
    topChar = "\u{FFFF}",
    bottomChar = "\u{0000}",
    topNumber = 1 / 0,
    bottomNumber = -1 / 0
  }
end)()
local Data_Bounded_I_topInt = Data_Bounded_I_foreign.topInt
local Data_Bounded_I_bottomInt = Data_Bounded_I_foreign.bottomInt
local Data_Bounded_I_topChar = Data_Bounded_I_foreign.topChar
local Data_Bounded_I_bottomChar = Data_Bounded_I_foreign.bottomChar
local Data_Bounded_I_topNumber = Data_Bounded_I_foreign.topNumber
local Data_Bounded_I_bottomNumber = Data_Bounded_I_foreign.bottomNumber
local Data_EuclideanRing_I_foreign = (function()
  return {
    intDegree = function(x) return math.min(math.abs(x), math.maxinteger) end,
    intDiv = function(x)
      return function(y)
        if y == 0 then return 0 end
        return y > 0 and math.floor(x / y) or -math.floor(x / -y)
      end
    end,
    intMod = function(x)
      return function(y)
        if y == 0 then return 0 end
        local yy = math.abs(y)
        return ((x % yy) + yy) % yy
      end
    end,
    numDiv = function(n1) return function(n2) return n1 / n2 end end
  }
end)()
local Data_EuclideanRing_I_intDegree = Data_EuclideanRing_I_foreign.intDegree
local Data_EuclideanRing_I_intDiv = Data_EuclideanRing_I_foreign.intDiv
local Data_EuclideanRing_I_intMod = Data_EuclideanRing_I_foreign.intMod
local Data_EuclideanRing_I_numDiv = Data_EuclideanRing_I_foreign.numDiv
local Effect_I_foreign = (function()
  return {

    pureE = function(a)
      return function()
        return a
      end
    end

    , bindE = function(a)
      return function(f)
        return function()
          return f(a())()
        end
      end
    end

    , untilE = function(f)
      return function()
        while not f() do end
      end
    end

    , whileE = function(f)
      return function(a)
        return function()
          while f() do
            a()
          end
        end
      end
    end

    , forE = function(lo)
      return function(hi)
        return function(f)
          return function()
            for i = lo, hi do
              f(i)()
            end
          end
        end
      end
    end

    , foreachE = function(as)
      return function(f)
        return function()
          for i, v in ipairs(as) do
            f(v)()
          end
        end
      end
    end

  }
end)()
local Effect_I_pureE = Effect_I_foreign.pureE
local Effect_I_bindE = Effect_I_foreign.bindE
local Effect_I_untilE = Effect_I_foreign.untilE
local Effect_I_whileE = Effect_I_foreign.whileE
local Effect_I_forE = Effect_I_foreign.forE
local Effect_I_foreachE = Effect_I_foreign.foreachE
local Control_Applicative_I_pure = function(dict) return dict.pure end
local Control_Applicative_I_liftA1 = function(dictApplicative)
  return function(f)
    return function(a)
      return (function(dict)
        return dict.apply
      end)(dictApplicative.Apply0(Prim_I_undefined))(Control_Applicative_I_pure(dictApplicative)(f))(a)
    end
  end
end
local Control_Monad_I_ap = function(dictMonad)
  return (function()
    local bind = (function(dict)
      return dict.bind
    end)(dictMonad.Bind1(Prim_I_undefined))
    return function(f)
      return function(a)
        return bind(f)(function(fPrime)
          return bind(a)(function(aPrime)
            return Control_Applicative_I_pure(dictMonad.Applicative0(Prim_I_undefined))(fPrime(aPrime))
          end)
        end)
      end
    end
  end)()
end
local Effect_I_monadEffect
local Effect_I_bindEffect
local Effect_I_applicativeEffect
local Effect_I__S___lazy_functorEffect
local Effect_I__S___lazy_applyEffect
Effect_I_monadEffect = {
  Applicative0 = function(unused0) return Effect_I_applicativeEffect end,
  Bind1 = function(unused1) return Effect_I_bindEffect end
}
Effect_I_bindEffect = {
  bind = Effect_I_bindE,
  Apply0 = function(unused0) return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I_applicativeEffect = {
  pure = Effect_I_pureE,
  Apply0 = function(unused0) return Effect_I__S___lazy_applyEffect(0) end
}
Effect_I__S___lazy_functorEffect = _S___runtime_lazy("functorEffect")(function( unused0 )
  return { map = Control_Applicative_I_liftA1(Effect_I_applicativeEffect) }
end)
Effect_I__S___lazy_applyEffect = _S___runtime_lazy("applyEffect")(function( unused1 )
  return {
    apply = Control_Monad_I_ap(Effect_I_monadEffect),
    Functor0 = function(unused0) return Effect_I__S___lazy_functorEffect(0) end
  }
end)
return {
  main = (function(dictApplicative)
    return Control_Applicative_I_pure(dictApplicative)(Data_Unit_I_unit)
  end)(Effect_I_applicativeEffect)
}
