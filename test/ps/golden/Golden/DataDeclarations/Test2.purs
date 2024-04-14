module Golden.DataDeclarations.Test2 where

import Golden.DataDeclarations.Test1 as TDD1

data TySameName = CtorSameName

test :: TDD1.TySameName -> TySameName -> Boolean
test TDD1.CtorSameName CtorSameName = true

