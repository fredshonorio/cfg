module Plan (
  Plan
  , always, never, unless
  , run, runAll
  , flatten
  , when, whenNot
  ) where

data Plan = Plan { skip :: IO Bool
                 , action :: IO ()
                 }

always :: IO () -> Plan
always op = Plan (return False) op

never :: IO () -> Plan
never op = Plan (return True) op

-- document this or rename, this is just whenNot but lower level
unless :: IO Bool -> IO () -> Plan
unless skip op = Plan skip op

when :: IO Bool -> Plan -> Plan
when whenTrue plan =
  let
    skipA = not <$> whenTrue -- flip the predicate
    skipB = skip plan
    skip' = (||) <$> skipA <*> skipB
  in
    Plan skip' (action plan)

whenNot :: IO Bool -> Plan -> Plan
whenNot whenFalse plan =
  when (not <$> whenFalse) plan

run :: Plan -> IO ()
run plan = do
  skip <- skip plan
  if skip
    then return ()
    else action plan

runAll :: [Plan] -> IO ()
runAll plans =
  mapM_ run plans

flatten :: [Plan] -> Plan -- there's a dang monoid around here
flatten plans = always (runAll plans)
