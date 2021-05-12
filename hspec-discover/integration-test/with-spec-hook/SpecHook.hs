module SpecHook where
hook :: SpecWith Int -> Spec
hook = before (return 23)
