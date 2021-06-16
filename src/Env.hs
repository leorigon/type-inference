module Env where

import Type (Literal (Bool, Int), SimpleType (TApp, TArr, TCon, TLit, TVar))

env =
  [ ("True", TLit Bool),
    ("False", TLit Bool),
    ("+", TArr (TLit Int) (TArr (TLit Int) (TLit Int))),
    ("-", TArr (TLit Int) (TArr (TLit Int) (TLit Int))),
    ("*", TArr (TLit Int) (TArr (TLit Int) (TLit Int))),
    ("/", TArr (TLit Int) (TArr (TLit Int) (TLit Int))),
    ("<", TArr (TLit Int) (TArr (TLit Int) (TLit Bool))),
    (">", TArr (TLit Int) (TArr (TLit Int) (TLit Bool))),
    ("<=", TArr (TLit Int) (TArr (TLit Int) (TLit Bool))),
    (">=", TArr (TLit Int) (TArr (TLit Int) (TLit Bool))),
    ("==", TArr (TLit Int) (TArr (TLit Int) (TLit Bool))),
    ("/=", TArr (TLit Int) (TArr (TLit Int) (TLit Bool))),
    ("Just", TArr (TVar "a") (TApp (TCon "Maybe") (TVar "a"))),
    ("Nothing", TApp (TCon "Maybe") (TVar "a")),
    ( "Right",
      TArr (TVar "b") (TApp (TApp (TCon "Either") (TVar "a")) (TVar "b"))
    ),
    ( "Left",
      TArr (TVar "a") (TApp (TApp (TCon "Either") (TVar "a")) (TVar "b"))
    )
  ]