module LambdaInterp where

    type Name = Char

    --Interp. Data repsrentation for terms
    data Expr = Var Name | Lambda Name Expr | App Expr Expr deriving (Show)

    --Substitution. Absrt
    subst :: Name -> Expr -> Expr -> Expr
    subst x newVal (Lambda y body)
        | x == y = Lambda y (subst x newVal body)
        | otherwise = Lambda y body
    subst x newVal (App e1 e2) = App (subst x newVal e1) (subst x newVal e2)
    subst x newVal (Var y)
        | x == y = newVal
        | otherwise = Var y

    eval :: Expr -> Expr
    eval (Lambda x e) = Lambda x (eval e)
    eval (Var x) = Var x
    eval (App e1 e2) = case (eval e1) of
        Lambda x body -> eval (subst x e1 body)
        res -> App res e2

    main :: IO()
    main = do 
        print (eval (Lambda 'x' (App ((Lambda 'y') (Var 'z')) (Var 'n'))))
