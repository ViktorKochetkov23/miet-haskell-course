module SimpleLang where
-- Язык Simple -- очень простой императивный язык.
-- В нём только один тип данных: целые числа.

data Expression =
    Var String                   -- Переменные
  | Val Int                      -- Целые константы
  | Op Expression Bop Expression -- Бинарные операции
  deriving (Show, Eq)

data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt       -- >
  | Ge       -- >=
  | Lt       -- <
  | Le       -- <=
  | Eql      -- ==
  deriving (Show, Eq)

data Statement =
    -- присвоить переменной значение выражения
    Assign   String     Expression
    -- увеличить переменную на единицу
  | Incr     String
    -- ненулевые значения работают как истина в if, while и for
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
    -- как { ... } в C-подобных языках
  | Block [Statement]
    -- пустая инструкция
  | Skip
  deriving (Show, Eq)

-- примеры программ на этом языке в конце модуля

-- по состоянию можно получить значение каждой переменной
-- (в реальной программе скорее использовалось бы Data.Map.Map String Int)
type State = String -> Int

boolInt :: Bool -> Int
boolInt cond = if cond then 1 else 0

-- Задание 1 -----------------------------------------

-- в начальном состоянии все переменные имеют значение 0
empty :: State
empty _ = 0

-- возвращает состояние, в котором переменная var имеет значение newVal,
-- все остальные -- то же, что в state
extend :: State -> String -> Int -> State
extend state var newVal = \variable -> (if var == variable then newVal else state variable)

-- Задание 2 -----------------------------------------

-- возвращает значение выражения expr при значениях переменных из state.
eval :: State -> Expression -> Int
eval state expr = case expr of
  Var s -> state s
  Val i -> i
  Op left_expr op right_expr -> case op of
    Plus -> left + right
    Minus -> left - right
    Times -> left * right
    Divide -> left `div` right
    Gt -> boolInt (left > right)
    Ge -> boolInt (left >= right)
    Lt -> boolInt (left < right)
    Le -> boolInt (left <= right)
    Eql -> boolInt (left == right)
    where
      left = eval state left_expr
      right = eval state right_expr

-- Задание 3 -----------------------------------------

-- Можно выразить Incr через Assign, For через While, Block через
-- последовательное выполнение двух инструкций (; в C).
-- Следующий тип задаёт упрощённый набор инструкций (промежуточный язык Simpler).
data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

-- упрощает программу Simple
desugar :: Statement -> DietStatement
desugar stmt = case stmt of
  Assign var expr -> DAssign var expr
  Incr var -> DAssign var (Op (Var var) Plus (Val 1))
  If expr stmt1 stmt2 -> DIf expr (desugar stmt1) (desugar stmt2)
  While expr body -> DWhile expr (desugar body)
  For init expr next body -> DSequence (desugar init) (DWhile expr (DSequence (desugar body) (desugar next)))
  Block [] -> DSequence DSkip DSkip
  Block (stmt1:stmts) -> DSequence (desugar stmt1) (desugar (Block stmts))
  Skip -> DSkip

-- Задание 4 -----------------------------------------

-- принимает начальное состояние и программу Simpler
-- и возвращает состояние после работы программы
runSimpler :: State -> DietStatement -> State
runSimpler state dstmt = case dstmt of
  DAssign var expr -> extend state var (eval state expr)
  DIf expr dstmt1 dstmt2 -> if (eval state expr) /= 0 then runSimpler state dstmt1 else runSimpler state dstmt2
  DWhile expr body -> if (eval state expr) /= 0 then runSimpler state (DSequence body dstmt) else runSimpler state DSkip
  DSequence dstmt1 dstmt2 -> runSimpler (runSimpler state dstmt1) dstmt2
  DSkip -> state

--
-- in s "A" ~?= 10

-- принимает начальное состояние и программу Simple
-- и возвращает состояние после работы программы
run :: State -> Statement -> State
run state stmt = runSimpler state (desugar stmt)

-- Программы -------------------------------------------

{- Вычисление факториала

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Вычисление целой части квадратного корня

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = Block [Assign "Out" (Val 0),
  While (Op (Var "In") Ge (Op (Var "Out") Times (Var "Out"))) (Incr "Out"),
  Assign "Out" (Op (Var "Out") Minus (Val 1))]

{- Вычисление числа Фибоначчи

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = Block [Assign "F0" (Val 1),
                   Assign "F1" (Val 1),
                   If (Op (Var "In") Eql (Val 0))
                      (Assign "Out" (Var "F0"))
                      (If (Op (Var "In") Eql (Val 1))
                         (Assign "Out" (Var "F1"))
                         (For (Assign "C" (Val 2))
                             (Op (Var "C") Le (Var "In"))
                             (Incr "C")
                             (Block [Assign "T" (Op (Var "F0") Plus (Var "F1")),
                                     Assign "F0" (Var "F1"),
                                     Assign "F1" (Var "T"),
                                     Assign "Out" (Var "T")]
                             )
                         )
                      )]
