-- David Bergevoet	s1043736
-- Harm van der Heide s1047460
module Hardware
where

data Bit  =  O | I
  deriving (Eq, Ord, Show)

-- AND operator between 2 bits
infixr 3 ∧∧
(∧∧) :: Bit -> Bit -> Bit
O ∧∧ _b  =  O
I ∧∧ b   =  b

-- OR operator for 2 bits
infixr 2 <#>
(<#>) :: Bit -> Bit -> Bit
O <#> b   =  b
I <#> _b  =  I

-- XOR operator for 2 bits
infixr 4 ><
(><) :: Bit -> Bit -> Bit
O >< O  =  O
O >< I  =  I
I >< O  =  I
I >< I  =  O

func :: (Bit,Bit) -> (Bit,Bit)
func (x,s) = ((s <#> x), (s<#>x))

mapr :: ((a, state) -> (b, state)) -> (([a], state) -> ([b], state))
mapr _ ([], s) = ([], s)
mapr f ((x : xs), s) =
  let (r, s') = f (x, s) in
  let (rs, s'') = mapr f (xs, s') in
  ((r : rs), s'')

type Carry  =  Bit

halfAdder :: (Bit, Bit) -> (Bit, Carry)
halfAdder (a,b) = (a >< b, a ∧∧ b)

fullAdder :: ((Bit, Bit), Carry) -> (Bit, Carry)
fullAdder ((a,b),c) = let (s1,c1) = halfAdder(a,b) in 
 let (s2,c2) = halfAdder (s1,c) in
 (s2, c1 <#> c2)
