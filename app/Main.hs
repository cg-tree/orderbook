
module Main where

main :: IO ()
main = putStrLn (if and tests then "passed all tests" else show tests)
--processOrderBatch testOrders initialState

initialState = (([],[]),[])
testOrders = [ Limit s 1 p 1 | s<-[Buy, Sell], p <-[1,3]]
test1 = [Market s 0 1|s<-[Buy,Sell] ]

tests = [testBidEnque0,testBidEnque1,testAskEnque0,testAskEnque1,testMarket0,testMarket1  ]
-- test ordering when prices are the same
testBidEnque0 = a == b
  where
    a = processOrderBatch [Limit Buy i 1 1|i<-[1..5] ] initialState
    b = (([Limit Buy 1 1.0 1.0,Limit Buy 2 1.0 1.0,Limit Buy 3 1.0 1.0,Limit Buy 4 1.0 1.0,Limit Buy 5 1.0 1.0],[]),[])

-- test ordering when prices differ
testBidEnque1 = a == b
  where
    a = processOrderBatch [Limit Buy 0 i 1|i<-[1..5] ] initialState
    b = (([Limit Buy 0 5.0 1.0,Limit Buy 0 4.0 1.0,Limit Buy 0 3.0 1.0,Limit Buy 0 2.0 1.0,Limit Buy 0 1.0 1.0],[]),[])

-- test ordering when prices are the same
testAskEnque0 = a == b
  where
    a = processOrderBatch [Limit Sell i 1 1|i<-[1..5] ] initialState
    b = (([],[Limit Sell 1 1.0 1.0,Limit Sell 2 1.0 1.0,Limit Sell 3 1.0 1.0,Limit Sell 4 1.0 1.0,Limit Sell 5 1.0 1.0]),[])

-- test ordering when prices differ
testAskEnque1 = a == b
  where
    a = processOrderBatch [Limit Sell 0 i 1|i<-[1..5] ] initialState
    b = (([],[Limit Sell 0 1.0 1.0,Limit Sell 0 2.0 1.0,Limit Sell 0 3.0 1.0,Limit Sell 0 4.0 1.0,Limit Sell 0 5.0 1.0]),[])

testMarket0 = a == initialState
  where
    a = processOrderBatch [Market Buy 1 1] initialState

testMarket1 = a == b
  where
    a = processOrderBatch [Market Buy 1 1.5] (([],[Limit Sell 0 1.0 1.0,Limit Sell 0 2.0 1.0,Limit Sell 0 3.0 1.0,Limit Sell 0 4.0 1.0,Limit Sell 0 5.0 1.0]),[])
    b = (([],[Limit Sell 0 2.0 0.5,Limit Sell 0 3.0 1.0,Limit Sell 0 4.0 1.0,Limit Sell 0 5.0 1.0]),[(Market Buy 1 0.5,Limit Sell 0 2.0 0.5),(Market Buy 1 1.0,Limit Sell 0 1.0 1.0)])


--An Exchange is a match-making queue that extracts a fee.
--An Exchange is a match-making queue that extracts a fee.
--Opposing orders are placed until a match occurs.
--A match occurs when a purchase and sale occur at the same price.
--A match always occurs between a maker and a taker.
--The maker has a limit order placed at a predetermined price
--which the market has not yet reached.
--The taker places a market order at the current bid or ask,
--and is always matched with the best offer.
--The maker and taker both pay a fee but the taker pays twice as much.
--Orders have types. The type is one of 
--{ Limit, Market, Stop, Trailing_Stop }
--A limit order set at a price that triggers immediate execution
--is a market order.
--a market order is a limit order that occurs at the sequence of
--prices that facilitates transacting the entire quantity immediately
--Orders have sides. The side is one of { Buy, Sell }
--Orders have pricing rules. rule:: Price -> Bool
--When a match occurs a number of orders are executed.
--Execution is the process of applying the appropriate
--credits and debits to the accounts involved. This includes
--collecting fees.
data Side = Buy | Sell
  deriving (Eq, Show)

data Order = Limit Side Int Float Float | Market Side Int Float | None
  deriving (Eq, Show)
type Book = ([Order], [Order])
type Trade = (Order, Order)
type State = (Book, [Trade])
{--


--}
processOrder:: Order -> State -> State
processOrder order = case order of
  None -> id
  Market _ _ _ ->  processMarket order
  Limit _ _ _ _ -> processLimitOrder order

processOrderBatch:: [Order]-> State -> State
processOrderBatch orders = compL fs
  where
    fs = map processOrder orders

compL :: [ a -> a ] -> a -> a
compL [] = id
compL (f:fs) = compL fs . f

orderHead [] = None
orderHead (x:_)= x

orderTail [] = []
orderTail (_:xs)= xs

processLimitOrder :: Order -> State -> State
processLimitOrder ( Limit side id price qty ) (book, trades) = nstate
  where
    limit = Limit side id price qty
    (bids, asks) = book
    maker = case side of
      Buy -> orderHead asks
      Sell -> orderHead bids
      
    (bid, ask) = case side of
      Buy -> (limit, orderHead asks)
      Sell -> (orderHead bids, limit)

    matches = isMatch bid ask
      
    (rtaker, rmaker, trade) = if matches then match limit maker else (limit, [maker], [] )

    partialbook = case side of
      Buy -> (bids,rmaker ++ (orderTail asks))
      Sell -> (rmaker ++ (orderTail bids), asks)
     
    nstate = if matches then processOrder rtaker (partialbook, trade ++ trades) else (enqueueLimit limit book,trades)


processMarket :: Order -> State -> State
processMarket (Market side id qty) (book,trades) = processOrder rtaker partialstate
  where
    (bids, asks) = book
    maker = case side of
      Buy -> orderHead asks
      Sell -> orderHead bids
      
    (rtaker, rmaker, trade) = match (Market side id qty) maker
    
    partialbook = case side of
      Buy -> (bids,rmaker ++ (orderTail asks))
      Sell -> (rmaker ++ (orderTail bids), asks)
    
    partialstate = (partialbook, trade ++ trades)
  

execute :: state -> state
execute = id

limitInsertOp (Limit side _ price _) = (\(Limit _ _ x _) -> if side == Buy then (x>=price) else (x<=price))

state2book :: State -> Book
state2book (book, _) = book

book2asks :: Book -> [Order]
book2asks (_,xs) = xs

book2bids:: Book -> [Order]
book2bids (xs,_) = xs

enqueueLimit :: Order -> Book -> Book
enqueueLimit (Limit side id price quantity) book =
  if  side == Buy then ( (insert op limit bids), asks)
  else (bids, (insert op limit asks) )
  where
    bids = book2bids book
    asks = book2asks book
    limit = Limit side id price quantity
    op = limitInsertOp limit
    

enqueue (bids, asks) (id, side, price, quantity)
    | side == "buy" =( (insert (\(_,b,_)-> price <= b ) (id, price, quantity) bids), asks)
    | otherwise = (bids, (insert (\(_,a,_)-> price >= a ) (id, price, quantity) asks) )

insert op x [] = [x]
insert op x xs = ys
  where
    ys = (takeWhile op xs) ++[x]++ (dropWhile op xs)

maker id offset price = [Limit Buy id ( price - offset ) 1, Limit Sell id ( price + offset ) 1]

--totake [] _ = 0
totake ((_,_,q):xs) qty
  | and [(qty - q) >= 0, qty > 0] = 1 + totake xs (qty - q)
  | otherwise = 0

isMatch :: Order -> Order -> Bool
isMatch None _ = False
isMatch _ None = False
isMatch (Market _ _ _) _ = True
isMatch _ (Market _ _ _) = True
isMatch bid ask = priceb >= pricea
  where
    Limit Buy idb priceb qtyb = bid
    Limit Sell ida pricea qtya = ask


matchOrders :: State -> State
matchOrders (book,trades) = (book,trades)
  where
   bids = book2bids book
   asks = book2asks book
   bid = orderHead bids
   ask = orderHead asks

   Limit Buy idb priceb qtyb = bid
   Limit Sell ida pricea qtya = ask
   qty = min qtyb qtya
   price = (priceb + pricea) / 2
   
--returns remaining portion of orders 

match :: Order -> Order -> (Order, [Order], [Trade])
match _ None = (None, [], [])
match None None = (None, [], [])
match None rmaker = (None, [rmaker], [])
match taker maker = (rtaker, rmaker, [(etaker, emaker)])
  where
    Limit sm im pm qm = maker
    qt = case taker of
      Limit _ _ _ q -> q
      Market _ _ q -> q
    qty = min qt qm
    price = pm
    rtaker = if qty == qt then None else case taker of
      Limit st it pt qt -> Limit st it pt (qt-qty)
      Market st it qt -> Market st it (qt-qty)
    etaker = case taker of
      Limit st it pt qt -> Limit st it price qty
      Market st it qt -> Market st it qty
    rmaker = if qty == qm then [] else [Limit sm im pm (qm-qty) ]
    emaker = Limit sm im pm qty


