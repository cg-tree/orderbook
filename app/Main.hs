
module Main where

main :: IO ()
main = putStrLn . show $ testMarket0
--processOrderBatch testOrders initialState

initialState = (([],[]),[])
testOrders = [ Limit s 1 p 1 | s<-[Buy, Sell], p <-[1,3]]
test1 = [Market s 0 1|s<-[Buy,Sell] ]

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

testMarket0 = a
  where
    a = processOrderBatch [Market Buy 1 1] initialState

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
isMarket :: Order ->state-> Bool
isMarket order state = case order of
  Market _ _ _ -> True
  Limit side _ price _ -> case side of
    Buy -> price >= ask
    Sell -> price <= bid
bestBid :: state -> Float
bestBid (book, _) = price
  where
  (bids, _) = book
  (bid:_) = bids
  (_,_,price,_) = bid
bestAsk :: state -> Float
bestAsk (book, _) = price
  where
  (asks, _) = book
  (ask:_) = asks
  (_,_,price,_) = ask
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

{-order2lim :: Order -> Limit
order2lim order = case order of
  Limit side a b c -> case side of
    Buy -> Bid a b c
    Sell -> Ask a b c
-}
orderHead [] = None
orderHead (x:_)= x

orderTail [] = [None]
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


--marketbuy :: ([(b,d,c)],[(b,d,c)])-> (b,c) -> ([(b,d,c)],[(b,d,c)]) [(b,b,d,c)]
--marketbuy book (id, 0) = book []
{--marketbuy (bids, (aid, price, qty):[]) (id, quantity) = (bids, newasks) trades
  where
    minq = min qty quantity
    newqty = (max qty quantity) - minq
    newasks
      | qty > quantity = [(aid, price, newqty)]
      | otherwise = []
    trades = [(id, aid, price, minq )]
--}
{--marketbuy (bids, asks) (id, quantity) = (bids, newasks) trades
  where
    ntake = totake asks quantity
    fulltrades = [(id, aid, price, q) | (aid, price, q) <- (take ntake asks)]
    nqty = quantity - (sum [q | (_,_,_,q) <- fulltrades])
    nasks = drop ntake asks
    
    traderemainder = [(id,aid,price,nqty)|(aid, price, _)<-[(head nasks)], nqty > 0]
    askremainder = [(aid,price, qty - nqty)|(aid, price, qty)<-[(head nasks)], qty - nqty > 0]
    
    trades = fulltrades ++ traderemainder
    newasks = askremainder ++ (tail nasks)

marketsell :: ([(b,d,c)],[(b,d,c)])-> (b,c) -> ([(b,d,c)],[(b,d,c)]) [(b,b,d,c)]
marketsell (bids, asks) (id, quantity) = (newbids, asks) trades
  where
    ntake = totake bids quantity
    fulltrades = [(bid, id, price, q) | (bid, price, q) <- (take ntake bids)]
    nqty = quantity - (sum [q | (_,_,_,q) <- fulltrades])
    nbids = drop ntake bids
    
    traderemainder = [(bid,id,price,nqty) | (bid, price, _) <- [(head nbids)], nqty > 0]
    bidremainder = [(bid,price, qty - nqty) | (bid, price, qty) <- [(head nbids)], qty - nqty > 0]
    
    trades = fulltrades ++ traderemainder
    newbids= bidremainder ++ (tail nbids)

market (bids, asks) (id, side, quantity)
  | side == "buy" = marketbuy (bids, asks) (id, quantity)
  | otherwise = marketsell (bids, asks) (id, quantity)

--}
