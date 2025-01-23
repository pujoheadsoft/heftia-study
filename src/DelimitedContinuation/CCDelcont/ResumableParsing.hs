{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module DelimitedContinuation.CCDelcont.ResumableParsing (program) where

import Control.Monad.CC (MonadDelimitedCont, shift, reset, runCC)

{-
次の例は、OCamlメーリングリストへの投稿のHaskellバージョンに関するものだ。
この訳はもともとhaskell-cafeメーリングリストに投稿されたもので、完全なコードと追加的な議論はそちらで見ることができる。
この問題は上記のイテレータの例と似ている。
具体的には、一度に入力の断片を受け取り、断片が提供されるまで、それぞれの断片の後にさらなる入力を中断できるパーサーが必要なのだ。
しかし、優れたパーシング・ライブラリはすでにたくさんあるので、この再開可能なパーサー機能のためだけに、新しいライブラリをゼロから書き直す必要はない。
結局のところ、限定継続は、このような場合に、ケーキを食べながらケーキを食べるような、とても簡単な方法を提供してくれる。
まず、resume可能なparser用のデータ型が必要だ。
-}
data Request m a
  = Done a
  | ReqChar (Maybe Char -> m (Request m a))

{-
このようなパーサーは、完全であるか、より多くの文字を要求している状態である。
ここでも、データ型を操作するための便利な関数がいくつか用意されている：
-}
provide :: Monad m => Char -> Request m a -> m (Request m a)
provide _ d@(Done _)  = return d
provide c (ReqChar k) = k (Just c)

provideString :: Monad m => String -> Request m a -> m (Request m a)
provideString []     s = return s
provideString (x:xs) s = provide x s >>= provideString xs

finish :: Monad m => Request m a -> m (Request m a)
finish d@(Done _)  = return d
finish (ReqChar k) = k Nothing

{-
つまり、'provide'はパーサーに文字を送り、'provideString'は文字列を送り、'finish'はもう文字がないことをパーサーに知らせる。
最後に、パーシングを中断して文字を待つ方法が必要だ。
これがまさに区切り文字による継続処理である。 パーサーを制御するために使うフックは、入力として受け取る文字ストリームである：
-}
toList :: Monad m => m (Maybe a) -> m [a]
toList gen = gen >>= maybe (return []) (\c -> (c:) <$> toList gen)

streamInvert :: MonadDelimitedCont p s m => p (Request m a) -> m (Maybe Char)
streamInvert p = shift p (\k -> return $ ReqChar (k . return))

invertParse :: MonadDelimitedCont p s m => (String -> a) -> m (Request m a)
invertParse parser = reset $ \p -> Done . parser <$> toList (streamInvert p)

{-
つまり、'toList'は単に、文字を生成する可能性のあるモナディック・アクショ ンを受け取り、それを使って文字のリストを生成する（'Nothing'を見つけたら停止する）。
streamInvert」はまさにそのようなモナドで、文字を生成するアクションである（区切り文字が与えられる）。
実行されるたびに、このアクションはサブ継続（ここでは「リスト生成の残り」）をキャプチャし、それをRequestオブジェクトに入れる。
その後、Requestオブジェクトを渡して、(上記の'provide'と'provideString'を使って) 望みの文字を送り込み、解析される文字のリストを徐々に作っていくことができる。

invertParse'メソッドでは、この徐々に生成されるリストがパーサー (String -> a型なので、私たちが使っている区切り継続モナドを知る必要はない)に渡される。
パーサーの出力は終了した(Done)Requestオブジェクトにパッケージされるので、 最後に'finish'を呼び出すと、パーサーの結果にアクセスできるようになる。
この例では、words関数がパーサーとして十分である：
-}
gradualParse :: [String]
gradualParse = runCC $ do p1 <- invertParse words
                          p2 <- provideString "The quick" p1
                          p3 <- provideString " brown fox jum" p2
                          p4 <- provideString "ps over the laz" p3
                          p5 <- provideString "y dog" p4 >>= finish
                          p6 <- provideString "iest dog" p4 >>= finish
                          let (Done l1) = p5
                              (Done l2) = p6
                          return (l1 ++ l2)

program :: IO ()
program = mapM_ putStrLn gradualParse