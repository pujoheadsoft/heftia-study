{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}
module Control.Monad.CCSpec where

import Prelude hiding (either, any)
import Test.Hspec
import Control.Monad.CC
import Control.Monad.IO.Class (liftIO, MonadIO)
import GHC.IO (evaluate)
import Test.MockCat

{-
[CC.hsのコメントの内容]

$Examples
このモジュールは、多くの異なる制御演算子を提供しており、それらから適切なものを選ぶ際に、ここで示す例が役立つことを願っています。
最も基本的なのは、MonadDelimitedCont型クラスに含まれる4つの演算子です。
最初のものはもちろんnewPromptで、これは十分直感的に理解できるでしょう。
次にpushPromptがあり、これは計算を区切る基本操作です。
ほかの制御演算子がない場合、これは単なる無操作（no-op）です。つまり：

> pushPrompt p (return v) == return v

withSubContは部分継続をキャプチャするための基本操作です。
callCCとは異なり、withSubContはキャプチャした区切られた継続を中断します。
したがって：
> pushPrompt p ((1:) `liftM` (2:) `liftM` withSubCont p (\k -> return []))
を実行すると、[1, 2]ではなく[]が結果となります。

最後の基本的な制御演算子はpushSubContで、これはwithSubContでキャプチャした部分継続を利用するために使用されます。
例えば：
> pushPrompt p ((1:) `liftM1 (2:) `liftM`
                withSubCont p (\k -> pushSubCont k (return [])))
を実行すると、[1, 2]という結果が得られます。
しかしながら、部分継続をキャプチャしてすぐにプッシュする操作は無操作ではありません。
部分継続には区切りプロンプトが含まれておらず、pushSubContも元のプロンプトを再設定しません。
そのため、部分継続をキャプチャしてプッシュすると、1つの区切りが失われます。
この効果を打ち消すには、プロンプトを再度プッシュする必要があります。

これら4つの基本操作を基に、1つまたは複数の操作を統合したさまざまな機能的抽象が構築されています。
区切りに関連するものとしてresetがあり、これはプロンプトの作成と区切りを1つにまとめたものです。
このテーマに関する論文（例: Shift to Control）では、各キャプチャ演算子に対応する区切り演算子がペアになっていることがあります。
しかし、この実装ではプロンプトが明示的に渡されるため、単一の区切り演算子で全てのキャプチャ演算子をサポートできます
（ただし、プロンプトを複数回明示的にプッシュする場合はpushPromptを使用する必要があります）。

最も単純な制御フロー演算子はabortで、その名前が示す通り、指定された部分継続を単純に中断します。
たとえば、上記の2番目の例は次のように書き直せます：
> pushPrompt p ((1:) `liftM` (2:) `liftM` abort p (return []))


残りの関数は、部分継続を関数として具象化し、それを使用可能にします。
shiftやcontrolといった制御演算子は、この点で類似していますが、計算の異なる部分をどのように区切るかに違いがあります。
以下の説明を理解しやすくするためにいくつか名前を付けます：

> shift p (\f -> e)

ここで：
/p/: プロンプト
/f/: 具象化された継続
/e/: 中断されたコンテキストで実行される計算
これらを念頭に置いて、制御演算子の動作を以下に示します：

shift: /e/と/f/の全ての呼び出しを区切ります。そのため、shiftを使用すると、制御効果が区切りを脱出することはありません。

次のような計算：
> reset (\p -> <computations with shift p>)
は外部から見ると純粋（pure）に見えます。

control: /e/を区切りますが、/f/内の部分継続は区切りません。
したがって、/f/の部分継続に他のcontrol呼び出しが含まれている場合、その効果は囲まれた区切りを脱出する可能性があります。
たとえば：
> reset (\p -> shift p (\f -> (1:) `liftM` f (return [])) >>= \y -> shift p (\_ -> return y))
の結果は[1]ですが、shiftをcontrolに置き換えると結果は[]になります。

shift0: /f/を区切りますが、/e/は区切りません。
したがって：
> reset (\p -> (1:) `liftM` pushPrompt p
                            (shift0 p (\_ -> shift0 p (\_ -> return []))))
の結果は[]です。
一方でshiftを使用すると[1]になります。

control0: /e/も/f/も区切りません。
これは、withSubContとpushSubContを直接使用する具象化されたアナログです。

これら4つの制御演算子についてより詳細な議論は、Chung-chieh Shanの「Shift to Control」を参照してください。

小さな例
以下は、単純な例として、モナディックループをイテレータオブジェクトに具象化する方法を示します：

data Iterator r a = I a (CC r (Iterator r a)) | Done

current :: Iterator r a -> Maybe a
current (I a _) = Just a
current Done    = Nothing

next :: Iterator r a -> CC r (Iterator r a)
next (I _ m) = m
next Done    = return Done

iterator :: ((a -> CC r ()) -> CC r ()) -> CC r (Iterator r a)
iterator loop = reset $ \p ->
                 loop (\a ->
                    shift p $ \k ->
                        return $ I a (k $ return ())) >> return Done

test = do i <- iterator $ forM_ [1..5]
          go [] i
 where
 go l Done = return l
 go l i    = do let (Just a) = current i
                    l' = replicate a a ++ l
                i' <- next i
                go l' i'
この結果は、以下のように期待される動作になります：

*Test> runCC test
[5,5,5,5,5,4,4,4,4,3,3,3,2,2,1]
-}

spec :: Spec
spec = do
  describe "moduleの説明に書かれていたコードのテスト" do
    it "pushPrompt" do
      runCC (newPrompt >>= \p -> pushPrompt p (pure "x")) `shouldBe` "x"

    it "reset" do
      -- reset e = newPrompt >>= \p -> pushPrompt p (e p) なので次のように書き換えられる
      -- 今後同じコードはこの形で書く
      -- 他の制御オペレーターが存在しないため何もしない操作に等しい
      runCC (reset \_ -> pure "x") `shouldBe` "x"

    it "withSubCont" do
      {-
        withSubCont は部分継続をキャプチャすることを可能にする基本的な操作です。
        callCC とは異なり、withSubCont はキャプチャした区切られた継続を中断します。
        したがって次は、実行すると [1, 2] ではなく [] を結果として返します。
      -}
      runCC (reset \p -> (1:) <$> (2:) <$> withSubCont p (\_ -> return [])) `shouldBe` []

    it "pushSubCont" do
      {-
        最後の基本的な制御オペレーターは pushSubCont で
        これは withSubCont を使用してキャプチャされた部分継続を利用することを可能にします。
      -}
      runCC (reset \p -> (1:) <$> (2:) <$> withSubCont p (\k -> pushSubCont k (return []))) `shouldBe` [1, 2]

    it "pushPromptせずにwithSubContを使うとエラーになる" do
      let
        r = runCC $ newPrompt >>= \p -> withSubCont p \_ -> pure 3
        -- こうすればいける(これはreset \p -> abort p xxと同じ)
        -- r = runCC $ newPrompt >>= \p -> pushPrompt p $ withSubCont p \_ -> pure 3
      r `shouldThrow` errorCall "Prompt was not found on the stack."

  describe "限定継続のテスト" do
    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> do
            s <- shift p $ \k -> k $ pure (5 * 2)
            pure $ 3 + s - 1 -- 3 + 10 - 1
      r `shouldBe` 12

    it "継続を破棄することができる(shift版)" do
      let r = runCC $ reset $ \p -> do
            s <- shift p $ \_ -> pure $ 5 * 2
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄される
      r `shouldBe` 10

    it "pushPromptで計算を区切った中のshiftで継続を破棄しても外側の継続は破棄されない" do
      let r = runCC $ reset $ \p -> do
            s <- pushPrompt p $ shift p $ \_ -> pure $ 5 * 2
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄されない
      r `shouldBe` 12

    it "pushPromptで計算を区切った中のshiftで継続を破棄しても外側の継続は破棄されないが、内側の継続は破棄される" do
      let r = runCC $ reset $ \p -> do
            -- 継続 [..] * 10 は破棄される
            s <- pushPrompt p ((* 10) <$> shift p (\_ -> pure $ 5 * 2))
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄さない
      r `shouldBe` 12

    it "継続を破棄することができる(abort版)" do
      let r = runCC $ reset $ \p -> do
            s <- abort p $ pure $ 5 * 2
            pure $ 3 + s - 1 -- 継続部分 3 + [..] - 1は破棄される
      r `shouldBe` 10

    it "継続を使って実行順序を変更できる" do
      let r = runCC $ reset $ \p -> do
            s <- shift p \k -> (* 2) <$> k (pure 3)
            pure $ 1 + s
      r `shouldBe` 8 -- 2 * 3 + 1 に見えるが、(1 + 3) * 2 になる

    it "継続を複数回使うことができる" do

      printMock <- createMock $ any @String |> pure @IO ()

      let
        printStub = stubFn printMock

        -- 継続kを2回使う
        either p a b = shift p \k -> k a >> k b

        -- resetの中ではprintStubは一回の呼び出しに見える
        r :: (MonadDelimitedCont p s m, MonadIO m) => m ()
        r = reset \p -> do
          x <- either p (pure "a") (pure "b")
          liftIO $ printStub x
          pure ()

      (runCCT r >>= evaluate) `shouldReturn` ()

      -- printStubは2回呼ばれている
      printMock `shouldApplyInOrder` [ "a", "b" ]

    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> do
            k <- shift p $ \k -> do
              l <- shift p $ \l -> k (l (pure 5))
              pure $ 2 * l
            pure $ 1 + k
      r * 3 `shouldBe` 33

    it "継続を使って計算することができる" do
      let r = runCC $ reset $ \p -> pushPrompt p do
            k <- shift0 p $ \k -> do
              l <- shift0 p $ \l -> k (l (pure 5))
              pure $ 2 * l
            pure $ 1 + k
      r * 3 `shouldBe` 33

    it "継続を取り出すことができる" do
      x <- runCCT $ reset \p -> do
        k <- shift p \k -> k (pure id)
        pure $ (+ 3) <$> k <$> (* 10)
      x 3 `shouldBe` 33

    {-
       【shiftたちの違い】
       どれもwithSubContにより継続をキャプチャしている。
       0がつかないものはまずpushPromptで区切っているが、0がついているものは区切っていない。
       また、どれもwithSubContして、pushSubContしている。
       違いは、どのタイミングでpushPromptしているかである。

       shift    p f = withSubCont p $ \sk -> pushPrompt p $ f (\a -> pushPrompt p $ pushSubCont sk a)
       shift0   p f = withSubCont p $ \sk ->                f (\a -> pushPrompt p $ pushSubCont sk a)
       control  p f = withSubCont p $ \sk -> pushPrompt p $ f (\a -> pushSubCont sk a)
       control0 p f = withSubCont p $ \sk ->                f (\a -> pushSubCont sk a)

       【abortというものもある】
       abort    p e = withSubCont p (\_ -> e)
    -}
    describe "shiftとshift0の違い" do
      it "pushPromptで区切らない場合同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> shift  p (\_ -> pure 2))
          r2 = runCC $ reset (\p -> (+ 1) <$> shift0 p (\_ -> pure 2))

        r1 `shouldBe` 2
        r2 `shouldBe` 2

      it "pushPromptで区切った場合も同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift  p (\_ -> pure 2)))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift0 p (\_ -> pure 2)))

        r1 `shouldBe` 3
        r2 `shouldBe` 3

      it "pushPromptで区切った中でネストした場合、shift0は外側の継続が破棄される" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift  p (\_ -> shift  p (\_ -> pure 2))))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift0 p (\_ -> shift0 p (\_ -> pure 2))))
          -- ↑ r2は継続 [..] + 1 が破棄される。↓のようにネストした中でpushPromptすればshift0でも外側の継続が破棄さない
          r3 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (shift0 p (\_ -> pushPrompt p $ shift0 p (\_ -> pure 2))))
        r1 `shouldBe` 3
        r2 `shouldBe` 2
        r3 `shouldBe` 3

      {-
         これらの結果については、いま一度shiftとshift0の違いを確認すればわかる。
         shiftでは中でpushPromptしているが、shift0では中でpushPromptしていない。
         だからshift0でも自前でpushPromptしてやれば継続が破棄されないわけだ。

         shift    p f = withSubCont p $ \sk -> pushPrompt p $ f (\a -> pushPrompt p $ pushSubCont sk a)
         shift0   p f = withSubCont p $ \sk ->                f (\a -> pushPrompt p $ pushSubCont sk a)
      -}

    describe "controlとcontrol0の違い" do
      it "pushPromptで区切らない場合同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> control  p (\_ -> pure 2))
          r2 = runCC $ reset (\p -> (+ 1) <$> control0 p (\_ -> pure 2))

        r1 `shouldBe` 2
        r2 `shouldBe` 2

      it "pushPromptで区切った場合も同じ結果になる" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control  p (\_ -> pure 2)))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control0 p (\_ -> pure 2)))

        r1 `shouldBe` 3
        r2 `shouldBe` 3

      it "pushPromptで区切った中でネストした場合、control0は外側の継続が破棄される" do
        let
          r1 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control  p (\_ -> control  p (\_ -> pure 2))))
          r2 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control0 p (\_ -> control0 p (\_ -> pure 2))))
          -- ↑ r2は継続 [..] + 1 が破棄される。↓のようにネストした中でpushPromptすればshift0でも外側の継続が破棄さない
          r3 = runCC $ reset (\p -> (+ 1) <$> pushPrompt p (control0 p (\_ -> pushPrompt p $ control0 p (\_ -> pure 2))))
        r1 `shouldBe` 3
        r2 `shouldBe` 2
        r3 `shouldBe` 3

    describe "shiftとcontrolの違い" do
      it "shift/control" do
        let
          -- control p (\f -> e)
          -- とおいたとき /e/を区切りますが、/f/内の部分継続は区切りません。
          -- したがって、/f/の部分継続に他のcontrol呼び出しが含まれている場合、それらの効果が外側の区切りを抜け出す可能性があります。
          rs = runCC $ reset (\p -> shift   p (\k -> (1:) <$> k (pure [])) >>= \y -> shift   p (\_ -> pure y))
          rc = runCC $ reset (\p -> control p (\k -> (1:) <$> k (pure [])) >>= \y -> control p (\_ -> pure y))

        rs `shouldBe` [1]
        rc `shouldBe` []

      it "shift/shift0/control/control0" do
        let
          -- 一番最初だけ全部異なる関数にした。滅茶苦茶見づらいけど全部異なる結果になる。
          s1 = runCC $ reset (\p -> (1:) <$> pushPrompt p (shift    p (\_ -> shift   p (\k -> (2:) <$> k (pure [])) >>= \y -> shift   p (\_ -> pure y))))
          s2 = runCC $ reset (\p -> (1:) <$> pushPrompt p (shift0   p (\_ -> shift   p (\k -> (2:) <$> k (pure [])) >>= \y -> shift   p (\_ -> pure y))))
          c1 = runCC $ reset (\p -> (1:) <$> pushPrompt p (control  p (\_ -> control p (\k -> (2:) <$> k (pure [])) >>= \y -> control p (\_ -> pure y))))
          c2 = runCC $ reset (\p -> (1:) <$> pushPrompt p (control0 p (\_ -> control p (\k -> (2:) <$> k (pure [])) >>= \y -> control p (\_ -> pure y))))

        s1 `shouldBe` [1, 2] -- shiftは外側も内側も破棄されない
        s2 `shouldBe` [2] -- shift0は外側の継続が破棄されるが、内側は破棄されない
        c1 `shouldBe` [1] -- controlは外側の継続が破棄されないが、内側は破棄される
        c2 `shouldBe` []  -- control0は外側も内側も破棄される
        {-
          shift p (\f -> e)
          /p/: プロンプト
          /f/: 具象化された継続
          /e/: 中断されたコンテキストで実行される計算
          として
          shift:    /e/と/f/の全ての呼び出しを区切ります。
          shift0:   /f/を区切りますが、/e/は区切りません。
          control:  /e/を区切りますが、/f/内の部分継続は区切りません。
          control0: /e/も/f/も区切りません。
          上記を実装と比べてみよう。
          shift    p x = withSubCont p $ \sk -> pushPrompt p $ x (\a -> pushPrompt p $ pushSubCont sk a)
          shift0   p x = withSubCont p $ \sk ->                x (\a -> pushPrompt p $ pushSubCont sk a)
          control  p x = withSubCont p $ \sk -> pushPrompt p $ x (\a -> pushSubCont sk a)
          control0 p x = withSubCont p $ \sk ->                x (\a -> pushSubCont sk a)

          基本的には、withSubCont と pushSubCont を用いて部分継続をキャプチャして利用している。これは全部共通。
          pushPrompt で区切る箇所が異なる。

          まず x は  ((m a -> m b) -> m b) という型の関数。したがって
          /f/: (m a -> m b) 具象化された継続
          /e/: m b          中断されたコンテキストで実行される計算
          である。
          /f/ に x を適用すると、/e/ が得られる。
          つまり x (\a -> pushPrompt p $ pushSubCont sk a) などは /e/ にあたり、pushPrompt p $ x /e/ というわけで、
          これは /e/ を区切る部分である。定義と実装もそうなっている。
          そして、(\a -> pushPrompt p $ pushSubCont sk a) などは /f/ にあたり、その中で pushPrompt しているのが /f/ を区切る部分である。
          これも定義と実装がその通りになっている。

          上記を踏まえてテストコードを見るとわかりやすいかもしれない
          reset (\p -> (1:) <$> pushPrompt p (shift    p (\_ -> shift   p (\k -> (2:) <$> k (pure [])) >>= \y -> shift   p (\_ -> pure y))))
          reset (\p -> (1:) <$> pushPrompt p (shift0   p (\_ -> shift   p (\k -> (2:) <$> k (pure [])) >>= \y -> shift   p (\_ -> pure y))))
          reset (\p -> (1:) <$> pushPrompt p (control  p (\_ -> control p (\k -> (2:) <$> k (pure [])) >>= \y -> control p (\_ -> pure y))))
          reset (\p -> (1:) <$> pushPrompt p (control0 p (\_ -> control p (\k -> (2:) <$> k (pure [])) >>= \y -> control p (\_ -> pure y))))
        -}