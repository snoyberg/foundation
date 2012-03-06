{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Foundation
    ( -- * Basic types
      Prelude.Bool (..)
    , (Prelude.&&)
    , (Prelude.||)
    , Prelude.not
    , Prelude.otherwise
    , Prelude.Maybe (..)
    , Prelude.maybe
    , Prelude.Either (..)
    , Prelude.either
    , Prelude.Ordering (..)
    , Prelude.Char
    , Prelude.fst
    , Prelude.snd
    , Prelude.curry
    , Prelude.uncurry
    , Prelude.Eq (..)
    , Prelude.Ord (..)
    , Prelude.Enum (..)
    , Prelude.Bounded (..)
    , Prelude.Int
    , Prelude.Integer
    , Prelude.Float
    , Prelude.Double
    , Prelude.Rational
    , Prelude.Num (..)
    , Prelude.Real (..)
    , Prelude.Integral (..)
    , Prelude.Fractional (..)
    , Prelude.Floating (..)
    , Prelude.RealFrac (..)
    , Prelude.RealFloat (..)
    , Prelude.subtract
    , Prelude.even
    , Prelude.odd
    , Prelude.gcd
    , Prelude.lcm
    , (Prelude.^)
    , (Prelude.^^)
    , Prelude.fromIntegral
    , Prelude.realToFrac
    , Prelude.Monad (..)
    , (Prelude.=<<)
    , Prelude.Functor (..)
    , (Control.Applicative.<$>)
    , Control.Applicative.Applicative (..)
    , Prelude.mapM
    , Data.Foldable.mapM_
    , Control.Monad.forM
    , Data.Foldable.forM_
    , Prelude.sequence
    , Data.Foldable.sequence_
    , Control.Category.Category (..)
    , (Control.Category.<<<)
    , (Control.Category.>>>)
    , Prelude.const
    , Prelude.flip
    , (Prelude.$)
    , Prelude.until
    , Prelude.asTypeOf
    , error
    , Prelude.undefined
    , Prelude.seq
    , (Prelude.$!)
    , map
    , (++)
    , Prelude.filter
    , foldl
    , Data.List.foldl'
    , Prelude.foldr
    , Prelude.and
    , Prelude.or
    , Prelude.any
    , Prelude.all
    , concat
    , Prelude.concatMap
    , Prelude.scanl
    , Prelude.scanl1
    , Prelude.scanr
    , Prelude.scanr1
    , Prelude.iterate
    , Prelude.repeat
    , Prelude.replicate
    , Prelude.cycle
    , TakeDrop (..)
    , TakeDropWhile (..)
    , Elem (..)
    , Prelude.zip
    , Prelude.zip3
    , Prelude.zipWith
    , Prelude.zipWith3
    , Prelude.unzip
    , Prelude.unzip3
    , LinesWords (..)
    , show
    , readMay
    , Prelude.Show
    , Prelude.Read
    , Prelude.IO
      -- ** File path
    , Filesystem.Path.CurrentOS.FilePath
    , (Filesystem.Path.CurrentOS.</>)
    , (Filesystem.Path.CurrentOS.<.>)
    , Filesystem.Path.CurrentOS.filename
    , Filesystem.Path.CurrentOS.directory
    , Filesystem.Path.CurrentOS.basename
      -- * Extra exports
    , Data.Text.Text
    , Data.Monoid.Monoid (..)
      -- * Newly defined stuff
    , ToCharList (..)
    , Null (..)
    , Length (..)
    ) where

import qualified Prelude
import qualified Control.Applicative
import qualified Control.Monad
import qualified Data.Foldable
import qualified Control.Category
import qualified Data.Text
import qualified Data.Monoid
import qualified Data.List
import qualified Data.String
import qualified Filesystem.Path.CurrentOS

class ToCharList a where
    toCharList :: a -> [Prelude.Char]

instance ToCharList [Prelude.Char] where
    toCharList = Prelude.id

instance ToCharList Data.Text.Text where
    toCharList = Data.Text.unpack

error :: ToCharList a => a -> b
error = Prelude.error Prelude.. toCharList

map :: Prelude.Functor f => (a -> b) -> f a -> f b
map = Prelude.fmap

infixr 5  ++
(++) :: Data.Monoid.Monoid m => m -> m -> m
(++) = Data.Monoid.mappend

class Null a where
    null :: a -> Prelude.Bool

instance Null [a] where
    null = Prelude.null

instance Null Data.Text.Text where
    null = Data.Text.null

class Length a where
    length :: Prelude.Integral i => a -> i

instance Length [a] where
    length = Prelude.fromIntegral Prelude.. Prelude.length

instance Length Data.Text.Text where
    length = Prelude.fromIntegral Prelude.. Data.Text.length

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl = Data.List.foldl'

concat :: Data.Monoid.Monoid m => [m] -> m
concat = Data.Monoid.mconcat

class TakeDrop a where
    take :: Prelude.Integral i => i -> a -> a
    drop :: Prelude.Integral i => i -> a -> a
    splitAt :: Prelude.Integral i => i -> a -> (a, a)
instance TakeDrop [a] where
    take i = Prelude.take (Prelude.fromIntegral i)
    drop i = Prelude.drop (Prelude.fromIntegral i)
    splitAt i = Prelude.splitAt (Prelude.fromIntegral i)
instance TakeDrop Data.Text.Text where
    take i = Data.Text.take (Prelude.fromIntegral i)
    drop i = Data.Text.drop (Prelude.fromIntegral i)
    splitAt i = Data.Text.splitAt (Prelude.fromIntegral i)

class TakeDropWhile container content | container -> content where
    takeWhile :: (content -> Prelude.Bool) -> container -> container
    dropWhile :: (content -> Prelude.Bool) -> container -> container

    break :: (content -> Prelude.Bool) -> container -> (container, container)
    span :: (content -> Prelude.Bool) -> container -> (container, container)
instance TakeDropWhile [a] a where
    takeWhile = Prelude.takeWhile
    dropWhile = Prelude.dropWhile
    break = Prelude.break
    span = Prelude.span
instance TakeDropWhile Data.Text.Text Prelude.Char where
    takeWhile = Data.Text.takeWhile
    dropWhile = Data.Text.dropWhile
    break = Data.Text.break
    span = Data.Text.span

class Elem container content | container -> content where
    elem :: content -> container -> Prelude.Bool
    notElem :: content -> container -> Prelude.Bool
instance Prelude.Eq a => Elem [a] a where
    elem = Prelude.elem
    notElem = Prelude.notElem

class LinesWords a where
    lines :: a -> [a]
    words :: a -> [a]
instance LinesWords [Prelude.Char] where
    lines = Prelude.lines
    words = Prelude.words
instance LinesWords Data.Text.Text where
    lines = Data.Text.lines
    words = Data.Text.words

show :: (Prelude.Show a, Data.String.IsString s) => a -> s
show = Data.String.fromString Prelude.. Prelude.show

readMay :: Prelude.Read a => [Prelude.Char] -> Prelude.Maybe a
readMay s =
    case Prelude.reads s of
        (x, []):_ -> Prelude.Just x
        (_, l):_ -> Prelude.Nothing
        [] -> Prelude.Nothing
