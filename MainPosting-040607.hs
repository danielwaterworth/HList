{-# OPTIONS -fglasgow-exts #-}
{-# OPTIONS -fallow-overlapping-instances #-}

{-

Hi Mike,

You might find heterogeneous lists useful.
http://www.cwi.nl/~ralf/HList/
See the treatment of your example below.

Cheers,
Ralf

-}

import MainGhcGeneric1
import Data.Typeable

-- These are your two "implementations".

data MyImplementation1 = MyImplementation1 Int deriving (Show,Eq,Typeable)
data MyImplementation2 = MyImplementation2 Int deriving (Show,Eq,Typeable)


-- This is your interface class and the two instances.

class MyInterface a 
 where
  foo :: a -> Int

instance MyInterface MyImplementation1
 where
  foo (MyImplementation1 i) = i

instance MyInterface MyImplementation2
 where
  foo (MyImplementation2 i) = i


-- Here is your list,
-- without the noise you don't like.

list1 =  MyImplementation1 10
     .*. MyImplementation2 20
     .*. HNil


-- If you like, this is the type, but it is not needed.
-- This list is not opaque. Less trouble in our experience.
-- (When compared to using existentials.)

type MyList =     MyImplementation1
            :*: ( MyImplementation2
            :*:   HNil )


-- Perhaps you want to make sure that you have a list of implementations
-- of MyInterface. Here is *one* way to do it. But you don't need to do it
-- because this will be automatically checked (statically) whenever you
-- try to use the fooISH interface.

class ListOfMyInterface l
 where
  listOfMyInterface :: l -> l
  listOfMyInterface = id

instance ListOfMyInterface HNil
instance ( MyInterface e
         , ListOfMyInterface l
         )
      =>   ListOfMyInterface (HCons e l)


-- So you apply the id function with the side effect of statically 
-- ensuring that you are given a list of implementations of MyInterface.

list2 :: MyList
list2 = listOfMyInterface list1


-- Here is another way to do it.
-- You apply a heterogenous fold to the list.
-- This second solution is just for fun.

data ImplementsMyInterface = ImplementsMyInterface

instance Apply ImplementsMyInterface (e,l) (HCons e l)
 where
  apply _ (e,l) = HCons e l

myKindOfList l = hFoldr ImplementsMyInterface HNil l


-- Basically again you apply the identity function; a deep one this time.

list3 :: MyList
list3 = myKindOfList list1


-- Your equality can indeed not work because the existentially quantified
-- implementations are of course opaque. You cannot compare apples and
-- oranges. Equality of heterogeneous lists is trivial; it is just derived.
-- To make it a little bit more interesting, we can consider heterogeneous
-- or stanamic equality. So you will always get a Boolean even for lists
-- of different types. See below.


-- Here is your bar function.
-- It uses one sort of maps on heterogeneous lists.

bar :: MyList -> Int
bar = sum . hMapOut Foo

data Foo = Foo -- type driver for class-level application

instance MyInterface e => Apply Foo e Int
 where
  apply _ e = foo e


-- Yet another heterogeneous equality.
-- Just for fun.
--
yaHEq :: (Typeable a, Typeable b, Eq a) => a -> b -> Bool
yaHEq a b = case cast b of
             Just a' -> a == a'
             Nothing -> False

-- Yet another heterogeneous list; a bit less typed.
data AnyMyInterface = forall a. ( Eq a
                                , Typeable a
                                , MyInterface a
                                ) => AnyMyInterface a

type MyList' = [AnyMyInterface]
list4 = [ AnyMyInterface $ MyImplementation1 10
        , AnyMyInterface $ MyImplementation1 10
        ]
list5 = [ AnyMyInterface $ MyImplementation1 10
        , AnyMyInterface $ MyImplementation2 10
        ]
list6 = [ AnyMyInterface $ MyImplementation1 10
        , AnyMyInterface $ MyImplementation2 20
        ]

instance Eq AnyMyInterface
 where
  (AnyMyInterface x) == (AnyMyInterface y) = x `yaHEq` y


{-

Demo follows.

*Main> :l gh-users-040607.hs
Compiling FakePrelude      ( ./FakePrelude.hs, interpreted )
Compiling HType            ( HType.hs, interpreted )
Compiling HList            ( ./HList.hs, interpreted )
Compiling HArray           ( ./HArray.hs, interpreted )
Compiling HTypeDriven      ( ./HTypeDriven.hs, interpreted )
Compiling Main             ( gh-users-040607.hs, interpreted )
Ok, modules loaded: Main, HTypeDriven, HArray, HList, HType, FakePrelude.
*Main> list1
HCons (MyImplementation1 10) (HCons (MyImplementation2 20) HNil)
*Main> bar list1
30
*Main> list1 == list1
True
*Main> list1 `hStagedEq` hReverse list1
False
*Main> list4!!0 == list4!!1
True
*Main> list5!!0 == list5!!1
False
*Main> list6!!0 == list6!!1
False

-}

main = print
             ( list1
           , ( list1 == list1
           , ( list1 `hStagedEq` hReverse list1
           , ( list4!!0 == list4!!1
           , ( list5!!0 == list5!!1
           , ( list6!!0 == list6!!1
           ))))))

{-

Mike Aizatsky wrote:

>Hello,
>
>I'm in process of rewriting the old Java application. While this is for sure
>lots of fun, there're some problems in modeling the java interfaces.
>
>Here's the common Java scenario (it's actually the pattern, common for all
>OO-languages, so there should be no problems in understanding it):
>
>interface MyInterface {
>	int foo();
>}
>
>class MyImplementation1 implements MyInterface { int foo() {...} }
>class MyImplementation2 implements MyInterface { int foo() {...} }
>
>And, somewhere in the code:
>
>int bar(List<MyInterface> list) { .... sum up all foos & return .... }
>
>I've found quite an obvious translation of it to Haskell:
>
>module Ex where
>
>class MyInterface a where
>	foo :: a -> Int
>
>data AnyMyInterface = forall a. (MyInterface a) => AnyMyInterface a
>
>instance MyInterface AnyMyInterface where
>	foo (AnyMyInterface a) = foo a
>
>
>data MyImplementation1 = MyImplementation1 Int
>
>instance MyInterface MyImplementation1 where
>	foo(MyImplementation1 i) = i
>
>data MyImplementation2 = MyImplementation2 Int
>
>instance MyInterface MyImplementation2 where
>	foo(MyImplementation2 i) = i
>
>
>type MyList = [AnyMyInterface]
>
>list1 :: MyList
>list1 = [AnyMyInterface (MyImplementation1 10), AnyMyInterface
>(MyImplementation2 20)]
>
>bar :: MyList -> Int
>bar l = sum (map foo l)
>
>
>However there're some problems with this way to go:
>
>1. It's quite verbose. I already have a dozen of such interfaces, and I'm a
>bit tired of writing all this AnyInterface stuff. I'm already thinking about
>writing the Template Haskell code to generate it. Is anything similar
>available around?
>
>2. I don't like the fact that I need to wrap all implementations inside the
>AnyMyInterface when returning values (see list1). Any way to get rid of it?
>
>3. The big problem. I can't make AnyMyInterface to be an instance of Eq. I
>write:
>
>data AnyMyInterface = forall a. (MyInterface a, Eq a) => AnyMyInterface a
>instance Eq AnyMyInterface where
>	(==) (AnyMyInterface a1) (AnyMyInterface a2) = a1 == a2
>
>And it gives me an error (ghc 6.2.1):
>
>    Inferred type is less polymorphic than expected
>        Quantified type variable `a1' is unified with another quantified
>type variable `a'
>    When checking an existential match that binds
>        a1 :: a
>        a2 :: a1
>    The pattern(s) have type(s): AnyMyInterface
>                                 AnyMyInterface
>    The body has type: Bool
>    In the definition of `==':
>        == (AnyMyInterface a1) (AnyMyInterface a2) = a1 == a2
>    In the definition for method `=='
>
>Honestly, I don't understand what's going on. My guess is that the problem
>comes from the fact that a1 & a2 might be of different Implementations. Is
>it right? Any way to define the Eq instance of AnyMyInterface?
>
>
>So, it looks like that existential data types do allow you to mimic the
>polymorphic data structures, found in OO languages. But it results in much
>more verbose code. Are there any other ways to do the same stuff?
>
>_______________________________________________
>Glasgow-haskell-users mailing list
>Glasgow-haskell-users@haskell.org
>http://www.haskell.org/mailman/listinfo/glasgow-haskell-users
>

-}
