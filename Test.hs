import Control.Monad(zipWithM_)
import System.Exit (exitFailure)

import Types.Ents(mkEnt)
import Types.Names
import Types.ModSysAST
import Util.Set

import ModSysSem
import Modules

entF x  = mkEnt x "f" emptySet


ex1 = [
  Module
  { modName     = ModName "A"
  , modExpList  = Nothing
  , modImports  = []
  , modDefines  = Defns $ mkSet [(Name "f", entF "A") ]
  },

  Module
  { modName     = ModName "B"
  , modExpList  = Just [EntExp (Ent (mkUnqual (Name "f")) Nothing)]
  , modImports  =
    [ Import
      { impQualified  = False
      , impSet        = ImportSet
                        { imsSource     = ModName "A"
                        , imsAs         = ModName "A"
                        , imsHiding     = True
                        , imsList       = []
                        }
      }
    ]
  , modDefines  = mempty
  },

  Module
  { modName     = ModName "C"
  , modExpList  = Just [EntExp (Ent (mkUnqual (Name "f")) Nothing)]
  , modImports  =
    [ Import
      { impQualified  = False
      , impSet        = ImportSet
                        { imsSource     = ModName "A"
                        , imsAs         = ModName "A"
                        , imsHiding     = True
                        , imsList       = []
                        }
      }
    ]
  , modDefines  = mempty
  },

  Module
  { modName     = ModName "D"
  , modExpList  = Just [ModuleExp (ModName "B")]
  , modImports  =
    [ Import
      { impQualified  = True
      , impSet        = ImportSet
                        { imsSource     = ModName "B"
                        , imsAs         = ModName "B"
                        , imsHiding     = True
                        , imsList       = []
                        }
      }
      , Import
      { impQualified  = False
      , impSet        = ImportSet
                        { imsSource     = ModName "C"
                        , imsAs         = ModName "C"
                        , imsHiding     = True
                        , imsList       = []
                        }
      }
    ]
  , modDefines  = mempty
  }
  ]


ex2 = [
  Module
  { modName     = ModName "A"
  , modExpList  = Just [EntExp (Ent (mkUnqual (Name "f")) Nothing)]
  , modImports  =
    [ Import
      { impQualified  = False
      , impSet        = ImportSet
                        { imsSource     = ModName "B"
                        , imsAs         = ModName "B"
                        , imsHiding     = True
                        , imsList       = []
                        }
      }
    ]
  , modDefines  = mempty
  },

  Module
  { modName     = ModName "B"
  , modExpList  = Just [ModuleExp (ModName "A")]
  , modImports  =
    [ Import
      { impQualified  = False
      , impSet        = ImportSet
                        { imsSource     = ModName "A"
                        , imsAs         = ModName "A"
                        , imsHiding     = True
                        , imsList       = []
                        }
      }
    ]
  , modDefines  = Defns $ mkSet [(Name "f", entF "B") ]
  }
  ]


ex3 = [
  Module
  { modName     = ModName "A"
  , modExpList  = Just [EntExp (Ent (mkQual (ModName "B") (Name "f")) Nothing)]
  , modImports  =
    [ Import
      { impQualified  = True
      , impSet        = ImportSet
                        { imsSource     = ModName "B"
                        , imsAs         = ModName "B"
                        , imsHiding     = True
                        , imsList       = []
                        }
      },

    Import
      { impQualified  = False
      , impSet        = ImportSet
                        { imsSource     = ModName "A"
                        , imsAs         = ModName "B"
                        , imsHiding     = True
                        , imsList       = []
                        }
      }

    ]
  , modDefines  = Defns $ mkSet [(Name "f", entF "A") ]
  },

  Module
  { modName     = ModName "B"
  , modExpList  = Nothing
  , modImports  = []
  , modDefines  = Defns $ mkSet [(Name "f", entF "B") ]
  }
  ]



test mods = either (zipWithM_ printErr mods)
                   (zipWithM_ printRels mods)
                   (mProgram mods)
  where
  printErr m es
    | null es = return ()
    | otherwise = putStrLn ("Error(s) in module " ++ show (modName m) ++ ": "
                  ++ show es) >> exitFailure
  printRels m (Scope ins,Exports outs) =
    do putStrLn ("in scope of module " ++ show (modName m) ++ ":")
       printRel ins
       putStrLn ("exports of module " ++ show (modName m) ++ ":")
       printRel outs
       putStrLn ""


printRel r = sequence_ [ putStrLn $ show x ++ " |-> " ++ show y
                              | (x,y) <- setToList r ]

main = do putStrLn $ "test 1 " ++ replicate 50 '-'
          test ex1
          putStrLn $ "test 2 " ++ replicate 50 '-'
          test ex2
          putStrLn $ "test 3 " ++ replicate 50 '-'
          test ex3
