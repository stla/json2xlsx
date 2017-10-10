{-# LANGUAGE ForeignFunctionInterface #-}
module Export
  where
import JSONtoXLSX (writeXlsx6)
import Foreign
import Foreign.C

foreign export ccall json2xlsx :: Ptr CString -> Ptr CString -> Ptr CString ->
                                                            Ptr CString -> IO ()
json2xlsx :: Ptr CString -> Ptr CString -> Ptr CString -> Ptr CString -> IO ()
json2xlsx json1 json2 json3 outfile = do
  json1 <- (>>=) (peek json1) peekCAString
  json2 <- (>>=) (peek json2) peekCAString
  json3 <- (>>=) (peek json3) peekCAString
  outfile <- (>>=) (peek outfile) peekCAString
  writeXlsx6 json1 json2 json3 outfile
