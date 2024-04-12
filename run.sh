#!/bin/bash

alex Lexer.x  
happy Parser.y
runhaskell Main.hs
