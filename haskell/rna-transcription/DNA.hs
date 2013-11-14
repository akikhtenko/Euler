module DNA where

toRNA::String -> String
toRNA dna = map transcribe dna
        where 
                transcribe 'T' = 'U'
                transcribe c = c