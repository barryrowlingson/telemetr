      double precision function atof(words)
      character words*(*)
      read(words,fmt='(bn,f8.0)') atof
      return
      end
