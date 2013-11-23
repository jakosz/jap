ni <-
function(df,        
         Vector,    # wektor indeksów kolumn lub nazw kolumn
         what) {    # co ma być wynikiem? 'i' (indeksy) czy 'n' (nazwy)?
    if (what == 'i') {
        if (is.numeric(Vector)) {
            return(Vector)
        } else {
            return(n2i(df, Vector))
        }
    }
    if (what == 'n') {
        if(is.numeric(Vector)) {
            return(i2n(df, Vector))
        } else {
            return(Vector)
        }
    }
}
