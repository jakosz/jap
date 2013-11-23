scale_to_range <-
function(x, from = 1, to = 100) {
    # skalowanie:
    x = scale(x)[,1]
    # przesunięcie: wartość min. wyrównać do 0:
    x = x+abs(min(x))
    # przesunięcie: wartość max. wyrównać do (to - from):
    x = x*((to-from)/max(x))
    # przesunięcie o wartość from;
    return(x+from)
}
