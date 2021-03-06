a = quote( a <- f(1, g(x, h(y))))
b = quote( a[[g(x)]] <- f(1, g(x, h(y))))
c = substitute( for(i in g(x)) {  a <- f(1, g(x, h(y))) ; a[[z(x)]] = g(x, h(y))} )
d = substitute( if(g(x) > 10 ) {  a <- f(1, g(x, h(y))) ; a[[z(x)]] = g(x, h(y))} else { a = foo(bar(z)); a[[2]] <- zoo(abc) })

cb =
function()
{
  ctr = 20L
  ans = foo(bob, x = ctr, 1L) # structure(1:10, class = "Jane", names = letters[1:10]))
}


do =
function(x)
{
    lapply(x, table)
    sapply(x, function(x) sort(x))
    mapply(f, 1:10, 2:11)
    apply(m, 1, order)
}

ns =
function()
{
    x = base::seq(1, 10)
    y = foo(10)
    x + y
}

nested0 =
    #
    #
    #
function(x, alpha = 2)
{
  sapply(x, function(e) g(e + alpha) )
}



nested =
    #
    #
    #
function(x)
{
  sapply(x, function(e) g(e + alpha) )
}



nested2 =
function(x)
{
  sapply(x, function(e) lapply(e, g, beta))
}


nested3 =
function(x)
{
  sapply(x, function(e) for(i in e) g(i, beta))
}

whileLoop =
function(x)
{
    while(f(a) > x) {
        a = x + a
    }
    b
}
