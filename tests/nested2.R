f =
function(x, y)
{
    a = x + y
    g = function(w)
            w + a / x^2

    g(300)
}

g =
function(x, y)
{
    a = x + y
    g = function(w) {
            foo = function() 2
            foo(w + a / x^2)
        }

    g(300)
}

h =
function(x, y)
{
    a = x + y
    g = function(w) {
            foo = function() 2
            foo(w + a / x^2)
    }

    bar = function(x) {
        do = function() {

                it = function() {
                    4
                }

                sapply(x, function(a) it(a + x))
        }
        x + 2 + do()
    }

    g(300)
}

