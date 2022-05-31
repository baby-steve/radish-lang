function fib(n)
    if n < 2 then return n end
    return fib(n - 2) + fib(n - 1)
end
  
local start = os.clock()

print(fib(35))

io.write(string.format("time: %.8f\n", os.clock() - start))