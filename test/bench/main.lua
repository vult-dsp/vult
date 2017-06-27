vult = loadfile("./bench.lua")()

data = vult.Bench_process_init()

time = 44100 * 200

local start = os.clock()

while time > 0 do
   vult.Bench_process(data)
   time = time -1
end

local finish = os.clock() - start

print("\n### Lua Execution time: ", finish)
