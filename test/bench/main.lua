vult = loadfile("bench.lua")()

data = vult.Bench_process_init()

time = 44100 * 100

while time > 0 do
   vult.Bench_process(data)
   time = time -1
end