import time,random

t1 = int(round(time.time() * 1000))
print(t1)
sum_global = 0
num = 100000000
for q in range(num):
    x = random.random()
    y = random.random()
    d = x**2 + y**2
    if  d <= 1 :
        sum_global += 1

pi = float(sum_global)/num*4.0
t2 = int(round(time.time() * 1000))
print("pi = ", pi, " time = ",t2-t1," ms")
