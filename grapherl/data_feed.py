import json
import socket
import calendar
import time
import random


sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
ts = calendar.timegm(time.gmtime())

for i in range(1, 2):
    for j in range(1, 15000):
        val = random.random()
        D = json.dumps({'mid': {'mn': "cpu_usage", 'cn': "www.server01.com"}, 'mp': {ts:val} })
        print(D)
        ts += 60
        sock.sendto(str.encode(D), ('localhost', 11111))
        time.sleep(0.005)
