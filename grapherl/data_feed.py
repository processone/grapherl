import json
import socket
import calendar
import time
import random




def generate_clients(N):
    List = []
    for i in range(1, N+1):
        List.append("www.server" + str(i) + ".com")
    return List

def main():
    N       = 200
    Clients = generate_clients(N)

    sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    ts = calendar.timegm(time.gmtime())

    for i in range(1, 10000):
        for j in range(0, N):
            val = random.random()
            D = json.dumps({'mid': {'mn': "cpu_usage", 'cn': Clients[j]}, 'mp': {ts:val} })
            print(D)
            sock.sendto(str.encode(D), ('localhost', 11111))
            time.sleep(0.005)
        ts += 5

if __name__ == "__main__":
    main()
