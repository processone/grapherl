import json
import socket
import calendar
import time
import random


# generate N clients
def generate_clients(N):
    List = []
    for i in range(1, N+1):
        List.append("website" + str(i) + ".com")
    return List

def main():
    N       = 2
    Clients = generate_clients(N)
    sock    = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    ts      = calendar.timegm(time.gmtime())

    for i in range(1, 3):
        for j in range(0, N):
            val = random.random()
            DataPoint = Clients[j] + "/metric" + str(i) + ":g/" + str(ts) + ":" + str(val)
            print(DataPoint)
            sock.sendto(str.encode(DataPoint), ('localhost', 11111))
            time.sleep(0.002)
        ts += 5

if __name__ == "__main__":
    main()
