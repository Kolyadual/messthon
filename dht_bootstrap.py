# dht_bootstrap.py
import socket
import json
import threading
import time
import hashlib
from collections import OrderedDict

class DHTBootstrapNode:
    def __init__(self, host='0.0.0.0', port=6881):
        self.host = host
        self.port = port
        self.node_id = hashlib.sha1(f"bootstrap-{port}".encode()).digest()  # фиктивный ID
        self.routing_table = OrderedDict()  # node_id -> (ip, port, last_seen)
        self.lock = threading.Lock()
        
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.sock.bind((self.host, self.port))
        
        print(f"🔥 DHT Bootstrap Server запущен на {self.host}:{self.port}")
        print(f"🆔 Мой ID: {self.node_id.hex()}")
        
    def distance(self, id1, id2):
        return int.from_bytes(id1, 'big') ^ int.from_bytes(id2, 'big')
    
    def add_node(self, node_id, ip, port):
        now = time.time()
        with self.lock:
            self.routing_table[node_id] = (ip, port, now)
            # Ограничим размер таблицы (например, 1000 записей)
            if len(self.routing_table) > 1000:
                # удаляем самую старую
                self.routing_table.popitem(last=False)
    
    def find_nearest(self, target_id, count=8):
        with self.lock:
            nodes = [(nid, ip, port) for nid, (ip, port, _) in self.routing_table.items()]
            nodes.sort(key=lambda x: self.distance(x[0], target_id))
            return nodes[:count]
    
    def handle_ping(self, addr, data):
        """Ответ на ping: просто pong"""
        response = {
            'type': 'pong',
            'node_id': self.node_id.hex(),
            'host': self.host,
            'port': self.port
        }
        self.sock.sendto(json.dumps(response).encode(), addr)
        print(f"PING от {addr} -> PONG")
    
    def handle_find_node(self, addr, data):
        """Возвращаем ближайшие узлы к целевому ID"""
        target_id = bytes.fromhex(data['target'])
        nearest = self.find_nearest(target_id)
        nodes_list = [{'node_id': nid.hex(), 'host': ip, 'port': port} for nid, ip, port in nearest]
        response = {
            'type': 'found_nodes',
            'nodes': nodes_list
        }
        self.sock.sendto(json.dumps(response).encode(), addr)
        print(f"FIND_NODE от {addr} -> возвращено {len(nodes_list)} узлов")
    
    def run(self):
        while True:
            try:
                data_raw, addr = self.sock.recvfrom(4096)
                data = json.loads(data_raw.decode())
                
                # Если в запросе есть node_id отправителя, добавляем его в таблицу
                if 'node_id' in data:
                    node_id = bytes.fromhex(data['node_id'])
                    self.add_node(node_id, addr[0], data.get('port', addr[1]))
                
                # Обработка типов запросов
                if data['type'] == 'ping':
                    self.handle_ping(addr, data)
                elif data['type'] == 'find_node':
                    self.handle_find_node(addr, data)
                # можно добавить другие типы, но для bootstrap достаточно
                    
            except Exception as e:
                print(f"Ошибка: {e}")

if __name__ == '__main__':
    import sys
    port = 6881
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    node = DHTBootstrapNode(port=port)
    node.run()
