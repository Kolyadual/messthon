# dht.py
import hashlib
import random
import threading
import time
import socket
import json
from typing import Dict, List, Tuple, Optional

class DHTNode:
    """Узел распределённой хеш-таблицы (Kademlia-подобный)"""
    
    def __init__(self, node_id: bytes, host: str, port: int):
        self.node_id = node_id
        self.host = host
        self.port = port
        self.routing_table: Dict[bytes, Tuple[str, int]] = {}  # ID -> (host, port)
        self.max_bucket_size = 8
        self.lock = threading.Lock()
        
    def distance(self, node_id1: bytes, node_id2: bytes) -> int:
        """XOR-расстояние между узлами (как в торрентах) [citation:5]"""
        return int.from_bytes(node_id1, 'big') ^ int.from_bytes(node_id2, 'big')
    
    def add_node(self, node_id: bytes, host: str, port: int):
        """Добавить узел в таблицу маршрутизации"""
        with self.lock:
            if len(self.routing_table) < self.max_bucket_size:
                self.routing_table[node_id] = (host, port)
            else:
                # Сортируем по расстоянию и оставляем ближайшие
                sorted_nodes = sorted(
                    self.routing_table.items(),
                    key=lambda x: self.distance(x[0], self.node_id)
                )[:self.max_bucket_size-1]
                self.routing_table = dict(sorted_nodes)
                self.routing_table[node_id] = (host, port)
    
    def find_nearest(self, target_id: bytes, count: int = 5) -> List[Tuple[bytes, str, int]]:
        """Найти ближайшие узлы к целевому ID"""
        with self.lock:
            nodes = [(nid, *addr) for nid, addr in self.routing_table.items()]
            nodes.sort(key=lambda x: self.distance(x[0], target_id))
            return nodes[:count]
    
    def bootstrap(self, bootstrap_nodes: List[Tuple[str, int]]):
        """Подключение к DHT сети через известные узлы"""
        for host, port in bootstrap_nodes:
            try:
                # Отправляем PING
                sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
                sock.settimeout(2)
                msg = json.dumps({
                    'type': 'ping',
                    'node_id': self.node_id.hex(),
                    'host': self.host,
                    'port': self.port
                })
                sock.sendto(msg.encode(), (host, port))
                
                # Получаем ответ
                data, _ = sock.recvfrom(4096)
                response = json.loads(data)
                if response['type'] == 'pong':
                    self.add_node(
                        bytes.fromhex(response['node_id']),
                        response['host'],
                        response['port']
                    )
                sock.close()
            except:
                continue
