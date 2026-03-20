# node.py
import socket
import threading
import json
import hashlib
import time
from typing import Dict, List, Tuple, Optional, Callable
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa, padding
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.backends import default_backend
import base64
import os

# ==================== CryptoManager ====================
class CryptoManager:
    """Управление шифрованием (RSA + AES)"""
    
    def __init__(self):
        # Генерируем RSA ключи
        self.private_key = rsa.generate_private_key(
            public_exponent=65537,
            key_size=2048,
            backend=default_backend()
        )
        self.public_key = self.private_key.public_key()
        self.peer_public_keys = {}  # peer_id -> public_key
        
    def get_node_id(self) -> bytes:
        """ID узла = SHA256 публичного ключа"""
        pub_bytes = self.public_key.public_bytes(
            encoding=serialization.Encoding.PEM,
            format=serialization.PublicFormat.SubjectPublicKeyInfo
        )
        return hashlib.sha256(pub_bytes).digest()
    
    def encrypt_message(self, message: str, peer_id: bytes) -> bytes:
        """Шифрование сообщения для конкретного получателя"""
        if peer_id not in self.peer_public_keys:
            raise ValueError("Нет публичного ключа получателя")
        
        # Генерируем AES ключ для этого сообщения
        aes_key = os.urandom(32)
        iv = os.urandom(16)
        
        # Шифруем само сообщение AES
        cipher = Cipher(algorithms.AES(aes_key), modes.CBC(iv), backend=default_backend())
        encryptor = cipher.encryptor()
        
        # Padding сообщения
        padded_message = message.encode() + b' ' * (16 - len(message) % 16)
        encrypted_msg = encryptor.update(padded_message) + encryptor.finalize()
        
        # Шифруем AES ключ RSA получателя
        encrypted_key = self.peer_public_keys[peer_id].encrypt(
            aes_key,
            padding.OAEP(
                mgf=padding.MGF1(algorithm=hashes.SHA256()),
                algorithm=hashes.SHA256(),
                label=None
            )
        )
        
        # Упаковываем всё вместе
        packet = {
            'key': base64.b64encode(encrypted_key).decode(),
            'iv': base64.b64encode(iv).decode(),
            'msg': base64.b64encode(encrypted_msg).decode()
        }
        
        return json.dumps(packet).encode()
    
    def decrypt_message(self, encrypted_data: bytes) -> str:
        """Расшифровка полученного сообщения"""
        packet = json.loads(encrypted_data.decode())
        
        encrypted_key = base64.b64decode(packet['key'])
        iv = base64.b64decode(packet['iv'])
        encrypted_msg = base64.b64decode(packet['msg'])
        
        # Расшифровываем AES ключ своим RSA
        aes_key = self.private_key.decrypt(
            encrypted_key,
            padding.OAEP(
                mgf=padding.MGF1(algorithm=hashes.SHA256()),
                algorithm=hashes.SHA256(),
                label=None
            )
        )
        
        # Расшифровываем сообщение AES
        cipher = Cipher(algorithms.AES(aes_key), modes.CBC(iv), backend=default_backend())
        decryptor = cipher.decryptor()
        decrypted = decryptor.update(encrypted_msg) + decryptor.finalize()
        
        return decrypted.rstrip(b' ').decode()

# ==================== DHTNode ====================
class DHTNode:
    """Узел распределённой хеш-таблицы (Kademlia-подобный) с UDP"""
    
    def __init__(self, node_id: bytes, host: str, port: int):
        self.node_id = node_id
        self.host = host
        self.port = port
        self.routing_table: Dict[bytes, Tuple[str, int]] = {}  # ID -> (host, port)
        self.max_bucket_size = 20
        self.lock = threading.Lock()
        
        # UDP сокет для DHT запросов
        self.udp_sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.udp_sock.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        self.udp_sock.bind((self.host, self.port))
        
        # Запускаем поток для прослушивания UDP
        self.running = True
        self.udp_thread = threading.Thread(target=self._udp_listener)
        self.udp_thread.daemon = True
        self.udp_thread.start()
        
    def _udp_listener(self):
        """Слушаем UDP запросы (ping, find_node) от других узлов"""
        while self.running:
            try:
                data_raw, addr = self.udp_sock.recvfrom(4096)
                data = json.loads(data_raw.decode())
                
                if data['type'] == 'ping':
                    # Ответ pong
                    response = {
                        'type': 'pong',
                        'node_id': self.node_id.hex(),
                        'host': self.host,
                        'port': self.port
                    }
                    self.udp_sock.sendto(json.dumps(response).encode(), addr)
                    
                elif data['type'] == 'find_node':
                    target_id = bytes.fromhex(data['target'])
                    nearest = self.find_nearest(target_id)
                    nodes_list = [{'node_id': nid.hex(), 'host': h, 'port': p} for nid, (h, p) in nearest]
                    response = {
                        'type': 'found_nodes',
                        'nodes': nodes_list
                    }
                    self.udp_sock.sendto(json.dumps(response).encode(), addr)
                    
                elif data['type'] == 'found_nodes':
                    # Получен ответ на наш запрос find_node
                    for node_info in data['nodes']:
                        nid = bytes.fromhex(node_info['node_id'])
                        self.add_node(nid, node_info['host'], node_info['port'])
                        
            except Exception as e:
                print(f"UDP listener error: {e}")
    
    def distance(self, node_id1: bytes, node_id2: bytes) -> int:
        return int.from_bytes(node_id1, 'big') ^ int.from_bytes(node_id2, 'big')
    
    def add_node(self, node_id: bytes, host: str, port: int):
        with self.lock:
            if node_id == self.node_id:
                return
            self.routing_table[node_id] = (host, port)
            # ограничим размер таблицы
            if len(self.routing_table) > self.max_bucket_size:
                # удаляем самого дальнего
                sorted_nodes = sorted(
                    self.routing_table.items(),
                    key=lambda x: self.distance(x[0], self.node_id),
                    reverse=True
                )
                for nid, _ in sorted_nodes[self.max_bucket_size:]:
                    del self.routing_table[nid]
    
    def find_nearest(self, target_id: bytes, count: int = 8) -> List[Tuple[bytes, str, int]]:
        with self.lock:
            nodes = [(nid, *addr) for nid, addr in self.routing_table.items()]
            nodes.sort(key=lambda x: self.distance(x[0], target_id))
            return nodes[:count]
    
    def bootstrap(self, bootstrap_nodes: List[Tuple[str, int]]):
        """Подключение к DHT сети через известные узлы (например, наш сервер)"""
        for host, port in bootstrap_nodes:
            try:
                # Отправляем find_node для себя, чтобы получить ближайших
                self.find_node(self.node_id, (host, port))
            except Exception as e:
                print(f"Bootstrap error to {host}:{port} - {e}")
    
    def find_node(self, target_id: bytes, addr: Tuple[str, int]):
        """Отправить запрос find_node удалённому узлу"""
        msg = {
            'type': 'find_node',
            'node_id': self.node_id.hex(),
            'target': target_id.hex(),
            'host': self.host,
            'port': self.port
        }
        self.udp_sock.sendto(json.dumps(msg).encode(), addr)
    
    def ping(self, addr: Tuple[str, int]):
        """Отправить ping"""
        msg = {
            'type': 'ping',
            'node_id': self.node_id.hex(),
            'host': self.host,
            'port': self.port
        }
        self.udp_sock.sendto(json.dumps(msg).encode(), addr)

# ==================== P2PNode ====================
class P2PNode:
    """Основной P2P узел мессенджера"""
    
    def __init__(self, host: str = '0.0.0.0', port: int = 0, bootstrap_servers: List[Tuple[str, int]] = None):
        self.host = host
        self.port = port or self._find_free_port()
        self.crypto = CryptoManager()
        self.node_id = self.crypto.get_node_id()
        
        # DHT для поиска пиров
        self.dht = DHTNode(self.node_id, self.host, self.port)
        
        # Активные соединения: peer_id -> (socket, address)
        self.peers: Dict[bytes, socket.socket] = {}
        self.peers_lock = threading.Lock()
        
        # Очередь сообщений для GUI
        self.message_queue: List[dict] = []
        self.callbacks: List[Callable] = []
        
        # Запуск TCP сервера
        self.running = True
        self.server_thread = threading.Thread(target=self._run_server)
        self.server_thread.daemon = True
        self.server_thread.start()
        
        # Bootstrap DHT, если указаны серверы
        if bootstrap_servers:
            self.dht.bootstrap(bootstrap_servers)
        
    def _find_free_port(self) -> int:
        """Найти свободный порт"""
        sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        sock.bind(('', 0))
        port = sock.getsockname()[1]
        sock.close()
        return port
    
    def _run_server(self):
        """TCP сервер для входящих соединений"""
        server = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        server.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        server.bind((self.host, self.port))
        server.listen(10)
        
        print(f"🟢 Узел запущен: {self.node_id.hex()[:8]}@{self.host}:{self.port}")
        
        while self.running:
            try:
                client, addr = server.accept()
                thread = threading.Thread(target=self._handle_peer, args=(client, addr))
                thread.daemon = True
                thread.start()
            except:
                break
    
    def _handle_peer(self, sock: socket.socket, addr: tuple):
        """Обработка входящего TCP соединения (handshake)"""
        try:
            # Получаем handshake
            data = sock.recv(4096)
            handshake = json.loads(data.decode())
            
            if handshake['type'] == 'handshake':
                peer_id = bytes.fromhex(handshake['node_id'])
                
                # Сохраняем публичный ключ пира
                pub_key_pem = handshake['public_key'].encode()
                from cryptography.hazmat.primitives.serialization import load_pem_public_key
                self.crypto.peer_public_keys[peer_id] = load_pem_public_key(
                    pub_key_pem, backend=default_backend()
                )
                
                # Отправляем свой handshake
                response = {
                    'type': 'handshake_ack',
                    'node_id': self.node_id.hex(),
                    'public_key': self.crypto.public_key.public_bytes(
                        encoding=serialization.Encoding.PEM,
                        format=serialization.PublicFormat.SubjectPublicKeyInfo
                    ).decode()
                }
                sock.send(json.dumps(response).encode())
                
                # Добавляем пира в DHT и в список активных
                self.dht.add_node(peer_id, addr[0], addr[1])
                with self.peers_lock:
                    self.peers[peer_id] = sock
                
                print(f"✅ Подключён пир {peer_id.hex()[:8]} от {addr}")
                
                # Запускаем прием сообщений от этого пира
                self._receive_messages(sock, peer_id)
                
        except Exception as e:
            print(f"Ошибка handshake: {e}")
            sock.close()
    
    def _receive_messages(self, sock: socket.socket, peer_id: bytes):
        """Прием сообщений от пира (TCP)"""
        while self.running:
            try:
                # Сначала читаем длину сообщения (4 байта big-endian)
                raw_len = sock.recv(4)
                if not raw_len:
                    break
                msg_len = int.from_bytes(raw_len, 'big')
                
                # Читаем само сообщение
                data = b''
                while len(data) < msg_len:
                    chunk = sock.recv(min(4096, msg_len - len(data)))
                    if not chunk:
                        break
                    data += chunk
                if len(data) != msg_len:
                    break
                
                # Расшифровываем
                message = self.crypto.decrypt_message(data)
                
                # Добавляем в очередь
                msg_data = {
                    'from': peer_id.hex()[:8],
                    'message': message,
                    'time': time.strftime('%H:%M:%S')
                }
                self.message_queue.append(msg_data)
                
                # Уведомляем GUI
                for callback in self.callbacks:
                    callback(msg_data)
                    
            except Exception as e:
                print(f"Ошибка приема от {peer_id.hex()[:8]}: {e}")
                break
        
        # Пир отключился
        with self.peers_lock:
            if peer_id in self.peers:
                del self.peers[peer_id]
        sock.close()
        print(f"🔴 Пир {peer_id.hex()[:8]} отключился")
    
    def connect_to_peer(self, peer_id_hex: str, host: str = None, port: int = None) -> bool:
        """Подключение к другому узлу по ID (и опционально адресу)"""
        target_id = bytes.fromhex(peer_id_hex)
        
        # Если уже подключены
        with self.peers_lock:
            if target_id in self.peers:
                print(f"Уже подключены к {peer_id_hex[:8]}")
                return True
        
        # Если есть прямой адрес, пробуем подключиться напрямую
        if host and port:
            return self._connect_direct(target_id, host, port)
        
        # Иначе ищем через DHT
        nearest = self.dht.find_nearest(target_id)
        for nid, nhost, nport in nearest:
            # Пробуем подключиться к ближайшему узлу, чтобы через него найти целевой
            # Но в нашей упрощённой реализации просто пробуем прямой коннект по адресу из DHT
            if self._connect_direct(target_id, nhost, nport):
                return True
        return False
    
    def _connect_direct(self, peer_id: bytes, host: str, port: int) -> bool:
        """Прямое TCP подключение к пиру"""
        try:
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            sock.settimeout(5)
            sock.connect((host, port))
            
            # Отправляем handshake
            handshake = {
                'type': 'handshake',
                'node_id': self.node_id.hex(),
                'public_key': self.crypto.public_key.public_bytes(
                    encoding=serialization.Encoding.PEM,
                    format=serialization.PublicFormat.SubjectPublicKeyInfo
                ).decode()
            }
            sock.send(json.dumps(handshake).encode())
            
            # Получаем ответ
            data = sock.recv(4096)
            response = json.loads(data.decode())
            
            if response['type'] == 'handshake_ack':
                # Сохраняем ключ пира
                from cryptography.hazmat.primitives.serialization import load_pem_public_key
                self.crypto.peer_public_keys[peer_id] = load_pem_public_key(
                    response['public_key'].encode(), backend=default_backend()
                )
                
                with self.peers_lock:
                    self.peers[peer_id] = sock
                self.dht.add_node(peer_id, host, port)
                
                # Запускаем прием сообщений
                thread = threading.Thread(target=self._receive_messages, args=(sock, peer_id))
                thread.daemon = True
                thread.start()
                
                print(f"✅ Подключено к {peer_id.hex()[:8]}")
                return True
                
        except Exception as e:
            print(f"Ошибка подключения к {peer_id.hex()[:8]}: {e}")
            try:
                sock.close()
            except:
                pass
        return False
    
    def send_message(self, peer_id_hex: str, message: str) -> bool:
        """Отправка сообщения пиру"""
        peer_id = bytes.fromhex(peer_id_hex)
        
        with self.peers_lock:
            if peer_id not in self.peers:
                # Пытаемся найти и подключиться
                print(f"Нет подключения к {peer_id_hex[:8]}, ищем...")
                # Для простоты возвращаем False, можно вызвать connect_to_peer и потом повторить
                return False
            
            sock = self.peers[peer_id]
        
        try:
            encrypted = self.crypto.encrypt_message(message, peer_id)
            # Отправляем длину (4 байта) + данные
            sock.send(len(encrypted).to_bytes(4, 'big'))
            sock.send(encrypted)
            
            # Добавляем в очередь (своё сообщение)
            msg_data = {
                'from': 'me',
                'to': peer_id.hex()[:8],
                'message': message,
                'time': time.strftime('%H:%M:%S')
            }
            self.message_queue.append(msg_data)
            
            for callback in self.callbacks:
                callback(msg_data)
                
            return True
        except Exception as e:
            print(f"Ошибка отправки: {e}")
            return False
    
    def on_message(self, callback: Callable):
        """Подписка на новые сообщения"""
        self.callbacks.append(callback)
