#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import socket
import threading
import json
import os
import time
import hashlib
from datetime import datetime

class MessthonServer:
    def __init__(self, host='0.0.0.0', port=54321):
        self.host = host
        self.port = port
        self.server_socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.server_socket.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        
        self.online_users = {}
        self.user_codes = {}
        self.user_contacts = {}
        
        self.data_file = 'messthon_data.json'
        self.load_data()
        self.running = True
    
    def generate_code(self, username):
        timestamp = str(time.time())
        hash_obj = hashlib.md5(f"{username}{timestamp}".encode())
        hash_hex = hash_obj.hexdigest()
        return f"{hash_hex[:4]}-{hash_hex[4:8]}-{hash_hex[8:12]}".upper()
    
    def load_data(self):
        if os.path.exists(self.data_file):
            try:
                with open(self.data_file, 'r') as f:
                    data = json.load(f)
                    self.user_codes = data.get('codes', {})
                    self.user_contacts = data.get('contacts', {})
                print(f"✓ Загружено {len(self.user_codes)} пользователей")
            except Exception as e:
                print(f"✗ Ошибка загрузки: {e}")
    
    def save_data(self):
        data = {
            'codes': self.user_codes,
            'contacts': self.user_contacts
        }
        try:
            with open(self.data_file, 'w') as f:
                json.dump(data, f, indent=2)
        except Exception as e:
            print(f"✗ Ошибка сохранения: {e}")
    
    def handle_client(self, client_socket, address):
        print(f"✓ Новое подключение: {address}")
        username = None
        
        try:
            client_socket.settimeout(60)
            
            while self.running:
                try:
                    data = client_socket.recv(4096).decode('utf-8')
                    if not data:
                        break
                    
                    message = json.loads(data)
                    msg_type = message.get('type')
                    
                    if msg_type == 'login':
                        username = message['username']
                        code = message.get('code', '')
                        
                        if username in self.user_codes:
                            if self.user_codes[username] == code:
                                self.online_users[username] = (client_socket, address)
                                response = {
                                    'type': 'login_success',
                                    'code': self.user_codes[username],
                                    'contacts': self.user_contacts.get(username, [])
                                }
                                print(f"✓ Вошел: {username}")
                            else:
                                response = {
                                    'type': 'login_failed',
                                    'message': 'Неверный код'
                                }
                        else:
                            code = self.generate_code(username)
                            self.user_codes[username] = code
                            self.user_contacts[username] = []
                            self.online_users[username] = (client_socket, address)
                            self.save_data()
                            
                            response = {
                                'type': 'login_success',
                                'code': code,
                                'contacts': []
                            }
                            print(f"✓ Новый пользователь: {username}")
                        
                        client_socket.send(json.dumps(response).encode('utf-8'))
                    
                    elif msg_type == 'get_user_by_code':
                        search_code = message['code']
                        found_user = None
                        for user, user_code in self.user_codes.items():
                            if user_code == search_code and user != username:
                                found_user = user
                                break
                        
                        if found_user:
                            response = {
                                'type': 'user_found',
                                'username': found_user,
                                'online': found_user in self.online_users
                            }
                        else:
                            response = {
                                'type': 'user_not_found',
                                'message': 'Пользователь не найден'
                            }
                        client_socket.send(json.dumps(response).encode('utf-8'))
                    
                    elif msg_type == 'add_contact':
                        if username:
                            friend_code = message['code']
                            friend_name = None
                            
                            for user, user_code in self.user_codes.items():
                                if user_code == friend_code and user != username:
                                    friend_name = user
                                    break
                            
                            if friend_name:
                                if friend_code not in self.user_contacts.get(username, []):
                                    self.user_contacts[username].append(friend_code)
                                    self.save_data()
                                    
                                    response = {
                                        'type': 'contact_added',
                                        'contact': {
                                            'code': friend_code,
                                            'name': friend_name,
                                            'online': friend_name in self.online_users
                                        }
                                    }
                                else:
                                    response = {
                                        'type': 'contact_exists',
                                        'message': 'Контакт уже добавлен'
                                    }
                            else:
                                response = {
                                    'type': 'contact_failed',
                                    'message': 'Код не найден'
                                }
                            
                            client_socket.send(json.dumps(response).encode('utf-8'))
                    
                    elif msg_type == 'message':
                        sender = username
                        recipient = message['recipient']
                        content = message['content']
                        timestamp = message.get('timestamp', datetime.now().isoformat())
                        
                        if recipient in self.online_users:
                            recipient_socket = self.online_users[recipient][0]
                            forward_msg = {
                                'type': 'message',
                                'sender': sender,
                                'content': content,
                                'timestamp': timestamp
                            }
                            try:
                                recipient_socket.send(json.dumps(forward_msg).encode('utf-8'))
                                response = {'type': 'delivered'}
                            except:
                                response = {'type': 'failed'}
                        else:
                            response = {
                                'type': 'user_offline',
                                'message': f'{recipient} не в сети'
                            }
                        
                        client_socket.send(json.dumps(response).encode('utf-8'))
                    
                    elif msg_type == 'ping':
                        client_socket.send(json.dumps({'type': 'pong'}).encode('utf-8'))
                        
                except socket.timeout:
                    continue
                except json.JSONDecodeError:
                    continue
                    
        except Exception as e:
            print(f"✗ Ошибка клиента {address}: {e}")
        finally:
            if username and username in self.online_users:
                del self.online_users[username]
                print(f"✗ Отключился: {username}")
            client_socket.close()
    
    def start(self):
        try:
            self.server_socket.bind(('0.0.0.0', self.port))
            self.server_socket.listen(5)
            self.server_socket.settimeout(1.0)
            
            hostname = socket.gethostname()
            local_ip = socket.gethostbyname(hostname)
            
            print("╔════════════════════════════════════╗")
            print("║       Messthon Server v2.0        ║")
            print("║          (Qt6 Edition)             ║")
            print("╚════════════════════════════════════╝")
            print(f"✓ Сервер запущен на порту {self.port}")
            print(f"✓ Для подключения используй:")
            print(f"  • localhost (если с этого же компьютера)")
            print(f"  • {local_ip} (если с других устройств в сети)")
            print(f"✓ Ожидание подключений...\n")
            
            while self.running:
                try:
                    client_socket, address = self.server_socket.accept()
                    print(f"✓ Новое подключение от {address}")
                    client_thread = threading.Thread(
                        target=self.handle_client,
                        args=(client_socket, address)
                    )
                    client_thread.daemon = True
                    client_thread.start()
                except socket.timeout:
                    continue
                except KeyboardInterrupt:
                    break
                    
        except Exception as e:
            print(f"✗ Ошибка сервера: {e}")
            print(f"✗ Проверь, не занят ли порт {self.port}")
        finally:
            self.server_socket.close()
            print("✓ Сервер остановлен")
    
    def stop(self):
        self.running = False

if __name__ == '__main__':
    server = MessthonServer()
    try:
        server.start()
    except KeyboardInterrupt:
        server.stop()
