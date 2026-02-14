#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import socket
import json
import threading
from datetime import datetime
from PySide6.QtWidgets import QApplication, QMessageBox
from PySide6.QtCore import QObject, Signal, QTimer

from crypto import CryptoManager
from login_window import LoginWindow
from chat_window import ChatWindow

class NetworkClient(QObject):
    """Сетевой клиент в отдельном потоке"""
    
    message_received = Signal(dict)
    connection_lost = Signal()
    
    def __init__(self):
        super().__init__()
        self.socket = None
        self.connected = False
        self.running = False
    
    def connect_to_server(self, host='127.0.0.1', port=54321):
        """Подключается к серверу"""
        try:
            print(f"Попытка подключения к {host}:{port}")
            self.socket = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            self.socket.settimeout(5)
            self.socket.connect((host, port))
            self.connected = True
            self.running = True
            print(f"✓ Успешно подключено к {host}:{port}")
            
            # Запускаем поток приема
            self.receive_thread = threading.Thread(target=self.receive_messages)
            self.receive_thread.daemon = True
            self.receive_thread.start()
            
            return True
        except socket.timeout:
            print(f"✗ Таймаут подключения к {host}:{port}")
            return False
        except ConnectionRefusedError:
            print(f"✗ Соединение отклонено. Сервер не запущен на {host}:{port}")
            return False
        except Exception as e:
            print(f"✗ Ошибка подключения: {e}")
            return False
    
    def receive_messages(self):
        """Поток для приема сообщений"""
        while self.running and self.connected:
            try:
                self.socket.settimeout(1.0)
                data = self.socket.recv(4096).decode('utf-8')
                if data:
                    message = json.loads(data)
                    self.message_received.emit(message)
            except socket.timeout:
                continue
            except Exception as e:
                print(f"Ошибка приема: {e}")
                self.connection_lost.emit()
                break
    
    def send_message(self, message):
        """Отправляет сообщение"""
        if self.connected:
            try:
                self.socket.send(json.dumps(message).encode('utf-8'))
                return True
            except Exception as e:
                print(f"Ошибка отправки: {e}")
                return False
        return False
    
    def disconnect(self):
        """Отключается от сервера"""
        self.running = False
        self.connected = False
        if self.socket:
            try:
                self.socket.close()
            except:
                pass


class MessthonClient:
    """Главный класс клиента"""
    
    def __init__(self):
        self.app = QApplication(sys.argv)
        self.app.setStyle('Fusion')
        
        # Инициализируем крипто-менеджер
        self.crypto = CryptoManager()
        
        # Сетевой клиент
        self.network = NetworkClient()
        self.network.message_received.connect(self.handle_message)
        self.network.connection_lost.connect(self.handle_connection_lost)
        
        # Окна
        self.login_window = None
        self.chat_window = None
        
        # Данные пользователя
        self.username = None
        self.user_code = None
        self.contacts = []
    
    def run(self):
        """Запускает приложение"""
        self.show_login_window()
        return self.app.exec()
    
    def show_login_window(self):
        """Показывает окно входа"""
        self.login_window = LoginWindow(self.crypto)
        self.login_window.login_successful.connect(self.handle_login)
        self.login_window.show()
    
    def handle_login(self, username, code, server="localhost"):
        """Обрабатывает попытку входа"""
        print(f"Попытка входа: username={username}, server={server}")
        
        # Подключаемся к серверу
        if not self.network.connect_to_server(host=server):
            self.login_window.login_failed(
                f"Не удалось подключиться к серверу {server}:54321\n"
                "Проверьте:\n"
                "1. Запущен ли сервер\n"
                "2. Правильный ли адрес\n"
                "3. Не блокирует ли файрвол"
            )
            return
        
        # Отправляем запрос на вход
        login_msg = {
            'type': 'login',
            'username': username,
            'code': code
        }
        
        if not self.network.send_message(login_msg):
            self.login_window.login_failed("Ошибка отправки запроса")
            self.network.disconnect()
            return
        
        # Ждем ответ
        QTimer.singleShot(5000, lambda: self.check_login_timeout())
    
    def check_login_timeout(self):
        """Проверяет таймаут входа"""
        if not self.username:
            self.login_window.login_failed("Таймаут подключения. Сервер не отвечает.")
            self.network.disconnect()
    
    def handle_message(self, message):
        """Обрабатывает входящие сообщения"""
        msg_type = message.get('type')
        
        if msg_type == 'login_success':
            self.username = self.login_window.username_input.text()
            self.user_code = message['code']
            self.contacts = message.get('contacts', [])
            
            # Сохраняем данные
            config = {
                'username': self.username,
                'code': self.user_code,
                'last_login': datetime.now().isoformat()
            }
            self.crypto.save_config(config)
            
            # Показываем главное окно
            self.login_window.login_completed(self.username, self.user_code)
            self.login_window.close()
            self.show_chat_window()
            
        elif msg_type == 'login_failed':
            self.login_window.login_failed(message.get('message', 'Ошибка входа'))
            self.network.disconnect()
        
        elif msg_type == 'user_found':
            pass
            
        elif msg_type == 'user_not_found':
            pass
            
        elif msg_type == 'contact_added':
            if self.chat_window:
                contact = message.get('contact')
                if contact:
                    self.contacts.append(contact)
                    self.chat_window.update_contacts_list(self.contacts)
                    QMessageBox.information(self.chat_window, "Успех", "Контакт добавлен!")
            
        elif msg_type == 'contact_exists':
            if self.chat_window:
                QMessageBox.information(self.chat_window, "Информация", 
                                      message.get('message', 'Контакт уже добавлен'))
            
        elif msg_type == 'contact_failed':
            if self.chat_window:
                QMessageBox.warning(self.chat_window, "Ошибка", 
                                  message.get('message', 'Не удалось добавить контакт'))
            
        elif msg_type == 'message':
            if self.chat_window:
                self.chat_window.handle_incoming_message(
                    message['sender'],
                    message['content'],
                    message.get('timestamp', datetime.now().isoformat())
                )
        
        elif msg_type == 'delivered':
            pass
    
    def handle_connection_lost(self):
        """Обрабатывает потерю соединения"""
        if self.chat_window:
            self.chat_window.close()
        
        QMessageBox.critical(None, "Ошибка", 
                           "Потеряно соединение с сервером")
        self.show_login_window()
    
    def show_chat_window(self):
        """Показывает главное окно чата"""
        self.chat_window = ChatWindow(
            self.crypto,
            self.username,
            self.user_code,
            self.network.socket
        )
        self.chat_window.show()


if __name__ == '__main__':
    client = MessthonClient()
    sys.exit(client.run())
