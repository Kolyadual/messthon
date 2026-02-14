#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import os
import json
import base64
from cryptography.fernet import Fernet
from cryptography.hazmat.primitives import hashes
from cryptography.hazmat.primitives.kdf.pbkdf2 import PBKDF2HMAC

class CryptoManager:
    """Управление шифрованием данных"""
    
    def __init__(self):
        self.data_dir = os.path.expanduser("~/.messthon")
        self.key_file = os.path.join(self.data_dir, "key.key")
        self.config_file = os.path.join(self.data_dir, "config.enc")
        self.history_dir = os.path.join(self.data_dir, "history")
        
        # Создаем директории
        os.makedirs(self.data_dir, exist_ok=True)
        os.makedirs(self.history_dir, exist_ok=True)
        
        # Загружаем или создаем ключ
        self.key = self._load_or_create_key()
        self.cipher = Fernet(self.key)
    
    def _load_or_create_key(self):
        """Загружает существующий ключ или создает новый"""
        if os.path.exists(self.key_file):
            with open(self.key_file, 'rb') as f:
                return f.read()
        else:
            key = Fernet.generate_key()
            with open(self.key_file, 'wb') as f:
                f.write(key)
            # Устанавливаем права только для владельца
            os.chmod(self.key_file, 0o600)
            return key
    
    def encrypt_data(self, data):
        """Шифрует данные (словарь -> зашифрованная строка)"""
        json_str = json.dumps(data, ensure_ascii=False)
        encrypted = self.cipher.encrypt(json_str.encode('utf-8'))
        return base64.b64encode(encrypted).decode('utf-8')
    
    def decrypt_data(self, encrypted_str):
        """Дешифрует данные (зашифрованная строка -> словарь)"""
        try:
            encrypted = base64.b64decode(encrypted_str.encode('utf-8'))
            decrypted = self.cipher.decrypt(encrypted)
            return json.loads(decrypted.decode('utf-8'))
        except Exception as e:
            print(f"Ошибка дешифровки: {e}")
            return None
    
    def save_config(self, config):
        """Сохраняет конфигурацию (username, code и т.д.)"""
        encrypted = self.encrypt_data(config)
        with open(self.config_file, 'w', encoding='utf-8') as f:
            f.write(encrypted)
        os.chmod(self.config_file, 0o600)
    
    def load_config(self):
        """Загружает конфигурацию"""
        if os.path.exists(self.config_file):
            with open(self.config_file, 'r', encoding='utf-8') as f:
                encrypted = f.read()
            return self.decrypt_data(encrypted)
        return None
    
    def save_chat_history(self, friend_code, messages):
        """Сохраняет историю чата с другом"""
        history_file = os.path.join(self.history_dir, f"{friend_code}.enc")
        encrypted = self.encrypt_data(messages)
        with open(history_file, 'w', encoding='utf-8') as f:
            f.write(encrypted)
    
    def load_chat_history(self, friend_code):
        """Загружает историю чата с другом"""
        history_file = os.path.join(self.history_dir, f"{friend_code}.enc")
        if os.path.exists(history_file):
            with open(history_file, 'r', encoding='utf-8') as f:
                encrypted = f.read()
            return self.decrypt_data(encrypted) or []
        return []
