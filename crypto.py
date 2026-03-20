# crypto.py
import hashlib
from cryptography.hazmat.primitives import hashes, serialization
from cryptography.hazmat.primitives.asymmetric import rsa, padding
from cryptography.hazmat.primitives.ciphers import Cipher, algorithms, modes
from cryptography.hazmat.backends import default_backend
import os
import base64

class CryptoManager:
    """Управление шифрованием (RSA + AES)"""
    
    def __init__(self):
        # Генерируем RSA ключи (как в ptp-chat [citation:9])
        self.private_key = rsa.generate_private_key(
            public_exponent=65537,
            key_size=2048,
            backend=default_backend()
        )
        self.public_key = self.private_key.public_key()
        self.peer_public_keys = {}  # peer_id -> public_key
        
    def get_node_id(self) -> bytes:
        """ID узла = хеш публичного ключа"""
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
