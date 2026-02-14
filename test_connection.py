#!/usr/bin/env python3
import socket
import sys

def test_connection(host='localhost', port=54321):
    print(f"Тестирование подключения к {host}:{port}")
    print("-" * 40)
    
    try:
        # Проверяем резолвинг имени
        ip = socket.gethostbyname(host)
        print(f"✓ {host} резолвится в {ip}")
    except socket.gaierror:
        print(f"✗ Не удается найти {host}")
        return False
    
    # Пробуем подключиться
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(3)
    
    try:
        result = sock.connect_ex((ip, port))
        if result == 0:
            print(f"✓ Порт {port} открыт и доступен")
            
            # Пробуем отправить тестовое сообщение
            test_msg = '{"type": "ping"}'
            sock.send(test_msg.encode())
            print("✓ Тестовое сообщение отправлено")
            
            # Пробуем получить ответ
            try:
                data = sock.recv(1024)
                if data:
                    print(f"✓ Получен ответ: {data.decode()[:50]}")
                else:
                    print("✗ Нет ответа от сервера")
            except socket.timeout:
                print("✗ Таймаут при ожидании ответа")
        else:
            print(f"✗ Порт {port} недоступен (код ошибки: {result})")
            print(f"  Проверь: запущен ли сервер?")
        
        sock.close()
        return result == 0
        
    except Exception as e:
        print(f"✗ Ошибка: {e}")
        return False
    finally:
        sock.close()

if __name__ == "__main__":
    host = sys.argv[1] if len(sys.argv) > 1 else "localhost"
    test_connection(host)
