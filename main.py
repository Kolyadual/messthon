# main.py
import hashlib
from node import P2PNode
from gui import P2PChatGUI
import sys

def main():
    # Параметры из командной строки
    host = '0.0.0.0'
    port = 0
    
    if len(sys.argv) > 1:
        port = int(sys.argv[1])
    
    # Создаём P2P узел
    node = P2PNode(host, port)
    
    # Запускаем GUI
    gui = P2PChatGUI(node)
    gui.run()

if __name__ == '__main__':
    main()
