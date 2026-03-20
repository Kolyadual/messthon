# gui.py
import tkinter as tk
from tkinter import scrolledtext, ttk, messagebox
import threading
import time

class P2PChatGUI:
    def __init__(self, node):
        self.node = node
        self.node.on_message(self.display_message)
        
        self.window = tk.Tk()
        self.window.title(f"Messthon DHT - {node.node_id.hex()[:8]}")
        self.window.geometry("750x550")
        self.window.configure(bg='#2b2b2b')
        
        self.setup_ui()
        
    def setup_ui(self):
        # Верхняя панель с ID (теперь можно выделить и скопировать)
        info_frame = tk.Frame(self.window, bg='#3c3f41', height=40)
        info_frame.pack(fill='x')
        info_frame.pack_propagate(False)
        
        # Собственный ID в поле для выделения
        self.my_id_entry = tk.Entry(
            info_frame,
            bg='#2b2b2b',
            fg='#4a6fa5',
            font=('Arial', 9, 'bold'),
            bd=2,
            relief='flat',
            readonlybackground='#2b2b2b',
            justify='center'
        )
        self.my_id_entry.insert(0, self.node.node_id.hex())
        self.my_id_entry.config(state='readonly')
        self.my_id_entry.pack(side='left', padx=(10,5), pady=10, fill='x', expand=True)
        
        # Кнопка копирования ID
        copy_self_btn = tk.Button(
            info_frame,
            text="📋 Копировать мой ID",
            bg='#4a6fa5',
            fg='white',
            font=('Arial', 8),
            command=self.copy_my_id
        )
        copy_self_btn.pack(side='left', padx=5, pady=10)
        
        # Порт узла (для информации)
        port_label = tk.Label(
            info_frame,
            text=f"📡 {self.node.host}:{self.node.port}",
            bg='#3c3f41',
            fg='#6b8e23',
            font=('Arial', 9)
        )
        port_label.pack(side='right', padx=10, pady=10)
        
        # Основная область
        main_frame = tk.Frame(self.window, bg='#2b2b2b')
        main_frame.pack(fill='both', expand=True, padx=10, pady=10)
        
        # Левая панель - список пиров
        peer_frame = tk.Frame(main_frame, bg='#3c3f41', width=200)
        peer_frame.pack(side='left', fill='y', padx=(0,10))
        peer_frame.pack_propagate(False)
        
        tk.Label(
            peer_frame,
            text="👥 Пиры (двойной клик для копирования ID)",
            bg='#3c3f41',
            fg='white',
            font=('Arial', 10, 'bold')
        ).pack(pady=5)
        
        # Список пиров с возможностью выделения
        self.peer_listbox = tk.Listbox(
            peer_frame,
            bg='#2b2b2b',
            fg='white',
            selectbackground='#4a6fa5',
            selectforeground='white',
            font=('Arial', 10),
            height=15,
            exportselection=False  # чтобы можно было выделять текст
        )
        self.peer_listbox.pack(fill='both', expand=True, padx=5, pady=5)
        
        # Привязываем двойной клик для копирования ID выбранного пира
        self.peer_listbox.bind('<Double-Button-1>', self.copy_selected_peer_id)
        
        # Кнопка копирования ID выбранного пира
        btn_frame = tk.Frame(peer_frame, bg='#3c3f41')
        btn_frame.pack(fill='x', padx=5, pady=5)
        
        copy_peer_btn = tk.Button(
            btn_frame,
            text="📋 Копировать ID пира",
            bg='#4a6fa5',
            fg='white',
            font=('Arial', 9),
            command=self.copy_selected_peer_id
        )
        copy_peer_btn.pack(fill='x', pady=2)
        
        # Поле ввода ID пира для подключения
        tk.Label(peer_frame, text="ID пира для подключения:", bg='#3c3f41', fg='white', font=('Arial', 8)).pack(pady=(5,0))
        self.peer_id_entry = tk.Entry(
            peer_frame,
            bg='#2b2b2b',
            fg='white',
            font=('Arial', 9),
            insertbackground='white'
        )
        self.peer_id_entry.pack(fill='x', padx=5, pady=2)
        
        connect_btn = tk.Button(
            peer_frame,
            text="🔌 Подключиться",
            bg='#6b8e23',
            fg='white',
            font=('Arial', 9, 'bold'),
            command=self.connect_to_peer
        )
        connect_btn.pack(fill='x', padx=5, pady=5)
        
        # Правая панель - чат
        chat_frame = tk.Frame(main_frame, bg='#2b2b2b')
        chat_frame.pack(side='right', fill='both', expand=True)
        
        # Область сообщений
        self.chat_area = scrolledtext.ScrolledText(
            chat_frame,
            wrap=tk.WORD,
            bg='#3c3f41',
            fg='white',
            font=('Arial', 10),
            state='disabled'
        )
        self.chat_area.pack(fill='both', expand=True)
        
        # Цветовые теги
        self.chat_area.tag_config('system', foreground='#888888')
        self.chat_area.tag_config('me', foreground='#4a6fa5')
        self.chat_area.tag_config('peer', foreground='#6b8e23')
        
        # Нижняя панель ввода сообщения
        bottom_frame = tk.Frame(self.window, bg='#3c3f41', height=60)
        bottom_frame.pack(fill='x', side='bottom')
        bottom_frame.pack_propagate(False)
        
        self.message_entry = tk.Entry(
            bottom_frame,
            bg='#2b2b2b',
            fg='white',
            font=('Arial', 11),
            insertbackground='white'
        )
        self.message_entry.pack(side='left', fill='both', expand=True, padx=10, pady=10)
        self.message_entry.bind('<Return>', self.send_message)
        
        send_btn = tk.Button(
            bottom_frame,
            text="📤 Отправить",
            bg='#4a6fa5',
            fg='white',
            font=('Arial', 10, 'bold'),
            command=self.send_message
        )
        send_btn.pack(side='right', padx=10, pady=10)
        
        # Запускаем обновление списка пиров
        self.update_peer_list()
        
    def copy_my_id(self):
        """Копирование своего ID в буфер обмена"""
        self.window.clipboard_clear()
        self.window.clipboard_append(self.node.node_id.hex())
        self.show_tooltip("✅ Мой ID скопирован!")
        
    def copy_selected_peer_id(self, event=None):
        """Копирование ID выбранного пира из списка"""
        selection = self.peer_listbox.curselection()
        if not selection:
            self.show_tooltip("❌ Нет выбранного пира")
            return
        
        peer_text = self.peer_listbox.get(selection[0])
        # Формат: "a1b2c3d4... 🟢", извлекаем ID до пробела
        peer_id = peer_text.split()[0]
        self.window.clipboard_clear()
        self.window.clipboard_append(peer_id)
        self.show_tooltip(f"✅ ID {peer_id} скопирован")
        
    def show_tooltip(self, message):
        """Временное сообщение в строке статуса"""
        # Удаляем предыдущую метку, если она существует
        if hasattr(self, 'status_label') and self.status_label:
            try:
                self.status_label.destroy()
            except:
                pass
        # Создаём новую метку
        self.status_label = tk.Label(
            self.window,
            text=message,
            bg='#2b2b2b',
            fg='#4a6fa5',
            font=('Arial', 9)
        )
        self.status_label.pack(side='bottom', pady=2)
        # Удалим через 2 секунды
        self.window.after(2000, lambda: self._destroy_status_label())
    
    def _destroy_status_label(self):
        """Безопасное удаление метки"""
        if hasattr(self, 'status_label') and self.status_label:
            try:
                self.status_label.destroy()
            except:
                pass
            self.status_label = None
        
    def connect_to_peer(self):
        peer_id = self.peer_id_entry.get().strip()
        if not peer_id:
            self.show_tooltip("❌ Введите ID пира")
            return
            
        threading.Thread(
            target=self.node.connect_to_peer,
            args=(peer_id,)
        ).start()
        self.show_tooltip(f"🔌 Подключение к {peer_id[:8]}...")
        
    def send_message(self, event=None):
        message = self.message_entry.get().strip()
        if not message:
            return
            
        # Получаем выбранного пира
        selection = self.peer_listbox.curselection()
        if not selection:
            self.display_message({
                'from': 'system',
                'message': '❌ Выберите пира из списка'
            })
            return
            
        peer_text = self.peer_listbox.get(selection[0])
        peer_id = peer_text.split()[0]  # ID пира
        
        # Отправляем в отдельном потоке
        threading.Thread(
            target=self.node.send_message,
            args=(peer_id, message)
        ).start()
        
        self.message_entry.delete(0, 'end')
        
    def display_message(self, msg_data):
        def _display():
            self.chat_area.config(state='normal')
            
            if msg_data['from'] == 'system':
                tag = 'system'
                prefix = '🔹 '
            elif msg_data['from'] == 'me':
                tag = 'me'
                prefix = f"👤 Я -> {msg_data.get('to', 'всем')} [{msg_data['time']}]: "
            else:
                tag = 'peer'
                prefix = f"👥 {msg_data['from']} [{msg_data['time']}]: "
            
            self.chat_area.insert('end', prefix + msg_data['message'] + '\n', tag)
            self.chat_area.config(state='disabled')
            self.chat_area.see('end')
            
        self.window.after(0, _display)
    
    def update_peer_list(self):
        """Обновление списка пиров (их ID)"""
        self.peer_listbox.delete(0, 'end')
        for peer_id in self.node.peers.keys():
            self.peer_listbox.insert('end', f"{peer_id.hex()} 🟢")
        self.window.after(2000, self.update_peer_list)
    
    def run(self):
        self.window.mainloop()
