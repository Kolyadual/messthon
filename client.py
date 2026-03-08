# client.py
import tkinter as tk
from tkinter import scrolledtext, messagebox
import threading
import websocket
import requests
import json
from datetime import datetime

class MessengerClient:
    def __init__(self):
        self.window = tk.Tk()
        self.window.title("Messthon - Global Messenger")
        self.window.geometry("800x600")
        self.window.configure(bg='#2b2b2b')
        
        self.user_id = None
        self.username = None
        self.server_url = None
        self.ws_url = None
        self.ws = None
        self.users = {}  # id -> username
        self.current_recipient = None
        
        self.show_connect_screen()
        self.window.protocol("WM_DELETE_WINDOW", self.on_closing)
    
    # ---------- Экран подключения к серверу ----------
    def show_connect_screen(self):
        self.clear_window()
        
        tk.Label(self.window, text="Messthon", font=("Arial", 24, "bold"),
                 bg='#2b2b2b', fg='#4a6fa5').pack(pady=30)
        
        frame = tk.Frame(self.window, bg='#2b2b2b')
        frame.pack(expand=True)
        
        tk.Label(frame, text="Адрес сервера:", bg='#2b2b2b', fg='white').grid(row=0, column=0, pady=5)
        self.server_entry = tk.Entry(frame, width=25, font=('Arial', 11))
        self.server_entry.insert(0, "194.87.234.1:8000")  # пример IP
        self.server_entry.grid(row=0, column=1, pady=5, padx=10)
        
        tk.Label(frame, text="Пример: 194.87.234.1:8000", bg='#2b2b2b', fg='#888888', font=('Arial', 8)).grid(row=1, column=1, sticky='w')
        
        tk.Button(frame, text="Подключиться к серверу", bg='#4a6fa5', fg='white', font=('Arial', 12),
                  command=self.connect_to_server).grid(row=2, column=0, columnspan=2, pady=20)
    
    # ---------- Экран входа/регистрации ----------
    def show_auth_screen(self):
        self.clear_window()
        
        tk.Label(self.window, text=f"Сервер: {self.server_url}", bg='#2b2b2b', fg='#4a6fa5').pack(pady=5)
        tk.Label(self.window, text="Вход / Регистрация", font=("Arial", 18),
                 bg='#2b2b2b', fg='white').pack(pady=20)
        
        frame = tk.Frame(self.window, bg='#2b2b2b')
        frame.pack(expand=True)
        
        tk.Label(frame, text="Логин:", bg='#2b2b2b', fg='white').grid(row=0, column=0, pady=5)
        self.login_entry = tk.Entry(frame, width=20, font=('Arial', 11))
        self.login_entry.grid(row=0, column=1, pady=5, padx=10)
        
        tk.Label(frame, text="Код доступа:", bg='#2b2b2b', fg='white').grid(row=1, column=0, pady=5)
        self.code_entry = tk.Entry(frame, width=20, font=('Arial', 11), show="*")
        self.code_entry.grid(row=1, column=1, pady=5, padx=10)
        
        btn_frame = tk.Frame(frame, bg='#2b2b2b')
        btn_frame.grid(row=2, column=0, columnspan=2, pady=20)
        
        tk.Button(btn_frame, text="Войти", bg='#4a6fa5', fg='white', width=10,
                  command=self.login).pack(side='left', padx=5)
        tk.Button(btn_frame, text="Регистрация", bg='#6b8e23', fg='white', width=12,
                  command=self.register).pack(side='left', padx=5)
        tk.Button(btn_frame, text="Назад", bg='#888888', fg='white', width=8,
                  command=self.show_connect_screen).pack(side='left', padx=5)
        
        self.status_label = tk.Label(self.window, text="", bg='#2b2b2b', fg='#888888')
        self.status_label.pack(side='bottom', pady=10)
    
    # ---------- Логика подключения ----------
    def connect_to_server(self):
        server = self.server_entry.get().strip()
        if not server:
            return
        self.server_url = server
        self.ws_url = f"ws://{server}/ws"
        self.show_auth_screen()
    
    def register(self):
        username = self.login_entry.get().strip()
        code = self.code_entry.get().strip()
        
        if not username or not code:
            self.status_label.config(text="Введите логин и код доступа")
            return
        
        try:
            resp = requests.post(f"http://{self.server_url}/register",
                                 params={"username": username, "access_code": code})
            if resp.status_code == 200:
                messagebox.showinfo("Успех", "Регистрация успешна! Теперь войдите.")
            else:
                error = resp.json().get("detail", "Ошибка")
                messagebox.showerror("Ошибка", error)
        except Exception as e:
            messagebox.showerror("Ошибка", f"Не удалось подключиться к серверу: {e}")
    
    def login(self):
        username = self.login_entry.get().strip()
        code = self.code_entry.get().strip()
        
        try:
            resp = requests.post(f"http://{self.server_url}/login",
                                 params={"username": username, "access_code": code})
            if resp.status_code == 200:
                data = resp.json()
                self.user_id = data["user_id"]
                self.username = data["username"]
                self.init_websocket()
                self.load_users()
                self.show_main_screen()
            else:
                self.status_label.config(text="Неверный логин или код доступа", fg='red')
        except Exception as e:
            self.status_label.config(text=f"Ошибка: {e}", fg='red')
    
    # ---------- WebSocket ----------
    def init_websocket(self):
        ws_url = f"{self.ws_url}/{self.user_id}"
        self.ws = websocket.WebSocketApp(ws_url,
                                         on_message=self.on_ws_message,
                                         on_error=self.on_ws_error,
                                         on_close=self.on_ws_close)
        wst = threading.Thread(target=self.ws.run_forever)
        wst.daemon = True
        wst.start()
    
    def on_ws_message(self, ws, message):
        try:
            data = json.loads(message)
            if data["type"] == "message":
                self.display_message(data["sender_id"], data["content"], data["timestamp"])
            elif data["type"] == "user_status":
                self.update_user_status(data["user_id"], data["is_online"])
        except:
            pass
    
    def on_ws_error(self, ws, error):
        print(f"WebSocket error: {error}")
    
    def on_ws_close(self, ws, close_status_code, close_msg):
        print("WebSocket closed")
    
    # ---------- Загрузка пользователей ----------
    def load_users(self):
        try:
            resp = requests.get(f"http://{self.server_url}/users")
            if resp.status_code == 200:
                for u in resp.json():
                    self.users[u["id"]] = u["username"]
        except:
            pass
    
    # ---------- Главный экран ----------
    def show_main_screen(self):
        self.clear_window()
        
        # Верхняя панель
        top_frame = tk.Frame(self.window, bg='#3c3f41', height=40)
        top_frame.pack(fill='x')
        top_frame.pack_propagate(False)
        
        tk.Label(top_frame, text=f"👤 {self.username} (ID: {self.user_id})", 
                 bg='#3c3f41', fg='#4a6fa5').pack(side='left', padx=10)
        tk.Button(top_frame, text="🚪 Выйти", bg='#b22222', fg='white',
                  command=self.logout).pack(side='right', padx=10)
        
        # Основная область
        main_frame = tk.Frame(self.window, bg='#2b2b2b')
        main_frame.pack(fill='both', expand=True, padx=10, pady=10)
        
        # Левая панель - список пользователей
        left_frame = tk.Frame(main_frame, bg='#3c3f41', width=200)
        left_frame.pack(side='left', fill='y')
        left_frame.pack_propagate(False)
        
        tk.Label(left_frame, text="🌍 Пользователи онлайн", 
                 bg='#3c3f41', fg='white', font=('Arial', 11, 'bold')).pack(pady=5)
        self.users_listbox = tk.Listbox(left_frame, bg='#2b2b2b', fg='white', 
                                         selectbackground='#4a6fa5', font=('Arial', 10))
        self.users_listbox.pack(fill='both', expand=True, padx=5, pady=5)
        self.users_listbox.bind('<<ListboxSelect>>', self.on_user_select)
        
        # Правая панель - чат
        right_frame = tk.Frame(main_frame, bg='#2b2b2b')
        right_frame.pack(side='right', fill='both', expand=True, padx=(10,0))
        
        # Область сообщений
        self.chat_area = scrolledtext.ScrolledText(right_frame, wrap=tk.WORD,
                                                    bg='#3c3f41', fg='white', state='disabled')
        self.chat_area.pack(fill='both', expand=True)
        
        # Нижняя панель ввода
        bottom_frame = tk.Frame(right_frame, bg='#3c3f41', height=60)
        bottom_frame.pack(fill='x', pady=(10,0))
        bottom_frame.pack_propagate(False)
        
        self.message_entry = tk.Entry(bottom_frame, bg='#2b2b2b', fg='white', font=('Arial', 11))
        self.message_entry.pack(side='left', fill='both', expand=True, padx=10, pady=10)
        self.message_entry.bind('<Return>', self.send_message)
        
        tk.Button(bottom_frame, text="📤", bg='#4a6fa5', fg='white', font=('Arial', 12),
                  command=self.send_message).pack(side='right', padx=10)
        
        # Запускаем обновление списка
        self.update_users_list()
    
    def update_users_list(self):
        """Обновление списка пользователей (каждые 2 секунды)"""
        try:
            resp = requests.get(f"http://{self.server_url}/users")
            if resp.status_code == 200:
                self.users_listbox.delete(0, tk.END)
                for u in resp.json():
                    if u["id"] != self.user_id:
                        status = "🟢" if u["is_online"] else "⚪"
                        self.users_listbox.insert(tk.END, f"{status} {u['username']} (ID: {u['id']})")
                        self.users[u["id"]] = u["username"]
        except:
            pass
        self.window.after(2000, self.update_users_list)
    
    def on_user_select(self, event):
        selection = self.users_listbox.curselection()
        if not selection:
            return
        selected = self.users_listbox.get(selection[0])
        # Извлекаем ID
        import re
        match = re.search(r'ID: (\d+)', selected)
        if match:
            self.current_recipient = int(match.group(1))
            self.load_chat_history(self.current_recipient)
    
    def load_chat_history(self, recipient_id):
        try:
            resp = requests.get(f"http://{self.server_url}/messages/{recipient_id}",
                               params={"current_user_id": self.user_id})
            if resp.status_code == 200:
                self.chat_area.config(state='normal')
                self.chat_area.delete(1.0, tk.END)
                for msg in resp.json():
                    sender_name = "Я" if msg['sender_id'] == self.user_id else self.users.get(msg['sender_id'], "?")
                    time_str = datetime.fromisoformat(msg['timestamp']).strftime('%H:%M')
                    self.chat_area.insert(tk.END, f"[{time_str}] {sender_name}: {msg['content']}\n")
                self.chat_area.config(state='disabled')
                self.chat_area.see(tk.END)
        except:
            pass
    
    def display_message(self, sender_id, content, timestamp):
        if hasattr(self, 'current_recipient') and sender_id == self.current_recipient:
            self.chat_area.config(state='normal')
            sender_name = self.users.get(sender_id, str(sender_id))
            time_str = datetime.fromisoformat(timestamp).strftime('%H:%M')
            self.chat_area.insert(tk.END, f"[{time_str}] {sender_name}: {content}\n")
            self.chat_area.config(state='disabled')
            self.chat_area.see(tk.END)
    
    def send_message(self, event=None):
        if not hasattr(self, 'current_recipient') or not self.current_recipient:
            messagebox.showwarning("Предупреждение", "Выберите собеседника из списка")
            return
        msg = self.message_entry.get().strip()
        if not msg:
            return
        
        # Отправляем через WebSocket
        ws_msg = json.dumps({
            "type": "message",
            "recipient_id": self.current_recipient,
            "content": msg
        })
        self.ws.send(ws_msg)
        self.message_entry.delete(0, tk.END)
        
        # Отображаем своё сообщение
        self.chat_area.config(state='normal')
        self.chat_area.insert(tk.END, f"[{datetime.now().strftime('%H:%M')}] Я: {msg}\n")
        self.chat_area.config(state='disabled')
        self.chat_area.see(tk.END)
    
    def update_user_status(self, user_id, is_online):
        # Можно обновить цвет в списке (автоматически обновляется)
        pass
    
    def logout(self):
        if self.ws:
            self.ws.close()
        self.user_id = None
        self.username = None
        self.show_auth_screen()
    
    def clear_window(self):
        for widget in self.window.winfo_children():
            widget.destroy()
    
    def on_closing(self):
        if self.ws:
            self.ws.close()
        self.window.destroy()
    
    def run(self):
        self.window.mainloop()

if __name__ == "__main__":
    client = MessengerClient()
    client.run()
