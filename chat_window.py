#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import json
from datetime import datetime
from PySide6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, 
                               QListWidget, QListWidgetItem, QTextEdit,
                               QPushButton, QLabel, QSplitter, QFrame,
                               QDialog, QLineEdit, QMessageBox, QScrollArea)
from PySide6.QtCore import Qt, Signal, QTimer, QDateTime
from PySide6.QtGui import QFont

class MessageBubble(QFrame):
    """Виджет для отображения сообщения в виде пузырька"""
    
    def __init__(self, sender, message, timestamp, is_me=False):
        super().__init__()
        self.setup_ui(sender, message, timestamp, is_me)
    
    def setup_ui(self, sender, message, timestamp, is_me):
        layout = QVBoxLayout()
        layout.setContentsMargins(10, 5, 10, 5)
        
        # Основной контейнер пузырька
        bubble = QFrame()
        if is_me:
            bubble.setStyleSheet("""
                QFrame {
                    background-color: #00ff88;
                    border-radius: 15px;
                    border-bottom-right-radius: 5px;
                    margin-left: 50px;
                }
            """)
            bubble_layout = QVBoxLayout(bubble)
            bubble_layout.setContentsMargins(10, 5, 10, 5)
            
            # Имя отправителя
            name_label = QLabel("Вы")
            name_label.setFont(QFont("DejaVu Sans", 12, QFont.Weight.Bold))
            name_label.setStyleSheet("color: #000000;")
            bubble_layout.addWidget(name_label)
            
            # Текст сообщения
            msg_label = QLabel(message)
            msg_label.setFont(QFont("DejaVu Sans", 14))
            msg_label.setStyleSheet("color: #000000;")
            msg_label.setWordWrap(True)
            bubble_layout.addWidget(msg_label)
            
            # Время
            time_label = QLabel(timestamp)
            time_label.setFont(QFont("DejaVu Sans", 10))
            time_label.setStyleSheet("color: #333333;")
            time_label.setAlignment(Qt.AlignmentFlag.AlignRight)
            bubble_layout.addWidget(time_label)
            
        else:
            bubble.setStyleSheet("""
                QFrame {
                    background-color: #2b2b2b;
                    border-radius: 15px;
                    border-bottom-left-radius: 5px;
                    margin-right: 50px;
                }
            """)
            bubble_layout = QVBoxLayout(bubble)
            bubble_layout.setContentsMargins(10, 5, 10, 5)
            
            # Имя отправителя
            name_label = QLabel(sender)
            name_label.setFont(QFont("DejaVu Sans", 12, QFont.Weight.Bold))
            name_label.setStyleSheet("color: #00ff88;")
            bubble_layout.addWidget(name_label)
            
            # Текст сообщения
            msg_label = QLabel(message)
            msg_label.setFont(QFont("DejaVu Sans", 14))
            msg_label.setStyleSheet("color: #ffffff;")
            msg_label.setWordWrap(True)
            bubble_layout.addWidget(msg_label)
            
            # Время
            time_label = QLabel(timestamp)
            time_label.setFont(QFont("DejaVu Sans", 10))
            time_label.setStyleSheet("color: #888888;")
            time_label.setAlignment(Qt.AlignmentFlag.AlignRight)
            bubble_layout.addWidget(time_label)
        
        layout.addWidget(bubble)
        
        if is_me:
            layout.setAlignment(Qt.AlignmentFlag.AlignRight)
        else:
            layout.setAlignment(Qt.AlignmentFlag.AlignLeft)
        
        self.setLayout(layout)
        self.setContentsMargins(0, 0, 0, 0)
        self.setAttribute(Qt.WidgetAttribute.WA_StyledBackground, True)


class AddContactDialog(QDialog):
    """Диалог добавления контакта"""
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setWindowTitle("Добавить друга")
        self.setFixedSize(350, 200)
        self.setStyleSheet("""
            QDialog {
                background-color: #2b2b2b;
            }
            QLabel {
                color: white;
                font-size: 14px;
            }
            QLineEdit {
                background-color: #3b3b3b;
                border: 2px solid #4a4a4a;
                border-radius: 5px;
                padding: 8px;
                color: white;
                font-size: 14px;
            }
            QPushButton {
                background-color: #00ff88;
                border: none;
                border-radius: 5px;
                padding: 10px;
                color: black;
                font-weight: bold;
            }
            QPushButton:hover {
                background-color: #00dd77;
            }
        """)
        
        layout = QVBoxLayout()
        layout.setContentsMargins(20, 20, 20, 20)
        layout.setSpacing(15)
        
        # Инструкция
        label = QLabel("Введите код друга:")
        label.setFont(QFont("DejaVu Sans", 14))
        label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        layout.addWidget(label)
        
        # Поле ввода кода
        self.code_input = QLineEdit()
        self.code_input.setPlaceholderText("XXXX-XXXX-XXXX")
        self.code_input.setFont(QFont("DejaVu Sans", 14))
        layout.addWidget(self.code_input)
        
        # Кнопки
        button_layout = QHBoxLayout()
        
        add_btn = QPushButton("Добавить")
        add_btn.setFont(QFont("DejaVu Sans", 12, QFont.Weight.Bold))
        add_btn.clicked.connect(self.accept)
        button_layout.addWidget(add_btn)
        
        cancel_btn = QPushButton("Отмена")
        cancel_btn.setFont(QFont("DejaVu Sans", 12))
        cancel_btn.clicked.connect(self.reject)
        cancel_btn.setStyleSheet("background-color: #4a4a4a; color: white;")
        button_layout.addWidget(cancel_btn)
        
        layout.addLayout(button_layout)
        
        self.setLayout(layout)
    
    def get_code(self):
        return self.code_input.text().strip()


class ChatWindow(QWidget):
    """Главное окно чата"""
    
    def __init__(self, crypto_manager, username, user_code, socket):
        super().__init__()
        self.crypto = crypto_manager
        self.username = username
        self.user_code = user_code
        self.socket = socket
        self.contacts = []
        self.current_chat = None
        self.messages = {}
        
        self.init_ui()
        self.load_saved_chats()
        self.request_contacts()
    
    def init_ui(self):
        self.setWindowTitle(f"Messthon - {self.username}")
        self.setMinimumSize(900, 600)
        
        # Устанавливаем шрифт по умолчанию
        self.setFont(QFont("DejaVu Sans", 10))
        
        # Основной стиль
        self.setStyleSheet("""
            QWidget {
                background-color: #1e1e1e;
            }
            QListWidget {
                background-color: #2b2b2b;
                border: none;
                border-radius: 10px;
                padding: 5px;
                color: white;
                font-size: 14px;
            }
            QListWidget::item {
                padding: 10px;
                border-radius: 5px;
            }
            QListWidget::item:selected {
                background-color: #00ff88;
                color: black;
            }
            QListWidget::item:hover {
                background-color: #3b3b3b;
            }
            QTextEdit {
                background-color: #2b2b2b;
                border: 2px solid #3b3b3b;
                border-radius: 10px;
                padding: 10px;
                color: white;
                font-size: 14px;
            }
            QTextEdit:focus {
                border: 2px solid #00ff88;
            }
            QPushButton {
                background-color: #00ff88;
                border: none;
                border-radius: 8px;
                padding: 10px 20px;
                color: black;
                font-size: 14px;
                font-weight: bold;
            }
            QPushButton:hover {
                background-color: #00dd77;
            }
            QPushButton:disabled {
                background-color: #4a4a4a;
                color: #888888;
            }
            QLabel {
                color: white;
            }
        """)
        
        # Главный сплиттер
        splitter = QSplitter(Qt.Orientation.Horizontal)
        
        # Левая панель - контакты
        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        left_layout.setContentsMargins(10, 10, 10, 10)
        
        # Заголовок контактов
        contacts_header = QLabel("Друзья")
        contacts_header.setFont(QFont("DejaVu Sans", 16, QFont.Weight.Bold))
        contacts_header.setStyleSheet("color: #00ff88; padding: 5px;")
        left_layout.addWidget(contacts_header)
        
        # Кнопка добавления контакта
        add_contact_btn = QPushButton("+ Добавить друга")
        add_contact_btn.setFont(QFont("DejaVu Sans", 12, QFont.Weight.Bold))
        add_contact_btn.clicked.connect(self.show_add_contact_dialog)
        left_layout.addWidget(add_contact_btn)
        
        # Список контактов
        self.contacts_list = QListWidget()
        self.contacts_list.setFont(QFont("DejaVu Sans", 12))
        self.contacts_list.itemClicked.connect(self.on_contact_selected)
        left_layout.addWidget(self.contacts_list)
        
        # Информация о пользователе
        user_info = QLabel(f"Ваш код:\n{self.user_code}")
        user_info.setFont(QFont("DejaVu Sans", 12))
        user_info.setStyleSheet("""
            background-color: #2b2b2b;
            border-radius: 5px;
            padding: 10px;
            color: #888888;
        """)
        user_info.setWordWrap(True)
        left_layout.addWidget(user_info)
        
        # Правая панель - чат
        right_panel = QWidget()
        self.chat_layout = QVBoxLayout(right_panel)
        self.chat_layout.setContentsMargins(10, 10, 10, 10)
        
        # Заголовок чата
        self.chat_header = QLabel("Выберите друга для общения")
        self.chat_header.setFont(QFont("DejaVu Sans", 14))
        self.chat_header.setStyleSheet("color: #888888; padding: 5px;")
        self.chat_layout.addWidget(self.chat_header)
        
        # Область сообщений
        self.messages_area = QWidget()
        self.messages_layout = QVBoxLayout(self.messages_area)
        self.messages_layout.setAlignment(Qt.AlignmentFlag.AlignTop)
        self.messages_layout.setSpacing(5)
        
        # Scroll area для сообщений
        self.scroll_area = QScrollArea()
        self.scroll_area.setWidget(self.messages_area)
        self.scroll_area.setWidgetResizable(True)
        self.scroll_area.setStyleSheet("""
            QScrollArea {
                border: none;
                background-color: transparent;
            }
        """)
        self.chat_layout.addWidget(self.scroll_area)
        
        # Область ввода сообщения
        input_layout = QHBoxLayout()
        
        self.message_input = QTextEdit()
        self.message_input.setPlaceholderText("Напишите сообщение...")
        self.message_input.setMaximumHeight(100)
        self.message_input.setFont(QFont("DejaVu Sans", 14))
        input_layout.addWidget(self.message_input)
        
        send_btn = QPushButton("→")
        send_btn.setMaximumWidth(50)
        send_btn.setFont(QFont("DejaVu Sans", 16, QFont.Weight.Bold))
        send_btn.clicked.connect(self.send_message)
        input_layout.addWidget(send_btn)
        
        self.chat_layout.addLayout(input_layout)
        
        # Добавляем панели в сплиттер
        splitter.addWidget(left_panel)
        splitter.addWidget(right_panel)
        splitter.setSizes([250, 650])
        
        # Главный layout
        main_layout = QVBoxLayout()
        main_layout.addWidget(splitter)
        self.setLayout(main_layout)
        
        # Таймер для обновления статусов
        self.status_timer = QTimer()
        self.status_timer.timeout.connect(self.update_contacts_status)
        self.status_timer.start(30000)
    
    def show_add_contact_dialog(self):
        """Показывает диалог добавления контакта"""
        dialog = AddContactDialog(self)
        if dialog.exec() == QDialog.DialogCode.Accepted:
            code = dialog.get_code()
            if code:
                self.add_contact(code)
    
    def add_contact(self, code):
        """Добавляет контакт по коду"""
        msg = {
            'type': 'add_contact',
            'code': code
        }
        try:
            self.socket.send(json.dumps(msg).encode('utf-8'))
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось отправить запрос: {e}")
    
    def request_contacts(self):
        """Запрашивает список контактов"""
        msg = {'type': 'get_contacts'}
        try:
            self.socket.send(json.dumps(msg).encode('utf-8'))
        except:
            pass
    
    def update_contacts_list(self, contacts):
        """Обновляет список контактов"""
        self.contacts = contacts
        self.contacts_list.clear()
        
        for contact in contacts:
            status = "🟢" if contact.get('online', False) else "⚪"
            item_text = f"{status} {contact['name']}\n{contact['code']}"
            item = QListWidgetItem(item_text)
            item.setFont(QFont("DejaVu Sans", 12))
            item.setData(Qt.ItemDataRole.UserRole, contact)
            self.contacts_list.addItem(item)
    
    def on_contact_selected(self, item):
        """Обрабатывает выбор контакта"""
        contact = item.data(Qt.ItemDataRole.UserRole)
        self.current_chat = contact['name']
        
        # Обновляем заголовок
        status = "🟢 В сети" if contact.get('online', False) else "⚪ Не в сети"
        self.chat_header.setText(f"{contact['name']} - {status}")
        
        # Очищаем область сообщений
        self.clear_messages_area()
        
        # Загружаем историю сообщений
        self.load_chat_history(contact['code'])
    
    def clear_messages_area(self):
        """Очищает область сообщений"""
        while self.messages_layout.count():
            child = self.messages_layout.takeAt(0)
            if child.widget():
                child.widget().deleteLater()
    
    def load_chat_history(self, friend_code):
        """Загружает историю чата"""
        history = self.crypto.load_chat_history(friend_code)
        if history:
            for msg in history:
                self.display_message(
                    msg['sender'],
                    msg['content'],
                    msg['timestamp'],
                    msg['sender'] == self.username
                )
    
    def display_message(self, sender, content, timestamp, is_me=False):
        """Отображает сообщение в чате"""
        # Форматируем время
        try:
            dt = QDateTime.fromString(timestamp, Qt.ISODate)
            time_str = dt.toString("hh:mm")
        except:
            time_str = timestamp
        
        bubble = MessageBubble(sender, content, time_str, is_me)
        self.messages_layout.addWidget(bubble)
        
        # Прокручиваем вниз
        QTimer.singleShot(100, self.scroll_to_bottom)
    
    def scroll_to_bottom(self):
        """Прокручивает область сообщений вниз"""
        scrollbar = self.scroll_area.verticalScrollBar()
        scrollbar.setValue(scrollbar.maximum())
    
    def send_message(self):
        """Отправляет сообщение"""
        if not self.current_chat:
            QMessageBox.information(self, "Информация", "Выберите друга для общения")
            return
        
        text = self.message_input.toPlainText().strip()
        if not text:
            return
        
        # Очищаем поле ввода
        self.message_input.clear()
        
        # Отправляем на сервер
        timestamp = QDateTime.currentDateTime().toString(Qt.ISODate)
        msg = {
            'type': 'message',
            'recipient': self.current_chat,
            'content': text,
            'timestamp': timestamp
        }
        
        try:
            self.socket.send(json.dumps(msg, ensure_ascii=False).encode('utf-8'))
            
            # Отображаем свое сообщение
            self.display_message(self.username, text, timestamp, True)
            
            # Сохраняем в историю
            self.save_message_to_history(self.current_chat, {
                'sender': self.username,
                'content': text,
                'timestamp': timestamp
            })
            
        except Exception as e:
            QMessageBox.critical(self, "Ошибка", f"Не удалось отправить сообщение: {e}")
    
    def save_message_to_history(self, friend_name, message):
        """Сохраняет сообщение в историю"""
        # Находим код друга
        friend_code = None
        for contact in self.contacts:
            if contact['name'] == friend_name:
                friend_code = contact['code']
                break
        
        if friend_code:
            history = self.crypto.load_chat_history(friend_code)
            history.append(message)
            self.crypto.save_chat_history(friend_code, history)
    
    def load_saved_chats(self):
        """Загружает сохраненные чаты"""
        pass
    
    def update_contacts_status(self):
        """Обновляет статусы контактов"""
        pass
    
    def handle_incoming_message(self, sender, content, timestamp):
        """Обрабатывает входящее сообщение"""
        # Отображаем сообщение
        self.display_message(sender, content, timestamp, False)
        
        # Сохраняем в историю
        self.save_message_to_history(sender, {
            'sender': sender,
            'content': content,
            'timestamp': timestamp
        })
