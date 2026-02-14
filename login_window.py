#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from PySide6.QtWidgets import (QWidget, QVBoxLayout, QHBoxLayout, 
                               QLabel, QLineEdit, QPushButton, 
                               QMessageBox, QFrame)
from PySide6.QtCore import Qt, Signal
from PySide6.QtGui import QFont, QPalette, QColor, QLinearGradient, QFontDatabase
from datetime import datetime

class LoginWindow(QWidget):
    login_successful = Signal(str, str, str)  # username, code, server
    
    def __init__(self, crypto_manager):
        super().__init__()
        self.crypto = crypto_manager
        self.init_ui()
        
        # Пытаемся загрузить сохраненные данные
        self.load_saved_data()
    
    def init_ui(self):
        self.setWindowTitle("Messthon - Вход")
        self.setFixedSize(450, 650)
        
        # Устанавливаем поддерживаемый шрифт
        font = QFont("DejaVu Sans", 10)  # Шрифт с хорошей поддержкой Unicode
        self.setFont(font)
        
        # Градиентный фон
        self.setAutoFillBackground(True)
        palette = self.palette()
        gradient = QLinearGradient(0, 0, 0, self.height())
        gradient.setColorAt(0.0, QColor(30, 30, 30))
        gradient.setColorAt(1.0, QColor(18, 18, 18))
        palette.setBrush(QPalette.Window, gradient)
        self.setPalette(palette)
        
        # Главный контейнер
        main_layout = QVBoxLayout()
        main_layout.setSpacing(20)
        main_layout.setContentsMargins(40, 40, 40, 40)
        
        # Логотип/Заголовок
        title_label = QLabel("Messthon")
        title_font = QFont("DejaVu Sans", 36, QFont.Weight.Bold)
        title_label.setFont(title_font)
        title_label.setStyleSheet("color: #00ff88;")
        title_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        main_layout.addWidget(title_label)
        
        # Подзаголовок
        subtitle = QLabel("Secure Messenger")
        subtitle_font = QFont("DejaVu Sans", 12)
        subtitle.setFont(subtitle_font)
        subtitle.setStyleSheet("color: #888888;")
        subtitle.setAlignment(Qt.AlignmentFlag.AlignCenter)
        main_layout.addWidget(subtitle)
        
        main_layout.addSpacing(30)
        
        # Карточка входа
        login_card = QFrame()
        login_card.setStyleSheet("""
            QFrame {
                background-color: rgba(43, 43, 43, 200);
                border-radius: 15px;
                padding: 30px;
            }
            QLabel {
                color: #ffffff;
                font-size: 14px;
            }
            QLineEdit {
                background-color: #3b3b3b;
                border: 2px solid #4a4a4a;
                border-radius: 8px;
                padding: 12px;
                color: white;
                font-size: 14px;
                selection-background-color: #00ff88;
            }
            QLineEdit:focus {
                border: 2px solid #00ff88;
            }
            QPushButton {
                background-color: #00ff88;
                border: none;
                border-radius: 8px;
                padding: 15px;
                color: black;
                font-size: 16px;
                font-weight: bold;
            }
            QPushButton:hover {
                background-color: #00dd77;
            }
            QPushButton:pressed {
                background-color: #00bb66;
            }
            QPushButton:disabled {
                background-color: #4a4a4a;
                color: #888888;
            }
        """)
        
        card_layout = QVBoxLayout(login_card)
        card_layout.setSpacing(15)
        
        # Поле username
        username_label = QLabel("Имя пользователя")
        username_label.setFont(QFont("DejaVu Sans", 14))
        card_layout.addWidget(username_label)
        
        self.username_input = QLineEdit()
        self.username_input.setPlaceholderText("Введите ваш никнейм")
        self.username_input.setFont(QFont("DejaVu Sans", 14))
        self.username_input.textChanged.connect(self.validate_inputs)
        card_layout.addWidget(self.username_input)
        
        # Поле кода
        code_label = QLabel("Код доступа")
        code_label.setFont(QFont("DejaVu Sans", 14))
        card_layout.addWidget(code_label)
        
        self.code_input = QLineEdit()
        self.code_input.setPlaceholderText("XXXX-XXXX-XXXX (оставьте пустым для нового аккаунта)")
        self.code_input.setFont(QFont("DejaVu Sans", 14))
        self.code_input.textChanged.connect(self.validate_inputs)
        card_layout.addWidget(self.code_input)
        
        card_layout.addSpacing(10)
        
        # Поле адреса сервера
        server_label = QLabel("Адрес сервера")
        server_label.setFont(QFont("DejaVu Sans", 14))
        card_layout.addWidget(server_label)
        
        self.server_input = QLineEdit()
        self.server_input.setPlaceholderText("localhost (или IP сервера)")
        self.server_input.setText("localhost")
        self.server_input.setFont(QFont("DejaVu Sans", 14))
        card_layout.addWidget(self.server_input)
        
        # Подсказка для локального сервера
        hint_label = QLabel("💡 Для локального сервера оставьте localhost")
        hint_label.setFont(QFont("DejaVu Sans", 10))
        hint_label.setStyleSheet("color: #888888;")
        card_layout.addWidget(hint_label)
        
        card_layout.addSpacing(20)
        
        # Кнопка входа
        self.login_button = QPushButton("Войти в Messthon")
        self.login_button.setFont(QFont("DejaVu Sans", 16, QFont.Weight.Bold))
        self.login_button.clicked.connect(self.handle_login)
        self.login_button.setEnabled(False)
        card_layout.addWidget(self.login_button)
        
        # Подсказка
        hint_label2 = QLabel("💡 Новый пользователь? Просто введите имя и нажмите Войти")
        hint_label2.setFont(QFont("DejaVu Sans", 11))
        hint_label2.setStyleSheet("color: #888888;")
        hint_label2.setWordWrap(True)
        hint_label2.setAlignment(Qt.AlignmentFlag.AlignCenter)
        card_layout.addWidget(hint_label2)
        
        main_layout.addWidget(login_card)
        main_layout.addStretch()
        
        self.setLayout(main_layout)
    
    def validate_inputs(self):
        """Проверяет заполнение полей"""
        username = self.username_input.text().strip()
        self.login_button.setEnabled(len(username) > 0)
    
    def load_saved_data(self):
        """Загружает сохраненные данные пользователя"""
        config = self.crypto.load_config()
        if config:
            self.username_input.setText(config.get('username', ''))
            self.code_input.setText(config.get('code', ''))
    
    def handle_login(self):
        """Обработка входа"""
        username = self.username_input.text().strip()
        code = self.code_input.text().strip()
        server = self.server_input.text().strip() or "localhost"
        
        if not username:
            QMessageBox.warning(self, "Ошибка", "Введите имя пользователя")
            return
        
        self.login_button.setEnabled(False)
        self.login_button.setText("Подключение...")
        
        # Здесь будет подключение к серверу
        self.login_successful.emit(username, code, server)
    
    def login_failed(self, message):
        """Обработка неудачного входа"""
        self.login_button.setEnabled(True)
        self.login_button.setText("Войти в Messthon")
        QMessageBox.critical(self, "Ошибка входа", message)
    
    def login_completed(self, username, code):
        """Успешный вход - сохраняем данные"""
        config = {
            'username': username,
            'code': code,
            'last_login': str(datetime.now())
        }
        self.crypto.save_config(config)
