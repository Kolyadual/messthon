# server.py
from fastapi import FastAPI, WebSocket, WebSocketDisconnect, Depends, HTTPException, status
from fastapi.middleware.cors import CORSMiddleware
from sqlalchemy.orm import Session
from datetime import datetime
from typing import Dict
import json
import asyncio
import hashlib

from database import SessionLocal, User, Message

app = FastAPI()
app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Зависимость для БД
def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()

# -------------------- Утилиты --------------------
def hash_access_code(code: str) -> str:
    """Хешируем код доступа (чтобы не хранить в открытом виде)"""
    return hashlib.sha256(code.encode()).hexdigest()

def verify_access_code(code: str, hashed: str) -> bool:
    return hash_access_code(code) == hashed

# -------------------- REST API --------------------
@app.post("/register")
async def register(username: str, access_code: str, db: Session = Depends(get_db)):
    """Регистрация: логин + код доступа"""
    # Проверяем, не занят ли логин
    existing = db.query(User).filter(User.username == username).first()
    if existing:
        raise HTTPException(status_code=400, detail="Username already exists")
    
    # Создаём пользователя
    hashed_code = hash_access_code(access_code)
    user = User(username=username, access_code=hashed_code)
    db.add(user)
    db.commit()
    db.refresh(user)
    return {"message": "User created successfully", "user_id": user.id}

@app.post("/login")
async def login(username: str, access_code: str, db: Session = Depends(get_db)):
    """Вход: логин + код доступа"""
    user = db.query(User).filter(User.username == username).first()
    if not user or not verify_access_code(access_code, user.access_code):
        raise HTTPException(status_code=401, detail="Invalid username or access code")
    
    # Возвращаем токен (просто для идентификации, без JWT сложностей)
    return {"user_id": user.id, "username": user.username}

@app.get("/users")
async def get_users(db: Session = Depends(get_db)):
    """Список всех пользователей"""
    users = db.query(User).all()
    return [{"id": u.id, "username": u.username, "is_online": u.is_online} for u in users]

@app.get("/messages/{user_id}")
async def get_messages(user_id: int, current_user_id: int, db: Session = Depends(get_db)):
    """История переписки между current_user_id и user_id"""
    messages = db.query(Message).filter(
        ((Message.sender_id == current_user_id) & (Message.recipient_id == user_id)) |
        ((Message.sender_id == user_id) & (Message.recipient_id == current_user_id))
    ).order_by(Message.timestamp).all()
    
    return [
        {
            "id": m.id,
            "sender_id": m.sender_id,
            "content": m.content,
            "timestamp": m.timestamp.isoformat(),
            "is_read": m.is_read
        } for m in messages
    ]

# -------------------- WebSocket менеджер --------------------
class ConnectionManager:
    def __init__(self):
        self.active_connections: Dict[int, WebSocket] = {}  # user_id -> websocket

    async def connect(self, websocket: WebSocket, user_id: int):
        await websocket.accept()
        self.active_connections[user_id] = websocket
        # Обновляем статус онлайн
        db = SessionLocal()
        db.query(User).filter(User.id == user_id).update({"is_online": True})
        db.commit()
        db.close()
        await self.broadcast_user_status(user_id, True)

    def disconnect(self, user_id: int):
        if user_id in self.active_connections:
            del self.active_connections[user_id]
        # Обновляем статус офлайн
        db = SessionLocal()
        db.query(User).filter(User.id == user_id).update({"is_online": False})
        db.commit()
        db.close()
        asyncio.create_task(self.broadcast_user_status(user_id, False))

    async def send_personal_message(self, message: str, user_id: int):
        if user_id in self.active_connections:
            await self.active_connections[user_id].send_text(message)

    async def broadcast_user_status(self, user_id: int, is_online: bool):
        status_msg = json.dumps({"type": "user_status", "user_id": user_id, "is_online": is_online})
        for conn in self.active_connections.values():
            try:
                await conn.send_text(status_msg)
            except:
                pass

manager = ConnectionManager()

@app.websocket("/ws/{user_id}")
async def websocket_endpoint(websocket: WebSocket, user_id: int):
    """WebSocket соединение для конкретного пользователя"""
    await manager.connect(websocket, user_id)
    try:
        while True:
            data = await websocket.receive_text()
            message_data = json.loads(data)
            
            if message_data["type"] == "message":
                recipient_id = message_data["recipient_id"]
                content = message_data["content"]
                
                # Сохраняем в БД
                db = SessionLocal()
                new_msg = Message(sender_id=user_id, recipient_id=recipient_id, content=content)
                db.add(new_msg)
                db.commit()
                db.refresh(new_msg)
                
                # Отправляем получателю
                await manager.send_personal_message(
                    json.dumps({
                        "type": "message",
                        "id": new_msg.id,
                        "sender_id": user_id,
                        "content": content,
                        "timestamp": new_msg.timestamp.isoformat()
                    }),
                    recipient_id
                )
                db.close()
                
    except WebSocketDisconnect:
        manager.disconnect(user_id)
