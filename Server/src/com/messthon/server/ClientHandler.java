package com.messthon.server;

import java.io.*;
import java.net.*;
import com.messthon.server.models.Message;

public class ClientHandler implements Runnable {
    private Socket socket;
    private ObjectInputStream in;
    private ObjectOutputStream out;
    private String username;
    private String userCode;
    
    public ClientHandler(Socket socket) {
        this.socket = socket;
        try {
            out = new ObjectOutputStream(socket.getOutputStream());
            in = new ObjectInputStream(socket.getInputStream());
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    @Override
    public void run() {
        try {
            while (true) {
                Message message = (Message) in.readObject();
                handleMessage(message);
            }
        } catch (IOException | ClassNotFoundException e) {
            if (username != null) {
                Server.userDisconnected(username);
            }
        } finally {
            try {
                socket.close();
            } catch (IOException e) {}
        }
    }
    
    private void handleMessage(Message message) {
        switch (message.getType()) {
            case "LOGIN":
                handleLogin(message);
                break;
            case "ADD_CONTACT":
                handleAddContact(message);
                break;
            case "GET_CONTACTS":
                handleGetContacts(message);
                break;
            case "MESSAGE":
                handleTextMessage(message);
                break;
        }
    }
    
    private void handleLogin(Message message) {
        String username = message.getSender();
        String code = message.getContent();
        
        String storedCode = Server.getUserCode(username);
        if (storedCode == null) {
            // New user
            storedCode = Server.registerUser(username);
            this.username = username;
            this.userCode = storedCode;
            Server.userConnected(username, this);
            sendMessage(new Message("SERVER", username, "LOGIN_SUCCESS", storedCode));
        } else {
            // Existing user
            if (storedCode.equals(code)) {
                this.username = username;
                this.userCode = code;
                Server.userConnected(username, this);
                sendMessage(new Message("SERVER", username, "LOGIN_SUCCESS", code));
            } else {
                sendMessage(new Message("SERVER", username, "LOGIN_FAILED", "Invalid code"));
            }
        }
    }
    
    private void handleAddContact(Message message) {
        String username = message.getSender();
        String friendCode = message.getContent();
        
        if (Server.addContact(username, friendCode)) {
            sendMessage(new Message("SERVER", username, "CONTACT_ADDED", friendCode));
        } else {
            sendMessage(new Message("SERVER", username, "CONTACT_FAILED", "Invalid code"));
        }
    }
    
    private void handleGetContacts(Message message) {
        String username = message.getSender();
        java.util.List<String> contacts = Server.getContacts(username);
        StringBuilder contactsStr = new StringBuilder();
        for (String code : contacts) {
            String friendName = Server.getUsernameByCode(code);
            if (friendName != null) {
                contactsStr.append(code).append(":").append(friendName).append(";");
            }
        }
        sendMessage(new Message("SERVER", username, "CONTACTS_LIST", contactsStr.toString()));
    }
    
    private void handleTextMessage(Message message) {
        String recipient = message.getRecipient();
        ClientHandler recipientHandler = Server.getOnlineUser(recipient);
        
        if (recipientHandler != null) {
            recipientHandler.sendMessage(message);
            sendMessage(new Message("SERVER", message.getSender(), "DELIVERED", "Message delivered"));
        } else {
            sendMessage(new Message("SERVER", message.getSender(), "USER_OFFLINE", 
                "User " + recipient + " is offline"));
        }
    }
    
    private void sendMessage(Message message) {
        try {
            out.writeObject(message);
            out.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
