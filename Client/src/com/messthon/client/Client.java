package com.messthon.client;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.function.Consumer;

public class Client {
    private Socket socket;
    private ObjectOutputStream out;
    private ObjectInputStream in;
    private String serverAddress;
    private int port;
    private String username;
    private String userCode;
    private boolean connected;
    private String lastError;
    private Consumer<Message> messageHandler;
    private Thread listenerThread;
    private List<String> contactsCache = new ArrayList<>();
    
    public Client(String serverAddress, int port, Consumer<Message> messageHandler) {
        this.serverAddress = serverAddress;
        this.port = port;
        this.messageHandler = messageHandler;
    }
    
    public boolean connect(String username, String code) {
        try {
            socket = new Socket(serverAddress, port);
            out = new ObjectOutputStream(socket.getOutputStream());
            in = new ObjectInputStream(socket.getInputStream());
            
            this.username = username;
            
            sendMessage(new Message(username, "SERVER", "LOGIN", code));
            
            Message response = (Message) in.readObject();
            
            if (response.getType().equals("LOGIN_SUCCESS")) {
                this.userCode = response.getContent();
                this.connected = true;
                
                listenerThread = new Thread(this::listenForMessages);
                listenerThread.start();
                
                requestContacts();
                
                return true;
            } else {
                lastError = response.getContent();
                close();
                return false;
            }
            
        } catch (Exception e) {
            lastError = e.getMessage();
            close();
            return false;
        }
    }
    
    private void listenForMessages() {
        try {
            while (connected) {
                Message message = (Message) in.readObject();
                if (messageHandler != null) {
                    messageHandler.accept(message);
                }
            }
        } catch (IOException | ClassNotFoundException e) {
            connected = false;
        }
    }
    
    public void sendMessage(Message message) {
        try {
            out.writeObject(message);
            out.flush();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    public void sendMessage(String recipient, String content) {
        sendMessage(new Message(username, recipient, "MESSAGE", content));
    }
    
    public void addContact(String friendCode) {
        sendMessage(new Message(username, "SERVER", "ADD_CONTACT", friendCode));
    }
    
    public void requestContacts() {
        sendMessage(new Message(username, "SERVER", "GET_CONTACTS", ""));
    }
    
    public List<String> getContacts() {
        return contactsCache;
    }
    
    public void updateContacts(List<String> contacts) {
        this.contactsCache = contacts;
    }
    
    public void close() {
        connected = false;
        try {
            if (socket != null) socket.close();
        } catch (IOException e) {}
    }
    
    public String getUserCode() { return userCode; }
    public String getLastError() { return lastError; }
    public boolean isConnected() { return connected; }
}
