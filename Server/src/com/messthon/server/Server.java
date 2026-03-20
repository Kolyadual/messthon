package com.messthon.server;

import java.io.*;
import java.net.*;
import java.util.*;
import java.util.concurrent.*;

public class Server {
    private static final int PORT = 54321;
    private static Map<String, ClientHandler> onlineUsers = new ConcurrentHashMap<>();
    private static Map<String, String> userCodes = new ConcurrentHashMap<>();
    private static Map<String, List<String>> userContacts = new ConcurrentHashMap<>();
    private static final String DATA_DIR = "messthon_data/";
    
    public static void main(String[] args) {
        System.out.println("╔════════════════════════════════════╗");
        System.out.println("║       Messthon Server v1.0        ║");
        System.out.println("║      Running on Debian 13         ║");
        System.out.println("╚════════════════════════════════════╝");
        
        new File(DATA_DIR).mkdirs();
        loadUserData();
        
        try (ServerSocket serverSocket = new ServerSocket(PORT)) {
            System.out.println("✓ Server started on port " + PORT);
            System.out.println("✓ Waiting for connections...\n");
            
            while (true) {
                Socket clientSocket = serverSocket.accept();
                ClientHandler clientHandler = new ClientHandler(clientSocket);
                new Thread(clientHandler).start();
            }
        } catch (IOException e) {
            System.err.println("✗ Server error: " + e.getMessage());
        }
    }
    
    private static void loadUserData() {
        File usersFile = new File(DATA_DIR + "users.dat");
        if (usersFile.exists()) {
            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(usersFile))) {
                userCodes = (Map<String, String>) ois.readObject();
                userContacts = (Map<String, List<String>>) ois.readObject();
                System.out.println("✓ Loaded " + userCodes.size() + " users");
            } catch (Exception e) {
                System.err.println("✗ Failed to load user data: " + e.getMessage());
            }
        }
    }
    
    private static void saveUserData() {
        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(DATA_DIR + "users.dat"))) {
            oos.writeObject(userCodes);
            oos.writeObject(userContacts);
        } catch (IOException e) {
            System.err.println("✗ Failed to save user data: " + e.getMessage());
        }
    }
    
    public static synchronized String registerUser(String username) {
        if (userCodes.containsKey(username)) {
            return null;
        }
        String code = generateUniqueCode();
        userCodes.put(username, code);
        userContacts.put(username, new ArrayList<>());
        saveUserData();
        return code;
    }
    
    public static synchronized boolean addContact(String username, String friendCode) {
        List<String> contacts = userContacts.get(username);
        if (contacts != null && !contacts.contains(friendCode)) {
            for (Map.Entry<String, String> entry : userCodes.entrySet()) {
                if (entry.getValue().equals(friendCode) && !entry.getKey().equals(username)) {
                    contacts.add(friendCode);
                    saveUserData();
                    return true;
                }
            }
        }
        return false;
    }
    
    public static String getUsernameByCode(String code) {
        for (Map.Entry<String, String> entry : userCodes.entrySet()) {
            if (entry.getValue().equals(code)) {
                return entry.getKey();
            }
        }
        return null;
    }
    
    public static String getUserCode(String username) {
        return userCodes.get(username);
    }
    
    public static List<String> getContacts(String username) {
        return userContacts.getOrDefault(username, new ArrayList<>());
    }
    
    public static boolean isUserOnline(String username) {
        return onlineUsers.containsKey(username);
    }
    
    public static void userConnected(String username, ClientHandler handler) {
        onlineUsers.put(username, handler);
        System.out.println("✓ User connected: " + username + " (Online: " + onlineUsers.size() + ")");
    }
    
    public static void userDisconnected(String username) {
        onlineUsers.remove(username);
        System.out.println("✗ User disconnected: " + username + " (Online: " + onlineUsers.size() + ")");
    }
    
    public static ClientHandler getOnlineUser(String username) {
        return onlineUsers.get(username);
    }
    
    private static String generateUniqueCode() {
        String code;
        do {
            code = String.format("%04d-%04d-%04d", 
                (int)(Math.random() * 10000),
                (int)(Math.random() * 10000),
                (int)(Math.random() * 10000));
        } while (userCodes.containsValue(code));
        return code;
    }
}
