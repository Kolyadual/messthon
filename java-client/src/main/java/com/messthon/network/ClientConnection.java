package com.messthon.network;

import java.io.*;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.function.Consumer;

public class ClientConnection {
    private Socket socket;
    private DataInputStream input;
    private DataOutputStream output;
    private Thread receiverThread;
    private Consumer<String> messageHandler;
    private String host;
    private int port;
    private volatile boolean connected = false;
    
    public ClientConnection(String host, int port) {
        this.host = host;
        this.port = port;
    }
    
    public void connect() throws IOException {
        socket = new Socket(host, port);
        socket.setTcpNoDelay(true);
        input = new DataInputStream(socket.getInputStream());
        output = new DataOutputStream(socket.getOutputStream());
        connected = true;
        
        startReceiver();
    }
    
    public void disconnect() {
        connected = false;
        try {
            if (socket != null && !socket.isClosed()) {
                socket.close();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
    
    public boolean isConnected() {
        return connected && socket != null && socket.isConnected() && !socket.isClosed();
    }
    
    public void sendMessage(String message) throws IOException {
        if (!isConnected()) {
            throw new IOException("Not connected to server");
        }
        
        byte[] data = message.getBytes("UTF-8");
        byte[] lengthBytes = ByteBuffer.allocate(4)
                .order(ByteOrder.BIG_ENDIAN)
                .putInt(data.length)
                .array();
        
        synchronized (output) {
            output.write(lengthBytes);
            output.write(data);
            output.flush();
        }
    }
    
    public void setMessageHandler(Consumer<String> handler) {
        this.messageHandler = handler;
    }
    
    private void startReceiver() {
        receiverThread = new Thread(() -> {
            try {
                while (connected && !socket.isClosed()) {
                    // Читаем длину сообщения (4 байта)
                    byte[] lengthBytes = new byte[4];
                    if (input.read(lengthBytes) != 4) {
                        break;
                    }
                    
                    int length = ByteBuffer.wrap(lengthBytes)
                            .order(ByteOrder.BIG_ENDIAN)
                            .getInt();
                    
                    if (length <= 0 || length > 10 * 1024 * 1024) { // 10MB max
                        System.err.println("Invalid message length: " + length);
                        break;
                    }
                    
                    // Читаем сообщение
                    byte[] messageBytes = new byte[length];
                    int totalRead = 0;
                    while (totalRead < length) {
                        int read = input.read(messageBytes, totalRead, length - totalRead);
                        if (read == -1) {
                            break;
                        }
                        totalRead += read;
                    }
                    
                    if (totalRead < length) {
                        break;
                    }
                    
                    String message = new String(messageBytes, "UTF-8");
                    
                    if (messageHandler != null) {
                        messageHandler.accept(message);
                    }
                }
            } catch (Exception e) {
                if (connected) {
                    System.err.println("Connection error: " + e.getMessage());
                }
            } finally {
                connected = false;
                if (messageHandler != null) {
                    messageHandler.accept("{\"type\":\"disconnected\",\"message\":\"Connection lost\"}");
                }
            }
        });
        
        receiverThread.setDaemon(true);
        receiverThread.start();
    }
}
