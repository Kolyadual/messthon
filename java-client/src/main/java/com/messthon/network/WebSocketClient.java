package com.messthon.network;

import org.java_websocket.client.WebSocketClient;
import org.java_websocket.handshake.ServerHandshake;
import java.net.URI;
import java.util.function.Consumer;

public class WebSocketClientImpl extends WebSocketClient {
    private Consumer<String> messageHandler;
    
    public WebSocketClientImpl(URI serverUri) {
        super(serverUri);
    }
    
    public void setMessageHandler(Consumer<String> handler) {
        this.messageHandler = handler;
    }
    
    @Override
    public void onOpen(ServerHandshake handshake) {
        System.out.println("WebSocket connection opened");
    }
    
    @Override
    public void onMessage(String message) {
        if (messageHandler != null) {
            messageHandler.accept(message);
        }
    }
    
    @Override
    public void onClose(int code, String reason, boolean remote) {
        System.out.println("WebSocket connection closed: " + reason);
    }
    
    @Override
    public void onError(Exception ex) {
        System.err.println("WebSocket error: " + ex.getMessage());
    }
}
