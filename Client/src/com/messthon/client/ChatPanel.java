package com.messthon.client;

import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.scene.text.Text;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class ChatPanel extends VBox {
    private VBox messageArea;
    private TextField inputField;
    private Client client;
    private String myName;
    private String friendName;
    private ScrollPane scrollPane;
    
    public ChatPanel(Client client, String myName, String friendName) {
        this.client = client;
        this.myName = myName;
        this.friendName = friendName;
        
        setSpacing(10);
        setPadding(new Insets(10));
        setStyle("-fx-background-color: #1e1e1e;");
        
        messageArea = new VBox(10);
        messageArea.setPadding(new Insets(10));
        
        scrollPane = new ScrollPane(messageArea);
        scrollPane.setFitToWidth(true);
        scrollPane.setStyle("-fx-background: #1e1e1e; -fx-background-color: transparent;");
        VBox.setVgrow(scrollPane, Priority.ALWAYS);
        
        HBox inputBox = new HBox(10);
        inputBox.setAlignment(Pos.CENTER);
        inputBox.setPadding(new Insets(10, 0, 0, 0));
        
        inputField = new TextField();
        inputField.setPromptText("Type a message...");
        inputField.setStyle("-fx-background-color: #2b2b2b; -fx-text-fill: white;");
        HBox.setHgrow(inputField, Priority.ALWAYS);
        
        Button sendBtn = new Button("Send");
        sendBtn.setStyle("-fx-background-color: #00ff88; -fx-text-fill: black;");
        sendBtn.setOnAction(e -> sendMessage());
        
        inputField.setOnAction(e -> sendMessage());
        
        inputBox.getChildren().addAll(inputField, sendBtn);
        
        getChildren().addAll(scrollPane, inputBox);
    }
    
    private void sendMessage() {
        String text = inputField.getText().trim();
        if (!text.isEmpty()) {
            client.sendMessage(friendName, text);
            addMessage(myName, text, LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME));
            inputField.clear();
        }
    }
    
    public void addMessage(String sender, String text, String timestamp) {
        HBox messageBox = new HBox();
        messageBox.setAlignment(sender.equals(myName) ? Pos.CENTER_RIGHT : Pos.CENTER_LEFT);
        messageBox.setPadding(new Insets(5));
        
        VBox messageContent = new VBox(2);
        messageContent.setMaxWidth(400);
        messageContent.setStyle(sender.equals(myName) ? 
            "-fx-background-color: #00ff88; -fx-background-radius: 10; -fx-padding: 8 12 8 12;" : 
            "-fx-background-color: #2b2b2b; -fx-background-radius: 10; -fx-padding: 8 12 8 12;");
        
        Label senderLabel = new Label(sender);
        senderLabel.setStyle(sender.equals(myName) ? 
            "-fx-text-fill: black; -fx-font-size: 10;" : 
            "-fx-text-fill: #00ff88; -fx-font-size: 10;");
        
        Text messageText = new Text(text);
        messageText.setStyle(sender.equals(myName) ? 
            "-fx-fill: black;" : 
            "-fx-fill: white;");
        
        Label timeLabel = new Label(formatTime(timestamp));
        timeLabel.setStyle(sender.equals(myName) ? 
            "-fx-text-fill: #333333; -fx-font-size: 8;" : 
            "-fx-text-fill: #888888; -fx-font-size: 8;");
        
        messageContent.getChildren().addAll(senderLabel, messageText, timeLabel);
        messageBox.getChildren().add(messageContent);
        
        Platform.runLater(() -> {
            messageArea.getChildren().add(messageBox);
            scrollPane.setVvalue(1.0);
        });
    }
    
    private String formatTime(String timestamp) {
        try {
            LocalDateTime time = LocalDateTime.parse(timestamp);
            return time.format(DateTimeFormatter.ofPattern("HH:mm"));
        } catch (Exception e) {
            return "";
        }
    }
}
