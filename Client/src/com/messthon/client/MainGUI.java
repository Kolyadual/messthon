package com.messthon.client;

import javafx.application.Application;
import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import javafx.collections.*;
import javafx.scene.text.Font;
import javafx.scene.text.FontWeight;
import java.util.*;

public class MainGUI extends Application {
    private Client client;
    private String username;
    private String userCode;
    private Map<String, ChatPanel> openChats = new HashMap<>();
    private ListView<String> contactsList;
    private ObservableList<String> contacts = FXCollections.observableArrayList();
    private TabPane chatTabs;
    private Label statusLabel;
    
    public static void main(String[] args) {
        launch(args);
    }
    
    @Override
    public void start(Stage primaryStage) {
        primaryStage.setTitle("Messthon Messenger");
        primaryStage.setMinWidth(800);
        primaryStage.setMinHeight(600);
        showLoginWindow(primaryStage);
    }
    
    private void showLoginWindow(Stage primaryStage) {
        VBox loginBox = new VBox(15);
        loginBox.setAlignment(Pos.CENTER);
        loginBox.setPadding(new Insets(40));
        loginBox.setStyle("-fx-background-color: #2b2b2b;");
        
        Label title = new Label("Messthon");
        title.setFont(Font.font("Arial", FontWeight.BOLD, 36));
        title.setStyle("-fx-text-fill: #00ff88;");
        
        Label subtitle = new Label("Private Messenger");
        subtitle.setFont(Font.font("Arial", 14));
        subtitle.setStyle("-fx-text-fill: #888888;");
        
        TextField usernameField = new TextField();
        usernameField.setPromptText("Enter your username");
        usernameField.setMaxWidth(300);
        usernameField.setStyle("-fx-background-color: #3b3b3b; -fx-text-fill: white;");
        
        TextField codeField = new TextField();
        codeField.setPromptText("Enter your code (leave empty for new account)");
        codeField.setMaxWidth(300);
        codeField.setStyle("-fx-background-color: #3b3b3b; -fx-text-fill: white;");
        
        Button loginBtn = new Button("Connect");
        loginBtn.setStyle("-fx-background-color: #00ff88; -fx-text-fill: black; -fx-font-weight: bold;");
        loginBtn.setMaxWidth(300);
        
        Label messageLabel = new Label();
        messageLabel.setStyle("-fx-text-fill: #ff4444;");
        
        loginBtn.setOnAction(e -> {
            String user = usernameField.getText().trim();
            String code = codeField.getText().trim();
            
            if (user.isEmpty()) {
                messageLabel.setText("Username cannot be empty");
                return;
            }
            
            loginBtn.setDisable(true);
            loginBtn.setText("Connecting...");
            
            new Thread(() -> {
                try {
                    client = new Client("localhost", 54321, this::handleIncomingMessage);
                    if (client.connect(user, code)) {
                        Platform.runLater(() -> {
                            username = user;
                            userCode = client.getUserCode();
                            showMainWindow(primaryStage);
                        });
                    } else {
                        Platform.runLater(() -> {
                            messageLabel.setText("Connection failed: " + client.getLastError());
                            loginBtn.setDisable(false);
                            loginBtn.setText("Connect");
                        });
                    }
                } catch (Exception ex) {
                    Platform.runLater(() -> {
                        messageLabel.setText("Error: " + ex.getMessage());
                        loginBtn.setDisable(false);
                        loginBtn.setText("Connect");
                    });
                }
            }).start();
        });
        
        loginBox.getChildren().addAll(title, subtitle, usernameField, codeField, loginBtn, messageLabel);
        
        Scene scene = new Scene(loginBox, 400, 400);
        primaryStage.setScene(scene);
        primaryStage.show();
    }
    
    private void showMainWindow(Stage primaryStage) {
        BorderPane mainPane = new BorderPane();
        mainPane.setStyle("-fx-background-color: #1e1e1e;");
        
        HBox topBar = new HBox(10);
        topBar.setPadding(new Insets(10));
        topBar.setStyle("-fx-background-color: #2b2b2b;");
        topBar.setAlignment(Pos.CENTER_LEFT);
        
        Label titleLabel = new Label("Messthon");
        titleLabel.setFont(Font.font("Arial", FontWeight.BOLD, 18));
        titleLabel.setStyle("-fx-text-fill: #00ff88;");
        
        Label userLabel = new Label(username + " [" + userCode + "]");
        userLabel.setStyle("-fx-text-fill: #888888;");
        
        statusLabel = new Label("● Online");
        statusLabel.setStyle("-fx-text-fill: #00ff88;");
        
        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);
        
        Button addContactBtn = new Button("+ Add Friend");
        addContactBtn.setStyle("-fx-background-color: #00ff88; -fx-text-fill: black;");
        
        topBar.getChildren().addAll(titleLabel, userLabel, spacer, statusLabel, addContactBtn);
        
        VBox leftPanel = new VBox(10);
        leftPanel.setPadding(new Insets(10));
        leftPanel.setPrefWidth(250);
        leftPanel.setStyle("-fx-background-color: #252525;");
        
        Label contactsTitle = new Label("Friends");
        contactsTitle.setFont(Font.font("Arial", FontWeight.BOLD, 14));
        contactsTitle.setStyle("-fx-text-fill: #00ff88;");
        
        contactsList = new ListView<>(contacts);
        contactsList.setStyle("-fx-control-inner-background: #2b2b2b; -fx-text-fill: white;");
        contactsList.setPrefHeight(400);
        
        contactsList.setOnMouseClicked(e -> {
            String selected = contactsList.getSelectionModel().getSelectedItem();
            if (selected != null) {
                openChat(selected);
            }
        });
        
        leftPanel.getChildren().addAll(contactsTitle, contactsList);
        
        chatTabs = new TabPane();
        chatTabs.setStyle("-fx-background-color: #1e1e1e;");
        
        Tab welcomeTab = new Tab("Welcome");
        welcomeTab.setContent(createWelcomePanel());
        welcomeTab.setClosable(false);
        chatTabs.getTabs().add(welcomeTab);
        
        mainPane.setTop(topBar);
        mainPane.setLeft(leftPanel);
        mainPane.setCenter(chatTabs);
        
        addContactBtn.setOnAction(e -> showAddContactDialog());
        
        loadContacts();
        
        Scene scene = new Scene(mainPane, 900, 600);
        primaryStage.setScene(scene);
    }
    
    private VBox createWelcomePanel() {
        VBox welcome = new VBox(20);
        welcome.setAlignment(Pos.CENTER);
        welcome.setPadding(new Insets(50));
        welcome.setStyle("-fx-background-color: #1e1e1e;");
        
        Label welcomeLabel = new Label("Welcome to Messthon!");
        welcomeLabel.setFont(Font.font("Arial", FontWeight.BOLD, 24));
        welcomeLabel.setStyle("-fx-text-fill: #00ff88;");
        
        Label infoLabel = new Label(
            "Your code is:\n" + userCode + 
            "\n\nShare this code with friends to start chatting!\n" +
            "Click '+' to add a friend by their code."
        );
        infoLabel.setStyle("-fx-text-fill: #cccccc; -fx-font-size: 14px;");
        infoLabel.setTextAlignment(javafx.scene.text.TextAlignment.CENTER);
        
        welcome.getChildren().addAll(welcomeLabel, infoLabel);
        return welcome;
    }
    
    private void showAddContactDialog() {
        Dialog<String> dialog = new Dialog<>();
        dialog.setTitle("Add Friend");
        dialog.setHeaderText("Enter your friend's code");
        
        ButtonType addButton = new ButtonType("Add", ButtonBar.ButtonData.OK_DONE);
        dialog.getDialogPane().getButtonTypes().addAll(addButton, ButtonType.CANCEL);
        
        TextField codeField = new TextField();
        codeField.setPromptText("XXXX-XXXX-XXXX");
        
        VBox content = new VBox(10);
        content.getChildren().addAll(new Label("Friend's Code:"), codeField);
        dialog.getDialogPane().setContent(content);
        
        dialog.setResultConverter(dialogButton -> {
            if (dialogButton == addButton) {
                return codeField.getText();
            }
            return null;
        });
        
        dialog.showAndWait().ifPresent(code -> {
            if (!code.isEmpty()) {
                client.addContact(code);
            }
        });
    }
    
    private void openChat(String contactInfo) {
        String[] parts = contactInfo.split(":");
        String friendCode = parts[0];
        String friendName = parts[1];
        
        if (!openChats.containsKey(friendName)) {
            ChatPanel chatPanel = new ChatPanel(client, username, friendName);
            openChats.put(friendName, chatPanel);
            
            Tab tab = new Tab(friendName);
            tab.setContent(chatPanel);
            tab.setOnClosed(e -> openChats.remove(friendName));
            chatTabs.getTabs().add(tab);
            chatTabs.getSelectionModel().select(tab);
        } else {
            for (Tab tab : chatTabs.getTabs()) {
                if (tab.getText().equals(friendName)) {
                    chatTabs.getSelectionModel().select(tab);
                    break;
                }
            }
        }
    }
    
    private void loadContacts() {
        new Thread(() -> {
            List<String> contactList = client.getContacts();
            Platform.runLater(() -> {
                contacts.clear();
                contacts.addAll(contactList);
            });
        }).start();
    }
    
    private void handleIncomingMessage(Message msg) {
        Platform.runLater(() -> {
            if (msg.getType().equals("MESSAGE")) {
                String sender = msg.getSender();
                for (String contact : contacts) {
                    if (contact.contains(sender)) {
                        openChat(contact);
                        ChatPanel chat = openChats.get(sender);
                        if (chat != null) {
                            chat.addMessage(sender, msg.getContent(), msg.getTimestamp());
                        }
                        break;
                    }
                }
            } else if (msg.getType().equals("CONTACT_ADDED")) {
                loadContacts();
            } else if (msg.getType().equals("USER_OFFLINE")) {
                statusLabel.setText("● User offline");
                statusLabel.setStyle("-fx-text-fill: #ff4444;");
            } else if (msg.getType().equals("CONTACTS_LIST")) {
                String[] contactsArray = msg.getContent().split(";");
                Platform.runLater(() -> {
                    contacts.clear();
                    for (String contact : contactsArray) {
                        if (!contact.isEmpty()) {
                            contacts.add(contact);
                        }
                    }
                });
            }
        });
    }
}
