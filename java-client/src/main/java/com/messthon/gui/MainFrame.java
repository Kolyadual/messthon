package com.messthon.gui;

import com.messthon.network.ClientConnection;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.util.HashMap;
import java.util.Map;
import java.util.UUID;

public class MainFrame extends JFrame {
    private ClientConnection connection;
    private String userId;
    private Map<String, String> onlineUsers = new HashMap<>();
    
    // GUI компоненты
    private JTextArea chatArea;
    private JTextField messageField;
    private JList<String> usersList;
    private DefaultListModel<String> usersListModel;
    private JTextField usernameField;
    
    private ObjectMapper mapper = new ObjectMapper();
    
    public MainFrame() {
        setupUI();
        setupConnection();
        setTitle("Messthon Messenger");
        setSize(900, 600);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);
    }
    
    private void setupUI() {
        // Основной контейнер
        Container container = getContentPane();
        container.setLayout(new BorderLayout());
        
        // Верхняя панель - регистрация
        JPanel topPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        topPanel.setBorder(BorderFactory.createTitledBorder("Регистрация"));
        
        JLabel usernameLabel = new JLabel("Имя пользователя:");
        usernameField = new JTextField(15);
        JButton registerButton = new JButton("Войти");
        JButton connectButton = new JButton("Подключиться");
        
        topPanel.add(usernameLabel);
        topPanel.add(usernameField);
        topPanel.add(registerButton);
        topPanel.add(connectButton);
        
        // Левая панель - список пользователей
        JPanel leftPanel = new JPanel(new BorderLayout());
        leftPanel.setPreferredSize(new Dimension(200, 0));
        leftPanel.setBorder(BorderFactory.createTitledBorder("Онлайн пользователи"));
        
        usersListModel = new DefaultListModel<>();
        usersList = new JList<>(usersListModel);
        JScrollPane usersScroll = new JScrollPane(usersList);
        
        JButton refreshButton = new JButton("Обновить");
        
        leftPanel.add(usersScroll, BorderLayout.CENTER);
        leftPanel.add(refreshButton, BorderLayout.SOUTH);
        
        // Центральная панель - чат
        JPanel centerPanel = new JPanel(new BorderLayout());
        centerPanel.setBorder(BorderFactory.createTitledBorder("Чат"));
        
        chatArea = new JTextArea();
        chatArea.setEditable(false);
        chatArea.setFont(new Font("Monospaced", Font.PLAIN, 12));
        JScrollPane chatScroll = new JScrollPane(chatArea);
        
        // Панель ввода сообщения
        JPanel inputPanel = new JPanel(new BorderLayout());
        messageField = new JTextField();
        JButton sendButton = new JButton("Отправить");
        
        inputPanel.add(messageField, BorderLayout.CENTER);
        inputPanel.add(sendButton, BorderLayout.EAST);
        
        centerPanel.add(chatScroll, BorderLayout.CENTER);
        centerPanel.add(inputPanel, BorderLayout.SOUTH);
        
        // Добавляем все панели
        container.add(topPanel, BorderLayout.NORTH);
        container.add(leftPanel, BorderLayout.WEST);
        container.add(centerPanel, BorderLayout.CENTER);
        
        // Обработчики событий
        registerButton.addActionListener(e -> registerUser());
        connectButton.addActionListener(e -> connectToServer());
        refreshButton.addActionListener(e -> refreshUsers());
        sendButton.addActionListener(e -> sendMessage());
        messageField.addActionListener(e -> sendMessage());
        
        usersList.addMouseListener(new MouseAdapter() {
            public void mouseClicked(MouseEvent evt) {
                if (evt.getClickCount() == 2) {
                    String selected = usersList.getSelectedValue();
                    if (selected != null && selected.contains(":")) {
                        String userId = selected.split(":")[0].trim();
                        messageField.setText("@[" + userId + "] ");
                        messageField.requestFocus();
                    }
                }
            }
        });
    }
    
    private void setupConnection() {
        connection = new ClientConnection("127.0.0.1", 8080);
        connection.setMessageHandler(this::handleIncomingMessage);
    }
    
    private void connectToServer() {
        try {
            connection.connect();
            JOptionPane.showMessageDialog(this, "Подключено к серверу!");
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this, 
                "Ошибка подключения: " + e.getMessage(),
                "Ошибка", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    private void registerUser() {
        String username = usernameField.getText().trim();
        if (username.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Введите имя пользователя!");
            return;
        }
        
        if (!connection.isConnected()) {
            JOptionPane.showMessageDialog(this, "Сначала подключитесь к серверу!");
            return;
        }
        
        userId = "user_" + UUID.randomUUID().toString().substring(0, 8);
        
        try {
            ObjectNode registerMsg = mapper.createObjectNode();
            registerMsg.put("type", "register");
            registerMsg.put("user_id", userId);
            registerMsg.put("username", username);
            
            connection.sendMessage(registerMsg.toString());
            appendToChat("Система", "Регистрация отправлена...");
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this, 
                "Ошибка регистрации: " + e.getMessage(),
                "Ошибка", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    private void refreshUsers() {
        if (!connection.isConnected()) {
            JOptionPane.showMessageDialog(this, "Не подключено к серверу!");
            return;
        }
        
        try {
            ObjectNode getUsersMsg = mapper.createObjectNode();
            getUsersMsg.put("type", "get_users");
            connection.sendMessage(getUsersMsg.toString());
        } catch (Exception e) {
            appendToChat("Система", "Ошибка обновления списка: " + e.getMessage());
        }
    }
    
    private void sendMessage() {
        String text = messageField.getText().trim();
        if (text.isEmpty() || userId == null) {
            return;
        }
        
        String selectedUser = usersList.getSelectedValue();
        if (selectedUser == null && !text.startsWith("@[")) {
            JOptionPane.showMessageDialog(this, 
                "Выберите пользователя из списка или укажите @[user_id]");
            return;
        }
        
        String toUserId;
        String message;
        
        if (text.startsWith("@[")) {
            // Извлекаем user_id из @[user_id]
            int end = text.indexOf("]");
            if (end == -1) {
                JOptionPane.showMessageDialog(this, "Неверный формат!");
                return;
            }
            toUserId = text.substring(2, end);
            message = text.substring(end + 1).trim();
        } else {
            toUserId = selectedUser.split(":")[0].trim();
            message = text;
        }
        
        try {
            ObjectNode messageMsg = mapper.createObjectNode();
            messageMsg.put("type", "message");
            messageMsg.put("from", userId);
            messageMsg.put("to", toUserId);
            messageMsg.put("content", message);
            
            connection.sendMessage(messageMsg.toString());
            
            appendToChat("Вы -> " + onlineUsers.getOrDefault(toUserId, toUserId), 
                        message);
            messageField.setText("");
            
        } catch (Exception e) {
            appendToChat("Система", "Ошибка отправки: " + e.getMessage());
        }
    }
    
    private void handleIncomingMessage(String jsonMessage) {
        SwingUtilities.invokeLater(() -> {
            try {
                ObjectNode message = (ObjectNode) mapper.readTree(jsonMessage);
                String type = message.get("type").asText();
                
                switch (type) {
                    case "register_response":
                        if ("success".equals(message.get("status").asText())) {
                            appendToChat("Система", "Регистрация успешна!");
                            refreshUsers();
                        } else {
                            appendToChat("Система", "Ошибка регистрации");
                        }
                        break;
                        
                    case "users_list":
                        usersListModel.clear();
                        onlineUsers.clear();
                        
                        message.get("users").forEach(user -> {
                            String userId = user.get(0).asText();
                            String username = user.get(1).asText();
                            onlineUsers.put(userId, username);
                            usersListModel.addElement(userId + ": " + username);
                        });
                        break;
                        
                    case "message_received":
                        String from = message.get("from_name").asText();
                        String content = message.get("content").asText();
                        appendToChat(from, content);
                        break;
                        
                    case "error":
                        appendToChat("Система", "Ошибка: " + 
                                    message.get("message").asText());
                        break;
                        
                    default:
                        appendToChat("Система", "Неизвестное сообщение: " + type);
                }
                
            } catch (Exception e) {
                appendToChat("Система", "Ошибка обработки сообщения: " + 
                            e.getMessage());
            }
        });
    }
    
    private void appendToChat(String sender, String message) {
        chatArea.append("[" + java.time.LocalTime.now().format(
            java.time.format.DateTimeFormatter.ofPattern("HH:mm:ss")) + "] ");
        chatArea.append(sender + ": " + message + "\n");
        chatArea.setCaretPosition(chatArea.getDocument().getLength());
    }
}
