import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.*;
import java.util.*;

public class MessengerClient extends JFrame {
    private Socket socket;
    private BufferedReader in;
    private PrintWriter out;
    private String username;
    
    // GUI компоненты
    private JTextArea chatArea;
    private JTextField messageField;
    private JList<String> usersList;
    private DefaultListModel<String> usersListModel;
    private JTextField usernameField;
    private JButton connectButton;
    private JButton registerButton;
    
    private Timer refreshTimer;
    
    public MessengerClient() {
        setTitle("Messthon by Kolyadual");
        setSize(800, 500);
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);
        
        initUI();
    }
    
    private void initUI() {
        // Основной контейнер
        Container container = getContentPane();
        container.setLayout(new BorderLayout());
        
        // Верхняя панель
        JPanel topPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        topPanel.setBorder(BorderFactory.createTitledBorder("Подключение"));
        
        JLabel userLabel = new JLabel("Имя:");
        usernameField = new JTextField(10);
        connectButton = new JButton("Подключиться");
        registerButton = new JButton("Зарегистрироваться");
        registerButton.setEnabled(false);
        
        topPanel.add(userLabel);
        topPanel.add(usernameField);
        topPanel.add(connectButton);
        topPanel.add(registerButton);
        
        // Левая панель - пользователи
        JPanel leftPanel = new JPanel(new BorderLayout());
        leftPanel.setPreferredSize(new Dimension(150, 0));
        leftPanel.setBorder(BorderFactory.createTitledBorder("Онлайн"));
        
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
        JScrollPane chatScroll = new JScrollPane(chatArea);
        
        // Панель ввода
        JPanel inputPanel = new JPanel(new BorderLayout());
        messageField = new JTextField();
        JButton sendButton = new JButton("Отправить");
        
        inputPanel.add(messageField, BorderLayout.CENTER);
        inputPanel.add(sendButton, BorderLayout.EAST);
        
        centerPanel.add(chatScroll, BorderLayout.CENTER);
        centerPanel.add(inputPanel, BorderLayout.SOUTH);
        
        // Добавляем все
        container.add(topPanel, BorderLayout.NORTH);
        container.add(leftPanel, BorderLayout.WEST);
        container.add(centerPanel, BorderLayout.CENTER);
        
        // Обработчики событий
        connectButton.addActionListener(e -> connectToServer());
        registerButton.addActionListener(e -> registerUser());
        refreshButton.addActionListener(e -> refreshUsers());
        sendButton.addActionListener(e -> sendMessage());
        messageField.addActionListener(e -> sendMessage());
        
        // Таймер для обновления списка пользователей
        refreshTimer = new Timer(5000, e -> refreshUsers());
    }
    
    private void connectToServer() {
        try {
            String serverHost = "127.0.0.1";
            int serverPort = 8080;
            
            socket = new Socket(serverHost, serverPort);
            in = new BufferedReader(new InputStreamReader(socket.getInputStream()));
            out = new PrintWriter(socket.getOutputStream(), true);
            
            appendToChat("Система", "Подключено к серверу");
            connectButton.setEnabled(false);
            registerButton.setEnabled(true);
            
            // Запускаем поток для чтения сообщений
            new Thread(this::readMessages).start();
            
            // Запускаем таймер обновления
            refreshTimer.start();
            
        } catch (Exception e) {
            JOptionPane.showMessageDialog(this,
                "Ошибка подключения: " + e.getMessage(),
                "Ошибка", JOptionPane.ERROR_MESSAGE);
        }
    }
    
    private void registerUser() {
        username = usernameField.getText().trim();
        if (username.isEmpty()) {
            JOptionPane.showMessageDialog(this, "Введите имя пользователя!");
            return;
        }
        
        out.println("REGISTER " + username);
        registerButton.setEnabled(false);
        usernameField.setEnabled(false);
    }
    
    private void refreshUsers() {
        if (out != null) {
            out.println("ONLINE");
        }
    }
    
    private void sendMessage() {
        String text = messageField.getText().trim();
        if (text.isEmpty() || username == null) {
            return;
        }
        
        String selectedUser = usersList.getSelectedValue();
        if (selectedUser == null) {
            JOptionPane.showMessageDialog(this, "Выберите получателя из списка!");
            return;
        }
        
        out.println("SEND " + selectedUser + " " + text);
        appendToChat("Вы -> " + selectedUser, text);
        messageField.setText("");
    }
    
    private void readMessages() {
        try {
            String line;
            while ((line = in.readLine()) != null) {
                processServerMessage(line);
            }
        } catch (Exception e) {
            SwingUtilities.invokeLater(() -> {
                appendToChat("Система", "Соединение разорвано");
                connectButton.setEnabled(true);
                registerButton.setEnabled(false);
                refreshTimer.stop();
            });
        }
    }
    
    private void processServerMessage(String message) {
        SwingUtilities.invokeLater(() -> {
            if (message.startsWith("REGISTERED как")) {
                appendToChat("Система", "Вы зарегистрированы как " + username);
                refreshUsers();
            } else if (message.startsWith("ОНЛАЙН:")) {
                updateUsersList(message.substring(7).trim());
            } else if (message.startsWith("SENT")) {
                // Подтверждение отправки
            } else if (message.contains(":")) {
                // Входящее сообщение
                String[] parts = message.split(":", 2);
                if (parts.length == 2) {
                    String from = parts[0].trim();
                    String msg = parts[1].trim();
                    appendToChat(from, msg);
                }
            } else {
                appendToChat("Сервер", message);
            }
        });
    }
    
    private void updateUsersList(String usersStr) {
        usersListModel.clear();
        
        // Парсим список пользователей из Erlang формата
        usersStr = usersStr.replace("[", "").replace("]", "").replace("\"", "");
        String[] users = usersStr.split(",");
        
        for (String user : users) {
            user = user.trim();
            if (!user.isEmpty() && !user.equals(username)) {
                usersListModel.addElement(user);
            }
        }
    }
    
    private void appendToChat(String sender, String message) {
        String time = new java.text.SimpleDateFormat("HH:mm:ss")
            .format(new java.util.Date());
        chatArea.append("[" + time + "] " + sender + ": " + message + "\n");
    }
    
    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                new MessengerClient().setVisible(true);
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }
}
