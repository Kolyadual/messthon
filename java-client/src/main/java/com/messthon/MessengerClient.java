package com.messthon;

import com.messthon.gui.MainFrame;
import com.messthon.network.ClientConnection;

import javax.swing.*;

public class MessengerClient {
    public static void main(String[] args) {
        // Запускаем GUI в Event Dispatch Thread
        SwingUtilities.invokeLater(() -> {
            try {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
                new MainFrame().setVisible(true);
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }
}
