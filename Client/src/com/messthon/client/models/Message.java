package com.messthon.client;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

public class Message implements Serializable {
    private static final long serialVersionUID = 1L;
    private String sender;
    private String recipient;
    private String type;
    private String content;
    private String timestamp;
    
    public Message(String sender, String recipient, String type, String content) {
        this.sender = sender;
        this.recipient = recipient;
        this.type = type;
        this.content = content;
        this.timestamp = LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
    }
    
    public String getSender() { return sender; }
    public String getRecipient() { return recipient; }
    public String getType() { return type; }
    public String getContent() { return content; }
    public String getTimestamp() { return timestamp; }
}
