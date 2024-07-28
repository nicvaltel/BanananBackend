// Create a new WebSocket object
const socket = new WebSocket('ws://localhost:1234/session?sessionId=111');

// Connection opened
socket.addEventListener('open', (event) => {
    console.log('WebSocket connection opened:', event);

    // Send a message to the server
    socket.send('Hello, server!');
});

// Listen for messages from the server
socket.addEventListener('message', (event) => {
    console.log('Received message from server:', event.data);
});

// Listen for WebSocket errors
socket.addEventListener('error', (event) => {
    console.error('WebSocket error:', event);
});

// Listen for WebSocket closures
socket.addEventListener('close', (event) => {
    console.log('WebSocket connection closed:', event);
});

// Close the WebSocket connection after 5 seconds (for demonstration purposes)
setTimeout(() => {
    socket.close();
}, 5000);
