let ws;

onconnect = function(event) {
    const port = event.ports[0];

    port.onmessage = function(event) {
        const { type, sId, message } = event.data;

        if (type === 'init' && (!ws || ws.readyState === WebSocket.CLOSED)) {
            // Инициализация WebSocket с переданным sId
            ws = new WebSocket(`ws://localhost:1234/session?sessionId=${sId}`);

            ws.onopen = () => {
                console.log('WebSocket connection established in SharedWorker');
                port.postMessage({ type: 'connected' });
            };

            ws.onmessage = function(message) {
                port.postMessage({ type: 'message', data: message.data });
            };

            ws.onclose = () => {
                console.log('WebSocket connection closed in SharedWorker');
                port.postMessage({ type: 'closed' });
            };

            ws.onerror = (error) => {
                console.error('WebSocket error in SharedWorker:', error);
                port.postMessage({ type: 'error', error });
            };
        } else if (type === 'send' && ws && ws.readyState === WebSocket.OPEN) {
            // Отправка сообщения через WebSocket
            console.log('sending: ', message);
            ws.send(message);
        }
    };
};
