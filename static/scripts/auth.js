// Инициализация SharedWorker
const worker = new SharedWorker('scripts/sharedWorker.js');

async function getSession() {
    try {
        let response = await fetch('/api/getsession');
        if (!response.ok) {
            throw new Error('Network response was not ok');
        }
        let data = await response.json();
        return data;
    } catch (error) {
        console.error('Error fetching session:', error);
    }
}
    

function initWebSocket(sId) {
    return new Promise((resolve, reject) => {
        worker.port.onmessage = function(event) {
            const { type, data, error } = event.data;

            if (type === 'connected') {
                console.log('WebSocket connection established via SharedWorker');
                resolve(worker.port);  // Возвращаем порт для взаимодействия
            } else if (type === 'message') {
                console.log('Message from WebSocket:', data);
            } else if (type === 'closed') {
                console.log('WebSocket connection closed');
            } else if (type === 'error') {
                console.error('WebSocket error:', error);
                reject(error);
            }
        };

        worker.port.start();  // Запуск порта для получения сообщений

        // Отправляем сообщение в SharedWorker для инициализации WebSocket с sId
        worker.port.postMessage({ type: 'init', sId });
    });
}


// login and init ws routine
document.addEventListener('DOMContentLoaded', () => {
    async function mainLogin() {
        try {
            const { sId, uId } = await getSession();
            const port = await initWebSocket(sId);
            // Сохраняем данные в sessionStorage
             sessionStorage.setItem('ws', 'shared');
             sessionStorage.setItem('sId', sId);
             if (uId) { sessionStorage.setItem('uId', uId);} // uId is null for anon user

            window.location.href = '/lobby';
        } catch (error) {
            console.error('Error in main function:', error);
        }
    }

    mainLogin();
});
