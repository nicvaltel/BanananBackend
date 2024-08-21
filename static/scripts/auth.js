
// document.addEventListener('DOMContentLoaded', () => {
//   function fetchSessionId() {
//       fetch('/api/users')
//           .then(response => response.json())
//           .then(jsonString => {
//               const data = JSON.parse(jsonString);
//               if (data.sId) {
//                   // Save sId and uId into Local Storage
//                   localStorage.setItem('uId', data.uId);
//                   localStorage.setItem('sId', data.sId);
//                   window.location.href = '/lobby';
//               } else {
//                   console.error('sId not found in server response');
//               }
//           })
//           .catch(error => console.error('Error:', error));
//   }

//   // call function for getting sId
//   fetchSessionId();
// });


document.addEventListener('DOMContentLoaded', () => {

    // Функция для получения сессии
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

    // Функция для инициализации WebSocket соединения
    function initWebSocket(sessionId, userId) {
        return new Promise((resolve, reject) => {
            const ws = new WebSocket(`ws://yourserver.com/websocket?sessionId=${sessionId}`);

            ws.onopen = () => {
                console.log('WebSocket connection established');
                resolve(ws);
            };

            ws.onerror = (error) => {
                console.error('WebSocket error:', error);
                reject(error);
            };

            ws.onclose = () => {
                console.log('WebSocket connection closed');
            };
        });
    }

    // Функция для отправки JSON с userId и sessionId на '/api/login'
    async function sendLoginData(ws, userId, sessionId) {
        const loginData = JSON.stringify({ userId: userId, sessionId: sessionId });
        try {
            ws.send(loginData);
            console.log('Login data sent:', loginData);
            // Переход на страницу '/lobby' после успешной отправки
            window.location.href = '/lobby';
        } catch (error) {
            console.error('Error sending login data:', error);
        }
    }

    // Основная функция, которая выполняет последовательность действий
    async function main() {
        try {
            const { sessionId, userId } = await getSession();
            const ws = await initWebSocket(sessionId, userId);
            await sendLoginData(ws, userId, sessionId);

            // Сохраняем данные в sessionStorage
            sessionStorage.setItem('ws', ws.url);
            sessionStorage.setItem('sessionId', sessionId);
            sessionStorage.setItem('userId', userId);

            // Переход на страницу /lobby
            window.location.href = '/lobby';
        } catch (error) {
            console.error('Error in main function:', error);
        }
    }

    // Запуск основного процесса
    main();
});