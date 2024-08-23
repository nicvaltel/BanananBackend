// Connect to SharedWorker
const worker = new SharedWorker('scripts/sharedWorker.js');

function sendWebSocketMessage(message) {
  return new Promise((resolve, reject) => {
      // Настраиваем обработку сообщений от WebSocket через SharedWorker
      worker.port.onmessage = function(event) {
          const { type, data, error } = event.data;

          if (type === 'message') {
              console.log('Received message from WebSocket:', data);
              resolve(data);
          } else if (type === 'error') {
              console.error('WebSocket error:', error);
              reject(error);
          }
      };

      // Отправляем сообщение в SharedWorker, чтобы он передал его через WebSocket
      worker.port.postMessage({ type: 'send', message });
  });
}


function fetchData() {
    fetch('/api/lobbytable')
        .then(response => response.json())
        .then(data => populateTable(data))
        .catch(error => console.error('Error fetching data:', error));
}

function populateTable(data) {
    const tableBody = document.querySelector('table tbody');
    tableBody.innerHTML = ''; // Clear the table body

    data.forEach(row => {
      const tr = document.createElement('tr');
      tr.addEventListener('click', function() {
          window.location.href = row.link; 
      });

      const playerNameTd = document.createElement('td');
      playerNameTd.textContent = row.playerName;
      tr.appendChild(playerNameTd);

      const ratingTd = document.createElement('td');
      ratingTd.textContent = row.rating;
      tr.appendChild(ratingTd);

      const gameTypeTd = document.createElement('td');
      gameTypeTd.textContent = row.gameType;
      tr.appendChild(gameTypeTd);

      const gameModeTd = document.createElement('td');
      gameModeTd.textContent = row.gameMode;
      tr.appendChild(gameModeTd);

      tableBody.appendChild(tr);
  });
}

function checkLobbyStatus(sId) {
  const intervalId = setInterval(function() {
      fetch(`/api/checklobbygamestatus/${sId}`)
          .then(response => response.text())
          .then(data => {
              if (data) { // If the response is not empty
                  // Remove surrounding quotes from the string
                  const sanitizedData = data.replace(/^"(.*)"$/, '$1');
                  if (sanitizedData) { // If the response is not empty
                      console.log(sanitizedData);
                      clearInterval(intervalId); // Stop polling
                      window.location.href = sanitizedData; // Redirect to the URL in the response
                  }
              }
          })
          .catch(error => {
              clearInterval(intervalId); // Stop polling on error
              console.error('Error checking lobby game status:', error);
          });
  }, 500);
}


document.getElementById('startBot').addEventListener('click', function() {
  console.log('Start Game with Bot button pressed');
});

document.getElementById('waitOpponent').addEventListener('click', function() {
  console.log('Wait for Opponent button pressed');
  fetch('/api/addgametolobby', {
      method: 'POST',
      headers: {
          'Content-Type': 'application/json'
      },
      body: JSON.stringify({ action: 'wait_for_opponent' })
  })
  .then(response => response.json())
  .then(data => {
      console.log('Game added to lobby:', data);
      console.log('lobbyId = :', data.lobbyId);
      fetchData(); // Refresh table data after adding game
      const sId = sessionStorage.getItem('sId');
      checkLobbyStatus(sId); // Start polling the lobby status
  })
  .catch(error => console.error('Error adding game to lobby:', error));
});


document.addEventListener('DOMContentLoaded', function() {

  sendWebSocketMessage('Hello from /lobby page!')
        .then(response => {
            console.log('Message successfully sent and received:', response);
        })
        .catch(error => {
            console.error('Error sending WebSocket message:', error);
        });

  // Fetch data initially and then every 0.5 seconds
  fetchData();
  setInterval(fetchData, 500); // 0.5 second
});
