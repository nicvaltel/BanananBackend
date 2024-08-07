

document.addEventListener('DOMContentLoaded', function() {
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
        fetchData(); // Refresh table data after adding game
    })
    .catch(error => console.error('Error adding game to lobby:', error));
});

  // Fetch data initially and then every second
  fetchData();
  setInterval(fetchData, 500); // 0.5 second
});
