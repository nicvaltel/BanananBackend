document.getElementById('fetchDataBtn').addEventListener('click', function() {
  console.log ("CLICK");
  // Fetch data from the API endpoint
  fetch('/api/sessionrepo')
      .then(response => response.json())
      .then(data => {
        console.log(data);

          // Populate the table with the fetched data
          const tableBody = document.getElementById('dataTable').getElementsByTagName('tbody')[0];
          tableBody.innerHTML = ''; // Clear existing data

          data.sessionIds.forEach(item => {
              // Create a new row
              const row = document.createElement('tr');

              const cell = document.createElement('td');
              cell.textContent = item;
              row.appendChild(cell);

              
              // // Create and append cells to the row
              // Object.values(item).forEach(value => {
              //     const cell = document.createElement('td');
              //     cell.textContent = value;
              //     row.appendChild(cell);
              // });

              // Append the row to the table body
              tableBody.appendChild(row);
          });
      })
      .catch(error => {
          console.error('Error fetching data:', error);
      });
});
