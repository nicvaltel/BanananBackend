// Get the relative URL (path and query string, without domain)
const relativeUrl = window.location.pathname + window.location.search;

// Assuming relativeUrl looks like 'gameroom/1', extract the game room number
const gameRoomNumber = relativeUrl.split('/').pop(); // Extracts the last part after the slash

// Construct the API endpoint (without encoding the game room number)
const apiEndpoint = `/api/joingame`;

// Call the API endpoint using fetch, passing the game room number in the JSON body
fetch(apiEndpoint, {
    method: 'POST',
    headers: {
        'Content-Type': 'application/json'
    },
    body: gameRoomNumber // Send the game room number as a string
    // body: JSON.stringify({
    //     lbId: gameRoomNumber
    // })
})

// TODO - get response, check it, if response incorrect redirect to lobby
// .then(response => response.json())
// .then(data => {
//     console.log('Joined game:', data);
//     // Optionally, redirect the user or update the UI based on the response
// })
// .catch(error => console.error('Error joining game:', error));
