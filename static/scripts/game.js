// Get the relative URL (path and query string, without domain)
const relativeUrl = window.location.pathname + window.location.search;

// Encode the URL to make it safe for use in a path parameter
const gameUrl = encodeURIComponent(relativeUrl);

// Construct the API endpoint with the encoded URL as a parameter
const apiEndpoint = `/api/joingame/${gameUrl}`;

console.log(gameUrl);

// Call the API endpoint using fetch
fetch(apiEndpoint, {
    method: 'POST',
    headers: {
        'Content-Type': 'application/json'
    }
})
.then(response => response.json())
.then(data => {
    console.log('Joined game:', data);
    // Optionally, redirect the user or update the UI based on the response
})
.catch(error => console.error('Error joining game:', error));
