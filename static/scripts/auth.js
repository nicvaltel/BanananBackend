
document.addEventListener('DOMContentLoaded', () => {
  function fetchSessionId() {
      fetch('/api/users')
          .then(response => response.json())
          .then(jsonString => {
              const data = JSON.parse(jsonString);
              if (data.sId) {
                  // Save sId and uId into Local Storage
                  localStorage.setItem('uId', data.uId);
                  localStorage.setItem('sId', data.sId);
                  window.location.href = '/lobby';
              } else {
                  console.error('sId not found in server response');
              }
          })
          .catch(error => console.error('Error:', error));
  }

  // call function for getting sId
  fetchSessionId();
});
