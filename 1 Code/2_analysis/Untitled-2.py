import requests

# Your API key
api_key = '92d79661-8e06-4d48-9796-618e067d48cd'

# Sirene API endpoint for searching companies
url = 'https://api.sirene.fr/entreprises'

# Set up headers with your API key for authentication
headers = {
    'Authorization': f'Bearer {api_key}'
}

# Optional parameters to filter based on employee size (trancheeffectif)
# You can modify the query to suit your needs, this is just an example
params = {
    'trancheeffectif': '1-9',  # Example: Looking for companies with 1-9 employees
    # Add other filtering parameters if needed
    'per_page': 100,  # Number of results per page
    'page': 1  # Page number, adjust as necessary for pagination
}

# Make the GET request to the API
response = requests.get(url, headers=headers, params=params)

# Check the response status
if response.status_code == 200:
    data = response.json()  # Parse the JSON data
    
    # Loop through the results and extract SIRET and trancheeffectif
    for company in data.get('etablissements', []):  # 'etablissements' contains the company info
        siret = company.get('siret')
        trancheeffectif = company.get('trancheeffectif')
        print(f"SIRET: {siret}, Tranche Effectif: {trancheeffectif}")
else:
    print(f"Error: {response.status_code}, {response.text}")
