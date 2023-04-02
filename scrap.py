import requests
from bs4 import BeautifulSoup
import pandas as pd

# Load the Excel file into a pandas DataFrame
df = pd.read_excel(r"C:\Users\HP\Downloads\web.xlsx")

# Define a function to extract the address from a college's website
def get_address(url):
    try:
        response = requests.get(url)
        soup = BeautifulSoup(response.content, 'html.parser')
        address = soup.find('address').get_text().strip()
    except:
        address = ''
    return address

# Add a new column to the DataFrame for the address
#df['address'] = df['website'].apply(get_address)

# Write the updated DataFrame to a new Excel file
df.to_excel(r"C:\Users\HP\Downloads\web.xlsx", index=False)

print(df)
