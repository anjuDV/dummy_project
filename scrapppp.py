import requests
from bs4 import BeautifulSoup
url = "https://twitter.com"

r = requests.get(url)
htmlContent = r.content
print(htmlContent)

soup = BeautifulSoup(htmlContent, 'html.parser')
#print(soup.prettify)

title = soup.title
# print(type(title))
# print(type(title.string))

paras = soup.find_all('p')
print(paras)

anchors = soup.find_all('a')
all_links = set()

for link in anchors:
    if(link.get('href') != '#'):
        link = "https://twitter.com" + link.get('href')
        all_links.add(link)
        print(link)








