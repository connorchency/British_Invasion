from bs4 import BeautifulSoup
import urllib, re, pandas as pd

first = urllib.urlopen('https://en.wikipedia.org/wiki/List_of_British_Invasion_artists')
soup = BeautifulSoup(first, "lxml")
l = soup.find_all('div', 'mw-content-ltr', 'ltr')[0].find_all('ul')[0].find_all('li')

artists_1st = []
for i in l:
    if i.find_all('b'):
        # print i.find_all('b')[0].find_all('a')[0].text
        artists_1st.append(i.find_all('b')[0].find_all('a')[0].text)
    else:
        # print i.find_all('a')[0].text
        artists_1st.append(i.find_all('a')[0].text)

second = urllib.urlopen('https://en.wikipedia.org/wiki/List_of_Second_British_Invasion_artists')
soup2 = BeautifulSoup(second, "lxml")
l2 = soup2.find_all('table')[1].find_all('ul')

artists_2nd = []
for u in l2:
    for p in u.find_all('li'):#[0].find_all('a')[0].text
        artists_2nd.append(p.find_all('a')[0].text)
        #print p.find_all('a')[0].text


artists_2nd
#Some weird things in the bottom but the above loop most conveniently produces the outcome we want
artists_2nd = artists_2nd[:-3]


# save data
first = pd.DataFrame({'artist': artists_1st})
second = pd.DataFrame({'artist': artists_2nd})
first.to_csv('Data/List_of_British_Invasion_artists.csv')
second.to_csv('Data/List_of_Second_British_Invasion_artists.csv')
