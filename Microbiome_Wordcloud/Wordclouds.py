from wordcloud import WordCloud 
import matplotlib.pyplot as plt
from nltk.tokenize import word_tokenize

filename="data/D1_summary.txt"
file = open(filename, 'rt')
text = file.read()
file.close()
# split into words
tokens = word_tokenize(text)
# convert to lower case
tokens = [w.lower() for w in tokens]
# remove punctuation from each word
import string
table = str.maketrans('', '', string.punctuation)
stripped = [w.translate(table) for w in tokens]
# remove remaining tokens that are not alphabetic
words = [word for word in stripped if word.isalpha()]
# filter out stop words
from nltk.corpus import stopwords
stop_words = set(stopwords.words('english'))
words = [w for w in words if not w in stop_words]


wordcloud = WordCloud(max_font_size=50,background_color="white").generate( " ".join(words))
plt.figure()
plt.imshow(wordcloud, interpolation="bilinear")
plt.axis("off")
plt.show()
image = wordcloud.to_image()
image.save("fig/"filename+".png")