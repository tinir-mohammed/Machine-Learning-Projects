'''
Group Project
Title: Tweet Like Trump!

Members:
Iman Naimi Azahari (1710682)
Syed Mohammed Khalid (1718487)
Tinir Mohamed Sadi (1712786)
Wan Maisarah Wan Azhar (1710714)

'''

from nltk.lm.preprocessing import padded_everygram_pipeline

try: # Use the default NLTK tokenizer.
    from nltk import word_tokenize, sent_tokenize
except: # Use a naive sentence tokenizer and toktok.
    import re
    from nltk.tokenize import ToktokTokenizer
    sent_tokenize = lambda x: re.split(r'(?<=[^A-Z].[.?]) +(?=[A-Z])', x)
    toktok = ToktokTokenizer()
    word_tokenize = word_tokenize = toktok.tokenize


from nltk.tokenize.treebank import TreebankWordDetokenizer

detokenize = TreebankWordDetokenizer().detokenize

def generate_sent(model, num_words, random_seed=42):
    """
    :param model: An ngram language model from `nltk.lm.model`.
    :param num_words: Max no. of words to generate.
    :param random_seed: Seed value for random.
    """
    content = []
    for token in model.generate(num_words, random_seed=random_seed):
        if token == '<s>':
            continue
        if token == '</s>':
            break
        content.append(token)
    return detokenize(content)

import pandas as pd
import io
df = pd.read_csv('Donald-Tweets!.csv')
# Dataset is now stored in a Pandas Dataframe
# print(df.head())

trump_corpus = list(df['Tweet_Text'].apply(word_tokenize))


from nltk.lm import MLE
import random 

# Preprocess the tokenized text for 2-grams language modelling
n=2
train_data, padded_sents = padded_everygram_pipeline(n, trump_corpus)

# Training a bigram model
trump_model = MLE(n)
trump_model.fit(train_data, padded_sents)

print("\nBigram Model generated tweets:")
for i in range(1,6):
	print(str(i) + ". " + str(generate_sent(trump_model, num_words=random.randint(20,140), random_seed=random.random())))


# Preprocess the tokenized text for 2-grams language modelling
n=3
train_data, padded_sents = padded_everygram_pipeline(n, trump_corpus)

# Training a trigram model
trump_model = MLE(n)
trump_model.fit(train_data, padded_sents)

print("\nTrigram Model generated tweets:")
for i in range(1,6):
	print(str(i) + ". " + str(generate_sent(trump_model, num_words=random.randint(20,140), random_seed=random.random())))


# Preprocess the tokenized text for 4-grams language modelling
n=4
train_data, padded_sents = padded_everygram_pipeline(n, trump_corpus)

# Training a trigram model
trump_model = MLE(n)
trump_model.fit(train_data, padded_sents)

print("\n4-grams Model generated tweets:")
for i in range(1,6):
	print(str(i) + ". " + str(generate_sent(trump_model, num_words=random.randint(20,140), random_seed=random.random())))