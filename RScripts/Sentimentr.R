

library(sentimentr)
library(lexicon)
hash.sentiment <-
  sentiment(
    tweets.text,
    polarity_dt = lexicon::hash_sentiment_huliu,
    valence_shifters_dt = lexicon::hash_valence_shifters,
    hyphen = "",
    amplifier.weight = 0.8,
    n.before = 5,
    n.after = 2,
    question.weight = 1,
    adversative.weight = 0.85,
    missing_value = 0
  )
# Amplifiers increase polarity by 1+0.8;
# Valence Shifters: A data.table of valence shifters that can alter a polarized word’s meaning and an integer key for negators (1), amplifiers(2), de-amplifiers (3) and (4) adversative conjunctions
#n.before and n.after: Number of words to consider as valence shifters before and after the polarized word
# hypen: The character string to replace hyphens with.sugar-free’ becomes ’sugarfree’. Setting hyphen = " " would result in a space
# polarized_dt: A data.table of positive/negative words and weights
# adversative.weigth: Up- or downweigthing by adversative conjunctions as "but", "although","however".
#Weigthing of Questions: A 0 corresponds with the belief that questions (pure questions) are not polarized
# element_id - The id number of the original vector passed to sentiment
# sentence_id - The id number of the sentences within each element_id
# word_count - Word count
# sentiment - Sentiment/polarity score


View(hash.sentiment)
