#!/usr/bin/env python3

from janome.tokenizer import Tokenizer

# only use janome to do segmentation, show the base form
def segmentation(text):
    tokenizer = Tokenizer()
    words = [token.base_form if token.base_form != '*' else token.surface for token in tokenizer.tokenize(text)]
    return " ".join(words)
