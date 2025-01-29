#!/usr/bin/env python3
mecab_imported = False
tokenizer = None
def segmentation(text):
    global mecab_imported, tokenizer
    try:
        import MeCab
        tokenizer = MeCab.Tagger("-Owakati")
        mecab_imported = True
    except ImportError:
        from janome.tokenizer import Tokenizer
        tokenizer = Tokenizer()
    if tokenizer:
        if mecab_imported:
            tokens = tokenizer.parse(text)
        else:
            tokens = tokenizer.tokenize(text)
    return tokens

# def segmentation(text):
#     import MeCab
#     tokenizer = MeCab.Tagger("-Owakati")
#     tokens = tokenizer.parse(text)
#     return tokens
