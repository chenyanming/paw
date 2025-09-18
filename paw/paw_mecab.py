#!/usr/bin/env python3

import json
from janome.tokenizer import Tokenizer

# only use janome to do segmentation, show the base form
def segmentation(text):
    # tokenizer = Tokenizer()
    # words = [token.base_form if token.base_form != '*' else token.surface for token in tokenizer.tokenize(text)]
    # return " ".join(words)

    tokenizer = Tokenizer()
    result = []
    for token in tokenizer.tokenize(text):
        result.append({
            "surface": token.surface,            # 原始词
            "base_form": token.base_form,        # 基本形
            "reading": token.reading             # 读音（片假名）
        })
    return json.dumps(result, ensure_ascii=False)
