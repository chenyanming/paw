#!/usr/bin/env python3

import json
from janome.tokenizer import Tokenizer

def katakana_to_hiragana(text):
    return "".join(
        chr(ord(c) - 0x60) if "ァ" <= c <= "ン" else c
        for c in text
    )

def segmentation(text):
    tokenizer = Tokenizer()
    result = []
    for token in tokenizer.tokenize(text):
        base = token.base_form if token.base_form != '*' else token.surface

        if base != token.surface:
            reading = "".join(
                rt.reading if rt.reading else rt.surface
                for rt in tokenizer.tokenize(base)
            )
        else:
            reading = token.reading

        # 转换为平假名
        reading_hira = katakana_to_hiragana(reading) if reading else ""

        result.append({
            "surface": token.surface,
            "base_form": base,
            "reading": reading_hira
        })
    return json.dumps(result, ensure_ascii=False)
