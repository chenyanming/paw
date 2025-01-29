#!/usr/bin/env python3
import sys
import time
import os
import io
import sqlite3
import re
import csv
from pathlib import Path
from collections import OrderedDict
try:
    import json
except ImportError:
    import simplejson as json

mecab_imported = False
tokenizer = None
try:
    import MeCab
    tokenizer = MeCab.Tagger("-Owakati")
    mecab_imported = True
except:
    from janome.tokenizer import Tokenizer
    tokenizer = Tokenizer()

class JpDict (object):
    def __init__ (self, filename, verbose = False):
        self.__dbname = filename
        if filename != ':memory:':
            os.path.abspath(filename)
        self.__conn = None
        self.__verbose = verbose
        self.__open()
    def __open (self):
        sql = '''
        CREATE TABLE IF NOT EXISTS "jlpt_table" (
            "id" INTEGER,
            "jmdict_seq" INTEGER,
            "kanji" TEXT,
            "kana" TEXT,
            "waller_definition" TEXT,
            "origin" TEXT,
            "original" TEXT,
            "level" TEXT
        );
        '''
        self.__conn = sqlite3.connect(self.__dbname, isolation_level = "IMMEDIATE")
        self.__conn.isolation_level = "IMMEDIATE"
        sql = '\n'.join([ n.strip('\t') for n in sql.split('\n') ])
        sql = sql.strip('\n')
        self.__conn.executescript(sql)
        self.__conn.commit()
        fields = ( 'id', 'jmdict_seq', 'kanji', 'kana', 'waller_definition', 'origin', 'original', 'level' )
        self.__fields = tuple([(fields[i], i) for i in range(len(fields))])
        self.__names = { }
        for k, v in self.__fields:
            self.__names[k] = v
        self.__enable = self.__fields[3:]
        return True
    def close (self):
        if self.__conn:
            self.__conn.close()
        self.__conn = None
    def __del__ (self):
        self.close()
    def out (self, text):
        if self.__verbose:
            print(text)
        return True
    def query (self, key):
        c = self.__conn.cursor()
        record = None
        if isinstance(key, str):
            c.execute('select * from jlpt_table where kanji = ? or kana = ?', (key,key))
        else:
            return None
        record = c.fetchone()
        return self.__record2obj(record)
    def query_batch(self, words, tags=None):
        original_words = words
        sql = 'select * from jlpt_table where '
        if words is None:
            return None
        if not words:
            return []
        querys = []
        params = ', '.join(['?' for _ in words])  # Create '?' placeholders for each value
        if isinstance(words[0], int):
            querys.append(f'id in ({params})')
        elif words[0] is not None:
            querys.append(f'(kanji in ({params})')
            querys.append(f'OR kana in ({params}))')
            words.extend(words)
        sql = sql + ' '.join(querys)

        querys = []
        if tags is not None:
            params = ', '.join(['?' for _ in tags])  # Create '?' placeholders for each value
            querys.append(f' AND level in ({params})')
            words.extend(tags)
        sql = sql + ' '.join(querys) + ';'
        # print(sql)
        # print(words)
        query_objs = []
        query_kanji = {}
        query_kana = {}
        query_id = {}
        c = self.__conn.cursor()
        c.execute(sql, tuple(words))
        rows = c.fetchall()
        for row in rows:
            # print(row)
            obj = self.__record2obj(row)
            query_objs.append(obj)
            query_id[obj['id']] = obj
            query_kanji[obj['kanji']] = obj
            query_kana[obj['kana']] = obj
            # print(query_kana)


        # print(original_words)
        # In this code, =sort_key= will return the first index at which each
        # word from =query_objs= appears in the =words= list or =len(words)= for
        # an item not in this list. The =sorted()= function will then sort =query_objs= based on these indices.
        def sort_key(obj):
            indices = [i for i, word in enumerate(original_words) if word in [obj['kanji'], obj['kana']]]
            return (min(indices) if indices else len(words))
        results = sorted(query_objs, key=sort_key)

        # print(query_objs)
        # print(len( rows ))
        # print(len(  query_objs  ))
        # print(len(query_kanji))
        # print(len( query_kana ))

        # reorder based on the original words sequence
        # results = []
        # for word in original_words:
        #     if isinstance(word, int):
        #         results.append(query_id.get(word, None))
        #     elif word is not None:
        #         if query_kanji.get(word, None) is not None:
        #             # print(query_kanji.get(word, None))
        #             results.append(query_kanji.get(word, None))
        #         elif query_kana.get(word, None) is not None:
        #             # print(query_kana.get(word, None))
        #             results.append(query_kana.get(word, None))
        #         else:
        #             # TODO handle the unknown words?
        #             pass
        #     else:
        #         results.append(None)
        # print(results)
        return results
    # 数据库记录转化为字典
    def __record2obj (self, record):
        if record is None:
            return None
        word = {}
        for k, v in self.__fields:
            # print(k)
            word[k] = record[v]
        # if word['waller_definition']:
        #     text = word['waller_definition']
        #     try:
        #         obj = json.loads(text)
        #     except:
        #         obj = None
        #     word['waller_definition'] = obj
        return word

    def load_text (self, filename, encoding = None):
        content = None
        try:
            content = open(filename, 'rb').read()
        except:
            return None
        if content[:3] == b'\xef\xbb\xbf':
            text = content[3:].decode('utf-8')
        elif encoding is not None:
            text = content.decode(encoding, 'ignore')
        else:
            text = None
            guess = [sys.getdefaultencoding(), 'utf-8']
            if sys.stdout and sys.stdout.encoding:
                guess.append(sys.stdout.encoding)
            for name in guess + ['gbk', 'ascii', 'latin1']:
                try:
                    text = content.decode(name)
                    break
                except:
                    pass
            if text is None:
                text = content.decode('utf-8', 'ignore')
        return text

def detect_delimiter(file_path):
    delimiters = [',', '\t'] # Add other delimiters if needed
    with open(file_path, 'r') as file:
        header = file.readline()
        for delimiter in delimiters:
            if delimiter in header:
                return delimiter
        return ','  # Default delimiter

def process_csv_file(file_path):
    words = set()
    delimiter = detect_delimiter(file_path)
    with open(file_path, 'r') as file:
        reader = csv.reader(file, delimiter=delimiter)
        for row in reader:
            words.add(row[0].lower())  # or any other column depends on your file
    return words

def process_other_file(file_path):
    words = set()
    with open(file_path, 'r') as file:
        for line in file:
            word = line.strip()  # You might need to adapt this to fit the format of the text file
            words.add(word.lower())
    return words

def iterate_csv_file(file_path):
    rows = []
    delimiter = detect_delimiter(file_path)
    with open(file_path, 'r') as file:
        reader = csv.reader(file, delimiter=delimiter)
        for row in reader:
            row.append(Path(file_path).stem)  # Add the file path to the row
            rows.append(row)
    return rows

def iterate_other_file(file_path):
    rows = []
    with open(file_path, 'r') as file:
        for line in file:
            row = [line.strip()]  # You might need to adapt this to fit the format of the text file
            row.append(Path(file_path).stem)  # Add the file path to the row
            rows.append(row)
    return rows

def search(dictionary, search_type, word_or_sentence, tags=None, wordlists=None, known_words_files=None):
    db = os.path.abspath(dictionary)
    jd = JpDict(db, False)
    # print(sentence)
    if search_type == 'WORDLIST':
        sentence = word_or_sentence
        if os.path.exists(sentence):
            sentence = jd.load_text(sentence)

        sentence = sentence.replace(" ", "") # remove all spaces
        # print(sentence)

        rows = []

        wordlists_paths = None
        if wordlists != '' and wordlists is not None:
            wordlists_paths = wordlists.split(',')
        # print(wordlists_paths)

        if wordlists_paths:
            # load wordlists one by one
            for wordlist in wordlists_paths:
                full_path = os.path.expanduser(wordlist)
                if os.path.exists(full_path):
                    _, file_extension = os.path.splitext(full_path)
                    if file_extension.lower() == '.csv':
                        rows += iterate_csv_file(full_path)
                    else:
                        rows += iterate_other_file(full_path)
        # print(rows)

        query_word = {}
        for row in rows:
            word = row[0] # the first column is the word
            if sentence == word: # exact match
                if len(row) > 1:
                    data = query_word.get(word, None)
                    if data is None:
                        query_word[word] = {'kanji': word, 'waller_definition': "-->" + row[-1] + "\n" + "-->" + word + "\n" + "\n".join(row[1:-1])}
                    else:
                        query_word[word] = {'kanji': word, 'waller_definition': "-->" + row[-1] + "\n" + "-->" + word + "\n" + '\n'.join(row[1:-1]) + "\n\n" + data['waller_definition']}
                else:
                    if query_word.get(word, None) is None:
                        result = jd.query(word)
                        if result is None:
                            query_word[word] = {'kanji': word, 'waller_definition': ''}
                        else:
                            query_word[word] = result
        # fallback and use stardict
        if query_word == {}:
            result = jd.query(sentence)
            if result is None:
                query_word[word] = {'kanji': word, 'waller_definition': ''}
            else:
                query_word[word] = result

        results = []
        for word in query_word:
            results.append(query_word.get(word, None))
        print(json.dumps(results, indent=4))
    elif search_type == 'WORD':
        word = word_or_sentence
        word = word.replace(" ", "") # remove all spaces
        results = []
        result = jd.query(word)
        results.append(result)
        if results:
            print(json.dumps(results, indent=4))
        else:
            print("[]")
    elif search_type == 'MATCH':
        sentence = word_or_sentence
        if os.path.exists(sentence):
            sentence = jd.load_text(sentence)

        sentence = sentence.replace(" ", "") # remove all spaces
        # print(sentence)

        rows = []

        wordlists_paths = None
        if wordlists != '' and wordlists is not None:
            wordlists_paths = wordlists.split(',')
        # print(wordlists_paths)

        if wordlists_paths:
            for wordlist in wordlists_paths:
                full_path = os.path.expanduser(wordlist)
                if os.path.exists(full_path):
                    _, file_extension = os.path.splitext(full_path)
                    if file_extension.lower() == '.csv':
                        rows += iterate_csv_file(full_path)
                    else:
                        rows += iterate_other_file(full_path)
        # print(rows)


        known_words_files_paths = None
        if known_words_files != '' and known_words_files is not None:
            known_words_files_paths = known_words_files.split(',')
        # print(tag, oxford, collins, bnc, frq, known_words_files_paths)


        known_words = set()
        if known_words_files_paths:
            for file_path in known_words_files_paths:
                if os.path.exists(file_path):
                    _, file_extension = os.path.splitext(file_path)
                    if file_extension.lower() == '.csv':
                        known_words.update(process_csv_file(file_path))
                    else:
                        known_words.update(process_other_file(file_path))
        # print(known_words)

        query_word = {}
        for row in rows:
            word = row[0]
            if word not in known_words:
                if sentence.find(word) != -1:
                    if len(row) > 1:
                        data = query_word.get(word, None)
                        if data is None:
                            query_word[word] = {'kanji': word, 'waller_definition': "-->" + row[-1] + "\n" + "-->" + word + "\n" + "\n".join(row[1:-1])}
                        else:
                            query_word[word] = {'kanji': word, 'waller_definition': "-->" + row[-1] + "\n" + "-->" + word + "\n" + '\n'.join(row[1:-1]) + "\n\n" + data['waller_definition']}
                    else:
                        if query_word.get(word, None) is None:
                            result = jd.query(word)
                            if result is None:
                                query_word[word] = {'kanji': word, 'waller_definition': ''}
                            else:
                                query_word[word] = result

        # print(query_word)
        results = []
        for word in query_word:
            results.append(query_word.get(word, None))
        print(json.dumps(results, ensure_ascii=False, indent=4))

        # return [ sd.query(word) for word in words if re.search(r'\b' + word + r'\b', sentence) ]
        # return [ word for row in words for word in row if word in sentence ]
    else:
        sentence = word_or_sentence
        if os.path.exists(sentence):
            sentence = jd.load_text(sentence)

        sentence = sentence.replace(" ", "") # remove all spaces
        # print(sentence)

        known_words = set()
        if known_words_files:
            for file_path in known_words_files:
                if os.path.exists(file_path):
                    _, file_extension = os.path.splitext(file_path)
                    if file_extension.lower() == '.csv':
                        known_words.update(process_csv_file(file_path))
                    else:
                        known_words.update(process_other_file(file_path))

        if tokenizer:
            if mecab_imported:
                words = tokenizer.parse(sentence).split()
            else:
                tokens = tokenizer.tokenize(sentence)
                words = [token.surface for token in tokens]

        hiragana = [chr(i) for i in range(12353, 12436)]
        katakana = [chr(i) for i in range(12449, 12533)]
        stopwords = hiragana + katakana + list(known_words)
        punctuations = ["。", "、", "「", "」", "『", "』", "（", "）", "【", "】", "！",
                                "？", "・", "．", "，", "…", "ー", "“", "”", '""', "''"]
        stopwords += punctuations
        filtered_words = []
        for word in words:
            if word not in stopwords:
                filtered_words.append(word)
        words = filtered_words

        # print(words)
        result = []
        # for word in words:
        #     found_word = jd.query(word)
        #     if found_word:
        #         result.append(found_word)
        # print(json.dumps(result, ensure_ascii=False, indent=4))

        terms_per_query = 500 # sqlite max depth is 1000
        max_i = int(len(words) / terms_per_query) + 1
        for i in range(max_i):
            start = i * terms_per_query
            end = (i + 1) * terms_per_query
            end = min(end, len(words))
            # pr.enable()
            batch_results = jd.query_batch(words[start:end], tags)
            # batch_results = sd.query_batch(words[start:end])
            # pr.disable()
            # pr.print_stats()
            result += batch_results
        # print(len(result))
        print(json.dumps(result, ensure_ascii=False, indent=4))
        # print(json.dumps(result))

        # print(words)
        # print(json.dumps(sd.query_batch(re.split('[ ,.;!:?]+', sentence))))
        # batch query
        # print(json.dumps(sd.query_batch(words)))
