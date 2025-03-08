#!/usr/bin/env python3
import sys
import json
import argparse
import sys
import os
sys.path.append(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
def en_search(dictionary_path, search_type, word_or_sentence, tag=None, wordlists=None, known_words_files=None, oxford=None, collins=None, bnc=None, frq=None):
    import concurrent.futures
    import os
    from pathlib import Path
    dictionaries_paths = dictionary_path.split(',')
    def search_single_dict(dictionary_path):
        from paw.paw_ecdict import search
        return search(
            dictionary=dictionary_path,
            search_type=search_type,
            word_or_sentence=word_or_sentence,
            tag=tag,
            wordlists=wordlists,
            known_words_files=known_words_files,
            oxford=oxford,
            collins=collins,
            bnc=bnc,
            frq=frq
        )
    with concurrent.futures.ThreadPoolExecutor(max_workers=len(dictionaries_paths)) as executor:
        future_to_dictionary = {executor.submit(search_single_dict, dictionary_path): dictionary_path for dictionary_path in dictionaries_paths}
        data = []
        for future in concurrent.futures.as_completed(future_to_dictionary):
            dictionary_path = future_to_dictionary[future]
            try:
                data += future.result()
            except Exception as exc:
                print('%r generated an exception: %s' % (dictionary_path, exc))
    print(json.dumps(data, indent=4))

def ja_search(dictionary_path, search_type, word_or_sentence, tag=None, wordlists=None, known_words_files=None, oxford=None, collins=None, bnc=None, frq=None):
    from paw.paw_jlpt import JpDict, iterate_csv_file, iterate_other_file, process_csv_file, process_other_file
    import concurrent.futures
    import os
    from pathlib import Path
    dictionaries_paths = dictionary_path.split(',')
    def search_single_dict(dictionary_path):
        from paw.paw_jlpt import search
        return search(
            dictionary=dictionary_path,
            search_type=search_type,
            word_or_sentence=word_or_sentence,
            tags=tag,
            wordlists=wordlists,
            known_words_files=known_words_files,
        )
    search_single_dict(dictionary_path)


def run_server(database_path, save_dir, port, wallabag_host, wallabag_username, wallabag_password, wallabag_clientid, wallabag_secret):
    from paw.paw_server import run_server
    run_server(database_path, save_dir, port, wallabag_host, wallabag_username, wallabag_password, wallabag_clientid, wallabag_secret)


def ja_segment(text):
    from paw.paw_mecab import segmentation
    print(segmentation(text))

def parse_segment_arguments():
    parser = argparse.ArgumentParser(description='Segment Japanese text')
    parser.add_argument('text', type=str, help='Text to segment')
    return parser

def parse_search_arguments():
    parser = argparse.ArgumentParser(description='Search in the dictionary')
    parser.add_argument('dictionary_path', type=str, help='Path to the dictionary file')
    parser.add_argument('search_type', type=str, choices=['SENTENCE', 'WORD', 'WORDLIST', 'MATCH'], help='Type of search')
    parser.add_argument('word_or_sentence', type=str, help='The word or sentence to search for')
    parser.add_argument('--tag', type=str, help='Tags for filtering results', default='')
    parser.add_argument('--wordlists', type=str, help='Comma-separated paths to wordlist files', default=None)
    parser.add_argument('--known-words-files', type=str, help='Comma-separated paths to known words files', default=None)
    parser.add_argument('--oxford', type=str, help='Oxford parameter', default=None)
    parser.add_argument('--collins', type=str, help='Collins parameter', default=None)
    parser.add_argument('--bnc', type=str, help='BNC parameter', default=None)
    parser.add_argument('--frq', type=str, help='FRQ parameter', default=None)
    return parser
def parse_server_arguments():
    parser = argparse.ArgumentParser(description='Run Flask API')
    parser.add_argument('--database', type=str, required=False, help='Path to SQLite database file')
    parser.add_argument('--save-dir', type=str, default='.', help='Directory to save received source code')
    parser.add_argument('--port', type=str, required=True, help='Server Port')
    parser.add_argument('--wallabag-host', type=str, required=False, help='Wallabag host URL')
    parser.add_argument('--wallabag-username', type=str, required=False, help='Wallabag username')
    parser.add_argument('--wallabag-password', type=str, required=False, help='Wallabag password')
    parser.add_argument('--wallabag-clientid', type=str, required=False, help='Wallabag client ID')
    parser.add_argument('--wallabag-secret', type=str, required=False, help='Wallabag client secret')
    return parser
def main():
    if len(sys.argv) < 2:
        print("Usage: paw <command> <args>")
        sys.exit(1)
    command = sys.argv[1]
    if command == "en_search":
        parser = parse_search_arguments()
        args = parser.parse_args(sys.argv[2:])
        en_search(
            dictionary_path=args.dictionary_path,
            search_type=args.search_type,
            word_or_sentence=args.word_or_sentence,
            tag=args.tag,
            wordlists=args.wordlists,
            known_words_files=args.known_words_files,
            oxford=args.oxford,
            collins=args.collins,
            bnc=args.bnc,
            frq=args.frq
        )
    elif command == "ja_search":
        parser = parse_search_arguments()
        args = parser.parse_args(sys.argv[2:])
        ja_search(
            dictionary_path=args.dictionary_path,
            search_type=args.search_type,
            word_or_sentence=args.word_or_sentence,
            tag=args.tag,
            wordlists=args.wordlists,
            known_words_files=args.known_words_files,
        )
    elif command == "run_server":
        parser = parse_server_arguments()
        args = parser.parse_args(sys.argv[2:])  # pass only the arguments related to run_server to the parser
        wallabag_host = args.wallabag_host
        wallabag_username = args.wallabag_username
        wallabag_password = args.wallabag_password
        wallabag_clientid = args.wallabag_clientid
        wallabag_secret = args.wallabag_secret
        run_server(args.database, args.save_dir, args.port, wallabag_host, wallabag_username, wallabag_password, wallabag_clientid, wallabag_secret)
    elif command == "ja_segment":
        parser = parse_segment_arguments()
        args = parser.parse_args(sys.argv[2:])
        ja_segment(text=args.text)
    else:
        print("Unknown command.")
        sys.exit(1)
if __name__ == "__main__":
    main()
