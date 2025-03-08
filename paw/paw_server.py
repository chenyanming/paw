import os
import sqlite3
from flask import Flask, request, jsonify
from flask_cors import CORS
import argparse
import requests
import sys
class Paw:
    def __init__(self, filename, verbose=False):
        self.__dbname = filename
        if filename != ':memory:':
            os.path.abspath(filename)
        self.__conn = None
        self.cursor = None
        self.__verbose = verbose
        self.__open()
    def __open(self):
        self.__conn = sqlite3.connect(self.__dbname, check_same_thread=False)
        self.__conn.isolation_level = None
        self.cursor = self.__conn.cursor()
        fields = ('word', 'exp')
        self.__fields = tuple([(fields[i], i) for i in range(len(fields))])
        self.__names = {}
        for k, v in self.__fields:
            self.__names[k] = v
        return True
    def candidates(self):
        with self.__conn:
            # Define the SQL query to join the items and status tables
            self.cursor.execute("""
                SELECT items.word, items.exp, status.origin_path, status.note
                FROM items
                JOIN status ON items.word = status.word
            """)
            items = self.cursor.fetchall()
            if not items:
                raise Exception("No items found")

            # Process the fetched items into the desired dictionary format
            words = {
                item[0].strip('"'): {
                    "word": item[0].strip('\"'),
                    "exp": "" if item[1] is None else item[1].strip('\"'),
                    "origin_path": "" if item[2] is None else os.path.basename(item[2].strip('\"')),  # origin_path from status table
                    "note": "" if item[3] is None else item[3].strip('\"')      # note from status table
                }
                for item in items
            }
            return words
    def delete(self, word):
        word = '"' + word + '"'
        with self.__conn:
            self.cursor.execute("DELETE FROM items WHERE word=?", (word,))
            if self.cursor.rowcount == 0:
                raise Exception("Word not found")
            self.__conn.commit()
            # Add additional deletion from 'status' table if necessary
            self.cursor.execute("DELETE FROM status WHERE word=?", (word,))
            self.__conn.commit()
        return True

app = Flask(__name__)
CORS(app)
# Example usage of the parsed arguments
database = None
save_dir = None
port = None
wallabag_host = None
wallabag_username = None
wallabag_password = None
wallabag_clientid = None
wallabag_secret = None
wallabag_token = None  # This will be set after requesting a token
paw = None
@app.route('/words', methods=['GET'])
def get_words():
    try:
        words = { "wordInfos":  paw.candidates() }
        # print(words)
        return jsonify(words)
    except Exception as e:
        return jsonify({"error": str(e)}), 500


@app.route('/words', methods=['DELETE'])
def delete_word():
    data = request.json
    word = data.get('word')
    if not word:
        return jsonify({"error": "No word provided"}), 400

    try:
        paw.delete(word)
        return jsonify({"status": "success", "word": word}), 200
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/source', methods=['POST'])
def receive_source():
    global save_dir
    source_code = request.json.get('source')
    if source_code:
        try:
            # Determine if save_dir is a file or directory
            if os.path.splitext(save_dir)[1]:  # save_dir has an extension, treat it as a file
                # Ensure the directory for the file exists
                os.makedirs(os.path.dirname(save_dir), exist_ok=True)
                temp_file_path = save_dir
            else:
                # Ensure the directory exists
                os.makedirs(save_dir, exist_ok=True)
                # Use a default filename within the directory
                temp_file_path = os.path.join(save_dir, "source.html")

            with open(temp_file_path, 'w') as temp_file:
                temp_file.write(source_code)
            print(f"Received source code saved to {temp_file_path}")
            return jsonify({"status": "success", "temp_file_path": temp_file_path}), 200
        except Exception as e:
            return jsonify({"error": str(e)}), 500
    return jsonify({"error": "No source code provided"}), 400


def request_token(wallabag_host, wallabag_username, wallabag_password, wallabag_clientid, wallabag_secret):
    try:
        response = requests.post(
            f"{wallabag_host}/oauth/v2/token",
            headers={
                "User-Agent": "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36",
                "Content-Type": "application/json"
            },
            json={
                "username": wallabag_username,
                "password": wallabag_password,
                "client_id": wallabag_clientid,
                "client_secret": wallabag_secret,
                "grant_type": "password"
            }
        )
        response.raise_for_status()
        return response.json().get('access_token')
    except requests.RequestException as e:
        print(f"Error requesting token: {e}")
        return None

@app.route('/wallabag/entry', methods=['POST'])
def wallabag_insert_entry():
    global wallabag_token
    data = request.json
    url = data.get("url")
    title = data.get("title")
    content = data.get("content")
    if not url:
        return jsonify({"error": "URL is required"}), 400
    def insert_entry_with_token(token):
        try:
            response = requests.post(
                f"{wallabag_host}/api/entries.json",
                headers={
                    "User-Agent": "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.101 Safari/537.36",
                    "Authorization": f"Bearer {token}"
                },
                json={
                    "url": url,
                    "title": title,
                    "content": content,
                    "archive": 0,
                    "starred": 0,
                    "tags": ""  # If you have tags, you can set them here or modify this accordingly
                }
            )
            if response.status_code == 401:
                return None  # Token might be expired
            response.raise_for_status()
            return response.json()
        except requests.RequestException as e:
            print(f"Error inserting entry: {e}")
            return None
    # Request a new token if it's not already set
    if wallabag_token is None:
        wallabag_token = request_token(wallabag_host, wallabag_username, wallabag_password,
                                       wallabag_clientid, wallabag_secret)
    if wallabag_token is None:
        return jsonify({"error": "Failed to obtain access token"}), 500
    result = insert_entry_with_token(wallabag_token)
    if result is None:
        # Token might be expired, get a new one and retry
        wallabag_token = request_token(wallabag_host, wallabag_username, wallabag_password,
                                       wallabag_clientid, wallabag_secret)
        if wallabag_token is None:
            return jsonify({"error": "Failed to obtain access token"}), 500
        result = insert_entry_with_token(wallabag_token)
        if result is None:
            return jsonify({"error": "Failed to insert entry after refreshing token"}), 500
    return jsonify({"status": "success", "data": result}), 200

def run_server(database_path, temp_dir, port, host, username, password, clientid, secret):
    global wallabag_host, wallabag_username, wallabag_password, wallabag_clientid, wallabag_secret, paw, save_dir
    paw = Paw(database_path)
    save_dir = temp_dir
    port = port
    wallabag_host = host
    wallabag_username = username
    wallabag_password = password
    wallabag_clientid = clientid
    wallabag_secret = secret
    wallabag_token = None
    app.run(host='0.0.0.0', port=port)
