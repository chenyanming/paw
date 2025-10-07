import os
import sqlite3
from flask import Flask, request, jsonify
from flask_cors import CORS
import argparse
import requests
import sys
import subprocess
import json
import threading
import time
import signal
from datetime import datetime
import logging


class Paw:
    def __init__(self, filename, verbose=False):
        self.__dbname = filename
        if filename != ':memory:':
            os.path.abspath(filename)
        self.__conn = None
        self.cursor = None
        self.__verbose = verbose
        self.__lock = threading.Lock()
        self.__open()

    def __open(self):
        """Open database connection with retry mechanism"""
        max_retries = 5
        for attempt in range(max_retries):
            try:
                self.__conn = sqlite3.connect(self.__dbname, check_same_thread=False, timeout=30.0)
                self.__conn.isolation_level = None
                self.cursor = self.__conn.cursor()
                fields = ('word', 'exp')
                self.__fields = tuple([(fields[i], i) for i in range(len(fields))])
                self.__names = {}
                for k, v in self.__fields:
                    self.__names[k] = v
                return True
            except sqlite3.Error as e:
                logging.warning(f"Database connection attempt {attempt + 1} failed: {e}")
                if attempt == max_retries - 1:
                    raise
                time.sleep(1)

    def __ensure_connection(self):
        """Ensure database connection is alive"""
        if self.__conn is None:
            self.__open()
        else:
            try:
                # Test connection
                self.cursor.execute("SELECT 1")
            except sqlite3.Error:
                logging.warning("Database connection lost, reconnecting...")
                self.__open()
    def candidates(self):
        with self.__lock:
            self.__ensure_connection()
            try:
                with self.__conn:
                    # Define the SQL query to join the items and status tables
                    self.cursor.execute("""
                        SELECT items.word, items.exp, status.origin_path, status.note
                        FROM items
                        JOIN status ON items.word = status.word
                    """)
                    items = self.cursor.fetchall()
                    if not items:
                        return {}  # Return empty dict instead of raising exception

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
            except Exception as e:
                logging.error(f"Error fetching candidates: {e}")
                return {}
    def delete(self, word):
        with self.__lock:
            self.__ensure_connection()
            try:
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
            except Exception as e:
                logging.error(f"Error deleting word {word}: {e}")
                raise

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

SECRET_TOKEN = "your-secure-token"

# -------------------------------
# Call paw-org-protocol in Emacs
# -------------------------------
def call_paw_org_protocol(data: dict):
    """
    Calls the existing Emacs function (paw-org-protocol data)
    using emacsclient -e safely.
    """
    # Convert Python dict to JSON string, escape double quotes
    # JSON to string, escape double quotes and newlines
    json_str = json.dumps(data, ensure_ascii=False)
    json_str = json_str.replace('\\', '\\\\').replace('"', '\\"')
    # print(json_str)

    elisp_code = f'(paw-org-protocol (json-parse-string "{json_str}" :object-type \'plist :array-type \'list))'

    try:
        result = subprocess.run(
            ["emacsclient", "-e", elisp_code],
            capture_output=True,
            text=True,
            timeout=5
        )
        return result.stdout.strip()
    except Exception as e:
        return f"ERROR: {e}"

# -------------------------------
# Flask endpoint /paw
# -------------------------------
@app.route("/paw", methods=["POST"])
def paw_endpoint():
    token = request.headers.get("X-Auth-Token")
    if token != SECRET_TOKEN:
        return jsonify({"status": "error", "error": "Unauthorized"}), 401

    data = request.json
    if not data:
        return jsonify({"status": "error", "error": "No data provided"}), 400

    result = call_paw_org_protocol(data)
    return jsonify({"status": "ok", "result": result})

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

    # Setup logging
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler('paw-server.log')
        ]
    )

    # Use environment variables as fallback
    wallabag_host = host or os.getenv('WALLABAG_HOST')
    wallabag_username = username or os.getenv('WALLABAG_USERNAME')
    wallabag_password = password or os.getenv('WALLABAG_PASSWORD')
    wallabag_clientid = clientid or os.getenv('WALLABAG_CLIENTID')
    wallabag_secret = secret or os.getenv('WALLABAG_SECRET')

    if database_path:
        try:
            # Expand user home directory (~) if present
            expanded_db_path = os.path.expanduser(database_path)
            paw = Paw(expanded_db_path)
            logging.info(f"Connected to database: {expanded_db_path}")
        except Exception as e:
            logging.error(f"Failed to connect to database {database_path}: {e}")
            sys.exit(1)

    save_dir = temp_dir or os.getenv('PAW_SAVE_DIR', '/tmp')
    port = int(port or os.getenv('PAW_PORT', 5001))

    logging.info(f"Starting PAW server on port {port}")
    logging.info(f"Save directory: {save_dir}")
    if wallabag_host:
        logging.info(f"Wallabag integration enabled for: {wallabag_host}")

    # Setup graceful shutdown
    def signal_handler(sig, frame):
        logging.info("Received shutdown signal, closing database connections...")
        if paw and paw._Paw__conn:
            paw._Paw__conn.close()
        sys.exit(0)

    signal.signal(signal.SIGINT, signal_handler)
    signal.signal(signal.SIGTERM, signal_handler)

    # Use environment variable to decide server type
    server_type = os.getenv('PAW_SERVER_TYPE', 'flask')
    logging.info(f"Server type detected: {server_type}")

    if server_type == 'production':
        try:
            from waitress import serve
            logging.info("Starting server with waitress (production WSGI server)...")
            serve(app, host='0.0.0.0', port=port)
        except ImportError as e:
            logging.warning(f"waitress not available ({e}), falling back to Flask development server")
            app.run(host='0.0.0.0', port=port, threaded=True)
        except Exception as e:
            logging.error(f"Error starting waitress server ({e}), falling back to Flask development server")
            app.run(host='0.0.0.0', port=port, threaded=True)
    else:
        logging.info("Starting server with Flask development server...")
        app.run(host='0.0.0.0', port=port, threaded=True)


def main():
    """Standalone server entry point"""
    parser = argparse.ArgumentParser(description='PAW Server - Standalone Mode')
    parser.add_argument('--database', type=str,
                       default=os.getenv('PAW_DATABASE_PATH'),
                       help='Path to SQLite database file (env: PAW_DATABASE_PATH)')
    parser.add_argument('--save-dir', type=str,
                       default=os.getenv('PAW_SAVE_DIR', '/tmp'),
                       help='Directory to save files (env: PAW_SAVE_DIR)')
    parser.add_argument('--port', type=int,
                       default=int(os.getenv('PAW_PORT', 5001)),
                       help='Server port (env: PAW_PORT)')
    parser.add_argument('--wallabag-host', type=str,
                       default=os.getenv('WALLABAG_HOST'),
                       help='Wallabag host URL (env: WALLABAG_HOST)')
    parser.add_argument('--wallabag-username', type=str,
                       default=os.getenv('WALLABAG_USERNAME'),
                       help='Wallabag username (env: WALLABAG_USERNAME)')
    parser.add_argument('--wallabag-password', type=str,
                       default=os.getenv('WALLABAG_PASSWORD'),
                       help='Wallabag password (env: WALLABAG_PASSWORD)')
    parser.add_argument('--wallabag-clientid', type=str,
                       default=os.getenv('WALLABAG_CLIENTID'),
                       help='Wallabag client ID (env: WALLABAG_CLIENTID)')
    parser.add_argument('--wallabag-secret', type=str,
                       default=os.getenv('WALLABAG_SECRET'),
                       help='Wallabag client secret (env: WALLABAG_SECRET)')
    parser.add_argument('--server-type', type=str,
                       choices=['flask', 'production'],
                       default=os.getenv('PAW_SERVER_TYPE', 'flask'),
                       help='Server type to use: flask (dev) or production (waitress) (env: PAW_SERVER_TYPE)')

    args = parser.parse_args()

    # Set server type environment variable
    os.environ['PAW_SERVER_TYPE'] = args.server_type

    run_server(
        database_path=args.database,
        temp_dir=args.save_dir,
        port=args.port,
        host=args.wallabag_host,
        username=args.wallabag_username,
        password=args.wallabag_password,
        clientid=args.wallabag_clientid,
        secret=args.wallabag_secret
    )


if __name__ == '__main__':
    main()
