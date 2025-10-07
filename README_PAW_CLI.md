# paw (point-and-write)
`emacs-paw` is a command line tool work with [paw.el](https://github.com/chenyanming/paw).

## Dependencies and Installation
### Python Dependencies
Install emacs-paw
```sh
pip install emacs-paw
```

Install:
```sh
python -m nltk.downloader stopwords
python -m nltk.downloader punkt
```

## Usage
### Command Line Interface
The `paw` command line tool is designed to work with `paw.el`, providing several commands to facilitate the setup and operation of the Emacs plugin. The available commands are:
- `run_server`: Start the PAW server for handling annotation requests (designed for Emacs integration).
- `server`: Start the PAW server in standalone mode with enhanced features (production server support, environment variables).
- `en_search`: Search in English dictionaries.
- `ja_search`: Search in Japanese dictionaries.
- `ja_segment`: Search in Japanese segmentation.
- `check_language`: Detect language of given text.
Refer to `cli.py` for more details on these commands.
### Example Usage

#### 1. Start the PAW server (Original Emacs integration mode):
```sh
paw run_server --database /home/damonchan/org/paw.sqlite --save-dir /tmp/source.html --port 5001 --wallabag-host https://example.com --wallabag-username username --wallabag-password password --wallabag-clientid clientid --wallabag-secret secret
```

#### 2. Start the PAW server (Standalone mode with enhanced features):
```sh
# Using command line arguments
paw server --database /home/damonchan/org/paw.sqlite --save-dir /tmp/ --port 5001 --server-type production --wallabag-host https://example.com --wallabag-username username --wallabag-password password --wallabag-clientid clientid --wallabag-secret secret

# Using environment variables (recommended for production)
export PAW_DATABASE_PATH="/home/damonchan/org/paw.sqlite"
export PAW_SAVE_DIR="/tmp/"
export PAW_PORT="5001"
export PAW_SERVER_TYPE="production"
export WALLABAG_HOST="https://example.com"
export WALLABAG_USERNAME="your_username"
export WALLABAG_PASSWORD="your_password"
export WALLABAG_CLIENTID="your_client_id"
export WALLABAG_SECRET="your_client_secret"

paw server
```

#### 3. Run server with uvicorn directly (production deployment):
```sh
# Create a shell script run_paw_server.sh:
#!/bin/bash
export PAW_DATABASE_PATH="/path/to/your/paw.sqlite"
export PAW_SAVE_DIR="/tmp/"
export PAW_PORT="5001"
export WALLABAG_HOST="https://your-wallabag.com"
export WALLABAG_USERNAME="username"
export WALLABAG_PASSWORD="password"
export WALLABAG_CLIENTID="clientid"
export WALLABAG_SECRET="secret"

cd /path/to/paw/
python -m paw.paw_server

# Make it executable and run
chmod +x run_paw_server.sh
./run_paw_server.sh
```

**Server Options:**
- `--database`: Path to SQLite database file (env: PAW_DATABASE_PATH)
- `--save-dir`: Directory to save files (env: PAW_SAVE_DIR)
- `--port`: Server port (env: PAW_PORT, default: 5001)
- `--server-type`: Server type - 'flask' or 'production' (env: PAW_SERVER_TYPE, default: flask)
- `--wallabag-*`: Wallabag configuration (env: WALLABAG_HOST, WALLABAG_USERNAME, etc.)

#### 4. Perform an English search:
```sh
paw en_search /home/damonchan/org/stardict.db MATCH hello --tag "" --wordlists /home/damonchan/org/5000.csv --known-words-files /home/damonchan/org/eudic.csv,/home/damonchan/org/english.txt
```
#### 5. Perform a Japanese search:
```sh
paw ja_search /home/damonchan/org/japanese.db MATCH 海外の大企業は「ダイバーシティ（多様性）＆インクルージョン（包括的） --tag "" --wordlist /home/damonchan/org/蓝宝书日语文法.csv --known-words-files /home/damonchan/org/japanese.txt
```
#### 6. Perform a Japanese Segmentation:
```
paw ja_segment 実在の女性を骨抜きにしたオスたちの話だけを紹介しており
[
  {
    "surface": "実在",
    "base_form": "実在",
    "reading": "ジツザイ"
  },
  {
    "surface": "の",
    "base_form": "の",
    "reading": "ノ"
  },
  {
    "surface": "女性",
    "base_form": "女性",
    "reading": "ジョセイ"
  },
  {
    "surface": "を",
    "base_form": "を",
    "reading": "ヲ"
  },
  {
    "surface": "骨抜き",
    "base_form": "骨抜き",
    "reading": "ホネヌキ"
  },
  {
    "surface": "に",
    "base_form": "に",
    "reading": "ニ"
  },
  {
    "surface": "し",
    "base_form": "する",
    "reading": "シ"
  },
  {
    "surface": "た",
    "base_form": "た",
    "reading": "タ"
  },
  {
    "surface": "オス",
    "base_form": "オス",
    "reading": "オス"
  },
  {
    "surface": "たち",
    "base_form": "たち",
    "reading": "タチ"
  },
  {
    "surface": "の",
    "base_form": "の",
    "reading": "ノ"
  },
  {
    "surface": "話",
    "base_form": "話",
    "reading": "ハナシ"
  },
  {
    "surface": "だけ",
    "base_form": "だけ",
    "reading": "ダケ"
  },
  {
    "surface": "を",
    "base_form": "を",
    "reading": "ヲ"
  },
  {
    "surface": "紹介",
    "base_form": "紹介",
    "reading": "ショウカイ"
  },
  {
    "surface": "し",
    "base_form": "する",
    "reading": "シ"
  },
  {
    "surface": "て",
    "base_form": "て",
    "reading": "テ"
  },
  {
    "surface": "おり",
    "base_form": "おる",
    "reading": "オリ"
  }
]
```
surface for segmentation, base_form for dictionary checking, reading for online sound service.
#### 7. Check language:
```
paw check_language --languages "english,chinese,japanese" --text "これは日本語の文です"
```

## Server Deployment

### Production Deployment with Waitress
For production deployment, it's recommended to use the standalone server mode with waitress:

```sh
# Install waitress first
pip install waitress

# Set environment variables for production
export PAW_DATABASE_PATH="/path/to/your/paw.sqlite"
export PAW_SAVE_DIR="/var/www/paw/uploads/"
export PAW_PORT="5001"
export PAW_SERVER_TYPE="production"
export WALLABAG_HOST="https://your-wallabag.com"
export WALLABAG_USERNAME="your_username"
export WALLABAG_PASSWORD="your_password"
export WALLABAG_CLIENTID="your_client_id"
export WALLABAG_SECRET="your_client_secret"

# Run the server
paw server
```

### Features
- **Enhanced Stability**: Improved error handling and automatic database reconnection
- **Environment Variable Support**: All configuration can be done via environment variables
- **Production Server Support**: Better performance with waitress WSGI server
- **Graceful Shutdown**: Proper cleanup on server shutdown
- **Logging**: Comprehensive logging to both console and file
- **Thread Safety**: Safe concurrent access to database

### Environment Variables
| Variable | Description | Default |
|----------|-------------|---------|
| `PAW_DATABASE_PATH` | Path to SQLite database | None |
| `PAW_SAVE_DIR` | Directory to save files | `/tmp` |
| `PAW_PORT` | Server port | `5001` |
| `PAW_SERVER_TYPE` | Server type (flask/production) | `flask` |
| `WALLABAG_HOST` | Wallabag server URL | None |
| `WALLABAG_USERNAME` | Wallabag username | None |
| `WALLABAG_PASSWORD` | Wallabag password | None |
| `WALLABAG_CLIENTID` | Wallabag client ID | None |
| `WALLABAG_SECRET` | Wallabag client secret | None |

## Author
Damon Chan
## License
This project is licensed under the GNU General Public License v3.0.
