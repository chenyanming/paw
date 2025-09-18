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
- `run_server`: Start the PAW server for handling annotation requests.
- `en_search`: Search in English dictionaries.
- `ja_search`: Search in Japanese dictionaries.
- `ja_sgment`: Search in Japanese segmentation.
Refer to `cli.py` for more details on these commands.
### Example Usage
2. Start the PAW server:
```sh
paw run_server --database /home/damonchan/org/paw.sqlite --save-dir /tmp/source.html --port 5001 --wallabag-host https://example.com --wallabag-username username --wallabag-password password --wallabag-clientid clientid --wallabag-secret secret
```
- `--save-dir`: The directory to save the html or the file name of the html file.

3. Perform an English search:
```sh
paw en_search /home/damonchan/org/stardict.db MATCH hello --tag "" --wordlists /home/damonchan/org/5000.csv --known-words-files /home/damonchan/org/eudic.csv,/home/damonchan/org/english.txt
```
4. Perform a Japanese search:
```sh
paw ja_search /home/damonchan/org/japanese.db MATCH 海外の大企業は「ダイバーシティ（多様性）＆インクルージョン（包括的） --tag "" --wordlist /home/damonchan/org/蓝宝书日语文法.csv --known-words-files /home/damonchan/org/japanese.txt
```
5. Perform a Japanese Segemtation:
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
6. Check language
```
paw check_language --language "english,chinese,japanese" --text "これは日本語の文です"
```
## Author
Damon Chan
## License
This project is licensed under the GNU General Public License v3.0.
