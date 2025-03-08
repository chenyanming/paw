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
paw ja_segment すもももももももものうち
すもも も もも も もも の うち
```
## Author
Damon Chan
## License
This project is licensed under the GNU General Public License v3.0.
