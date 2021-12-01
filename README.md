# heretic-web-gen
Generate simple articles website from markdown files.
Index with list of articles is created automatically.

## Limitations
Images won't move to destination automatically.

It's recommended to create img/ directory in your source directory and place all images there, use relative path in every article (`[Test image](img/test.png)`) and copy images to dest_dir/html/img.

## Installing
To build and install you need [stack](https://docs.haskellstack.org/en/stable/README/).

Instructions:
```
$ git clone https://github.com/soukev/heretic-web-gen.git
$ cd heretic-web-gen 
$ stack build
$ stack install
```

## Usage
For help just run `herewg`.

```
herewg usage:
 -fn "title" src_dir dest_dir      Build new project.
 -a file_path dest_dir             Add new article to existing project.
 -u file_path dest_dir             Update article in existing project.
 -rbf file_path dest_dir           Rebuild article. Useful when article md file is outside your main dir
 -rbd src_dir dest_dir             Rebuild all articles from directory. Useful when header or footer has been changed.
```

Exapmle:
```
herewg -fn "My Website" src dst
```

### Result
In `dest_dir` you can find `html/`, `conf.json`, `footer.html.conf`, `header.html.conf` and `style.css.conf`.

* `html/` contains generated html files.
* `conf.json` contains informations about articles.
* `[footer.html, header.html, style.css].conf` contains footer, header and style. Edit however is desired.