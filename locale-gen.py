#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import re
import configparser
import gzip
import shutil
import json
import argparse
from fontTools.ttLib import TTFont

UCD_DIR='./src/ucd'
ORIGINAL_FILE = f'{UCD_DIR}/UTF-8'

class UCD:
    def __init__(self, ucd_dir):
        self.ucd_dir = ucd_dir
        self.ucd, self.ucd_range = self.load_unicode_data()
        self.eaw = self.load_eaw()
        self.group = {}
        self.group['private'] = self.load_private()
        # Private領域を除外したamb list
        self.group['amb_ex_priv'] = self.load_amb()
        self.group['amb'] = self.group['amb_ex_priv'] + self.group['private']
        self.group['nerdfont'] = self.load_nerdfont()
        self.group['reference_mark'] = [0x203B]
        self.group['roman_numeral'] = range(0x2160, 0x2182 + 1)
        # 0x24EA と 0x1F10C は neutral だが...
        self.group['circled_digit'] = \
            list(range(0x2460, 0x2473 + 1)) + [0x24EA] + \
            list(range(0x2776, 0x2793 + 1)) + \
            list(range(0x24EB, 0x24F4 + 1)) + [0x24FF] + \
            list(range(0x24F5, 0x24FE + 1)) + \
            list(range(0x3248, 0x324F + 1)) + \
            [0x1F10C]
        self.group['parenthesized_digit'] = range(0x2474, 0x2487 + 1)
        self.group['digit_full_stop'] = list(range(0x2488, 0x249B + 1)) + [0x1F100]
        self.group['digit_comma'] = list(range(0x1F101, 0x1F10A + 1))
        self.group['parenthesized_latin'] = list(range(0x249C, 0x24B5 + 1)) + list(range(0x1F110, 0x1F129 + 1))
        self.group['circled_latin'] = list(range(0x24B6, 0x24E9 + 1)) + list(range(0x1F150, 0x1F169 + 1)) + list(range(0x1F12A, 0x1F12D + 1))
        self.group['squared_latin'] = list(range(0x1F130, 0x1F14F + 1)) + \
            list(range(0x1F170, 0x1F1AC + 1)) + \
            list(range(0x1F1E6, 0x1F1FF + 1))
        self.group['box_drawing'] = range(0x2500, 0x257F + 1)
        self.group['block_elements'] = range(0x2580, 0x259F + 1)
        self.group['geometric_shapes'] = range(0x25A0, 0x25FF + 1)
        self.group['miscellaneous_symbols'] = range(0x2600, 0x26FF + 1)
        self.group['progress_bar'] = range(0xEE00, 0xEE0B + 1)

    def get_block(self, name):
        return self.blocks.get(name)

    def get_name(self, code):
        data = self.ucd.get(code)
        if data:
            return data['name']
        for data_range in self.ucd_range:
            if data_range['first'] <= code <= data_range['last']:
                return data_range['name']
        return None

    def load_unicode_data(self):
        ucd = {}
        ucd_range = []
        path = f'{self.ucd_dir}/UnicodeData.txt'
        with open(path, mode='r') as f:
            for line in f:
                row = line.split(';')
                if row[1].endswith(', First>'):
                    first = row
                    continue
                if row[1].endswith(', Last>'):
                    ucd_range.append({
                        'first': int(first[0], 16),
                        'last': int(row[0], 16),
                        'name': first[1].removeprefix('<').removesuffix(', First>'),
                        'category': first[2],
                        'combining': first[3],
                        'comment': first[11],
                    })
                    continue
                ucd[int(row[0], 16)] = {
                    'name': row[1],
                    'category': row[2],
                    'combining': row[3],
                    'comment': row[11],
                }
        return ucd, ucd_range

    # East Asian Widthマップを生成
    def load_eaw(self):
        path = f'{self.ucd_dir}/EastAsianWidth.txt'
        ret = {}
        line_re = re.compile(r'([0-9A-Fa-f\.]+)\s*;\s*(\w+)\s+#\s+(.*)')
        f = open(path)
        for line in f:
            if line.strip() == '' or line.startswith('#'):
                continue
            match = line_re.match(line)
            if not match:
                print(f'unexpected format: {line}', file=sys.stderr)
                continue
            (code_or_range, eaw, comment) = match.groups()
            if '.' in code_or_range:
                # range code
                (first, last) = tuple(code_or_range.split('..'))
                code = int(first, 16)
            else:
                # single code
                code = int(code_or_range, 16)

            if '.' in code_or_range:
                for i in range(int(first, 16), int(last, 16) + 1):
                    ret[i] = eaw
            else:
                ret[code] = eaw
        f.close()
        return ret

    # EAW=Aのリスト、だがamb.txtを生成する都合上プライベート領域を除外している
    def load_amb(self):
        ret = []
        for c, w in self.eaw.items():
            if w != 'A':
                continue
            # exclude Combining Diacritical Marks
            if 0x0300 <= c <= 0x036F:
                continue
            # exclude Variation Selectors
            if 0xFE00 <= c <= 0xFE0F:
                continue
            # exclude Variation Selectors Supplement
            if 0xE0100 <= c <= 0xE01EF:
                continue
            # exclude Private Use Area
            if 0xE000 <= c <= 0xF8FF:
                continue
            # exclude Supplementary Private Use Area-A
            if 0xF0000 <= c <= 0xFFFFF:
                continue
            # exclude Supplementary Private Use Area-B
            if 0x100000 <= c <= 0x10FFFD:
                continue
            ret.append(c)
        return ret

    def load_private(self):
        ret = []
        # Private Use Area
        ret.extend(range(0xE000, 0xF8FF + 1))
        # Supplementary Private Use Area-A
        ret.extend(range(0xF0000, 0xFFFFF + 1))
        # Supplementary Private Use Area-B
        ret.extend(range(0x100000, 0x10FFFD + 1))
        return ret

    # nerdfontは基本的にプライベート領域 U+E000-U+F8FF, U+F0000-U+F1FFF を利用
    # 正確なコードポイントはこちら
    # https://github.com/ryanoasis/nerd-fonts/wiki/Glyph-Sets-and-Code-Points
    # 正確には23fb - 23fe, 2665, 26a1, 2b58 などの非プライベート領域を含んでいるので注意
    def load_nerdfont(self):
        ret = []
        with open('src/nerdfont/list.txt') as f:
            for line in f:
                if line.startswith('#'):
                    continue
                code = int(line, 16)
                ret.append(code)
        return ret

class FontWidthScanner:
    def __init__(self, font_paths):
        self.fonts = []
        for path in font_paths:
            try:
                font = TTFont(path)
                cmap = font.getBestCmap()

                if 'hmtx' not in font:
                    print(f"Warning: {path} has no hmtx table. Skipping.", file=sys.stderr)
                    continue

                hmtx = font['hmtx']

                # そのフォント自身の基準幅（'H' または先頭のグリフの幅）を計算
                if 0x0048 in cmap:
                    base_width = hmtx[cmap[0x0048]][0]
                elif 0x0020 in cmap:
                    base_width = hmtx[cmap[0x0020]][0]
                else:
                    base_width = next(iter(hmtx.metrics.values()))[0]

                self.fonts.append({
                    'path': path,
                    'cmap': cmap,
                    'hmtx': hmtx,
                    'base_width': base_width
                })
            except Exception as e:
                print(f"Warning: Failed to load font {path}: {e}", file=sys.stderr)

        if not self.fonts:
            import sys
            print("Error: No valid fonts loaded. Exiting.", file=sys.stderr)
            sys.exit(1)

    def get_width(self, code):
        """
        優先順位に従ってフォントを検索し、文字幅（1 または 2）を返します。
        """
        for font_info in self.fonts:
            if code in font_info['cmap']:
                glyph_name = font_info['cmap'][code]
                advance = font_info['hmtx'][glyph_name][0]

                # フォント自身の基準幅
                base_width = font_info['base_width']

                # 基準フォントの1セル幅（比率）と比較する
                if advance <= base_width * 1.2:
                    return 1
                else:
                    return 2

        return None

def main():
    # 引数でフォントのリストを受け取れるようにする
    parser = argparse.ArgumentParser(description="Generate Font Width List")
    parser.add_argument('--fonts', nargs='+', required=True,
                        help="List of font files in fallback order (e.g., Latin.ttf JP.ttf Emoji.ttf)")
    # フォールバック幅の指定引数を追加
    parser.add_argument('--fallback-amb', type=int, choices=[1, 2], default=1,
                        help="Width for Ambiguous characters if glyph is missing (default: 1)")
    parser.add_argument('--fallback-emoji', type=int, choices=[1, 2], default=2,
                        help="Width for Emoji if glyph is missing (default: 2)")
    parser.add_argument('--fallback-pua', type=int, choices=[1, 2], default=2,
                        help="Width for PUA/NerdFonts if glyph is missing (default: 2)")
    args = parser.parse_args()
    ucd = UCD(UCD_DIR)
    scanner = FontWidthScanner(args.fonts)

    print("Building comprehensive width map...")
    emoji_dict = load_emoji(f'{UCD_DIR}/emoji-data.txt')
    width_map, target_codes = build_comprehensive_width_map(ucd, scanner, emoji_dict, args)

    measured_narrow = []
    measured_wide = []

    for code in sorted(target_codes):
        # build_comprehensive_width_map で間引かれたASCII文字(U+00FF以下で幅1)の KeyError を防ぐ
        if code in width_map:
            if width_map[code] == 1:
                measured_narrow.append(code)
            elif width_map[code] == 2:
                measured_wide.append(code)

    # test ディレクトリに出力
    generate_list('test/measured_narrow.txt', measured_narrow, ucd)
    generate_list('test/measured_wide.txt', measured_wide, ucd)

    width_list = range_compress(width_map)

    generate_locale(width_list)
    generate_elisp(width_list)
    generate_wezterm(width_list)
    print('Generation complete.')

def generate_list(path, code_list, ucd):
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w', encoding='UTF-8')

    for code in code_list:
        name = ucd.get_name(code)
        c = chr(code)
        print(f'[{c}] U+{code:04X} {name}', file=out)
    print('done')

# 連続したコードポイントをレンジ形式にする
def range_compress(width_map):
    ret = []
    code_list = sorted(width_map.keys())
    start = code_list[0]
    end = code_list[0]
    end_width = width_map[end]
    for code in code_list[1:]:
        width = width_map[code]
        if code == end + 1 and width == end_width:
            end = code
            end_width = width
        else:
            ret.append((start, end, end_width))
            start = code
            end = code
            end_width = width
    ret.append((start, end, end_width))
    return ret

def load_emoji(fn):
    emoji = {}
    line_re = re.compile(r'([0-9A-Fa-f\.]+)\s+;\s+(\w+)\s+(.*)')
    with open(fn) as f:
        for line in f:
            if line.startswith('#'):
                continue
            match = line_re.match(line)
            if not match:
                continue
            (code_or_range, prop, comment) = match.groups()
            if prop != 'Emoji_Presentation':
                continue
            if '.' in code_or_range:
                 (start, end) = tuple(code_or_range.split('..'))
                 for code in range(int(start, 16), int(end, 16) + 1):
                     emoji[code] = True
            else:
                emoji[int(code_or_range, 16)] = True
    return emoji

def build_comprehensive_width_map(ucd, scanner, emoji_dict, args):
    """
    Ambiguous 領域 + どうしてこうなった領域のみを実測し、
    Emoji と PUA は指定された fallback 値で決め打ちにして統合
    """
    width_map = {}

    pure_amb = set(ucd.group['amb_ex_priv'])
    rescue_codes = set()
    # EAW の定義が実態と乖離しやすい「どうしてこうなった」文字群を強制レスキュー
    rescue_codes.update(ucd.group['reference_mark'])         # ※
    rescue_codes.update(ucd.group['roman_numeral'])          # Ⅰ, ⅱ, ⅺ, ⅻ
    rescue_codes.update(ucd.group['circled_digit'])          # ①, ⑳, ⓪
    rescue_codes.update(ucd.group['parenthesized_digit'])    # ⑴, ⒇
    rescue_codes.update(ucd.group['digit_full_stop'])        # ⒈, ⒛
    rescue_codes.update(ucd.group['digit_comma'])            # 🄁, 🄂
    rescue_codes.update(ucd.group['parenthesized_latin'])    # ⒜, ⒵
    rescue_codes.update(ucd.group['circled_latin'])          # Ⓐ, Ⓩ
    rescue_codes.update(ucd.group['squared_latin'])          # 🄰, 🅉
    rescue_codes.update(ucd.group['box_drawing'])            # ─, ┏, ┨ (罫線)
    rescue_codes.update(ucd.group['block_elements'])         # ▀, ▄, █
    rescue_codes.update(ucd.group['geometric_shapes'])       # ■, ▲, ○
    rescue_codes.update(ucd.group['miscellaneous_symbols'])  # ♠, ♦, ☃, ♨
    # rescue_codes.update(ucd.group['progress_bar'])           # 🮀, 🮆 (プログレスバー等)

    target_codes = (pure_amb | rescue_codes) \
    - set(emoji_dict.keys()) \
    - set(ucd.group['nerdfont'])

    for code in sorted(target_codes):
        # まずはフォント群から実測値を取得
        measured_width = scanner.get_width(code)

        if measured_width is not None:
            final_width = measured_width
        else:
            final_width = args.fallback_amb

        # U+00FF 以下の ASCII/Latin-1 領域で、かつ半角(1)と判定されたものは
        # ロケールデータの肥大化を防ぐために出力から除外
        if code <= 0x00FF and final_width == 1:
            continue

        width_map[code] = final_width

        for code in emoji_dict.keys():
            width_map[code] = args.fallback_emoji

        for code in ucd.group['private']:
            width_map[code] = args.fallback_pua

            # Nerd Fonts 領域 (Private 以外にはみ出している分を補完)
        for code in ucd.group['nerdfont']:
            width_map[code] = args.fallback_pua

        return width_map, target_codes

def print_locale(out, start, end):
    if end <= 0xffff:
        octet = 4
    else:
        octet = 8
    if start == end:
        print(f'<U{start:0{octet}X}> 2', file=out)
    else:
        print(f'<U{start:0{octet}X}>...<U{end:0{octet}X}> 2', file=out)

def generate_locale(width_list):
    path = f'UTF-8-EAW-CUSTOM'
    print(f'Generating {path} ... ', end='')
    out = open(path, 'w')
    orig = open(ORIGINAL_FILE)
    for line in orig:
        if line.startswith('END WIDTH'):
            break
        print(line, end='', file=out)
    orig.close()
    out.write('% Added By locale-from-fontset\n')
    for start, end, width in width_list:
        if width == 2:
            print_locale(out, start, end)
    out.write('END WIDTH\n')
    out.close()
    print('done')
    print(f'Generating {path}.gz ... ', end='')
    with open(path, 'rb') as locale_file:
        with gzip.GzipFile(f'{path}.gz', 'wb', mtime=0) as locale_file_gz:
            shutil.copyfileobj(locale_file, locale_file_gz)
    print('done')

def generate_elisp(width_list):
    path = f'eaw-custom.el'
    print(f'Generating {path} ... ', end='')
    with open(path, 'w') as out:
        out.write(open('src/eaw-header.el').read())
        print('(setq code-half \'(', file=out)
        for start, end, width in width_list:
            if width == 1:
                if start == end:
                    print(f'  #x{start:x}', file=out)
                else:
                    print(f'  (#x{start:x} . #x{end:x})', file=out)
        print('))', file=out)
        print('(setq code-wide \'(', file=out)
        for start, end, width in width_list:
            if width == 2:
                if start == end:
                    print(f'  #x{start:x}', file=out)
                else:
                    print(f'  (#x{start:x} . #x{end:x})', file=out)
        print('))', file=out)
        out.write(open('src/eaw-footer.el').read())
    print('done')

def generate_wezterm(width_list):
    path = f'eaw-custom-wezterm.lua'
    print(f'Generating {path} ... ', end='')
    with open(path, 'w') as out:
        print('-- Generated by locale-from-fontset', file=out)
        print('return {', file=out)
        for start, end, width in width_list:
            print(f'  {{first = 0x{start:x}, last = 0x{end:x}, width = {width}}},', file=out)
        print('}', file=out)
    print('done')

if __name__ == '__main__':
    main()

# def generate_flavor(config, ucd):
#     flavor = config.name
#     print(f'# {flavor} Flavor')
#     width_map = {}

#     for name in config:
#         set_width(width_map, ucd, name, config)

#     width_list = range_compress(width_map)
#     generate_locale(config, width_list)
#     generate_elisp(config, width_list)
#     generate_vimrc(config, width_list)
#     generate_mlterm(config, width_list)
#     generate_wezterm(config, width_list)
#     generate_json(config, width_list)

# def generate_mlterm(config, width_list):
#     flavor = config.name.lower()
#     path = f'dist/{flavor}.mlterm'
#     print(f'Generating {path} ... ', end='')
#     with open(path, 'w') as out:
#         print('# Generated by locale-eaw', file=out)
#         print('col_size_of_width_a = 1', file=out)
#         print('unicode_full_width_areas = ', file=out, end="")
#         for start, end, width in width_list:
#             if width == 2:
#                 print(f'U+{start:04X}-{end:04X},', file=out, end="")
#         print('', file=out)
#     print('done')


# def generate_vimrc(config, width_list):
#     flavor = config.name.lower()
#     path = f'dist/{flavor}.vim'
#     print(f'Generating {path} ... ', end='')
#     with open(path, 'w') as out:
#         print('" Generated by locale-eaw', file=out)
#         print('''
# if exists('&ambw')
#     set ambw=single
# endif
# call setcellwidths([\
# ''', file=out)
#         for start, end, width in width_list:
#             print(f'\\[0x{start:x},0x{end:x},{width}],', file=out)
#         print('\\])', file=out)
#     print('done')


# def generate_json(config, width_list):
#     flavor = config.name.lower()

# def generate_json(config, width_list):
#     flavor = config.name.lower()
#     path = f'dist/{flavor}.json'
#     print(f'Generating {path} ... ', end='')
#     with open(path, 'w') as out:
#         json.dump(width_list, out, indent=2)
#     print('done')


