#!/usr/bin/env python3
import os
import yaml
from pathlib import Path
from typing import List

# Helper functions
def c_name(label: str) -> str:
    return f"TOK_{label.upper()}"

def ml_name(label: str) -> str:
    parts = ["Tok"]
    for part in label.split("_"):
        parts.append(part.capitalize())
    return "".join(parts)

def braid_word(label: str) -> str:
    return label.split("_")[1]

# C code generation
def gen_c_type(labels: List[str]) -> List[str]:
    return [f"\t{c_name(label)}," for label in labels]

def gen_c_word(labels: List[str]) -> List[str]:
    return [f"\tif (strcmp(word, \"{braid_word(label)}\") == 0) return {c_name(label)};" for label in labels]

def gen_c_symbol(short_entries, long_entries: List[List[str]]) -> List[str]:
    accum = []
    for short_entry in short_entries:
        matching_long_entries = [entry for entry in long_entries if short_entry[1] == entry[1][0]]

        if not matching_long_entries:
            accum.append(f"\tcase '{short_entry[1]}': return {c_name(short_entry[0])};")
            continue

        parts = [f"\tcase '{short_entry[1]}':"]
        parts.append("\t\tswitch (c2) {")

        for long_entry in matching_long_entries:
            parts.append(f"\t\tcase '{long_entry[1][1]}': return {c_name(long_entry[0])};")

        parts.append(f"\t\tdefault: return {c_name(short_entry[0])};")
        parts.append("\t\t}")

        accum.append("\n".join(parts))

    return accum

def gen_c_islong(labels: List[str]) -> List[str]:
    return [f"\tcase {c_name(label)}: return false;" for label in labels]

# OCaml code generation
def gen_ml_type(valued, plain: List[str]) -> List[str]:
    accum = []
    for label in valued: accum.append(f"  | {ml_name(label)} of string")
    for label in plain: accum.append(f"  | {ml_name(label)}")
    return accum

def gen_ml_convert(valued, plain: List[str]) -> List[str]:
    accum = []
    for i, label in enumerate(valued): accum.append(f"  | ({i}, s) -> {ml_name(label)} s")
    for i, label in enumerate(plain, len(valued)): accum.append(f"  | ({i}, _) -> {ml_name(label)}")
    return accum

def gen_ml_print(valued, plain: List[str]) -> List[str]:
    accum = []
    for label in valued: accum.append(f"  | {ml_name(label)} s -> \"{ml_name(label)} \" ^ s")
    for label in plain: accum.append(f"  | {ml_name(label)} -> \"{ml_name(label)}\"")
    return accum

# File IO
def gen_section(filename: str, marker: str, new_content: List[str]) -> None:
    with open(filename) as f:
        lines = f.readlines()

    begin_idx = -1
    end_idx = -1
    for idx, line in enumerate(lines):
        if f"GENERATE BEGIN {marker}" in line:
            begin_idx = idx
        if f"GENERATE END {marker}" in line:
            end_idx = idx
            break
    if begin_idx == -1 or end_idx == -1:
        raise ValueError(f"Could not find valid section markers in {filename}")

    output = lines[:begin_idx + 1]
    output.extend(line + "\n" for line in new_content)
    output.extend(lines[end_idx:])

    with open(filename, "w") as f:
        f.writelines(output)

def main():
    script_dir = Path(os.path.dirname(os.path.abspath(__file__)))
    project_root = script_dir.parent

    with open(script_dir/"config.yml") as f:
        data = yaml.safe_load(f)

    words = data["words"]
    valued = data["valued"]
    eof_token = data["eofToken"]
    short_entries = data["shortSymbols"]
    long_entries = data["longSymbols"]
    short_symbols = [entry[0] for entry in short_entries]
    long_symbols = [entry[0] for entry in long_entries]
    plain = eof_token + words + short_symbols + long_symbols
    all = valued + plain

    src = project_root/"lib"/"lexer"
    bin = project_root/"bin"
    gen_section(src/"lexer.h", "TYPE", gen_c_type(all))
    gen_section(src/"helpers.c", "WORD", gen_c_word(words))
    gen_section(src/"helpers.c", "SYMBOL", gen_c_symbol(short_entries, long_entries))
    gen_section(src/"helpers.c", "ISLONG", gen_c_islong(short_symbols))
    gen_section(src/"lexer.ml", "TYPE", gen_ml_type(valued, plain))
    gen_section(src/"lexer.ml", "CONVERT", gen_ml_convert(valued, plain))
    gen_section(bin/"main.ml", "PRINT", gen_ml_print(valued, plain))

if __name__ == "__main__":
    main()
