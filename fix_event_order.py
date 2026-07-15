#!/usr/bin/env python3
"""
abapmerge only orders global-class DEFINITION blocks by INHERITING FROM
(see class_parser.js / class_list.js in the abapmerge npm package). It does
not know that "METHODS xxx FOR EVENT evt OF zcl_y" also requires zcl_y's
full DEFINITION to appear earlier in the flattened program (event handler
declarations are a component access, just like TYPE zcl_y=>some_type).

This works fine against real global classes in the SAP system (each class
is compiled/loaded independently), but breaks once abapmerge flattens
everything into one linear local-class program - the affected class's
DEFINITION block can end up textually *after* the class that declares a
handler for its events, causing a syntax error such as:

  Components of classes declared using "CLASS ZCL_SDE_TRANSMITTER
  DEFINITION DEFERRED" can only be accessed after the class is defined.

This script post-processes the merged standalone .abap file: it scans the
contiguous "class definitions" section, and for every class whose body
contains "FOR EVENT ... OF <other_class>", makes sure <other_class>'s
DEFINITION block is moved earlier (recursively) so it always precedes any
class that needs it. Only the block order changes - no line inside any
block is modified.
"""
import re
import sys


def fix_event_order(path):
    with open(path, encoding="utf-8") as f:
        lines = f.read().split("\n")

    class_def_re = re.compile(r"^CLASS\s+(\w+)\s+DEFINITION\b", re.IGNORECASE)
    deferred_re = re.compile(r"^CLASS\s+\w+\s+DEFINITION\s+DEFERRED\.", re.IGNORECASE)
    impl_re = re.compile(r"^CLASS\s+\w+\s+IMPLEMENTATION\.", re.IGNORECASE)

    def_start = None
    impl_start = None
    for i, line in enumerate(lines):
        if def_start is None and class_def_re.match(line) and not deferred_re.match(line):
            def_start = i
        if def_start is not None and impl_re.match(line):
            impl_start = i
            break

    if def_start is None or impl_start is None:
        print("fix_event_order: could not locate definitions section, skipping")
        return

    section = lines[def_start:impl_start]

    blocks = []
    i = 0
    n = len(section)
    while i < n:
        m = class_def_re.match(section[i])
        if not m:
            i += 1
            continue
        start = i
        j = i + 1
        while j < n and section[j].strip().upper() != "ENDCLASS.":
            j += 1
        blocks.append({"name": m.group(1).lower(), "lines": section[start:j + 1]})
        i = j + 1

    by_name = {b["name"]: b for b in blocks}
    event_re = re.compile(r"FOR\s+EVENT\s+\S+\s+OF\s+(\w+)", re.IGNORECASE)

    output = []
    placed = set()
    moved = []

    def place(block, needed_by=None):
        if block["name"] in placed:
            return
        placed.add(block["name"])
        body = "\n".join(block["lines"])
        for dep in sorted(set(m.lower() for m in event_re.findall(body))):
            if dep in by_name and dep not in placed:
                moved.append((dep, block["name"]))
                place(by_name[dep], needed_by=block["name"])
        output.extend(block["lines"])

    for b in blocks:
        place(b)

    if moved:
        for dep, needed_by in moved:
            print("fix_event_order: moved {} before {} (FOR EVENT ... OF dependency)".format(dep, needed_by))
        new_lines = lines[:def_start] + output + lines[impl_start:]
        with open(path, "w", encoding="utf-8", newline="\n") as f:
            f.write("\n".join(new_lines))
    else:
        print("fix_event_order: no reordering needed")


if __name__ == "__main__":
    fix_event_order(sys.argv[1])
