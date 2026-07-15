#!/usr/bin/env bash
set -e

SDE_SRC="/c/soft/GitHub/Simple-Data-Explorer/src"
TARGET_SRC="/c/soft/GitHub/src"
TARGET_DIR="/c/soft/GitHub"

echo "Copying src files..."
rm -rf "$TARGET_SRC"
mkdir -p "$TARGET_SRC"
cp "$SDE_SRC"/*.clas.abap "$TARGET_SRC/" 2>/dev/null || true
cp "$SDE_SRC"/*.intf.abap "$TARGET_SRC/" 2>/dev/null || true

echo "Preparing entrypoint..."
# abapmerge derives the "main report" name from the entrypoint filename
# (everything before the first dot) and only injects classes once it finds a
# matching "REPORT <name>." line in the file. zsde2.prog.abap actually
# declares "REPORT z_sde." - so the working copy must be named z_sde.prog.abap,
# otherwise abapmerge silently passes the file through unmerged.
cp "$SDE_SRC/zsde2.prog.abap" "$TARGET_SRC/z_sde.prog.abap"

echo "Running abapmerge..."
cd "$TARGET_DIR"
abapmerge -f src/z_sde.prog.abap -c z_sde_standalone -o z_sde_standalone.prog.abap

echo "Copying result back to Simple-Data-Explorer/src..."
cp "$TARGET_DIR/z_sde_standalone.prog.abap" "$SDE_SRC/z_sde_standalone.prog.abap"

echo "Fixing FOR EVENT ... OF ordering (abapmerge only orders by INHERITING FROM)..."
python "$SDE_SRC/../fix_event_order.py" "$SDE_SRC/z_sde_standalone.prog.abap"

echo "Restoring header comments..."
# Extract comment/blank lines from zsde2.prog.abap after line 1, stop at first code line
header=$(awk 'NR==1{next} /^[[:space:]]*($|")/{print; next} {exit}' "$SDE_SRC/zsde2.prog.abap")
# Insert header between line 1 and the rest of standalone
{ head -1 "$SDE_SRC/z_sde_standalone.prog.abap"; echo "$header"; tail -n +2 "$SDE_SRC/z_sde_standalone.prog.abap"; } \
  > /tmp/z_sde_standalone_fixed.abap
cp /tmp/z_sde_standalone_fixed.abap "$SDE_SRC/z_sde_standalone.prog.abap"

echo "Done. Generated z_sde_standalone.prog.abap"
