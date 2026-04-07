"""Fetch State of the Union addresses via nltk and produce a manifest.

Uses nltk's state_union corpus (1945–2006, ~65 addresses). Saves each
address as a text file and writes sotu/sotu_manifest.json.

Usage:
    python3 python/sotu_fetch.py
"""

import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
SOTU_DIR = REPO_ROOT / "sotu"
RAW_DIR = SOTU_DIR / "raw"


def main():
    try:
        import nltk
    except ImportError:
        print("ERROR: nltk not installed. Run: pip install nltk")
        sys.exit(1)

    nltk.download("state_union", quiet=True)
    from nltk.corpus import state_union

    RAW_DIR.mkdir(parents=True, exist_ok=True)

    manifest = []
    for fileid in sorted(state_union.fileids()):
        # fileid examples: "1945-Truman.txt", "1964-Johnson-1.txt"
        stem = fileid.replace(".txt", "")
        parts = stem.split("-")
        year = int(parts[0])
        president = parts[1]

        text = state_union.raw(fileid)
        out_path = RAW_DIR / fileid
        out_path.write_text(text, encoding="utf-8")

        manifest.append({
            "id": stem,
            "year": year,
            "president": president,
            "path": str(out_path.relative_to(REPO_ROOT)),
        })

    manifest_path = SOTU_DIR / "sotu_manifest.json"
    manifest_path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")

    print(f"Saved {len(manifest)} addresses to sotu/raw/")
    print(f"Manifest: {manifest_path.relative_to(REPO_ROOT)}")
    for entry in manifest:
        print(f"  {entry['year']} {entry['president']:20s}  {entry['id']}")


if __name__ == "__main__":
    main()
