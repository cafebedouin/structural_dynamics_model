#!/usr/bin/env python3
"""8-word shingle overlap probe (OQ-259 item 3, B1 specimen-independence gate).

Pinned spec: consider only lines >150 chars (card bodies, not headers/tags);
lowercase; strip punctuation; take all consecutive 8-word shingles per line into a
set per file. Pairwise overlap % = |A ∩ B| / min(|A|,|B|) × 100.

Thresholds (pinned from the measured control in PREREGISTRATION.md):
  specimen passes iff overlap with EACH item-1 source < 0.1%;
  positive control (CNDI × Biopower NW) must measure >= 5%.
"""
import re
import sys

K = "agent/analysis/originals/k_files"
FILES = {
    "AFRO_NW": f"{K}/Afropessimism K Aff And Neg - Northwestern 2026.md",
    "CAPK_NW": f"{K}/Capitalism K Aff And Neg - Northwestern 2026.md",
    "BIOP_NW": f"{K}/Biopower K Aff And Neg - Northwestern 2026.md",
    "CNDI": f"{K}/Biopower K - CNDI 2026.md",
}
PAIRS = [
    ("AFRO_NW", "CAPK_NW", "specimen vs item-1 source"),
    ("AFRO_NW", "BIOP_NW", "specimen vs item-1 source"),
    ("CNDI", "BIOP_NW", "POSITIVE CONTROL"),
]


def shingles(path, n=8):
    out = set()
    with open(path, encoding="utf-8") as f:
        for line in f:
            line = line.rstrip("\n")
            if len(line) <= 150:
                continue
            words = re.sub(r"[^a-z0-9\s]", " ", line.lower()).split()
            for i in range(len(words) - n + 1):
                out.add(tuple(words[i:i + n]))
    return out


def main():
    sh = {k: shingles(p) for k, p in FILES.items()}
    for k in FILES:
        print(f"{k}: {len(sh[k])} shingles")
    for a, b, role in PAIRS:
        shared = len(sh[a] & sh[b])
        smaller = min(len(sh[a]), len(sh[b]))
        pct = 100.0 * shared / smaller if smaller else float("nan")
        print(f"{a} x {b} [{role}]: shared={shared} smaller={smaller} pct={pct:.4f}%")


if __name__ == "__main__":
    sys.exit(main())
