#!/usr/bin/env python3
"""P2 two-sided calibration (OQ-259 item 2, Part C prereg work).

P2 (draft form under calibration): a mechanical, name-blind match-rate between a
manifest's kernel readings and the source file's own mechanically built TAG inventory
(pinned method: grep -n '^#{1,3} ' <file>, per OQ-264 TAG_INVENTORY.txt). The intent is
a meta-layer discriminator: a single-voice meta-layer file (AT Fiat) decomposes into
readings that ARE its own section positions (expected high match — the ceiling); an
ordinary arsenal (Biopower NW, Cap K NW) has debate-structural headers its kernel
readings should NOT match (expected low — the floor). The gap is the discriminating
quantity; if floor ~= ceiling the predicate measures decomposition-fidelity, not
meta-layer-ness, and must be strengthened or escalated.

Matching rule v1 (pinned for this calibration run):
  - reading text = the reading's `commitment` field ONLY (name-blind: reading_id and
    all other fields excluded).
  - header text = TAG line stripped of #s, digits-only tokens, debate-notation tokens
    {1nc,2nc,1ar,2ac,at,xt,l,tl,ext}, and stopwords; lowercased alphanumeric tokens.
  - reading r matches header h iff coverage(h in r) = |tok(h) & tok(r)| / |tok(h)|
    >= 0.5 (headers reduced to 0 content tokens are unmatchable, counted in output).
  - match-rate = |readings assigned to DISTINCT headers (greedy by coverage)| /
    |readings|.
Deterministic; no network; no corpus writes.
"""
import json
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
K = REPO / "agent/analysis/originals/k_files"

STOP = set("""a an and are as at be but by for from has have if in into is it its of on
or that the their this to was were will with may might should would can could not no
does do""".split())
NOTATION = {"1nc", "2nc", "1ar", "2ac", "at", "xt", "l", "tl", "ext"}

GROUPS = {
    "ATFIAT (ceiling)": {
        "source": K / "AT Fiat K - Michigan 2026 BCFP.md",
        "manifests": [
            "audits/2026-08-06_oq264_kredraw_variance/fiat_pedagogy_kernel_2026_20260806_142033.manifest.json",
            "audits/2026-08-06_oq264_kredraw_variance/fiat_utility_debate_2026_20260806_142156.manifest.json",
            "audits/2026-08-06_oq264_kredraw_variance/fiat_efficacy_kernel_2026_20260806_142314.manifest.json",
        ],
    },
    "BIOPOWER_NW (floor)": {
        "source": K / "Biopower K Aff And Neg - Northwestern 2026.md",
        "manifests": [
            "audits/2026-08-03_kritik_ingest/biopower_k_nhi_debate_2026_20260803_102652.manifest.json",
            "audits/2026-08-05_oq259_emphasis_discriminator/biopower_healthcare_kernel_2026_20260805_144612.manifest.json",
            "audits/2026-08-05_oq259_emphasis_discriminator/biopower_nhi_debate_2026_20260805_144823.manifest.json",
        ],
    },
    "CAPK_NW (floor)": {
        "source": K / "Capitalism K Aff And Neg - Northwestern 2026.md",
        "manifests": [
            "audits/2026-08-03_kritik_ingest/capitalism_k_ndi2026_20260803_102445.manifest.json",
            "audits/2026-08-05_oq259_emphasis_discriminator/capitalism_k_debate_2026_20260805_145017.manifest.json",
            "audits/2026-08-05_oq259_emphasis_discriminator/capitalism_kritik_ndi2026_20260805_145128.manifest.json",
        ],
    },
}


def toks(text):
    words = re.sub(r"[^a-z0-9\s]", " ", text.lower()).split()
    return {w for w in words
            if w not in STOP and w not in NOTATION and not w.isdigit()}


def tag_headers(path):
    out = []
    for line in open(path, encoding="utf-8"):
        if re.match(r"^#{1,3} ", line):
            out.append(line.strip().lstrip("#").strip())
    return out


def readings(manifest_path):
    d = json.load(open(REPO / manifest_path))
    csr = d.get("commitment_system_recognition") or {}
    return csr.get("readings") or []


def match_rate(rds, headers, verbose=False):
    htoks = [(h, toks(h)) for h in headers]
    scored = []
    for i, r in enumerate(rds):
        rt = toks(r.get("commitment", ""))
        for j, (h, ht) in enumerate(htoks):
            if not ht:
                continue
            cov = len(ht & rt) / len(ht)
            if cov >= 0.5:
                scored.append((cov, i, j, h))
    scored.sort(reverse=True)
    used_r, used_h, pairs = set(), set(), []
    for cov, i, j, h in scored:
        if i in used_r or j in used_h:
            continue
        used_r.add(i)
        used_h.add(j)
        pairs.append((i, rds[i].get("reading_id", f"reading[{i}]"), h, cov))
    n = len(rds)
    if verbose:
        for i, rid, h, cov in sorted(pairs):
            print(f"      match: {rid}  <->  '{h}'  (coverage {cov:.2f})")
    return (len(pairs), n)


def main():
    for gname, g in GROUPS.items():
        headers = tag_headers(g["source"])
        empt = sum(1 for h in headers if not toks(h))
        print(f"== {gname}: {g['source'].name}")
        print(f"   TAG entries: {len(headers)} ({empt} with zero content tokens)")
        for m in g["manifests"]:
            rds = readings(m)
            k, n = match_rate(rds, headers, verbose=True)
            rate = k / n if n else float("nan")
            print(f"   {Path(m).name}: matched {k}/{n} readings = {rate:.2f}")
        print()


if __name__ == "__main__":
    sys.exit(main())
