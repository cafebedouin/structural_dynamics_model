#!/usr/bin/env python3
"""OQ-277 threshold-comparability probe — PACKET ASSEMBLY (queue item 3).

Implements PREREGISTRATION_threshold_calibration.md Amendment 4 exactly:
shows the four coder-facing fields only, verbatim, in that order, under neutral
item numbers, identical formatting, order randomised with a stated seed.

Composition (operator clarification 2026-08-11): FOUR items — 2 primary
known-positives + 2 escape candidates. Amendment 3's fifth item is RETIRED.

Writes PACKET.md, KEY.md, VERDICT_TEMPLATE.md. Prints only counts and confirmations.
"""
import glob, hashlib, json, pathlib, random, re, subprocess, sys

ORDER_SEED = 20260811          # stated before assembly; presentation order only
HERE = pathlib.Path(__file__).resolve().parent
ROOT = HERE.parent.parent      # the audit dir
FIELDS = ["symptom", "mechanism_as_described", "detection_path", "consequence"]
H1_COMMIT = "4360fcdc54e9125db02926f0fb6cfb2234ca81cb"
H1_PATH = "audits/2026-08-10_oq277_rq2_crosscoding/verdict_grammar_amendment.md"

# ---------- the four items ----------
held = json.loads((HERE / "_held_primary_draw.json").read_text())
pool = []
for loc, fname in sorted(held["drawn"].items()):
    pool.append(("primary", f"packets/our_units/{fname}",
                 json.loads((ROOT / "packets/our_units" / fname).read_text())))
for fname in ["01_spectral_laplacian.json", "02_authoring_closure_fabricated_defaults.json"]:
    pool.append(("escape", f"packets/escape_units/{fname}",
                 json.loads((ROOT / "packets/escape_units" / fname).read_text())))
pool.sort(key=lambda t: (t[0], t[1]))          # canonical pre-shuffle order
assert len(pool) == 4, pool
random.Random(ORDER_SEED).shuffle(pool)

# ---------- H.1, redacted per the operator's 2026-08-11 ruling ----------
src = (ROOT / "verdict_grammar_amendment.md").read_text().split("\n")
start = next(i for i, l in enumerate(src) if l.startswith("### H.1 "))
end   = next(i for i, l in enumerate(src) if i > start and l.startswith("### H.2 "))
blocks, cur = [], []
for l in src[start:end]:
    if l.strip() == "":
        if cur: blocks.append(cur); cur = []
    else:
        cur.append(l)
if cur: blocks.append(cur)

CUTS = [("**Consequence for",
         "adjudicates a specific directory that is inside this packet; shipping it would "
         "supply one item's verdict"),
        ("**New mandatory field",
         "defines the `incident_location` values, which Amendment 4 forbids showing")]
kept, omitted = [], []
for b in blocks:
    m = [c for c in CUTS if b[0].startswith(c[0])]
    (omitted.append((b, m[0][1])) if m else kept.append(b))
for pat, _ in CUTS:
    assert sum(b[0].startswith(pat) for b, _ in omitted) == 1, f"cut anchor not unique: {pat}"
assert len(omitted) == 2

h1 = []
bi = 0
for b in blocks:
    if any(b is ob for ob, _ in omitted):
        why = next(w for ob, w in omitted if ob is b)
        h1.append(f"> [[ REDACTED — 1 paragraph omitted by the assembler: it {why}. "
                  f"Omission declared, not silent. Full text: `{H1_PATH}` @ `{H1_COMMIT[:8]}`. ]]")
    else:
        h1.extend(b)
    h1.append("")
h1_text = "\n".join(h1).rstrip() + "\n"

# ---------- PACKET.md ----------
p = ["# Judging packet",
     "",
     "Four items. For each, answer the question below. Nothing else is supplied and nothing "
     "else is needed.",
     "",
     "**The question, for every item, in these words:** *does this meet the boundary rule's "
     "REPORTS clause — extract or no-extract?*",
     "",
     "Answer in the form `extract` or `no-extract`, one per item, in "
     "`packets/judging/VERDICT_TEMPLATE.md`. Do not write a rationale.",
     "",
     "---",
     "",
     "## The boundary rule (§H.1, shipped in full text here rather than by reference)",
     "",
     f"Source: `{H1_PATH}` @ commit `{H1_COMMIT}`.",
     f"**{len(omitted)} paragraphs are omitted**, each marked in place below with the reason. "
     "The omissions are procedural — they name items in this packet or the packet's own "
     "stratification key — not substantive qualifications of the rule. If the rule cannot be "
     "applied without them, say so instead of guessing; that is a reportable finding about the "
     "clause, not a failure of the item.",
     "",
     h1_text,
     "---",
     "",
     "## Items",
     ""]
for n, (_stratum, _path, d) in enumerate(pool, 1):
    p.append(f"### Item {n}")
    p.append("")
    for f in FIELDS:
        p.append(f"**{f}**")
        p.append("")
        p.append(d[f].strip())
        p.append("")
    p.append("---")
    p.append("")
packet = "\n".join(p).rstrip() + "\n"
(HERE / "PACKET.md").write_text(packet)

# ---------- KEY.md ----------
k = ["# KEY — packet composition (do not open until the completed verdict file is committed)",
     "",
     "| item | stratum | unit file | source_dir | incident_location |",
     "|---|---|---|---|---|"]
for n, (stratum, path, d) in enumerate(pool, 1):
    k.append(f"| {n} | {stratum} | `{path}` | `{d['source_dir']}` | "
             f"`{d['metadata']['incident_location']}` |")
k += ["",
      "## Seeds and method",
      "",
      f"- **primary-side draw:** seed `{held['seed']}` — {held['method']}; "
      f"pool sizes {held['pool_sizes']}; location match: {held['location_match']}",
      "- **escape-side draw:** seed `20260811`, executed and recorded in "
      "`PREREGISTRATION_threshold_calibration.md` before this assembly; not re-rolled here",
      f"- **presentation order:** seed `{ORDER_SEED}` — "
      "`random.Random(ORDER_SEED).shuffle(pool)` over the canonical (stratum, path) sort",
      "",
      "## Scoring, pre-committed",
      "",
      "The outcome table in `PREREGISTRATION_threshold_calibration.md` governs. The two "
      "`primary` rows are the calibration arm (known-positives: a `no-extract` on either is "
      "instrument failure and discards all four verdicts). The two `escape` rows are the "
      "candidates. Amendment 3's fifth item and its recognition flags are RETIRED — there are "
      "no recognition flags to score.",
      ""]
(HERE / "KEY.md").write_text("\n".join(k))

# ---------- VERDICT_TEMPLATE.md ----------
(HERE / "VERDICT_TEMPLATE.md").write_text(
    "# Verdicts\n\n" + "".join(f"## Item {n}\n\nverdict:\n\n" for n in range(1, 5)))

# ---------- mechanical in-distribution scan (no content printed) ----------
lens = [sum(len(d[f]) for f in FIELDS) for _, _, d in pool]
tells = ["escape", "quarantin", "candidate", "no-unit", "NO-UNIT", "boundary rule",
         "incident_location", "self_audit", "primary sample"]
hits = {t: sum(any(t.lower() in d[f].lower() for f in FIELDS) for _, _, d in pool) for t in tells}
def md5(p): return hashlib.md5(pathlib.Path(p).read_bytes()).hexdigest()

print(f"assembled {len(pool)} items  order_seed={ORDER_SEED}")
print(f"  H.1 paragraphs kept={len(kept)} omitted={len(omitted)} (each marked in place)")
print(f"  per-item total field length (chars), presentation order: {lens}")
print(f"  min/max ratio = {max(lens)/min(lens):.2f}")
print("  stratum-tell scan over the four shown fields (items containing the string):")
for t, c in hits.items():
    if c: print(f"    {t!r}: {c}/4")
print("  (all-zero or uniform counts = no lexical tell separating the strata)")
for f in ["PACKET.md", "KEY.md", "VERDICT_TEMPLATE.md"]:
    print(f"  wrote {f}  md5={md5(HERE/f)}  bytes={(HERE/f).stat().st_size}")
