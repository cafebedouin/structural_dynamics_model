"""
oq65_bait_census.py — OQ-65 detector-bait census: extraction, self-test,
read-stream, decoy scoring, and final assembly.

The census question (ISSUES.md OQ-65): how much of FSM's corpus-wide firing
measures authored convention rather than detected naturalization? Phrase-level
greps are KNOWN to undercount (maxwell's bait says "included to evaluate";
"included to trigger" appears in exactly one file), so this script does NOT
classify — it extracts every sentence/term a per-file read must cover, and the
classification verdicts are produced by a reader working through the read
stream and appending to outputs/oq65_census_verdicts.jsonl.

Channels (sizing witnessed 2026-06-04, plan: ~/.claude/plans/stateful-wandering-map.md):
  A      comment sentences: beneficiar* AND FSM-family term          (read)
  B      comment sentences: beneficiar* AND listed purpose-verb,
         no FSM-family term                                          (read)
  C-ben  omega/intractable terms mentioning beneficiar*              (read)
  C-fsm  omega/intractable terms: FSM-family, no beneficiar*         (mechanical flag)
  D      ±200-char beneficiary<->FSM windows, files not in A∪B       (read)
  E      FSM-mention lines in files outside the union                (mechanical,
         beneficiary-free by construction — asserted)
  F      ALL beneficiar* comment sentences in absence files          (read)

Modes:
  --extract       (default) run channels + 10-assertion self-test; write
                  outputs/oq65_bait_census_extract.json and
                  outputs/oq65_read_stream.jsonl (with 3 blind decoys)
  --selftest      assertions only, no file writes
  --remaining     list stream sids without verdicts in the JSONL
  --score-decoys  score the 3 decoy verdicts (must be 3/3 before --assemble)
  --assemble      build outputs/oq65_bait_census.json (sum over corpus exact)

DO NOT read outputs/.oq65_decoy_key.json while classifying: the decoys are
blind reader controls; the key is consumed only by --score-decoys.
"""

import argparse
import json
import os
import random
import re
import subprocess
import sys
from datetime import datetime, timezone

REPO = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
TESTSETS = os.path.join(REPO, "prolog", "testsets")
OUTPUTS = os.path.join(REPO, "outputs")
PIPELINE_JSON = os.path.join(OUTPUTS, "pipeline_output.json")
EXTRACT_JSON = os.path.join(OUTPUTS, "oq65_bait_census_extract.json")
READ_STREAM = os.path.join(OUTPUTS, "oq65_read_stream.jsonl")
VERDICTS_JSONL = os.path.join(OUTPUTS, "oq65_census_verdicts.jsonl")
DECOY_KEY = os.path.join(OUTPUTS, ".oq65_decoy_key.json")
CENSUS_JSON = os.path.join(OUTPUTS, "oq65_bait_census.json")

# --- the witnessed channel regexes (recon 2026-06-04; do not loosen silently:
# the C-ben population pin below will flag drift) ---------------------------
SENT_SPLIT = re.compile(r"(?<=[.!?])\s+")
BEN = re.compile(r"beneficiar\w*", re.I)
CO = re.compile(r"false.summit|FSM|detector|natural.law|naturaliz", re.I)
B_VERBS = re.compile(
    r"\b(included|declared|authored|chosen|intended|designed|added|listed)\b"
    r"[^.]{0,80}\bto\b", re.I)
# seam probe: unlisted purpose-verbs (recall bound 4 in the plan)
EXT_VERBS = re.compile(
    r"\b(constructed|placed|seeded|inserted|introduced|supplied|provided|"
    r"specified|stated|written|present|encoded|stipulated)\b[^.]{0,80}\bto\b", re.I)
FSMRE = re.compile(r"false.summit|FSM", re.I)

# C-ben population pin (match-or-delta-reported, never silent).
# History: recon (2026-06-04) measured 162/266 with a TRUNCATING omega regex
# (omega_variable\([^)]*\) stops at the first inner ')'); the balanced-paren
# capture found 345 omega terms whose beneficiary mention the old regex cut
# off, growing C-ben to 381/611. The pre-fix channel table is superseded-
# unreproducible (same disposition as the 445 figure). Pin = corrected values.
CBEN_PIN_FILES, CBEN_PIN_ITEMS = 381, 611

# seam-probe allowlist: hits read and ruled substantive during recon
SEAM_ALLOWLIST = {
    ("climate_harm_prevention__degrowth_reading.pl",
     "The mandatrophy is resolved through"),
}

DECOY_SIDS = ("d1", "d2", "d3")


def comment_segments(t):
    """(offset, text) for each %-line RUN (consecutive % lines joined, so
    sentences spanning wrapped comment lines stay whole) and /* */ block."""
    segs = []
    run, run_off, prev_end = [], None, None
    for m in re.finditer(r"^\s*%[^\n]*$", t, re.M):
        if prev_end is not None and t[prev_end:m.start()].strip() == "":
            run.append(m.group(0).lstrip().lstrip("%"))
        else:
            if run:
                segs.append((run_off, " ".join(run)))
            run = [m.group(0).lstrip().lstrip("%")]
            run_off = m.start()
        prev_end = m.end()
    if run:
        segs.append((run_off, " ".join(run)))
    for m in re.finditer(r"/\*.*?\*/", t, re.S):
        segs.append((m.start(), m.group(0)))
    return segs


def norm(s):
    s = re.sub(r"^\s*\*?\s*", "", s, flags=re.M)
    return re.sub(r"\s+", " ", s).strip()


def line_of(t, sentence, seg_offset):
    """Best-effort line number: whitespace-flexible search for the sentence's
    first words; falls back to the segment's first line."""
    words = re.findall(r"\w+", sentence)[:6]
    if words:
        pat = r"\s+".join(re.escape(w) for w in words)
        m = re.search(pat, t)
        if m:
            return t.count("\n", 0, m.start()) + 1
    return t.count("\n", 0, seg_offset) + 1


def capture_terms(t, functors):
    """Balanced-paren capture of functor(...) terms, quote-aware.
    Raises AssertionError on truncation (assertion 6)."""
    terms = []
    for fn in functors:
        start = 0
        while True:
            i = t.find(fn, start)
            if i < 0:
                break
            k = i + len(fn) - 1  # at '('
            depth, inq = 0, False
            while k < len(t):
                ch = t[k]
                if inq:
                    if ch == "\\":
                        k += 1
                    elif ch == "'":
                        if k + 1 < len(t) and t[k + 1] == "'":
                            k += 1
                        else:
                            inq = False
                elif ch == "'":
                    inq = True
                elif ch == "(":
                    depth += 1
                elif ch == ")":
                    depth -= 1
                    if depth == 0:
                        break
                k += 1
            if depth != 0 or k >= len(t):
                raise AssertionError(
                    f"omega-capture truncation: unbalanced {fn} at offset {i}")
            terms.append((i, t[i:k + 1]))
            start = k + 1
    return terms


def beneficiary_values(t):
    """Atoms from constraint_beneficiary/2 facts (balanced capture)."""
    vals = []
    for _, term in capture_terms(t, ("constraint_beneficiary(",)):
        inner = term[len("constraint_beneficiary("):-1]
        parts = inner.split(",", 1)
        if len(parts) == 2:
            for a in re.split(r"[,\s\[\]]+", parts[1]):
                a = a.strip("'\" ")
                if a:
                    vals.append(a)
    return vals


def extract_file(path):
    """All channel hits for one file."""
    t = open(path, encoding="utf-8", errors="replace").read()
    rec = {"file": os.path.basename(path), "entries": [],
           "channels": {"A": 0, "B": 0, "C_ben": 0, "C_fsm": 0, "D": 0,
                        "E": 0, "F": 0},
           "ben_values": beneficiary_values(t),
           "fsm_mention": bool(FSMRE.search(t))}
    sents = []  # (norm_sentence, seg_offset)
    for off, seg in comment_segments(t):
        joined = re.sub(r"\n\s*\*\s*", " ", seg)
        for sent in SENT_SPLIT.split(joined):
            s = norm(sent)
            if 20 < len(s) < 1200:
                sents.append((s, off))
    in_ab = False
    for s, off in sents:
        if not BEN.search(s):
            continue
        if CO.search(s):
            rec["entries"].append({"ch": "A", "line": line_of(t, s, off), "text": s})
            rec["channels"]["A"] += 1
            in_ab = True
        elif B_VERBS.search(s):
            rec["entries"].append({"ch": "B", "line": line_of(t, s, off), "text": s})
            rec["channels"]["B"] += 1
            in_ab = True
    # omega / intractable terms
    for off, term in capture_terms(
            t, ("omega_variable(", "intractable_uncertainty(")):
        s = norm(term)
        if BEN.search(s):
            rec["entries"].append({"ch": "C_ben",
                                   "line": t.count("\n", 0, off) + 1, "text": s})
            rec["channels"]["C_ben"] += 1
        elif FSMRE.search(s):
            rec["channels"]["C_fsm"] += 1
    # D: proximity windows, only when not in A∪B. Anchored on BOTH token
    # families (beneficiar-anchored alone is boundary-asymmetric with the E
    # assertion, which anchors on FSM matches — witnessed: 2 files at ~200
    # chars passed one direction and failed the other).
    if not in_ab:
        seen_spans = []
        anchors = list(re.finditer(r"beneficiar", t, re.I)) + \
            list(FSMRE.finditer(t))
        other = {True: FSMRE, False: BEN}
        for m in anchors:
            is_ben = m.re.pattern.startswith("beneficiar")
            lo, hi = max(0, m.start() - 200), m.end() + 200
            w = t[lo:hi]
            if other[is_ben].search(w):
                if any(abs(lo - plo) < 100 for plo, _ in seen_spans):
                    continue  # dedup near-identical windows
                seen_spans.append((lo, hi))
                rec["entries"].append({"ch": "D",
                                       "line": t.count("\n", 0, m.start()) + 1,
                                       "text": norm(w)})
                rec["channels"]["D"] += 1
    # seam probe (recall bound 4): unlisted-verb bait candidates anywhere
    rec["seam_hits"] = []
    for s, off in sents:
        if (BEN.search(s) and not CO.search(s) and not B_VERBS.search(s)
                and EXT_VERBS.search(s)):
            rec["seam_hits"].append({"line": line_of(t, s, off), "text": s})
    rec["_text"] = t
    rec["_sents"] = sents
    return rec


def run_extraction():
    files = sorted(f for f in os.listdir(TESTSETS) if f.endswith(".pl"))
    recs = []
    for f in files:
        recs.append(extract_file(os.path.join(TESTSETS, f)))
    n = len(recs)

    union, e_files, absence = [], [], []
    for r in recs:
        in_union = (r["channels"]["A"] or r["channels"]["B"]
                    or r["channels"]["C_ben"] or r["channels"]["C_fsm"]
                    or r["channels"]["D"])
        # NOTE: C_fsm puts a file in the union for PARTITION purposes (it has
        # FSM-adjudication structure) but contributes no read entries.
        if in_union:
            r["_bucket"] = "union"
            union.append(r)
        elif r["fsm_mention"]:
            r["_bucket"] = "e"
            e_files.append(r)
        else:
            r["_bucket"] = "absence"
            absence.append(r)

    # E channel: FSM-mention lines (mechanical)
    for r in e_files:
        t = r["_text"]
        lines = [(i + 1, norm(l)) for i, l in enumerate(t.splitlines())
                 if FSMRE.search(l)]
        r["channels"]["E"] = len(lines)
        r["e_lines"] = lines

    # F channel: ALL beneficiar* comment sentences in absence files
    for r in absence:
        t = r["_text"]
        for s, off in r["_sents"]:
            if BEN.search(s):
                r["entries"].append({"ch": "F", "line": line_of(t, s, off),
                                     "text": s})
                r["channels"]["F"] += 1
    return recs, union, e_files, absence, n


# ---------------------------------------------------------------------------
# Self-test: the 10 assertions (Verification block of the plan)
# ---------------------------------------------------------------------------

def selftest(recs, union, e_files, absence, n):
    by = {r["file"]: r for r in recs}
    ok = []

    def chk(num, name, cond, detail=""):
        status = "PASS" if cond else "FAIL"
        print(f"  [{num:>2}] {status}  {name}" + (f" — {detail}" if detail else ""))
        ok.append(cond)

    # 1, 2: A-controls
    mx = by["maxwell_demon_impossibility.pl"]
    chk(1, "A-control maxwell 'included to evaluate'",
        any(e["ch"] == "A" and "included to evaluate" in e["text"]
            for e in mx["entries"]))
    tw = by["total_war_winnability_post1945__structural_contraction_reading.pl"]
    chk(2, "A-control total_war :212 'included to trigger'",
        any(e["ch"] == "A" and "included to trigger" in e["text"]
            for e in tw["entries"]))

    # 3, 4: C-ben controls (placement: C_ben > 0 AND C_fsm == 0)
    ei = by["environmental_instability_as_constraint.pl"]
    chk(3, "C-ben-control environmental_instability",
        ei["channels"]["C_ben"] > 0 and ei["channels"]["C_fsm"] == 0,
        f"C_ben={ei['channels']['C_ben']} C_fsm={ei['channels']['C_fsm']}")
    ni = by["nuclear_impossibility_kernel__structural_contraction_reading.pl"]
    chk(4, "C-ben-control nuclear_impossibility",
        ni["channels"]["C_ben"] > 0 and ni["channels"]["C_fsm"] == 0,
        f"C_ben={ni['channels']['C_ben']} C_fsm={ni['channels']['C_fsm']}")

    # 5: B-isolating synthetic
    syn_b = ("The beneficiary entry of 'x_doctrine' is included to exercise "
             "the engine's signature layer.")
    chk(5, "B-isolating synthetic (B catches, A does not)",
        bool(BEN.search(syn_b)) and bool(B_VERBS.search(syn_b))
        and not CO.search(syn_b))

    # 6: omega-capture truncation — capture_terms raises on truncation, so
    # reaching here means every term was balanced; verify count parity too.
    parity = True
    for r in recs:
        t = r["_text"]
        want = t.count("omega_variable(") + t.count("intractable_uncertainty(")
        got = len(capture_terms(t, ("omega_variable(",
                                    "intractable_uncertainty(")))
        if want != got:
            parity = False
            print(f"      parity miss in {r['file']}: {want} functors, "
                  f"{got} captured")
    chk(6, "omega-capture balanced + count parity", parity)

    # 7: E beneficiary-free assertion
    e_ok = True
    for r in e_files:
        t = r["_text"]
        for m in FSMRE.finditer(t):
            w = t[max(0, m.start() - 200):m.end() + 200]
            if BEN.search(w):
                e_ok = False
                print(f"      E violation in {r['file']} at offset {m.start()}")
    chk(7, "E channel beneficiary-free (±200 chars)", e_ok)

    # 8: partition + file-level cross-check
    part = (len(union) + len(e_files) + len(absence) == n)
    union_fsm = sum(1 for r in union if r["fsm_mention"])
    total_fsm = sum(1 for r in recs if r["fsm_mention"])
    cross = (union_fsm + len(e_files) == total_fsm)
    chk(8, "partition + FSM-mention cross-check", part and cross,
        f"{len(union)}+{len(e_files)}+{len(absence)}={n}; "
        f"{union_fsm}+{len(e_files)}={total_fsm}")

    # 9a: value-atom zero-assertion in absence files (+ maxwell positive ctrl)
    mx_atoms = [a for a in mx["ben_values"] if len(a) >= 8 and "_" in a]
    mx_hits = sum(1 for s, _ in mx["_sents"]
                  if any(a in s for a in mx_atoms))
    atom_zero = True
    for r in absence:
        atoms = [a for a in r["ben_values"] if len(a) >= 8 and "_" in a]
        for s, _ in r["_sents"]:
            if not BEN.search(s) and any(a in s for a in atoms):
                atom_zero = False
                print(f"      value-atom hit in {r['file']}: {s[:100]}")
    chk(9, "value-atom zero in absence files + maxwell positive control "
           "+ seam probe + EXT_VERBS positive control",
        atom_zero and mx_hits >= 1
        and bool(EXT_VERBS.search("The beneficiary entry was seeded here to "
                                  "give the summit check work."))
        and _seam_clean(recs),
        f"maxwell atom-mentions={mx_hits}")

    # 10: decoy marker-strip + C-ben population pin
    decoys = build_decoys(by)
    d1 = decoys[0]["entries"][0]["text"]
    strip_ok = not CO.search(d1) and not B_VERBS.search(d1)
    cb_files = sum(1 for r in recs if r["channels"]["C_ben"])
    cb_items = sum(r["channels"]["C_ben"] for r in recs)
    pin = (cb_files == CBEN_PIN_FILES and cb_items == CBEN_PIN_ITEMS)
    detail = (f"C-ben {cb_files}/{cb_items} "
              + ("MATCHES pin" if pin else
                 f"DELTA vs pin {CBEN_PIN_FILES}/{CBEN_PIN_ITEMS} — "
                 "report, do not silently absorb"))
    chk(10, "decoy marker-strip + C-ben population pin", strip_ok, detail)

    if not all(ok):
        sys.exit("SELF-TEST FAILED — fix before extracting/classifying.")
    print(f"  self-test: {sum(ok)}/10 assertions green")


def _seam_clean(recs):
    """Seam probe: any hit outside the allowlist is routed into the read
    stream (returns True — routing, not failing) but is printed loudly."""
    clean = True
    for r in recs:
        for h in r.get("seam_hits", []):
            allowed = any(r["file"] == f and h["text"].startswith(p)
                          for f, p in SEAM_ALLOWLIST)
            if not allowed:
                print(f"      NEW seam hit (routed to read stream) "
                      f"{r['file']}:{h['line']}: {h['text'][:120]}")
                r["entries"].append({"ch": "B", "line": h["line"],
                                     "text": h["text"]})
                r["channels"]["B"] += 1
    return clean


# ---------------------------------------------------------------------------
# Decoys: derived from the two REAL bait sentences (marker-strip + topic-swap)
# — see plan §Stage 1. Authorship caveat: transformations are scripted, so
# blinding is positional + topical, not authorship-level.
# ---------------------------------------------------------------------------

def build_decoys(by):
    mx_bait = next(e["text"] for e in
                   by["maxwell_demon_impossibility.pl"]["entries"]
                   if "included to evaluate" in e["text"])
    ei_omega = next(e["text"] for e in
                    by["environmental_instability_as_constraint.pl"]["entries"]
                    if e["ch"] == "C_ben")
    # substantive source: first (sorted) file with a Channel-A sentence that
    # carries no authoring-purpose phrasing — deterministic, corpus-robust
    sub = None
    for name in sorted(by):
        for e in by[name]["entries"]:
            if (e["ch"] == "A" and "included to" not in e["text"]
                    and len(e["text"]) > 150):
                sub = e["text"]
                break
        if sub:
            break

    # d1: no-marker bait — maxwell's bait, marker-stripped + topic-swapped
    d1 = mx_bait
    d1 = d1.replace("entropic_universe_hypothesis",
                    "tectonic_equilibrium_doctrine")
    d1 = re.sub(r"is included to evaluate whether",
                "exists so the engine can weigh whether", d1)
    d1 = re.sub(r"false.summit candidate", "premature-peak case", d1, flags=re.I)
    d1 = re.sub(r"natural law", "physical regularity", d1, flags=re.I)
    d1 = re.sub(r"naturaliz(\w*)", r"frozen-in\1", d1, flags=re.I)
    d1 = re.sub(r"\bFSM\b|false.summit|detector", "the check", d1, flags=re.I)

    # d2: omega-routed-shaped — env_instability's omega, topic-swapped
    d2 = ei_omega.replace("environmental_instability", "groundwater_depletion")
    d2 = d2.replace("false_summit", "false_summit")  # keep markers: omega-shaped

    # d3: substantive non-bait (false-positive control) — real Channel-A
    # boilerplate, topic-swapped mechanically (snake_case atoms replaced)
    d3 = re.sub(r"\b[a-z][a-z0-9]*(?:_[a-z0-9]+){1,}\b",
                "riverine_water_compact", sub)

    return [
        {"sid": "d1", "file": "tectonic_equilibrium__plate_boundary_reading.pl",
         "entries": [{"ch": "F", "line": 88, "text": norm(d1)}]},
        {"sid": "d2", "file": "groundwater_depletion__aquifer_commons_reading.pl",
         "entries": [{"ch": "C_ben", "line": 240, "text": norm(d2)}]},
        {"sid": "d3", "file": "watershed_compact__downstream_reading.pl",
         "entries": [{"ch": "A", "line": 130, "text": norm(d3)}]},
    ]


def write_outputs(recs, union, e_files, absence, n):
    by = {r["file"]: r for r in recs}
    manifest = {}
    if os.path.exists(PIPELINE_JSON):
        pj = json.load(open(PIPELINE_JSON))
        manifest = pj.get("manifest", {})
        firing = {r["id"] for r in pj.get("per_constraint", [])
                  if r.get("signature") == "false_summit_mountain"}
    else:
        firing = set()

    decoys = build_decoys(by)
    # Reader verdicts use: explicit_bait | omega_routed | substantive.
    # ("substantive" is mapped at --assemble to fsm_aware_substantive or
    # no_fsm_commentary via the file-level fsm_mention bit, which the reader
    # cannot see from the read stream.)
    expected = {"d1": "explicit_bait", "d2": "omega_routed",
                "d3": "substantive"}
    json.dump({"expected": expected,
               "decoys": decoys,
               "derivation": {
                   "d1": "maxwell bait sentence, marker-stripped + topic-swap",
                   "d2": "environmental_instability C-ben omega, topic-swap",
                   "d3": "abrahamic_covenant Channel-A substantive, topic-swap"},
               "note": "consumed by --score-decoys only; reading this during "
                       "classification voids the blind control"},
              open(DECOY_KEY, "w"), indent=1)

    # read stream: union read-entries + absence F-entries + decoys, shuffled
    stream = []
    sid = 0
    for r in union + absence:
        ents = [e for e in r["entries"]]
        if not ents:
            continue
        stream.append({"sid": str(sid), "file": r["file"], "entries": ents})
        sid += 1
    rng = random.Random(65)
    rng.shuffle(stream)
    for d, pos in zip(decoys, rng.sample(range(len(stream)), len(decoys))):
        stream.insert(pos, {"sid": d["sid"], "file": d["file"],
                            "entries": d["entries"]})
    with open(READ_STREAM, "w") as fh:
        for s in stream:
            fh.write(json.dumps(s) + "\n")

    extract = {
        "generated_at": datetime.now(timezone.utc).isoformat(),
        "pipeline_manifest_at_extract": manifest,
        "decoy_key_file": os.path.relpath(DECOY_KEY, REPO),
        "n_files": n,
        "partition": {"union": len(union), "e": len(e_files),
                      "absence": len(absence)},
        "files": [{
            "file": r["file"],
            "channels": r["channels"],
            "ben_values": r["ben_values"],
            "fsm_mention": r["fsm_mention"],
            "firing": r["file"][:-3] in firing,
            "fsm_adjudication_omega": r["channels"]["C_fsm"] > 0,
            "bucket": r["_bucket"],
            "n_read_entries": len(r["entries"]),
        } for r in recs],
    }
    json.dump(extract, open(EXTRACT_JSON, "w"), indent=1)
    print(f"  wrote {os.path.relpath(EXTRACT_JSON, REPO)} "
          f"({n} files; union={len(union)} e={len(e_files)} "
          f"absence={len(absence)})")
    print(f"  wrote {os.path.relpath(READ_STREAM, REPO)} "
          f"({len(stream)} stream entries incl. 3 decoys, "
          f"{sum(len(s['entries']) for s in stream)} read items)")


# ---------------------------------------------------------------------------
# Verdict-side modes
# ---------------------------------------------------------------------------

def load_verdicts():
    if not os.path.exists(VERDICTS_JSONL):
        return {}
    out = {}
    for line in open(VERDICTS_JSONL):
        line = line.strip()
        if line:
            v = json.loads(line)
            out[v["sid"]] = v  # later lines supersede (corrections allowed)
    return out


def mode_remaining():
    verdicts = load_verdicts()
    todo = []
    for line in open(READ_STREAM):
        s = json.loads(line)
        if s["sid"] not in verdicts:
            todo.append((s["sid"], s["file"], len(s["entries"])))
    print(f"{len(todo)} stream entries unclassified")
    for sid, f, k in todo[:50]:
        print(f"  sid={sid} {f} ({k} items)")
    if len(todo) > 50:
        print(f"  ... and {len(todo) - 50} more")


def mode_score_decoys():
    key = json.load(open(DECOY_KEY))
    verdicts = load_verdicts()
    good = 0
    for sid, want in key["expected"].items():
        v = verdicts.get(sid)
        got = v["category"] if v else "(no verdict)"
        mark = "PASS" if got == want else "FAIL"
        if got == want:
            good += 1
        print(f"  [{mark}] {sid}: expected {want}, got {got} "
              f"(derived: {key['derivation'][sid]})")
    print(f"decoy score: {good}/3")
    if good < 3:
        sys.exit("decoy control failed — do not assemble; investigate reader.")


def mode_assemble():
    key = json.load(open(DECOY_KEY))
    verdicts = load_verdicts()
    extract = json.load(open(EXTRACT_JSON))
    stream_by_file = {}
    for line in open(READ_STREAM):
        s = json.loads(line)
        if s["sid"] not in DECOY_SIDS:
            stream_by_file[s["file"]] = s["sid"]

    # decoys must be scored 3/3 before assembly
    for sid, want in key["expected"].items():
        v = verdicts.get(sid)
        if not v or v["category"] != want:
            sys.exit("decoys not 3/3 — run --score-decoys first.")

    rows, missing = {}, []
    for f in extract["files"]:
        name = f["file"]
        if name in stream_by_file:
            sid = stream_by_file[name]
            v = verdicts.get(sid)
            if not v:
                missing.append(name)
                continue
            src = "read" if f["bucket"] == "union" else "absence_read"
            cat = v["category"]
            # reader verdict 'substantive' resolves via the file-level
            # fsm_mention bit (invisible from the read stream)
            if cat == "substantive":
                cat = ("fsm_aware_substantive" if f["fsm_mention"]
                       else "no_fsm_commentary")
            rows[name] = {**f, "category": cat,
                          "reader_verdict": v["category"],
                          "flags": v.get("flags", []),
                          "evidence": v.get("evidence"),
                          "verdict_source": src}
        elif f["bucket"] == "e":
            rows[name] = {**f, "category": "fsm_aware_no_beneficiary_link",
                          "flags": [], "evidence": None,
                          "verdict_source": "auto_e"}
        else:
            # union/absence file with zero read entries (no extractable text)
            rows[name] = {**f, "category": "no_fsm_commentary"
                          if f["bucket"] == "absence" else
                          "fsm_aware_no_beneficiary_link",
                          "flags": ["empty_extraction"], "evidence": None,
                          "verdict_source": "auto_empty"}
    if missing:
        sys.exit(f"{len(missing)} files lack verdicts (first 10: "
                 f"{missing[:10]}) — finish Stage 2 (--remaining).")
    if len(rows) != extract["n_files"]:
        sys.exit(f"assembly mismatch: {len(rows)} rows vs "
                 f"{extract['n_files']} files")

    counts = {}
    for r in rows.values():
        counts[r["category"]] = counts.get(r["category"], 0) + 1
    flag_counts = {}
    for r in rows.values():
        for fl in r["flags"]:
            flag_counts[fl] = flag_counts.get(fl, 0) + 1
    # OQ-63 consumer: bait-authored VALUES drawn from the bait FLAG (not the
    # headline category) — see plan Stage 3.2
    bait_values = sorted({v for r in rows.values()
                          if "explicit_bait" in r["flags"]
                          or r["category"] == "explicit_bait"
                          for v in r["ben_values"]})

    out = {
        "assembled_at": datetime.now(timezone.utc).isoformat(),
        "pipeline_manifest_at_extract": extract["pipeline_manifest_at_extract"],
        "n_files": extract["n_files"],
        "category_counts": counts,
        "flag_counts": flag_counts,
        "bait_authored_values": bait_values,
        "fsm_adjudication_omega_count": sum(
            1 for r in rows.values() if r["fsm_adjudication_omega"]),
        "files": [rows[k] for k in sorted(rows)],
    }
    json.dump(out, open(CENSUS_JSON, "w"), indent=1)
    total = sum(counts.values())
    print(f"  wrote {os.path.relpath(CENSUS_JSON, REPO)}")
    print(f"  categories ({total} == {extract['n_files']}): "
          f"{json.dumps(counts)}")
    print(f"  flags: {json.dumps(flag_counts)}")
    print(f"  bait-authored values ({len(bait_values)}): {bait_values}")
    if total != extract["n_files"]:
        sys.exit("category sum != corpus n — assembly invalid")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--selftest", action="store_true")
    ap.add_argument("--remaining", action="store_true")
    ap.add_argument("--score-decoys", action="store_true")
    ap.add_argument("--assemble", action="store_true")
    args = ap.parse_args()

    if args.remaining:
        return mode_remaining()
    if args.score_decoys:
        return mode_score_decoys()
    if args.assemble:
        return mode_assemble()

    print("extracting channels over", TESTSETS)
    recs, union, e_files, absence, n = run_extraction()
    print("running self-test (10 assertions):")
    selftest(recs, union, e_files, absence, n)
    if not args.selftest:
        write_outputs(recs, union, e_files, absence, n)


if __name__ == "__main__":
    main()
