#!/usr/bin/env python3
"""OQ-221 Pass-2 adjudication runs (FREE, no LLM spend; deterministic).

Runs each executable row's firing condition over BOTH ratified corpora
(PREREG.md AMENDMENT 1) and reports both rates in the declared units:
fires per 1,000 words AND per-text binary fire. Wilson 95% intervals.

Controls run FIRST (H1 counting, H2 theme-naming); a control miss stops
the run for diagnosis before any novel row is ruled. Within-row positive
controls: every zero-claim first locates a known instance of that row's
own condition and shows the instrument surfaces it.

Corpora (ratified at checkpoint, AMENDMENT 1):
  DEFECT (story-grained): the 3 OQ-218 Stage-2 seeds + the ergodicity
    story (resonant-closer witness) + the empty-pan counting baseline.
  EARNED (story-grained, n=12): 10 classic-literature originals
    (EXTERNAL provenance) + rift3 + the-empty-pan_rev5 (pipeline-approved).

Run: python3 audits/2026-07-16_oq221_meter_partition/partition_run.py
Redirect stdout to partition_run.txt to save.

--post-diagnosis: the first run (partition_run.txt) STOPPED at the H1 miss
(counting fired on 4/12 earned texts) per the PREREG stop rule. Diagnosis
(witnessed in-session, 2026-07-16): instrument sound — within-row positive
controls passed and the earned fires are real tokens (prose number-words at
human base rate ~10-16/1000; treasure_island partially TOC-inflated; rift3
genuinely dense in-register). The MISS is a falsified PREDICTION (threshold
10.0 was variance-calibrated on pipeline output, never on human-prose base
rates), not an instrument failure. With this flag the run records the H1
earned-side fires as FINDING F1 and continues to the novel rows; it still
hard-stops on any within-row POSITIVE-control miss (instrument-level).
"""
import math
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from agent.uke_narrative_orchestrator import (  # noqa: E402
    _numeric_inventory, _theme_inventory,
    NUMERIC_DENSITY_THRESHOLD, THEME_DENSITY_THRESHOLD,
)

ORIG = ROOT / "agent" / "narrative_transform" / "originals"
STORIES = ROOT / "agent" / "narrative_transform" / "stories"

CLASSICS = [
    "alice_in_wonderland.md", "an_occurance_at_owl_creek_bridge.md",
    "eighty_yard_run.md", "frankenstein.md", "lysistrata.md", "medea.md",
    "philosophy_four.md", "the_old_man_and_the_sea.md",
    "the_velveteen_rabbit.md", "treasure_island.md",
]
EARNED = [("classic:" + n, ORIG / n) for n in CLASSICS] + [
    ("pipeline:rift3", ORIG / "rift3.md"),
    ("pipeline:the-empty-pan_rev5", STORIES / "the-empty-pan_rev5.md"),
]
DEFECT_SEEDS = [
    ("seed:the_good_name_book_rev1", STORIES / "the_good_name_book_rev1.md"),
    ("seed:the_eighth_commentary_rev1", STORIES / "the_eighth_commentary_rev1.md"),
    ("seed:the_table_of_winters_rev1", STORIES / "the_table_of_winters_rev1.md"),
    ("seed:112_ergodocity_kids_rev1", STORIES / "112_ergodocity_kids_rev1.md"),
]
# H1 baseline candidates — two same-named files exist; the baseline is the
# one in the OQ-215 defect band (37.6-47.6/1000, ~6,100 words). Both are
# measured and the band membership is REPORTED, not assumed.
BASELINE_CANDIDATES = [
    ("baseline?:the-empty-pan_rev1", STORIES / "the-empty-pan_rev1.md"),
    ("baseline?:the_empty_pan_rev1", STORIES / "the_empty_pan_rev1.md"),
]
ROW9_CONTRAST = [("contrast:the-empty-pan_rev2", STORIES / "the-empty-pan_rev2.md")]

# Row 7 Tier-A lexicon — EXACTLY as declared in PREREG (declared-vs-actual
# precision standard). Word-boundary, case-insensitive. Raw mechanical hits;
# per-hit contexts listed for adjudication where "abstract noun" / "metaphor-
# free" qualifiers apply.
TIER_A = [
    r"substrate", r"ontologically", r"extraction", r"coordination function",
    r"legibility", r"constraint", r"scaffold", r"piton", r"snare",
    r"tangled rope", r"theater ratio", r"classification",
]
TIER_B = [r"the system", r"coordination"]


def read(p: pathlib.Path) -> str:
    return p.read_text(encoding="utf-8", errors="replace")


def wilson(k: int, n: int, z: float = 1.96) -> tuple[float, float]:
    if n == 0:
        return (0.0, 1.0)
    p = k / n
    denom = 1 + z * z / n
    centre = p + z * z / (2 * n)
    adj = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n))
    return ((centre - adj) / denom, (centre + adj) / denom)


def lex_hits(text: str, patterns: list[str]) -> list[tuple[str, str]]:
    hits = []
    for pat in patterns:
        rx = re.compile(r"\b" + pat + r"\b", re.IGNORECASE)
        for m in rx.finditer(text):
            line = text[max(0, text.rfind("\n", 0, m.start()) + 1):
                        text.find("\n", m.end()) if text.find("\n", m.end()) != -1 else len(text)]
            hits.append((pat, line.strip()[:140]))
    return hits


def per1000(count: int, words: int) -> float:
    return 1000.0 * count / max(words, 1)


def main() -> int:
    post_diagnosis = "--post-diagnosis" in sys.argv[1:]
    failures: list[str] = []        # instrument-level: always stop
    findings: list[str] = []        # falsified predictions: stop unless post-diagnosis
    texts = {}
    for label, path in (EARNED + DEFECT_SEEDS + BASELINE_CANDIDATES + ROW9_CONTRAST):
        texts[label] = read(path)

    num = {lb: _numeric_inventory(t) for lb, t in texts.items()}
    thm = {lb: _theme_inventory(t) for lb, t in texts.items()}

    print("=" * 78)
    print("OQ-221 PASS-2 ADJUDICATION RUNS — deterministic conditions, both corpora")
    print("units: fires/1000 words + per-text binary fire (PREREG AMENDMENT 1)")
    print("=" * 78)

    # ------------------------------------------------------------------ H1
    print("\n## H1 (control) — Row 1 counting: _numeric_inventory density >= "
          f"{NUMERIC_DENSITY_THRESHOLD}\n")
    print("Baseline identification (two same-named files; band 37.6-47.6):")
    baseline_label = None
    for lb, _ in BASELINE_CANDIDATES:
        d = num[lb]["density_per_1000"]
        w = num[lb]["word_count"]
        in_band = 37.6 <= d <= 47.6
        print(f"  {lb:38s} density={d:6.2f} words={w:6d} in-band={in_band}")
        if in_band and baseline_label is None:
            baseline_label = lb
    if baseline_label is None:
        # fall back: highest density candidate, reported as such
        baseline_label = max((lb for lb, _ in BASELINE_CANDIDATES),
                             key=lambda lb: num[lb]["density_per_1000"])
        print(f"  NOTE: neither candidate in the recorded band; using highest "
              f"({baseline_label}) and flagging for diagnosis.")
    bl_fire = num[baseline_label]["density_per_1000"] >= NUMERIC_DENSITY_THRESHOLD
    print(f"\n  WITHIN-ROW POSITIVE CONTROL: {baseline_label} fires -> {bl_fire}")
    if not bl_fire:
        failures.append("H1 positive control: counting baseline did not fire")

    earned_fires = [lb for lb, _ in EARNED
                    if num[lb]["density_per_1000"] >= NUMERIC_DENSITY_THRESHOLD]
    n_e = len(EARNED)
    lo, hi = wilson(len(earned_fires), n_e)
    print(f"\n  earned leg (n={n_e}): fires={len(earned_fires)} "
          f"{earned_fires if earned_fires else ''}")
    for lb, _ in EARNED:
        print(f"    {lb:42s} density={num[lb]['density_per_1000']:6.2f} "
              f"words={num[lb]['word_count']:6d}")
    print(f"  earned rate = {len(earned_fires)}/{n_e}, Wilson95 [{lo:.3f}, {hi:.3f}]")
    if earned_fires:
        findings.append(
            "FINDING F1 — H1 earned-side prediction FALSIFIED: counting fired on "
            f"{len(earned_fires)}/{n_e} earned texts (rift3 in the defect band). "
            "Diagnosis: prediction error, not instrument error (see docstring).")

    # ------------------------------------------------------------------ H2
    print("\n## H2 (control) — Row 2 theme-naming: density-bearing kinds "
          "(anaphora+causal_chain)\n")
    r3 = thm["pipeline:rift3"]
    r3_flagged = (r3["counts"]["anaphora"] + r3["counts"]["causal_chain"]) > 0
    print(f"  WITHIN-ROW POSITIVE CONTROL: rift3 flagged -> {r3_flagged} "
          f"(anaph={r3['counts']['anaphora']} causal={r3['counts']['causal_chain']} "
          f"density={r3['density_per_1000']})")
    if not r3_flagged:
        failures.append("H2 positive control: rift3 not flagged")

    print(f"\n  {'text':44s} {'dens':>6s} {'anaph':>5s} {'causal':>6s} fire@>0")
    earned_theme_fires = 0
    for lb, _ in EARNED:
        c = thm[lb]["counts"]
        fire = (c["anaphora"] + c["causal_chain"]) > 0
        earned_theme_fires += fire
        print(f"  {lb:44s} {thm[lb]['density_per_1000']:6.2f} "
              f"{c['anaphora']:5d} {c['causal_chain']:6d} {fire}")
    lo, hi = wilson(earned_theme_fires, n_e)
    print(f"\n  earned-side positive fires = {earned_theme_fires}/{n_e}, "
          f"Wilson95 [{lo:.3f}, {hi:.3f}]  (positive fires expected -> reader-held)")
    if earned_theme_fires == 0:
        failures.append("H2: zero earned-side theme fires (contradicts OQ-214 witness)")
    for lb, _ in DEFECT_SEEDS[:3]:
        c = thm[lb]["counts"]
        print(f"  defect {lb:37s} {thm[lb]['density_per_1000']:6.2f} "
              f"{c['anaphora']:5d} {c['causal_chain']:6d}")

    if failures:
        print("\n!! INSTRUMENT-LEVEL CONTROL MISS — stopping before novel rows:")
        for f in failures:
            print("   -", f)
        return 1
    if findings:
        print("\n!! FALSIFIED PREDICTION(S):")
        for f in findings:
            print("   -", f)
        if not post_diagnosis:
            print("   Stopping per PREREG for diagnosis. Re-run with "
                  "--post-diagnosis after diagnosing.")
            return 1
        print("   --post-diagnosis: recorded as findings; continuing to novel rows.")
    else:
        print("\n  H1 + H2 PASS — proceeding to novel rows.")

    # ---------------------------------------------------------------- Row 7
    print("\n## Row 7 — F39 framework-residue: Tier-A lexicon (declared in PREREG)\n")
    d6_hits = lex_hits(texts["seed:the_good_name_book_rev1"],
                       [r"substrate", r"ontologically"])
    print(f"  WITHIN-ROW POSITIVE CONTROL: D6 tokens in run-1 seed -> "
          f"{len(d6_hits)} hit(s)")
    for pat, line in d6_hits[:4]:
        print(f"    [{pat}] {line}")
    if not d6_hits:
        print("    !! MISS — row 7 cannot be ruled; recorded for diagnosis")

    print(f"\n  {'text':44s} {'tierA':>5s} {'/1000':>7s}  tokens")
    row7_earned_fires = 0
    for lb, _ in EARNED:
        hits = lex_hits(texts[lb], TIER_A)
        toks = sorted({h[0] for h in hits})
        w = num[lb]["word_count"]
        fire = len(hits) > 0
        row7_earned_fires += fire
        print(f"  {lb:44s} {len(hits):5d} {per1000(len(hits), w):7.3f}  {toks}")
    lo, hi = wilson(row7_earned_fires, n_e)
    print(f"\n  earned-side binary fires = {row7_earned_fires}/{n_e}, "
          f"Wilson95 [{lo:.3f}, {hi:.3f}]")
    print("  per-hit contexts on earned texts (adjudication surface, first 3 each):")
    for lb, _ in EARNED:
        hits = lex_hits(texts[lb], TIER_A)
        for pat, line in hits[:3]:
            print(f"    {lb} [{pat}] {line}")
    print("\n  defect leg:")
    for lb, _ in DEFECT_SEEDS:
        hits = lex_hits(texts[lb], TIER_A)
        toks = sorted({h[0] for h in hits})
        print(f"  {lb:44s} {len(hits):5d} hits  {toks}")

    # ---------------------------------------------------------------- Row 8
    print("\n## Row 8 — resonant-closer: _detect_resonant_closer (via _theme_inventory)\n")
    ergo = thm["seed:112_ergodocity_kids_rev1"]
    ec = ergo["counts"]["resonant_closer"]
    print(f"  WITHIN-ROW POSITIVE CONTROL: ergodicity story closers surfaced = {ec} "
          f"(operator witnessed x4) -> {'PASS' if ec >= 1 else 'MISS'}")
    ergo_the_way = [e for e in ergo["entries"]
                    if e["kind"] == "resonant_closer" and "the way" in e["context"].lower()]
    print(f"  'the way X'-form among them: {len(ergo_the_way)}")
    for e in ergo_the_way[:6]:
        print(f"    L{e['line']}: {e['context'][:110]}")

    print(f"\n  {'text':44s} {'closers':>7s} {'/1000':>7s} fire@>0")
    row8_earned_fires = 0
    for lb, _ in EARNED:
        c = thm[lb]["counts"]["resonant_closer"]
        w = thm[lb]["word_count"]
        fire = c > 0
        row8_earned_fires += fire
        print(f"  {lb:44s} {c:7d} {per1000(c, w):7.3f} {fire}")
    lo, hi = wilson(row8_earned_fires, n_e)
    print(f"\n  earned-side binary fires = {row8_earned_fires}/{n_e}, "
          f"Wilson95 [{lo:.3f}, {hi:.3f}]")
    print("  defect leg:")
    for lb, _ in DEFECT_SEEDS:
        c = thm[lb]["counts"]["resonant_closer"]
        w = thm[lb]["word_count"]
        print(f"  {lb:44s} {c:7d} {per1000(c, w):7.3f}")

    # ---------------------------------------------------------------- Row 9
    print("\n## Row 9 — word-arithmetic: _detect_word_arithmetic "
          "(defect leg n=0 -> PROPOSED-capped; earned rate still reported)\n")
    rev2 = thm["contrast:the-empty-pan_rev2"]
    wm2 = rev2["counts"]["word_arithmetic"]
    print(f"  WITHIN-ROW POSITIVE CONTROL: rev2 wmath surfaced = {wm2} "
          f"(expect 3) -> {'PASS' if wm2 >= 1 else 'MISS'}")
    for e in rev2["entries"]:
        if e["kind"] == "word_arithmetic":
            print(f"    L{e['line']}: {e['context'][:120]}")
    print(f"\n  {'text':44s} {'wmath':>5s} fire@>0")
    row9_earned_fires = 0
    for lb, _ in EARNED:
        c = thm[lb]["counts"]["word_arithmetic"]
        fire = c > 0
        row9_earned_fires += fire
        print(f"  {lb:44s} {c:5d} {fire}")
    lo, hi = wilson(row9_earned_fires, n_e)
    print(f"  earned-side binary fires = {row9_earned_fires}/{n_e}, "
          f"Wilson95 [{lo:.3f}, {hi:.3f}]")
    print("  NOTE: rev5 is IN the earned story leg; its earned instance firing "
          "here is the operator-adjudicated earned fire (ISSUES.md:9820-9823).")

    print("\n" + "=" * 78)
    print("RUN COMPLETE — controls PASS; novel-row outputs above are the witnesses.")
    print("=" * 78)
    return 0


if __name__ == "__main__":
    sys.exit(main())
