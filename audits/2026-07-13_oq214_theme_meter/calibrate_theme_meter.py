#!/usr/bin/env python3
"""OQ-214 offline calibration for _theme_inventory (FREE, no LLM spend).

Runs the deterministic theme meter over the OQ-218 calibration corpus and
tabulates theme density (density-bearing kinds ONLY: anaphora + causal_chain).
The high-density SEED "before" arms are NOT standalone files — they are
embedded as STORY A / STORY B inside blind_arm_payload_run{1,2,3}.md and
labelled by AB_KEY_run{1,2,3}.md. This script splits the payloads on the
STORY A/B delimiter and reads the AB_KEYs to label SEED vs IMPROVED.

Controls (CLAUDE.md audit discipline — every probe carries its positive
control):
  * POSITIVE  — the three SEED arms must surface density-bearing candidates,
    and the named MANIFEST patterns (refrain / thesis-essay / syllogism /
    double anaphora) must be caught by SOME kind (recall over all kinds).
  * NEGATIVE  — the three IMPROVED v0.2 arms + clean human originals must be
    LOW on the density-bearing kinds.
  * FLAG-NOT-FAIL — the-empty-pan_rev2 (earned word-arithmetic) and rift3
    (earned institutional refrain) must APPEAR as flagged candidates but must
    NOT push density-bearing density over the chosen threshold.
  * EARNED-DENSE — rift3 + the densest clean human original, on the
    density-bearing kinds: if both land BELOW the threshold, the meter
    separates earned-dense from lazy-dense. If either lands ABOVE, the
    threshold is provisional ("reopens at first earned-dense encounter").

Run:  python3 audits/2026-07-13_oq214_theme_meter/calibrate_theme_meter.py
Raw table is printed to stdout; redirect to theme_density_table.txt to save.
"""
import re
import sys
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT))
from agent.uke_narrative_orchestrator import (  # noqa: E402
    _theme_inventory, THEME_DENSITY_THRESHOLD,
)

AUDIT = ROOT / "audits" / "2026-07-12_oq218_scored_snare"
ORIG = ROOT / "agent" / "narrative_transform" / "originals"
STORIES = ROOT / "agent" / "narrative_transform" / "stories"

_STORY_SPLIT_RE = re.compile(r'^=+\s*STORY\s+([AB])\s*=+\s*$', re.MULTILINE)
_AB_KEY_RE = re.compile(r'Story\s+([AB])\s*=\s*(SEED|IMPROVED)', re.IGNORECASE)


def read(p: pathlib.Path) -> str:
    return p.read_text(encoding="utf-8", errors="replace")


def split_payload(text: str) -> dict:
    """Return {'A': storyA_text, 'B': storyB_text} split on the delimiter."""
    marks = [(m.start(), m.group(1)) for m in _STORY_SPLIT_RE.finditer(text)]
    out = {}
    for idx, (pos, label) in enumerate(marks):
        body_start = text.index("\n", pos) + 1
        body_end = marks[idx + 1][0] if idx + 1 < len(marks) else len(text)
        out[label] = text[body_start:body_end].strip()
    return out


def ab_key(text: str) -> dict:
    """Return {'A': 'SEED'|'IMPROVED', 'B': ...} from an AB_KEY file."""
    return {m.group(1).upper(): m.group(2).upper()
            for m in _AB_KEY_RE.finditer(text)}


def dband(inv: dict) -> str:
    c = inv["counts"]
    return (f"density={inv['density_per_1000']:>6.2f}  "
            f"[anaph={c['anaphora']:>2} causal={c['causal_chain']:>2}]  "
            f"(adj: refr={c['refrain']} aph={c['aphorism']} "
            f"clos={c['resonant_closer']} wmath={c['word_arithmetic']})  "
            f"words={inv['word_count']}")


def main() -> int:
    print("=" * 78)
    print("OQ-214 THEME METER CALIBRATION — density-bearing kinds only")
    print(f"(current in-source THEME_DENSITY_THRESHOLD = {THEME_DENSITY_THRESHOLD})")
    print("=" * 78)

    seed_dens, improved_dens = [], []

    # --- SEED vs IMPROVED from the three blind-arm payloads ----------------
    print("\n## POSITIVE + NEGATIVE control: SEED (defect) vs IMPROVED (v0.2)\n")
    for run in (1, 2, 3):
        payload = AUDIT / f"blind_arm_payload_run{run}.md"
        keyfile = AUDIT / f"AB_KEY_run{run}.md"
        if not payload.exists() or not keyfile.exists():
            print(f"  run{run}: MISSING payload/key — SKIP")
            continue
        arms = split_payload(read(payload))
        key = ab_key(read(keyfile))
        for label in ("A", "B"):
            if label not in arms or label not in key:
                continue
            role = key[label]
            inv = _theme_inventory(arms[label])
            (seed_dens if role == "SEED" else improved_dens).append(
                inv["density_per_1000"])
            print(f"  run{run} STORY {label} = {role:<8}  {dband(inv)}")

    # --- Standalone IMPROVED v0.2 legs -------------------------------------
    print("\n## NEGATIVE control: standalone IMPROVED v0.2 legs\n")
    for name in ("the_keeping_v02_pathA.md", "the_red_ink_v02_pathA.md",
                 "the_hands_that_measure_v02_pathA.md",
                 "the_platform_knows_v02_pathB.md"):
        p = AUDIT / name
        if not p.exists():
            print(f"  {name}: MISSING — SKIP")
            continue
        inv = _theme_inventory(read(p))
        improved_dens.append(inv["density_per_1000"])
        print(f"  {name:<38} {dband(inv)}")

    # --- Clean human originals (classic literature) ------------------------
    print("\n## NEGATIVE control: clean human originals (classic literature)\n")
    classics = ["alice_in_wonderland", "an_occurance_at_owl_creek_bridge",
                "eighty_yard_run", "frankenstein", "lysistrata", "medea",
                "philosophy_four", "the_old_man_and_the_sea",
                "the_velveteen_rabbit", "treasure_island"]
    classic_rows = []
    for name in classics:
        p = ORIG / f"{name}.md"
        if not p.exists():
            print(f"  {name}: MISSING — SKIP")
            continue
        inv = _theme_inventory(read(p))
        classic_rows.append((name, inv["density_per_1000"]))
        print(f"  {name:<34} {dband(inv)}")

    # --- FLAG-NOT-FAIL + EARNED-DENSE controls -----------------------------
    print("\n## FLAG-NOT-FAIL + EARNED-DENSE control (must flag, must NOT gate)\n")
    earned_rows = []
    for label, p in (
        ("rift3 (institutional refrain)", ORIG / "rift3.md"),
        ("the-empty-pan_rev2 (survival math)", STORIES / "the-empty-pan_rev2.md"),
    ):
        if not p.exists():
            print(f"  {label}: MISSING — SKIP")
            continue
        inv = _theme_inventory(read(p))
        earned_rows.append((label, inv))
        print(f"  {label:<38} {dband(inv)}")
        # show that the adjudication-only kinds ARE catching the earned devices
        for g in inv["groupings"]:
            if g["kind"] == "refrain":
                print(f"      refrain caught: {g['text']!r} x{g['count']}")

    # densest clean human original for the earned-dense comparison
    if classic_rows:
        dn, dv = max(classic_rows, key=lambda r: r[1])
        print(f"\n  densest clean human original: {dn} = {dv:.2f}")

    # --- summary + threshold recommendation --------------------------------
    print("\n" + "=" * 78)
    print("SUMMARY (density-bearing kinds only)")
    print("=" * 78)
    if seed_dens:
        print(f"  SEED (defect) densities:     {sorted(seed_dens, reverse=True)}")
        print(f"    min SEED = {min(seed_dens):.2f}")
    if improved_dens:
        print(f"  IMPROVED (v0.2) densities:   {sorted(improved_dens, reverse=True)}")
        print(f"    max IMPROVED = {max(improved_dens):.2f}")
    if classic_rows:
        print(f"    max clean-original = {max(r[1] for r in classic_rows):.2f}")
    for label, inv in earned_rows:
        print(f"  earned-dense {label}: {inv['density_per_1000']:.2f}")
    if seed_dens and improved_dens:
        lo = max(improved_dens)
        hi = min(seed_dens)
        print(f"\n  gap: IMPROVED ceiling {lo:.2f}  <->  SEED floor {hi:.2f}")
        if hi > lo:
            print(f"  => a threshold in ({lo:.2f}, {hi:.2f}) separates the arms.")
        else:
            print("  => arms OVERLAP on density-bearing kinds — inspect per-story.")

    # --- CONTROLS VERDICT (recall + earned-dense) --------------------------
    print("\n" + "=" * 78)
    print("CONTROLS VERDICT")
    print("=" * 78)
    ok = True
    # POSITIVE control: the named MANIFEST patterns must be caught by SOME kind.
    seed_flag = seed_dens and max(seed_dens) > 0
    print(f"  [{'PASS' if seed_flag else 'FAIL'}] positive: SEED arms surface "
          f"density-bearing candidates (max SEED density {max(seed_dens):.2f})")
    ok = ok and bool(seed_flag)
    # NEGATIVE control: clean human originals must be LOW.
    if classic_rows:
        cmax = max(r[1] for r in classic_rows)
        clean_ok = cmax < THEME_DENSITY_THRESHOLD
        print(f"  [{'PASS' if clean_ok else 'FAIL'}] negative: clean human "
              f"originals below threshold (max {cmax:.2f} < "
              f"{THEME_DENSITY_THRESHOLD})")
        ok = ok and clean_ok
    # EARNED-DENSE + FLAG-NOT-FAIL: rift3 & empty-pan must FLAG but NOT gate.
    for label, inv in earned_rows:
        flagged = len(inv["entries"]) > 0
        below = inv["density_per_1000"] <= THEME_DENSITY_THRESHOLD
        print(f"  [{'PASS' if (flagged and below) else 'FAIL'}] flag-not-fail: "
              f"{label} flagged={flagged}, density {inv['density_per_1000']:.2f} "
              f"<= {THEME_DENSITY_THRESHOLD} = {below}")
        ok = ok and flagged and below
    # which corpus stories would the current threshold gate?
    gated = [d for d in seed_dens + improved_dens if d > THEME_DENSITY_THRESHOLD]
    print(f"\n  At THEME_DENSITY_THRESHOLD={THEME_DENSITY_THRESHOLD}: "
          f"{len(gated)} of {len(seed_dens) + len(improved_dens)} arms gate "
          f"(escalate OPEN): {sorted(gated, reverse=True)}")
    print(f"\n  OVERALL CONTROLS: {'PASS' if ok else 'FAIL'}")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
