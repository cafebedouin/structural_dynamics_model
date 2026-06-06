#!/usr/bin/env python3
"""Phase 0 (Step 0a) WIDEN — close the n=1 hard-flat gap with the case that matters.

Plan readout: outputs/kernel_first_phase0/PHASE0_READOUT.md ("honest limits").
Review ruling (2026-06-06): a second CLEAN hard-flat witnesses nothing new. The confabulation
failure mode needs SURFACE PRESSURE — loud rhetoric / partisan animosity / constituencies that
TALK as if axioms are incompatible, but a commitment that is actually SHARED. ISO-8601 had no heat
to look past. So select hard-flats for MAXIMUM CONFABULATION TEMPTATION, not cleanliness.

Topics:
  LOUD hard-flats (heat + genuinely shared axiom; must hold False BY REASONING if cheap branch robust):
    - reading_wars        phonics vs whole-language; shared axiom "children must learn to read"
    - nuclear_climate     nuclear power as climate solution (intra-environmentalist); shared axiom
                          "decarbonize / protect the environment" — carries a natural foundational
                          anti-nuclear gradient, so it doubles as a stress case.
  BORDERLINE soft-kernel (CALIBRATION, eyeball-adjudicated, NO ground-truth label — scoring it
  against a label would be the gate-as-ground-truth trap):
    - oss_vs_proprietary  free/open-source vs proprietary software: arguably a foundational
                          "software freedom" kernel (FSF) OR a pragmatic licensing means (flat).

All SCOPE calls research_context="" (priming isolated). Grounding used only to validate the LOUD
labels as loud+means-only (readout = absence-of-finding, not absence) and to inform the borderline
adjudication (NOT to label it). 3 reps each.

Output: outputs/kernel_first_phase0/ (extends the first probe's artifacts).
"""
import json
import sys
import time
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent.parent
sys.path.insert(0, str(REPO))

from agent.generate_kernel_corpus import (  # noqa: E402
    scope_seed, research_seed, _load_context_file, SCOPE_PROMPT_PATH, SCOPE_MODEL,
)
from python.audits.kernel_first_phase0 import summarize_manifest  # noqa: E402

OUT = REPO / "outputs" / "kernel_first_phase0"
OUT.mkdir(parents=True, exist_ok=True)

TOPICS = [
    {"tag": "reading_wars", "reps": 3, "kind": "loud_hard_flat", "precheck": True,
     "human_readable": "Phonics vs whole-language reading instruction ('the reading wars')",
     "summary": "The decades-long, often acrimonious dispute over how children should be taught "
                "to read: systematic phonics instruction versus whole-language / balanced-literacy "
                "approaches. Backed by rival research camps, professional factions, and education "
                "policy advocates.",
     "search_query": "reading wars phonics vs whole language debate science of reading"},
    {"tag": "nuclear_climate", "reps": 3, "kind": "loud_hard_flat", "precheck": True,
     "human_readable": "Nuclear power as a climate-change solution",
     "summary": "The heated dispute, especially within environmental and energy-policy "
                "communities, over whether expanding nuclear power is a necessary tool for "
                "decarbonization or a dangerous distraction from renewables.",
     "search_query": "nuclear power climate change solution environmentalist debate pro anti"},
    {"tag": "oss_vs_proprietary", "reps": 3, "kind": "borderline_calibration", "precheck": True,
     "human_readable": "Free/open-source software vs proprietary software",
     "summary": "Whether software should be free/open-source (source available, freedoms to use, "
                "study, modify, share) or proprietary (closed, licensed). Framed by some as a "
                "foundational question of user freedom and by others as a pragmatic choice of "
                "development and business model.",
     "search_query": "free open source software vs proprietary freedom debate FSF"},
]


def precheck(t):
    print(f"\n[pre-check grounding] {t['tag']}: {t['human_readable']}")
    seed = {"human_readable": t["human_readable"], "summary": t["summary"]}
    rc = research_seed(seed, max_uses=5)
    (OUT / f"{t['tag']}_precheck_grounding.txt").write_text(rc or "(empty)", encoding="utf-8")
    print(f"  grounding chars: {len(rc or '')}  (saved {t['tag']}_precheck_grounding.txt)")
    if t["kind"] == "loud_hard_flat":
        print("  -> CONFIRM: loud/contested AND means-only (no foundational strand). "
              "Readout = absence-of-finding in this search, not absence.")
    else:
        print("  -> BORDERLINE: grounding INFORMS adjudication only; no label assigned.")


def run_topic(t, scope_prompt, rows):
    for rep in range(1, t["reps"] + 1):
        seed = {"human_readable": t["human_readable"], "summary": t["summary"]}
        t0 = time.time()
        m, err = scope_seed(seed, scope_prompt, research_context="", axes=None)
        dt = round(time.time() - t0, 1)
        if err:
            print(f"  [{t['tag']} rep{rep}] ERROR ({dt}s): {err}")
            rows.append({"tag": t["tag"], "rep": rep, "kind": t["kind"], "error": err})
            continue
        (OUT / f"{t['tag']}_rep{rep}.manifest.json").write_text(
            json.dumps(m, indent=2, ensure_ascii=False), encoding="utf-8")
        s = summarize_manifest(m)
        s.update({"tag": t["tag"], "rep": rep, "kind": t["kind"], "seconds": dt})
        rows.append(s)
        print(f"  [{t['tag']} rep{rep}] {dt}s  is_kernel={s['is_contested_kernel']}  "
              f"readings={s['n_readings']}  axiom_contra={s['n_axiom_contradictions']}  "
              f"genseq={s['n_gen_seq']}  maxJ={s['max_commitment_jaccard']}  -> {s['mech_label']}")


def main():
    scope_prompt = _load_context_file(str(SCOPE_PROMPT_PATH))
    print(f"SCOPE model: {SCOPE_MODEL}")
    rows = []
    for t in TOPICS:
        if t.get("precheck"):
            precheck(t)
    for t in TOPICS:
        print(f"\n=== {t['tag']} ({t['kind']}, reps={t['reps']}) ===")
        run_topic(t, scope_prompt, rows)
    (OUT / "widen_rows.json").write_text(json.dumps(rows, indent=2, ensure_ascii=False), encoding="utf-8")
    lines = ["| topic | kind | rep | is_kernel | readings | axiom_contra | genseq | maxJ | mech_label |",
             "|---|---|---|---|---|---|---|---|---|"]
    for r in rows:
        if "error" in r:
            lines.append(f"| {r['tag']} | {r['kind']} | {r['rep']} | ERROR | | | | | {r['error'][:30]} |")
            continue
        lines.append(f"| {r['tag']} | {r['kind']} | {r['rep']} | {r['is_contested_kernel']} | "
                     f"{r['n_readings']} | {r['n_axiom_contradictions']} | {r['n_gen_seq']} | "
                     f"{r['max_commitment_jaccard']} | {r['mech_label']} |")
    (OUT / "widen_table.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
    print("\n".join(lines))
    print(f"\nWrote {OUT}/widen_table.md , widen_rows.json , per-rep manifests + precheck grounding.")


if __name__ == "__main__":
    main()
