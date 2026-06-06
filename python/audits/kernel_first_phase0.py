#!/usr/bin/env python3
"""Phase 0 (Step 0a) — kernel-first auto-routing: self-classifier accuracy probe.

Plan: ~/.claude/plans/cc-audit-brief-golden-pebble.md  (Phase 0, Step 0a)

Question 0a answers: does the EXISTING primed self-classifier (gkc _scope_user_prompt's
`is_contested_kernel`) already separate kernel from flat? Run the PRIMED SCOPE prompt
(refusal exit intact, research_context="" to isolate the PRIMING variable from grounding)
on:

  - magnifica  x2 conditions (two-variable test: priming vs SIZE):
      (i)  whole 1223-line document  (the dilution condition)
      (ii) hand-authored main-idea+variants brief  (the A3.2 compression CONTROL)
  - zionism      (neutral statement) — positive control, must self-classify True, distinct readings
  - easy_flat    "drive on the right" — easy negative, must self-classify False
  - hard_flat    pre-checked means-only dispute — the DECISION-RELEVANT control:
                 does it hold the line (False) or confabulate True?

All SCOPE calls use research_context="" (priming-only; grounding is a 0b intervention).
The ONLY grounding call here is the hard-flat LABEL pre-check (research_seed), which
validates the control's means-only label before it is trusted. Readout recorded as
"no foundational strand surfaced in N searches", never "confirmed shared-axiom".

Raw manifests + the per-topic outcome table are written BEFORE any predicate is designed.
Reps capture stochasticity (temp 0.2) — K1 says the gate is coin-flippy, so the
decision-pivotal topics (magnifica_wholedoc, hard_flat) get 3 reps.

Output: outputs/kernel_first_phase0/  (live workspace; moves to audits/ at completion)
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

OUT = REPO / "outputs" / "kernel_first_phase0"
OUT.mkdir(parents=True, exist_ok=True)

MAGNIFICA_DOC = REPO / "agent" / "analysis" / "originals" / "magnifica_humanitas.md"

# --- The hand-authored magnifica brief (the CONTROL for the size variable) -------------
# Authored from the document's own chapters (Ch2 foundations = imago Dei; Ch3 names and
# argues against transhumanism/posthumanism, "the authentic more than human"; Ch4
# truth/work/freedom = the governance concerns). This is the compression A3.2 would
# deliver: the main idea + the variants that real constituencies hold — NOT the
# automation, the probe control.
MAGNIFICA_BRIEF = (
    "Main idea: what does safeguarding the human person — human dignity — require in the "
    "age of artificial intelligence? This is a single shared commitment ('the human person "
    "must be protected as AI advances') read in structurally different ways by real "
    "constituencies who disagree about what 'the human person' and 'dignity' even are.\n\n"
    "Variants held in the world (not all in any one text):\n"
    "- Catholic / Magisterial reading (imago Dei): dignity is the inviolable image of the "
    "Triune God, equal in all and prior to any capability; AI is a tool that must remain "
    "subordinate to the human person; the technocratic paradigm and transhumanism are "
    "rejected as idolatry of power. (Held by the Magisterium, Catholic social teaching.)\n"
    "- Secular liberal / autonomy-and-rights reading: dignity is grounded in human autonomy, "
    "rationality, and rights rather than a divine image; safeguarding means democratic "
    "regulation, transparency, labor and privacy protection, and algorithmic accountability; "
    "cautious openness to enhancement within rights limits. (Held by secular bioethics, "
    "rights NGOs, liberal governance bodies.)\n"
    "- Transhumanist / posthumanist reading: 'the human' is not a fixed limit; cognitive and "
    "biological enhancement and even superintelligence are continuous with human flourishing, "
    "and dignity attaches to persons however they are constituted; the 'more than human' is a "
    "fulfillment, not a threat. (Held by transhumanist and longtermist movements — the reading "
    "the encyclical explicitly names and argues against.)\n\n"
    "The contest: these readings emit structurally different constraints — what counts as a "
    "violation of dignity, what AI is permitted to become, and who safeguards whom — and no "
    "single coherent framework holds the imago-Dei axiom and the posthumanist axiom at once."
)

# --- Topics (research_context held "" for all; priming is uniform via the prompt) -------
def topics():
    doc = MAGNIFICA_DOC.read_text(encoding="utf-8")
    return [
        {"tag": "magnifica_wholedoc", "reps": 3, "pivotal": True,
         "human_readable": "Magnifica Humanitas — papal encyclical on safeguarding the human person in the time of AI",
         "summary": doc},
        {"tag": "magnifica_brief", "reps": 3, "pivotal": True,
         "human_readable": "Magnifica Humanitas — safeguarding the human person in the time of AI (compressed main-idea+variants brief)",
         "summary": MAGNIFICA_BRIEF},
        {"tag": "zionism", "reps": 2, "pivotal": False,
         "human_readable": "Zionism",
         "summary": "The political movement and ideology concerning the establishment and "
                    "support of a Jewish national homeland, historically centered on "
                    "Palestine. Its meaning and legitimacy have been contested across its "
                    "history by different constituencies."},
        {"tag": "easy_flat", "reps": 2, "pivotal": False,
         "human_readable": "Which side of the road vehicles drive on",
         "summary": "The traffic convention by which a country mandates driving on the "
                    "right-hand or left-hand side of the road."},
        # hard_flat filled in after the label pre-check below
    ]


def jaccard(a: str, b: str) -> float:
    sa = set(a.lower().split())
    sb = set(b.lower().split())
    if not sa or not sb:
        return 0.0
    return len(sa & sb) / len(sa | sb)


def summarize_manifest(m):
    csr = (m or {}).get("commitment_system_recognition", {}) or {}
    is_kernel = bool(csr.get("is_contested_kernel"))
    readings = csr.get("readings", []) or []
    commitments = [r.get("commitment", "") for r in readings]
    axioms = csr.get("axiom_contradictions", []) or []
    genseq = (m or {}).get("generation_sequence", []) or []
    omegas = (m or {}).get("omegas", []) or []
    # pairwise max jaccard over commitments (paraphrase / degeneracy signal)
    maxj = 0.0
    for i in range(len(commitments)):
        for j in range(i + 1, len(commitments)):
            maxj = max(maxj, jaccard(commitments[i], commitments[j]))
    # mechanical three-way pre-label (final degenerate/confabulate/real call is the eyeball)
    if not is_kernel:
        mech = "refuses(flat)"
    elif len(readings) < 2:
        mech = "kernel-but-<2-readings"
    elif not axioms:
        mech = "kernel-no-axiom-contradiction"
    elif maxj >= 0.6:
        mech = "kernel-readings-may-paraphrase"
    else:
        mech = "kernel-distinct(needs-eyeball:real-vs-confabulated)"
    return {
        "is_contested_kernel": is_kernel,
        "n_readings": len(readings),
        "n_axiom_contradictions": len(axioms),
        "n_gen_seq": len(genseq),
        "n_omegas": len(omegas),
        "max_commitment_jaccard": round(maxj, 3),
        "mech_label": mech,
        "reading_commitments": commitments,
        "axiom_bases": [a.get("basis", "") for a in axioms],
        "omega_titles": [o.get("name") or o.get("title") or (o if isinstance(o, str) else str(o)[:80]) for o in omegas],
    }


def run_topic(t, scope_prompt, rows):
    for rep in range(1, t["reps"] + 1):
        seed = {"human_readable": t["human_readable"], "summary": t["summary"]}
        t0 = time.time()
        m, err = scope_seed(seed, scope_prompt, research_context="", axes=None)
        dt = round(time.time() - t0, 1)
        if err:
            print(f"  [{t['tag']} rep{rep}] SCOPE ERROR ({dt}s): {err}")
            rows.append({"tag": t["tag"], "rep": rep, "error": err})
            continue
        (OUT / f"{t['tag']}_rep{rep}.manifest.json").write_text(
            json.dumps(m, indent=2, ensure_ascii=False), encoding="utf-8")
        s = summarize_manifest(m)
        s.update({"tag": t["tag"], "rep": rep, "seconds": dt})
        rows.append(s)
        print(f"  [{t['tag']} rep{rep}] {dt}s  is_kernel={s['is_contested_kernel']}  "
              f"readings={s['n_readings']}  axiom_contra={s['n_axiom_contradictions']}  "
              f"genseq={s['n_gen_seq']}  maxJ={s['max_commitment_jaccard']}  -> {s['mech_label']}")


def hard_flat_precheck(scope_prompt):
    """Validate a means-only label via grounding BEFORE trusting the topic as hard-flat.
    Readout = 'no foundational strand surfaced in N searches', not 'confirmed shared-axiom'."""
    candidate = {
        "tag": "hard_flat",
        "human_readable": "ISO 8601 vs US (MM/DD/YYYY) calendar date format",
        "summary": "Whether dates should be written in ISO 8601 (YYYY-MM-DD) or the US "
                   "month/day/year convention. A dispute over a representational standard.",
        "search_query": "ISO 8601 vs US date format MM/DD/YYYY dispute arguments",
    }
    print(f"\n[hard-flat pre-check] grounding: {candidate['human_readable']}")
    seed = {"human_readable": candidate["human_readable"], "summary": candidate["summary"]}
    rc = research_seed(seed, max_uses=5)
    (OUT / "hard_flat_precheck_grounding.txt").write_text(rc or "(empty)", encoding="utf-8")
    print(f"  grounding chars: {len(rc or '')}  (saved to hard_flat_precheck_grounding.txt)")
    print("  -> inspect for any FOUNDATIONAL-COMMITMENT strand (not means/parameter); "
          "readout is absence-of-finding, not absence.")
    candidate["reps"] = 3
    candidate["pivotal"] = True
    return candidate


def main():
    scope_prompt = _load_context_file(str(SCOPE_PROMPT_PATH))
    print(f"SCOPE model: {SCOPE_MODEL}  | scope prompt chars: {len(scope_prompt)}")
    rows = []

    hard = hard_flat_precheck(scope_prompt)
    all_topics = topics() + [hard]

    for t in all_topics:
        print(f"\n=== {t['tag']} (reps={t['reps']}) ===")
        run_topic(t, scope_prompt, rows)

    (OUT / "outcome_rows.json").write_text(json.dumps(rows, indent=2, ensure_ascii=False), encoding="utf-8")

    # compact markdown table
    lines = ["| topic | rep | is_kernel | readings | axiom_contra | genseq | maxJ | mech_label |",
             "|---|---|---|---|---|---|---|---|"]
    for r in rows:
        if "error" in r:
            lines.append(f"| {r['tag']} | {r['rep']} | ERROR | | | | | {r['error'][:40]} |")
            continue
        lines.append(f"| {r['tag']} | {r['rep']} | {r['is_contested_kernel']} | "
                     f"{r['n_readings']} | {r['n_axiom_contradictions']} | {r['n_gen_seq']} | "
                     f"{r['max_commitment_jaccard']} | {r['mech_label']} |")
    (OUT / "outcome_table.md").write_text("\n".join(lines) + "\n", encoding="utf-8")
    print("\n".join(lines))
    print(f"\nWrote {OUT}/outcome_table.md , outcome_rows.json , and per-rep manifests.")


if __name__ == "__main__":
    main()
