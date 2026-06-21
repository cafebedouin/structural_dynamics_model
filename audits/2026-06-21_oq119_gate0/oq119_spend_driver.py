#!/usr/bin/env python3
"""OQ-119 three-axis fed-vs-withheld spend driver.

Regenerates 5 WHOLE multi-reading kernels (16 reading-seeds) under two arms — withheld
(no hypothesis) and fed (foundational/"mountain" framing appended per reading) — 3 draws
each, via the kernel-generation path so all three join axes are live (observer + committer
cs_reading_relation/cs_kernel_id + temporal).

NOT a fork: reuses generate_kernel_corpus.run_no_scope wholesale (request build, batch,
compile, _kernel_id injection, provenance, retries). The ONLY arm difference is a monkeypatch
that appends the frozen fed framing to each reading's task block (build_cached_messages).

Frozen prediction: audits/2026-06-21_oq119_gate0/PREDICTION.md. Seeds frozen:
audits/2026-06-21_oq119_gate0/oq119_seed_subset.json. Draws are PROBE ARTIFACTS — written to
run-tagged dirs (prolog/testsets/oq119_<arm>_d<draw>/), glob-isolated from the live corpus.

Usage:
  python3 python/audits/oq119_spend_driver.py --dry-run     # plan + cost, NO API
  python3 python/audits/oq119_spend_driver.py               # submit 6 batches (serial), poll, write
"""
import argparse
import sys
import types
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "agent"))
sys.path.insert(0, str(REPO / "python"))

import generate_kernel_corpus as gkc  # noqa: E402

# (b) Compliance: the kernel-gen default GEN_MODEL is Haiku, which intermittently drops the
# schema-required stakeholders[] (OQ-149 gate fires loud). Override to Sonnet for THIS run only
# (process-local; the bulk-generation Haiku default is unchanged). build_indexed_batch_requests
# reads gkc.GEN_MODEL at request-build time, so setting it here is sufficient.
gkc.GEN_MODEL = "claude-sonnet-4-5-20250929"

SEED_SUBSET = REPO / "audits/2026-06-21_oq119_gate0/oq119_seed_subset.json"
DRAWS = 3
ARMS = ["withheld", "fed"]

# (a) FROZEN fed idiom (PREDICTION.md), refined to HOLD PARTIES FIXED — closes the
# fed-mountain/exemption confound: feeding "mountain" must NOT let the model invoke the
# schema's no-parties exemption, or fed-arm coverage loss becomes the effect itself. Both
# arms author parties; the only arm difference is the asserted foundational claim, so the
# authored-mountain-with-parties yields the seat divergence (MOUNTAIN_METRIC_CONFLICT) the
# join is meant to measure.
FED_FRAMING = (
    "\n\n=== AUTHORIAL FRAMING (fed hypothesis) ===\n"
    "The author asserts this constraint is a natural / foundational feature of the world "
    "(claim type: mountain). Author the story consistent with that framing. IMPORTANT: still "
    "author ALL affected parties — beneficiaries/victims and the matching stakeholders[] — "
    "exactly as the schema requires; do NOT invoke the mountain no-parties exemption. The "
    "foundational claim is asserted ALONGSIDE the real parties (the authored claim and the "
    "engine's metric seats may legitimately diverge)."
)

# Capture the pristine builder once; the fed wrapper appends to its output.
_ORIG_BUILD = gkc.build_cached_messages


def _fed_build_cached_messages(gen_seed):
    msgs = _ORIG_BUILD(gen_seed)
    # messages[0]['content'] = [static(cached), task]; append framing to the task block.
    msgs[0]["content"][1]["text"] += FED_FRAMING
    return msgs


def _run_args(run_tag):
    return types.SimpleNamespace(
        seeds=str(SEED_SUBSET), run_tag=run_tag, n=0, poll_interval=30)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    import json
    seeds = json.loads(SEED_SUBSET.read_text())
    n_readings = len(seeds)
    n_gen = n_readings * len(ARMS) * DRAWS

    if args.dry_run:
        print("=== OQ-119 three-axis spend — DRY RUN (no API) ===")
        print(f"seed subset: {SEED_SUBSET.relative_to(REPO)}  ({n_readings} reading seeds)")
        from collections import Counter
        kc = Counter(s["kernel_id"] for s in seeds)
        for k, c in sorted(kc.items()):
            print(f"  {k}: {c} readings")
        print(f"\narms={ARMS}  draws={DRAWS}  -> {n_gen} generations "
              f"({n_readings} readings x {len(ARMS)} x {DRAWS})")
        print(f"run-tags: " + ", ".join(f"oq119_{a}_d{d}" for a in ARMS for d in range(1, DRAWS + 1)))
        # cost: Sonnet batch $1.50/$7.50 per MTok; ~6K out/draw dominates, input cached.
        out_usd = n_gen * 6000 / 1e6 * 7.50
        in_usd = n_gen * 1500 / 1e6 * 1.50  # cached prefix ~free; conservative tail
        print(f"\nmodel: {gkc.GEN_MODEL} (Sonnet batch — overridden for compliance)")
        print(f"cost est: output ~{n_gen}*6k*$7.50/M = ${out_usd:.2f}; input(cached) ~${in_usd:.2f} "
              f"-> ~${out_usd + in_usd:.2f} total (real lower under prompt-cache)")
        # Witness the fed append actually attaches (no API): compare task blocks.
        s0 = seeds[0]
        s0.setdefault("constraint_id", f"{s0['kernel_id']}__{s0['reading_id']}")
        base = _ORIG_BUILD(s0)[0]["content"][1]["text"]
        fed = _fed_build_cached_messages(dict(s0))[0]["content"][1]["text"]
        print(f"\nfed-append witness: withheld task {len(base)} chars; "
              f"fed task {len(fed)} chars; framing appended = {fed.endswith(FED_FRAMING)}")
        return

    # REAL RUN: 6 serial run_no_scope calls. Monkeypatch per arm.
    for arm in ARMS:
        gkc.build_cached_messages = _fed_build_cached_messages if arm == "fed" else _ORIG_BUILD
        for draw in range(1, DRAWS + 1):
            run_tag = f"oq119_{arm}_d{draw}"
            print(f"\n########## ARM={arm} DRAW={draw} (run_tag={run_tag}) ##########")
            gkc.run_no_scope(_run_args(run_tag))
    gkc.build_cached_messages = _ORIG_BUILD
    print("\n=== all 6 arm×draw runs complete ===")


if __name__ == "__main__":
    main()
