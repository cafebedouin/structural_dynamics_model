#!/usr/bin/env python3
"""§E adjudication: frontier view vs an independent naive-cold-reader baseline.

The BASELINE is what a reader querying the flat ISSUES.md by eye produces: it
sees the prose dependency language and marks an OQ not-workable if ANY blocking
edge is authored, WITHOUT cheaply checking (a) the blocker's status or (b) its
Ω-type / human-gate routing. (Pattern-5 "unblocked" calls on the rest.)

The VIEW adds exactly those two checks. The diff is the confirm/contradict
ledger. Each CONTRADICT is settled by an EXTERNAL fact (the blocker's status
from issues_status, or the node's Ω-type) — not by preference — which is what
makes the verdict checkable (§E step 3).

Independence caveat (stated, not hidden): a single agent authored both the
edges and this baseline, so true two-party independence is not achieved. The
verdicts survive that limitation only because each CONTRADICT cites an external
fact that settles it regardless of who authored what.
"""
import sys
from pathlib import Path
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "python"))
import omega_resolver as r

BLOCKING = r.BLOCKING_RELATORS | {r.HUMAN_RELATOR}


def naive_baseline(entries):
    """Cold reader: workable unless ANY blocking edge is authored (status-blind,
    Ω-blind). Returns {oq: 'workable'|'blocked'} over active OQs."""
    out = {}
    for oq, e in entries.items():
        if not e.active:
            continue
        has_block = any(rel in BLOCKING for rel, _ in e.deps)
        out[oq] = "blocked" if has_block else "workable"
    return out


def view_label(buckets):
    """Flatten the view's buckets to {oq: bucket}."""
    out = {}
    for b, items in buckets.items():
        for item in items:
            for oq in (item if isinstance(item, list) else [item]):
                out[oq] = b
    return out


def main():
    entries, _ = r.parse_entries()
    buckets, sccs, nontrivial = r.frontier(entries)
    base = naive_baseline(entries)
    view = view_label(buckets)

    def view_workable(oq):
        return view[oq] == "workable_now"

    confirms, contradicts = [], []
    for oq in sorted(base, key=lambda o: int(o.split("-")[1])):
        b_workable = base[oq] == "workable"
        v_workable = view_workable(oq)
        e = entries[oq]
        if b_workable == v_workable and view[oq] != "standoff":
            confirms.append((oq, view[oq]))
        else:
            # what fact settles the disagreement?
            facts = []
            for rel, tgt in e.deps:
                if rel == r.HUMAN_RELATOR:
                    facts.append(f"human-gate '{tgt}' (not an OQ edge → cold reader misses it)")
                elif rel in r.BLOCKING_RELATORS and tgt in entries:
                    facts.append(f"{tgt} status={entries[tgt].status}, Ω-type={entries[tgt].omega}")
            if not facts:
                # Ω_P leaf: settled by the node's OWN authored Ω-type
                facts.append(f"own Ω-type={e.omega} (routes out to a human; may never resolve)")
            contradicts.append((oq, base[oq], view[oq], "; ".join(facts)))

    edged_confirms = sum(1 for oq, v in confirms if entries[oq].deps)
    meaningful = len(contradicts) + edged_confirms
    print(f"# §E adjudication — view vs a synthetic edge-ablation baseline (status/Ω-blind)")
    print(f"# active OQs: {len(base)}")
    print(f"# MEANINGFUL comparisons: {meaningful}  = {len(contradicts)} contradict + {edged_confirms} substantive confirm")
    print(f"#   ({len(confirms) - edged_confirms} of the {len(confirms)} confirms are EDGE-FREE OQs where baseline AND")
    print(f"#    view both apply the workable_now default — a shared default, NOT corroboration.")
    print(f"#    Do NOT headline the raw confirm count.)")
    print(f"# raw: confirms {len(confirms)} / contradicts {len(contradicts)} / standoff {len(nontrivial)}")
    print(f"# CAVEAT: this baseline is derived from the SAME authored Deps as the view (an ablation of")
    print(f"#   status-check + Ω-routing), NOT an independent prioritization. The independent")
    print(f"#   adjudication (held cold-reader baseline: OQ-44 hub, OQ-56 keystone) is the separate")
    print(f"#   adjudicator's — README → 'Adjudicator's independent verdict'.\n")
    print("## CONTRADICT (view overturns the cold-reader's prose-surface call — routing earning its keep)")
    for oq, b, v, fact in contradicts:
        print(f"  {oq}: baseline={b:8s} -> view={v:18s} | settled by: {fact}")
    print(f"\n## CONFIRM (view and baseline agree): {len(confirms)} OQs")
    # show the ones that have edges (the non-trivial confirms)
    edged = [(oq, v) for oq, v in confirms if entries[oq].deps]
    for oq, v in edged:
        print(f"  {oq}: agree -> {v}")
    print(f"  (+ {len(confirms)-len(edged)} edge-free OQs both call workable_now)")


if __name__ == "__main__":
    main()
