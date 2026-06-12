#!/usr/bin/env python3
"""OQ-110 §1.3 — aggregate the pin results into the pre-registered buckets.

Pre-registered (plan §1.3, pinned): ε-explained iff the ε-pinned type at T2 does not
produce the type change (pinned type != To). Survivors of the ε-pin are ε-unexplained
(residual), bucketed by the supp twin: supp-explained (vanish under supp pin) vs genuinely
unexplained (survive both single pins). Outcome meanings written before the run:
residual ε-unexplained-and-genuinely-unexplained count = 0 -> D-fork branch b does NOT
open; > 0 -> the bucketed residual set is the operator's branch-b decision package.

Cross-checks (halt on failure):
- the pin enumeration's (id, context, t1, t2, from, to) set must equal the join
  inventory's (1.2) — same flip set, two independent enumerations;
- no FAIL entries (errored is not a verdict);
- THIRD-TYPE visibility: ε-pinned types that are neither From nor To are listed; the
  criterion counts them as "type change not produced" (ε-explained). If any third-type
  case exists AND reclassifying it as surviving would move the genuinely-unexplained
  count across the 0/>0 boundary, HALT-AND-ESCALATE (criterion under-specified for the
  realized data) rather than amend inline.

Run from repo root: python3 audits/2026-06-11_oq110_residual_join/pin_aggregate.py
Writes: outputs/oq110_pin_aggregate.json
"""
import csv
import json
import sys

TSV = "outputs/oq110_pin_results.tsv"
JOIN = "outputs/oq110_residual_join.json"
OUT = "outputs/oq110_pin_aggregate.json"


def main():
    join = json.load(open(JOIN))
    manifest = join["manifest"]
    rows = list(csv.DictReader(open(TSV), delimiter="\t"))

    # --- enumeration identity vs the join inventory ---
    pin_set = {(r["constraint"], r["context"], int(r["t1"]), int(r["t2"]),
                r["from"], r["to"]) for r in rows}
    join_set = {(f["id"], f["context"], f["t1"], f["t2"], f["from"], f["to"])
                for f in (dict(x) for x in join_inventory(join))}
    if pin_set != join_set:
        print("HALT: pin enumeration != join inventory")
        print("  only in pins:", sorted(pin_set - join_set))
        print("  only in join:", sorted(join_set - pin_set))
        sys.exit(1)

    fails = [r for r in rows if "FAIL" in (r["eps_pin_type"], r["supp_pin_type"])]
    if fails:
        print("HALT: %d pin classifications FAILED (errored is not a verdict)" % len(fails))
        for r in fails:
            print("  -", r)
        sys.exit(1)

    eps_explained, supp_explained, unexplained, third_type = [], [], [], []
    for r in rows:
        if r["eps_pin_type"] != r["to"]:
            eps_explained.append(r)
            if r["eps_pin_type"] != r["from"]:
                third_type.append(r)
        elif r["supp_pin_type"] != r["to"]:
            supp_explained.append(r)
        else:
            unexplained.append(r)

    # third-type boundary halt: would counting them as survivors move 0/>0?
    if third_type and len(unexplained) == 0:
        # reclassifying any third-type as surviving would open the fork from 0
        print("HALT-AND-ESCALATE: %d third-type eps-pin outcome(s) and genuinely-"
              "unexplained=0 — the pinned criterion under-specifies the realized data;"
              " operator must rule before bucketing stands." % len(third_type))
        for r in third_type:
            print("  -", r)
        sys.exit(2)

    out = {
        "manifest": manifest,
        "criterion": "pre-registered plan §1.3: eps-explained iff eps-pinned type != to;"
                     " residual bucketed by supp twin",
        "n_backed_flips": len(rows),
        "buckets": {
            "eps_explained": len(eps_explained),
            "supp_explained_residual": len(supp_explained),
            "genuinely_unexplained_residual": len(unexplained),
        },
        "third_type_eps_pin_outcomes": third_type,
        "supp_explained_rows": supp_explained,
        "genuinely_unexplained_rows": unexplained,
    }
    json.dump(out, open(OUT, "w"), indent=1)

    m = manifest
    print("OQ-110 pin aggregate | substrate: run_at=%s commit=%s dirty=%s n=%s"
          % (m["pipeline_run_at"], m["code_commit_short"], m["code_dirty"],
             m["n_constraints"]))
    print("enumeration identity: pin set == join inventory (%d flips)" % len(rows))
    print("buckets over %d backed flips:" % len(rows))
    print("  eps-explained:                 %3d" % len(eps_explained))
    print("  supp-explained residual:       %3d  (NOT evidence for time-varying d)"
          % len(supp_explained))
    print("  genuinely unexplained residual:%3d" % len(unexplained))
    print("third-type eps-pin outcomes: %d" % len(third_type))
    for r in third_type:
        print("  - %(constraint)s %(context)s t=%(t1)s->%(t2)s %(from)s->%(to)s"
              " eps-pin->%(eps_pin_type)s" % r)
    for label, rs in (("SUPP-EXPLAINED", supp_explained),
                      ("GENUINELY UNEXPLAINED", unexplained)):
        if rs:
            print("%s rows:" % label)
            for r in rs:
                print("  - %(constraint)s %(context)s t=%(t1)s->%(t2)s"
                      " %(from)s->%(to)s eps-pin->%(eps_pin_type)s"
                      " supp-pin->%(supp_pin_type)s (eps %(eps1)s->%(eps2)s,"
                      " supp %(supp1)s->%(supp2)s)" % r)
    # The pre-registered fork keys on the eps-UNEXPLAINED count (flips surviving
    # eps-pinning) = supp_explained + genuinely_unexplained — NOT on genuinely-
    # unexplained alone. (v1 of this script printed a verdict keyed to the latter;
    # corrected to the pinned text. Buckets unchanged.)
    n_residual = len(supp_explained) + len(unexplained)
    verdict = ("eps-unexplained residual count = 0 -> D-fork branch b does NOT open"
               " (pre-registered outcome meaning)" if n_residual == 0 else
               "eps-unexplained residual count = %d (> 0) -> the bucketed residual set"
               " is the operator's branch-b decision package; the package ESCALATES,"
               " nothing auto-opens" % n_residual)
    print("pre-registered outcome: %s" % verdict)
    print("wrote %s" % OUT)


def join_inventory(join):
    for entry in join["join"]:
        for f in entry["flips"]:
            yield {"id": entry["id"], "context": f["context"], "t1": f["t1"],
                   "t2": f["t2"], "from": f["from"], "to": f["to"]}


if __name__ == "__main__":
    main()
