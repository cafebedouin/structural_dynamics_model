#!/usr/bin/env python3
"""OQ-110 §1.1 — diff the in-process residual export against pipeline_output.json.

Identity ⇒ the temporal_residual blocks downstream consumers read are equal to an
independent in-engine recomputation (fresh swipl process) on the same corpus+code.
Positive control: a deliberate mutation of one parsed value must be reported as a
mismatch by the same comparator before the real comparison is trusted.

Run from repo root: python3 audits/2026-06-11_oq110_residual_join/backed_e2e_diff.py
"""
import copy
import json
import sys

PIPE = "outputs/pipeline_output.json"
INPROC = "outputs/oq110_residual_inprocess.json"


def load():
    pipe = json.load(open(PIPE))
    inproc = json.load(open(INPROC))
    pipe_tr = {e["id"]: e.get("temporal_residual") for e in pipe["per_constraint"]}
    return pipe["manifest"], pipe_tr, inproc


def compare(pipe_tr, inproc):
    mismatches = []
    ids_p, ids_i = set(pipe_tr), set(inproc)
    for cid in sorted(ids_p ^ ids_i):
        mismatches.append((cid, "present only in one side: pipeline=%s inproc=%s"
                           % (cid in ids_p, cid in ids_i)))
    for cid in sorted(ids_p & ids_i):
        if pipe_tr[cid] != inproc[cid]:
            mismatches.append((cid, "block differs:\n  pipeline: %r\n  inproc:   %r"
                               % (pipe_tr[cid], inproc[cid])))
    return mismatches


def main():
    manifest, pipe_tr, inproc = load()
    print("manifest: run_at=%(pipeline_run_at)s commit=%(code_commit_short)s "
          "dirty=%(code_dirty)s n=%(n_constraints)s" % manifest)
    print("pipeline ids: %d | in-process ids: %d" % (len(pipe_tr), len(inproc)))

    # Positive control: the comparator must flag a planted difference.
    planted = copy.deepcopy(inproc)
    victim = next(cid for cid, blk in planted.items() if blk)
    ctx = next(iter(planted[victim]))
    planted[victim][ctx]["flips"] += 1
    ctl = compare(pipe_tr, planted)
    if not any(cid == victim for cid, _ in ctl):
        print("POSITIVE CONTROL FAILED: planted mutation in %s not flagged" % victim)
        sys.exit(1)
    print("positive control: planted flips+1 in %s/%s flagged (%d mismatch[es]) — comparator fires"
          % (victim, ctx, len(ctl)))

    mismatches = compare(pipe_tr, inproc)
    if mismatches:
        print("MISMATCHES: %d" % len(mismatches))
        for cid, msg in mismatches:
            print(" -", cid, msg)
        sys.exit(1)
    nulls = sorted(cid for cid, blk in inproc.items() if blk is None)
    print("IDENTITY over %d constraints (incl. %d null = no measurement/5 facts: %s)"
          % (len(inproc), len(nulls), ", ".join(nulls)))


if __name__ == "__main__":
    main()
