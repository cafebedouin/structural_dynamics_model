#!/usr/bin/env python3
"""
OQ-49 override re-measure — read-only signature-override prevalence probe.

Reconstructs the ad-hoc OQ-49 probe (run in worktrees 2026-06-01, never saved) and
re-runs it on the LIVE corpora after the 2026-06-05 reset and OQ-70 (deletion of the
`claimed_natural` bait source, commit 72ec2cdd). See the plan
`review-oq-49-in-issues-md-twinkly-mochi.md` and the OQ-49 ISSUES.md block.

For every reading (= corpus_constraint) at the default analytical context:
  MT  = drl_core:metric_based_type_indexed/3   (override suppressed = raw metric read)
  FT  = drl_core:dr_type/3                      (override active)
  Sig = signature_detection:constraint_signature/2  (driving structural signature)
Override is *effective* iff MT != FT. Effective overrides split into
  confident-overwrite (MT != unknown)  — laundering-candidate
  unknown-fill        (MT == unknown)  — load-bearing-candidate
For every false_natural_law firing the FNL source is tagged source-1
(constraint_claim(_,mountain)) vs source-2 (natural_law_signature profile) — the test
of the OQ-70-collapse premise.

DISCIPLINE (CLAUDE.md):
  * ONE swipl process per corpus — controls are PER-PROCESS; sidesteps the Boltzmann/cache
    staleness trap and the overlay-ordering hazard.
  * Corpus overlaid with `asserta` (plain assertz is silently ignored -> loads default 57).
  * Load count confirmed from corpus_loader:corpus_constraint/1, cited against the manifest.
  * MANDATORY positive controls printed in every process (a collapsed/0 result is only a
    finding if the probe demonstrably CAN fire):
      PC_CLAUSE878  — resolve_modal_signature_conflict(snare,false_natural_law,R), R==tangled_rope
                      => the FNL->TR override clause computes a change (reachable).
      PC_SOURCE1    — assert a synthetic constraint_claim(_,mountain), confirm
                      claimed_natural/2 picks explicit_mountain_claim, retract.
      PC_LIVECHANGE — count of MT!=FT registered on the corpus (probe registers overrides).

Read-only: loads the engine and corpora, computes, prints. Writes NOTHING to the engine
or corpus. Raw output is captured by the caller into the audit directory.

Usage:
    python3 python/audits/oq49_override_remeasure.py            # all three corpora
    python3 python/audits/oq49_override_remeasure.py testsets   # one corpus
"""

import subprocess
import sys
from collections import Counter, defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"

CORPORA = ["testsets", "testsets_haiku", "testsets_flash"]

# ---------------------------------------------------------------------------
# Prolog probe goal. Printed as greppable token-prefixed lines on stdout.
#   LOADCOUNT <n>
#   CLAIMUNIVERSE <n>                 (# constraints carrying constraint_claim(_,mountain))
#   PC_CLAUSE878 <ok|fail>
#   PC_SOURCE1 <ok|fail>
#   PC_LIVECHANGE <n_effective>
#   ROW <id> <MT> <FT> <Sig> <eff:0/1> <fnl_source:none|source1|source2>
# Anything else on stderr is the engine's own load chatter.
# ---------------------------------------------------------------------------
_GOAL_TMPL = r"""
:- asserta(config:param(corpus_path, '{corpus}')).
:- [stack], corpus_loader:load_all_testsets.

probe_row(C) :-
    constraint_indexing:default_context(Ctx),
    ( drl_core:metric_based_type_indexed(C, Ctx, MT0) -> true ; MT0 = unknown ),
    ( drl_core:dr_type(C, Ctx, FT0) -> true ; FT0 = unknown ),
    ( signature_detection:constraint_signature(C, Sig0) -> true ; Sig0 = none ),
    ( MT0 == FT0 -> Eff = 0 ; Eff = 1 ),
    ( Sig0 == false_natural_law
      -> ( signature_detection:claimed_natural(C, explicit_mountain_claim)
           -> Src = source1
           ; signature_detection:claimed_natural(C, natural_law_signature_match)
           -> Src = source2
           ; Src = none )
      ; Src = none ),
    format("ROW ~w ~w ~w ~w ~w ~w~n", [C, MT0, FT0, Sig0, Eff, Src]).

run_probe :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("LOADCOUNT ~w~n", [N]),
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          narrative_ontology:constraint_claim(C, mountain) ), NClaim),
    format("CLAIMUNIVERSE ~w~n", [NClaim]),
    % PC_CLAUSE878: the FNL->TR override clause computes a type change.
    ( signature_detection:resolve_modal_signature_conflict(snare, false_natural_law, R878),
      R878 == tangled_rope -> format("PC_CLAUSE878 ok~n", []) ; format("PC_CLAUSE878 fail~n", []) ),
    % PC_SOURCE1: source-1 claimed_natural path reachable (synthetic, retracted).
    ( assertz(narrative_ontology:constraint_claim(pc_oq49_synthetic, mountain)),
      ( signature_detection:claimed_natural(pc_oq49_synthetic, explicit_mountain_claim)
        -> format("PC_SOURCE1 ok~n", []) ; format("PC_SOURCE1 fail~n", []) ),
      retractall(narrative_ontology:constraint_claim(pc_oq49_synthetic, _)) ),
    % Per-reading table.
    forall(corpus_loader:corpus_constraint(C), probe_row(C)),
    aggregate_all(count,
        ( corpus_loader:corpus_constraint(C),
          constraint_indexing:default_context(Ctx),
          ( drl_core:metric_based_type_indexed(C, Ctx, MT) -> true ; MT = unknown ),
          ( drl_core:dr_type(C, Ctx, FT) -> true ; FT = unknown ),
          MT \== FT ), NEff),
    format("PC_LIVECHANGE ~w~n", [NEff]).

:- ( run_probe -> true ; format("PROBE_ERROR run_probe failed~n", []) ), halt.
"""


def main(argv):
    corpora = argv[1:] if len(argv) > 1 else CORPORA
    overall_ok = True
    for corpus in corpora:
        print(f"\n{'='*78}\nCORPUS: {corpus}\n{'='*78}")
        # Write goal to a temp .pl and consult it (more robust than stdin user-stream).
        import tempfile
        with tempfile.NamedTemporaryFile("w", suffix=".pl", dir=PROLOG_DIR, delete=False) as tf:
            tf.write(_GOAL_TMPL.format(corpus=corpus))
            goal_path = tf.name
        try:
            proc = subprocess.run(
                ["swipl", "-q", goal_path],
                cwd=PROLOG_DIR,
                capture_output=True,
                text=True,
                timeout=900,
            )
        finally:
            Path(goal_path).unlink(missing_ok=True)

        out = proc.stdout
        # --- raw ROW sidecar (per-id, for the haiku/flash paired diff) ---
        audit_dir = ROOT / "audits" / "2026-06-14_oq49_remeasure"
        if audit_dir.is_dir():
            raw = [l for l in out.splitlines() if l.startswith("ROW ")]
            (audit_dir / f"rows_{corpus}.tsv").write_text("\n".join(raw) + "\n")
        # --- parse tokens ---
        loadcount = claimuniverse = livechange = None
        pc878 = pc_src1 = None
        rows = []
        probe_error = None
        for line in out.splitlines():
            t = line.split()
            if not t:
                continue
            if t[0] == "LOADCOUNT":
                loadcount = int(t[1])
            elif t[0] == "CLAIMUNIVERSE":
                claimuniverse = int(t[1])
            elif t[0] == "PC_CLAUSE878":
                pc878 = t[1]
            elif t[0] == "PC_SOURCE1":
                pc_src1 = t[1]
            elif t[0] == "PC_LIVECHANGE":
                livechange = int(t[1])
            elif t[0] == "ROW" and len(t) == 7:
                rows.append({
                    "id": t[1], "mt": t[2], "ft": t[3],
                    "sig": t[4], "eff": t[5] == "1", "fnl_src": t[6],
                })
            elif t[0] == "PROBE_ERROR":
                probe_error = line

        # --- report ---
        if probe_error:
            print(f"  !! {probe_error}")
            print("  stderr tail:")
            print("\n".join("    " + l for l in proc.stderr.splitlines()[-15:]))
            overall_ok = False
            continue

        print(f"LOADCOUNT          {loadcount}")
        print(f"CLAIMUNIVERSE      {claimuniverse}  (constraint_claim(_,mountain) carriers)")
        print(f"PC_CLAUSE878       {pc878}   (FNL->TR override clause reachable)")
        print(f"PC_SOURCE1         {pc_src1}   (source-1 claimed_natural reachable)")
        print(f"PC_LIVECHANGE      {livechange}  (MT!=FT registered on corpus)")
        n_pc_fail = sum(1 for v in (pc878, pc_src1) if v != "ok")
        if n_pc_fail:
            print("  !! POSITIVE CONTROL FAILED — collapsed result would be a dead probe; not a finding.")
            overall_ok = False

        sig_dist = Counter(r["sig"] for r in rows)
        eff_rows = [r for r in rows if r["eff"]]
        confident = [r for r in eff_rows if r["mt"] != "unknown"]
        unknown_fill = [r for r in eff_rows if r["mt"] == "unknown"]

        print(f"\n  signatures present (all {len(rows)} readings):")
        for sig, n in sig_dist.most_common():
            print(f"    {sig:32s} {n}")

        print(f"\n  override effective (MT!=FT): {len(eff_rows)}")
        print(f"    confident-overwrite (MT!=unknown): {len(confident)}")
        print(f"    unknown-fill        (MT==unknown): {len(unknown_fill)}")

        # per-clause/per-transition table
        trans = Counter((r["sig"], r["mt"], r["ft"]) for r in eff_rows)
        print(f"\n  per-transition (sig | MT->FT | N | sample):")
        samples = defaultdict(list)
        for r in eff_rows:
            k = (r["sig"], r["mt"], r["ft"])
            if len(samples[k]) < 2:
                samples[k].append(r["id"])
        for (sig, mt, ft), n in trans.most_common():
            print(f"    {sig:28s} {mt:10s}->{ft:12s} {n:5d}  {', '.join(samples[(sig,mt,ft)])}")

        # FNL source split
        fnl_rows = [r for r in rows if r["sig"] == "false_natural_law"]
        fnl_eff = [r for r in fnl_rows if r["eff"]]
        src_dist = Counter(r["fnl_src"] for r in fnl_rows)
        print(f"\n  FNL firings (all): {len(fnl_rows)}  | FNL override-effective: {len(fnl_eff)}")
        print(f"    source split (all FNL firings): {dict(src_dist)}")
        unaccounted = [r["id"] for r in fnl_rows if r["fnl_src"] == "none"]
        if unaccounted:
            print(f"    !! KILL CONDITION: {len(unaccounted)} FNL firing(s) tagged NEITHER source — "
                  f"third unaccounted path. DO NOT close. ids: {unaccounted[:10]}")
            overall_ok = False
        else:
            print(f"    every FNL firing tags source-1 or source-2 (collapse witness holds).")

    print(f"\n{'='*78}")
    print("ALL POSITIVE CONTROLS + KILL CONDITION:", "PASS" if overall_ok else "FAIL")
    return 0 if overall_ok else 1


if __name__ == "__main__":
    sys.exit(main(sys.argv))
