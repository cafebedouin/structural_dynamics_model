#!/usr/bin/env python3
"""OQ-72 baker: ratified assignments TSV -> prolog/axiom_concept_registry.pl.

The registry .pl is CANONICAL; the ratified TSV is the audit witness. The baker
REFUSES (exit nonzero, no write) on any unratified or malformed row — fail-closed:
an unratified row is an absence of the operator's seat, and absence must not bake
into a success-shaped registry (Build Discipline Pattern 5).

Refusal conditions (each names the offending row):
  - wrong column count
  - ratified_status != 'ratified'
  - concept atom not kernel-namespaced ('<kernel>__<slot>') and not 'no_slot'
  - same axiom name mapped to two different concepts (name-keyed registry:
    conflicting duplicates would make the mapping order-dependent)

Usage: python3 python/axiom_concept_bake.py [ratified.tsv] [out.pl]
Defaults: audits/2026-07-03_oq72_concept_key_pilot/assignments_ratified.tsv
          prolog/axiom_concept_registry.pl
"""
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[1]
DEFAULT_TSV = REPO / "audits/2026-07-03_oq72_concept_key_pilot/assignments_ratified.tsv"
DEFAULT_OUT = REPO / "prolog/axiom_concept_registry.pl"
ATOM = re.compile(r"^[a-z][a-z0-9_]*$")

HEADER = """% ============================================================================
% AXIOM_CONCEPT_REGISTRY — the RATIFIED alignment seat for axiom_diff's
% `concept` key (OQ-72).
% ============================================================================
% SEAT DECLARATION: every fact below is a RATIFIED human ruling, operator as
% author of record — not a discovered fact. The concept key relocates the
% axiom-alignment ruling from per-pair hand-authoring to vocabulary granularity;
% it does NOT move the axiom axis to "discovered" footing. Criterion: same
% subject / functional slot (opposed poles of one subject share a concept).
% Concept atoms are KERNEL-SCOPED (`<kernel_id>__<slot>`) — no cross-kernel
% equivalence is expressible. The registry KEY is the bare axiom name: a
% mapping applies wherever that name occurs, on any corpus leg.
%
% CANONICAL: this file. Audit witness: the ratified TSV named per tranche.
% Regenerate ONLY via python/axiom_concept_bake.py (fail-closed on unratified
% rows); hand-edits lose their ratification provenance.
%
% PROVENANCE (tranche 1, pilot):
%   proposal:  audits/2026-07-03_oq72_concept_key_pilot/PROPOSAL.md (R1 2026-07-04)
%   ratified:  audits/2026-07-03_oq72_concept_key_pilot/assignments_ratified.tsv
%              (R3 2026-07-04, operator; false-merge 0/71)
%   vocab:     audits/2026-07-03_oq72_concept_key_pilot/vocabulary_draft.md
%              (R2 2026-07-04, ratified as drafted)
%   flash reach: animals_are_rights_bearing_individuals also occurs in
%     testsets_flash/animal_moral_status__abolitionist_reading.pl (same kernel,
%     same reading role) — the name-keyed mapping reaches it by construction;
%     ratified with this disclosure (R3 ask 3).
% ============================================================================

:- multifile axiom_diff:axiom_concept/2.

"""


def fail(msg: str) -> None:
    print(f"REFUSED: {msg}", file=sys.stderr)
    sys.exit(1)


def main() -> None:
    tsv = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_TSV
    out = Path(sys.argv[2]) if len(sys.argv) > 2 else DEFAULT_OUT
    lines = tsv.read_text().splitlines()
    header = lines[0].split("\t")
    try:
        i_kernel = header.index("kernel")
        i_axiom = header.index("axiom")
        i_concept = header.index("proposed_concept")
        i_status = header.index("ratified_status")
    except ValueError as e:
        fail(f"missing required column: {e}")
    ncol = len(header)
    facts: dict[str, str] = {}
    n_no_slot = 0
    for lineno, l in enumerate(lines[1:], start=2):
        f = l.split("\t")
        if len(f) != ncol:
            fail(f"line {lineno}: {len(f)} columns, expected {ncol}")
        kernel, axiom, concept, status = f[i_kernel], f[i_axiom], f[i_concept], f[i_status]
        if status != "ratified":
            fail(f"line {lineno}: axiom '{axiom}' has ratified_status='{status}' "
                 f"(only 'ratified' rows may bake)")
        if not ATOM.match(axiom):
            fail(f"line {lineno}: malformed axiom name '{axiom}'")
        if concept == "no_slot":
            n_no_slot += 1
            continue
        if not concept.startswith(kernel + "__") or not ATOM.match(concept):
            fail(f"line {lineno}: concept '{concept}' is not kernel-namespaced "
                 f"for kernel '{kernel}'")
        if axiom in facts and facts[axiom] != concept:
            fail(f"line {lineno}: axiom '{axiom}' already mapped to "
                 f"'{facts[axiom]}', conflicting '{concept}' (name-keyed registry)")
        facts[axiom] = concept
    body = "".join(
        f"axiom_diff:axiom_concept({a}, {c}).\n" for a, c in sorted(facts.items()))
    out.write_text(HEADER + body)
    print(f"baked {len(facts)} axiom_concept/2 facts ({n_no_slot} no_slot rows "
          f"skipped by design) -> {out}")


if __name__ == "__main__":
    main()
