# Findings — OQ-92 step-3 build stages (running log; one section per landed stage)

## Stage A — schema (landed 2026-06-10; witness: `stage_a_schema_witness.{py,txt}`)

`gain_flow` + `fixing_cost` as optional root properties (tri-valued semantics, fabrication ban
and compiler-enforcement notes in the descriptions; two stakeholders-dependency riders). 8/8
witness cases: three provenance shapes valid; wrong-type / bad-enum / both dependency negatives
bit at their intended guards. Two-sided additivity: 91/134 `json/` specs invalid IDENTICALLY
pre- and post-change (zero new, zero fixed) — pre-existing residue of the 2026-06-09
required-fields tightening (KNOWN_STATE 2026-06-09), now numbered; latent only (`run_pipeline.py`
does not read `json/`; the generator validates on entry).

**Documented-open at Stage A:** the ghost-seat case (case 8) — schema-valid BY DESIGN, because
Draft7 cannot express the cross-field reference; integrity deferred to the compiler.

## Stage B — compiler (landed 2026-06-10; witness: `stage_b_compiler_witness.txt`, pilots in `pilots/`)

Emission of `narrative_ontology:stakeholder_gain_flow/2` + `fixing_cost_class/2` in
`generate_constraint_pl.py` (decls + facts, conditional on AUTHORED presence); referential
integrity check at the top of `generate_pl()` — on every generation path, `--no-validate` does
not bypass it; declarations added to `narrative_ontology.pl` (multifile + dynamic).

Witnesses, two-sided as pre-committed:
1. **0-diff keystone:** 134/134 `json/` specs byte-identical old-vs-new compiler
   (`--no-validate` both sides so older-regime specs are covered), zero exit divergence.
2. **Pilot branches:** named-seat and diffuse pilots emit exactly one fact per field; the
   absent pilot emits ZERO — a silence witnessed against its two firing twins, not a dead grep.
3. **Ghost seat REJECTED loud on both paths** (exit 1; the ValueError names the ghost AND the
   valid seat list; `--no-validate` also rejects). **This closes the loop Stage A left open:
   the case documented schema-valid-by-design with integrity deferred (Stage A case 8) is now
   the REJECTED pilot's subject — the deferral came due and was paid.**
4. **swipl load:** the named pilot consults clean against the updated `narrative_ontology.pl`;
   both facts queryable (`receiver=aging_blue_collar_workers`, `class=prohibitive`).

Per the derived-diffs expectation: no story authors the fields yet, so `pipeline_output.json`
and classification are untouched by construction (emission conditional on presence; 0-diff
sweep is the witness).

## Next

**Stage C (prompt)** — authoring guidance, example value varied/omitted (OQ-70). **Before the
first Stage-C batch is READ: the diffuse-audit "obvious capturing seat" criterion must be
written — operator-in-the-loop by design** (preregistration Q1: criterion pre-written, K=0 on
the observable, N = whole-batch-or-≥30, "0/N observed" never "clean"). Everything between Stage
B and the criterion is execution; the criterion is the next human gate. Stage D (classification
wiring: `seat_captures`, piton reads, benignity gates rows 1–3 + maxent congruence) follows the
audit.
