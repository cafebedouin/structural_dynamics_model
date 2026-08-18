# Stakeholder-Layer Migration Plan (framing-perturbation-aware)

**Basis:** `AUDIT.md` (this dir), all verdicts witnessed 2026-06-07. Tracker entry: ISSUES.md OQ-83.

**Governing reframe:** this migration is the engine's first framing-perturbation. The
(P,T,E,S) tuple is the framing; observer sweeps are within-framing. The diff between the
same topics authored under both surfaces is the PRIMARY MEASUREMENT of the move, and the
four-tuple arm is the control arm. **R4 (reversed from multimodel consensus): the
four-tuple surface is not retired before the cross-framing diff is produced and
preserved.** Straddle (one corpus, mixed schemas) rejected; preserve-the-pair (two clean
parallel corpora on shared topics) required.

**Rulings of record** (operator, 2026-06-06/07 — build to these):
- **R1** drop authored per-seat perception (`classification_type` per perspective). Reason:
  redundancy + perturbability (perceived type computable from power/exit per Axiom 2;
  authored field inert, computed field perturbable) — not merely leakage. KEEP the
  computed perceived-vs-real gap (seat-level false-mountain diagnostic). Story-level
  `claimed_type` stays (diegetic).
- **R2** role dial-set {agenda_setter, beneficiary, payer, excluded, observer}: DECLARED,
  SWAPPABLE, caller-supplied — never "complete topology." Stated bundling: backgrounds
  role's time-index (static roles now; time-varying role/d is OQ'd, operator-ruled
  2026-06-07). **Contender ruled NO (operator, 2026-06-07): contention is a RELATION
  between seats, derived by the engine from two same-power stakeholders with opposed
  roles — never authored as a role attribute.** Authoring it would let the author assert
  the conclusion the Phase-A falsifier measures (the contention story must flip because
  the engine computed it, not because the author typed it). The A4 contender residue
  (6.3%) stays in the ledger as declared evidence the frame treats contention
  relationally. Dial-set is declared at FIVE roles; the engine layer (step 3) gains a
  derived `in_contention(C, Name1, Name2)` relation as the computed counterpart.
- **R3** `excluded` is commentary-grade, never correction-grade. Consumer: the
  consensus-provenance check (unanimous-mountain because real, or because dissenting
  seats were never in the room).
- **R5** sixth question (genealogy/obsolescence): authored field + MISMATCH consumer only
  (founding-problem dead + world-rearranges ⇒ capture/zombie flag); narrative never
  consumed as a claim; R3-style corroboration. Per A7: REWIRE the dangling mandatrophy
  apparatus (schema `mandatrophy_resolved` → 0 emissions; `has_mandatrophy_declaration` /
  `is_mandatrophy_resolved` read inputs nothing produces) and cross-check against computed
  piton/theater. One canonical thing — extend, don't fork. **R5 is RECOVERY of a
  pre-existing dangling intent, not net-new scope** (mandatrophy = the
  authoritarian-grip-limits thesis; see OQ-83 post-audit rulings). **Abandonment-reason
  check CLOSED (2026-06-07, git-witnessed): the emission was never written in any
  version — a dropped seam at the hand-authored→JSON-template migration (`6f997d71` →
  `3641ae71`), not a gameability wall. R5 inherits no hidden design wall.** The
  silent-mistake is promoted to CLAUDE.md Critical Distinctions until the rewire lands
  (then retire the note). Committer-axis delegation of the "still live" half → deferred OQ.
- Type C vs Type B: deferred — ruled against the Phase-A diff, not in advance.

**Build constraints (stated, non-negotiable):** one-axis-at-a-time;
declare-the-key-never-bake-it (role dial-set and all alignment keys are explicit
arguments / config, never silent defaults); fail-closed on absence (authored-empty ≠
absent — Pattern 5); every new producer wired to a consumer in the same change
(Pattern 1).

---

## Phase A — additive (no existing behavior changes)

1. **Schema** (`schemas/constraint_story_schema.json`): add OPTIONAL `stakeholders[]`
   alongside `perspectives[]`. Per stakeholder: `name` (snake_case, domain-specific),
   `role` (from the DECLARED dial-set — schema `$comment` carries the declaration +
   bundling statement + A4 ledger pointer), `power`, `time_horizon`, `exit_options`,
   `spatial_scope`, five-questions text fields, R5 genealogy fields
   (`founding_problem`, `founding_problem_status` ∈ {live, dead, contested},
   `corroboration` — R3-style provenance), optional `agent` boolean gate (A4 non_agent
   class; reuse the non_agent registry principle). Authored-empty rule:
   `stakeholders: []` requires explicit `disappearance_test: world_unchanged`.
   Multi-role: keep single `role` + optional `secondary_role` (A4 dual_role class,
   ~1.3%) — operator may simplify at review.
2. **Compiler** (`python/generate_constraint_pl.py`): emit
   `narrative_ontology:stakeholder/7` facts + derive `constraint_beneficiary`/`victim`
   from roles (beneficiary→beneficiary, payer→victim), preserving per-story
   domain-specific naming. **A6 constraint: stakeholder names ride the NEW predicate
   only; no name stabilization across readings into beneficiary/victim** (guard
   asymmetry: `inferred_coupling_protocol.pl:218–222` consumes `shared_agent_link` with
   no intra-kernel filter — re-coupling there would be silent). The asymmetry itself is
   split out as **OQ-84** (engine-hygiene, true independent of this migration; operator
   2026-06-07): resolve-or-rule it before Phase A lands sibling readings, but it is not
   gated on the migration.
3. **Engine (additive module, e.g. `stakeholder_seats.pl`):** deterministic projection
   stakeholder → context/4; `derive_directionality_for_stakeholder(C, Name, D)` keyed
   (C, Name) — role-derived base d (agenda_setter/beneficiary low, payer/excluded high)
   + exit modulation, overridable per-(C,Name); `dr_type_for_stakeholder(C, Name, Type)`
   = dr_type at the projected context with the per-stakeholder d; derived
   `in_contention(C, Name1, Name2)` (two same-power stakeholders, opposed roles — the
   computed counterpart of the ruled-out contender role); computed perceived-vs-real
   gap per seat (R1 keep); R3 consensus-provenance check (commentary-grade output only);
   R5 mismatch consumer wired into the mandatrophy rewire (A7), zombie flag
   cross-checked against computed piton. Canonical-4 / product-156 measurement sites
   untouched.
   **Step-3 sequencing constraints (operator pins, 2026-06-07):**
   - **A6 guard lands BEFORE-OR-WITH the projection, in the same pass — never as a
     "step 3b".** Step 2's A6-clean covers derived names only; projection introduces
     the bespoke authored names (the 504/25/38 population) against the unguarded
     `inferred_coupling_protocol.pl:218–222`. Either mirror the
     `drl_purity_network.pl:96–98` guard at that site, or carry an explicit OQ-84
     operator ruling that the asymmetry is intentional — landed in the same change
     as the projection, with the edge-set diff witnessed.
   - **Step 3 is the first non-purely-additive step** (five wirings). Scope correction
     (operator, 2026-06-07): step 3's run is the MECHANISM witness — it cannot fail on
     hand-authored stories with opposed config params, so it witnesses that distinct d
     flows through the name-keyed path to distinct type (the plumbing), via the CONTRAST
     against the atom-keyed path on the same substrate. The EXPERIMENT — can the framing
     change move a verdict on independently authored arms, ε pinned — is step 4
     (generated arms), and the real straitjacket verdict lives there.
   - **STEP 3 LANDED 2026-06-07.** Wirings: `extractiveness_for_agent_d/4` (delegation
     refactor, A1-harness byte-identical), `dr_type_with_d/4`, `stakeholder_seats.pl`
     (projection, per-(C,Name) d from the declared role-d seat params, `in_contention/3`,
     `seat_perceived_vs_real/4`, `consensus_provenance/2`, `zombie_piton_crosscheck/2`),
     narrative_ontology fact decls + R5 zombie clause, OQ-84 guard at
     `inferred_coupling_protocol` (bug branch, git-witnessed). Mechanism witness
     (step3_mechanism.txt): atom-keyed `[0.15]`/one type vs name-keyed 0.12/0.85 split,
     causally traced via payer-param overlay; control story no-split; guard 72=72 live +
     synthetic 1→0; zombie overlay fires / live 0 / restore clean. Untested this pass
     (deliberate): exit-mod arms beyond trapped(+0.05) and the d clamp.
4. **Pilot (the measurement):** N shared topics (incl. ≥1 two-powerful-agents contention
   story and ≥1 mountain-profile story) authored under BOTH surfaces. ε per A5: generate
   unpinned; compute the cross-framing diff twice — raw AND ε-pinned via post-hoc overlay
   (witnessed feasible, A1-mut-ii machinery); report ε-delta as its own axis. Compute
   each arm's signature-resolved orbit; **produce and PRESERVE the cross-framing-
   invariance diff** (which observer-axis mountains survive the framing change, which
   flip). Both arm corpora are preserved (separate run-tagged dirs; never co-loaded —
   the corpus_loader glob isolates subdirs, see CLAUDE.md Corpus Loading).
   **FLIP CRITERION — PINNED 2026-06-07, before step 4's plan or run (canonical text in
   ISSUES.md OQ-83):** escape requires all four of (1) the stakeholder arm AUTHORS the
   same-power opposition (else UNEVALUABLE, a generation finding); (2) the d-split
   survives ε-pinning (pinned run load-bearing); (3) the four-tuple arm's collapse is
   witnessed all-solutions; (4) the non-contention topic shows no split on generated
   arms. Renamed-not-escaped verdicts pre-declared, incl. the system-level one: if the
   generator cannot author same-power opposition at all, the schema escaped but the
   pipeline re-imposed the straitjacket. The criterion is not revised against step-4
   output.
   - **Declared falsifier (straitjacket fix):** A1/A2 prove the computed path ignores
     perspectives and the collapse exists; neither proves per-(C,Name) d RESOLVES the
     collapse. The contention story must flip across framings. **If it does not flip,
     the stakeholder layer renamed the straitjacket, not escaped it.**
   - The Phase-A diff is also the evidence for the deferred Type-C/B ruling (operator).

## Phase B — cutover (only after the Phase-A diff is pasted and accepted)

Prompt rewrite (`prompts/constraint_story_generation_prompt_json.md` — five-questions +
R5 interview replaces the P/T/E/S + per-seat-type sections); NEW example replacing
`agent/verification_bottleneck.json` (kills the OQ-70 mountain-template bait convention
— **cutover re-opens FNL-regime re-witnessing**; signature-prevalence stats reset
again); consumer migration per AUDIT A3 table — (a)-class Prolog (perspectival_gap →
computed-over-seats; mountain-unanimity retire-or-recompute; validation/repair gates →
stakeholder presence; report_generator mandatrophy gaps → rewired R5 consumer) and
(a)-class Python; linter rules (powerless/institutional-required → role-coverage rules;
Rule 18 → per-(C,Name) overrides); AUDIT OPEN-1/2 (cross_context_analysis,
boltzmann_compliance:472) resolved as part of this migration, witnesses pasted.
`reading_diff` stays pointed at authored cells through Phase B — it is the four-tuple
arm's instrument (A3).

## Phase C — regenerate / retire (never before B)

Full corpus regen under the new schema; `perspectives[]` retired from the schema;
`reading_diff` re-pointed (stakeholder cells / computed seats); the A/B pair corpora
archived under `prolog/archives/datasets/` with manifests (control arm preserved
permanently). Old-vs-new output diff per the build-discipline witness rule
*prove before you replace* before any standalone is deleted.

---

**Effort (unchanged from feasibility estimate):** schema+example 1 session; prompt 1;
compiler+linter 1; engine layer + consumer migration 1–2; pilot+diff 1; regen ~0 (compute).
