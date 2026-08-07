# PROPOSAL — OQ-261 forced-gluing experiment (pre-registered; FROZEN pending R2)

**Drafted:** 2026-08-07, after RECON.md (same dir) and before ANY C3 execution.
**Status: awaiting operator sign-off (ruling R2). The freeze is the seat — no C3 run
until the operator approves this document as written; any post-approval edit is a new
freeze.** No LLM spend anywhere in C3; compute-only Prolog/Python probes over the
existing corpus (manifest + fingerprints cited in RECON.md).

## The question

The ballot (a debate round's decision procedure) always emits a total verdict. OQ-261's
corrected mapping says it does so by sectioning a PERFORMANCE presheaf (how the round
was debated — the debate-community seats), not by gluing the TOPIC presheaf (the kernel's
reading family, which has H¹ > 0). The deliverable is a pre-registered observation that
DISTINGUISHES:

- **H_perf** — ballot = total verdict over the performance presheaf: the
  debate-community seats every fiat reading names. Predicts a global section exists on
  those seats even where the topic family obstructs.
- **H_topic** — ballot = gluing of the topic presheaf: the family reading vector itself.
  Predicts a total verdict is only available where the family glues; over a
  `real_closure` family a total verdict requires discarding/excluding readings, and that
  discard is observable.

## Objects (all already computed or computable read-only; frames per RECON §F)

1. **Topic presheaf, committer frame:** `cs_kernel_obstruction_status(fiat_efficacy_kernel)`
   = `real_closure` (H1r=2, Plur=13) — authored-edge-only. **Observer-blindness
   constraint: these inputs stay authored-edge-only; no observer read is ever fed in.**
2. **Topic presheaf, observer frame (comparison alongside, never an input):** per-context
   family H¹ (RECON §C: 73 glued / 80 obstructed / 3 undetermined; two blocs).
3. **Performance presheaf (new in C3):** for each of the 6 readings, the sub-vector of
   stakeholder-frame seat types (`stakeholder_seats:stakeholder_type_vector/2` domain)
   restricted to PERFORMANCE seats — seats whose referent is the debate community
   (debaters, coaches, judges, programs, theorists). Seat classification into
   performance vs topic-community is authored BY THIS PROPOSAL from the seat names in
   RECON §D's rosters, fixed before C3 runs (list below). Family-level performance
   vector = concatenation of the six readings' performance sub-vectors; gluing read =
   pure `grothendieck_cohomology:obstruction_from_vector/3` (H¹=0 ⟺ global section),
   OQ-51 null discipline throughout.
4. **Flat control:** same objects over `fiat_efficacy_kernel_flat_control` — same-topic,
   metric-range-interior, but NOT same-substrate (RECON §E: no cs_kernel_id, no
   coordination_type, near-disjoint seat roster). Claims scoped accordingly: the control
   contrasts "topic without the family cover," nothing stronger.

**Pre-fixed performance-seat classification (frozen with this proposal).** A seat is
PERFORMANCE iff its name denotes debate-round participants or the debate institution:
`competitive_debaters_running_fiat`, `opposing_debaters_forced_to_engage_analogy`,
`coaching_programs_teaching_activism_framing`, `debate_institutions_claiming_civic_relevance`,
`judges_and_tournament_administrators`, `academic_debate_community`,
`competitive_debaters_without_research_access`, `debate_theory_analysts`,
`declaring_debaters`, `competitive_debate_circuit`, `opposing_debaters_forced_into_frame`,
`debate_theory_observers`, `policy_debate_theorists`, `competitive_debate_coaches`,
`student_debaters`, `competitive_debate_participants`, `debate_coaches_and_programs`,
`competitive_debaters`, `debate_coaches`, `debate_league_administrators`,
`debate_theorists`, `interdisciplinary_synthesis_researchers`(EXCLUDED — research
community), `analytical_observer`(EXCLUDED — meta-seat). All other seats are
TOPIC-COMMUNITY. Ambiguity rule: a seat naming both (none currently) would be EXCLUDED
from both sub-vectors and reported.

## The 2×2 and what each cell means (written before running)

Rows: presheaf read (performance sub-vector | topic family). Columns: kernel family |
flat control. The gluing read per cell: H¹ over the pooled vector (with per-reading
sub-vectors also reported; OQ-51 null if <2 real seats).

- **Cell 1 — family × performance.** H¹=0 (glues): H_perf's load-bearing prediction
  holds — a total verdict is available on the performance seats OVER a family whose
  topic presheaf is `real_closure`. H¹>0 (obstructs): **H_perf falsified as stated**
  (the performance presheaf does NOT always admit a section); the mapping needs revision
  — record and stop, do not repair mid-run.
- **Cell 2 — family × topic.** Committer frame is already `real_closure` (no global
  section; RECON §B). The C3 read here is the DISCARD OBSERVATION: what must be removed
  for the pooled topic vector to glue (minimum readings excluded to reach H¹=0 —
  computable exactly by bloc structure). H_topic predicts a ballot modeled on this
  presheaf must visibly discard (exclude readings / abstain); if the pooled topic vector
  in fact glues with nothing discarded, the family's obstruction is not doing the work
  the experiment assumes — **the experiment's premise fails; abort to redesign** (this
  is the fireable falsifier for the premise, not for either H).
- **Cell 3 — control × performance.** H¹=0 expected under BOTH hypotheses
  (non-discriminating cell; recorded for symmetry). H¹>0 here: the performance-seat
  classification is unsound (control's policy-community roster has ≤1 performance seat —
  a performance read that obstructs on near-empty input violates OQ-51 null discipline;
  such an outcome means a probe bug, not evidence).
- **Cell 4 — control × topic.** H¹=0 or null (control is a singleton story — no family
  vector; the "topic presheaf" here is its own stakeholder vector, expected to glue or
  read null): consistent with the design — obstruction is the FAMILY's property.
  H¹>0: the control obstructs without a kernel cover — the family/control contrast
  cannot discriminate topic-obstruction from topic-noise; **abort to redesign**.

**Discrimination statement.** H_perf vs H_topic is decided by Cell 1 alone; Cells 2–4
are premise checks and symmetry controls. Cell 1 H¹=0 supports H_perf (and the corrected
OQ-261 mapping survives as testable content); Cell 1 H¹>0 falsifies H_perf as stated.
No cell outcome VERIFIES H_topic (the ballot's actual mechanism is not observable in
this corpus — only which presheaf CAN carry a total verdict); the writeup must not
upgrade "H_perf supported" into "ballot = performance sectioning proven."

**Fireable-falsifier check (each names a possible world):** Cell 1 H¹>0 is possible
(the 6 readings' performance seats could be typed differently — bloc B's seats might
type rope while bloc A's type tangled_rope, giving cross-bloc disagreement on
performance seats). Cell 4 H¹>0 is possible (a stakeholder vector with ≥2 disagreeing
real seats). Cell 2 glue-with-nothing-discarded is possible (if the pooled topic vector's
real seats happen to agree despite the committer-frame closure — the two axes are
independent, Theorem 7). None is contradictory under its own definitions.

## Second positive control (probe validity)

The family-H¹ probe (`obstruction_from_vector` over reading vectors) must detect
non-gluing where it is KNOWN: the `state_execution_authority` triplet
(abolition/retributive/deterrence, `archives/datasets/kernel_test/*.pl` — the registry's
known-divergent family; all 3 pairs diverge). C3 loads those three files alongside the
live corpus (additive `-l`, no corpus_path overlay) and runs the same probe; expected:
divergent contexts found (H¹>0 somewhere). If the probe reads uniform H¹=0 there, the
probe is broken — fix the probe, not the data, and re-run C3 from the start.

## Manufactured-consensus cross-check (OQ-261 item (c))

Recorded ALONGSIDE, per-story only (frame-mismatch note, RECON §F): all 7 stories
already read `manufactured_consensus_candidate[_untypeable]` (RECON §D). C3 re-reports
these tokens next to the cell results and the excluded-seat lists; it does NOT aggregate
them into a family-level verdict (no such surface exists — any aggregation here would
manufacture the very absorption the OQ names).

## Placement

The family-frame H¹ read stays an audit-dir probe this session (thin join over two
exported predicates). Promotion to the engine happens ONLY if the writeup names
"substitute-presheaf verdict channel" as an engine object — then same-change mandatory:
registration in `prolog/reading_registry.pl` (one `aggregatable_reading` line + typed
absence tokens on singletons, OQ-51 convention).

## Outputs (C3, after R2 sign-off)

`performance_presheaf_probe.pl` + raw output; cell table; discard-minimum computation;
positive-control output; all in this dir. WRITEUP.md (C4) quotes this proposal verbatim
in its frozen form and declares residue.
