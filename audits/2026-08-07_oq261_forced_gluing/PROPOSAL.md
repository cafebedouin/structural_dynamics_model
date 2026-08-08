# PROPOSAL — OQ-261 forced-gluing experiment (v2, pre-registered; FROZEN pending R2)

**Drafted:** 2026-08-07 after RECON.md, before ANY C3 execution. **v2 amended
2026-08-07 per operator amendments 1–4 on the v1 freeze (v1 was never signed; v1 text
in git history at `83a647ea`). Amendment log at bottom.**
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

## Blinding declaration (amendment 1 — on the record)

**Seat-type information WAS in hand when the performance-seat classification was
authored.** RECON §C (the two-bloc structure, the readings' `claimed_type`s) and RECON
§D (per-story seat reads) were computed before this proposal fixed the partition, and
v1's falsifier text explicitly reasoned about which bloc's seats might type which way.
The partition below was authored from seat NAMES, but a name-partition that happens to
glue could have been selected by an eye already exposed to the type structure — the
ordinary route, not a dishonest one. Mitigation is structural, not testimonial:
**three partition variants run, all pre-registered here, and Cell 1 is reported under
all three.** H¹=0 under all three is robust to the classification seat; H¹=0 under only
some is a finding about the partition, not the presheaf, and the writeup must say so.

## Objects (all computed or computable read-only; frames per RECON §F)

1. **Topic presheaf, committer frame:** `cs_kernel_obstruction_status(fiat_efficacy_kernel)`
   = `real_closure` (H1r=2, Plur=13) — authored-edge-only. **Observer-blindness
   constraint: these inputs stay authored-edge-only; no observer read is ever fed in.**
2. **Topic presheaf, observer frame (comparison alongside, never an input):** per-context
   family H¹ (RECON §C: 73 glued / 80 obstructed / 3 undetermined; two blocs).
3. **Performance presheaf (new in C3):** per-reading sub-vectors of stakeholder-frame
   seat types (`stakeholder_seats:stakeholder_type_vector/2` domain) restricted to
   PERFORMANCE seats, pooled across the family; gluing read = pure
   `grothendieck_cohomology:obstruction_from_vector/3` (H¹=0 ⟺ global section), OQ-51
   null discipline at BOTH levels (per-reading sub-vectors reported; the Cell-1 VERDICT
   is over the POOLED vector — amendment 2).
4. **Flat control:** same objects over `fiat_efficacy_kernel_flat_control` — same-topic,
   metric-range-interior, NOT same-substrate (RECON §E). Claims scoped accordingly.

## Performance-seat partition — three frozen variants (amendment 1)

**V_frozen (primary).** A seat is PERFORMANCE iff its name denotes debate-round
participants or the debate institution:
`competitive_debaters_running_fiat`, `opposing_debaters_forced_to_engage_analogy`,
`coaching_programs_teaching_activism_framing`, `debate_institutions_claiming_civic_relevance`,
`judges_and_tournament_administrators`, `academic_debate_community`,
`competitive_debaters_without_research_access`, `debate_theory_analysts`,
`declaring_debaters`, `competitive_debate_circuit`, `opposing_debaters_forced_into_frame`,
`debate_theory_observers`, `policy_debate_theorists`, `competitive_debate_coaches`,
`student_debaters`, `competitive_debate_participants`, `debate_coaches_and_programs`,
`competitive_debaters`, `debate_coaches`, `debate_league_administrators`,
`debate_theorists`, `novice_debaters_and_students` (control roster — the list is the
rule's extension over ALL SEVEN rosters, control included). EXCLUDED from both
sub-presheaves:
`interdisciplinary_synthesis_researchers` (research community), `analytical_observer`
(meta-seat). All other seats are TOPIC-COMMUNITY.

**V_inclusive (adversarial, maximally inclusive).** V_frozen plus the two EXCLUDED
seats folded back into PERFORMANCE (`interdisciplinary_synthesis_researchers`,
`analytical_observer`).

**V_restrictive (adversarial, round participants only).** Only seats denoting people IN
the round — debaters and judges; institutions, coaches, programs, leagues, circuits,
theorists, observers, researchers, communities all drop:
`competitive_debaters_running_fiat`, `opposing_debaters_forced_to_engage_analogy`,
`judges_and_tournament_administrators` (bundles tournament admins — kept, bundling
noted), `competitive_debaters_without_research_access`, `declaring_debaters`,
`opposing_debaters_forced_into_frame`, `student_debaters`,
`competitive_debate_participants`, `competitive_debaters`.

Ambiguity rule (all variants): a seat matching both classes would be EXCLUDED from both
sub-vectors and reported (none currently).

## Sparsity floor (amendment 2 — pre-committed)

The Cell-1 verdict is over the POOLED performance vector. C3's cell table must name
**n (real seats) per reading and pooled, per variant**. Floor, fiat: **pooled
n_real ≥ 6 AND ≥ 4 of 6 readings contributing ≥ 1 real performance seat.** Below the
floor **Cell 1 reads NULL, not PASS** — a thin vector that glues is evidence of thin
counting, not of a section (the OQ-23 green-by-sparsity trap). Comparator families
(amendment 3) use the normalized floor: pooled n_real ≥ n_readings AND ≥ ⌈2·n_readings/3⌉
readings contributing ≥ 1; below → that family's cell reads NULL and is reported as
NULL in the base rate (never dropped, never counted as glue).

## The 2×2 and what each cell means (written before running)

Rows: presheaf read (performance | topic). Columns: kernel family | flat control.

- **Cell 1 — family × performance (decides H_perf; three variants).** H¹=0 under ALL
  THREE variants at/above floor: H_perf's load-bearing prediction holds robustly.
  H¹>0 under ALL THREE: **H_perf falsified as stated** — record and stop, no mid-run
  repair. Mixed across variants: a finding about the partition, not the presheaf —
  reported as such, H_perf neither supported nor falsified. Below floor: NULL.
- **Cell 2 — family × topic.** Committer frame already `real_closure` (RECON §B). C3
  reads the DISCARD OBSERVATION: the minimum readings excluded for the pooled topic
  vector to reach H¹=0 (computable exactly from the bloc structure). If the pooled
  topic vector glues with nothing discarded, the family's obstruction is not doing the
  work the experiment assumes — **premise fails; abort to redesign**.
- **Cell 3 — control × performance.** The control's performance seats per variant:
  V_frozen = {`academic_debate_community`, `novice_debaters_and_students`} (n ≤ 2);
  V_inclusive identical (neither folded-back seat is in the control roster);
  V_restrictive = {`novice_debaters_and_students`} (n ≤ 1 — it denotes round
  participants, so it survives the restrictive cut; `academic_debate_community` is an
  institution and drops). Expected: H¹=0 where pooled n_real = 2 (the control's
  stakeholder frame reads h1_stakeholder=0 over 7 real seats — RECON/verdict_join
  table — so its sub-vectors agree), NULL where n_real < 2. Non-discriminating either
  way. **Probe-bug license (amendment-minor), pinned to arithmetic:** a NUMERIC H0/H¹
  at pooled n_real < 2 is the bug witness (OQ-51 N/A rule) — that is the only condition
  under which "probe bug, not evidence" may be invoked, here or in any cell; H¹>0 at
  n_real ≥ 2 is evidence, never rescued.
- **Cell 4 — control × topic.** The control is a singleton story (no family vector);
  its own pooled stakeholder vector expected to glue or read null — obstruction is the
  FAMILY's property. H¹>0: the control obstructs without a kernel cover — the
  family/control contrast cannot discriminate; **abort to redesign**.

**Discrimination statement.** H_perf vs H_topic is decided by Cell 1 alone; Cells 2–4
are premise checks and symmetry controls. No cell outcome VERIFIES H_topic (the
ballot's actual mechanism is not observable in this corpus — only which presheaf CAN
carry a total verdict); the writeup must not upgrade "H_perf supported" into
"ballot = performance sectioning proven."

**Fireable-falsifier check (each names a possible world):** Cell 1 H¹>0 is possible
(bloc B's performance seats could type rope while bloc A's type tangled_rope). Cell 4
H¹>0 is possible (≥2 disagreeing real seats). Cell 2 glue-with-nothing-discarded is
possible (the two axes are independent, Theorem 7). None is contradictory under its own
definitions.

## Base rate across every `real_closure` family (amendment 3)

Cell 1 over fiat alone is one observation on one family (OQ-264 standard: 1-of-n is an
observation; presence needs the set). C3 therefore runs a **comparator column over ALL
16 `real_closure` kernels** (the 13 newly typed by the edge-naming fix — correction on
record: 13, not 11 — plus `constitutional_text_authority`, `federation_membership`,
`jewish_sovereignty_palestine`), zero additional spend.

The fiat name-list partition does not transfer to other topics, so the comparator uses
the **mechanical partition already authored in the corpus**: PERFORMANCE-analog = the
non-excluded AGENT seats (`stakeholder_seats:stakeholder_agent_seats/2`),
TOPIC-COMMUNITY-analog = the `excluded`-role seats. Rationale (from RECON §D): in the
fiat family the authors put the discourse-practice community in the room (agent roles)
and the topic community out of it (excluded roles) — the agent/excluded boundary IS the
substitute-presheaf boundary the OQ names, and it is authored per-story, involving no
per-family judgment by this proposal. Fiat runs BOTH reads (name-list variants above +
this mechanical read), which also measures how well the mechanical proxy tracks the
hand partition. Per family: pooled agent-seat H¹ (normalized floor above; NULL rows
reported as NULL). Report: fiat as the named case, the other 15 as the comparator
column. If fiat glues but k of 15 comparators obstruct, that base rate is a finding in
its own right and the writeup reports it before any H_perf statement.

## Second positive control — probe validity, numeric criterion (amendment 4)

The family-H¹ probe must detect non-gluing where KNOWN: the `state_execution_authority`
triplet (`archives/datasets/kernel_test/*.pl`; registry record: all 3 pairs diverge,
**253/468 divergent pair-contexts** over 156 contexts × 3 pairs). Derived floor: 253
divergent pair-contexts spread over 156 contexts at ≤ 3 pairs each require
**≥ ⌈253/3⌉ = 85 of 156 contexts with H¹ > 0**. Criteria, both pre-committed:
1. **Obstructed-context count ≥ 85** — the probe FAILS below this (this catches a lossy
   probe, not just a uniform-zero one).
2. **Exact join invariant:** Σ over contexts of the probe's H¹ == the same-run live
   `cs_kernel_divergence/4` solution count for the triplet (definition-identical:
   both count both-real-different pairs). Any inequality is a probe bug.
The 253/468 figure is a registry record with shelf life; if the live divergence count
differs from 253 the writeup reports both numbers, but the ≥85 floor stands — probe
failure below it halts C3 for probe repair (fix the probe, never the data), then C3
restarts from the top.

Loading: additive `-l` of the three archive files alongside the live corpus (no
`corpus_path` overlay).

## Engine-witness validity note (pre-C3; operator minor rider)

`test_cs_kernel_registry` sits at 24/25 pre-existing: the failing assertion
(`divergence_silent_at_observed_agreement_context`, tests/test_cs_kernel_registry.pl:134)
exercises `cs_kernel_divergence/4` — the dr_type/divergence path, byte-identical before
and after the edge-naming rewrite, which touched only `cs_kernel_obstruction/4`'s
pair-matching, the resolver, and the unresolved-complement. The rewritten pair-matching
path is covered GREEN by `test_cs_trifurcation` (19/19, constructed kernels per branch).
C3 may cite the engine's obstruction reads; it may NOT lean on `cs_kernel_divergence/4`
without noting the standing 24/25.

## Manufactured-consensus cross-check (OQ-261 item (c))

Recorded ALONGSIDE, per-story only (frame-mismatch note, RECON §F): all 7 stories
already read `manufactured_consensus_candidate[_untypeable]` (RECON §D). C3 re-reports
these tokens next to the cell results and the excluded-seat lists; it does NOT aggregate
them into a family-level verdict (no such surface exists — any aggregation here would
manufacture the very absorption the OQ names).

## Operational constraint: corpus frozen for the duration of C3 (operator rider)

The ~20 untracked `testsets/*_contradictions.pl` files feed `cs_axiom_contradiction/2`,
whose joins just went live (0→11, 0→28); committing them mid-C3 moves the axiom counts
and stales the fingerprints the writeup must cite. **They are a next-session item, not
a during-C3 item.** C3 re-verifies the five-leg md5 fingerprints (RECON header) before
its first probe and after its last; any drift aborts C3 for a fresh baseline.

## Placement

The family-frame H¹ read stays an audit-dir probe this session (thin join over two
exported predicates). Promotion to the engine happens ONLY if the writeup names
"substitute-presheaf verdict channel" as an engine object — then same-change mandatory:
registration in `prolog/reading_registry.pl` (one `aggregatable_reading` line + typed
absence tokens on singletons, OQ-51 convention).

## Outputs (C3, after R2 sign-off)

`performance_presheaf_probe.pl` + raw output; cell table with per-reading and pooled n
per variant; three-variant Cell-1 readout; discard-minimum computation; 16-family
comparator table; positive-control output with both numeric criteria; all in this dir.
WRITEUP.md (C4) quotes this proposal verbatim in its frozen form and declares residue.

## Amendment log (v1 → v2, 2026-08-07, operator review)

1. Blinding declared (type info was in hand at partition authoring); two adversarial
   partition variants pre-registered; Cell 1 reported under all three.
2. Sparsity floor made explicit at the pooled level with pre-committed thresholds;
   below-floor reads NULL, never PASS; per-reading n in the cell table.
3. Comparator column over all 16 `real_closure` families via the mechanical
   agent/excluded partition (also corrects v1-session-report "11" → 13 newly typed).
4. Positive control given numeric criteria (≥85/156 obstructed contexts + exact join
   invariant vs live `cs_kernel_divergence/4`).
Minor: Cell 3's probe-bug license pinned to the n_real<2 arithmetic condition;
engine-witness validity note added (24/25 failing test is off the pair-matching path);
corpus-freeze operational constraint added.
