# OQ-306 — story membership becomes a checked fact; the denominator stops silently admitting non-stories

## Context

`manifest.n_constraints` (in `outputs/pipeline_output.json`) counts every `prolog/testsets/*.pl`
file. 26 of those are `*_contradictions.pl` axiom meta-files — each carries exactly one predicate
family (`narrative_ontology:cs_axiom_contradiction/2`) and no story content. Every corpus rate over
that denominator is ~10% off, and the stratum grew 9 → 26 with nothing going red: the growth, not
the presence, is the defect (`docs/technical/build_discipline.md:1837-1877`). OQ-306
(ISSUES.md:7809, P1) requires: (1) a checkable story-membership predicate, (2) a denominator
ruling, (3) a consumer sweep with per-consumer dispositions, (4) a guard on the share over time.

**Membership statement, precise:** the *story* arm is a pure fact-family test
(`constraint_metric/3`). The *axiom_contradiction* arm is a fact-family existence test plus a
source-file attribution join (clause provenance → basename), because the facts are axiom-keyed and
member ids are basename-keyed — the residual filename dependency is inherited from
`corpus_constraint/1` itself, not introduced here.

## Operator rulings — R1–R3 RULED (2026-08-21; exploration ran 2026-08-20 evening, rulings arrived
post-midnight — the date sequence is correct as stamped)

- **R1 — `n_constraints` stays the member count** (same-run identity key for 4 consumers, not a
  semantic count); additive manifest keys `n_stories`, `n_nonstory_members`, `nonstory_kinds`.
  **Conditions (verbatim):** (a) kinding TOTAL — an unkinded member is a **hard refusal**, never
  default-to-story; (b) `nonstory_kinds` emitted **sorted**; (c) record the naming debt
  (`n_constraints` now means members) for the rebuild era — at the emitter, not only the closed OQ.
- **R2 — contradictions files STAY in `testsets/`**, typed by the membership predicate; relocation
  deferred to rebuild era. **Condition:** arm the deferral — relocation must fail LOUD (D4; the
  sufficiency of the arming as designed is escalated as R-E).
- **R3 — `per_constraint` keeps all members**, each entry gains `member_kind`. **Conditions:**
  (a) derived from `corpus_member_kind/2` at emit time — ONE computation; (b) golden re-bless as
  its own read commit. **Condition (b) dissolved by evidence 2026-08-21:** `golden_file_check.py`
  extracts only `{id: [4 perspective types]}` — added entry/manifest keys are invisible to it;
  no re-bless exists; C3 becomes a verification step with a baseline-provenance witness (D6).
- Through-line: the three-way identity gate (`per_constraint == glob == n_constraints`) is the one
  thing that catches corpus/manifest divergence — every choice preserves it.

## PENDING RULINGS — R-A..R-G and R-I/R-J are ESCALATED, NOT RULED (R-H is RULED, below).
## Blocking map: C2 waits on R-B + R-C + R-I; C3's FORM waits on R-F (verification-only vs a
## retained read step); C4 waits on R-A + R-G; C1 is fully executable (its registration limb
## was resolved by R-H: decline + site marker); C5 waits on C2 AND C4 having landed — hence on
## R-A/R-B/R-C — plus R-J (whether R-E's answer must precede the close or may be recorded as
## declared-residue-pending); the post-implementation evaluation waits on R-D. Note: R-H's
## decline RAISES THE STAKES ON R-A — C4 now carries the catch-all's only real-input coverage
## (see the R-H residue below).
## R-E cannot be ruled before R-A (its arm-2 loudness is a function of the re-pin regime R-A
## selects — fresh-pass Finding 18). C1 is the ONLY step executable before any ruling lands;
## C3 requires no ruling but is SEQUENCED after C2 and waits with it. Execution STOPS at the
## first blocked step — never skip past one: a close (C5) written without C2/C4 is a false
## resolution landed in substrate, the exact failure class this OQ exists to fix. A ruling the
## operator declines or leaves unanswered at close is recorded OPEN with its blocking
## consequence — never omitted or paraphrased into "as ruled".
##
## (Tag legend: N-/M-/P-numbered tags cite findings from review rounds 1-4 of the 2026-08-21
## plan-review loop; "fresh-pass Finding N" cites the fresh-eyes pass. They are provenance
## markers only — every obligation they mark is stated in full where it appears.)

- **R-A — who may quiet the `corpus census` gate row.** Ordinary corpus authoring (the generator
  is live; R2 keeps its output in `testsets/`) will legitimately move the stratum. Options:
  executor-licensed re-pin / stop-and-ask on any delta / licensed re-pin **with mandatory
  recorded cause per re-pin** (recommended — each baseline entry carries `cause` + `authorized_by`
  so the time series stays explained).
- **R-B — scope of the totality refusal.** Hard `SystemExit` on unkindable members everywhere
  breaks archived-corpus retro-audits on documented legacy filename≠subject skew (v5 and
  original_json carry it — source: CLAUDE.md Critical Distinctions / the OQ-20 audit, KNOWN_STATE
  2026-06-22; the figures are recalled prose, RE-DERIVE before any ruling that turns on their
  magnitude). Scope it narrowly and the guard is weakest where corpora are strangest.
  **Recommendation:** hard refusal on the five live legs; other `corpus_path` values continue
  loudly with `unknown`/`dual_family` named. **Rule this with N2's consequences in hand:** the
  continue path makes `member_kind` four-valued in those artifacts and requires the
  `n_unclassified` arithmetic below; the live-leg branch stops the pipeline mid-session and
  therefore carries the remediation text (D2). Also requires a canonical live-leg list (D2, N7).
- **R-C — `schema_version` for the additive keys.** Repo evidence: literal `2` at
  `run_pipeline.py:119`; the 1→2 bump (`ce9a26ec`, OQ-98) shipped with exactly this shape —
  additive serialization keys. **Recommendation:** bump 2→3 in C2. **If bumped:** D5 gains a
  conditional `schema_version` reader sweep (grep with positive control; disposition each hit —
  a reader asserting `== 2` breaks silently otherwise).
- **R-D — who performs the post-implementation RUNS.md evaluation.** **Recommendation:** the
  executor spawns a fresh general-purpose subagent holding the OQ text, the plan, and the diff.
  **Stated at its true altitude (N17):** the harness injects CLAUDE.md, memory, and gitStatus
  into every subagent (OQ-334), so the evaluator is independent **of the executor's session**,
  not context-free — the plan records no blindness it cannot deliver. The evaluator's RAW output
  lands in the audit dir (a transcription by the party under evaluation, with no artifact behind
  it, is a compression whose selection rule is the executor's attention); the evaluator is told
  to distinguish findings from recalls of injected rules. Fallback if not performed:
  `post-impl gaps: not evaluated`.
- **R-E — is R2's "relocation must fail LOUD" discharged by the arming as designed?** D4 declares
  one relocation shape uncovered (generator redirected) and permits dropping arm 3 if it cannot
  be scoped past `load_warning_gate`. Options: accept as declared residue / require arm 3
  workable before landing / treat the N14 generator-site marker as the third arm and close the
  residue. **Recommendation:** land the N14 marker and accept the remaining residue as declared.
  An operator condition partially discharged by executor judgment is exactly the escalation case.
  **Rule AFTER R-A** (fresh-pass Finding 18): arm 2's loudness under relocation is a function of
  the re-pin regime — bare executor-licensed re-pin lets the relocator clear the RED; mandatory
  recorded cause leaves an explained record. R-E is unanswerable before R-A picks.

**Five further rulings from the fresh-eyes pass (2026-08-21), stated neutrally:**

- **R-F — was R3 condition (b) ("golden re-bless as its own read commit") the operator's to
  dissolve?** The plan converted a ruled condition into a verification step on executor-side repo
  evidence (the extractor is blind to added keys), while escalating the structurally identical
  R-E on the principle that executor discharge of an operator condition is the escalation case.
  Say whether (b) was about *necessity* (dissolved by the evidence) or *process* (a separate
  read step regardless — then C3 keeps a read obligation even with no diff expected).
- **R-G — may a second, textually-derived membership definition exist for the twin legs (the D3
  ratchet), or must all five legs derive membership from the single Prolog predicate?** One
  definition costs per-gate swipl latency across five legs; two definitions fork canonicity —
  the shape this OQ exists to remove — softened only by a docstring disclaimer and the
  re-verify-before-reporting rule. The 30s threshold is an unsourced number with no re-measure
  trigger; ruling G includes accepting or replacing it.
- **R-H — RULED 2026-08-21 (operator): DECLINE, with the site marker.** Three grounds, first
  decisive: (1) the OQ-137 gate would verify a by-construction property — `member_kind_/2`'s
  fresh-variable catch-all cannot fail, so totality and exactly-one are structural; and the
  gate runs against the live population, which is expected to hold zero unknowns, so even the
  delete-the-catch-all future edit passes it — the registry's coverage is a strict subset of
  what D2's `n_unclassified` refusal and D3's totality arm already provide, and blind in
  precisely the case that would justify it. (2) The per-run cost (unmemoized clause enumeration
  over ~all members) lands OUTSIDE the one instrument scoped to catch it (the C1 load-delta
  measures `load_all_testsets/0`, not the gate); declining keeps the C1 witness valid as
  specified. (3) Category: membership determines what gets read — upstream of the registry's
  domain. Robust to substrate 15 (if the registry can't express the domain, decline was
  already the answer). **DECLARED RESIDUE, load-bearing:** the decline's justification rests on
  D3's planted-unknown selftest, which lands in C4 (blocked on R-A + R-G). If C4 never lands,
  nothing exercises the catch-all against a real input — record this dependency in the close;
  it also raises the stakes on R-A.
- **R-I — does the D2 refusal get an escape hatch?** It fires mid-`c-orchestrator` topic run;
  the repo's comparable refusal (`corpus_empty`) carries a named override
  (`allow_empty_corpus`). Deliberate no-hatch (declared at the refusal site) and a documented
  hatch are both defensible; silence is not — the next person adds an undocumented one.
- **R-J — Finding 1's blocking-map choice:** must R-E be ANSWERED before C5 lands (the close
  waits), or may C5 land with R-E recorded as declared-residue-pending? (b) ships a close whose
  arming-sufficiency is undetermined; (a) gates the whole close on a judgment about a marker
  comment.

## Established facts (exploration 2026-08-20 — narrative context ONLY; where this section and
## Assumed substrate overlap, **Assumed substrate GOVERNS** and its blanket verify-before-rely
## rule applies to every fact here too, counts and non-counts alike — fresh-pass Finding 24)

- Membership minted twice: `corpus_loader.pl:126-132` (`corpus_constraint/1`, id = file basename;
  docstring :34-43 declares it authoritative) and `run_pipeline.py:107` (glob count).
- Three-way identity refusal `run_pipeline.py:274-281`; in-Prolog gates at :243, :249.
  `per_constraint` enumerates `corpus_constraint/1` (`json_report.pl:63-66`).
- Nine fact families separate 253/253 vs 0/26 on the live leg. Discriminator:
  **`constraint_metric/3` with metric name UNBOUND** (config-binding would make membership
  config-sensitive; `story_provenance`/stakeholder/founding-problem families are OQ-202-mutable —
  disqualified). Conjunction with `corpus_constraint/1` required (metric-only enumeration picks
  up engine demos, e.g. `catholic_church_1200`).
- **`cs_axiom_contradiction/2` args are AXIOM ids, not the kernel id** (verified 2026-08-20) —
  attribution via clause provenance: `clause(narrative_ontology:cs_axiom_contradiction(_,_),
  true, Ref), clause_property(Ref, source(File))`, basename = `<Id>.pl`.
- `data_validation.pl:355-369` wants the member population by design. Twin legs carry **zero**
  contradictions files. Generator: `agent/generate_kernel_corpus.py:1424-1512`, destination
  parameterized.
- `golden_file_check.py`: extractor projects `per_constraint` to `{id: [perspectives]}` only;
  baseline `outputs/golden_classifications.json` — gitignored dir, docstring's "committed
  baseline" already inaccurate (pre-existing; catalog, don't fix).
- Loader `[corpus]` info lines vs `load_warning_gate.py`: the gate matches warning records
  (verify; allowlist entry only if it captures the new lines).
- Live-leg count MOVED during exploration (279 vs 280): never bake a literal; baseline JSON from
  execution-time counts; fingerprint diff pairs; serialize behind any running orchestrator.

## Design

### D1. Canonical predicates — `prolog/corpus_loader.pl` (derived rules, not asserts)

Export from the module header:

- `corpus_story(C)` :- `corpus_constraint(C), has_story_facts(C).`
- `corpus_member_kind(C, Kind)` — total over `corpus_constraint/1`;
  `Kind ∈ {story, axiom_contradiction, dual_family, unknown}`. **`dual_family`, not `ambiguous`
  (N12):** `ambiguous` is already a live token (`constraint_signature(C, ambiguous)`) with its
  own misreading history; minting a colliding second one repeats the four-purity-banders class.
  Fresh-variable head + unify-after-cut (else the `dispatch head`/`bound selector` gate rows
  fire); **the story test is semi-deterministic (N3)** — without `once`, a failed contradiction
  scan backtracks into `constraint_metric/3` and re-runs the (unmemoized) clause enumeration
  once per authored metric per member:

  ```prolog
  has_story_facts(C) :- once(narrative_ontology:constraint_metric(C, _, _)).

  corpus_member_kind(C, Kind) :-
      corpus_constraint(C),
      member_kind_(C, K),
      Kind = K.
  member_kind_(C, K) :-                          % disjointness first: a member
      has_story_facts(C),                        % satisfying BOTH families is
      contributes_axiom_contradiction(C), !,     % dual_family, fail-closed —
      K = dual_family.                           % never silently story
  member_kind_(C, K) :- has_story_facts(C), !, K = story.
  member_kind_(C, K) :- contributes_axiom_contradiction(C), !, K = axiom_contradiction.
  member_kind_(_, unknown).
  ```

  `dual_family` routes to the same fail-closed path as `unknown` everywhere (D2 refusal on
  refusal-scope runs, D3 RED) — a dual-satisfying member is a discovery, not a default.
- `contributes_axiom_contradiction(C)`: some `cs_axiom_contradiction/2` clause whose source-file
  basename is `C.pl`. **NO memoization** — plain enumeration; if measured cost is a problem,
  report the measurement (the C1 load-delta witness below is the measurement site) and ask.
- Derived, not asserted: no second assert to desync, no OQ-326 half-overlay surface.
- **Census stderr line** in `load_all_testsets/0`, printed **BEFORE** the existing final
  `[corpus] Loaded N testsets successfully.` line (last-line parsers undisturbed):
  `[corpus] census: ~w stories, ~w non-story, ~w other.` **Zero-guard (N16, respecified per
  fresh-pass Finding 2 — the first form could not fail):** the two sides come from DIFFERENT
  derivations — N from the loader's own per-file success counter (the number the `Loaded N`
  line prints), the census from a `corpus_member_kind/2` enumeration over `corpus_constraint/1`
  — so a load-loop/registry divergence is exactly what the mismatch branch catches. Print the
  census only if the kind counts sum to that independently-derived N; on mismatch print a loud
  discrepancy line instead. Build check: if you cannot construct an input that makes the
  discrepancy line print, the guard is not built yet. A `0/0/0` census on a corpus loaded
  **without `allow_empty_corpus`** is a guard-ordering bug, never an empty corpus (M9 — under
  the escape hatch a genuinely empty corpus produces `0/0/0` legitimately; check which regime
  before diagnosing). The discrepancy line is a third new stderr record — substrate 10's
  warning-gate verification covers it too (M8).
- **Registration DECLINED (R-H, ruled 2026-08-21).** The site marker lands at the predicate
  definition, VERBATIM (operator's text); the sweep artifact carries a copy:

  ```prolog
  % corpus_member_kind/2 is deliberately NOT registered in reading_registry.pl
  % (OQ-306 R-H, ruled 2026-08-21). Membership predicate, not a reading — it
  % determines what gets read, upstream of the registry's domain. Totality is
  % structural: member_kind_/2's final clause is a fresh-variable catch-all and
  % cannot fail, so the OQ-137 gate would check a by-construction property
  % against a population expected to hold zero unknowns. Enforcement lives at
  % D2's n_unclassified refusal (run_pipeline.py) and corpus_census_check.py's
  % totality arm, whose planted unknown-shape selftest exercises the catch-all
  % against an input the live corpus does not supply. Do not re-register
  % without first retiring those.
  ```

  `reading_registry.pl` leaves C1's file list; substrate 15 becomes moot for execution (kept
  for the record).
- Fix `drl_core.pl:99-101` stale comment same commit (Fix-simple-errors license).

### D2. Manifest + per_constraint (R1/R3; C2 blocked on R-B + R-C)

- `json_report.pl`: each `per_constraint` entry gains `member_kind` via
  `corpus_loader:corpus_member_kind/2` at write time; plus a top-level `member_census` object in
  `pipeline_output.json` that **always emits all four kinds, zeros included (P3)** — a zero is
  authored, not absent, so no reader ever needs the forbidden `.get(kind, 0)` idiom and the
  identities below are total, not conditional. **Census provenance, pinned (fresh-pass Finding
  3):** `member_census` is computed by an INDEPENDENT Prolog enumeration of
  `corpus_member_kind/2` over `corpus_constraint/1` — NEVER by tallying the `per_constraint`
  entries just written (a tally of the emitted values would make identities (i)/(ii) compare
  the same emission against itself, catching only serialization faults). R3a's "ONE
  computation" means one DEFINITION — the predicate — read twice (per-entry emission; census
  enumeration), not one read.
- `run_pipeline.py` `build_manifest`/`inject_manifest`: additive keys `n_stories`,
  `n_nonstory_members`, `nonstory_kinds` (a JSON object kind → count, keys **sorted** — R1b
  verbatim), plus **`n_unclassified` (N2):** count of `unknown` + `dual_family` entries.
  **Sum invariant: `n_stories + n_nonstory_members + n_unclassified == n_constraints`**, checked
  beside the three-way gate and in `classify_corpus`. **Refusal semantics (R1a × R-B):** on
  refusal-scope runs (per R-B; recommended: the five live legs), `n_unclassified > 0` →
  `SystemExit` naming the ids **with remediation in the message (N15):** "re-key the story to
  its filename or verify filename==subject (OQ-306/OQ-20); a deliberate new non-story kind needs
  a kind-taxonomy ruling + baseline update." On continue-scope runs the artifact ships with
  `n_unclassified` nonzero and the ids listed loudly. **The partition, stated once (M2):**
  `nonstory_kinds` covers exactly the KNOWN non-story kinds (today: `axiom_contradiction` only);
  `unknown` and `dual_family` are counted ONLY in `n_unclassified` and never appear in
  `nonstory_kinds` or in the D3 baseline. **The kind-value guarantee, scoped (N2):**
  artifacts from refusal-scope runs carry `member_kind ∈ {story, axiom_contradiction}` (enforced
  by the refusal — `n_unclassified == 0` is the arithmetic form); artifacts from continue-scope
  runs are four-valued. Documented at the emit site + the B1 comment; readers must not write
  `.get("member_kind", "story")` defaults. **Cross-boundary consistency (N5 + M2 + P2/P11),
  every identity comparing a PROLOG-derived number against a PYTHON-derived one — an identity
  whose two sides come from the same loop is a total recomputed from its own parts and cannot
  fail:** (i) `member_census["story"] == n_stories` (python per-entry count); (ii) per-kind:
  for each known non-story kind K, `member_census[K] == nonstory_kinds[K]` (python per-entry
  count); (iii) `n_unclassified == member_census["unknown"] + member_census["dual_family"]` —
  the one identity that catches a python-side miscount of the fourth state.
  (`sum(nonstory_kinds.values()) == n_nonstory_members` is NOT asserted — both sides come from
  the same python pass; vacuous by construction, dropped with this reason recorded.)
  **Live-leg list (M4 + P4):** mint `LIVE_LEGS` in a NEW dedicated constants module
  **`python/shared/corpus_legs.py`** — import-side-effect-free (constants only, nothing
  executable at import), NOT in a checker and NOT in `run_pipeline.py` (a checker importing the
  pipeline inverts the dependency the other way); `module_boundary_check.ALL_ARM_C_LEGS`
  references it — binding refusal scope to another analysis's arm-C membership is a false
  unification (coextensive today, distinct in principle; recorded at both sites).
  **Scope test canonicalizes BEFORE comparing (fresh-pass Finding 4):** `corpus_path` takes
  both relative names and absolute paths; a naive `if corpus_path in LIVE_LEGS` silently
  downgrades an absolute-path live-leg run to continue-scope — the permissive direction.
  Resolve the configured path the same way `resolve_corpus_dir/2` does, then compare the
  resolved directory against the resolved forms of `LIVE_LEGS`; `classify_corpus` uses the
  same canonicalization. `schema_version` per R-C.
  **Naming-debt comment at `build_manifest` (R1c):** "`n_constraints` counts corpus MEMBERS
  (stories + meta-files); rename is rebuild-era debt — OQ-306 close."
- Twin-leg corroboration: one `classify_corpus` twin run with `n_nonstory_members: 0` and
  `n_unclassified: 0`.

### D3. Growth guard — `python/corpus_census_check.py` + gate row `corpus census` (C4 blocked on R-A)

- Terminology, pinned (fresh-pass Finding 23): **"default leg" = `prolog/testsets/`;
  "refusal-scope legs" = whatever R-B rules (recommended: all five)**. In this section the swipl
  arm covers the DEFAULT leg; the twins are covered by the ratchet (or by swipl under the <30s
  branch). D4 arm 3 fires on the default leg only.
- `--check`: (1) totality fail-closed — every DEFAULT-leg member classifies story XOR a known
  non-story kind; `unknown`/`dual_family` → RED naming files; (2) stratum pin vs committed
  `python/corpus_census_baseline.json` — per-kind non-story counts per leg (live:
  `axiom_contradiction: <execution-time count>`; twins: `{}`). RED on ANY delta. **The pin
  compares COUNTS only; the per-entry `cause` + `authorized_by` metadata fields are excluded
  from comparison** (a cause-text edit must not turn the row RED). Re-pin licensing per R-A.
  Story counts NOT baselined (the corpus grows daily by design).
- Same-path derivation: live leg via swipl query of `corpus_loader:corpus_member_kind/2`
  (checker owns zero classification logic). Twin legs: textual ratchet (zero
  `cs_axiom_contradiction` heads; every file carries a `constraint_metric` head) — a drift
  ratchet, not a second definition, stated in the docstring. Fork pinned: measure per-leg swipl
  cost once at build time, **stamped with date and per-leg file counts** (a one-time number
  governing a permanent choice over growing legs); < 30s total across five legs drops the
  textual arm, ≥ 30s keeps it; number + choice recorded in the docstring; a twin-leg textual
  failure is re-verified via swipl before being reported as drift.
- Selftest rides every run (`spec_enum_check.py` skeleton): naturally-arising two-sided control
  (first real contradictions file → `axiom_contradiction`; first real story file → `story`;
  runtime-picked), plus planted tempfile mutations (unknown-shape member → RED; baseline
  off-by-one → RED). **All plants live in a tempdir/scratchpad; the selftest is WRITE-FREE with
  respect to all five legs** (fresh-pass Finding 22 — it runs at every gate invocation and must
  not race operator topic runs). **The `dual_family` arm's only control is a planted fixture —
  bottom rung; report it at that altitude**, never under the naturally-arising claim that covers
  the story/axiom arms.
- **Discrimination record, mechanics stated (N13 + M3):** find a stratum-growing commit N and
  parent N−1; materialize each corpus state into a scratch tree via `git ls-tree`/checkout of
  `prolog/testsets/` only; **build a scratch BASELINE pinned to N−1's stratum count** — against
  the committed (today's) baseline both historical states go RED on the pin for a reason
  unrelated to the stratum, and "fires at N, declines at N−1" cannot be observed. Run the HEAD
  checker against both scratch trees via `corpus_path` overlay (never a full worktree — the
  gitignored-`outputs/` trap), against the N−1 scratch baseline: expect N−1 GREEN, N RED. The
  write-up states which instrument ran, which baseline the pair ran against, **and which ARM
  produced each verdict (P5)** — the totality arm can redden N−1 for reasons unrelated to the
  stratum (an older fact-family, a filename≠subject member); a totality-RED at N−1 is a finding
  about the historical corpus (route it), then move to an earlier pair or take the plant-only
  label. If only a reconstruction is achievable, take the **plant-only** label — the honest
  fallback.
- `scripts/gate.sh` row after `module bounds`; last line `corpus_census_check: GREEN|RED ...`.
- Pipeline-side [X-GATE] declined (latency; the D2 refusal already guards in-pipeline) —
  decline recorded in the sweep artifact AND at the D2 refusal site.

### D4. Arming the R2 deferral — per relocation shape (sufficiency escalated as R-E)

| Relocation shape | What fires |
|---|---|
| Files moved out of `testsets/` (members vanish from `corpus_constraint/1`) | Arm 2 ONLY: baseline pin `axiom_contradiction: N → 0` ≠ baseline → gate RED. Arm 3 fires if live leg. |
| Facts moved/renamed while stub files remain | Arm 1: stub kinds `unknown` → D2 refusal + D3 RED. |
| Generator redirected (new files stop arriving) | No load/gate arm fires while the stratum is static. **Field marker (N14):** a comment at `emit_axiom_contradiction_facts` — "changing this destination silently vacates OQ-306's arming and requires a census-baseline update" — the person redirecting the generator is reading that function, not an audit artifact. Residue beyond the marker: R-E. |

Arm 3 (loader warning, live leg only — resolved corpus dir `testsets` AND zero
`cs_axiom_contradiction/2` clauses → loud `[corpus] WARNING: ...`): if `load_warning_gate`
captures it, the allowlist entry may be added only if scoped so it cannot mask the warning;
**if that scoping is impossible, arm 3 is DROPPED (not demoted to an ignored info line —
Pattern 6) and D4 declares TWO arms.** Arm count is per-branch.

### D5. Consumer sweep — `audits/<exec-date>_oq306_denominator_census/CONSUMERS.md`

**Derivation procedure (roster checkable, not received):** union of four `/usr/bin/grep -rn`
sweeps — (a) `"n_constraints"`, (b) `"corpus_constraint"`, (c) `"per_constraint"`, (d)
`"_contradictions"` — **all four recursive over the same six trees
`python/ prolog/ docs/ agent/ audits/ scripts/`** (M6: (b) is not scoped narrower than its
siblings — membership is queried from Python too; `.claude/` excluded, stated: no code
consumers — hooks invoke python scripts already in-tree). Each grep re-run at execution with a
positive control **chosen outside the narrowest tree** so the control tests coverage, not just
firing (e.g. (a) must hit `audits/oq140_divergence_extract.py`; (b) must hit
`python/run_pipeline.py`'s in-Prolog gate string; (c) must hit `prolog/json_report.pl` — the
per_constraint writer, outside `python/` (P6); (d) must hit
`agent/generate_kernel_corpus.py`). Pin `/usr/bin/grep` in anything producing a reported count.
**Boundary justification (fresh-pass Finding 17):** the sweep ALSO covers repo-root `*.md`
(`ISSUES.md`, `KNOWN_STATE.md`, `AGENTS.md`, `README.md`, `CLAUDE.md` — plausible carriers of
an `n_constraints` claim) and `json/`; a positive control inside the union cannot reveal the
union is too small, so the boundary carries this stated justification instead. Excluded with
reasons: `.claude/` (no code consumers), `outputs/` (gitignored, regenerable),
`prolog/archives/` + `prolog/testsets*/` (data, not consumers).
Plus one row per OQ-named seed site — `:608/:4038/:5345/:6208` as filed, located at
`:609/:4129/:5489/:6352` on 2026-08-20 (ISSUES line drift, noted in the artifact) — each
explicitly retired or re-routed so closure back-maps to the OQ. **Conditional on R-C bump:**
a `schema_version` reader grep (same trees, own positive control), each hit dispositioned.
**Stale-prose principle (fresh-pass Finding 19):** stale comments in files this plan EDITS are
fixed under the Fix-simple-errors license (`drl_core.pl:99`); stale prose in files this plan
only RUNS or reads is cataloged, not fixed (`golden_file_check.py` docstring) — edit scope
follows the commit's file list, so no fix rides into an untouched file.

Dispositions (minimal fixes; the rest routed):
- **by-design members** (inline comment, no change): `data_validation.pl:355-369`.
- **identity keys, unchanged**: three-way gate, `enhanced_report.py:1693`, `w1_sheaf_join.py:71`,
  `orbit_operator.py:193-204`, `audits/oq140_divergence_extract.py:373-375`.
- **routed to owning OQs** (site comment naming `corpus_story/1`): `commentary_census.pl:236-258`
  → OQ-136/OQ-202; `kernel_orbit_export.pl:24,34-40`; `probe_oq197_controls.pl:38-61`;
  `batch_claim_reconciliation.py`; `q_provenance_readout.py`; `cs_kernel_registry.pl:511,526,663`;
  `run_pipeline.py:249` provenance-coverage gate → OQ-202.
- **verified unaffected**: `golden_file_check.py` — extractor projects `{id: perspectives}` only;
  C3 verifies with baseline provenance (D6); its gitignored-"committed"-baseline inconsistency
  cataloged, not fixed.
- **cataloged, left as-is**: the five filename-suffix local exclusions; `validation_suite.pl`
  (generated; `unknown_interval` sentinel); ISSUES.md orbit-holes row; `n_sotu_constraints` —
  checked path `<repo>/prolog/testsets_sotu/` (`run_pipeline.py:31-34` constant), absent
  2026-08-20 by direct `ls`; archive sibling `prolog/archives/datasets/testsets_sotu/` exists
  and is a DIFFERENT path — both paths + instrument recorded.

### D6. Commit plan

Before every diff pair: md5-fingerprint the live leg both halves; serialize behind any
orchestrator; Edit/Write only; `known_state_status.py --file` per target; open
`audits/INVESTIGATIONS.md` lines before unknown-answer checks (CLAUDE.md Audit Methodology,
commit `f4124b27`). **Sequencing:** if any other staged plan lands manifest/`json_report.pl`/
`run_pipeline.py` changes first, recompute C2's before/after baseline and C4's census baseline
against the landed state — never merge stale. **Blocking map (M1): C2 ← R-B + R-C; C4 ← R-A;
C5 ← C2 AND C4 landed (hence every pending ruling) + R-E; post-impl ← R-D. Only C1 and C3
unblocked. STOP at the first blocked step — never execute past one.**

1. **C1 (behavior-preserving) — predicates + census line + R-H decline marker.**
   Files: `corpus_loader.pl` (predicates + census line + the verbatim R-H marker),
   `drl_core.pl` (comment), possibly `load_warning_allowlist.txt`, **plus the audit
   dir gets its copy of this plan AT C1, not C5** (fresh-pass Finding 10 — under the expected
   stop-after-C1 path, C5 may be far off and the machine-local plan file is otherwise the sole
   copy for the whole executable window). **C1 also lands a site marker at
   `corpus_member_kind/2`** — "emit-side consumer lands in C2 (OQ-306); this severance is
   by-plan, not unfinished" (fresh-pass Finding 11 — the stop rule deliberately creates a
   produced-but-not-consumed state; mark it at the site so it reads as intended, not abandoned).
   Witness: swipl one-shot (census line agreeing with the `Loaded N` line, kind breakdown, zero
   `dual_family`/`unknown` on the live leg) pasted; two-sided classifier control pasted;
   same-session clean-vs-edited pipeline diff — exit 0 + output mtime advanced, byte-identical
   at `per_constraint` (normalize `pipeline_run_at`); OQ-137 totality gate green;
   **load-time delta pasted (N4 + M5 + P7):** before/after wall-clock of `load_all_testsets` on
   the same md5-pinned corpus — warm cache, 3 runs per side, medians compared, **spread =
   max−min of the three baseline-side runs**, both numbers reported separately. **Signal iff
   the median delta exceeds the spread AND exceeds max(10%, 2s)** — two independent clauses,
   not a sum. On signal, stop and ask (this is also the F7 measurement site).
   **C1 prep's FIRST action anchors the golden baseline (M7 + P8):** record
   `outputs/golden_classifications.json`'s md5 and id count BEFORE any edit — C3 compares
   against these bytes, not mtime (mtime moves on clone/copy/touch and pins nothing). **If the
   baseline is ABSENT at anchor time** (gitignored dir; fresh clone or outputs/ wipe), say so
   THEN and choose: generate one from the pre-change pipeline output and anchor that, or
   declare C3 non-witnessing in the write-up — discovering an absent witness at C3, after the
   work, is the strictly worse ordering.
2. **C2 (output-changing; BLOCKED until R-B, R-C and R-I are answered in this plan) —
   member_kind + member_census + manifest keys + refusals + arming warning.**
   Files: `json_report.pl`, `run_pipeline.py`, **`python/shared/corpus_legs.py` (new) and
   `python/module_boundary_check.py` (P4 — it now references `LIVE_LEGS`)**.
   Witness: full run exit 0 + mtime; before/after diff showing ONLY the additive keys +
   per-entry `member_kind` + `member_census` (+ `schema_version` if bumped) — **normalization
   set enumerated in full (fresh-pass Finding 5): `pipeline_run_at`, `code_commit`,
   `code_commit_short`, `code_dirty`** (the manifest re-stamps all four every run; a criterion
   naming only `pipeline_run_at` is false as written and forces the executor to widen it
   silently); sum + cross-boundary (P2/P11) invariants green; **`module bounds` gate row still
   green (P4)**;
   refusal demonstrated on a planted unkinded fixture (scratch corpus) with the remediation
   text visible; one twin `classify_corpus` run with `n_nonstory_members: 0`,
   `n_unclassified: 0`.
3. **C3 (verification, no expected diff) — golden check with baseline provenance (N6 + M7).**
   Witness: FIRST paste the baseline's md5 and id count and confirm both MATCH the values
   anchored at C1 prep (bytes, not mtime); THEN run `golden_file_check.py` and paste the
   comparison result. Expected green; any red = stop (classifications moved — C1/C2 must not do
   that). A green with no stated baseline provenance is byte-identical to a run that never
   compared. **If a ruling wait intervened between C1 and C3** (fresh-pass Finding 12): the
   gitignored `outputs/` regenerates under any pipeline or topic run, so the C1 anchor is
   EXPECTED to rot — re-anchor at C3 time from a fresh pre-C2-state comparison, and say in the
   write-up that you did.
4. **C4 (behavior-preserving apparatus; BLOCKED until R-A and R-G are answered) — guard.**
   Files: `corpus_census_check.py`, `corpus_census_baseline.json` (execution-time counts;
   `cause`/`authorized_by` excluded from comparison), `scripts/gate.sh` row.
   Witness: gate GREEN pasted; RED-capability pasted twice (planted unknown-shape; baseline
   decrement in scratch); natural-pair check performed per D3 mechanics, result + instrument
   stated.
5. **C5 — close + docs.**
   Files: ISSUES.md (OQ-306 close per D7; OQ-136/OQ-202 annotations; Deps/Priority hygiene),
   KNOWN_STATE.md entry — **the naming-debt warning filed `Tier: tripwire` (N18), so the
   PreToolUse hook delivers it to future `run_pipeline.py` editors; a standing warning filed
   `landed` is never delivered** — CLAUDE.md Build-Discipline denominator paragraph (resolved
   framing + gate-row pointer), `build_discipline.md` detail entry, audit dir WRITEUP
   (`Fired:` bit) + CONSUMERS.md + a copy of this plan (the evaluator's retrievable artifact),
   `omega_resolver.py index` regen.
   Witness (ITEMIZED, fresh-pass Finding 25 — a green gate cannot see most of these): full
   `[GATE]` green pasted, PLUS per-deliverable: (a) the ISSUES close diff with R1b's "sorted"
   verbatim and each R-x recorded as answered-or-OPEN; (b) `known_state_status.py --file
   run_pipeline.py` output showing the naming-debt entry DELIVERED at tripwire tier; (c) the
   `build_manifest` naming-debt comment pasted; (d) the D7.1 correction shown APPENDED (old
   text intact above it); (e) the audit dir listing (WRITEUP with Fired: bit, CONSUMERS.md,
   plan copy); (f) `omega index --check` fresh.

### D7. The OQ-306 close records

1. Dated correction appended (not rewritten), scoped and instrumented: "`six_questions`/
   `base_properties` appear in zero `prolog/testsets/*.pl` files (grep sweep, 2026-08-20, re-run
   at close) — stale as *`.pl` fact-family markers*; no claim about the concepts' other carriers
   (the `json/` specification side was not swept; sweep it or say so)."
2. `drl_core.pl:99` comment fix — cite C1.
3. OQ-202 interaction: provenance stamping for contradictions files SURVIVES (why
   `story_provenance` was rejected as discriminator — record that constraint IN OQ-202);
   stakeholder/founding-problem authoring for contradictions files MOOT under the non-story
   ruling; `run_pipeline.py:249` disposition routed there.
4. Rulings R1–R3 as ruled, conditions verbatim (R1b is "sorted" — the word the operator used);
   R-A..R-E **each recorded as actually answered — and any the operator declines or leaves
   unanswered at close is recorded OPEN with its blocking consequence, never omitted or
   paraphrased into the "as ruled" list (M1c)**; naming debt at its read-sites (emitter comment
   + KNOWN_STATE tripwire; closed OQ is provenance only). The R-B skew figures re-derived or
   explicitly marked recalled (N8) before they are quoted in the close.
5. No new design_gaps entry (extensibility of the kind taxonomy is carried by the fail-closed
   `unknown`/`dual_family` arms). **The R-H residue recorded explicitly:** the decline's
   justification rests on C4's planted-unknown selftest — until C4 lands, nothing exercises the
   catch-all against a real input; the close states this dependency rather than letting the
   decline read as self-sufficient.
6. Deps/Priority authored on touched OQs; index regenerated.

## Verification (end-to-end)

1. Loader census line pasted, agreeing with the `Loaded N` line; swipl totals sum
   (`corpus_constraint` = story + non-story + unclassified, unclassified 0 on live + twin legs),
   with corpus md5 + manifest timestamp. Load-time delta within threshold.
2. Two-sided classifier control: real contradictions id → `axiom_contradiction`; real story id →
   `story`; planted dual-satisfying fixture in scratch → `dual_family`.
3. Refusal fires on a planted unkinded member (scratch) with remediation text; gate
   RED-capability both plants; natural-pair result + instrument recorded; live GREEN.
4. Pipeline manifest diff: ONLY additive keys + `member_kind` + `member_census`
   (`pipeline_run_at`-normalized); exit 0 + mtime both halves; three-way + sum + cross-boundary
   invariants green.
5. Twin run: `n_nonstory_members: 0`, `n_unclassified: 0`. Golden check green with baseline
   provenance pasted (C3).
6. Full `./scripts/gate.sh` GREEN including `corpus census`; `[GATE]` pasted at `[PUSH]`.

## Cross-OQ notes

OQ-136/OQ-202 own the census-arithmetic restatements. OQ-305 untouched beyond the close mention.
OQ-202 limb (b) reshaped per D7.3.

## Assumed substrate (executor verifies each before relying on it; locate by content, not line)

1. `prolog/corpus_loader.pl` — `register_corpus_constraint/1` asserts `corpus_constraint/1`
   keyed on file basename after successful consult; export list in module header;
   `corpus_loaded` asserted at end of `load_all_testsets/0`; final load line is
   `[corpus] Loaded N testsets successfully.`
2. `python/run_pipeline.py` — `n_constraints = len(glob("*.pl"))` (~:107); three-way refusal
   (~:274-281); in-Prolog gates (~:243, ~:249); B1 inertness comment (~:101-105);
   `schema_version: 2` literal (~:119).
3. `prolog/json_report.pl` — `per_constraint` enumerates `corpus_loader:corpus_constraint/1`.
4. Fact-family separation on the LIVE leg: every non-contradictions `testsets/*.pl` carries
   `constraint_metric/3` with first arg == file basename; no `*_contradictions.pl` carries any
   story family. RE-RUN at execution; a filename≠subject member kinds `unknown` and surfaces
   loudly (by design; zero expected on the live leg).
5. Same check on the four twin legs: every twin file carries `constraint_metric/3`
   (subject==basename), zero `*_contradictions.pl` per leg.
6. `cs_axiom_contradiction/2` args are axiom ids, NOT the kernel id.
7. `covering_analysis.pl` `all_corpus_constraints/1` binds ExtName from config — different
   question; leave it.
8. `data_validation.pl` orphan check enumerates the member population by design.
9. `agent/generate_kernel_corpus.py` `emit_axiom_contradiction_facts` writes
   `{kernel_id}_contradictions.pl` into a parameterized testsets_dir.
10. `python/load_warning_gate.py` matches normalized warning records against
    `prolog/load_warning_allowlist.txt`; verify whether it captures ANY of the three new stderr
    lines — the census line, the N16 discrepancy line, and the D4 arm-3 WARNING — before
    landing them (M8).
11. `scripts/gate.sh` `run()` prints only a checker's LAST line; house checker skeleton =
    `python/spec_enum_check.py`.
12. `drl_core.pl` (~:99-101) carries the stale "two non-story files" comment.
13. Stderr parser enumeration: grep `python/ prolog/ scripts/ agent/ audits/` for consumers of
    `[corpus]`-prefixed loader output (positive control: must find `load_warning_gate.py`'s
    capture of load stderr); the census line lands only after this confirms no last-line parser
    breaks.
14. CLAUDE.md → Audit Methodology carries the INVESTIGATIONS-ledger rule — landed commit
    `f4124b27`, 2026-08-20; verify by reading the section (the rule postdates some
    injected-context snapshots).
15. `prolog/reading_registry.pl` — entry format and what a declared domain may express (read
    before registering; if it cannot express `corpus_constraint/1`, take the D1 decline path).
16. `python/golden_file_check.py` — extractor reads only `per_constraint[].id` +
    `.perspectives`; baseline `outputs/golden_classifications.json` (gitignored dir);
    `--generate` is the bless path.
17. D5 consumer roster sites exist as cited (each grep re-run at execution with its
    outside-the-narrowest-tree positive control; a missing site is a finding, not a silent
    drop).
18. `schema_version` precedent: the 1→2 bump was `ce9a26ec` (OQ-98, additive serialization
    keys) — verify if R-C bumps.
19. `python/module_boundary_check.py` carries `ALL_ARM_C_LEGS` (~:344), the existing five-leg
    list — per M4/P4 it will REFERENCE the new `python/shared/corpus_legs.py` `LIVE_LEGS`
    constant, not serve as the home; verify `python/shared/` exists and is importable as a
    package (it carries `shared.loader` today) and that nothing in it executes at import.
20. **`clause/3` access to the contradiction facts (fresh-pass Finding 13):** verify BEFORE
    writing `contributes_axiom_contradiction/1` that
    `clause(narrative_ontology:cs_axiom_contradiction(_,_), true, _)` succeeds on the loaded
    corpus — consulted multifile facts are normally clause-accessible in SWI, but a compiled/
    static loading path would throw, and a throw here kills every corpus load. Also decide
    `source` vs `file` in `clause_property/2` with the `include/1` caveat in view (they differ
    when a clause arrives via include; testsets are consulted whole today — verify) and record
    the choice. The `narrative_ontology:` qualification in `has_story_facts/1` is substrate
    too — a wrong qualifier kinds the entire corpus `unknown` (loud, but check first).
21. **`python/corpus_census_baseline.json` is actually COMMITTABLE (fresh-pass Finding 16):**
    verify `python/*.json` is not gitignored before calling the baseline "committed" — an
    untracked baseline makes the pin unshareable and the gate row locally green everywhere,
    the exact defect this plan catalogs in `golden_file_check.py`'s docstring.

## Executor prompt (run in a FRESH session)

> Execute the OQ-306 plan at
> `/home/scott/.claude/plans/let-s-try-reviewing-306-vectorized-kernighan.md` (a copy lands in
> the audit dir at C5 — use either). Read it whole, then CLAUDE.md's Build Discipline and
> Corpus Loading sections. **R1/R2/R3 are RULED (recorded with conditions) — do not re-open
> them. R-A through R-J are PENDING operator rulings — the plan carries recommendations, not
> answers — except R-H, RULED: registration DECLINED, land the verbatim site marker instead.
> Blocking map: C2 waits on R-B + R-C + R-I; C3's form waits on R-F; C4 waits on R-A + R-G;
> C5 waits on C2 AND C4 having landed plus R-J; the post-implementation evaluation waits on
> R-D. C1 is fully executable and is the ONLY step executable before rulings land; C3 requires
> no ruling of its own but is SEQUENCED after C2 and waits with it. STOP at the FIRST
> blocked step — never skip past one and continue: executing C1 → C3 → C5 would land a written
> close, a CLAUDE.md edit and a KNOWN_STATE entry for work never performed — a false resolution
> in substrate, the failure class this OQ exists to fix. Implementing a recommendation as if
> ruled is the failure the escalation exists to prevent.**
>
> Sequence: verify every Assumed-substrate line against the repo before C1 (a mismatch is a
> finding — report it; proceed only if the design survives it; stop if it does not). Then
> C1→C5 in order, stopping at the first blocked step, each landed step with its witness
> PASTED in the same turn (paste-or-untag; a pipeline diff witnesses only with exit 0 AND
> output mtime advanced). Count everything at execution time; md5-fingerprint the corpus around
> both halves of every diff pair; serialize behind any running c-orchestrator. Edit/Write only
> (never Bash sed — the tripwire hook matches Edit|Write only); run
> `python3 python/known_state_status.py --file <path>` before touching each target; open an
> `audits/INVESTIGATIONS.md` line before any check whose answer you do not know, and close it
> with its Fired: bit.
>
> Stop-and-ask triggers: (a) any existing gate goes red under C1 (registration is DECLINED per
> R-H — if you find yourself registering `corpus_member_kind/2` in `reading_registry.pl`, stop:
> that contradicts a ruling); (b) `golden_file_check` shows any red at C3, or its baseline is missing /
> post-dates C1 (a green without baseline provenance is not a witness); (c) the census or
> refusal shows `unknown`/`dual_family` on any REAL member of any leg — a discovery about the
> corpus, not a fixture problem; (d) any twin leg shows a nonzero non-story count — or a twin
> `classify_corpus` run REFUSES (on refusal-scope legs a bad member presents as a SystemExit,
> not a nonzero field; a refusal is this trigger, not a tooling problem); (e) the
> stderr-parser enumeration finds a last-line parser the census line would break; (f) the C1
> load-time delta exceeds its threshold; (g) anything needing a new operator ruling.
>
> License to refuse, stated: if an instruction in this plan is correct in prose and wrong when
> executed, say so rather than comply — report the refusal at the volume of a completion and
> route it back; do not silently repair the plan.
>
> Mid-run question routing: spec-interpretation → spawn `repo-blind-reviewer` with the OQ text +
> the plan's Design and Assumed-substrate sections + the question (NOT the Established-facts
> block); evidence → re-derive against the substrate yourself; RULING → stop and ask the
> operator.
>
> After implementation (once R-D is ruled; the plan's recommendation): spawn a fresh
> general-purpose subagent holding the OQ text, the plan, and the final diff — note it is
> independent of YOUR session, not context-free (the harness injects CLAUDE.md/memory/gitStatus
> into every subagent; OQ-334). It must re-run the claimed commands, compare every claimed
> number against its artifact, classify every zero, paste witnesses, and distinguish findings
> from recalls of injected rules. Its RAW output lands in the audit dir; then fill the RUNS.md
> row identified by **run-id `2026-08-21-1`** (target OQ-306) in
> `.claude/skills/plan-review/RUNS.md` with `post-impl gaps: N (what)` — the row's date column
> is the REGISTRATION date, not the execution date; do not mint a second line because the dates
> differ. **If that row is absent (file changed, registration never landed), REPORT the absence
> rather than minting a row or skipping the obligation (P10).** Filled either way; if the
> evaluation is not performed, write `post-impl gaps: not evaluated`.
