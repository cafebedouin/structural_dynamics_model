# Known State — Session Changelog

This is the dated session log split out of `CLAUDE.md` (2026-05-31) to cut the
auto-loaded instruction file's per-session token cost (~3,050 tokens / 45% of CLAUDE.md
were this section). **It is NOT auto-loaded** — read it on demand, and prefer the
query below to reading the whole file.

**Entry grammar (machine-readable, added 2026-06-04).** Every entry is:

```
## YYYY-MM-DD — <title>
**Files:** <comma-separated paths the entry concerns>
**Tier:** tripwire | correction-key | landed | history
```

Tiers: `tripwire` = standing do-not / silent-mistake warning; `correction-key` =
corrects prior claims or qualifies how results may be cited; `landed` = change/audit
shipped and witnessed; `history` = narrative/archival (roll-off candidate). Checker:
`python3 python/known_state_status.py --check` (run after editing this file; sibling of
`issues_status.py`).

**Before touching a file, query instead of reading everything:**
`python3 python/known_state_status.py --file <path>` lists the entries whose `Files:`
line names it — read those. (The old hand-maintained "read before touching" list is
superseded by the `Files:` lines; high-traffic files currently include
`signature_detection.pl`, `drl_composition.pl`, `json_report.pl`,
`generate_kernel_corpus.py`, `enhanced_report.py`.)

**Roll-off rule (monthly, with the CLAUDE.md "Memory Consolidation Review"):** entries
older than ~30 days get the CLAUDE.md promotion test once more, then are **compressed in
place** — keep the header + `Files:`/`Tier:` lines + a 2–4 line verdict + pointers
(commit hash, `audits/<date>_<slug>/`, OQ number); drop the body. Full text stays in
this file's git history; never create a separate archive file (Build Discipline
Pattern 2). `tripwire` entries are compressed only if their warning is promoted to an
always-loaded CLAUDE.md section or superseded.

**Standing warnings lifted into auto-loaded `CLAUDE.md` sections** (the tripwire lives there;
full provenance stays here):
- Green cut `product_site_export.pl:75–77` → `CLAUDE.md` Architecture Invariants.
- Run-tagged subdir glob isolation → `CLAUDE.md` Corpus Loading.
- Corpus is 223 not 3,337 / cite the manifest → `CLAUDE.md` Critical Distinctions.

Entries are roughly chronological. New session findings go here (see `CLAUDE.md`
End-of-Session Documentation Review), not in CLAUDE.md.

---

## 2026-07-25 — [landed] OQ-255 seat-cost measure delivered; two corpus facts: `emerges_naturally`'s true-pole holds FIVE types, and `coordination_vitality` is authored-empty on every live leg
**Files:** audits/2026-07-25_oq255_seat_cost_measure/, prolog/drl_core.pl, docs/seat-theorem-v1.md, docs/deferential_realism_paper_v8.md, docs/the-few-seats-worth-choosing-v2.md, ISSUES.md
**Tier:** landed

OQ-253/254/255 minted and OQ-255 executed same day (commits `03e57ec3`, `2fa3eca2`, this one) —
the seat-theorem Q-upstream-of-Π revision proposal, its Q-provenance extension, and the seat-cost
measure (gate + grade over foreclosure-sets; kill condition did NOT fire — exhibited Q3/Q5 pair;
audit has the census, controls, and scope declarations). OQ-253 now rides purely on the operator
ruling (adopt/reject the three edits + the three-site exemption-sentence sweep:
seat-theorem-v1.md:138, v8:796, few-seats-v2:31 — the last is the STRONGEST form).

**Two corpus facts worth knowing independent of the philosophy** (manifest
2026-07-25T07:20:20Z, n=199, ee025a0; slots = constraint × context):

1. **`emerges_naturally=true` does NOT mean mountain.** The true-pole is inhabited by five
   types — mountain 6, rope 32 (the `drl_core.pl:423` bypass, inhabited), scaffold 18, snare 8
   (nlwb blocked by `agent_beneficiary` alone), unknown 8. A probe or report treating
   `emerges_naturally` as a mountain proxy misreads 66/72 of the flag's true-pole slots.
2. **`coordination_vitality/2` is authored by ZERO stories on all five live legs and
   kernel_v1** — only the legacy `original_json` archive authors it (grep control ladder in the
   audit). The dead-coordination piton path (`drl_core.pl:354-357`, clause 1 at `:381-388`) is a
   live gate over an authored-empty table; ALL 32 live piton slots ride the theater fallback
   (`:440-448`). Fails closed (dormant, not defective). If a drift/dead-coordination verdict is
   ever wanted measured, re-emit the fact at the generation frontier
   (`generate_constraint_pl.py` emit set) or declare the absence in `design_gaps.md`.

Also of record: `per_constraint.classifications` empty corpus-wide (known OQ-148) — type
censuses must read `per_constraint.perspectives`.

---

## 2026-07-25 — [correction-key] OQ-67 CLOSED: the legacy χ = ε × π path is fully drained; it was UNREACHABLE, not merely deprecated
**Files:** prolog/drl_audit_core.pl, prolog/drl_composition.pl, prolog/stack.pl, prolog/constraint_indexing.pl, prolog/config.pl, python/sweeps/bifurcation_sweep.py, docs/design/design_gaps.md, docs/lawvere_glossary.md, ISSUES.md, audits/2026-07-25_oq67_legacy_chi_retire/
**Tier:** correction-key

Commit `a8ec22f0`. `drl_audit_core.pl` deleted; `constraint_indexing:power_modifier/2` deleted with
it (sole reader). **The χ = ε × π path no longer exists anywhere in the engine** — every χ in the
tree is now the canonical sigmoid χ = ε × f(d) × σ(S).

**The correction this entry carries:** OQ-67 posed a two-way fork — (a) last unmigrated caller, or
(b) deliberately-separate audit path needing a declared-exemption comment. **Both premises were
wrong.** The path was *unreachable*: `stack.pl` loaded the module with an empty import list, its
only importer was `drl_composition`, and all five call sites there sat behind `constraint_data/2` /
`agent_index/2`, both ending in unconditional fail-stubs nothing in the live tree ever asserted.
Migrating dead code, or exempting a path that never runs, would both have been wrong answers to a
correctly-posed question. Three long-standing comments compounded this by naming
**`transition_paths.pl`** as a legacy-path member (`config.pl`, `bifurcation_sweep.py`,
`lawvere_glossary.md`) — already FALSE at HEAD: it computes `derive_directionality_at → sigmoid_f →
scope_modifier` and contains zero `power_modifier` references. All three corrected.

**How citations must change:** citing `power_modifier_*` as a live classifier input is now wrong.
The six params REMAIN in `config.pl:57-62` (specs `config_schema.pl:43-48`) but have **no reader at
all** — they survive as the calibration anchors the `canonical_d_*` values are fitted to
approximate. Consequence for sweeps: a null/zero-flip sensitivity result for those six now means
"no consumer," **not** "no sensitivity" — unperturbable by construction. Pre-2026-07-25 sweep
outputs that report them as inert are describing a different (still-read) regime.

**Disposal was by product, not wiring** (Build Discipline *Unwired ≠ worthless*): 3 of 4 exports
were duplicates (`structural_signature/3` ≡ `omega1_audit:determine_primary_gate/11`; `fm_alert` ≡
`drl_core:type_1_false_summit`; `omega_risk/4` ≡ `drl_core` + `transition_paths`). The 4th was
unique and is preserved as **GAP-29** — the no-exit corner (ε≈1 ∧ χ≈1), inexpressible because
`snare` is unbounded above (`drl_core.pl:389-398` gates on three floors, no ceiling). Two defects
in the deleted code are recorded in ISSUES OQ-67 so they die with it: `fm_alert` bound
`suppression_score` where `logic.md:749` Rule FM specifies ε (and dropped the `∃I(¬■C[I])` leg),
and `omega_risk`'s `type_vi` label is Type **I** per `logic.md:3293`. **Out of scope:**
`omega1_audit.pl` is itself uncalled and retains the surviving χ-only bander — not adjudicated here.

**Method note worth reusing.** The load-bearing witness was the *stub removal*, not the
reachability probe. While the fail-stubs existed, the predicates were defined-and-failing, so any
caller built by `call/N`, `=..`, or meta-dispatch — which a `forall` probe structurally cannot see
— failed silently. Deleting the stubs made them **undefined**, so a post-deletion exit-0 pipeline
run is a *positive* result rather than a null diff. That property was itself witnessed (KILL #2:
all six goals throw `existence_error`) rather than assumed. Breadth was bought on the cheap
instrument: the probe ran against all six corpora (199/960/960/1005/1001/1106, per-process
controls in each leg), the pipeline pair only on `testsets/`.

**Witnesses:** `per_constraint` byte-identical at n=199 across the run pair (exit 0 both, mtime
advanced 02:17:58 → 02:20:47, corpus md5 legs re-checked identical); `check_stack` byte-identical
to a pristine HEAD extract via `git archive` (no worktree); `load_warning_gate` 3/3 allowlisted, 0
unexpected; `./scripts/gate.sh` GREEN.

---

## 2026-07-25 — [correction-key] Gate 2 for `entropic_universe_hypothesis` RE-RULED; its June basis was void two days after it was made
**Files:** prolog/narrative_ontology.pl, prolog/signature_detection.pl, ISSUES.md, audits/2026-07-25_oq66_nlwb_filter_cutover/GATE2_REWITNESS.md
**Tier:** correction-key

**Do not cite the June gate-2 note.** The provenance question OQ-248 raised was RESOLVED by running
the discriminator (era engine extracted via `git archive`, no worktree). Three arms, A vs C being the
single-variable isolation:

| arm | engine | corpus | signature | dr_type | shadow |
|---|---|---|---|---|---|
| A | HEAD | kernel_v1 (1106) | `coupling_invariant_rope` | rope | rope-0.95 |
| B | `f600599b` | era testsets (1103) | `natural_law` | mountain | mountain-0.95 |
| C | `f600599b` | kernel_v1 (1106) | `natural_law` | mountain | mountain-0.95 |

Same corpus, different engine, opposite answer ⇒ **corpus regime REFUTED, engine regime CONFIRMED.**
The 2026-06-03 read was substantively CORRECT in its own regime. Cause of the change: **OQ-70**
(`72ec2cdd`, 2026-06-05) removed the `claimed_natural/2` source maxwell's certification rode.
**The gate-2 premise expired two days after it was ruled, and the entry it licensed was never
re-checked** — it then operated as certified for ~7 weeks on a void basis.

**RULED (operator, 2026-07-25): KEEP the entry, as a NEW DATED ruling — not a re-citation.** The
entry was NOT continuously certified 2026-06-03 → 2026-07-25. Recording it as a re-citation would
hand the next reader a pass that was never re-taken.

**METHOD TRIPWIRE — gate-2 reads must name the DISCRIMINATING surface.** The June pass cited
evidence that could not discriminate: the PASS case (maxwell) and the HELD case
(tech_inevitability) carry **identical** ε=0.08 and suppression=0.02, and maxwell-identical shadows.
Metrics and shadow were **decorative**. The June note's "omegas authored empty" is also factually
wrong — maxwell authors 11. The real discriminator is **what the omegas are ABOUT**: maxwell's bear
on physics grounding (is the second law fundamental or emergent); the held case's bear directly on
agency (*"does deployment require intentional beneficiary strategy?"*) — which is gate 2's own
question. **Rule now in the two-gate block: state which surface DISCRIMINATES and check it against a
known gate-2 FAIL; a surface shared with the failing case is corroboration at best.** The host's own
"no human agent benefits" is admitted as HOST TESTIMONY, not independent evidence — it is authored
by the story that gains from the release.

**Two OQs minted.** **OQ-251** (Priority 2): post-OQ-70, does ANY path exist by which a paradigm
natural law certifies `natural_law` absent an explicit story-level claim — *and did OQ-70 intend
that scope?* Removing an over-broad bait clause and eliminating every route are different rulings.
If `natural_law_signature` now fires only on explicit declarations, it has drifted from measuring
**structural naturality** to measuring **authorial declaration**, and every downstream consumer
inherits that silently. Gates the OQ-248 kill condition. **OQ-252**: rulings carry no back-reference
to what they license — witnessed twice this session (the reset at 7 weeks, OQ-70 at 2 days).

**`Licenses:` is FORWARD-FACING ONLY (operator ruling, 2026-07-25).** Added when a ruling is created
or revisited from this date onward; **never backfilled**, no sweep. **Corollary: absence on an older
ruling means "predates the convention," NEVER "licenses nothing"** — reading a missing field as an
assertion of no dependents is Pattern 5. First instance is on the re-ruled gate 2 in
`narrative_ontology.pl`.

---

## 2026-07-25 — [tripwire] OQ-66 CLOSED: nlwb agent-filter landed; a plain `[stack]` load leaves MaxEnt UNFITTED while reads fail soft
**Files:** prolog/drl_core.pl, prolog/tests/test_agent_beneficiary.pl, prolog/tests/fixtures/nlwb_controls/, python/run_pipeline.py, prolog/maxent_classifier.pl, prolog/abductive_triggers.pl, prolog/narrative_ontology.pl, ISSUES.md, CLAUDE.md
**Tier:** tripwire

**THE TRIPWIRE (the reason this is not just history).** **A plain `[stack]` + corpus load leaves
MaxEnt UNFITTED** — `maxent_dist/3` is empty, `maxent_run_info/3` is empty — and
**`maxent_entropy/3` / `maxent_top_type/3` FAIL rather than throw** on the missing fact. Witnessed:

```
MAXENT maxent_dist_facts_after_stack_load=0
MAXENT sample=abrahamic_covenant__isaac_covenant_reading top=FAILED(no fit)
ENTROPY FAILED (no exception) -- catch/3 does NOT intercept
```

Two consequences a fresh instance will otherwise hit silently:

1. **`catch/3` around a MaxEnt read does not intercept the unfitted case.** It fails. A
   `catch(maxent_entropy(...), _, (H = 0.0))` recovery goal never runs; the enclosing clause
   just fails.
2. **Any probe or suite that reads MaxEnt observables under `[stack]` alone measures NOTHING,
   and a soft-failure mapped to a placeholder makes that indistinguishable from a real
   result.** This is the defect that made the OQ-66 guard vacuous for its whole life: the old
   `test_agent_beneficiary.pl` mapped the failure to `no_top` in BOTH arms of a raw-vs-filtered
   diff, so it compared `[no_top,no_top,no_top,no_top]` against itself and presented as
   zero-diff. Pattern 6, inside the instrument.

**To read MaxEnt at all:** `maxent_classifier:maxent_cleanup, maxent_classifier:maxent_multi_run(Ctxs, _)`
first, then ASSERT `maxent_dist/3` is non-empty before any read. MaxEnt is corpus-fitted state
deliberately OUTSIDE `cache_registry`, so `clear_all_caches/0` does not touch it — a cache clear is
not a refit. Template: `audits/2026-07-25_oq66_nlwb_filter_cutover/nlwb_diff_harness.pl`.

**Scoped to the class, and the wider claim was CHECKED AND REFUTED.** The suspicion that
`abductive_triggers.pl`'s six plausible-value fallbacks (`HNorm = 0.0` at `:86,:135,:358,:711,:771`,
`ShadowTop = unknown` at `:188`) are a live Pattern-6 — an entropy of 0.0 reading as maximal
certainty — **is not real, twice over.** (a) Those sites are bare `catch/3`, and the reads fail
rather than throw, so the recovery goal never runs. (b) Every one of those clauses is gated at its
FIRST goal by `subsystem_available(maxent)` (`:75,:126,:177,:231`), which checks `maxent_run_info/3`
— empty under a plain `[stack]` load, so the clause fails before reaching the fallback.
`abductive_triggers.pl` already carries the provenance guard. **No OQ minted for those sites.**

**LANDED (OQ-66 resolved).** `drl_core:natural_law_without_beneficiary/1` now reads
`narrative_ontology:agent_beneficiary/2` instead of raw `constraint_beneficiary/2` — ruling 63-A,
operator Q1 2026-07-25. It was the last unmigrated consumer of that class.

**Say the result at the right quantity — "behaviourally free" is the WRONG label and it is the
one a later reader will reuse.** **ZERO OBSERVABLE DIFF on six legs** (five live + `kernel_v1`),
under cache-cleared and MaxEnt-refitted arms, with a planted-flip fixture leg proving the harness
can see a change. **But ONE PREDICATE-TRUTH FLIP** at `maxwell_demon_impossibility` (kernel_v1) —
downstream-invisible only because it classifies `rope` in both arms. And the no-op is **STRUCTURAL
on the five live legs** (forced by `registry_hits=0` ⇒ extensional identity) and **CONTINGENT on
`kernel_v1`** (holds only because one constraint's metrics land in rope territory). Forward
statement to cite: *no observable change on the checked corpora; the first live constraint carrying
a registered non-agent beneficiary with snare-range metrics will classify differently than it would
have pre-cutover.* Consumer surface + declared residue (the tangled_rope block has no dedicated
fixture — `nlwb` forbids `requires_active_enforcement` by construction):
`audits/2026-07-25_oq66_nlwb_filter_cutover/RELEASE_NOTE.md`.

**METHOD NOTE (carry forward).** The plan's stop point was specced to fire on a non-zero *diff*,
but what it protected — the operator's seat on the release note and the consumer re-audit scope —
is triggered by a *predicate flip*. The flip happened, the trigger did not fire, and the release
note got written after the commit instead of before. **Key a stop point on the quantity that
carries the meaning, not the one the harness happens to emit.**

New standing pipeline
gate `_prolog_agency_gate()`; its FIXTURE pass is what makes it non-vacuous, because the live legs
carry zero registered beneficiary values and a revert of `drl_core.pl` keeps the live-corpus suite
GREEN. Break control witnessed: reverting throws `agency_nlwb_set([nlwb_ctl_no_beneficiary])`.

**CORRECTION-KEY rider — the maxwell gate-2 evidence does not re-witness.** The registry entry for
`entropic_universe_hypothesis` records its gate-2 justification as "MaxEnt shadow 0.990 mountain /
entropy 0.031" (2026-06-03). The first properly-fitted read of `maxwell_demon_impossibility` on
`kernel_v1` gives **shadow rope 0.95 / entropy 0.156 / mountain 0.010**, signature
`coupling_invariant_rope`, `dr_type` rope at all four contexts. Controlled against a degenerate fit
(same run spans all six shadow types, `mountain-39 … tangled_rope-641`, entropy 0.0011–0.6111).
**Scope: this says the numbers do not reproduce on `kernel_v1` at HEAD — NOT that the 2026-06-03
read was wrong.** That read was on the then-live pre-reset corpus, MaxEnt is corpus-fitted, and the
signature layer has changed repeatedly since; attributing the gap to corpus vs. engine regime needs
a stage-hash diff, not run. **Not acted on** — re-ruling a `non_agent_beneficiary/1` entry is a
gate-2 ruling and the operator's seat. Routed to OQ-248 as its opening datum and flagged.

**Ledger.** Both gate-two items close **moot-by-reset** (`technological_inevitability_interpretation`
absent from all five live legs; the `statutory_debt_ceiling` names in `haiku`/`flash` are new draws,
not the measured story). Findings relocated, not folded: shadow separability → **OQ-248** (Ω_E,
GAP-19 cross-link in prose — `Deps:` edges take OQ targets only); (ε, theater) × type census →
**OQ-249** (Ω_E, gates OQ-90).

**Five live legs, not three** — see the CLAUDE.md Critical Distinctions correction in this session.

Evidence: `audits/2026-07-25_oq66_nlwb_filter_cutover/FINDINGS.md`, commit `1613c3cc`.

---

## 2026-07-25 — [tripwire] OQ-62 CLOSED: four purity banders renamed to disjoint vocabularies; exactly one `purity_zone/2` survives
**Files:** prolog/logical_fingerprint.pl, prolog/fpn_report.pl, prolog/giant_component_analysis.pl, prolog/abductive_helpers.pl, prolog/abductive_triggers.pl, prolog/signature_detection.pl, prolog/purity_scoring.pl, prolog/tests/test_purity_bands.pl, prolog/tests/test_purity_absence.pl, python/husk_signature_read.py, python/enhanced_report.py, docs/logic_extensions.md
**Tier:** tripwire

**The tripwire.** There is now **exactly one** bander named `purity_zone/2` and it is the
canonical spec one (`logical_fingerprint.pl:614`, logic_extensions.md §2.3). The other three are
`fpn_report:ep_band/2`, `giant_component_analysis:action_band/2` and
`abductive_helpers:fpn_band/2`, and they are **not interchangeable with it or each other** —
different quantities, different cut points. A future agent who unifies them, or who "restores"
the shared name, reintroduces a defect that fails *silently*: the bands still compute, the
reports still render, and the numbers are wrong by one cut-point. Convention table with the
quantity each one bands: `docs/logic_extensions.md` §2.3.1.

**Second tripwire, opposite direction.** All four banders return the SAME `unknown` token, which
is a literal overlap against the disjointness rule and is **deliberate** — unlike the colliding
words, `unknown` means the same thing everywhere (input absent or out of range, fail closed).
Do not "fix" it; doing so undoes the fail-closed guarantee. The guard clause order is also
load-bearing: `\+ number(S)` must precede `S < 0.0`, because the comparison throws on the atom.
Exactly 0.0 is a real score, not an absence, and still bands worst.

**What was wrong.** Three modules each defined `purity_zone/2`; three words collided, not the one
OQ-62 recorded — `contaminated` ([0.30,0.50) vs [0.40,0.60)), `degraded` (<0.30 vs [0.30,0.50)),
`critical` (<0.30 vs <0.20). With the categorical `contaminated(Reasons)` (now `purity_fail`),
one word meant four things. All four banders also mapped the −1.0 epistemic-gate-fail sentinel to
their WORST zone, and two threw `type_error(evaluable, unknown/0)` on the OQ-60 no-data atom.

**Three premise corrections** (detail + witnesses in ISSUES OQ-62 and
`audits/2026-07-25_oq62_band_vocabulary_fork/CALL_SITE_CENSUS.md`):
1. *The fork was 4 banders, not 2.* The authoring audit (2026-06-03) never mentions `fpn_report`
   or `giant_component`, and does not cite `audits/2025-05-15_recon_2/`, which had already
   recorded three `purity_zone/2` implementations. **Predicts sibling undercounts from the same
   audit** — treat its other counts as floors, not totals.
2. *The sentinel path is structurally unfiltered but empirically inert — three claims, three
   warrants, do not merge them.* **(a)** unfiltered = code read (only the intrinsic is gated).
   **(b)** no leg exercises it = WITNESSED, six corpora, measured at the bander INPUT, pure
   `value` on every leg (testsets 153 rows / haiku 492 / flash 668 / kimi 700 / sonnet 930 /
   kernel_v1 1102). **(c)** *why* = DATA on one leg, NOT traced: IP-absence and EP-absence are
   set-equal on testsets (28 ≡ 28, membership not cardinality), so the `IP >= 0.0` filter is
   co-extensive with EP-absence *here*; whether it structurally guarantees exclusion was never
   traced. Under (c) the path is **unexercised, not unreachable** — the guard's real value is that
   it converts a data-dependent property into a code-guaranteed one. Do not cite a 0-`critical`
   count on one leg as evidence about reachability; it is that leg's purity distribution.
3. *The throw was never loud.* `abductive_engine.pl:145` wraps every trigger in
   `catch(_, _, true)`, so the `type_error` was already being discarded. Guarding converted one
   silent path into another.

**Two method traps hit and recorded** (both produced confident wrong answers before the control
caught them):
- A reachability probe using the atom `default` as context, instead of
  `constraint_indexing:default_context/1`, landed off the authored grid: `fpn_run/3` failed and
  every accessor reported 0 successes — which reads exactly like "the path is unreachable"
  (OQ-178 dual). The `fpn_run` success count is now the probe's positive control.
- **In-process multi-leg iteration is unsound.** Retracting `corpus_loaded/0` and
  `corpus_constraint/1` does NOT retract the `narrative_ontology` facts the testset files
  asserted, so legs accumulate and `sort/2` masks it behind ID dedup. The tell was kimi and
  sonnet returning byte-identical counts; re-run one leg per **process**, they differ (700 vs 930
  rows). Any future multi-leg sweep must fork per leg. **Blast radius is not local: this
  invalidates any prior in-process multi-leg measurement in this project** — now **OQ-246**,
  Priority 1, carrying the detection recipe (two distinct legs agreeing to the row is the
  signature; a contaminated leg reports a SUPERSET, so "found X on leg L" may be "found X on legs
  1..L"). The six-leg table above was measured per-process AFTER this discovery — verifiable from
  the numbers, since the in-process run put haiku at 642 rows and the table carries 492.

**The rename check byte-identity could not provide (and the defect behind it).** `fpn_band/2`'s
only consumer is trigger 6, which fires 0, and `abductive_engine.pl:145` swallows every trigger
exception — so a missed call site would have left 0 firings, a byte-identical
`abductive_report.md` and a green gate, exactly as a correct rename does. Closed by
`trigger6_control.pl`: T6 called directly outside the catch on all 181 constraints → **0
exceptions**; reach-depth then shows control actually arrives at the renamed goals
(`:525 fpn_band/2 → unknown`, `:526 one_hop_band/3 → failed cleanly`; a missing predicate throws
rather than fails, so a cleanly-failing goal resolved). The overlay route was unavailable — both
blockers are static procedures, so `assertz` raises `permission_error`. `:534`'s `evidence_line`
key is term data, not a goal, so it is read-verified only. **Incidental:** `:525 → unknown` is the
Phase-1b guard firing live in the real trigger path (pre-guard `fpn_critical`), so the guard does
change an intermediate value at the 28 `-1.0` constraints — "inert" is exact about output, not
evaluation. **The blanket `catch(_, _, true)` is now OQ-247:** all ten trigger firing counts are
ambiguous between "didn't fire" and "errored," which means the 0-firing count for
`accelerating_pathology` cited when OQ-62 opened was never a witness of non-firing.

**Straggler class worth remembering.** `python/husk_signature_read.py` parses
`outputs/fpn_report.md` and gated `proxy_husk` on the literal string `"critical"`. Post-rename
that matches nothing and reports zero proxy husks — success-shaped, reads like a finding. It is
**not wired into `run_pipeline`**, so no pipeline diff would ever have caught it; only the
unfiltered Pass-B token sweep did. Its columns were also named `fpn_zone`/`one_hop_zone` while
holding `fpn_report` values, i.e. named after the wrong bander.

**Witnesses.** `a2ef8147` (docs) · `a1902cb1` (guard) · `295260e7` (2a renames) · `13877a0c`
(2b categorical). `test_purity_bands.pl` RED at HEAD (7 failed / 9 passed, both throws captured)
→ GREEN 16/16 with 7 positive controls. Pipeline exit 0 + mtime advanced at each phase;
`per_constraint` byte-identical throughout; `fpn_report.md` byte-identical after back-substituting
the new atoms; the other two reports byte-identical untouched. `structural_purity` verdict mix
preserved exactly across the rename (purity_fail 151 / inconclusive 35 / inconclusive_nodata 4 /
pure_coordination 9). purity_absence 7/7, reading-totality 10/10, `[GATE]` GREEN.
Follow-ons minted: **OQ-244** (scalar identity — do any two band the same quantity?),
**OQ-245** (is the ≤0.05 excess bar calibrated, or is 96.6% failure the finding?), **OQ-246**
(in-process leg accumulation), **OQ-247** (blanket trigger catch-all).

---

## 2026-07-25 — [landed] schemas.py caught up to three producer landings; the drift warning is the only thing that noticed
**Files:** python/shared/schemas.py, prolog/json_report.pl, python/enrich_pipeline_json.py
**Tier:** landed

`PIPELINE_FIELDS` now registers `epsilon_provenance` (OQ-205), `fingerprint_shift`
(OQ-53/GAP-04) and `repair_transitions` (OQ-91), plus the two enriched ε-stability fields.
**OQ-205 was already RESOLVED (build landed 2026-07-03)** — this was its missing last step.

**The lag is the finding.** json_report.pl emitted all three for ~3 weeks while the schema
contract did not list them, and the only signal was `validate_pipeline_output`'s "unexpected
field" drift warning — firing 3× on each of 199 rows, into stderr, every enrich run, noticed
by nobody. A drift warning that never escalates is a Pattern-6 channel: it distinguishes
*contract-complete* from *contract-lagging* correctly and then emits both into the same
ignored stream. **Registering a field in `python/shared/schemas.py` belongs in the same
commit as the `json_report.pl` emit** — the contract is a consumer of the emit, and Pattern 1
("a producer is not done until something consumes its output") covers it.

**Nullability method (reusable).** A wrong NON-nullable declaration makes `enrich_pipeline_json
.py` hard-exit — same failure class as the stale `purity_class` that broke the chain the day
before. So each declaration was witnessed at two altitudes: emit-site structure (which branch
can write what) AND branch coverage on the live leg. `epsilon_provenance` non-null because
`write_epsilon_provenance/2` is a total if-then-else with both arms writing `{...}` (all four
paths fired: 71 authored / 110 derived / 18 unknown_author); `repair_transitions` non-null
**by construction** — the emit writes literal `[` / `]` around `write_repair_array`, so no
corpus can make it null (196 empty / 3 non-empty); `fingerprint_shift` nullable via its
explicit `FsList = null` arm (`json_report.pl:313`) — that arm did NOT fire here (199/199
lists), noted because it is the permissive direction and so cannot break a consumer.
Presence/nullability is emit-structure-determined, not corpus-determined, which is why this
was not run across the other four legs.

Witnesses (manifest 2026-07-25T05:34:25Z, commit `13877a0`, n=199): both validators 0 errors,
and **0 unexpected-field drift warnings** on either artifact — the contract is now complete
w.r.t. what the engine emits.

## 2026-07-24 — [landed] OQ-60 consumer sweep came due: `unknown` crashed the trajectory step; Prolog stderr reporting was masking it
**Files:** prolog/context_profile_mining.pl, python/run_pipeline.py, python/shared/schemas.py
**Tier:** landed

Pipeline went 47/48 → **48/48** (`python3 python/run_pipeline.py`, exit 0, 28.6s;
`trajectory ok [3.1s]`, `outputs/context_profile_report.md` 7,125 bytes — previously
written EMPTY because the step errored).

**The crash (commit `ab748fc6`).** `context_profile_mining.pl:434` read
`normalize_purity(P, 0.5) :- (P =:= -1.0 ; \+ number(P)), !.` Both disjuncts intend to
map an absent purity to 0.5, but the guards are in the fatal order — `=:=` evaluates its
args, so `P =:= -1.0` **throws** on the atom `unknown` before `\+ number(P)` is tried:
`ERROR: =:=/2: Arithmetic: 'unknown/0' is not a function`, at
`[trajectory] Computing 16290 pairwise distances`. Reordered (non-number guard first),
split into two clauses so each OQ-60 token is named at its own site. This is the **OQ-60
consumer sweep coming due, not a new defect**: `purity_scoring.pl:49-55` introduced
`Score = unknown` with the comment "propagate `unknown` rather than feeding it to the
weighted sum (which would throw)" and marked the path "inert until a producer emits
`unknown`" — a producer has now landed (live corpus: `purity_class` = 153 scored / 35
gate_fail / **11 no_data**), and this consumer one level down did exactly that throw.
`normalize_purity/2` is the sole chokepoint (line 426 `PurDiff` is the only purity
arithmetic in the trajectory path; swept `context_profile_mining.pl` +
`context_profile_report.pl` for other `Pur*` arithmetic — none).

**Why it was hard to see (commit `55c8b242`).** `run_prolog`'s failure path did
`result.stderr[:300]`. SWI emits load-time warnings for hundreds of lines before the
ERROR, so a head-slice is structurally guaranteed to be noise **on every failure across
all 12 Prolog steps**. The real stderr here was 259,426 chars / 2,311 lines; the head-300
showed two "Local definition ... overrides weak import" warnings and cut off mid-word, so
the summary reported a warning and never mentioned the exception that ended the run.
Added `salient_stderr()`: prefer ERROR lines, fall back to the **tail**, never the head
(Build Discipline Pattern 6 — a channel that cannot tell payload from noise emits
noise-shaped output either way).

**Checked, NOT a defect.** `json_report.pl:1347/1349` (`write_one_neighbor`) filters
neighbor purity with a bare `NP \= -1.0` — no `number/1` guard — where its twin
`write_contamination_network:1282` uses `number(IP1), IP1 \= -1.0`. `unknown \= -1.0`
succeeds, so the atom does pass that filter. It is **defended at the emit boundary**:
`write_json_number/2:2549` has an explicit `unknown → null` clause plus a non-numeric
catch-all. Verified on output, with the positive control that the site is genuinely
reached: 26 neighbor-writes involve `no_data` constraints, and all 26 emitted `null`
(neighbor purity values: 188 float / 28 null / **0 string**). The asymmetry is
redundancy, not a bug — do not "fix" it expecting a behavior change.

**Open (needs a ruling), not filed as an OQ yet:** `normalize_purity` maps `unknown` to
**0.5** — a fabricated plausible value inside an HAC distance component (Pattern 6). The
fix preserved the clause's evident pre-existing intent; excluding the purity component
and re-weighting when either side is absent would change clustering output. Also unswept:
~50 other `purity_score/2` call sites across ~15 modules. The loud shape (arithmetic)
would crash and the pipeline is green, so none of the *reached* sites throw on this
corpus — but that is "didn't find it," not "isn't there."

## 2026-07-24 — [landed] OQ-61 CLOSED: header purity/cascade three rulings (severe fraction + type×band tab + gate_fail/no_data split); "purity restates type composition" premise WITHDRAWN; residual → OQ-239/240/241
**Files:** prolog/json_report.pl, prolog/network_dynamics.pl, python/enhanced_report.py, prolog/tests/test_purity_absence_class.pl, python/tests/test_oq61_network_render.py, ISSUES.md, python/shared/schemas.py
**Tier:** landed

Commit `ae9b0848`. Three operator rulings on the corpus header, **report/aggregation only** —
proven additive (behavior-preservation: two changed-code runs canonicalize identical; HEAD-vs-changed
adds 6 diagnostic keys + per-row `purity_class`, ZERO changed shared values, `network_stability`
token byte-identical). Q1: header severe **fraction** (four fail-closed branches from clause order)
replaces the saturated categorical (633/643 severe at absolute threshold 3); shared helpers
`network_drifting_constraints/2`+`network_severe_constraints/3` extracted (behavior-preserving);
`severity_by_type` backstop tab (severe total == `network_n_severe`, asserted). Q2: type×band tab
(render-only, marginal-asserted) headlining the off-diagonal residual. Q3: unscored split into
`gate_fail`(−1.0)/`no_data`(`unknown`); `malformed`(out-of-range) is a **fail-closed guard-class,
NOT a fifth token** — the emit halts on it. Reproduces the census sentinel+flip split exactly on
all four target legs (testsets 46=35+11, haiku 468=466+2, flash 292=212+80, kernel_v1 4=2+2);
kimi/sonnet fresh. Fixtures: 14 Prolog classifier tests + 16 Python render tests, existing 17
purity-absence green.

**Correction (operator ruling, escalated call):** the plan pre-registered a rule gating Q1 on
"the corpus-stability line has no residual signal beyond type composition." It **FAILS across the
corpus family** — off-diagonal severe mass (severe rope+mountain / n_drifting) > 5% on all six legs
(scored-denominator: 5/6, only kimi passes); mountain within-type under-severity real on large-n
legs (flash n=51). The premise is **WITHDRAWN** — do NOT cite OQ-61 as establishing that purity
restates type composition. The header change stands on **saturation grounds alone** (633/643 is
information-free regardless), and the residual *strengthens* the ruling's per-component future.
Nothing implemented reverses. Residual split into OQ-239 (per-component severity home + the rule's
two defects: no n-floor, unprincipled 5%), OQ-240 (off-diagonal cover-story population — a
classification/calibration question, not report-text), OQ-241 (`ep_base_severity` fixed-0.70 cut
type-interaction). Audit: `audits/2026-07-24_oq61_header_purity_cascade/`.

**Follow-ups (both done at operator request, 2026-07-24):** (1) `purity_class` registered in
`python/shared/schemas.py` `PIPELINE_FIELDS` (non-nullable) + `PipelineConstraint` — commit
`ae3090d2`, isolated from the operator's uncommitted OQ-205 schema WIP via stash/commit/restore
(non-overlapping region; verified 0 validation errors, no drift warning). (2) CLAUDE.md updated
**THREE→FIVE LIVE LEGS** (added `testsets_kimi` 1005, `testsets_sonnet` 1001; disk-verified
2026-07-24); MEMORY.md `project_corpus_reset_2026_06_05` note refreshed. NB: the operator's
uncommitted set (`schemas.py`, `validation_suite.pl`, `cs_reading_relation_quarantine.json`,
`oracle_gap_results.json`) is OQ-205 ε-provenance engine WIP, **not** essay constraint stories —
left intact.

## 2026-07-24 — [landed] OQ-152 / OQ-153 / OQ-227 husk bundle CLOSED; `update_authority` field is validated-but-dormant; two unfireable reviver conditions
**Files:** ISSUES.md, docs/design/design_gaps.md, docs/design/update_authority_rubric.md, prolog/narrative_ontology.pl, prolog/data_validation.pl, prolog/cs_drift_engine.pl, prolog/tests/test_cs_drift_engine.pl
**Tier:** landed

The three closes (reasoning in ISSUES.md; evidence in `audits/2026-07-24_oq152_seat_crosssection/` and
`audits/2026-07-24_oq153_step3_blind_pass/`):
- **OQ-152 disposed** — the per-seat naturalization-collapse cross-section is unfingerprintable:
  suppression is a constraint-level *gate* (not a seat dial) and the seat-χ ordering is config-fixed
  by `role→d` (0/158 within-constraint crossings). Spun out **GAP-27** (`agent_power` is inert for
  seat χ under δ=0).
- **OQ-153 resolved → (c) decline** the five-condition husk annotation. Two findings drove it:
  `dead∧frozen = 0/8` under enrichment (the husk conjunction is empty — a *mechanism* for
  `husk_signature_read.py` K=0), and condition-5 independence is *untested* (the corpus cannot populate
  non-canon `frozen`; 3/4 shape-test items failed to instantiate the shape — not "proxy," untested).
- **OQ-227 resolved on C1+C2** — surviving-referent precondition is a structural tripwire
  (`test_cs_drift_engine.pl terminal_set_pinned`); `acknowledgment_collapse` routed to the standing
  trigger; `sealed_closure` row OPEN.

**`update_authority` — the item most likely to be wrongly deleted.** A validated institutional field
(enum `{licensed_revisable, frozen, absent_diffuse}` + authoring token `unauthored`; rubric
`docs/design/update_authority_rubric.md`) with **ZERO authored facts, no consumer, no generation-schema
emission — declared-dormant by design** (authored-on-demand by audit passes). Surface = the dynamic
fact in `narrative_ontology.pl` + validators in `data_validation.pl` (enum/uniqueness/orphan +
measure-only `inconsistent_update_authority/2`). It is NOT dead schema — provenance is the OQ-153 close.
Do not remove it as "unused."

**Two UNFIREABLE conditions — reviver documentation, nothing monitors them** (do not read as tracked):
GAP-28's reopening condition (≥3 non-canon live-foreclosed-amendment instances — only checkable by
authoring the dormant field) and OQ-227's `sealed_closure` near-miss (no authored referent-dissolution
signal exists to search on). Both are surfaced only by a **corpus-expansion / generation-scope
decision**, not a query.

**Pattern for the next instrument build:** *selection on the outcome variable* recurred four times
across this arc (all caught) — enriching a sample so a needed value is present severs the sample from
the question. Guard: pre-register a selected-for value as supply-only and exclude it from any test that
reads presence as evidence; only *absence under enrichment* carries information. Full note:
`audits/2026-07-24_oq153_step3_blind_pass/RESULTS.md` → "PATTERN FOR THE NEXT INSTRUMENT BUILD".

---

## 2026-07-23 — [tripwire] OQ-60 RESOLVED: no-data purity is `unknown`/JSON null (never 1.0); fabricated boltzmann_floor_default removed; two absence tokens must never be coerced or averaged
**Files:** prolog/purity_scoring.pl, prolog/boltzmann_compliance.pl, prolog/signature_detection.pl, prolog/json_report.pl, prolog/network_dynamics.pl, prolog/giant_component_analysis.pl, prolog/maxent_report.pl, prolog/grothendieck_cohomology.pl, prolog/drl_boltzmann_analysis.pl, prolog/context_profile_mining.pl, prolog/tests/test_purity_absence.pl, prolog/tests/test_coexists_fpn_canary.pl, python/enhanced_report.py, prolog/config.pl, docs/logic_extensions.md
**Tier:** tripwire

OQ-60 deliberate pass completed (rulings R1–R4; commits `bc9bffde`→`d051d06c`; full witnesses in
`audits/2026-07-17_oq60_purity_absence/*_2026-07-23.md`).

**READ rule (promoted to CLAUDE.md Architecture Invariants):** purity now carries TWO absence
tokens — engine `unknown` (no-data) and `-1.0` (epistemic-gate-fail sentinel); JSON serializes
BOTH as `null`. Never coerce either to a number, never average them in, never read `.get(...,0)`.
`purity_zone(unknown)=unknown`; a `purity_band` of JSON null covers both causes.

**WRITE rule:** a clean/dispositive aggregate over purity (pristine/stable/pure_*) gates at
coverage 1.0 → distinct abstention token (`inconclusive(no_data)`, `undetermined`); positive
existentials (contaminated/cascading/drift) fire through unknown members; every descriptive
purity stat carries `n_scored/n_total` unconditionally (json `diagnostic.purity_n_scored/_n_total`;
report coverage lines).

**Ordering trap:** atoms sort BEFORE numbers — an `unknown` reaching msort/max_member silently
heads the list. Guarded at the two cache boundaries (fpn/gc precompute collapse unknown→-1.0 for
their `>= 0.0` filters); new sorts over purity must guard `number/1` (ordering audit:
`ORDERING_AUDIT_2026-07-23.md`; tests 6–7 of test_purity_absence.pl).

**Fixture rule:** synthetic test constraints that need SCORABLE purity must now AUTHOR
`coordination_type` (+ extractiveness) — the engine no longer fabricates a floor (witnessed: the
FPN canary fixtures and the preflight non-target control both broke on this and were repaired).

**Ripple (declared, attributed):** removing 93 fabricated floors moved corpus-relative layers
(maxent empirical-profile fits, wasserstein, arakelov, signature_pressure, FPN contamination) —
headline `classifications` changed on ZERO rows; 12 near-boundary rows flipped shadow
maxent_top_type, 9 downgraded verdict_join red→yellow via the maxent-divergence alert; 1
gate-fail flash row lost its fabricated excess_above_floor FCR failure. Scorable-mean purity by
leg: testsets 0.5450, haiku 0.4916, flash 0.5711 (moved DOWN as predicted — the operator's
falsifier), kernel_v1 0.4813. Cross-leg scorable means NOT comparable (OQ-236).

---

## 2026-07-23 — [correction-key] *Hearts of Glass* is NOT a pipeline artifact (provenance witnessed); Commitment Systems adjudicated as blocked-B refinement of the debugging-philosophy taxonomy
**Files:** blog/2026-07/hearts-of-glass.md, blog/2026-07/implied_machine_reader.md, agent/analysis/originals/machine_reader.md, prolog/cs_drift_engine.pl, docs/commitment_systems/type_b_adjudication_2026-07-23.md, docs/debugging_philosophy.md, ISSUES.md
**Tier:** correction-key

Two rulings from a Claude-web planning conversation (2026-07-23), landed to substrate.

**1. *Hearts of Glass* (`blog/2026-07/hearts-of-glass.md`, commit `50d7ddab`) sits OUTSIDE
the narrative_transform/uke pipeline — do not cite it as pipeline output.** The conversation's
kill condition ("a stage-1 constraint spec or a .pl behind it locally would refute this") was
run this session and does NOT fire. Witness: no `agent/narrative_transform/uke/hearts*` run
dir, no `originals/`/`stories/` entry, no `prolog/testsets/*.pl`, no `json/` spec; positive
control `quellcrist` found across all those surfaces, so the probe finds pipeline artifacts.
Corroborated three ways by the operator: (a) not written with agent/uke-narrative (where a uke
artifact would appear); (b) the process leaves a characteristic artifact shape (uke run dir +
`source_story.txt` + staged outputs) the story lacks; (c) the pipeline requires a seed — the
story's actual seed was a conversation about human hibernation. **Distinction to preserve:
"the system" (Prolog/uke) ≠ LLM assistance** — the commit carries a Claude co-author line, so
model collaboration happened; pipeline involvement did not. Operator framing: "the system is
my creating Prolog that thinks like I do." FLAG RESOLVED same day (operator authorized the
edit): the provenance sentence — "from this site's pipeline — grown ... through the
multi-model process" — corrected in BOTH copies (`blog/2026-07/implied_machine_reader.md`,
the later revision staged to publish, and the older draft
`agent/analysis/originals/machine_reader.md`) to affirm multi-model involvement while denying
pipeline involvement. The story remains OQ-227's *test-rig* (the 2026-07-18 entry below).
Sharper description than "outside the system" (operator, same day): the story is an
*instrument* for the thing the engine cannot yet formalize — `acknowledgment_collapse` (the
ratifying authority that can no longer tell faith from its performance) is the Keeper's
terminal state near verbatim. Not pipeline output; aimed at the pipeline's frontier.

**2. Commitment Systems classified: a refinement on Type B, by the fix-identity criterion
(operator ruling) — full adjudication in
`docs/commitment_systems/type_b_adjudication_2026-07-23.md`.** Types are individuated by
their FIX, not their generating mechanism ("drift-generated" labels the A bucket, doesn't
gate entry; frame-fixing passes trivially-and-uninformatively on a drifting institution,
which is not a confused reasoner). The refinement precisely: debugging_philosophy's Type B
silently assumed the reviser exists (Russell→ZF, Liar→Tarski); drop that and B splits into
revisable-B vs **blocked-B** — Commitment Systems is the theory of blocked-B, the five
patterns its map. A proposed "Type D (extraction-generated)" collapses into B (standing is
constitution-level). Two marked open notes in the adjudication file: the Euclid exception's
seat at axiom-set choice, and remedy-identity making the framework a theory of repair rather
than drift. **Round-2 sharpening (same day, adjudication Addendum 1): blockage is a GATE on
remedy execution, not a type** — restoring standing is the enabling condition for running a
fix, not a fix (nobody resolves Sorites by acquiring authority); trifurcation stays at three;
Commitment Systems is the theory of the gate; Stage 0 in precise form = "does an agent exist
who can execute the fix and wants to." **Round 3 (Addendum 2 + OQ-235 minted):** three-valued
gate proposed, unruled (vacancy/capture/bandwidth — treatments disjoint: build authority /
change incentives / add throughput); the §5.9 closable-by-citation check returned the
OPPOSITE of the Euclid case — v8:700–706 names the same wall (acknowledged bit authored-not-
detected, evidence thinnest where formation completed) as its own honest residual. The open
capability question — a detected (vs authored) acknowledgment surface via self-account/
non-report-practice divergence, plus conversion-rate predictors — is **OQ-235**.

---

## 2026-07-23 — [correction-key] Axiom 2's empirical anchor currently has no runnable falsifier (OQ-232 resolved; falsifiers rescoped in v8 §9.5 + v6.13.1:88; class OQ-234 minted)
**Files:** docs/deferential_realism_paper_v8.md, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, audits/2026-07-23_oq232_falsifier_redesign/
**Tier:** correction-key

**Headline: Axiom 2's empirical anchor currently has no runnable falsifier: the E-arm is
instantiable on its categorical read (its magnitude criterion is sub-resolution at the
powerless position the kill names, on realistic rating scales) but refutes only (Axiom 2 ∧
no P-channel adaptation); the P-arm is specified and uninstantiated — sign-discriminating
only positive-vs-flat at realistic instruments, so its instantiation spec is a floor-free or
fine-resolution measure. Do not cite either paper's pre-amendment wording as the
falsifiability warrant.**

Arm bookkeeping (below the headline, not in place of it). OQ-232 rev-4 redesign landed
(commit `e9ab87ac`; probe + writeup in `audits/2026-07-23_oq232_falsifier_redesign/`): the
single confounded kill (controlled information-access and position, not preference
adaptation) replaced by two scoped falsifiers, each refuting an explicit conjunction —
E-arm 2×2 Cell 4 (position-matched inside/outside cohorts; candidates: FTC non-compete
window, H-1B portability, plant closures with pre-period surveys); P-arm sign read
(position change at held entrapment; candidates: promotion studies, union-officer
elections, tenure cases; OPEN-instantiation). Probe findings that scope any future
citation: (a) the cancellation band's lower edge is −23.6% of the axiom's own P-slope and
the band is UNBOUNDED ABOVE at 7-point resolution — the bounded scale floors the
inversion, so the sign table's negative row is unreachable under proportional adaptation
(flat = axiom-false ∨ cancellation ∨ dominant-adaptation-floored); (b) the E-arm magnitude
criterion's 1× bound sits below one 7-point scale step at every held position and its
3×-inflated bound crosses resolution at mid-slope positions — the 1×–3× indeterminacy is
live; the criterion is licensed only under an approximately linear reporting channel.
Part B: Axiom 7's falsifier carries the same confound class (PRESENT per pre-registered
criterion); its v8 kill is now conditioned on structural witnesses (consistent negation,
present beneficiary, §5.9) via explicit cross-reference. Old falsifier text survives only
in archived v6.x versions and docs/v8/foundations/ snapshots — historical records,
contained by pointer, not amended. Class tracking: OQ-234 (evidentiary-bridge class;
channel enumeration partially discharged — site indices are exactly {P, T, E, S}, only
{P, E} adaptation-indexable; open question sharpened to index-set completeness).

## 2026-07-23 — [landed] Recall limit + borrowed-example rule (uke_write §5.4, OQ-233); Axiom-2 falsifier confound (OQ-232); perspectival-gaps essay landed (blog/, GAP-04/06 notes)
**Files:** agent/uke_write_v2.2.md, ISSUES.md, blog/2026-05_or_before/diagnostic_value_of_perspectival_gaps.md, docs/design/design_gaps.md
**Tier:** landed

Day's three verified Claude-web critiques, each landed at its right altitude: (1) **OQ-232** —
Axiom 2's falsifier (v8 §9.5 / v6.13.1:88) is confounded by adaptive preference (controls
information-access and position but not preference-adaptation-from-entrapment, a mechanism the
framework itself models); candidate repair is perturbation-shaped (hold position, vary
entrapment) per docs/the_perturbation_principle.md; papers' kill condition not citable as
falsifiability warrant until amended. (2) **Perspectival-gaps essay** (2026-02-11, was
published-never-landed) → blog/; three-feature test → GAP-06 as upstream
diagnostic-vs-erroneous pre-filter (GAP-06's stale "Deferred" corrected — router was built
2026-06-20 via OQ-55); GAP-04 population-layer note, operator-framed as generative not factual
(orbit = computed attitude-range; whether assigned position moves authoring routes to
OQ-73/OQ-228). (3) **OQ-233 + uke_write §5.4** — verification is precision-shaped
(draft-as-query-source; fresh reader inherits the seat from the artifact; recall needs a
different question, not a different reader); borrowed-example rule (query the example in its
home field) landed as protocol with checklist items + §6 "Corpus consulted" declaration
(essay-layer sibling of OQ-230's coverage field); pattern claim pre-registered off n=2 with the
reviewer's kill condition and the egocenter probe recorded before running. §5.4 placement chosen
to keep the OQ-185 uke_write:180 anchor stable (verified).

**OQ-233 probe RUN same day (n=1 datum, operator's separate-corpus seat; load-bearing citation
independently spot-verified here via WebSearch — the one external element per the declared
seam).** Scored precisely: the borrowed-example rule **HIT on location** (counterexample at the
import point, in vision science's home field, as predicted) and **MISSED on substance** (the
pre-registered egocenter-shift hypothesis was half-wrong; the real counterexample is Erkelens &
van Ee, Vision Research 2002, "sometimes inappropriate, always irrelevant" — the cyclopean eye as
theorist's bookkeeping, not brain-synthesized, which would degrade the essay's Type-B isomorphism
to analogy). But home-field-UNSETTLED (Ono/Mapp/Howard counter-camp, 20+ yrs unconverged,
verified), so by the essay's own persistence-criterion the honest move is retype settled→live and
take persistence as support FROM the borrowed field. **Repo exposure corrected:**
`docs/cyclopean-point.md:12` typed binocular-vision facts as `bedrock`; added a marked grounding
correction (geometry stands; the *manufacture* claim the isomorphism rests on is home-field
contested, not bedrock) — annotation, not rewrite, matching the essay's own declaration
discipline. **Fork noted (not annotated):** `agent/analysis/originals/cyclopean-point.md` is a
variant copy (different header/subtitle) carrying the same 7 manufacture/isomorphism references;
left as analysis-input material (originals/ = sources to analyze, operator 2026-07-23), NOT
canonical — the canonical doc cited by `unknown_reading_review.md` + the perturbation principle is
`docs/cyclopean-point.md`. If the originals copy is ever promoted, carry the correction.

## 2026-07-22 — [landed] uke_write v2.1→v2.2 (Forecast Register) + uke_score v0.1 companion rubric + OQ-229 minted (pre-registered fragility-bias hypothesis)
**Files:** agent/uke_write_v2.2.md, agent/uke_score_v0.1.md, agent/analysis.py, quick_start.md, agent/manual/prompts.md, docs/technical/generator_emission_map.md, ISSUES.md, issues/INDEX.md
**Tier:** landed

Operator-relayed review feedback on the planned essay-archive grading pass (scoreability is
authored, not conferred by time; mechanism-ID and magnitude-estimation fail independently;
fragility-bias hypothesis + kill condition) was landed as: (1) **uke_write v2.2** — renamed
from v2.1 (single-file lineage convention; `git mv`), adding §1.6 Scoreable Prediction
Requirement, §6.1 machine-extractable `FORECAST REGISTER v1` YAML block (two-column
mechanism/magnitude pairs, absolute dates, named resolvers, `p_essay`+`p_baseline`,
fragility/stability direction tags), §5.7 Scoreability Gate, F-UNSCOREABLE-PREDICTION;
(2) **`agent/uke_score_v0.1.md`** — the register's consumer, a standard rubric a subsequent
model applies to the register block alone (producer+consumer in one change per Build
Discipline Pattern 1); (3) **OQ-229** — both arms (forward instrument landed-unwitnessed;
retrospective triage-then-grade pass not started) + the pre-registered hypothesis and kill
condition, recorded before any grading run. All 8 live `uke_write_v2.1.md` references updated
(analysis.py:4,80 hardcoded load path; quick_start; manual/prompts; generator_emission_map;
OQ-185's three citations — its `:173` scaffold-row line anchor verified still valid post-edit,
and its `grep suppression_requirement` empty-witness still returns 0). Audit-dir references
left historical. **Protocol change is under model-swap discipline: unwitnessed until the
first v2.2 essay run passes the §5.7 gate on real output** (OQ-229 graduation step).
Checks: issues_status 229/0 malformed, omega check 0 problems, index regenerated fresh.

Same-session extension (still v2.2 — landed unpushed/unwitnessed, so extended rather than
bumped): **§2.4 Tensegrity Architecture for the multi-seat essay** (from the operator's
stereo-pair essay design cycle) — seats as compression struts at measured strength (χ),
declared disparities with kill conditions as the tension net (§1.4 extended to disparities;
tensegrity-without-tension = both-sides filler), no privileged front (author's seat declared
as one strut), hydrostatic local commitment (declared/costed/temporary, emits a Forecast
Register pair when outcome-shaped). §2.3 scoped as the convergent mode with an explicit
mode-choice rule; §1.5.2 gauge-variant bullet routes to §2.4 (same-line edit — the
uke_write:173 scaffold-row anchor re-verified intact); new F-MANUFACTURED-CENTER
anti-pattern (seatless synthesis voice / fake tensegrity). Aligns with OQ-101
(plurality collapses by form) and the verdict-omits-seat finding.

Third same-session extension — **§2.4 absent-strut provenance rule** (consumer-found: the
writing model, applying §2.4 live on Draft 4, hit the gap that a seat nobody authored
renders at a χ magnitude as if measured — beneficiary seats inference-only, `powerful=0,
organized=0` — so the χ spread over present struts presents as a complete stereo picture).
Fix is the Build Discipline spine applied at the essay layer (carry the provenance bit with
the value; h1-null rule analog): per-strut provenance measured/inferred/absent;
flat-without-measured = UNDETERMINED (two indistinguishable causes: genuinely-flat vs
suppressed-below-hearing); absent struts named in-body; eye-selection decision attributed.
Plus F-ABSENT-STRUT anti-pattern + a §6 metadata "Strut provenance" line so the declaration
can't be silently skipped. uke_write:173 anchor re-verified.

Consumer round-trip refinement (same session, writing model's correction accepted): the
attribution clause was a defect as first written — "attribute the eye-selection decision"
invited laundering a method-side absence (the author's own sourcing limits) into a
world-side suppression claim, re-importing the verdict through the provenance rule. Amended
to method-level attribution only; world-side cause = a second undetermined one level up
(suppressed eye and unsourced eye present identically from the author's seat). §9 now also
scopes the rule plainly: representational fix, not epistemic (makes the writer say they
can't tell; doesn't let them tell). NOT added, deliberately: a rule against the
second-order no-seat pose (scrupulous UNDETERMINED-tagging as a reconstructed
view-from-nowhere) — the writing model flagged it as its own likeliest Draft-4 failure and
correctly called it writer-vigilance, not protocol; carried here as a Draft-4 REVIEW
watch-item instead of an over-promoted rule.

Sibling-protocol propagation (same session, operator request): **uke_think v1.1→v1.2** —
new §4.4 Multi-Seat Architecture (tensegrity import: positions as struts at argued
strength, kill-conditioned disparities, writer's position as one declared strut, declared
flattening w/ falsifier + Forecast Register pair when outcome-shaped) + position-provenance
rule (occupied/constructed/absent; silence = UNDETERMINED between no-objection and
never-occupied — §0's counterexample principle made structural) + §8 multi-seat gate +
F-MANUFACTURED-CENTER; **uke_opinion v1.3→v1.4** — tensegrity NOT imported (the verdict
form is the protocol's purpose); instead §1.5 "The Declared Flattening": collapse legit
only when indexed to a named seat where readings are position-indexed, falsifier attached,
consensus-absence ≠ assent (absent seat attributed to citation base, method level), plus a
routing rule (divergence-is-the-finding → UKE_THINK §4.4 / UKE_W §2.4); stray trailing
fences removed. Versions are in-file only (filenames unversioned, no code loads them);
essays' historical UKE_META stamps naming v1.1/v1.3 left as provenance records.

Second consumer-found correction (Draft 4 → Draft 5, operator-relayed): the writing model
put χ values in prose while claiming Mode B — §2.4's "rendered at its measured strength
(χ magnitude)" read as a Mode B exception. Closed in uke_write v2.2: χ calibrates prose
intensity, the number never appears; §5.5 checklist hardened (χ magnitudes named; "§2.4 is
not an exception to Mode B"). Plus three additions from the same failure: the **Rashomon
rule** in §2.4 (+ uke_think §4.4) — integration lives in the READER; seats are inhabitable
accounts, not exhibits; a reconciling fifth voice is the manufactured center returning;
depth-fusion (single recoverable answer) reserved for hydrostatic joints with a named
resolver — the stereo-pair geometry over-promised fusion where no fact-of-the-matter
exists; the **replicate-stability rule** (§1.5.2): run-stability/spreads are evidence about
the authoring (authored corpus + engine determinism), never about the world — the
essay-layer form of the report-scalars-are-not-measurement finding; and
**F-ENGINE-AS-TRUTH** (auditing the reports instead of writing the story the reports
pointed at). Register placement question answered from the existing §6 rule: trim from the
public copy at will, the ARCHIVED essay copy retains the register (single carrier — a
separate scoring file would fork the artifact, Pattern 2).

Third reviewer-found correction (Draft 6, second-Claude review, operator-relayed):
**snapping was ungoverned** — the Rashomon rule gives the writer authorship of every
account, so any account can be scripted to self-destruct and the collapse looks inevitable
rather than sound (witnessed: the grower seat's margin-claim silently swapped for a
refutable floor-claim, concession ventriloquized, under form-pressure to snap something).
uke_write §2.4 snapping rules landed (fired pre-stated kill condition only; engage the
seat's stated claim; holder-signs-whole-account test; zero snaps = complete; like joints
on the same missing resolver share status) + F-VENTRILOQUIZED-CONCESSION; the agency line
(discretion ≠ constraint; symmetric form must not launder operators into weather — the
"no one is responsible" view-from-nowhere via equal treatment); the shared-instrument
convergence rule (plurality as fingerprint of a missing resolver; per-instance attribution
of instrument retirements, no coordinator inferred from the pattern); "the architecture is
scaffolding too" (frame machinery invisible to the reader; excess frames dropped).
uke_think §4.4 mirrored compactly (earned snaps + agency line). Essay-side calls NOT made
here (operator/writer's): monopsony-commit vs hold-open on the grower joint — the
like-joints rule now forces consistency either way — and the measurement-destruction
reframe for Draft 7.

Claude-web ENGINE critique (emotives.md run) verified per agent-inventory discipline —
scorecard 1 confirmed / 1 refuted / 1 sharpened / 1 rerouted: (1) CONFIRMED missing
`golden_rule_consistency_reading` (mandated by the kernel's own decomposition note at
authority_vacuum_incommensurability.pl:262; both siblings dangle cs_reading_relation edges
to it) → **OQ-231** (P2; also records the REFUTED half so the fix isn't executed);
(2) REFUTED "flat control not registered to its kernel": flat_control_of/2 present
(…flat_control.pl:110), 17/17 corpus-wide, and cs_kernel_id-on-flat-controls is excluded by
the 2026-06-05 operator ruling; (3) SHARPENED "no source-provenance field":
`provenance.source_essay` EXISTS and is emitted (generate_constraint_pl.py:856) but reads
`'unspecified'` in all three stories despite the run being invoked with the source file as
argv — no filler, no consumer, no coverage field (source's self-declared skip at
emotives.md:7) → **OQ-230** (P3); (4) REROUTED "showing-face field unauthored": the
declared-vs-concealed instrument exists on the CS axis (cs_drift_ack_witness); topic-run
stories author 0 cs_* facts — OQ-223-class question, recorded in OQ-230 cross-refs, not
re-minted.

## 2026-07-20 — [landed] Kimi-k2.6 twin COMPLETE at n=1005; five-leg cross-model comparison
**Files:** prolog/testsets_kimi/, json_kimi/, prolog/beta_processed_kimi.txt, agent/run_no_scope_kimi.py, python/audits/five_leg_twin_comparison.py, audits/2026-07-20_five_leg_twin_comparison/
**Tier:** landed

The balance-blocked full run (below) was completed after a recharge landed. **Key operational
finding: batch tail latency is batch-SIZE dependent.** A 350-request batch stalled at ~332/350 for
hours (30 reqs stuck at +2/hr, rode toward the 24h window); **335/336-request batches completed
335/335 and 336/336 with NO stall.** So keep kimi batches ≤ ~335. **Cancel returns completed rows**
in the output file (output_file_id populates on cancel) — `--resume-batch <id> --n <same>` harvests
them with no regen; used once to recover 329 from the stalled 350-batch. Path to full n=1005: pilot
5 + harvest 329 + round-1 335 + round-2 336. Actual cost ~$0.043/story batch (round-1: $14.3/335);
balance drew ~$65 total from the ~$150 recharged pool. `testsets_kimi/` is now the FIFTH full leg.

**Five-leg comparison** (`audits/2026-07-20_five_leg_twin_comparison/`, all 5 legs classified at one
HEAD `9c226e8`): (1) **kimi-k2.6 is strikingly homogeneous — 63% of stories in H¹ band-3** (vs
26–34% others), N-invariant (63% at both n=334 and n=1005) — the sharpest single-model signature.
(2) **H¹ obstruction is overwhelmingly model-dependent**: across 957 shared seeds, all-4 twins agree
on h1_band only 14.5% (maxent type 35.1%) — empirical support for seat-indexed verdicts. (3)
**CORRECTION:** the partial-N (334) "kimi is cleanest, 0.3% red" claim was a first-334-seeds artifact
— at full N kimi red% = 2.7%, comparable to sonnet/haiku. Lesson: marginals over a non-random slice
mislead; paired agreement rates were N-stable. sonnet remains the type outlier (only tangled_rope >
snare leg, high piton).

---

## 2026-07-19 — [correction-key] Kimi batch WORKS on kimi-k2.6 (was model-gated, not account-gated); status_code==0 batch-extraction bug fixed; twin retargeted k2.6; 5 pilot landed
**Files:** agent/run_no_scope_kimi.py, prolog/testsets_kimi/, json_kimi/, prolog/beta_processed_kimi.txt, docs/technical/bulk_corpus_generation.md
**Tier:** correction-key

Resuming the Kimi twin after a machine restart (prior instance's `testsets_kimi/` etc. were empty
by design — the 5 pilot stories had been relocated to `testsets/`, 2026-07-18 entry below).
**Corrects two claims from 2026-07-18:**
1. **Batch is NOT account-blocked — it was MODEL-gated.** `POST /v1/batches` returns **200 on
   `kimi-k2.6`**; `kimi-k2.7-code` and `kimi-k3` 404 "resource_not_found". The 2026-07-18 "account-
   level block" tested only the two non-eligible models. Live-verified 2026-07-19 (our pilot batch
   `batch_6a5d1f28…` completed 5/5). `completion_window` must be an h-unit Go duration ("24h"; "1d"
   rejected). Twin **retargeted to `kimi-k2.6` --batch** (DEFAULT_MODEL).
2. **k2.6 is reasoning-HEAVY too**, NOT the "cheaper non-thinking / fairer twin" the k2.7-code note
   assumed. Measured k2.6 batch: **input ≈29.6k / output ≈15.5k tok/story, of which ~11.7k are
   reasoning tokens** (prompt caching fires, ~28.7k cached in). Stays a *thinking-model* twin.

**Bug fixed (was actively billing):** Moonshot's batch output rows carry `response.status_code == 0`
on SUCCESS (not 200), completion in `body`, null row-level `error`. The driver gated on
`status_code == 200`, so it **rejected all 5 valid results and auto-looped into a 2nd batch**
(cancelled before it billed inference). Fixed in `_batch_row_to_result` (gate on payload, not
status_code) — **do not reinstate a `== 200` check.** Added `--resume-batch <id>` (reprocess a
completed batch, no regeneration); used it to recover the already-paid pilot → **5/5 into
`testsets_kimi/`, classify_corpus GREEN on model kimi-k2.6, h1_band populated (2,3,3,3,5)**. Also
`_api_key()` now accepts `KIMI_API_KEY` (the .bashrc export) as well as `MOONSHOT_API_KEY`.

**Full-run attempt BLOCKED on account balance (2026-07-19).** With a spend-go, the full 1000-seed
run was launched. Two mechanical issues found + fixed: (a) Moonshot `/files` hard limit is **100 MB**
and each request inlines the ~139 KB prompt, so a 1000-request jsonl is ~143 MB → 400 "File size is
too large"; fixed by size-chunking `run_batch` into <90 MB batches (`_chunk_lines`; 1000 → 630+370;
commit d92b3cb7). (b) The 630-request batch then **failed on `failed_precondition: user has
insufficient balance`** — Moonshot reserves cost against `max_tokens` (32000), and the reservation
for 630 requests exceeds the account's **available_balance = $51.85** ($50 cash + $1.85 voucher). The
370-request batch cleared the reservation but was cancelled for a clean slate. **The full ~1000-story
k2.6 batch needs a recharge** (reservation ~$82–140 at max_tokens=32000; ACTUAL spend lower since
output ≈15.5k vs reserved 32k). Kimi leg stays at **n=5** until funded. Resume after recharge: one
clean `--batch` run (ladder skips the 5). Balance endpoint: `GET /v1/users/me/balance`. Runbook §7b.

---

## 2026-07-18 — [landed] Kimi (K3) twin driver built + 5-seed pilot PASSED; batch unprovisioned → sync-only; PAUSED pending batch enablement
**Files:** agent/run_no_scope_kimi.py, prolog/testsets_kimi/, json_kimi/, prolog/beta_processed_kimi.txt, docs/technical/bulk_corpus_generation.md
**Tier:** landed

New Moonshot/Kimi twin driver `agent/run_no_scope_kimi.py` (same Anthropic-result-shaped shim as
`run_no_scope_gemini.py`; reuses `build_cached_messages` + `process_batch_results` from the canonical
`agent/generate_kernel_corpus.py`; dest `testsets_kimi/` + `json_kimi/` + `beta_processed_kimi.txt`,
registry scoped to the kimi dir per runbook §6). **Pilot (5 seeds, sync) PASSED:** 5/5 valid `.pl`,
engine-load OK, `reading_relations` resolved, provenance stamped `kimi-k3` (five-defect fix intact),
0 rejections/failures. **Two findings (detail: runbook §7b):** (1) `kimi-k3` is REASONING-ONLY
(`supports_thinking_type:"only"`, effort only `["max"]`) — thinking can't be disabled, so this is a
*thinking-model* twin, asymmetric to the haiku/flash/sonnet twins (output ~16.5k tok/story). (2)
**batch-create is NOT provisioned on the staff/preview key** — file-upload + batch-list work, but a
fully valid `POST /v1/batches` 404s "resource_not_found" (endpoint/duration/file all verified valid),
so the full run is **sync-only at interactive rate, measured $0.289/story** (operator-confirmed
$1.44677/5 pilot), ≈ $291 for the 1005-seed pool vs ~$145 if batch is enabled. **PAUSED at 5 pilot
stories** (operator ruling: enable batch first). RESUME: `python3 -m agent.run_no_scope_kimi --seeds
prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json --batch` once create works (ladder skips
the 5 done), or `--sync` now. Needs `MOONSHOT_API_KEY` in env (never repo). **Key hygiene:** the API
key was pasted in chat — operator should ROTATE it.

**Update (same day):** the 5 kimi-k3 pilot stories were **relocated into the live heterogeneous
`prolog/testsets/`** (+ json into `json/`; operator ruling — testsets/ tolerates mixed models), so
the LIVE corpus is now **150** (was 145; loads clean; local `pipeline_output.json` manifest stale at
145 until a `run_pipeline` folds them in). The twin (`testsets_kimi/`) is **retargeted to
`kimi-k2.7-code`** (the batch-eligible production model; DEFAULT_MODEL changed) and cleared to empty,
ladder reset. **Batch is STILL account-blocked even with k2.7-code** — POST /v1/batches 404s
identically across every model + completion_window, and Moonshot's own OpenAPI documents only
400/401/500 here, so the 404 is out-of-contract = account-level batch access, NOT model/request
(re-verified 2026-07-18). **k2.7-code cost finding:** 2-story sync sample → **output ~8.0k tok/story,
HALF of k3's 16.5k** (input ~29.6k, same) → k2.7-code sync is markedly cheaper than k3 sync; it is
also NOT reasoning-forced (a fairer twin than k3). Provenance sampling_params label corrected to
`reasoning=model_default` (we only set max_tokens). PENDING operator decision: full twin via
**sync-k2.7-code now** vs **wait for batch access**.

---

## 2026-07-18 — [correction-key] commitment-drift terminals are complete only under a SURVIVING-REFERENT precondition (OQ-227, from the *Hearts of Glass* fiction test-rig)
**Files:** prolog/cs_drift_engine.pl, ISSUES.md, docs/design/design_gaps.md, docs/deferential_realism_paper_v8.md, blog/2026-07/no-four-oclock-v8.md
**Tier:** correction-key

The six `cs_terminal_attractor/4` terminals (`stable_pattern`, `husk`, `extinction`, `revival`,
`repudiation`, `axiom_foreclosure`) each **presuppose a surviving referent** (a Temple to be hollow
about; a cosmology to foreclose; a text to depart from) — and all their examples are time-bounded.
Do NOT treat the six as exhaustive: deep-time **referent-dissolution** (the referent a commitment is
*about* decays while its internal form and grounding persist perfectly) is unhandled state-space.
OQ-227 (minted this session, `bundled_with OQ-153`) logs three candidates: **`sealed_closure`**
(commitment axis — warm, complete, "checks against nothing"), **`referent_dissolution`** (a new
`cs_drift_state/3` direction), and **`acknowledgment_collapse`** (standing axis — the ratifying
authority can no longer tell faith from its perfect performance; the terminal form of the
`design_gaps.md` self-consuming-standing trigger). **Load-bearing discriminator = recognizability,
not referent-presence:** `husk`/Kodashim is a *recognized* hollowing (the community knows the Temple
is gone); `sealed_closure` is unrecognizable by any observer incl. the keeper — the commitment-scale
instance of the essay's terminal fox (v8 §5.9). It is undetectable by construction (glues from every
synchronic angle → H¹=0, per the v8 §9.4 synchronic-invariant prohibition added same session; no
pre-closure snapshot), so **authored-only if ever built** — the drift analog of `h1_band=null`, same
shape as OQ-51's ruled-not-yet-built 4th sheaf value. Provenance: fiction as eon-scale test rig
(Claude-web review). Zero live-corpus cases — the additive path stays deferred; the recorded reasoning
is for a reviver. Also landed same session: v8 §9.4 H¹-synchronic citation prohibition; `design_gaps.md`
self-consuming-standing trigger; blog Part IV "The otherwise you can't erase" (temporal/Euclid repair).

---

## 2026-07-17 — [correction-key] blog essay "Everything Becomes Taste" → three-legs = trifurcation; forced-vs-chosen invariant; the synchronic mountain/naturalized read is WRONG
**Files:** docs/deferential_realism_paper_v8.md, docs/design/design_gaps.md, prolog/cs_drift_engine.pl, docs/debugging_philosophy.md
**Tier:** correction-key

Reading of `blog/2026-07/no-four-oclock-v8.md` (adaptive-preference / "is this preference *yours*")
against the engine, corrected mid-session by the operator. **The correction:** a first pass claimed
completed naturalization is indistinguishable from a genuine mountain — true **only synchronically**
(single time-slice), which is exactly the essay's own trap (its three dead probes each fix one leg
and vary within it). The engine's three legs are the paradox trifurcation of
`docs/debugging_philosophy.md`: observer/gauge = Type-C index, temporal/drift (`classify_at_time`,
`snapshot_type`, `drift_trajectory`, `cs_drift_engine`) = Type-A frame, axiom (`cs_axiom_engine`,
`axiom_foreclosure`) = Type-B structure. Naturalization is a **Type-A drift**, seeable by
frame-fixing (hold ε at t0) and tracing — the analytical/sub-specie position does exactly this.
**Operator's sharpening (Euclid):** temporal invariance is necessary-not-sufficient for mountain;
a *held choice* is invariant too (Euclidean geometry read as a mountain for 2000y; the parallel
postulate was a chosen axiom, demoted by a consistent otherwise, NOT by drift). Three-way partition,
each boundary cut by a different leg: **mountain** (forced — no beneficiary/no consistent negation,
honest no-seat pose) vs **declared choice** (Euclid/librarian — seated, otherwise live, acknowledged)
vs **naturalized foreclosure** (fox/Euclid-that-forgot — seated but posed-as-fact, unacknowledged).
mountain/choice is cut by the **beneficiary leg** (`false_natural_law`, a *structural* otherwise —
fires with no authored history at all); choice/foreclosure is cut ONLY by the **acknowledged bit**
(`cs_drift`: rescue-to-`stable_pattern` vs `husk`). Euclid's demotion = Oracle Gap (Theorem 4): a
site lacking the hyperbolic observer; widen the site → H¹>0. **The residual** (essay's real earned
limit) is choice-vs-foreclosure, and it is an **evidence-base limit not a meter limit**: the
acknowledged bit is authored, and the most-foreclosed positions author the least ("record density
tracks power"). Preference↔constraint mapping ruled **analogical only** (operator: "probably a
category error") — informs framing/docs, not asserted as a structural bridge; no H¹ experiment run.
Applied: v8 §5.9 (new), design_gaps.md GAP-01 reframe, cs_drift_engine.pl header cross-ref.

---

## 2026-07-16 — [correction-key] OQ-221 partition run: the merit-independent-signature law is DEPLOYMENT-RELATIVE (F1 — counting fires on earned external prose); OQ-221 mitigated, OQ-226 build queue minted
**Files:** agent/uke_narrative_orchestrator.py, ISSUES.md, audits/2026-07-16_oq221_meter_partition/
**Tier:** correction-key

The OQ-221 two-corpus partition (PREREG `a823cd47` + checkpoint ratification `ad132911` predate
every run; results `a6820230`) falsified the H1 earned-side prediction: `_numeric_inventory`'s
counting condition (≥10.0/1000) fired on **4/12 ratified earned texts** — ordinary earned prose
runs ~10–16/1000 number-words, and operator-approved **rift3 measured 46.04/1000, inside the
recorded defect band** (its vent-logging register IS the craft). Diagnosis witnessed: prediction
error, not instrument error (positive controls passed; fires are real tokens; threshold 10.0 was
variance-calibrated on pipeline output, never human-prose base rates). **Correction to how the
OQ-214 law may be cited: "counting is gateable" is true only relative to the pipeline's own
output distribution (defect band 37.6–50.6 vs improved ≤0.5) — never as a universal craft meter.
A gateable verdict must name its denominator.** Doc restatement for build_discipline.md /
design_discipline.md §11b proposed-and-flagged (operator ratifies), not applied. Partition:
rows 2/7/8 RULED reader-held; 3/4 UNSPECIFIABLE (P3 confirmed); 5 PROPOSED; 6/9/10
PROPOSED-capped (defect n=0 — notably SDZ has NO witnessed genuine misfire, earned 5/7 →
cross-note on OQ-127); 11 BLOCKED-ON-SEAT (OQ-185). Secondary witnessed gap: rev5's
operator-adjudicated earned word-arithmetic is percentage-form and EVADES `_WORD_ARITH_RE`
(recall narrower than the defect-class name) — extension queued in OQ-226 item 2. Floor claim
re-scoped: the defect roster contains only reader-noticed defects (R5 selection check: counting
was noticed before the meter existed).

---

## 2026-07-14 — logic_symbolic.md §IV reconciled to the ENGINE + gate-context drift guard (silent-fork + Stage-5-role fork resolved)
**Files:** agent/narrative_transform/logic_symbolic.md, agent/narrative_transform/logic_narrative_translation.md, agent/uke_narrative_orchestrator.py, python/check_logic_symbolic_drift.py, ISSUES.md
**Tier:** landed

Reconciled the narrative pipeline's constraint-logic references to `prolog/config.pl` +
`drl_core.pl:classify_from_metrics/6` (**the engine wins**, not `docs/logic*` — those are stale too;
see correction-key below). Two forks resolved:

- **Silent fork (Build Discipline Pattern 2).** `logic_symbolic.md §IV` hand-mirrored the gate
  thresholds and had drifted (Snare `χ>0.70` → real `χ≥0.66 ∧ ε≥0.46 ∧ Supp≥0.60`; Tangled
  `0.46≤χ≤0.70` → `0.35<χ≤0.90 ∧ ε≥0.30 ∧ Supp≥0.40`; Scaffold `χ≤0.35/theater≤0.40` →
  `χ≤0.45/theater≤0.70`; Piton fallback rewritten to `χ≤0.45 ∧ ε>0.10 ∧ theater≥0.70`; Naturalized
  `χ<0.40` → `χ<0.35`; **added** the dead-coordination piton pre-check `ε>0.10 ∧ theater≥0.70`; cascade
  relabelled the **metric** cascade, pre-signature-override). Anti-fork mechanism:
  `python/check_logic_symbolic_drift.py` derives its checklist from the `config:param` calls in
  `classify_from_metrics/6`, reads values from `config.pl`, asserts each on its §IV gate line
  (value-in-context; catches right-number-wrong-gate). GREEN (15 params) → RED on a wrong-gate swap
  (scaffold 0.45↔snare 0.66, presence-grep stays green) → GREEN reverted. **NOT wired into a pipeline
  gate** (operator say-so required). Deferral tracked: **OQ-222** (guard now, load-time injection on a
  churn reopen trigger). Guard scope stated: structural predicate gates + the hardcoded scaffold
  theater `TR>0.70` literal are numeric-unguardable by design (RECURSE-IF-REFACTORED note in-file).

- **Stage-5-role fork.** The doc header claimed §IV serves "verification (Stage 5)" and the
  orchestrator comment `uke_narrative_orchestrator.py:1471` said "stages 0, 1, 5" — both wrong.
  `STAGE_INPUTS["narrative"]` wires `dr_logic_symbolic` to **stage_0 (classification) + stage_1
  (formalization) only**; stage_5 is a narrative-critique Discovery pass (`["stage_4",
  "constraint_reports"]`) with no logic ref. Both corrected. Assembled-prompt probe confirms the
  corrected §IV reaches the stage-0 prompt in gate-line context.

- **Part B (committer/CS axis):** confirmed absent from the narrative pipeline (observer-axis only);
  recorded as **OQ-223**, held pending corpus graduation (committer dimension is a standing null vs 319
  omegas per `commitment_systems_sketch_v5_1.md`; only the has-beneficiaries bit graduated, already fed
  via `d`). No code.

## 2026-07-14 — [correction-key] docs/logic.md diverges from config.pl (OQ-37 doc-lag); logic_thresholds.md does NOT → OQ-224
**Files:** docs/logic.md
**Tier:** correction-key

Surfaced while reconciling `logic_symbolic.md` (OQ-222); do NOT reconcile the narrative docs *to*
these — the engine is the source of truth. **Witnessed (grep, not the plan's restatement — the plan
overstated):** the divergences are all in `docs/logic.md`, from OQ-37 Move 1 (2026-06-01,
`tangled_rope_chi_floor` 0.40→0.35) being only partially propagated: `:1695` param dump still `0.40`;
Naturalized `χ<0.40` (`:2077`/`:2083`, code `<0.35`); a quick-ref table `:2565` says piton `χ≤0.25`
while `logic.md`'s OWN prose (`:1966`/`:2012`/`:1995`) correctly says `0.45` (internally inconsistent).
**`docs/logic_thresholds.md` does NOT diverge** for these params (its table `:197` is correct with the
OQ-37 note); **no "Scaffold χ≤0.30" exists** in `logic.md` (its scaffold dump `:1893` is correct at
0.45). Not edited here (separate canonical surface). **Now ticketed: OQ-224** (bundled_with OQ-222).

## 2026-07-13 — OQ-214 Phase A LANDED: `_theme_inventory` theme-naming meter (mitigated; Phase B spend-gated)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stage7.md, agent/narrative_transform/stage8.md, agent/narrative_transform/stage10.md, agent/uke_narrative_architecture_v0_3.md, python/tests/test_theme_inventory.py, audits/2026-07-13_oq214_theme_meter/, ISSUES.md
**Tier:** landed

`_theme_inventory(text)` built on the `_numeric_inventory` (OQ-215) template — the last waivable
absence-claim in the editorial audit layer (theme-naming) gets a deterministic backstop. Six kinds
split by the **bucket rule** (density-bearing ⟺ flagging it in rift3.md is NOT a false positive):
**density-bearing** = `anaphora` (≥2 consecutive sentences sharing a ≥3-word initial phrase),
`causal_chain` (stacked/near because-therefore formulas); **adjudication-only** = `refrain`,
`aphorism`, `resonant_closer`, `word_arithmetic`. `density_per_1000` uses the two density-bearing
kinds ONLY; the full list is injected into stages 7/8; the post-stage-8 gate escalates OPEN, **never
auto-rejects** (protected INVARIANT + kill condition written verbatim in-source; refrain-doesn't-gate
locked by `python/tests/test_theme_inventory.py`).

**The finding is a NULL, and the null IS the deliverable (operator ruling — see WRITEUP.md).** The
density-bearing kinds do NOT separate the OQ-218 defect from its v0.2 fix. Runs 1&2 SEED vs IMPROVED
have IDENTICAL anaphora/causal counts (18/10, 14/5) — the improvement lived entirely in refrain
(40→20), the kind the ruling forbids gating on. Earned-dense rift3 = 5.12 outscores two of three
SEED defects (3.64, 3.31). The gateable axis is orthogonal to the defect. `THEME_DENSITY_THRESHOLD =
8.0` sits above everything good and will essentially never fire — correct for a gate that cannot
discriminate and would damage craft if it tried.

**GENERAL LAW (correction-key, learned twice): a defect is mechanically gateable IFF it has a
merit-INDEPENDENT signature.** Counting had one (a digit means the same in a defect and a
masterpiece) → the meter worked and could gate. Theme-naming does NOT (its surface IS the surface
earned prose uses on purpose) → the meter cannot gate and collapses to a candidate list under
adjudication. Not a difference of meter quality. **The bucket rule turned a false success into a
true null:** had refrain stayed on the gate, calibration would have LOOKED successful (defect-high,
fix-low) while suppressing rift3's creed and every earned refrain. **Honest close: the last waivable
absence-claim is now a mechanical candidate list, but explanation-over-run is NOT metered and cannot
be by this approach — the register problem stays reader-held (the Q2 double-No), unreachable by
regex. The meter's existence does not move it.**

**Two design claims RULED (operator, 2026-07-13):** (1) **Mechanization floor — provisionally yes,
with falsifier:** *no remaining KNOWN defect has a merit-independent signature; the falsifier is
finding one that does* — if it surfaces the meter approach revives for it and the architecture
extends (same shape as every threshold ruling: provisional, reopens). (2) **Assisted posture —
PERMANENT architecture, by the structure of the problem** (not a maturity stage): the mechanization
boundary runs along merit-correlation; above it is instrumented (counting/invariant/break-address,
R1–R14), below it is definitionally reader-held (arm-2 D9 model-judge rubber-stamped the negative
control, only the reader discriminated; the Q2 double-No). So "improve the pipeline" now means
improve the READERS in the loop, not build more meters — the cold human read is the first instance
of the actual remaining work, not a gate before resuming it. Repo-wide test of the law = **OQ-221**
(does the meterable-vs-reader-held partition apply beyond this defect).

**Tripwire (promotion candidate → judged history):** a future optimizer must NOT arm the theme gate
on refrain/aphorism/closer/word_arithmetic "for determinism" — that turns the meter into a
craft-suppressor (the hard-ban failure in a third costume). This is enforced in-substrate three ways
(in-source invariant comment, `THEME_CAVEAT` rendered at every read site, the regression test), so it
is a LOUD failure, not a silent one → stays history, not promoted to CLAUDE.md.

**Graduation — human read FIRST, may MOOT Phase B:** the cold human read answers whether the
register problem is even the right target (if the improved stories read machine-made at the sentence
level, the meter treated a symptom). Do the read; THEN decide — one Phase-B run (engine-change
witness; low-information now the gate barely fires) OR close OQ-214 as "candidate list shipped;
explanation-over-run confirmed un-meterable; register work reassigned to the read layer." No tokens
spent yet.

## 2026-07-13 — OQ-219 RESOLVED: Stage-2 dominance clause implemented + validated (routing outcome a); no v0.3
**Files:** ISSUES.md, agent/narrative_transform/stage0.md, agent/uke_narrative_orchestrator.py, python/tests/test_stage2_dominance_gate.py, agent/uke_story_v0.2.md, audits/2026-07-13_oq219_missing_floor/READOUT_dominance_clause.md, audits/2026-07-13_oq219_missing_floor/READOUT_datum_stone.md
**Tier:** landed

**Clause implemented + validated (commits `83ecf045` impl+fixture, + clause run/READOUT).** stage0.md
authors `primary="yes/no"` on the invariant contract (at most one invariant primary; never inferred
downstream). Orchestrator `_contract_marks_floor_primary` / `_stage2_dominance_suffix` inject the
dominance-ordering clause into the Stage-2 prompt IFF `missing_floor present="yes" primary="yes"` —
STRUCTURAL gate (R3(b) third application), behavior-preserving on all committed contracts (none carry
`primary=` → INERT → identical prompt; output-changing only on newly-flagged floor-primary sources).
Free negative-control fixture `python/tests/test_stage2_dominance_gate.py` 5/5 PASS (over-fire on
grain-primary structurally impossible — the hard-ban mistake relocated, guarded by a fixture not a
run). Paired re-run (control = no-clause Datum Stone `a02246f7`) met all pre-registered criteria:
subordination beat nameable, cold recovery 3/3 (Sonnet+Gemini+Haiku, same arms held constant) ≥ 2.5/3
baseline, **Haiku rescued partial→full floor** (predicted sensitive indicator), grain preserved (kill
condition unmet). Confound noted: clause run = different stochastic surface (determinism frontier
forbids same-story on/off); clean signals = subordination beat + Haiku rescue + grain-preservation +
fixture. NOTE the OQ-216 intermittent stage-2 SECTION-0 guard fired on the first clause draw (also
witnessed pre-clause) and cleared on re-draw — not clause-induced. Below is the mitigation-stage record.

**Plan-file "wobbly-torvalds" typed VOID (not a deferred task).** The plan filename
`review-oq-219-and-present-wobbly-torvalds.md` carries no second task — its 21KB body is OQ-219
only. `wobbly-<name>` is the operator's plan-slug naming convention (cf. sibling
`~/.claude/plans/can-you-review-oq-91-wobbly-cray.md`), not a "present X" deliverable. The plan is
fully discharged by this resolution; no open item remains from it.

## 2026-07-13 — OQ-219 (mitigation record): floor-recovery tracks dominance (routing outcome a)
**Files:** ISSUES.md, agent/narrative_transform/stage2.md, audits/2026-07-13_oq219_missing_floor/READOUT_datum_stone.md, audits/2026-07-13_oq219_missing_floor/READOUT_v02repair.md
**Tier:** landed

Commits: pilot `86c36f29`/`30120d32`, routing pre-commit `354ef198`, resolution (this). Reframed
OQ-219 (v0.2-repair: can v0.2 dramatize a contract-only floor?) resolved via TWO runs. **Pilot**
(Margins, grain-primary, contract-only floor): v0.2 CAN dramatize the floor (existence) but FRAGILE
— §6 cold arms split 1/2 (Gemini floor, Sonnet grain). Cause unassignable at n=1. **Isolating run**
(Datum Stone, floor-PRIMARY, ~$1.65, "The Long Breath"): cold floor-recovery ≈2.5/3, and **Sonnet
FLIPPED** grain→floor with dominance. **Floor-recovery TRACKS DOMINANCE** → the fragility is
**dual-grain competition, NOT a missing protocol socket** → pre-committed **routing outcome (a): NO
v0.3.** Fix = **seed-side Stage-2 dominance-ordering clause** (when the contract marks the floor
primary, Stage 2 subordinates the grain on-screen — the §1a two-reals machinery applied to
floor-vs-grain); implementation-pending, gated on operator go for a generation-protocol change.
**Standing taxonomy ruling (operator):** the floor is the grain's **structural sibling, not a
break-species** (presupposition vs unreadability; fairer-authority vs better-instrument falsifier;
instrument's-honest-operation vs character's-knowing carrier) — the §1b break-rider was the correct
vehicle, only the §1 structural home is debated, and outcome (a) says it's not even missing.
**Label-delta method (reusable):** record primed (stage-9, contract-threaded) vs cold (§6 blind)
floor-recovery; cold recovery is presuppositional so it runs BELOW grain-recovery and is
measured-and-reported, not required, in the load-bearing bar. Not a tripwire (loud; the reframe +
fix live in the OQ-219 entry).

## 2026-07-13 — OQ-219 REFRAMED: UKE originals corpus is architected dual-grain — no corpus-drawn pure-Detector-B source
**Files:** ISSUES.md, audits/2026-07-13_oq219_missing_floor/PROPOSAL.md, audits/2026-07-13_oq219_missing_floor/TRIAGE.md, agent/narrative_transform/originals/
**Tier:** landed

Commits `90631e4e` (pre-registration) + `6ff9f480` (Step-0 triage + raw artifacts). OQ-219 asked
whether R14's floor-contract (Detector-B "missing floor") is LOAD-BEARING in output, needing a
**pure-Detector-B leg B** as the naming probe's POSITIVE CONTROL (without it a dual-grain leg-A
null is uninterpretable, sharpening #2). **Finding: no corpus-drawn pure-B source exists.** Prose
pre-screen across 11 `originals/` sources + Stage-0 `--dry-run` engine certs on the two best shots
(`rift3` "Insufficient", `rift2` "Load-Bearing") ALL certify `untranslatable_real present="yes"`
alongside `missing_floor present="yes"`. **The UKE originals corpus is architected dual-grain by
construction** — every source authored for the detector schema pairs a Detector-B codifying
instrument with a live Detector-A untranslatable-real (the datum/table/book/commentaries/baseline
vs the walking/boat-grammar/live-judgment/palm-to-stone), and the untranslatable-real is typically
dominant. This is almost certainly WHY every OQ-215 arm-3 source led with a Detector-A grain.
**Operator ruled (this session): corpus-drawn only, no authoring → reframe.** R14's floor is a
**co-presence grain** here (never authored standalone-primary); the graded existence-run is retired
and OQ-219 is reframed to its own alternative — the **v0.2-repair** question (can UKE_STORY v0.2
dramatize a contract-only floor?), still spend-gated. Triage spend ~$0.06 (2 Stage-0 dry-runs,
gemini-2.5-pro, exit 0). Not a tripwire (loud: any future pure-B hunt re-runs the same triage);
the reframe lives in the OQ-219 entry.

## 2026-07-13 — OQ-217 RESOLVED: consensus_provenance/2 real-seat verdicts; verdict⟺H¹ now an EXACT biconditional
**Files:** prolog/stakeholder_seats.pl, prolog/commentary_census.pl, prolog/tests/test_h1_stakeholder_spectrum.pl, prolog/tests/test_seat_totality.pl, python/audits/oq207_stakeholder_h1_census.py, python/audits/oq217_movement_diff.py
**Tier:** landed

Commits `871e69ac` (tightening, output-changing) + `cb60bd0a` (movement census). Operator ruled
option 3 (filter `unknown` from verdict computation everywhere + annotated unanimity; rationale
at the clause header: unanimity is universal — untypeable seats weaken it, so the caveat rides in
the TOKEN; plurality is existential — unknowns can't undermine it) + the
`manufactured_consensus_candidate_untypeable/1` extension (ruled in session, distinct from the
2026-07-11 D4 ruling). Verdict set grew by three tokens: `insufficient_real_seats` (<2 real-typed
seats — absorbs retired divergence cells (a)/(b) and the 1-real mixed cell),
`unanimous_with_untypeable_seats`, `manufactured_consensus_candidate_untypeable/1`.
`consensus_bucket/2` gained the three rows (insufficient = MEASURED bucket, not absence —
declared choice, revisable). Witnesses: plunit 37/37 + 19/19 + 20/20; census PASS on 4 legs
(retired cells (a)/(b)/mixed = 0 via KEPT-LIVE detectors; all controls fire); per-id movement
diff == the pre-derived prediction, 0 mismatches (4 cell-(b) + 1 cell-(a) stories →
insufficiency; full named table `audits/2026-07-12_oq217_consensus_tightening/README.md`);
pipeline per_constraint byte-identical (same-session clean-vs-edited runs, both exit 0).

Two correction-grade riders: (1) `mcc_untypeable` is HEAVILY LIVE (12/50/39 across
testsets/haiku/flash), NOT the predicted-zero cell — any OQ-204-era consumer reading only the
bare `manufactured_consensus_candidate` token silently drops the larger untypeable stratum
(constraint recorded in OQ-204). (2) The v1 movement prediction was under-determined because the
pre-OQ-217 census dump AUTHORED `n_excluded: 0` for non-mcc verdicts instead of measuring it
(Pattern 5, caught by the diff comparator on real data — `movement_diff.v1_flagged.json`); the
dump now measures `n_excluded` per record by direct fact query.

---

## 2026-07-13 — OQ-218 RESOLVED (operator ruling): rev6 = variance not class; R3(b) STANDING; watch closed w/ travelling reopen
**Files:** ISSUES.md, audits/2026-07-12_oq218_scored_snare/READOUT_STAGE2.md, agent/uke_story_v0.2.md, docs/the_taught_hole.md
**Tier:** landed

Ruling filed VERBATIM in OQ-218 (D9 precedent) + mirrored in the READOUT appendix. Stage 1:
repair confirmed both legs; v0.2 Ω_E1 resolved (possibility, not guarantee) — propagated into
the protocol file. Class: variance not class (1 weak in 8, ending-saved), scoped to
certified-grain sources; R3(b) conditional → STANDING (hard-ban fallback documented, not armed);
reopen condition travels (one Type-B seed post-re-baseline = re-arm; two in five = reopen).
Type-A over-run formally = OQ-214's mandate. NOT ruled: v0.2 §5 untested (Ω_E2 open); register
ceiling; human gold arm gates PUBLICATION (Clean Small Song first) not the close; improved
stories are audit artifacts (rev6-improved carries the Cliffside 806-812 flag). OQ-220
pre-registration hardened before spend: cold-recovery AND delta as separate measurements
(closes the absent-pole alias) + one 4.5-pinned rerun as the model-confound disambiguator.
Taught-hole doc corrected: transmission-through-the-protocol, not resonance.

## 2026-07-13 — OQ-218 sweep witness + OQ-214 promoted + stage-2 SECTION-0 guard (first Sonnet-5 production run fired OQ-216's candidate)
**Files:** ISSUES.md, agent/uke_narrative_orchestrator.py, audits/2026-07-12_oq218_scored_snare/READOUT_STAGE2.md
**Tier:** landed

- **Absence claim discharged:** correctable-vocabulary sweep over the three Stage-2 seeds with
  rev6 as positive control (fired: 16 hits, the rebuild subplot recovered exactly); seeds
  0 / 1-homograph / 0. READOUT addendum carries the paste. Operator per-source leg staged as
  `OPERATOR_LEG_BUNDLE.md` (six files, one upload).
- **OQ-214 PROMOTED (Priority 4→2)** with the witnessed calibration corpus: 3/3 Type-A seed
  instances (manifest anchors), false-positive class (rev5 + run-1 earned lines), Sonnet-r3's
  blind pattern enumeration, and the resonant-closer tic (operator Web-Claude read of the
  ergodicity story: units landing on "the way X" images, ×4 in 2,280 words). Residue-inversion
  design warning recorded: Q2 nominates, register gate vetoes, neither rules alone.
- **First Sonnet-5-default production run** (`112_ergodocity_kids_1783916200`, operator-invoked):
  ran end-to-end (migration held; density point 4.84/1000), but **stage 2 omitted SECTION 0**
  (folded into SECTION 1 "Step 0") — OQ-216's stage-2 census candidate fired in production; R13
  threading ran dead behind a warning. Guard built (warn → fail-loud StepResult error), two-sided
  witness: fires on the ergodocity stage_2, passes on all three batch stage_2s. OQ-216 updated.
- Standing instrument rules recorded on OQ-218: blind payloads get a "these may be identical —
  say so" escape; arm factual claims get grep-adjudicated before filing. Ruling-scope line
  recorded (certified-grain sources; watch reopens lightly at the Sonnet-5 re-baseline).
  OQ-219 remains gated on its own spend-go.
- **OQ-220 minted (label-delta blind read; math_stories stress-test class, 5 sources, spend
  held):** the operator's Web-Claude read of the ergodicity story REVERSED in a differentiated
  way when handed the word "ergodicity" (retraction/sharpening/intact) — reader-seat-indexing at
  the vocabulary level. Design: cold read then concept-primed re-read; the delta measures how
  much of a formal concept survives naturalization without its label. The sharpened register
  finding (an ergodic prose voice metabolizing a non-ergodic subject back into pattern) recorded
  on OQ-214 — the resonant-closer class can be subject-interfering, not merely mannered.
  math_stories Zone.Identifier sidecars removed (same artifact class as the agent/ pair).
- **OQ-220 pilot artifact 2:** operator had Claude web run UKE_STORY v0.2 on the ergodicity
  story WITH the label → `blog/2026-07/the_clean_small_song.md` (committed). It exercised the
  protocol's reject-and-regenerate branch (first time anywhere) and re-translated the concept
  one layer deeper: the ergodic hypothesis + non-stationary collapse (fast convergence as danger
  signal; absence satisfying the measurement) — Build Discipline Patterns 5/6 as fiction, with
  the taught "hole" as carry-the-provenance-bit. Recorded on OQ-220 with honest costs
  (single-voice; closer tic persists at the final image).
- **OQ-220 pilot datum 3 (Perplexity, two-stage on the rewrite): both poles of the label-delta
  scale witnessed.** Cold read recovered the concept structurally without the word ("convergence
  condition indistinguishable from impoverishment"; "loss function has silently changed");
  label produced sharpening-only (no retraction) — vs the seed pilot's reorganization. Delta
  magnitude tracked dramatization as pre-registered. Confound stated (different readers,
  different stories; both operator-adjacent). Convergent editorial datum: the unprimed reader
  independently requested the error-vs-incommensurability sharpening = §1a/F-CORRECTABLE-REAL
  from outside the protocol's vocabulary.
- **`docs/the_taught_hole.md` written (operator request):** the story read back as method for a
  general audience — coming-to-true = convergence-is-not-adequacy (Pattern 5/6), any-teller-
  any-route = consensus-needs-positive-controls, three reefs = seat-indexed verdicts, Lo = the
  fresh instance (KNOWN_STATE/tripwire rationale), the taught hole = carry-the-provenance-bit
  (nullable h1_band as "a held rest with a schema"). Standalone; v8-note-or-standalone left as
  the operator's editorial call, stated in the doc.

## 2026-07-12 — OQ-218 Stage 2 batch RUN (3 sources, 4.5-pinned): rev6 weakness 0/3; residual defect is Type-A register-level
**Files:** ISSUES.md, audits/2026-07-12_oq218_scored_snare/READOUT_STAGE2.md, agent/uke_narrative_orchestrator.py
**Tier:** landed

Stage-1 ruling (repair confirmed, both legs) opened the gate; batch ran same day, all Anthropic
stages pinned to `claude-sonnet-4-5-20250929` per operator instruction (R12 confound dissolved).
Full detail: `READOUT_STAGE2.md` + per-run manifests/arms in the audit dir. Headlines: (1) the
rev6 Type-B scored-Snare weakness appeared on **0/3** fresh sources — all seeds HOLDS on
structural grounds mid-story; maps to pre-registered outcome 3, ruling pending (operator's
seat); (2) all three v0.2 triages = **Type A explanation over-run** — the pipeline's residual
template signature is register-level (blind Sonnet r3 enumerated the house patterns), the
OQ-214 meter's target class, now witnessed 3/3; (3) both addressed deformations named blind by
both arms; Q4 re-confirmed as THE discrimination instrument; (4) subtraction-only deltas sit
below Gemini's detection floor — it confabulated a difference (grep-witnessed false claim),
so near-identical A/B pairs need an "identical? say so" escape or strong-arm-only Q4;
(5) residue-inversion: cold readers Q2-pick F39 framework residue as inimitable — Q2 alone
must never adjudicate keeps; (6) one OQ-216 cap-hit fire (stage_3 12288, run-2 attempt 1) —
guard fail-loud worked, cap → 16384 (`25b27343`), loud retry clean.

## 2026-07-12 — Sonnet 4.5 → Sonnet 5 across the three agent entry points; sampling params gated per model
**Files:** agent/llm_call.py, agent/c-orchestrator.py, agent/generate_kernel_corpus.py, agent/uke_narrative_orchestrator.py
**Tier:** landed

Operator request. `claude-sonnet-4-5-20250929` → `claude-sonnet-5` (exact ID, no date suffix) in
c-orchestrator architect, generate_kernel_corpus SCOPE_MODEL, and all ten uke stage models
(Haiku researcher/GEN_MODEL and Gemini stage-0 unchanged). Sonnet 5 rejects non-default
`temperature` (400) and runs ADAPTIVE thinking when the field is omitted (would spend the
calibrated per-stage max_tokens caps on thinking) — new `llm_call.sampling_overrides(model,
temperature)` gates both per model (Sonnet 5: drop temperature + pin `thinking: disabled`;
Opus 4.7+/Fable: drop temperature; legacy: unchanged), consumed by llm_call.call, the
generate_kernel_corpus single-call AND batch-wave paths, and duplicated locally in the uke
AnthropicProvider (self-contained module). `MODEL_CONTEXT_WINDOW` gains claude-sonnet-5 = 1M
(old entries kept — still served). Witness: py_compile clean; live `OK` round-trip on all three
call paths with claude-sonnet-5 at non-default temperature (no 400). OPEN/[TUNE]: Sonnet 5's
new tokenizer runs ~30% more tokens for the same text — per-stage max_tokens caps and the
0.48–47.6 density baselines were calibrated on Sonnet 4.5 output; the cap-hit guard fails loud
if a cap binds, and the next pipeline run is the re-baseline.

## 2026-07-12 — OQ-215 CLOSED on operator read; posture ruled assisted-by-design; OQ-218/OQ-219 minted (spend HELD); probe sources staged + Stage-0 certified
**Files:** ISSUES.md, agent/uke_narrative_orchestrator.py, agent/uke_narrative_architecture_v0_3.md, agent/uke_story_v0.1.md, agent/uke_story_v0.2.md, docs/design/design_discipline.md, agent/narrative_transform/originals/the_good_name_book.md, agent/narrative_transform/originals/the_eighth_commentary.md, agent/narrative_transform/originals/the_table_of_winters.md, agent/narrative_transform/originals/the_datum_stone.md
**Tier:** landed

- **OQ-215 resolved** (commit `0e353a24`): no kill fired; counting dissolved with R2 live;
  invariant 4/5 strong + rev6 partial-via-ending; flinch withdrawn; D9-adversarial conforming.
  **Run↔rev correction:** arm-3 runs are rev3–rev7 (rev6 = "The Platform Knows" = run 4,
  `the_empty_pan_1783872143`); rev2 is arm 1. **rev6 stage-9 HOLDS hand-checked GENUINE** —
  the correctable-reading pressure (Kiran's representative-sample rebuild) is foreclosed by the
  ending (Sokol's compliance reframe; "changes nothing"). Compressed entry keeps the operative
  rulings (D9 composed fixes + HOLDS-guard; Forty-Hertz partial; rift3 class; R3(b) trail).
  OQ-214 gained the rev5 EARNED word-arithmetic calibration datum.
- **Carried flag wired** (commit `e96b2bf3`): `DENSITY_CAVEAT` ("density measures counting only;
  invariant survival is adjudicated by blind stage-9 + operator read; 0.0 is not evidence the
  invariant held") now renders in the sidecar JSON, the inventory prompt block, and a new
  always-emitted numeric_gate summary line (pass path was silent before). Also in the
  architecture doc, which previously had no density-gate section at all.
- **Posture ruled assisted-by-design** (commit `9d08165f`): design_discipline.md §11 — the
  sharpening judgment is operator-held BY NECESSITY (arm-2 witness: stage-10 D9 scored the
  negative control 5/5); autonomous is structurally foreclosed, not deferred. `--edit FILE`
  documented as the first-class assisted mode. **Protocol files renamed** `uke_resleeve_v0.*.md`
  → `agent/uke_story_v0.1.md` (superseded draft) / `agent/uke_story_v0.2.md` (current): the
  pipeline is the TRANSLATION instrument, UKE_STORY is the IMPROVEMENT protocol — no translation
  protocol file exists; v0.2 footer + v0.1 Ω-NAME note corrected; Zone.Identifier sidecars deleted.
- **OQ-218/OQ-219 minted, spend HELD** (commit `b59ec941`): scored-Snare reframed to "can
  UKE_STORY v0.2 repair a Type-B seed?" (rev6 = the gating Ω_E1 control; fresh sources are a
  conditional Stage 2; executor separation binding — improver ≠ blind reader); missing-floor
  probe with "load-bearing" pre-registered. Both `blocked_on_human operator-spend-go`.
- **OQ-218 Stage 1 RUN (spend-go granted same day; operator concurred with the Phase-0
  hand-check at closer range).** Pre-registration committed BEFORE the pass
  (`audits/2026-07-12_oq218_scored_snare/PROPOSAL.md`, commit `b2c2c542`). Improvement executed
  by this instance under v0.2 Path B: platform-record seat (4 interstitials; the §III fail-count
  moved into the instrument's own grammar — the stage-9 weakness repaired with the seed's device),
  grain scene drafted in isolation (sensor crossing: the trace records the preparation, cannot
  record that it was preparation), falsifier granted with indifference (notebook: "reads her
  exactly as deep as the cruel one did"), stage-10 flinch line cut, consolation level 1 (terminal
  beat = intake record scheduling the daughter). **Paused pre-§6 per executor separation** —
  manifest carries blind_read PENDING; unlabeled randomized A/B payload staged for two fresh
  arms of different model families (+ human gold arm; Q3 delayed; Q4 added). Adjudication:
  blind arms test the break; operator takes the contaminated §1a audit; operator rules.
  Also landed: `[EDGE]` convention added to CLAUDE.md (operator instruction).
- **Four probe sources authored + certified** (commit `434ec74d`, ~$0.12 total dry-run spend):
  three scored-Snare in distinct instrument classes (credit standing / examination / actuarial
  table) + one Detector-B-primary. All four Stage-0 dry-runs witnessed: `inherent_instrument
  value="yes"` (the snares), `missing_floor present="yes"` (datum stone, primary),
  `untranslatable_real present="yes"` on ALL FOUR, break contracts authored. **Full probe runs
  did not run** — held for operator spend-go, against the seed→UKE_STORY chain.

## 2026-07-12 — Break-contract threading landed: stage 0 authors the break's ADDRESS; carried to stages 2/9/10 (rides R13/R14 plumbing)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stage0.md, agent/narrative_transform/stage2.md, agent/narrative_transform/stage9.md, agent/narrative_transform/stage10.md, python/tests/test_break_contract_threading.py
**Tier:** landed

Posture ruling (operator, 2026-07-12): the pipeline AUTHORS the break's address;
EXECUTION belongs to the story stages downstream. No break-execution instructions in
stage4; no auto-reject on break-absence anywhere.

- **stage0.md B6a:** source-sighted `<break_contract>` — `original_break` (expectation
  the source violated for its first readers), `prior_status` (LIVE/DEAD), `target_prior`
  (if DEAD, nearest living expectation the resleeve must violate; if LIVE, restated in
  current terms). All surface-free; same phrasing test as the invariant contract.
- **Orchestrator:** `_extract_stage0_break_contract` + `_STAGE0_BREAK_RE`; anonymized
  via `_anonymize_stage_1` (air-gap backstop) and saved as sidecar `break_contract`;
  cache-restore recomputes from a cached stage_0 when the sidecar predates threading.
  STAGE_INPUTS: stage_9 blind assert is NOW
  `["stage_8","invariant_contract","break_contract"]` (supersedes the two-element list
  cited in the 2026-07-11 R1–R14 entry); stage_10 gains the key; stage_2 receives it
  via `_run_stage_2(…, stage0_break)`. Generic runner has a NOT AVAILABLE fallback
  (break presence UNVERIFIED, never N/A). `_S9_FALSIFIER_RE` lookahead now includes
  BREAK — without it the new stage-9 BREAK section is swallowed into the D9 payload.
- **stage2.md:** one affordance-gate line — reject a naturalization whose substrate
  FORECLOSES the target_prior violation (world leaves the break executable; need not
  execute it). **stage9.md:** BREAK NAMING as a reader (name from text FIRST, then
  compare to target_prior); non-naming is a FINDING, not a failure. **stage10.md:**
  D10 Break Presence — informational only: reported, never summed, exempt from every
  override including the =1 rule.
- **Witnesses:** `python/tests/test_break_contract_threading.py` 5/5 PASS (extraction
  ± negative control; anonymization surface-free with two-sided map-driven control;
  STAGE_INPUTS threading; falsifier-stops-before-BREAK). Dry-run Stage 0 on
  the-empty-pan.md: `<break_contract>` authored, prior_status ruled DEAD, zero
  source-vocabulary hits in the block (probe positive control: same sweep hits
  Verrel/King/seal in the source-sighted remainder of the output).

## 2026-07-12 — OQ-207 RESOLVED: stakeholder-frame H¹ built, emitted, censused; D4 kill condition FIRED → OQ-217 minted; TWO ABSENCE TOKENS tripwire
**Files:** prolog/stakeholder_seats.pl, prolog/reading_registry.pl, prolog/tests/test_h1_stakeholder_spectrum.pl, prolog/json_report.pl, python/shared/schemas.py, python/audits/oq207_stakeholder_h1_census.py, docs/h1_gap_spectrum_general_n.md
**Tier:** tripwire

**TRIPWIRE — two absence tokens coexist BY DESIGN in the per-seat type surfaces; never
unify them.** `untyped` is CENSUS-FACING (`seat_perceived_vs_real/4`'s Computed when the
per-seat derivation fails); `unknown` is KERNEL-FACING (`stakeholder_type_vector/2`'s
token for the same failure, and the literal `dr_type_with_d/4` fallback type). The
kernel's OQ-51 filter `is_real_type/1` tests `\== unknown` ONLY — an `untyped` leaking
into the H¹ vector is counted as a REAL DISAGREEING TYPE and silently inflates
`h1_stakeholder`. Conversely a reader "normalizing" the vector's `unknown` to `untyped`
breaks the null rule. Positive control on the actual failure path:
`test_h1_stakeholder_spectrum.pl` `no_untyped_in_vector`.

Landed (commits `8048a568`, `96047f19`, `cbd44d19` + docs): `stakeholder_obstruction/5`
(memoized; cache_registry hook; coverage in-band; domain = `stakeholder_agent_seats/2`
extracted from `consensus_provenance/2` — no fork), three OQ-137 registrations,
per-constraint `h1_stakeholder`/`_n_seats`/`_n_real` (null = UNDETERMINED, never 0 —
same OQ-51 read rule as `h1_band`), schemas.py contract + consistency check, census
`audits/2026-07-12_oq207_stakeholder_h1/`: 0 spectrum violations / 1,316 numbered H¹s,
kernel_v1 all-null PASS, planted-violation FLAGGED, zero-seat = OQ-202 mint exactly.
**Cell (b) live population 4 → the pre-committed tightening is now OBLIGATORY →
OQ-217** (also scopes the newly-pinned mixed `plural([T,unknown])` cell, 19/66/129
live). D4 case table lives at the `consensus_provenance/2` clause header + the plunit
`coherence_case/5` — OQ-217 must update BOTH in its commit or the suite goes red.

---

## 2026-07-12 — OQ-215 arm 3 COMPLETE: 5/5 variance runs, R2 live for the first time, composed D9 conforming, invariant HOLDS 5/5; threshold recalibrated 25→10
**Files:** agent/uke_narrative_orchestrator.py, python/audits/oq215_arm3_variance.py, audits/2026-07-12_oq215_arm3_variance/
**Tier:** landed

Five serial full-pipeline runs of the-empty-pan.md at the post-ruling instrument state
(pre-registered PROPOSAL.md; driver-enforced kill conditions; neither fired). Read separately:
**M1** `<numeric_register>` complete 5/5 (first live firings — arm 1's was truncated off);
stage-8 densities 0.0/0.12/0.0/0.47/0.0 vs anchored 47.6; six surviving number-words total,
all ordinary prose. **M2** 4/4 stage-10 runs produced conforming D9 (both witness subsections,
hostile own candidates, explicit stage-9 adjudication; zero bare-5s); run 5 exited at review
(STRATEGY at cycle limit — designed). **M3** blind falsifier HOLDS 5/5, floor authored 5/5,
five distinct instrument-unreadable substrates. **NUMERIC_DENSITY_THRESHOLD recalibrated
25.0 → 10.0** per the 2026-07-11 ruling (improved ceiling ~0.5/1000 over six improved runs;
defect band 37.6–47.6). Run-3 word-arithmetic logged to OQ-214's calibration set. Remaining
for OQ-215 close: operator reads (refutation quality, foam-class substrates, ≥1 full story) +
close-out. Readout: `audits/2026-07-12_oq215_arm3_variance/READOUT.md`.

## 2026-07-12 — OQ-215 arms 1–2 run: R3(b) holds (operator-witnessed); blind falsifier discriminates, stage-10 D9 does not; truncation class fixed (caps + cap-hit guard + mode injection)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stories/the-empty-pan_rev2.md, audits/2026-07-12_oq215_arm2_d9_control/
**Tier:** landed

**Arm 1 (R3(b) symmetric control): PASS, operator-witnessed by direct read.** Run
`uke/the_empty_pan_1783838645/` → `stories/the-empty-pan_rev2.md`. Stage 0 authored the R14
contract unprompted (inherent_instrument: yes); instrument survived as mechanism (SSA
certification world) with page density 0.48/1000 vs baseline 47.6 (3 numerals, all
institution-held certificate IDs — earned); blind stage-9 falsifier HOLDS; D9=5. Operator
watch-item: word-arithmetic in prose ("Quota minus rejections…") — invisible to the digit meter,
OQ-214's calibration case. **Caveat: stage_2 AND stage_3 hit their caps exactly (silent
truncation, OQ-216) — stage_3's blueprint lost `<numeric_register>`, so R2's field never reached
generation; arm 1's density win is attributable to the stage-2 gate + R1 exemplars + R6 only.**

**Arm 2 (D9 discrimination control): SPLIT.** Blind stage-9 DISCRIMINATES (Assessment → LOST,
kill passage named + grep-verified, ROUTE STRATEGY as pre-registered). Stage-10 D9 scored 5 on
everything including the negative control — quote-and-rationalize witnessed (cited a
by-eye-recoverable value as proof of unrecoverability), plus FULL-mode hallucinated with no spec
on both runs. **Consequence: any stage-10 D9=5 (incl. rev2's) is weak evidence alone; the
discriminating witnesses are the blind stage-9 falsifier + an operator read.** Readout + proposed
D9 adversarial-obligation fix (awaiting ruling): `audits/2026-07-12_oq215_arm2_d9_control/`.

**Infrastructure (the silent-green class, OQ-216):** stage-0 Gemini truncation guard (`b715f3dc`);
stage_1/2/3 caps raised 16384/16384/12288; universal cap-hit guard in `_call` (tout ≥ cap → fail
loud; Gemini exempt-by-accounting — needs semantic closure checks, noted in OQ-216); stage-10
validation mode now orchestrator-injected (never model judgment). All guards witnessed two-sided
offline.

## 2026-07-12 — CORRECTIONS to the R1–R14 landing: density threshold PROVISIONAL; rift3 "exclusion" retracted (witnessed meter false-positive class); prior D5/origin-obfuscation scores taken with the air gap partially open
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/originals/rift3.md, ISSUES.md
**Tier:** correction-key

Three qualifications on the 2026-07-11 entry below (operator review, 2026-07-12):

1. **`NUMERIC_DENSITY_THRESHOLD = 25.0` is PROVISIONAL, not settled.** Two-point calibration with
   an 8-point dead band above the cleanest human prose (16.3) and 12 more below the nearest
   positive (37.6) — it passes stories counting 2x any clean control. To be RECALIBRATED from the
   OQ-215 arm-3 variance densities (the first real data on the improved-vs-anchored boundary for
   generated output). Do not cite 25.0 as a validated boundary.
2. **The rift3.md calibration "exclusion" was UNSOUND and is retracted.** Calling it
   "counting-saturated by design" was the agent pre-deciding the verdict the instrument exists to
   deliver. Witnessed read (no spend, `_numeric_inventory` + per-line contexts): 46.0/1000 — above
   both positives — but the narrator is a gauge-owning institutional POV whose every reading is
   taken and acted on in-scene (the 12.1%-logged / 11.6%-read discrepancy IS the story's hinge),
   plus numeric proper nouns ("Vent Fourteen" ×10). rift3 is therefore the witnessed
   FALSE-POSITIVE CLASS of the meter: density measures numeric REGISTER, narrower than the defect
   (UNEARNED counting); the discriminator is R2's positional access, applied only by the R6
   per-instance adjudication + OPEN-flag escalation (the gate never auto-rejects — correct). Full
   read + protocol implications in OQ-215; OQ-214 inherits it as the template problem (a character
   who EARNS a thesis-shaped line).
3. **Prior D5/origin-obfuscation scores were taken with the air gap partially open.** The
   stage_1_anon ANONYMIZATION note listed ORIGINAL character names into stages 2/3 for the whole
   life of the anonymizer (fixed in `a3d0fdc4`). Invariant/topology findings are unaffected (those
   were about structure, not names), but any pre-2026-07-11 D5/origin-obfuscation score or
   displacement read was measured over a pipeline that leaked source names into setting design —
   treat those numbers as upper bounds on obfuscation, not clean measurements. Applies to all runs
   in `agent/narrative_transform/uke/` predating `a3d0fdc4`.

OQ-215 protocol also REORDERED (operator ruling): R3(b) symmetric control runs FIRST (highest
information, most likely to fail; its failure collapses the conditional ruling to the hard ban),
then the R13 D9 positive control, then the 5-run variance only if both hold.

## 2026-07-11 — UKE narrative pipeline: counting-defect plan R1–R14 landed (deterministic numeric meter, computed word counts, invariant threading, counting-incentive prompt fixes)
**Files:** agent/uke_narrative_orchestrator.py, agent/narrative_transform/stage0.md, agent/narrative_transform/stage2.md, agent/narrative_transform/stage3.md, agent/narrative_transform/stage4.md, agent/narrative_transform/stage5.md, agent/narrative_transform/stage6.md, agent/narrative_transform/stage7.md, agent/narrative_transform/stage8.md, agent/narrative_transform/stage9.md, agent/narrative_transform/stage10.md
**Tier:** landed

Implemented the full counting-defect plan (`~/.claude/plans/we-are-evaluating-the-zany-biscuit.md`
rev 2; Claude-web comments reconciled) in five committed phases starting at `600abbae` (precursor:
Claude-web's stage2.md invariant-recovery rewrite + the evidence runs, incl. the witnessed
"Forty-Hertz" counting run `the_empty_pan_1783821245`).

- **Instruments (load-bearing, landed before prompt edits):** `_numeric_inventory()` — deterministic
  extraction of numerals/number-words/count-verbs/monotone sequences, injected complete into
  stage 7/8 prompts; model only adjudicates per instance. Post-stage-8 density gate: one targeted
  revision call, then `NUMERIC_DENSITY_OPEN.md` (fail-visible). **Threshold 25/1000 words,
  calibrated:** positives (must flag) stage_4=37.6, stage_8=47.6; clean human originals 2.3–16.3
  pass; inherent-instrument source `the-empty-pan.md` 18.8 passes; rift3 (46.0, counting-saturated
  draft) and the_waste_land (33.1, line-number artifact) excluded from calibration. All story
  word counts now orchestrator-computed and injected ("any other figure is wrong"); stage-8 manifest
  WORD COUNT line overwritten (model had fabricated 13,400 over a 5,927-word file and stage 9
  reasoned from it).
- **Invariant threading:** stage-2 SECTION 0 INVARIANT CONTRACT extracted and fed to stages 9
  (blind falsifier; assertion now `["stage_8","invariant_contract"]`) and 10 (new D9, reported not
  summed, D9≤2 → cannot PUBLISH; UNVERIFIED never N/A in craft mode; F50 fracture code). stage0.md
  authors the source-sighted Detector-A/B contract + `inherent_instrument` flag (surface-free),
  carried into stage 2.
- **Counting incentives killed at source (R3 per operator ruling (b)):** stage3.md exemplars
  rewritten so narrators structurally can't count (doctrine kept); numeric-register blueprint field
  (gauge-indexed access; numbers only when acted on in-scene); stage2.md Scored-Snare
  default-reject, exception gated on the Stage-0 flag only; stage5/6/9 stop rewarding counting
  (ANCHOR CHECK + invariant probe).
- **Mechanical:** final story ships without the EDIT MANIFEST (sidecar); stage4/stage7 prompt fork
  deduped (orchestrator appends canonical craft directives at load time); dead
  `_run_stage_5_narrative` + no-op `--skip-final-audit` removed; per-model cost table; stage-6
  feasible-range injection from `MAX_TOKENS` (16,384 tokens ⇒ ceiling ~11,468 words).
- **Fix-on-sight:** the stage_1_anon ANONYMIZATION note listed ORIGINAL character names (source
  leak into stages 2/3); now labels only. Cycle-2 stage_4 slot gets story-only (manifest stripped).
  Byte-identical dupes deleted: `stage2-original.md` (== HEAD stage2.md), `originals/the_empty_pan.md`
  (would double-process in --batch).

All wiring witnessed offline with fake providers (no spend). **Verification runs NOT yet done** —
protocol pre-registered as **OQ-215** (blocked_on_human: spend-go): 5-run variance vs the 37.6/47.6
baseline, R13 D9 positive control (pre-rewrite "Assessment" story must fail D9), R3(b) symmetric
control (Empty Pan under the flag must clear the meter AND pass D9). Theme-naming meter gap minted
as **OQ-214**.

## 2026-07-11 — OQ-188 + OQ-186 RESOLVED: pre-registered read-site flags (role-flip standing glyph; common-cause independence bit); false-cartel defensibility ruling downgraded
**Files:** python/shared/role_flip.py, python/shared/independence.py, python/enhanced_report.py, python/tensions_ledger.py, python/evaluative_convergence.py, prolog/config.pl, prolog/tests/test_oq186_common_cause_clique.pl, python/tests/test_role_flip_flag.py
**Tier:** landed

Full pre-registration (`PREREG.md` committed `57159a36` BEFORE any run) → evidence →
read-site fix, all in `audits/2026-07-11_oq186_oq188_readsite/`.

- **OQ-188 (98.1% branch).** Fire-rate census (manifest 2026-07-05T19:55:12Z, n=130):
  103/105 matched institutional seats flip under a single authored role change
  (agenda_setter 0.12 ↔ beneficiary 0.25 straddle the f(d) root d\*≈0.16418) →
  pre-registered ≥50% STANDING form: one legend sentence + per-line `‡` glyph
  (`shared/role_flip.py`, zero free parameters — role ladder, sigmoid, and root all
  from the SERIALIZED config). Buckets surfaced (unmatched=16 incl. 0.15×6, null=9);
  1 powerless firing → glyph is per-fired-seat (declared deviation, audit README).
  `config.pl:156-160` straddle note is comment-only; d values + OQ-01 bypass untouched
  (flag is commentary-grade — annotates, never overrides).
- **OQ-186 (outcome (a)).** A/B probe: co-authored slices form the full 3-clique,
  distinct-agent topic forms zero edges — node independence is not expressible
  Prolog-side (dedup keeps one edge per pair); discriminator `shared/independence.py`
  (≥1 shared beneficiary AND ≥1 shared victim AND |Δε|≤0.02; ε clause kept by census,
  9/21=42.9% of non-both-sides pairs inside margin). The one live both-sides pair IS
  the witnessed `moral_causation_locus` family.
- **Joint defect fixed.** `evaluative_convergence.py` `build_defensibility` no longer
  rules "coordinated rather than independent operation" on artifact-channel sets
  (either `all_members_knife_edge` or `members_common_cause_clique` true) — downgraded
  to a caveated constrained position; XCON elevation suppressed likewise. Two-sided
  witness: the dispositional_reading set fires both booleans and is caveated;
  network_2638bfb4 / network_a6b8a722 stay knife=False clique=False with the original
  ruling byte-unchanged.
- **Fix-on-sight (tensions_ledger).** Serialized neighbor keys are
  `constraint_id`/`edge_strength` — the old `id`/`edge_contamination` lookups rendered
  EVERY ledger edge as `? [...; strength ?]`; fixed, and the stale "provenance NOT
  CARRIED — OQ-103 open" note dropped (OQ-103 resolved 2026-06-12).

Regressions: plunit `test_oq186_common_cause_clique` (3/3) +
`python/tests/test_role_flip_flag.py` (6/6) + existing OQ-103 salience test still
green. No new Prolog reading predicate → `reading_registry.pl` obligation N/A.

## 2026-07-06 — OQ-213(a): `twin_comparison.py` graduated to N-general (Sonnet now a full paired leg); 3-leg run at HEAD, intersection 957
**Files:** python/audits/twin_comparison.py, outputs/pipeline_output.haiku.json, outputs/pipeline_output.flash.json, outputs/pipeline_output.sonnet.json, ISSUES.md, AGENTS.md
**Tier:** landed

Wired the built Sonnet leg from a marginals-only control into a full paired twin, and made
the harness **N-general** (a 4th Opus leg is a one-line add). OQ-213(a) resolved; (b) stays open
(kill-conditioned). Interpretation (which model is the odd one out) stays with OQ-123/124 —
this entry is the WIRING + preservation witness only.

- **Precondition (one coherent triple at one commit).** `twin_comparison.py` refuses to join
  legs at different `code_commit`; the legs straddled commits (haiku/flash `bbf5c92`, sonnet
  `ea8ed72`). Re-classified all three at HEAD **`1169170`** in ONE serialized `classify_corpus`
  batch (shared `pipeline_output.raw.json`; no interleaved working-tree edit → identical stamp is
  honest), each passing the four refusals (zero-glob / load-completeness / single-model fingerprint
  / raw-freshness). n = 960 / 960 / 1001, all `code_dirty:True` (shared → guard compares commit only).
- **Engine change (`twin_comparison.py`).** `--twin` takes ≥2 legs; refuse-to-join guards run over
  **all pairs** (no two share `corpus_path`; **all** legs one `code_commit`); intersection is the
  **common** id set; each pre-registered pairwise falsifier fans out over every unordered
  `combinations(sorted(labels),2)` with a **per-pair salted RNG** `random.Random(f"{seed}:{x}:{y}")`
  (buys P1 order-independence, NOT P2 old==new byte-identity — the old bare-seed stream is threaded
  across fields, so any re-seed moves the permutation numbers by construction). New
  `analyse_agreement_nway` emits the N-way partition (unanimous / odd-one-out + odd-leg tally /
  split-multi) **with the missingness complement carried** (map-intersection ≠ field-non-null; guards
  against residual (b) silently biasing the surface). `sonnet_control` + the conditioned OQ-125/123
  block left AS-IS (byte-unchanged; run on their own 2-leg intersection). JSON keys renamed
  `structural_H1`→`structural_H1_pairs`, `continuous_H2`→`continuous_H2_pairs`, added
  `structural_agreement_nway` + `pairs` (no external consumers — only prose docs reference the artifact).
- **Verification (`audits/2026-07-06_oq213a_twin_sonnet_leg/`).** V1 behavior-preservation **SPLIT**
  witness on pinned pre-reclassify inputs: the rng-free deterministic bucket (`agreement/disparity/
  rate/wilson_lo,hi/n_both/one_sided/exemplars` — traced rng-free by reading the helper bodies, not
  inferred from the seed site) is **byte-identical** OLD vs NEW; the permutation bucket agrees within
  the **measured** two-salt MC envelope (ran NEW at two seeds to size it — all `|old−new|`≤0.001), no
  `H1_holds` flip. V2 3-leg at HEAD: common intersection **n=957**, three crossings/field, all seven
  partitions close (`n_all_populated=unanimous+odd+split` AND `n_all_populated+missing=957`). V3:
  the haiku×flash pairwise delta from the historical 960 is **exactly** the 3 non-triple ids
  `(haiku∩flash)−sonnet` = the treaty/legal seeds (`article_27_veto_power`, `nsl_legal_text`,
  `unsc_242_withdrawal_clause`) — residual (b)'s footprint, not a behavior change. V4 ingestion
  control: sonnet JSON leg id-set == raw `.pl` source (1001==1001).
- **Now-decidable INPUT (not verdicts).** odd-leg tallies: `persp:powerless` {sonnet 190, flash 185,
  haiku 175} (near-even — no localization on one triple); `signature` {haiku 135, flash 89, sonnet 59}.
  `verdict` missingness is 204 ids spread across all legs (sonnet 113 / flash 91 / haiku 71) — the
  general `verdict_join`-null (OQ-51/98), NOT the residual-(b) sonnet drop; perspectives have 0 missing.
- **Note the leg JSONs are now at HEAD `1169170`** (gitignored/regenerable) — a later citation of
  `pipeline_output.{haiku,flash,sonnet}.json` must name BOTH corpus and commit (Running the System).

## 2026-07-05 — THIRD model-twin leg built: `testsets_sonnet/` (claude-sonnet-5, 1001 stories) — matched triple 957/960; unblocks the 3-model divergence OQs
**Files:** agent/run_no_scope_sonnet.py, prolog/testsets_sonnet/, json_sonnet/, prolog/beta_processed_sonnet.txt, prolog/testsets/, json/, python/audits/twin_comparison.py, ISSUES.md
**Tier:** landed

Built a **third matched twin** over the SAME 1005-seed pool as the haiku/flash twins
(`prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json`), so the model-divergence
OQs (OQ-123/124/149/211/212) get a third data point instead of a 2-model pair.
- **Tool:** `agent/run_no_scope_sonnet.py` — Anthropic Batch-API mirror of
  `generate_kernel_corpus.run_no_scope`, **byte-identical prompt** (reuses `build_cached_messages`
  + `process_batch_results` unchanged), **thinking DISABLED** (`{"type":"disabled"}` — twin parity;
  Sonnet 5 runs adaptive thinking by default), temperature omitted (Sonnet 5 400s on non-default).
  Dir-scoped registry (SONNET dir + `beta_processed_sonnet.txt` ladder only) so basenames == seed
  cids and the three legs pair by filename (twin recipe, `bulk_corpus_generation.md` §6).
- **Result:** `testsets_sonnet/` = **1001 stories**, all `claude-sonnet-5` provenance; **~2% attrition**
  (matches the haiku/flash band). Matched triple **sonnet∩haiku∩flash = 957/960** (99.7%).
- **Third-leg pipeline output:** `outputs/pipeline_output.sonnet.json` (gitignored, regenerable) via
  `classify_corpus('testsets_sonnet','pipeline_output.sonnet.json','claude-sonnet-5')` — passed all
  four refusals (zero-glob, load-completeness, single-model `claude-sonnet-5` fingerprint,
  raw-freshness); manifest n_constraints=1001.
- **Extras merge (twin-parity rule):** 44 Sonnet extras (basenames not in the haiku∩flash 960). The
  **39 collisions are exactly the twins' own extras already in `testsets/`** (28 haiku_only + 11
  flash_only) — NOT overwritten (Build Discipline #3; their Sonnet draws stay in `testsets_sonnet/`),
  matching how flash was handled vs haiku. The **5 genuinely-new** extras (`.pl`+`.json`) merged into
  `testsets/` (130→135, +5 sonnet stories in the canonical live corpus).
- **Spend:** ~$48 total (full run ~$46 + rerun ~$1.65), Sonnet-5 batch at intro promo rates.
- **Systematic finding (→ OQ-213):** 4 seeds fail persistently across 3 fresh redraws — all
  `'stakeholders' is a required property` post-repair, concentrated on treaty/legal kernels
  (bitcoin_consensus, nsl_legal_text, article_27_veto, unsc_242_withdrawal). This is a Sonnet-SPECIFIC
  schema-conformance mode (haiku/flash hit the `status:'contested'` enum instead); NOT chased further
  because a prompt/schema patch would break twin-parity. 3 of the 4 are in the haiku∩flash intersection
  (why the triple is 957 not 960).
- **Wiring gap (→ OQ-213):** `twin_comparison.py` wires Sonnet only as a marginals-only, non-blind
  `sonnet_control` (reads `.pl` directly, "says NOTHING about (c2)"). With a full classified leg now
  present, it can graduate to a full paired third twin (haiku-flash / haiku-sonnet / flash-sonnet).

## 2026-07-04 — OQ-88 MITIGATED: false-mountain detector sweep (positive WITNESSED N=2; D′ discriminator SATURATES; Ω_P auto-route ruling handed to operator)
**Files:** python/audits/oq88_false_mountain_detector.py, ISSUES.md, audits/2026-07-04_oq88_false_mountain_detector/
**Tier:** landed

Pre-registered sweep (PROPOSAL.md frozen before any run) of the OQ-88 candidate detector
`D = flat-routed ∧ engine-false-mountain` (claimed mountain ∧ maxent-top rope ∧
`type_1_false_summit` alert — keyed on alert TYPE, not severity: the archived World3 positive
is pre-OQ-128 `severe`, live firings are `informational`). **Cell 1 WITNESSED:** both full-D
positives fire (`demographic_skill_mismatch_c0` live via china 163143 flat manifest;
`collapse_mechanism_ambiguity` via World3 171605 flat manifest ∧ the 06-11 oq90 archived
Layer B); dispatch controls two-sided green. **Cell 2 INFERRED-weak:** no labeled flinch;
kernel_v1 fresh `classify_corpus` at e438723b (1,106 stories, `pipeline_output_kernel_v1_oq88.json`
— the e8189d1-era `pipeline_output_kernel_v1.json` NOT reused, output-changing OQ-138/OQ-205
commits landed between; not overwritten either) gives 41 claimed mountains, ONE alert-firing
false-mountain, flinch tail 0/1; alert channel live (1,082/1,106 alert-bearing). **Cell 3 DEAD:
the pre-registered regime-omega discriminator saturates** (present on 4/4 live flat, 4/4
routing-unknown, 1/1 kernel_v1 — the corpus omega convention frames uncertainty as
natural-vs-constructed nearly universally), so D′ ≡ D and cannot be the refined gate.
Live partition of the 9 alert-firing: 4 flat / 1 kernel-routed (caught:
`neutron_star_bombardment_reading`) / 4 routing-unknown fail-closed — a ~44% Layer-A coverage
hole any gate wiring must state; `organization_floor_c0` = the h1-null no-alert undetermined
bucket. **Residual (blocked_on_human, in OQ-88):** the Ω_P ruling — auto-route vs the light
"operator kernel-vs-flat review prompt" seat; optional flinch label upgrades cell 2 to
witnessable. Verdict per the pre-registered cells: **gate-signal CANDIDATE, not a witnessed
gate.**

*Twin-legs addendum (2026-07-05, `TWINS_ADDENDUM.md` in the audit dir):* fresh per-twin
`classify_corpus` at `8a529c73` (960/960 each, fingerprint enforced). D fires on ZERO twin
stories **by construction** — both twins are 100% kernel-linked (960/960 in-file `cs_kernel_id`),
so no flat conjunct exists. **New instrument:** in-file `cs_kernel_id` is a second Layer-A
routing source the manifest walk misses (seed-pipeline corpora have no manifests); any D wiring
must use manifest-map ∨ in-file `cs_kernel_id`. Live partition retro-checked and stands (0/5
routing-unknown+undetermined carry it). Discriminator saturation replicates 69/69 → D′ dead
corpus-family-wide. Mountain→rope rates regime/model-bound (OQ-70 rule, not citable as
contest-tracking): live 10/18, kernel_v1 2/41, haiku 22/72, flash 55/104.

*kernel_v1 re-partition (2026-07-05, `KERNEL_V1_ADDENDUM.md`):* with the stamp instrument,
kernel_v1's Layer A is computable for 875/1,106 (873 stamped `__`-readings + 2 manifest-flat)
and D is STILL 0 there — the firing false-mountain (`maxwell_demon_impossibility`) is
pre-router (unstamped, no manifest, no provenance ⇒ D-inapplicable, no routing decision
exists); the no-alert mountain→rope (`statutory_debt_ceiling__...`) stamp-resolves to
kernel-routed = caught. Pin 1 result stands, reasoning sharpened: pre-router archive stories
are D-INAPPLICABLE, not D-negative.

*Post-review amendments + Ω_P RULED (2026-07-05, FINDINGS.md §Post-review amendments):*
(a) snare read — alert type `type_1_false_summit` is SHARED across divergence targets
(severity is dr_type-keyed, crosses maxent top: `institutional_trust_erosion_c0` snare-top w/
informational); predicate uniform (exclusion rides maxent-top-rope, never severity; type-only
w/o conjunct = 14 not 9); positive control = **N=1 per engine regime** (411db0e / 23b7faa).
(b) cell 2 has NO archive leg — kernel_v1's D-applicable population empty by era; rests on
the 4 live flat candidates, no witness either direction. (c) Phase 2c retroactively vacuous
(saturation ⇒ regime-omega never validated as discriminator on any instance). (d) World3
06-08 manifest join witnessed (Pin-2 OPEN discharged). **(e) operator ruling: review-prompt
light seat, NO auto-route** (kill condition: labeled live candidate adjudicated
suppressed-seat kernel at a rate holding across the 4 reopens it). Follow-ups: prompt wiring;
optional flinch label.

## 2026-07-04 — OQ-125 RESOLVED (value-invariance beyond H1) + OQ-123 MITIGATED ((a) refuted; (b)-or-(c2) live): conditioned twin re-analyses
**Files:** python/audits/twin_comparison.py, ISSUES.md, audits/2026-07-04_twin_conditioned/
**Tier:** correction-key

Pre-registered conditioned re-analyses on the `bbf5c92` twin pair (n=960; pre-reg committed
`bc04d809` BEFORE any run; results commit `1314fecf`). **OQ-125:** the below-band |Δχ| tail
survives same-side (same-seat-type) conditioning at ALL 4 typed χ seats — observed and permute
band recomputed from the same stamped id set — so it is real continuous value-invariance beyond
H1, not threshold-colocation; `theater_ratio` stays exploratory (headline is 4/5). Citation rule:
"value-invariance confirmed (4 typed seats)" — never unqualified. **OQ-123:** authored/imputed/
absent source-join partition — (a) imputation-drag REFUTED (authored-both n=805 agreement 0.3925
vs unconditioned 0.3937; imputed cells ≤5/960, flash has ZERO imputation-eligible stories);
(c1) directional (haiku omits 72 vs flash 36) but not the driver; live remainder (b)-or-(c2),
B4 NOT armed (frozen: powered LOW only). Harness extended additively (`--conditioned-outdir` /
`--source` / `--control`); pristine-vs-extended regression byte-identical; positive-controlled.
**Correction-keys for future citation:** (1) the "imputed ⇒ powerless d==0.90" tell is FALSE
(witnessed d=0.95 on a sentinel-victim story; d has more inputs than the victim bridge — never
classify imputation from d); (2) the 2026-06-13 twin tables are `8126231`-regime — the `bbf5c92`
unconditioned tables live in `audits/2026-07-04_twin_conditioned/unconditioned_bbf5c92/`;
(3) "authored-both clears its chance band" is trivially expected wherever corpus-wide H1 held —
an (a)-style drag claim needs the comparative clause (cell rate ≫ unconditioned rate), not
band-clearing alone.

---

## 2026-07-04 — OQ-140 RESOLVED: `author_engine_divergence` characterized (confound re-ranks kinds; one kind — Ω_E stratum reproduces on both twins, Ω_C reading 3/3 twin-confirmed)
**Files:** ISSUES.md, prolog/routing_sink.pl, python/audits/oq140_divergence_extract.py, audits/2026-07-04_oq140_divergence_characterization/
**Tier:** landed

Branch-3 (hybrid G-A) characterization audit of the routing sink's majority address
(`author_engine_divergence`, 277/512 on 96/128 at HEAD `7762b2c0`). No engine edits.

- **Method finding (reusable):** partialling the mechanical confound BEFORE decomposing
  re-ranks the population. ε is uniform across seats 96/96 ⟹ 100% of per-seat orbit variation
  is `d`-driven; confound-free (G-A uniform-orbit) = 56/277 (20.2%), confound-exposed = 221/277
  (79.8%, *granularity-expected, not kind-shaped*). Pre-confound lead `tangled_rope→snare` (111)
  dissolves; surviving confound-free lead is `rope→scaffold`.
- **One validated kind (operator-ruled):** **`naturalization-over-claim (rope→scaffold
  correction)` [Ω_E]** — author over-claims `rope` for a constructed frame/reading/standard the
  engine corrects to `scaffold` (`emerges_naturally=False` 9/9); one-directional 21/28
  rope-claims→scaffold. Pre-registered name "permanence disagreement" KILLED (predicted sunset;
  `has_sunset_clause` NO 8/9).
- **Two-tier promotion (do NOT collapse):** Ω_E stratum reproduces on BOTH twins (rope→scaffold
  G-A: haiku 49 / flash 64); Ω_C reading is live-corpus + 3/3 twin-confirmed. Contested-kernel
  members (6/9) carry an Ω_P sub-note. **Ω_E typing clean 5/6 on the structured witness
  (`emerges_naturally` seat-blind 6/6); one member — `fictional_construct_reading:204` — holds
  an UNRESOLVED Ω_P prose-signal** (an authored seat-declaration of its rope-claim, the
  pre-registered falsifier firing through a channel the structured grep didn't cover). Does NOT
  retract the kind's Ω_E typing (correction flag seat-blind on all 9); open item parked in
  OQ-211(d), not a curiosity.
- **Scope pin (freshness):** kind name + counts valid only relative to `route_address/5` at HEAD
  `7762b2c0`; any OQ-138 sibling-clause edit invalidates the taxonomy (OQ-211 carries this).
- **Controls:** emit-independence byte-agreement 277/277 (`constraint_claim/2`+`dr_type/3` vs
  sink); D-ladder 49 baseline raw≠final seats; mountain 0-count w/ same-path positive control.
- Residuals → **OQ-211**. Commits `e90bf3db` (Phase 0/1), `9d7baf07` (Phase 2), this (Phase 4).

*Promotion test:* no CLAUDE.md tripwire — this is a resolved research finding, not a silent
pre-edit footgun. The one durable caution (scope-pin: sibling-clause edits invalidate the
taxonomy) rides OQ-211's `bundled_with OQ-138` edge and the audit WRITEUP, where a reader
editing `routing_sink.pl`/`signature_detection.pl` will meet it; not always-loaded material.

---

## 2026-07-04 — Drone-report audit (Claude-web critique): d-header fixed, signature wording softened, OQ-209/210 minted, regulatory_lag H¹ fracture witnessed ROBUST
**Files:** python/enhanced_report.py, prolog/signature_detection.pl, ISSUES.md
**Tier:** landed

External critique of the four 2026-07-03 22:16 drone reports (procurement_inertia,
technology_diffusion_asymmetry, weaponization_accessibility, regulatory_lag_extraction). Triage +
actions:

- **FIXED — d-comparability header was factually false** (`enhanced_report.py:356`). Old text: "d is
  a function of the observer POSITION (a config lookup)… identical d across constraints for the same
  position is by design." But `derive_directionality/3` (`constraint_indexing.pl:408`) precedence is
  override → `beneficiary_victim_directionality` (power role + has-benef/victim + `exit_modulation`,
  all authored per story) → `canonical_d_for_power` fallback. Only the fallback is a config lookup;
  the common path is authored, so the SAME position label carries different d (institutional d ∈
  {0.72, 0.45, 0.15, 0.12} across the four reports). Header rewritten to state the precedence and
  that cross-constraint "same seat" d-comparison is NOT apples-to-apples. Reports on disk keep the
  old header until regenerated.
- **FIXED — `coupling_invariant_rope` explanation overclaimed** (`signature_detection.pl:769,772`).
  "Passes all structural purity tests" was false: the signature gates on Boltzmann compliance + scope
  invariance only; `ExcessEps` is reported, not tested (procurement certified at excess 0.580, which
  its own drift section flags as `excess_above_floor(0.58)` + 2 critical drift events). Softened to
  "coupling-clean (snapshot) … NOT an excess-extraction or drift gate." Behavior-preserving (display
  atom only; grep-verified no parser). = OQ-210 (resolved).
- **OQ-209 minted (open)** — single-constraint scenario reports render corpus-scope metrics as
  success-shaped defaults: W1=0.0000 printed beside H¹=4 (`wasserstein_corpus_fracture` silently
  skips constraints lacking MaxEnt distributions → skip-zero rendered as measured-zero, and "Corpus"
  is a misnomer in a 1-constraint run); "Network stability: stable" is a 1-node network beside a
  corpus header of "cascading". Pattern-6; bundled_with OQ-97. Graduation = witness skip-vs-genuine.
- **FALSIFIER RUN (the "one thing the reports don't show") — regulatory_lag H¹=4 is ROBUST, not a
  config/transfer-function artifact.** Baseline orbit `[tangled_rope,snare,tangled_rope,snare]`,
  H¹=4, reproduced. Swept metric-ε and the f(d) seat-curve (`config:param(cognitive_displacement)`),
  clearing caches each step. H¹>0 survives ε ∈ [0.50,0.90] and d_offset ∈ [-0.15,+0.20]; collapses to
  agreement only at extreme shifts (d_offset −0.20 → all rope). The invariant throughout is the 2+2
  structure powerless≡institutional ≠ moderate≡analytical (Hub-2 immutability axis) — that IS the
  perspectival finding, and it is stable under perturbation of exactly the authored ε/d values the
  critique questioned. NB: `domain_priors:base_extractiveness` is STATIC (χ-side ε unperturbable by
  retract; the sweep moved the dynamic `constraint_metric(extractiveness)` metric-ε). The d-curve
  perturbation directly moved f(d) and the fracture held. Probe scripts in scratchpad (not committed).
  Caveat unchanged: ε/d are authored (OQ-102a) and Fisher/persistence remain STALE (OQ-29) — this
  witnesses robustness for THIS constraint, not a corpus-wide re-validation.

---

## 2026-07-04 — OQ-193 report-surface build: giant_comp provenance split (pooled + cross-kernel stratum)
**Files:** prolog/giant_component_analysis.pl, python/run_pipeline.py, python/enhanced_report.py
**Tier:** landed

The owed OQ-193 report-surface build (RULED (c) 2026-07-02) landed at **zero engine-behavior change**:
`pipeline_output.json` `per_constraint` is **byte-identical** (sha256 match) before/after a full pipeline run;
`constraint_neighbors_existing/2` and the `drl_purity_network.pl` sibling warnings are untouched. Two surfaces —
(1) `giant_component_analysis.pl` gains a `## Provenance split (OQ-193)` md section + a same-run
`giant_component_analysis.raw.json` co-product (pooled vs sibling-stripped stratum + per-constraint
membership/degree); (2) `enhanced_report.py` gains a per-constraint "NETWORK POSITION (OQ-193)" L1 section +
additive `network_position` sidecar with a four-branch interpretation.

**Method = retract-recompute, dead-last.** `deduplicate_neighbors` keeps the strongest edge per pair, so a
post-hoc `gc_edge` filter would miss an inferred edge that resurfaces on recompute; the faithful strip retracts
the same-kernel-explicit `affects_constraint` **substrate** and recomputes. Placed dead-last in
`run_giant_component_analysis` (after `report_embedded_facts`), in a subprocess that then exits, so the strip is
**never restored** (the probe's re-assert step is intentionally dropped) and nothing downstream reads stripped
topology. Does its own fresh pooled `measure_topology` first because phases 2/4 mutate gc state.

**Commit-Gate-1 outcome (the witnessed cause, not the assumed one):** `same_kernel_edges_surviving = 0` on
`testsets/` — dedup-resurfaced 0, never-stripped 0 (partition identity M1+M2==M asserted, held). So the
**`cross_kernel` label is HONEST** and the dedup subtlety was defensive-only (did not bite); **no operator
escalation triggered.** The M>0 branch (rename to `explicit_sibling_stripped` + escalate) exists in code but is
unexercised on this corpus.

**Witnessed values (testsets, 2026-07-04):** 68 sibling edges stripped; pooled giant 12 / 72 components →
stratum giant 9 / 95 components; positive control ok (raw `affects_constraint` 241→173, dropped 68 = strip
count). Matches the frozen probe at both endpoints. Node set = `all_corpus_constraints/1` = **119**
(extractiveness-bearing), a **subset of the 128-constraint corpus** (manifest `n_constraints`) — the plan's
"per_constraint == manifest" premise was slightly off; 119 is giant_comp's own denominator (phase-1 "Total nodes
= 119"), and the 9 excluded are all `*_contradictions` stories lacking an extractiveness metric, correctly
surfaced by enhanced_report as "not in node set."

**Run-scoped binding.** `_prolog_giant_comp` pre-deletes `giant_component_analysis.raw.json` as its FIRST
statement (`unlink(missing_ok=True)`) and asserts the `## Provenance split (OQ-193)` marker is in stdout before
writing the md (a standing guard against a future Prolog-side catch-wrap/soft-fail silently dropping the owed
section). `_manifest_step` stamps `giant_component_analysis.manifest.json` (mirroring the orbit sidecar) **only
when the `giant_comp` StepResult is `status=="ok"` AND raw.json exists** — executed-stage membership, so a
skip/fail path can never pair a stale raw.json with a fresh stamp. enhanced_report joins via `manifest_key`
same-run guard and degrades to NOT ASSESSED on stale/missing/unparseable.

**tripwire — giant_comp intermittently times out (900s) in the parallel Phase-2.** First full-pipeline run this
session, `giant_comp` hit the 900s subprocess timeout **despite running in 0.64 s / 18 MB standalone** — the
documented intermittent co-residency stall (OQ-182 class), NOT a regression from this split. The degrade design
worked exactly as intended: step logged `status=error` (non-critical, pipeline continued to exit 0), the
pre-deleted raw.json stayed absent, the md was not overwritten, and `_manifest_step` **skipped** the sidecar
stamp (no fabricated current identity). A re-run completed `giant_comp` cleanly and produced all artifacts
same-run. If a run's giant_comp surface is stale/absent, check for this timeout before suspecting the code.

**Probe is a frozen dated snapshot.** `audits/2026-07-02_oq193_giant_comp_ruling/probe_giant_ripple.pl` is NOT
edited; production adapts its strip/measure logic and is expected to diverge (the drift is declared, not silent).

---

## 2026-07-04 — OQ-75(b) grain precursor probe: throw LARGE, cell-count non-monotone under coarsening (statistic-spec inputs)
**Files:** python/audits/oq75b_grain_probe.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Pre-registered unratified grain arms over the tranche-1 registry (10 pilot kernels, 42
pairs, both legs; `audits/2026-07-04_oq75b_grain_probe/`). One arbitrary refinement step:
cells 47→21, tordesillas conversion dead, contradiction-pair co-slotting 3/3→0/3.
Coarsen-max: alignment mass grows (theorem, disclosed) but the raw cell count FELL 47→42 by
vantage consolidation — a cell/vantage-count invariance statistic reads coarsening with the
WRONG SIGN. Verdicts grain-labile both directions (key_fragile 26→38→12). Constraints fed
to the future §7.1 correlation-statistic spec (recorded in OQ-75's ruled sub-item): grain
normalization load-bearing; no raw-count statistic; contradiction-pair reads
refinement-brittle; grain-stamp ax_stability_verdict aggregations. Controls fired:
overlay-took-effect (fact counts + A1 atom set), known-changer (A1 merged digital_money's
slots), A0 externally consistent with the OQ-72 sweep (47==47). Canonical registry never
edited (arms are in-process overlays). Stage 1 proper NOT discharged — statistic unbuilt.

## 2026-07-04 — OQ-72 consumer wiring: axiom concept alignment section in tensions_ledger (three-valued coverage); baker emits tranche-kernel facts
**Files:** python/tensions_ledger.py, python/axiom_concept_bake.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Operator-directed post-close wiring (the ledger, NOT enhanced_report — per-constraint is the
wrong altitude for a cross-reading product). `tensions_ledger.py` now appends a kernel-level
"Axiom concept alignment" section: swipl subprocess each run (fresh compute, no stale sidecar),
both keys per within-kernel pair, agree/disparity cells rendered (a disparity cell = same
subject, opposed groundings = a tension by construction). Coverage is THREE-VALUED per kernel
and never collapsed: RATIFIED (cells) / NOT-YET-RATIFIED (tranche never ruled; blind BY DESIGN,
never "no shared subjects" — GAP-24) / single-reading (no pair exists; named, not dropped);
kernel-less constraints counted. Fails LOUD on swipl error (a missing section must not read as
measured-no-tensions), and carries an in-run TWO-SIDED join control closing the
CLEAN-EMPTY hole (reviewer catch 2026-07-04: a present-but-empty registry loads fine and
renders `concept 0/0/N` identically to a genuine no-shared-occupants pair): known
same-concept pair must align AND known distinct pair must not, else halt(3); fired-status
rendered into the section header. Both arms witnessed (positive: section line; negative:
in-process retractall -> exit 3). The control's own falsifier caught a real bug in its
first version (format/2-vs-format/3 on the failure branch). Baker now also emits `axiom_diff:axiom_concept_tranche_kernel/1` (one per
kernel in the ratified TSV, incl. hypothetical all-no_slot kernels) — the coverage provenance
bit travels in the registry; regen witnessed concept-facts byte-identical + C6 refusal re-run.
Witnesses: mixed-scope run (2 ratified kernels w/ cells + seat_gauge NOT-YET-RATIFIED + 1
kernel-less) and full-128 run (46 kernels: 3 ratified / 10 unratified multi-reading / 33
singletons named / 58 kernel-less), both pasted in-session 2026-07-04. New tension surfaced
immediately: moral_causation_locus accountability_intervention_locus disparity
[deontological]|[instrumental].

## 2026-07-04 — OQ-72 resolved: ratified concept key for the axiom axis (pilot); axiom_concept_registry born; westphalia tests re-frozen
**Files:** prolog/axiom_concept_registry.pl, python/axiom_concept_bake.py, prolog/stack.pl, prolog/tests/test_axiom_diff.pl, prolog/axiom_diff.pl, ISSUES.md, docs/the_perturbation_principle.md, docs/design/design_gaps.md
**Tier:** landed

OQ-72 closed at the scoped altitude "mechanism demonstrated" (mixed haiku/live 10-kernel
pilot; audit `audits/2026-07-03_oq72_concept_key_pilot/`, WRITEUP.md there has the control
table). The formerly-empty `axiom_diff:axiom_concept/2` seat is now populated by the NEW
CANONICAL `prolog/axiom_concept_registry.pl` (71 ratified facts, tranche 1), loaded from
`stack.pl`; regenerate ONLY via `python/axiom_concept_bake.py` (fail-closed on unratified
rows) from a ratified TSV — hand-edits lose ratification provenance. All six pre-registered
controls passed (C1 3/3, C2 3/3, C3 10/10, C4 fired w/ planted control, C5 green, C6
refusal); false-merge 0/71; both kill legs clear. Three standing cautions for future
sessions: (1) **the registry is name-keyed** — a mapping applies wherever the axiom name
occurs on ANY leg (witnessed: one pilot name recurs in the flash twin; disclosed in the
registry header) — never assume per-leg scoping; (2) **`cs_axiom_contradiction` is not
universally same-subject** (2 of visual_evidentiary's 3 pairs oppose across subjects and
cannot align under any assignment — don't read their non-alignment as proposer failure, and
don't build scale-up gates on contradiction⟹same-subject); (3) **epistemic reframe**: the
key makes the axiom axis RATIFIED-legible, not discovered (§7.1 amendment 2026-07-04);
OQ-75(b) carries a labeled asymmetry + pending blocked_on_human parity ruling. Also fixed
en route (pre-existing): `tests/test_axiom_diff.pl` westphalia tests had been silently
unrunnable-green since the 2026-06-20 regime swap (froze old corpus axiom names; now
fixture-local + corpus-independent), and their cleanup's blanket
`retractall(axiom_concept(_,_))` would have wiped the baked registry mid-session (now
scoped; post-run count 71 witnessed). SCOPE-time concept-slot emission = GAP-24. Scale-up
= separate spend-go (recipe in the OQ-72 resolution).

## 2026-07-03 — OQ-03 RESOLVED: operator declared DR's own seat (extraction-seeking skepticism); 03b mooted; self-application run snapshotted
**Files:** ISSUES.md, audits/2026-07-03_oq03_self_application/
**Tier:** landed

**The close.** Operator ruling in session: 03b (the empirical limb) is MOOTED — where DR sits is
not a fact a redraw could measure ("it doesn't matter how many times we reran this"); it is the
declared seat itself. Declaration (operative text in ISSUES.md OQ-03): DR is a variety of
philosophical skepticism whose seat is to look for extraction everywhere — a technique for
surfacing seats, particularly hidden ones, and the cover stories that conceal them; a lens with
different apertures and positions (`docs/seat-theorem-v1.md`, `docs/commitment_systems/*`,
`docs/debugging_philosophy.md`), not the truth; the focus shapes what it can see, which is what
makes it a seat. Known limit: the outside seat cannot read some internal dynamics
(`essays/2026-06/the_same_paper.md`).

**The datum.** Same day, the operator ran `docs/deferential_realism_paper_v8.md` through
`c-orchestrator` (5 stories, commit `72ab7663`, manifest n=128): seat-indexed plurality, no
single type — kernel siblings diverged (snare-family w/ extraction_blindness mismatch vs
all-scaffold w/ false_ci_rope commentary), flat control unknown/scaffold. Inputs LLM-drawn,
single draw, not pre-registered → illustrative seated datum only, never "DR is X." Ledger + 5
reports snapshotted (outputs/ is gitignored): `audits/2026-07-03_oq03_self_application/`.

---

## 2026-07-03 — OQ-205 RESOLVED: ε declaration discipline BUILT (11 units, Controls P/S green through the recurring gate)
**Files:** prolog/constraint_indexing.pl, prolog/boltzmann_compliance.pl, prolog/narrative_ontology.pl, prolog/data_validation.pl, prolog/json_report.pl, prolog/reading_registry.pl, prolog/tests/test_epsilon_declaration.pl, prolog/tests/fixtures/eps_controls/, python/generate_constraint_pl.py, python/run_pipeline.py, python/enrich_pipeline_json.py, python/enhanced_report.py, python/sweeps/epsilon_stability.py, python/epsilon_authorship_readout.py, docs/design/epsilon_declaration_discipline.md, docs/deferential_realism_paper_v8.md, ISSUES.md, audits/2026-07-03_oq205_build/
**Tier:** landed

Build phase U1–U11 landed same-day as the spec (commits `e9041905`…close; unit→commit map +
all transcripts: `audits/2026-07-03_oq205_build/README.md`). All five §9 graduation criteria
met; OQ-205 → resolved. Highlights a future session needs:
- **Both §3 fabrication fallbacks are DEAD** (U1 `get_true_metric` 0.0 → `unknown`; U2
  boltzmann `BaseEps=0.5`/`Supp=0` → fail-closed mirroring `is_X/3`). Witness: live +
  haiku + flash + kernel_v1 all byte-identical post-normalization; the U2 first cut
  (`Type=unknown` token) was REJECTED for emitting computed-looking `scope_violations: 0`
  over an unknown grid (Pattern 6) — fail is the honest arm.
- **No-backfill ruling recorded** (spec §3/§9): generator-forward; the whole pre-build
  corpus is the declared loud-null stratum (`"none_authored"` emission token, census
  110+1 live); corpus-complete at rebuild.
- **New recurring gates:** `_prolog_epsilon_declaration_gate` (suite + Control P fixture
  corpus through the real load path, `tests/fixtures/eps_controls/`) and the ε-stability
  sweep in the post-parallel slot (Control S selftest first, fail-closed; R3 tripwires
  fatal-live/advisory-overlay). Deliberate-break controls witnessed for both.
- **Sweep tripwire for probe authors:** `drl_core:base_extractiveness/2` is multifile
  STATIC; `carbon_tax_2026`'s direct fact is clause 1 and an UNPINNED read backtracks past
  it to any matching solution — took-effect guards must `once/1` the read (witnessed: the
  unpinned guard "passed" under the shadow).
- **New corpus finding:** `unstable_off_grid` (final type flips under ε±0.02 with ε
  band-interior — χ-gate crossings) is the largest flag class on every leg (43/110 live,
  452/1106 kernel_v1): ε-sensitivity is mostly NOT ε-threshold proximity. Routed to
  OQ-78/OQ-48 consumers.
- **OQ-78 standing readout** (`python/epsilon_authorship_readout.py`, pipeline Phase 9c)
  reproduces the census exactly (0.68×46/110=41.8%, .x8/.x2 rail; flash on .x5/.x0).

---

## 2026-07-03 — OQ-205 spec landed: ε declaration discipline (provenance + stability), read-only census with control PASS
**Files:** docs/design/epsilon_declaration_discipline.md, docs/design/design_discipline.md, ISSUES.md, audits/2026-07-03_oq205_epsilon_census/
**Tier:** landed

Spec-only session (no engine change, no threshold change; Controls P/S pre-registered, NOT
run — running them would un-pre-register them). `docs/design/epsilon_declaration_discipline.md`
authored per the approved plan: disambiguation vs DP-001/OQ-26 (never title anything "ε
invariance"), provenance carrier recommendation `epsilon_provenance/5` (R2), read-site table
anchored at `6c59615e`, stability protocol with census-informed r = 0.02 (R3, two kill
conditions), commentary-grade flag disposition (R4), graduation criteria. OQ-205 → `partial`;
design_discipline §7 cross-pointer added (bidirectional refs same-commit).

**Census findings** (`audits/2026-07-03_oq205_epsilon_census/`, 4 legs, planted in-memory
control at snare_epsilon_floor+0.0005 PASS): (1) **testsets_flash authors ε exactly ON
classification thresholds** — 218/960 (22.7%) at distance 0.000 (its .x5/.x0 grid lands on
0.45/0.30/0.25/0.10); these are unstable at every radius by authoring convention. (2) The
(0.45, 0.46) open interval is EMPTY on all four legs — the tight-radius binding constraint is
moot on current corpora (re-check at regeneration; kill condition on R3). (3) OQ-78
re-baseline: live 0.68-mode share 41.8% (46/110); the last-digit rail is model-specific
(flash 5/0, haiku+live+kernel_v1 8/2). (4) Recon corrections to the plan: the ε threshold set
includes `mountain_extractiveness_max` 0.25 (plan omitted it); a SECOND fabrication fallback
found at `boltzmann_compliance.pl:248–252` (`BaseEps = 0.5`, OQ-89 class) beside the known
`constraint_indexing.pl:902–903` `Val = 0.0`; every story file authors ε TWICE
(`domain_priors:base_extractiveness/2` + `constraint_metric/3`) — silent-fork surface, spec
§3 requires the build to equality-check or declare canonical.

**Same-day ratification (operator):** R2–R4 RATIFIED with two spec-text amendments folded in —
three-site equality check (§3: `ValueAsWritten` is a third ε site, covered by the check or
it's a fork) and two-class stability flag (§5: `on_threshold_grid` vs `near_threshold`; both
block anchors, split is for the readout); R4 gained its promotion trigger (concealed flip
that mattered downstream ⇒ verdict-grade). Audit-dir tracking witnessed (`git ls-files`, 8
files in `a2a87dc5`).

---

## 2026-07-03 — OQ-138 FNL sub-part BUILT: RECLASSIFY→ROUTE landed (d248a6b1 + 82aa372e), consumers keyed on the lever, census type-inert was default-context-scoped
**Files:** prolog/signature_detection.pl, prolog/config.pl, prolog/config_schema.pl, prolog/abductive_helpers.pl, prolog/maxent_classifier.pl, ISSUES.md, audits/2026-07-02_oq138_fnl_evidence/
**Tier:** landed
The OQ-138 FNL CONVERT ruling's owed build, in two commits with the twin-diff hard gate between them (operator approved with one condition, folded in). U1 (`d248a6b1`, output-changing): `:925` overwrite → route behind NEW `false_natural_law_override_enabled` (0=route default, 1=legacy; schema spec added — config_schema.pl gate fails loud on a spec-less param); `fnl_routed/1` outcome-keyed (dr_type/3 non-circularity TRACED at HEAD: 152-pred closure, 3 positive controls); victim-discriminated severity (vic>0→moderate). U2 (`82aa372e`, wiring): seat_overrides + maxent boost keyed on the LEVER, a deliberate departure from the plan's `\+ fnl_routed` shape.
- **Tripwire-grade finding: `fnl_routed/1` (and `fcr_routed/1`-style seat predicates generally) are DEFAULT-CONTEXT-keyed while `resolve_modal_signature_conflict` overwrites are ORBIT-wide.** Witnessed: `organization_floor_c0` ("type-inert" in the census) routes tangled_rope→scaffold at the INSTITUTIONAL position while default-context unknown — the census's type-inert column was default-context-scoped only. Consumers that would lie under default-keying (probe_signature via seat_overrides; the PER-CONTEXT maxent boost — apply_signature_override fires at all 4 Wasserstein contexts) were therefore keyed on the lever: at lever=0 NO seat overwrites (typed seats route, unknown seats abstain), so override-liveness IS the lever state, orbit-safe. FCR reconciliation: FCR's non-routed seats keep their boost because `fcr_override_enabled` defaults 1 (override still LIVE there) — one rule, "boost mirrors live overwrite," two outcomes. **Re-open condition (the SPECIFIC kill, not the general fact):** default-keying is FINE for the grade/severity consumers (`converted_at_seat` → SigGrade/severity are default-headlined by architecture, like verdict_join itself); what trips this is a FUTURE consumer that reads `fnl_routed`/`*_routed` for ORBIT-SENSITIVE override-liveness (anything evaluated per-context or aggregated over the orbit — a maxent-style per-context injector, an orbit-walking exporter). Such a consumer must key on the lever (or a per-context predicate), never on the default-keyed seat predicate.
- **Twin diff (THE behavior witness, `FNL_CONVERSION_DIFF.md`):** 8/14 routed seats render RED (census predicted green→yellow — prediction vs measurement, the FSM lesson again): type_1_false_summit informational→severe on routed snare + **h1 0→3 / sheaf→manifest** — the overwrite applied at every context and flattened the whole orbit into a manufactured global section (pasted orbit: competence_occupation OLD tangled_rope×4 → NEW {snare,snare,scaffold,snare}). Determinism control NEW-vs-NEW2 0/960; OLD arm byte-identical to the pre-conversion canonical baseline; twin spillover (8+31 seats) all maxent/ensemble refit, zero signature/type/grade changes; live leg 0 verdict changes (89 records move in wasserstein/arakelov/signature_pressure only — one seat's orbit change re-centers corpus-relative ensembles).
- **Gates:** 5-corpus sweep routed 0/6/8/0/0 with routed∩piton=0 retained as positive control; per-context consumer probe (org_floor_c0@institutional = scaffold + no_boost + agrees, BOOST-CONTROL fires on every leg); two-sided ablation (lever=1 restores legacy at every context, incl. the legacy computed-but-unrendered override_mismatch at org_floor_c0 — proving route-mode's `agrees` is an improvement, not a hidden artifact); gate.sh GREEN.
- **Correction-key (pre-existing failures, attributed NOT-mine by identical failure at HEAD-files+lever=1):** validation_suite has 119 PASS + 1 FAIL (`lycurgan_laws__demographic_trap_reading` BCE interval 480>330). Two-axis dating of that fail: the interval fact is byte-identical since pilot_05 (`f4c7b13d`, 2026-06-13) and the file WAS in the 2026-06-21 suite that read 92/0/0 — so the interval-validity check entered the regenerated suite AFTER 06-21; the plan-era "92/0/0" denominator is the 06-21 corpus size (suite is auto-generated 1 unit/file; corpus grew 92→119 via topic runs). Open corpus-content question flagged, not fixed (needs a BCE-encoding ruling, other BCE stories may share it): should BCE intervals be authored as negative years? `test_agent_beneficiary` fails 35/94 (per-testset threshold/profile validation units on the current corpus — the "green" expectation in the plan was stale); `test_contradiction_signatures` same 5-name set as its known baseline. Cite these as baselines, not regressions.

---

## 2026-07-03 — OQ-87 twins characterization DONE (zero-spend): committer axis byte-stable, magnitude convention model-idiosyncratic, existence proof re-scoped to de-baited rate; OQ-208 minted (CA-2 split)
**Files:** ISSUES.md, audits/2026-07-03_oq87_twins_ca3/, prompts/constraint_story_generation_prompt_DRIFTNEUTRAL.md
**Tier:** landed
Pre-registered read-only run (PLAN.md committed before any arm; four serialized swipl runs: kernel_v1 906-pool / testsets 89 / haiku 960 / flash 960; seven controls all discharged incl. two kill conditions). Full record: `audits/2026-07-03_oq87_twins_ca3/FINDINGS.md`; commits `8ac24afc`→`e99ccaf5` + this landing.
- **Committer axis byte-stable across 26 days of observer-engine evolution:** banked (2026-06-07) vs HEAD `dfe10734` on the 906 pool = **0 committer-verdict flips vs 42 observer-bucket changes** (same differ read both columns — internal positive control). Anchor diverge-A 74→82 wholly observer-side (11 gained / 3 lost, all stayed `dead`; OQ-51 null-exclusion = 0). Theorem-7-consistent characterization, NOT a proof (bait-bearing substrate).
- **fired = grep-candidate exactly on all four corpora** (16/16, 129/129, 136/136, 18/18): the `cs_axiom_foreclosed` conjunction is file-locally decidable at HEAD — grep-candidate counts may be cited as fired counts *at this code state*.
- **Magnitude convention is model-idiosyncratic:** substantial-rate haiku 0.870 vs flash 0.505 (|Δ|=0.365). Foreclosure-shaped authoring (`axiom_overriding`+non-minor+unack) clusters ≈0.21 on the three Anthropic-era corpora (0.213/0.206/0.211) and collapses on the Gemini twin (0.027). Flash fired-core 18 < pre-registered floor 20 → flash-side and shared-core rates are DESCRIPTIVE-ONLY (flash-rate 1,067-story and shared-rate 2,133-pair sizing figures may NOT license a spend; only the haiku-rate 141 is citable). Conditioned direction agreement 0.734 vs chance 0.687 — near-chance cross-model content under bait.
- **Rulings (operator):** (1) FOLD-IN — OQ-87's proof limb `blocked_on OQ-75`, with the edge tracking the MEASUREMENT (a de-baited fired-core rate), not the rebuild artifact; **DRIFTNEUTRAL pin currently ABSENT repo-wide** (controlled grep; only banked audit scripts reference the prompt) — pin note added to OQ-75; ~150-story pilot recorded as the sooner-option, trigger = a named downstream forcing function. (2) CA-2 SPLIT → **OQ-208** (Priority 3, `splits_from OQ-87`), Deps authored with BOTH exit branches: construct a framing-sensitive positive control OR prove none can exist (negative-by-construction close admitted; code-level branch (b) first, near-zero spend).
- **Tripwire (probe adaptation, carried in the audit dir):** the banked ca3 probes bucket `H0==1 else incoherent` — post-OQ-51 that silently misbuckets `H0=null` as incoherent (77/91 stories per twin are undetermined). Any reuse of pre-2026-06-25 H0-consuming probes needs the 3-way bucket.

---

## 2026-07-02 — Four blocked_on_human rulings landed: OQ-138 (CI-rope KEEP+close, FNL CONVERT), OQ-193 (giant_comp additive-split), OQ-75 (Stage-2 scoped-go)
**Files:** ISSUES.md, audits/2026-06-21_oq138_fsm_route_conversion/CIROPE_RED_ADJUDICATION.md, audits/2026-07-02_oq138_fnl_evidence/, audits/2026-07-02_oq193_giant_comp_ruling/, audits/2026-07-02_oq75_stage2_preflight/
**Tier:** landed
Witness-gathering + rulings for four blocked items (probes read-only w.r.t. engine substrate — reversible corpus overlays via `retractall+assertz`, verified restore, per-probe positive controls). No engine behavior changed; two CONVERT/build obligations recorded in ISSUES.md, not started.
- **OQ-138 CI-rope route-purity — RULED KEEP-as-written, limb CLOSED.** 5 rope-consumers re-witnessed at HEAD. Inherited neutron_star RED sub-item RESOLVED MOOT: at HEAD neither neutron_star nor superheavy is RED (OQ-128 discriminated severity + FCR-9 conversion each independently removed the cap). superheavy is a DOCUMENTED FCR-inert seat (CONSTRUCTED3_FINDINGS.md:21; 0-hit in FCR9_live_diff), verdict-absent because unknown-surfaced — absence discriminated by neutron_star's present verdict in the same dump. Kill condition stays live. Witness: `CIROPE_RED_ADJUDICATION.md`.
- **OQ-138 false_natural_law — WITNESSED + RULED CONVERT (build OWED, not started).** 4-leg census (testsets 1 inert / haiku 13, 6 changed / flash 8, 8 changed / kernel_v1 0). The 14 type-changers repeat the FSM/FCR shape (scaffold/snare→tangled_rope, green→yellow unmask, correction grade, claim+vic discriminant). ALL 22 firings source-1 explicit_mountain_claim, ZERO source-2 (OQ-70 fix holds). kernel_v1=0 is measured-empty (41 claims × 973 non-compliant, intersection 0). Both census + diff positive controls passed. Build owed: conversion + 5-corpus sweep + abductive_helpers/maxent consumer fixes. Near-free on the live leg. Witness: `audits/2026-07-02_oq138_fnl_evidence/FNL_EVIDENCE.md`.
- **OQ-193 giant_comp — RULED (c) additive provenance split (topology ruling, report-build OWED).** 3-leg ripple confirmed at HEAD (giant 12→9 / 549→47 / 334→70). Per-consumer price: FPN NO-DIFF (OQ-23 guard already zeroes sibling contamination — two-sided controlled: planted cross-kernel strip DOES move purity on testsets; haiku vacuous-but-consistent); json_report/network_dynamics/severity DO change (15/282 hub flips). Headline has zero downstream consumers. (c) = siblings stay in topology for all 5 consumers + giant_comp reports both pooled & cross-kernel counts. NOT zero-cost — it rules siblings intended topology. Witness: `audits/2026-07-02_oq193_giant_comp_ruling/RULING_EVIDENCE.md`.
- **OQ-75 Stage-2 — RULED SCOPED GO (a).** Part (a) diff-distribution authorized; part (b) cross-axis correlation (the OQ's headline staked falsifier) stays UNTESTED (standalone build; OQ-15 resolved 2026-06-24 so NOT gated on a mediator layer). Construction-pair stratum N/A this cohort (twins carry 0 flat_control facts vs testsets' 10; recorded in OQ-76). Preconditions before citable numbers: build the prevalence counter (harness-reuse extension of `oq49_override_remeasure.py` — confirm it counts prevalence not override-firing) + clean-tree twin reclassify (both twin manifests code_dirty). Witness: `audits/2026-07-02_oq75_stage2_preflight/PREFLIGHT.md`.
- **Correction-key:** two exploration-record errors corrected in the OQ-75 preflight — OQ-15 is RESOLVED (2026-06-24, `279d7c24`) not open; and a `false_*`/`dr_claim_mismatch` prevalence counter does NOT exist from scratch but CAN be built as an extension of `oq49_override_remeasure.py` (which counts override firing, not prevalence — confirm before citing the cost as cheap).

## 2026-07-02 — OQ-126 RESOLVED: drift terminal carries its authored-ack provenance (witness-not-verdict); external-anchoring tier ladder promoted to design_discipline.md §10
**Files:** prolog/json_report.pl, prolog/cs_drift_engine.pl, prolog/tests/test_cs_drift_engine.pl, python/shared/schemas.py, python/enhanced_report.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

Gap 1 (the cyclopean shape: the engine consumed the AUTHORED `Acknowledged` bit in
`cs_drift_state` gap/3 as if it settled the seated honor-vs-reabsorb verdict) fixed as
provenance labeling, commit `ee51cdff` — Pass-0 gate re-witnessed the scoping claim first
(`cs_terminal_attractor/4` internal to cs_drift_engine.pl; all 5 `cs_drift_trajectory/3`
production consumers commentary-grade; no path to `classify_from_metrics/6`; HALT clause
unused). New fields at every terminal surface incl. the no-CS-UID default branch (that branch
was MISSED on the first edited run — 30/119 entries lacked the fields; test w3 now pins it):
`cs_drift_terminal_basis: "authored_ack"|null` and `cs_drift_ack_witness {authored,
acknowledged, confrontation_path, confronted_by}` where `confrontation_path: "none_exists"`
is a NO-PATH sentinel (no external instrument exists; OQ-107 `future`), NOT "checked, none
found" — operator null-semantics rider. `enhanced_report.py` renders the terminal conditional
("if authored acknowledgment taken at face value") — the decoration kill-condition control;
before/after panel diff witnessed on `ability_ceiling_reading`. RED control witnessed (both
w-tests FAILED with emission dropped, then restored green 24/24; test_cs_trifurcation 19/19 —
OQ-55 single-bit twin untouched). Scale: clean-vs-edited diff n=119 additive-only, 0
pre-existing value changes, warning sets byte-identical (1,428 pre-existing `fingerprint_shift`/
`repair_transitions` warnings — pre-existing condition, not this change's); twins
`testsets_haiku` n=960 + `testsets_flash` n=960 at `5d6f219`-dirty, 0 missing/unfaithful.
Item (c): tier ladder (Tier 1 external/dated / Tier 2 retained-record + NORMATIVE declared
record-boundary MUST / Tier 3 no temporal handle / declared stop) promoted from the OQ entry
into `design_discipline.md` §10. OQ-126 compressed-on-close; stale cross-refs corrected
(OQ-74 was cited as "pending ruling" — resolved 2026-06-14). Ω_P core (honor/reabsorb seated,
never engine-certifiable) closed as DECLARED, not solved.

## 2026-07-02 — OQ-195 RESOLVED: general-n H¹ gap spectrum proven at every cardinality; stakeholder frame makes it the live law; OQ-207 minted
**Files:** docs/h1_gap_spectrum_general_n.md, python/audits/oq195_h1_spectrum_check.py, prolog/tests/test_h1_spectrum.pl, prolog/grothendieck_cohomology.pl, docs/deferential_realism_paper_v8.md, docs/deferential_realism_paper_v7.md, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, audits/2026-07-02_oq195_general_n_gap/
**Tier:** landed

New proof doc `docs/h1_gap_spectrum_general_n.md` (commit `5d052990` + close commit): min
nonzero H¹ = n−1 at every cardinality ({1..n−2} forbidden — the four-seat forbidden-{1,2} is
the n=4 instance); exact band decomposition by largest agreement bloc with a self-similar
recursion; unconditional band-floor lemma; inter-band gap iff n ≥ j+3+C(j+1,2) (every value
in the gap forbidden); type-token bound T=7 (derived from code) truncating the top for n>7 —
LIVE, not hypothetical: the operator's pre-check surfaced the stakeholder frame (named seats,
roles {agenda_setter, beneficiary, payer, excluded, observer}, per-seat computed types) at
3–12 seats/story across the live legs (kernel_v1: zero — the frame post-dates it). Verified
under pre-registered BLOCKING criteria n≤40 with PER-BAND bookkeeping — the plan review
caught that the band union is invariant under dropping the parts-constraint, so a union
check cannot verify the classification; the unconstrained classifier ran as a discriminating
control (unions identical ∀n, bands mismatch 38/39). Engine witness `test_h1_spectrum.pl`
23/23 (exhaustive n=2–4, constructive n=5–12, OQ-51 filter at n=12, two negative controls).
Adversarial multi-agent review was blocked by a session subagent limit — substituted by the
operator's hand-derivation + enumeration + an author re-derivation pass that caught one
prose defect (B_{j+1} is the band FLOOR, exact minimum only for j+1 ≤ n−j−1) — recorded in
the WRITEUP. Propagated: v8 §3.4/§9.6/Appendix; v7 dated amendment note (band values are
seat-count-conditioned); v6.13.1 changelog item-6 landed-pointer; `grothendieck_cohomology.pl`
both range comments (comment-only, behavior-preserving). **Line-drift correction-key:** the
stale-range flag cited repo-wide as `grothendieck_cohomology.pl:158` actually lived at
ll.167–182 — cite it by predicate header, never by that line. **OQ-207 minted** (stakeholder-
frame H¹ build: `dr_type_for_stakeholder/3` vectors → `obstruction_from_vector/3`;
commentary-grade; registry-registered; `consensus_provenance/2` is its H¹=0 special case).

---

## 2026-07-02 — OQ-70 premise-rot correction: canon said "until ruled" for 27 days after the ruling; v8/README inherited it on authoring day
**Files:** CLAUDE.md, README.md, docs/deferential_realism_paper_v8.md, ISSUES.md
**Tier:** correction-key

Found while ranking the frontier: OQ-70 (FNL bait confound) was RESOLVED 2026-06-05 (option A
class ruling, bait clauses removed at `72ec2cdd`, detector-intact positive control), but
CLAUDE.md Critical Distinctions kept the pre-ruling "Until OQ-70 is ruled" framing in present
tense — and the v8 paper (§9.4/§9.6/Appendix) and README, authored 2026-07-01/02, inherited it
from CLAUDE.md the day they were written. **The corrected canon (all surfaces now derive from
it):** OQ-70 resolved; what survives permanently is (a) pre-reset/archive prevalence is
regime-bound (authoring convention, never detection), (b) live prevalence is citable only as a
CLAIMS statistic, (c) statistics reset twice (2026-06-05 class fix; 2026-06-11 example cutover
— discount per `audits/2026-06-11_oq109_phase_b/EXAMPLE_INHERITED_SIGNATURES.md`). Engine
re-witnessed correct at HEAD before any edit (`signature_detection.pl:1081–1096` source-2
RULED-OUT + explicit-claim clause present; `:1404–1422` sibling likewise). Corrected: CLAUDE.md
FNL block (present-tense bait description → past/regime-scoped + canon), v8 §9.4 (resolved +
surviving prohibition), §9.6 + Appendix open-lists → {OQ-195, OQ-205}, README open-obligations
likewise, MEMORY.md hook. **Downstream unblock:** OQ-138's `false_natural_law` member was
deferred "pending OQ-70" 12 days AFTER the close (stale premise at authoring; no residual limb
exists in the OQ-70 body) — gate-expiry annotation added, member now rulable-once-witnessed
(ruling stays in the OQ-138 blocked_on_human queue); the parked `neutron_star`/FCR RED
adjudication (orphaned "under OQ-70") re-homed into OQ-138's route-purity limb. Probe note:
the stale-phrase grep needed a wrap-proof pattern ("OQ-70 is ruled", not "Until OQ-70 is
ruled") — the v8 instances line-wrapped and the first probe missed them; control caught it.

---

## 2026-07-02 — OQ-135 RESOLVED: v8 adopted (seat/gauge/orientation); v8 paper authored; README/CLAUDE.md refreshed; vocabulary migration wave
**Files:** docs/deferential_realism_paper_v8.md, README.md, CLAUDE.md, AGENTS.md, ISSUES.md, docs/seat-theorem-v1.md, docs/one_seat_audited.md, docs/design/design_discipline.md, docs/metrics_as_routing.md, docs/technical/paper_versioning.md, docs/v8/foundations/README.md, docs/logic.md, docs/logic_thresholds.md
**Tier:** landed

Operator ruled v8 adoption + spec Q4 **wholesale** (plan approval, 2026-07-02). Shipped in
four phased commits: `4ea2c2d5` the v8 paper (77KB, entry point + canonical vocabulary; §5.4
bridge table; §5.7 kill-condition; Theorem 2 |real-seat| caveat in-body → OQ-195; §6.4 ε
declaration discipline handed forward); `16143c15` review-response Appendix (operator ruling
after multi-model review: adopt only the Perplexity point — a clean current-state statement at
document end, no terminology-evolution baggage; other suggestions filtered as LLM-bias);
`7c4cca6f` README rewrite (all quantitative claims re-witnessed on disk same session);
`64a44514` CLAUDE.md what-this-repo-is + canonical-paper pointer (v8 entry point; v7/v6.13.1
stay the detailed records — v7 §-references elsewhere in CLAUDE.md remain valid). Phase-4
commit: OQ-135 close (dead-hash note: `fd1ee561` does not resolve; guard cited by
artifact/gate/audit), OQ-03 03b unblocked, OQ-195 propagation update, migration notes in the
five named docs, memory sweep (2 files), foundations README `core_v4.2`→`core_v4.3` link fix
(3 occurrences). Verification witnessed at each commit (obligations grep-checklist; two-seat
sweep judged per-hit; fresh-agent self-containedness control 7/7, its confusion list applied).

**Near-fork DECLARED (Pattern 2 flag, not resolved): `docs/v8/foundations/` is source
material; `docs/` + `config.pl` stay canonical.** Survey (2026-07-02): 8 files are byte-identical
copies of live docs (incl. `core_v4.3.md`, `debugging_philosophy.md`, `metrics_as_routing.md`;
`prolog_v6.8.md` = `deferential_realism_paper_v6.8.md` renamed); 4 are STALE pre-April
snapshots of live docs (`logic.md`, `logic_extensions.md`, `logic_thresholds.md`,
`omega_variables.md` — cite docs/ for current claims); 11 have no repo counterpart by filename
(the v4.x domain suite); and `deferential_realism_paper_v6.9.md` there is ~12KB LARGER than
docs/' copy (carries a related-literature section absent from the live file — which v6.9 is
"the" v6.9 is unresolved). foundations' seven-category framing (incl. "Naturalized" as a
category, internally contradicted at core_v4.3.md:46,117) is historical; the live taxonomy is
six types + naturalized as cascade outcome (v8 §3.3).

**Mojibake REPAIRED (operator ruled fix, 2026-07-02) — and the history corrects two guesses.**
Scope was far larger than the flagged 132 `Ï‡`: `docs/logic.md` carried **1,791** mojibake
sequences across 79 distinct patterns (→ — ε χ × § ≤ ✅ ∧ …), and `docs/logic_thresholds.md`
carried **172** more (same disease, found by sweep). History evidence (per-revision `Ï‡`
counts): the count sat at 127–135 from ≥2026-02-15 through HEAD across every edit — so this
was NOT reintroduced by recent edits (operator's guess) and NOT a regression since March (this
entry's earlier framing): the Feb-2026 "repair" noted in CLAUDE.md was partial, and the
mojibake persisted continuously. Corollary: the clean `docs/v8/foundations/logic.md` is a
separately-CLEANED variant, not a byte snapshot of the repo file. Repair method
(scratchpad `moji_fix.py`, 5 positive controls passed pre-write: repairs-known / clean-untouched /
idempotent / C1-fallback / mixed-run-splits): per-run cp1252 round-trip with C1 fallback, plus
5 hand mappings for truncated sequences whose third byte a later quote/space normalization
destroyed (`â€"`→—, `â†"`→↔, `â†'`→→, `âœ"`→✔, `â¤ |`→`⤠|`, each context-verified against the
clean variant). Witnesses: residual audit ZERO suspicious runs in both files; diff balanced
1,127/1,127 lines, encoding-only; the pre-March rope-bypass line now byte-identical to the
clean variant's. One spec-content oddity surfaced, NOT decided in the encoding pass: the
rope-gate bypass symbol is `⤠` (U+2920) in both the repo file and the clean variant — likely
an ancient corruption of `⊤` (vacuous-true) predating all snapshots; cosmetic (the engine
implements the OQ-01 bypass regardless), flag only.

---

## 2026-07-02 — OQ-137 RESOLVED (reading registry + totality suite + pipeline gate + sweep fixes); OQ-136 evidence in (haiku/contradictions authoring artifact vs genuine mcc)
**Files:** prolog/reading_registry.pl, prolog/tests/test_reading_totality.pl, prolog/commentary_census.pl, prolog/signature_detection.pl, prolog/report_generator.pl, prolog/cs_drift_engine.pl, prolog/cs_axiom_engine.pl, prolog/tests/test_cs_drift_engine.pl, python/run_pipeline.py, python/audits/oq136_bucket_provenance.py, audits/2026-07-02_oq136_census_bucket_provenance/, audits/2026-07-02_oq137_reading_totality/
**Tier:** landed

**OQ-137 close (Phase 5+6, commits `486756fe`/`ed851eb7`+gate):** 41 predicates classified
(classification_table.md); defects fixed: explain_signature missing `unknown` clause → the
report signature section silently TRUNCATED on one claim-authoring unknown-signature constraint
(planted witness 0/110 → 111/111; latent on live corpus — contradictions files author no claims);
cs_terminal_attractor overlapping rows (dup + order-dependent terminals; row-disjoint, first
solution preserved on all 42 combos); cs_has_axioms/cs_axiom_inconsistent doc key +C→+UID
(constraint-name key never fires silently). test_cs_drift_engine was RED since the corpus reset
(fixtures deleted) — rebuilt self-contained, 11/11. **OQ-136 RULINGS EXECUTED (operator,
2026-07-02, post-review):** R1/R2+R6 → OQ-202 minted (ONE generation OQ: haiku +
contradictions paths under-emit stakeholders[]/founding_problem_status; contradictions also
stamps no story_provenance — folded, same path one witness); R4 → OQ-203 (excluded-role
evidential-vs-structural vocabulary, standalone) + OQ-204 (mcc first-class reporting GO, with
candidate-semantics + denominator-caveat design constraints); R3 q6_signature_unknown kept with
a ONE-LEGGED caveat written into WRITEUP + close (statistical leg only — the pre-registered
hand-read leg was not run for that bucket); R5 kept; census `no_agent_seats` out-of-domain
declaration RATIFIED (provisional stamp replaced). OQ-136 → resolved (compress-on-close;
denominator caveat kept intact as still-operative). **Standing guard: `run_pipeline.py`
`_phase_prolog` opens with the reading-totality suite as a sequential fail-fast gate** (first
plunit gate in the pipeline; adds one corpus-loading swipl run to each pipeline invocation);
wiring control witnessed: planted broken registry entry → SystemExit red; clean → green;
per_constraint byte-identical. **Tripwires for future instances:** (1) `[C]-m:g(...)` and
`V^m:g(...)` parse WRONG (`:` is priority 600, looser than `-`/`^`) — parenthesize `(m:g(...))`
in templates/setof; the first OQ-137 sweep passed VACUOUSLY on this until its planted controls
caught it. (2) When adding a reading predicate an aggregate could consume, register it in
`reading_registry.pl` in the same change — registration is opt-in (named residual risk).

**OQ-137 slice (commits `a81d4c83` behavior-preserving + `2453b922` output-changing):**
`reading_registry.pl` (`aggregatable_reading/3`: 5 proven-total seeds + `in_contention/3`
partial-by-design; `census_source_backing/2` anti-fork bridge) + registry-driven
`test_reading_totality.pl` (exactly-one over declared domain; two positive controls — planted
silent stub flagged AT the hole, two-sided). `commentary_source(consensus)` added: compound
`manufactured_consensus_candidate(Excl)` flattened to functor; `no_agent_seats` out-of-domain
declared PROVISIONAL (source comment — it pre-judges the OQ-136 bucket); `seats_untyped` absence;
no prevalence bucket (candidate flag ≠ positive finding). Witness: suites 10/40/8 green;
run_pipeline exit 0 + mtimes advanced; census diff additive-only; per_constraint byte-identical.

**OQ-136 (pre-registered; PROPOSAL frozen `0ba48b4c` BEFORE the join; execution `2b66dedc`):**
q6_unmeasured (26) + no_agent_seats (26) cluster on model AND prompt_commit (p_holm=8e-4;
haiku 16/28 + all 9 `*_contradictions`; 25/26 overlap) = ONE generation-path artifact — haiku
path authors prose + constraint_beneficiary but NO founding_problem_status / stakeholders[]
(prose plans the seats it never emits); contradictions path also stamps NO story_provenance/8.
q6_signature_unknown (16) + manufactured_consensus_candidate (9): NOT clustered; mcc hand-read
8/9 genuine (1/9 radiative_levitation false-positive by its own text → excluded-role
evidential-vs-structural vocabulary gap). extraction_unnameable (3): compound (seat limb =
haiku artifact; victim limb genuine-to-the-reading 2/3 RULED). **Dispositions R1–R6 are
blocked_on_human** (ISSUES.md OQ-136; proposals in the audit WRITEUP.md).

## 2026-07-02 — Cross-leg check: OQ-52 replicates member-level; OQ-45's phenomenon recurs via DISJOINT members (draw-variance); live-leg hidden-winner exists
**Files:** audits/2026-07-01_oq45_oq52_hidden_winners/, prolog/testsets_haiku/temple_sacrifice_commitment__performance_only.pl
**Tier:** landed

Operator-requested check of the 2026-07-01 closes against `testsets_*` + `kernel_v1` (B5 section
of the audit WRITEUP). NL populations under the pre-fix cascade: kernel_v1=**26 (matches the
recorded 2026-06-10 matrix — aggregate control PASS)**, haiku=8, flash=5, live=0; all PRE/MID
dispatch controls PASS; twins classified via `classify_corpus` with the model-fingerprint gate
(`pipeline_output_{haiku,flash}.json`, n=960 each). All 39 twin+kernel NL members content-read
exhaustively (rubric v2): kernel_v1 → 4 hidden-winner (all social constraints; quotes verified
5/5), haiku → 0 (two reader calls downgraded on adjudication: gain lived in sibling readings the
file excludes), flash → **1 hidden-winner on a LIVE leg** (`temple_sacrifice_commitment__
performance_only`, "Messianic restorationists are beneficiaries" beside a Mountain claim). OQ-52's
authored-channel finding replicates at 100% on every live leg (haiku 113/113, flash 83/83, live
8/8). Two NL-gate coarseness data recorded in the OQ-45 addendum: victim-bearing stories certify
(gate checks beneficiaries only); the 404 h1=4 uniformity is an original_v6 template artifact
(twins mix h1∈{0,4}). Draw-variance witnessed (OQ-26): article_27/aneyoshi kernels read
differently across twins — distinct draws, not re-measurements. CITATION AMBIGUITY WITNESSED: 'HEAD yields strict=235' was read as a canonical-corpus count
when it was the HEAD *engine* on kernel_v1 (one computation; live n=119 leg: manifest=71,
strict=4, loose=4) — when citing counts across classify_corpus runs, name BOTH the corpus and
the code state (rule promoted to CLAUDE.md Running the System). VERB DISCIPLINE: only OQ-52
replicated member-level; OQ-45's twin/kernel hits are disjoint from the six 404 hits, the
EXPECTED consequence of new draws — the phenomenon recurred, no member-level replication is
claimed or possible.


## 2026-07-01 — OQ-45 RESOLVED (YES: hidden winners in the 404) + OQ-52 RESOLVED (W1 leg delivered; population counts are engine-regime-relative)
**Files:** python/w1_sheaf_join.py, prolog/signature_detection.pl, audits/2026-07-01_oq45_oq52_hidden_winners/
**Tier:** correction-key

Both OQs closed as the presents-as-natural / hidden-winner pair (OQ-52 = beneficiary-AUTHORED
side, OQ-45 = beneficiary-SILENT side; NOT exhaustive — a hidden-winner neither
false-mountain-shaped nor NL-certified falls through both). Full evidence + writeup:
`audits/2026-07-01_oq45_oq52_hidden_winners/WRITEUP.md`. Branch `oq45-oq52-hidden-winners`.

**Citation corrections (why correction-key):**
1. **The OQ-52 "16 of 98" false-mountain count is engine-regime-relative — do not cite it as a
   current fact.** On HEAD, kernel_v1 yields strict=235 + loose=58 of manifest_presheaf=944 (the
   944 matches the OQ-197 acceptance controls, `34ff919f`). Member-level assignments were the
   stable part: all 5 recorded names recover with EXACT H1 (quran=4, article_9=5, abrahamic=6).
   The original 16-list was never saved and is not reconstructible. General lesson: save member
   LISTS, not counts, when a selection is engine-computed.
2. **The "all false-mountain rows carry both authored channels" claim is now 289/293:** 4
   victim-only rows exist at HEAD (repair sentinels screened, 0/1106). Cite the 2026-07-01
   re-measure, not the 2026-06-02 absolute.
3. **A naive NL sweep on HEAD returns 0 everywhere** (has_viable_alternatives dead-by-range,
   `8b5a34b8`/OQ-113). The 404 population is recoverable ONLY via the pre-fix overlay swap —
   recipe + controls in `b1_nl404_probe.pl` (PRE=unknown / MID=false dispatch controls,
   Sig-UNBOUND sweep; aggregate control: count==404 PASS).
4. **OQ-45 answer is YES, per-story only — no prevalence claims** (chimera corpus, OQ-70/OQ-25).
   6 hidden-winner (spot-verified quotes): bucket (i) extraction wearing the mountain frame
   (`repeat_player_structural_advantage`, `demographic_elimination_imperative`,
   `attention_as_capturable_resource`, `capability_compulsion_gradient` borderline); bucket (ii)
   genuine natural core with unauthored ecosystem winner (`gilgamesh_mortality_limit`,
   `ecological_carrying_capacity`). Bucket (ii) is the design note for any future NL re-powering
   (GAP-08 §7): gain-AROUND-persistence ≠ gain-from-authoring.

**Landed:** `w1_sheaf_join.py` rows now carry `incomparable_mass` + `material`
(W1_MATERIAL_PROVISIONAL=0.05, a LABEL freezing OQ-51's "~0.05" prose gate, never a filter);
stale "unknown=N/A declared but unbuilt" prose retired (`e8189d10`; column-diff witnessed
behavior-preserving). `classify_corpus` precedent: kernel_v1 (n=1106) and original_v6 (n=3380)
classified into own manifest-bearing outputs without touching the canonical artifact.

**Method note (rubric control):** the B3 content-rubric v1 pre-flight FAILED 0/3 on known
false-mountains (their prose contests naturalness in narrator voice); v2 (in-frame naturalness
counts) passed 3/3. A content rubric is an instrument — pre-flight it on known positives before
reading the target population, or a 0-flagged read is unfalsified.


## 2026-07-01 — OQ-41 RESOLVED (row-26 five-site expansion) + OQ-40 RESOLVED (doc lift) + OQ-201 minted (row-22 spin-out)
**Files:** ISSUES.md, docs/design/two_axis_architecture_v7.md, audits/2026-07-01_oq41_row26_expansion/, prolog/signature_detection.pl, prolog/drl_fpn.pl, prolog/covering_analysis.pl, prolog/gap_diagnostic.pl, prolog/omega1_audit.pl
**Tier:** landed
Row-26 five-site expansion (HEAD `27afde7a`; no engine change — behavior-preserving; gate GREEN,
validation 0 errors). Step-0 grep: all 2026-06-24 cites exact at HEAD (no drift). Two flagged unknowns
resolved against substrate: **(1) `drl_fpn:197` is a sentinel pass-through (`IP<0.0 -> NewEP=IP`, `IP=-1.0`
when `fpn_intrinsic` absent), NOT a fabricated default → CARVED OUT of row-26**, no verdict assigned; the
prior entry had conflated `:197`'s label with `:206`'s trigger. **(2) `covering:490`'s `0.5` branch is a
`constraint_metric`-presence guard, NOT "interpolation off-grid" — the plan's off-grid trigger-class had
zero members.** Verdicts: `covering:490`, `gap:120`, `omega1:102` (+ Supp/Theater sibs) = **DORMANT/LOCKED**
(reject-guard + must-fire control fires + 0 pipeline callers; OQ-44 once-for-class); `drl_fpn:206`
`Immunity=0.5` = **NEUTRAL-by-corpus (cosmetic-if-fired)** — firing-marker patch shows 0 natural fires over
testsets/(119) AND kernel_v1(1106), positive control fires (measured-empty, not didn't-look), sink is
`fpn_ep`→diagnostic only, never `dr_type`. OQ-40 rows 19–20 split RULED-INTENDED lifted into
`two_axis_architecture_v7.md` §"Representation grounding" (`constraint_metric/3`=scalar/observer,
`measurement/5`=temporal/committer). Row-22 → **OQ-201**: `compute_temporal_stability` reads the scalar
store not `measurement/5`; coverage witness — folded metric=`suppression_requirement`, 107/110 (testsets) &
934/1106 (kernel_v1) reach-the-gate constraints author an ignored temporal series (SUBSTANTIAL → repoint is
eventual fix, deferred per off-grid trap), and **>1 scalar level = 0 on both corpora → variance path dead,
gate is a degenerate presence-check**. Positive control catches a known series on both corpora.

## 2026-07-01 — R4 RULED → OQ-200: detector_calibration carried as corpus-level OQ, NOT wired; module now TRACKED-but-unwired
**Files:** ISSUES.md, docs/design/detector_calibration_omega_proposal.md, prolog/detector_calibration.pl, audits/2026-07-01_oq197_r4_recompute/
**Tier:** tripwire

Operator ruling closing R4 of the detector_calibration proposal. After OQ-197 unblocked the baseline and the R4
recompute retracted the ~3× inflation (net-new = **39/41 determinable**, real undetermined-inflation only 4/12),
the per-firing + per-constraint diversity measurement showed the net-new is **low-KIND-entropy** (5–6 distinct
`(Class, author→engine)` signatures, ~90% two directional patterns). Decomposition: **false-summit re-surface**
(`mountain→tangled_rope`, 13/8 constraints = OQ-70/FNL through the author-engine axis, not genuinely new) + a
**`tangled_rope→rope` author-over-claims-contestation residual** (21/27 constraints, the constraint-majority and
the module's genuinely-distinct signal) + a small severity/singleton tail. Volume-vs-breadth: per-SEAT-firing
false-summit dominates (loud-narrow); per-CONSTRAINT `tangled_rope→rope` dominates (quiet-broad) — both correct,
different denominators. **Ruling: carry as an aggregate corpus-level OQ (OQ-200), do NOT wire per-constraint** —
39 near-repetitive firings each carrying the identical "calibration open" caveat is a query, not 39 findings. The
binding reporting condition (same as OQ-199 for the gap omega): firings are "author↔engine directional
disagreement, calibration open (Ω_E), FP-rate unset (Ω_P)," NEVER "miscalibration detected."

**TRIPWIRE — `prolog/detector_calibration.pl` is now TRACKED but UNWIRED (supersedes the UNTRACKED tripwire
below).** Committed this session as reference implementation; loaded by nothing, wired into no report. The
question it computes is carried at corpus scope by OQ-200. Do NOT wire it (into `run_pipeline`, any report, or
via `use_module`) without REOPENING R4 — that needs an external calibration answer (Ω_E) + an accepted FP-rate
(Ω_P). The committed `already_covered/1` behavior (undetermined-aware post OQ-197) is measurement-only.

---

## 2026-07-01 — gate check added: human gap surfaces must distinguish no_gap from undetermined (Pattern-6 guard)
**Files:** scripts/gate.sh, python/check_gap_status_surfaces.py, python/query.py
**Tier:** tripwire

New `scripts/gate.sh` check `gap surfaces` (`check_gap_status_surfaces.py`): a paired synthetic no_gap/
undetermined fixture asserting the three human-facing gap renderers (`tensions_ledger.build_block`,
`enhanced_report.build_omega_section`, `query.format_gaps_block`) produce DISTINGUISHABLE, correctly-labeled
text — converting the Pattern-6-downstream class (undetermined reading as "no finding") from "caught if someone
asks" to "fails red". Motivated by the enhanced_report catch running on a passing question, not a control, on the
highest-propagation surface; this bug recurred at 4–5 sites in the OQ-197 work, each caught by attention. The check
is positive-controlled (embedded self-test + external monkeypatch both confirm it goes RED on a collapsing renderer,
so it is not vacuous). **When you add a NEW human-facing surface that renders gap/omega state, add it to this check's
renderer list** — otherwise the guard silently under-covers (its own Pattern-1 risk). `query.py` gap block was
extracted to `format_gaps_block/1` for testability.

---

## 2026-07-01 — OQ-197 ruling (a) bound to OQ-199 reporting-condition; R4 recompute retracts the ~3× inflation
**Files:** ISSUES.md, audits/2026-07-01_oq197_r4_recompute/, prolog/detector_calibration.pl
**Tier:** correction-key

Ruling (a) (keep stakeholder source) finalized as non-redundancy-established / reliability-UNRESOLVED — bound to
OQ-199 as a BINDING reporting condition: while OQ-199 open, (a)-sourced gap-omega firings are reported as
"authored-stakeholder disagreement," never "validated cover-story detection." **R4 recompute done READ-ONLY**
(inverting the circular hold — proposal ruling was starved for the number R4 produces; loaded detector_calibration.pl
read-only, no wire/commit). On the fixed detector guarding on undetermined: net-new = **39/41 determinable** (not
14/12, not 43/53); genuine undetermined-inflation = only **4/12** (≈1.1–1.3×). **The "~3× inflation" is RETRACTED** —
it mislabeled the no_gap bucket (25/29: extraction_blindness examined-and-CLEARED, detector_calibration adds a distinct
author-vs-engine axis) as artifact, the same no_gap↔undetermined conflation OQ-197 fixed. Consequence: the
detector_calibration wire/no-wire proposal ruling can no longer rest on inflation/redundancy (net-new is substantial);
it now turns on the module's own open axes (calibrated? — Ω_E; acceptable FP rate? — Ω_P). Ruling stays operator's seat,
now fed a witnessed number. Do NOT cite the old 14/12-genuine or 3×-inflation forward — superseded by 39/41 + 4/12.

---

## 2026-07-01 — OQ-197 acceptance controls PASS (kernel_v1 944 + twins 29/41 reproduced from substrate); case-(ii) refinement
**Files:** audits/2026-07-01_oq197_acceptance_controls/, prolog/report_generator.pl
**Tier:** landed

The graduation witness for the OQ-197 chain. Counts reproduced from substrate, not the doc:
kernel_v1 canonical-varying=944 exactly (stakeholder_facts=0); twin detector_calibration net-new=43/53 and
net-new ∩ stakeholders-present ∩ detect_gap_pattern-fails=29/41 exactly (read-only load of untracked
detector_calibration.pl). **Case (i):** 944 read undetermined(no_seats) under source (a), never silent 0;
negative control same run — canonical (b) discriminates gap=944/no_gap=152. **Case (ii) REFINEMENT:** the
29/41 are NOT uniformly undetermined — three-valued split is haiku 4 undetermined + 25 no_gap, flash 12 + 29.
The 4/12 (<2 power positions) are the genuinely-inexaminable false-`[]` rescues → now undetermined; the 25/29
have ≥2 seats spanning ≥2 powers agreeing → genuine no_gap. None silent 0 (all labeled). The doc's premise
that the 29/41 were uniformly "insufficient" was imprecise — the fix is more precise. Negative control same
run — source (a) produces gap+no_gap+undetermined on both twins. OQ-197 fix witnessed end-to-end; only (5) R4
recompute remains, held on the detector_calibration proposal ruling.

---

## 2026-07-01 — OQ-197 consumer wiring landed (4 live sites, labeled); detector_calibration.pl is UNTRACKED/unwired WIP
**Files:** prolog/json_report.pl, python/shared/schemas.py, python/query.py, python/tensions_ledger.py, prolog/detector_calibration.pl
**Tier:** tripwire

Commit `fffca9d1`. Wired the OQ-197 three-valued `gap_status` through every LIVE read site so undetermined
never collapses into measured-no-gap (Pattern 6), carrying the human-readable LABEL not just the internal
representation: json_report per-constraint `"gap_status"`+`"gap_undetermined_reason"` (schema-registered) and
corpus-level `constraints_gap_examined`/`constraints_gap_undetermined`; `query.py --detail` (also fixes a
latent `len(None)` crash — `.get("gaps",[])` returns None on present-null); `tensions_ledger.py` dedicated
gap-operability line; `enhanced_report.py` `build_omega_section` (a 5TH live site first cleared WRONG — grepped
for a "gaps" render, a proxy, not "distinguishes undetermined at a human surface"; it collapsed no_gap/undetermined
into "not yet enriched" at the primary `constraint_reports/*.md` surface; caught by the operator's question, now
leads with a gap_status line). Witnessed at the JSON boundary (pipeline exit 0, mtime advanced): behavior preserved
(`constraints_with_gaps`=57, `omega_count`=57), companions examined=89/undetermined=30, 0 consistency
violations, schema 0 errors, labels distinguish gap/no_gap/undetermined on both human surfaces.

**TRIPWIRE — `prolog/detector_calibration.pl` — SUPERSEDED by the 2026-07-01 R4-ruling entry at the top of
this file.** (At the time of this entry it was untracked and unwired, awaiting the proposal ruling.) It is now
TRACKED-but-unwired reference by the R4 ruling; still loaded by nothing and wired into no report. See the top
entry for the current disposition and the do-not-wire condition.

---

## 2026-07-01 — OQ-197 (a)/(b) cross-tab: canonical (b) ≡ h1_band, stakeholder (a) distinct; canonical-source bug fixed
**Files:** prolog/report_generator.pl, audits/2026-07-01_oq197_source_h1_crosstab/
**Tier:** correction-key

Commit `6bda83ec`. Made `gap_status`/`detect_gap_pattern` source-explicit (`/3`) to evaluate both (a)/(b)
sources per constraint; firing under default byte-identical (57=57). Cross-tab on the both-sources-determinate
testsets subset (n=84): canonical (b) firing EXACTLY coextensive with `h1_band>0` (58/58, 26/26, zero
off-diagonal — definitional, same orbit) ⇒ (b) is a redundant recomputation of `h1_band`; stakeholder (a)
distinct on 3/84 (authored-stakeholder disagreement `h1_band` lacks). Evidence points toward ruling (a); ruling
stays operator's seat, now evidence-fed. **Twins extension (2026-07-01):** on the both-determinate subsets
(testsets 84 + haiku 452 + flash 661 = 1197) canonical (b)↔`h1_band>0` has 0 off-diagonal (definitional —
confirms wiring, not new evidence); stakeholder (a) distinct on 36/1197 (3/19/14). Twin `h1` computed in
Prolog (`cohomological_obstruction/3`), positive-controlled vs pipeline `h1_band` on testsets (0/119) before
use. **Corpus-independence caveat:** haiku+flash are TWINS (same seed, different backend → correlated), so
this is ONE independent corpus + one correlated pair, NOT triple replication. Establishes non-redundancy
ONLY — (a) irreducible to `h1_band` as a construction; whether the divergences are (a)-correct vs authoring
noise is OQ-199 (open). **RULING (a) — keep the stakeholder source (operator, 2026-07-01):** (b)=duplicate
of `h1_band` (cruft), (a)=non-redundant; `gap_seat_source` stays `stakeholder` (no code change — default
already implements (a)). Ruling resolves redundancy, NOT reliability (OQ-199). Evidence
`audits/2026-07-01_oq197_source_h1_crosstab/`. **Correction to b616e625:** its canonical (b) seat clause used
`constraint_classification/3` with an UNBOUND context (mode `+Context`) → 0 seats for every constraint (a dead,
unwitnessed branch — my contract witnesses were all stakeholder-path). Fixed to `drl_core:dr_type/3` via
`logical_fingerprint:standard_context_for_power/2` over the 4 canonical positions (the `write_perspectives`/
`h1_band` source). Lesson: a branch dead under the default config still needs its own witness — the cross-tab
was the first thing to exercise it. Twins extension pending (see ISSUES.md OQ-197 step 3).

---

## 2026-07-01 — OQ-197 three-valued gap operability CONTRACT landed (branch, behavior-preserving); 6th consumer found
**Files:** prolog/report_generator.pl, prolog/tests/test_gap_operability.pl, python/tensions_ledger.py, python/json_report.pl, prolog/detector_calibration.pl
**Tier:** landed

Branch `oq197-three-valued-gap-operability`, commit `b616e625`. Added `report_generator:gap_status/2`
→ `gap(...)` | `no_gap` | `undetermined(no_seats|single_seat|single_power_position)`, closing the
Pattern-6 collapse in the gap detector. Built **source-parameterized** per operator ruling (2026-07-01):
`gap_seat_source/1` (default `stakeholder`; `canonical` via `constraint_classification/3` written) feeds
BOTH `detect_gap_pattern/2` and `gap_status/2` through `seat_type_reading/2-3`, so the OQ-197 (a)/(b)
ruling is a one-line change. `detect_gap_pattern/2` firing logic UNCHANGED — the split is additive.
`gap_coverage/1` lifted from ≥1-seat proxy to the operability precondition (case-(ii) fix at the `"gaps"`
field). Witnessed: firing byte-identical (57=57 diff-empty on testsets); `gap_status` total/deterministic
(119/119; gap=57 no_gap=32 undetermined=30); `dataset_recycling_amplification → no_gap`; 9 two-sided
plunit controls pass; 0 new corpus-suite failures (20 pre-existing mountain/nl drift, baseline-confirmed
old==new).

**Finding — the ledger "no gap pattern matched" line is a SIXTH consumer with its own bug.**
`tensions_ledger.py:131` computes index-mismatch from `perspectives`, not the `gaps` field, and
`{v for v in persp.values() if v}` counts `unknown` as a diverging value — so
`dataset_recycling_amplification` reads "perspectives diverge" purely because `analytical` is untyped
(scaffold-vs-unknown, not a real gap). OQ-197's detector fix does not reach it; it needs repointing to
`gap_status` or an `unknown` filter. Remaining OQ-197 graduation steps + full 6-site consumer map are in
ISSUES.md OQ-197 (Progress 2026-07-01). Sequenced: wire consumers → two positive controls → h1_band
cross-tab → (a)/(b) ruling → R4 recompute (held).

---

## 2026-06-30 — detector self-assessment: Slice A (author×engine cross-tab) LANDED; Slice B (calibration omega) proposal awaiting ruling
**Files:** prolog/routing_sink.pl, docs/design/detector_calibration_omega_proposal.md, outputs/routing_sink.json
**Tier:** landed

From the Elias-Thorne report review: web-Claude asked whether Prolog can address the
"is the snare/rope detector calibrated" question. Answer split three ways — (A) computable
seat-agreement aggregate, (B) an authored apparatus-directed omega, (C) auto-closing the
verdict = category error (no ground truth in the testset; authored type is a seat, seat theorem).

**Slice A LANDED** (`routing_sink.pl`, commit `f6921ac1`): added `author_engine_crosstab` +
`author_engine_crosstab_summary` to `routing_sink.json`'s manifest — a (authored_type ×
engine_type) confusion cross-tab over the existing per-seat `seat_diff` records. Diagonal =
agreement (`no_route`), off-diagonal = divergence by type-pair (tangled_rope→snare=106 dominant).
**Hard label: SEAT-AGREEMENT, NOT calibration** — `divergence_rate` (0.77) is a two-seat
disagreement rate, never a detector false-positive rate (convergence is stable, not correct).
Positive control reconciled: diagonal 91==no_route 91; off-diagonal 305==author_engine_divergence
(255)+engine_exit_table_review(50); both_speak 396 + both_silent 36 + engine_abstained 44 = 476 =
119×4. (Caught a self-inflicted A-E-vs-A-Engine template typo — unbound key, Pattern-5 vacuous
guard — via that control before shipping; added nonvar/2 guard.)

**Slice B PROPOSAL, awaiting operator ruling** (`docs/design/detector_calibration_omega_proposal.md`,
commit `c4864999`): a `detector_calibration` omega the engine MINTS OPEN (computable firing
condition: computed snare/rope ∧ hidden-extraction shape — theater∨coupling-masked∨no-exit-victim;
all fields verified present) but does NOT close. Typed as an Ω_E (hit rate, awaits external labeled
data) + Ω_P (acceptable FP rate, a value-decision) PAIR — conflation is the "when to stop verifying"
trap. NOT wired/fired: R1 threshold, R2 typing, R3 engine-minted-vs-authored, R4 mint+wire are the
operator's seat. Generalizes the existing story-local detector-doubt convention
(press_reformation_causality omega).

## 2026-06-30 — perspective_chi d/f_d fork fixed (resolved-context derivation); report frame added
**Files:** prolog/constraint_indexing.pl, prolog/json_report.pl, python/enhanced_report.py
**Tier:** landed

`json_report:write_one_perspective_chi` exported `d`/`f_d` derived on the UNRESOLVED
canonical power atom, while `chi` (via `extractiveness_for_agent/3`) resolves coalition
power internally. For any perspective whose power coalition-resolves (`powerless→organized`),
the exported `f_d` (from d=0.9) forked from `chi` (from d=0.5): **40/119 live constraints had a
`powerless` row where `chi ≠ ε·f_d·σ`**. Surfaced by web-Claude reading the Elias-Thorne
constraint reports (`f(d)=1.358606` appearing with `d=0.500`); its two hypotheses (f saturates;
d-table reused) were both falsified — `f` is d-dependent and `d` is observer-position-keyed
(`constraint_indexing.pl:478-487 power_role_heuristic/4`).

Fix: factored resolve+derive into `constraint_indexing:agent_resolved_directionality/4`
(exported), used by BOTH `extractiveness_for_agent/3` and the JSON writer so they cannot fork.
Witness: model_collapse_feedback powerless before `chi=0.4056 d=0.9 f_d=1.358606` (0.78·1.3586·0.8=0.848≠chi)
→ after `chi=0.4056 d=0.5 f_d=0.65` (=chi). Behavior-preserving: 0 type/classification changes,
0 chi changes across 119 constraints (re-run pipeline exit 0, mtime advanced); forked rows 40→0/440.
Commit `6d1df7d1`.

Also (commit `5e5830df`): prepended a "HOW TO READ THIS REPORT" frame to `enhanced_report.build_header`
— purpose is to surface SEATS, divergence (between seats / from authored commentary) is the finding,
RED = authored victim/beneficiary direction (OQ-187) not a moral verdict, d is observer-position-derived.
Tripwire for a future agent: any NEW consumer that reports `d`/`f(d)` alongside `chi` must derive them
via `agent_resolved_directionality/4`, never `derive_directionality/3` on the raw canonical context —
or the fork reopens silently for coalition-resolving perspectives.

## 2026-06-30 — OQ-38 RESOLVED: reproducible orphan-xref tool built; four calibration orphans stripped; OQ-196 minted
**Files:** prolog/orphan_xref.pl, python/audits/oq38_orphan_sweep.py, prolog/drl_composition.pl, prolog/utils.pl, ISSUES.md, AGENTS.md, audits/2026-06-30_oq38_orphan_xref/
**Tier:** landed

Replaced the discredited 2026-05-31 ad-hoc grep sweep (`217-candidate upper bound`,
hand-transcribed into ISSUES.md) with a reproducible **tool-native funnel**.

- **New tool `prolog/orphan_xref.pl`** — `library(prolog_xref)` clause-head-vs-body separator;
  mirrors `check_stack.pl` (load-path-independent, **diagnostic NOT a pipeline gate**). Emits per
  `Name/Arity`: file, exported?, static-caller set (module-stripped), class
  (`LIVE`/`ENTRYPOINT_CLI`/`STATIC_ORPHAN`). Caller matching is global `Name/Arity` —
  conservative-by-design (biases LIVE; a false orphan is the only dangerous error).
- **Driver `python/audits/oq38_orphan_sweep.py`** — masks static orphans against the dynamic
  surface (Python goal-strings + Prolog name-construction prefixes), emits the funnel.
  *Self-exclusion gotcha:* the driver NAMES its strip targets in `CALIBRATION_FOUR`, so it must
  exclude its own path from the Python-surface grep or it false-positives every target as
  dynamically-reachable (witnessed + fixed this session).
- **Funnel (121 sources):** 614 exports (grep claimed 528 — **+86, grep undercounted**), 201
  STATIC_ORPHAN (grep 217 — −16), 29 dynamic-masked, **M=170 real-orphan upper bound** (post-strip).
- **Stage-1 hard gate:** `cs_reference_frame/2` LIVE (the OQ-35 adversarial case), and
  `non_monotonic_trajectory/2` LIVE with caller in **`metric_drift_report.pl`** — the OQ census's
  `drift_report.pl:164` cite was stale (file absent); corrected in ISSUES.md.
- **Four stripped** (commits A `736783e4` slope-pair, B `6a3acf1d` safe_get-pair; tool +
  `c9be12ca`). Behavior-preserving witnesses: load gate exit 0, validation suite byte-identical
  (timing-normalized), pipeline `per_constraint` sha256 unchanged `d9c85bec…` mtime advanced.
- **Cascade:** Commit B newly orphaned `safe_get_category/3` (sole caller removed) — routed to
  **OQ-196** (value-adjudicate the M=170 remainder), NOT stripped (scope ruling = strip only the
  four). Full writeup: `audits/2026-06-30_oq38_orphan_xref/WRITEUP.md`.

## 2026-06-30 — OQ-37 RESOLVED (read-but-unauthored metric census re-dispositioned); GAP-23 minted
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/data_validation.pl, python/generate_constraint_pl.py
**Tier:** landed

OQ-37's read-but-unauthored `constraint_metric` census re-dispositioned at its root: all six target
names (inevitability, internalization_depth, resistance_to_change, accumulation_speed, sunset_time,
alternatives_available) trace to the **fixed compiler emit set** (`generate_constraint_pl.py:608-635`)
— "author" = grow compiler+schema+validator+prompt, "remove" = strip a consumer. Authoritative
cross-corpus census (FACT pattern `constraint_metric(_,Name,_)`, not bare name): **all 6 are 0 on
testsets/haiku/flash/kernel_v1 = 3,142 stories**; controls resistance/extractiveness fire on every
leg. Witness + per-probe evidence: `audits/2026-06-30_oq37_census_redispose/`.

Dispositions: `inevitability` read already removed (D2 strip, `constraint_bridge.pl:20-25`),
capability superseded structurally by `false_natural_law` (`signature_detection.pl:1018,1040`);
compound grid metrics resolved by OQ-93; χ-partition closed (`3ab3ace4`); Part D masked-unknowns
moot post-reset; `accumulation_speed`/zero-caller helpers → OQ-38; supp/ε-floor → OQ-48. The two
genuine deferred capability livens (`sunset_time` self-supplied falsification tell;
`internalization_depth` manufactured-consent quadrant + `psych_bridge` never loaded) → **GAP-23**
(priced, operator-seat, reopen on analytical-product demand).

One behavior-preserving engine edit (commit `5b7a8b95`): dropped never-authored
`resistance_to_change` from `data_validation.pl:320` extreme-value monitor. Witness: validation
suite `✓ No extreme values`, 0/1/1 identical before/after; provably byte-identical (0 facts → member
never matches); validation-channel only, does not touch `pipeline_output.json`.

**Correction-key (OQ-64 instance):** `resistance` ≠ `resistance_to_change` — `resistance` is the
NL/coercion-GRID metric (`grid_first_contact_gate.py:48`, mountain-signature feature), a distinct
referent from drift-domain resistance-to-abolition. The proposed `metric_drift_events.pl:174,247`
repoint (resistance_to_change→resistance) was **DECLINED**: `safe_metric/3` fails silently
(`:66`), so `function_obsolescence` dies at its first goal (`alternatives_available`) — the repoint
buys zero behavior while baking a latent wrong-metric identification. Liven the detector's two inputs
together (GAP-23) or leave it dark; never repoint by name-stem.

## 2026-06-30 — OQ-27 RESOLVED (signature-resolved H¹ disclosure); OQ-195 minted (general-n gap)
**Files:** prolog/grothendieck_cohomology.pl, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, issues/INDEX.*, CLAUDE.md, KNOWN_STATE.md
**Tier:** correction-key

**Ruling: disclosure, not redefinition; no engine behavior change.** The engine already computes H¹
over the **signature-resolved** `dr_type` orbit; OQ-27's gap was that no doc/comment said so. Under
**append-versioning**, `v6.13.md` + v6.8–v6.12 stay frozen — precision landed only in
`v6.13.1.md` (dated OQ-27 amendment + two inline "signature-resolved" qualifications at the intro and
§5.1) and an engine comment at `grothendieck_cohomology.pl` (`orbit_vector/2` + `type_at_context/3`).
v7 §Thm 7 already carried it (no-op; v7 untouched, confirmed by `git diff --stat`).

Path disclosed: `cohomological_obstruction → orbit_vector → type_at_context → dr_type`; inside
`dr_type`, `metric_based_type_indexed` (raw `classify_from_metrics`) **then**
`integrate_signature_with_modal`. So H¹=0 means the *signature-resolved* orbit is a global section —
raw per-context metric types may be maximally heterogeneous (the signature is the cover story, Thm 1).

**Witnesses (manifest `2026-06-30T00:08:22Z`, n=116), denominators kept distinct:** 65 of 86
four-real-seat constraints at H¹>0 (discrimination); *separately* 116/116 reproduction of stored
`h1_band` from the serialized `perspectives` orbit (orbit-reproduction control). GATE GREEN.

**Discovery → OQ-195:** Theorem 2's gap {0,3,4,5,6}/forbidden {1,2} is proven only for **|real
seats|=4**; under the OQ-51 N/A rule the real-seat count varies. Reachable spectra (proven by
enumeration) are n=3→{0,2,3}, n=2→{0,1}; the four `h1_band=2` constraints are 3-real-seat, NOT
counterexamples. The unwritten **general-n** induction is **OQ-195** (P3); the `grothendieck_cohomology.pl:158`
range comment carries a stale-range flag pointing to it.

## 2026-06-30 — OQ-194 RESOLVED: embedded mountain/nl "failures" are correct commentary; one rotted phantom fixture fixed
**Files:** ISSUES.md, prolog/tests/test_phantom_neighbor_filter.pl, python/generate_constraint_pl.py, prolog/testsets/*.pl (16 claim=mountain files), KNOWN_STATE.md
**Tier:** correction-key

Running `cd prolog && swipl -g "[stack], [tests/test_phantom_neighbor_filter], run_tests, halt"`
loads the whole corpus and registers every testset's embedded validation units → **21 fail / 93
pass** (live, 2026-06-29). The 21 are two unrelated things:

- **20 embedded** = 13 `mountain_threshold_validation` + 7 `nl_profile_validation` (the 7 a strict
  subset of the 13). These are the apparatus **correctly commenting** that stories which *claim*
  mountain (`constraint_claim(C, mountain)`, the SURFACE claim) lack true-mountain metrics —
  claim ≠ actual is the DR core. **Not regressions, not WIP.** Tight bars intentional (failure
  marks contention). **The red-as-signal reading is conditional on these tests staying NON-GATING**
  (not in `gate.sh`): a failing *assertion* conventionally means defect; the analogy to a neutral
  linter (OQ-116's `MOUNTAIN_METRIC_CONFLICT`) breaks the moment anything gates on them. Structural
  evidence (holds even if no OQ existed): generator emits both tests ONLY on claim=mountain with
  hardcoded bars (E≤0.25, S≤0.05, AC≥0.85, R≤0.15); 12 of 32 embedded assertions PASS on clean
  mountains (protein/radiative/actinide). All 20 are plunit `: failed`, **zero `: error`**; all 13
  failing files declare `constraint_claim(_, mountain)`.
- **1 `phantom_neighbor_filter:real_target_edge_fires`** = a genuine **fixture-rot defect**. The two
  hardcoded positive-control names (`ai_governance_accountability`, `retirement_security_deficit`)
  rotted out of the corpus at the 2026-06-05 reset (sole archive holder: `kernel_v2_test2`), turning
  the control into a phantom and making the exclusion tests pass **VACUOUSLY** — the OQ-95 guard
  guarded nothing. **Fix:** `two_real_targets/2` self-selects two constraints the filter would NOT
  drop (`\+ drl_purity_network:phantom_subject/1`) and **throws** `insufficient_real_targets` on
  under-supply. Witnessed: 4/4 phantom-filter green on live corpus; loud-failure control on a
  1-constraint overlay throws (setup error, not vacuous pass). Silent rot is now unreachable.

**Landing:** explanatory comment emitted from the generator + backfilled into all 16 current
claim=mountain testsets (`grep -lE 'constraint_claim\([a-z0-9_]+, mountain\)' testsets/*.pl` → 16);
header signpost in `test_phantom_neighbor_filter.pl`. **Deferred calibration → OQ-48** (the hardcoded
bars added as recalibration targets; no new OQ). OQ-194 closed. Two commits (docs ruling / code fix).

## 2026-06-29 — OQ-23/OQ-24 RESOLVED (narrow same-kernel contamination guard); OQ-193 deferred
**Files:** prolog/drl_purity_network.pl, prolog/tests/test_coexists_fpn_canary.pl, prolog/giant_component_analysis.pl, ISSUES.md, audits/2026-06-29_oq23_coexists_fpn_canary/
**Tier:** landed

Full arc (operator-guided, multi-round): a positive-controlled canary
(`prolog/tests/test_coexists_fpn_canary.pl`) FALSIFIED the premise it was built to backstop — the
`coexists_with` "zero contamination by definition" exclusion was NOT latent but ALREADY VIOLATED on
every populated leg (testsets/ 2, haiku 178, flash 361, kernel_v1 662) via the authored
`affects_constraint` side channel between sibling readings (the DP-001 ε-invariance "link ε-distinct
constraints via affects_constraint/2" instruction; `affects_constraint` is overloaded across
ε-linkage / UKE-dependency / generic). forecloses leaked the same way (relation-agnostic).
Per-consumer reachability witness: of 4 consumers reading the sibling edge, only **FPN
`effective_purity`** (ships to `pipeline_output.json contamination_network`) and **coupling baseline**
(ships to `coupling_protocol.md`) reach a product; composition `detect_extraction_dominance` (no
callers) and counterfactual `dependency_chain` (`simulate_cut` has no live caller) are inert.

**FIX (OQ-23 + OQ-24):** a same-kernel-donor guard as the first clause of
`compute_edge_contamination/7` — a same-kernel sibling contributes ZERO contamination.
Contamination-local by design (NOT `constraint_neighbors_existing/2`), so giant_comp topology is
unchanged. Witnessed: canary census `leaked` 2→0 (forecloses 1→0) on testsets/; `effective_purity`
returns to intrinsic for the leaking pairs; cross-leg post-fix census `leaked=0`; giant_comp
connectivity zero-change control (testsets baseline 66/12 unchanged); plunit regression gate
`no_coexists_or_forecloses_leak_on_loaded_corpus` GREEN.

**Why landed (tier):** the fix is committed and witnessed; the leak no longer ships. **Deferred:
OQ-193** — stripping same-kernel sibling edges from giant_comp connectivity collapses the giant
component 334→70 (kernel_v1); whether that is a correction (siblings aren't cross-kernel coupling, per
the OQ-84 precedent) or a loss (legitimate topology) is an unsettled Ω_C ruling, NOT resolved by this
fix (the contamination-local siting leaves giant_comp untouched precisely so OQ-193 can be ruled on
its own evidence). The coupling-baseline ship (also wrong by the module's own OQ-84 logic) is a
separate fix candidate noted in HOLD_FINDINGS.

**Tripwires (promotion test → file-local, not CLAUDE.md):** `compute_edge_contamination/7` and the
`drl_purity_network.pl` header comment both carry "do NOT extend a same-kernel guard into
`constraint_neighbors_existing/2` without resolving OQ-193 (changes 5 contamination-topology
consumers + a shipped headline metric)." An editor of that file sees it; the canary regression gate
self-flags a reopened leak.

## 2026-06-27 — OQ-124/OQ-149 committer-axis convention control: A=SIGNAL, B=CONVENTION, C=OPEN
**Files:** ISSUES.md, prolog/signature_detection.pl, python/story_repair.py, agent/run_no_scope_gemini.py
**Tier:** landed

Ran the OQ-70 bait-confound control on the three cross-model-divergent fields, per-field
pre-registered (`audits/2026-06-27_oq124_oq149_committer_convention_control/`). Twins re-classified
at one commit `bbf5c92` (the on-disk outputs were at 20fab78/8126231, straddling the OQ-138 ROUTE
conversion of `false_ci_rope`+`constructed_high_extraction` — non-comparable for Field A). Positive
controls held (claimed_type 0.7208, cs_kernel_id 1.000). Verdicts:
- **Field A (signature fork) = SIGNAL.** The CHE↔FCR fork is ~13:1 asymmetric (157 haiku-CHE/flash-FCR
  vs 12 reverse), and the dominant lean is a continuous extraction-magnitude difference (0/157 ride
  the `constraint_claim(rope)` template alone; all 157 have flash ε below the rope ceiling / haiku
  above the snare floor; cross-twin ext Spearman 0.86, flash systematically lower). Two-sided
  `with_retracted` control discharged. → signature lean carries a model index (v8 §3/OQ-72).
- **Field B (`cs_reading_relation`) = CONVENTION.** Flash leans more foreclosing (p=0.020) but the
  call fails to covary with the settled substrate on disagreeing slots (Spearman 0.156/0.162 < 0.20;
  agreeing-slot control 0.256/0.258). → needs a provenance bucket (precedent `becd0f87`).
- **Field C (`overridden` 51-vs-4) = OPEN-pending-instrumentation.** Per-slot coercion witness
  unrecoverable. *Enrichment (tripwire-adjacent):* `overridden` is **coercion-invariant** — a missing
  `cs_axiom_status` makes `generate_pl` KeyError → the story FAILS generation (generate_constraint_pl.py:672),
  it is NOT silently defaulted to `holdable`; and the `contested/foreclosed→holdable` remap
  (story_repair.py:89-90) is silent. So `overridden` counts are real authored values; only flash's
  `holdable` splits authored-vs-coerced, and that needs raw pre-repair capture (instrument
  `story_repair._normalize_axiom_status` to log `cid`). Third-model spend now warranted (A=signal),
  operator-gated.
**Files:** python/cohort_stability.py, python/cohort_sigma_seat_eval.py
**Tier:** tripwire

A `stable`/`match` verdict in a per-field comparison table can mean three structurally different
things, two of them hollow: (1) **content reproduced** — the real signal (`scalar`/`cat`/`nameset`
comparing values); (2) **presence-only matched** — the comparator sees only PRESENT vs EMPTY
(`prose_presence`/`list_presence`; apparatus `*.presence`), so the field reads "stable" whenever
the model emitted anything non-empty; (3) **the field is a constant** — zero between-item variance,
so it *cannot* be unstable (`emerges_naturally` True 18/18; `claimed_type`, `has_sunset_clause`;
`omegas.count` range 0.00). Aggregating across fields without splitting these silently inflates the
"stable" side and can **invert** a partition statistic, not merely soften it.

**Rule:** before trusting any aggregate over per-field comparisons, witness what each field's
comparator actually compares (read the extractor, not the column name), and run a between-item
(cross-story) variance check to flag degenerate constants. Worked instance — the OQ-118 re-probe:
removing presence-hollow fields from a σ/seat partition dropped consistency 47.9%→39.7% (an
inversion toward the unstable cast multisets), and the degeneracy sweep caught four constant
"stable" fields. Witness + re-runnable probe: `audits/2026-06-27_oq118_reprobe/` (commit
`fc57e833`); ruling landed `82c0693c`.

This is the per-comparator face of CLAUDE.md Build Discipline **Pattern 6** (measured-empty vs
didn't-look) and a sibling of **Pattern 5** (absence satisfies the gate) — the abstract tripwire
lives there; this is the worked instance, **not** promoted (the always-loaded form already exists;
over-promotion defeats the token-saving purpose). Cross-ref OQ-118.

---

## 2026-06-27 — OQ-182 family product SHIPPED: trajectory serialized + trajectory_enabled 0→1
**Files:** python/run_pipeline.py, prolog/config.pl, CLAUDE.md, AGENTS.md, ISSUES.md
**Tier:** landed

Flipped `config.pl:571 trajectory_enabled` 0→1, unblocking the OQ-182 family-product flip
that was held by a witnessed-NEGATIVE freshness criterion (a flag=1 run intermittently
stalled). Root cause: **concurrency memory pressure** — the `trajectory` stage (HAC
clustering, O(N²)) ran in the 4-worker Phase-2 thread pool **co-resident** with `giant_comp`
(also O(N²)); the two heavy swipl subprocesses overlapped. NOT a giant_comp bug (OQ-77:
serially fine at 87× the corpus).

**Fix (surgical, Python-only, no engine/classification change):** `run_pipeline.py`
`_phase_prolog` pulls `trajectory` out of the parallel `tasks` list and runs it
**sequentially after** `_run_parallel` returns — the `with ThreadPoolExecutor` joins
giant_comp's worker (and its synchronous swipl child) before returning, so the two heavy
stages never co-reside. Order is correctness-irrelevant: trajectory's only output
`context_profile_report.md` has no downstream consumer (C0 invariant). The 11 remaining real
stages stay parallel (the proven-fine pre-trajectory pool).

**Witnessed** (`audits/2026-06-27_oq182_trajectory_serialization/`): mechanism witness via a
~0.1s ps/RSS sampler over flag=1 pipelines — PRE-FIX arm captures co-residency (0.64s window
overlap, deterministic run-1 positive control); CURED arm shows disjoint windows (trajectory's
swipl starts 0.79s after giant_comp's exits). N=10 liveness battery 10/10 GREEN. Freshness
positive control PASS (non-vacuous). C0 re-witness zero classification diff (positive-controlled).
Measured trajectory alive-window 1.5s ⇒ 300s timeout held (≥175× margin, not bumped to 900).
`validate_config` PASS at flag=1; `trajectory_weights_sum` gate active+satisfied (sum=1.0).

**Promotion test → tripwire promoted to CLAUDE.md (Running the System):** a fresh agent could
silently re-fold `trajectory` into the parallel `tasks` list and reintroduce the intermittent
stall — the two O(N²) stages must never run concurrently. Tripwire lives in CLAUDE.md; full
provenance here.

---

## 2026-06-26 — OQ-91 resolved: commentary-grade repair-transition detector + report surface
**Files:** prolog/transition_paths.pl, prolog/json_report.pl, python/enhanced_report.py, docs/repair_dynamics.md, ISSUES.md
**Tier:** landed

Closed the observer-axis one-way ratchet (engine encoded decay, not repair). New
`repair_transition/4` in `transition_paths.pl` — the upward dual of the 8 decay
heads, **reusing** `degradation_chain/3` (the snapshot_type series) as source,
"upward" = transitive closure of the 8 `transition_path/4` decay edges read
backwards (`unknown` excluded). 4th arg = named op (`maintain`/`splice`/`replace`
rope line-ops; `scaffold_struck` construction op), a function of from/to + chain
prefix. **COMMENTARY-GRADE** — must never feed `classify_from_metrics/6`, the
signature layer, or `verdict_join`. Serialized as the `repair_transitions`
per-constraint field (`json_report.pl`, hermetic `preserve_classify_globals/1`
wrapper around the snapshot_type nb-globals), rendered by
`enhanced_report.py:build_repair_section` (single data direction; silent on
decay-only = honest absence).

Witnessed (`audits/2026-06-26_oq91_repair/`): real-corpus B1-scan non-empty
(testsets/ 2, kernel_v1 30, incl. multi-step homoousios/versailles
snare->tangled_rope->rope) => close-state 1, no new authored atom. B4 invariant
PASS (pipeline_output.json classification fields byte-identical with/without the
surface; only the new field added). Bug found+fixed: `repair_op` clause selection
must key on from/to/pre, not a bound 4th arg (else a bound-Op query mislabels via
the default clause). Suite 0 errors, snapshot-migration 10/10, warning gate 3/3.
Promotion test: wiring repair into classification would be LOUD (output changes,
caught by the diff) -> no silent-mistake tripwire -> no CLAUDE.md promotion.

## 2026-06-26 — OQ-182 C-gen: family product is generation-EXPRESSIVE (A4 flip still operator-gated)
**Files:** prolog/config.pl, prolog/context_profile_mining.pl, ISSUES.md, audits/2026-06-25_oq182_trajectory_revive/
**Tier:** correction-key

Part-A progress toward the OQ-182 trajectory gate flip. **A1/C0 PASS** (flag 0->1
changes only `config.trajectory_enabled` in pipeline_output.json; all
classification fields byte-identical; positive-controlled). **A3 C-prov PASS on
kernel_v1** (1106; classify_at_time globals unset post trajectory_run). **A2 C-gen
FAILED at its locked bar** (haiku<->flash family ARI=0.117 < 0.50) — a live
falsifier, treated as the finding. Operator ruled option-2 (re-specify, no
laundering): a freshly pre-registered, granularity-insensitive **substrate read**
(which constraints split, do splits track real fingerprint_shift differences) gives
**TRACK=162/162=1.000** — every inter-leg family split is backed by a real
per-reading shift difference, ZERO cut-height artifact. Dual finding (both stand):
global partition does NOT recover across generation (ARI fail) AND that failure is
**generation-EXPRESSIVE, not clustering noise** (locally stable PRES=0.83
descriptive-only; globally expressive). **A4 gate flip remains the operator's seat
— NOT flipped** (`config.pl trajectory_enabled` stays 0); kernel_v1 C-null deferred
(cost; cannot unblock a flip A2 already gates). Caveat for A4: one flag=1 run
stalled (likely giant_comp under added parallel pressure); a second completed in
12.6s.

## 2026-06-26 — OQ-104 resolved (scoped): gate.sh gains a 7th check (audit-citation frozen-evidence)
**Files:** scripts/gate.sh, python/audit_citation_status.py, audits/2026-06-18_oq104_citation_checker/controls.py, audits/2026-06-18_oq104_citation_checker/controls_run.sh, ISSUES.md
**Tier:** landed

Operator ruling: **gate the OQ-104 danger class.** The frozen-evidence danger (a unique evidence
file a fresh clone needs and lacks — the spectral_laplacian origin) is distinguishable from benign
descriptive refs by **regenerability**: an untracked cited path is dangerous **iff it is not under
top-level `outputs/`** (repo-root `outputs/` is rebuilt by every `run_pipeline`).

Changes:
- `audit_citation_status.py:classify()` — split the single `untracked-pending` sublabel by
  `c.startswith("outputs/")` into `untracked-frozen-evidence` (**GATING** — intrinsic ERROR, no
  flag) and `untracked-regenerable` (non-gating WARN). `--check` exits 1 iff frozen-evidence
  non-empty OR parse `problems`. `--promote-untracked` now lifts `untracked-regenerable`.
- `scripts/gate.sh` — new 7th `run` line `audit cites` (`audit_citation_status.py --check`).
- controls.py 23/23 → **25/25**: matched-pair (identical fixture content; frozen-arm in an audit
  dir vs regenerable-arm under `outputs/`) isolating the **prefix** as the deciding variable, plus
  a dotted `./outputs/` post-normalization control. controls_run.sh rot-fixture (non-`outputs/`)
  now flips pass→frozen-evidence (gating).

Witnessed: all **39** distinct untracked paths under `outputs/` → `untracked-frozen-evidence:0` →
gate GREEN; end-to-end RED-on-frozen-citation / GREEN-on-removal; full `./scripts/gate.sh` GREEN
(7/7). **Scope (do not over-read "resolved"):** one of two origin routes mechanized. Two residuals
stay non-gating with kill conditions (ISSUES.md OQ-104): a **typo'd** path lands in
`missing-pending-M` (gating `missing` would FP on all 70); a frozen artifact **parked under
top-level `outputs/`** reads `untracked-regenerable` (the prefix is a convention, not an invariant).
Promotion test: a new gate check fails LOUDLY (gate prints RED) → no silent mistake → no CLAUDE.md
promotion needed.

## 2026-06-26 — GAP-04/OQ-53 increment: cross-kernel reading-stance transpose (fingerprint_shift spine)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/cross_kernel_stance_report.py, prolog/tests/test_cs_kernel_registry.pl, docs/design/design_gaps.md, ISSUES.md
**Tier:** landed

Built the **reading-stance transpose** GAP-04 names absent and OQ-53's 2026-06-20 close RESERVED
(it shipped the draw-robust *observer-signature* orbit and explicitly deferred the semantic-stance
one as "model-relative only"). New in `cs_kernel_registry.pl`:
- `declared_stance/2` — THE SEAT (hand-declared cohort table; initial, for the exercised stances).
- `reading_stance/2` — authority = declared only (morphology is never a query-time fallback).
- `stance_cohort/2` — readings of a stance across kernels (transpose of `cs_readings_for_kernel/2`).
- `cross_kernel_stance_profile/2` — gathers each member's `(kernel, fingerprint_shift)`, derives a
  per-position majority **consensus** pattern (`$wild` = no majority), partitions convergent vs
  divergent, and reports the verdict WITH cohort provenance (morphology-suggested vs hand-declared).
- `cross_kernel_stance_report/0` + `cross_kernel_stance_export/1` (JSON).

`json_report.pl` now serializes `fingerprint_shift` per `per_constraint` entry (was absent: grep=0
pre-change; 104 entries post-run, pipeline exit 0 + mtime advanced) so the Python consumer reads
COMPUTED shifts, never recomputing `classify_at_power`. Consumer
`python/cross_kernel_stance_report.py` runs the transpose over both live twins →
`outputs/cross_kernel_stance.{json,md}`.

**Why the cohort is DECLARED, not derived (Seat-Theorem Cor 2b).** Morphology is unreliable both
ways, witnessed on the 7-member abolition cohort: an exact-stem rule (stem `abolitionist`) catches
only **4/7** (the stems fragment to {abolitionist, abolition, categorical_abolition,
abolitionist_rejection}); a substring rule over-admits `dharmasastra_corpus__abolitionist_rejection`
— a *rejection* of abolitionism. So a human curates `declared_stance/2`; the profile carries each
member's provenance and the verdict inherits it.

**Witnessed results (both twins).** abolition → convergent on `shift(*,snare,rope,snare)` **5/7 on
BOTH twins** (draw-stable / situation-fixed); the 2 outliers are genuine structural divergences (one
of them, `animal_status__abolitionist_reading`, is morphology-*suggested* yet structurally divergent
— morphology ≠ structure). deterrence flips convergent(haiku 4/1)↔divergent(flash 2/3) — draw-variant
/ seat-expressive. originalist is kernel-divergent (5/11). property has no shared signature. Read the
convergent/divergent split as a σ/seat partition, NOT a fixed label (determinism frontier).

Pinned by `prolog/tests/test_cs_kernel_registry.pl` (5 new corpus-free `transpose_*` tests on the
consensus spine — robust where the file's divergence cases are snapshot-fragile). NOTE: the
pre-existing `divergence_silent_at_observed_agreement_context` failure (19→24 of 25 pass) is the
documented archive-draw data-fragility, NOT caused by this change (the edit is purely additive; the
`cs_kernel_divergence/4` body is untouched). Provenance: ISSUES.md OQ-53 addendum, GAP-04 status.

## 2026-06-26 — OQ-21(b) CLOSED as a recorded design absence: the single-instance barrier is the module-collision, not DP-001
**Files:** ISSUES.md, prolog/corpus_loader.pl, prolog/config_validation.pl, prolog/json_report.pl
**Tier:** correction-key

Corrects two prior framings of OQ-21(b) ("does A12's multi-instance render branch fire on
real pipeline data?"). The committed "the gate is the MERGE MECHANISM, not the data" framing
(2026-06-26) was directionally right but unwitnessed and under-specified; a session-internal
hypothesis that **DP-001 is the single-instance barrier was falsified by running it**. Two
real `abolition_reading` draws (ε=0.88, ε=0.68) from
`archives/datasets/kernel_test/abolition_reading.pl` and
`archives/datasets/kernel_test/kernel_run_02/abolition_reading.pl`, co-loaded through
`corpus_loader` with `corpus_path` overlaid at a scratch dir:

**Witness 1 — the operative single-instance barrier is the per-story
`:- module(constraint_<name>,[])` collision (load survives, exit 0).** Both files carrying the
same module declaration → the second throws on consult and is silently dropped; only one ε loads:

```
[corpus] Loading 2 testset files...
[corpus] WARNING: Failed to load …/abolition_reading_b.pl: error(permission_error(redefine,module,constraint_abolition_reading),context(module/2,…))
[corpus] SKIPPED: …/abolition_reading_b.pl
[corpus] Loaded 1 testsets successfully.
[witness1] abolition_reading epsilon values loaded: [0.88]
[witness1] corpus_constraint ids: [abolition_reading]
=== EXIT CODE: 0 ===
(no config_violations.log written)        % DP-001 did NOT fire
```

**Witness 2 — DP-001 is the correct *complementary* observer-axis backstop (exit 1).** Renaming
only the second file's module (`constraint_abolition_reading_b`) so both files actually load
produces a fact-level chimera; DP-001 fires as designed:

```
[corpus] Loaded 2 testsets successfully.
ERROR: CS ERROR (OQ-25): reading abolition_reading has conflicting ε values [0.68,0.88] (must be single-valued per reading — DP-001); chimera load detected — see docs/cs_load_discipline.md
1 config violation(s) after corpus load. See config_violations.log. Halting.
=== EXIT CODE: 1 ===
```

**Close.** A12 (committer multi-UID render) and DP-001 (observer one-ε seal) are the two halves
of the intended two-axis model, not a tension. A12's render branch is **correct** (shipped test
`prolog/tests/test_a12_multi_instance_render.pl`), but its trigger — a shared-ε, committer-varied
replicate set (one name → N UIDs, one ε) — **has no demonstrated populator**: stochastic
generation gives each draw a *different* ε (OQ-26 / Axiom 2), i.e. exactly the conflicting-ε
chimera DP-001 rejects. So (b) is a **declared design absence**, not a pending witness.
**Reopen condition:** a generation mode that canonicalizes ε per reading (committer variation as
the only multi-instance axis) would produce the set A12 needs; if such a populator is named,
OQ-21(b) reopens and Option 2 (replicate multi-instance loader) becomes the build. No code change.

## 2026-06-25 — OQ-21(a) RESOLVED: A12 multi-instance selector — dead recency clause fixed, @< pinned
**Files:** prolog/json_report.pl, prolog/tests/test_a12_multi_instance_render.pl, ISSUES.md
**Tier:** landed

The positive control written to close OQ-21(a) found a real defect instead of confirming
correctness. In `write_per_constraint_entry/4` the multi-instance branch's documented "pick
latest instance by `cs_created_at`" path was DEAD: `aggregate_all(max(T-U), …, max(_-UID))`
evaluates `T-U` as **arithmetic**, and UIDs are atoms (UUIDs), so it throws
`type_error(evaluable, …)`, is swallowed by the surrounding `catch(_, fail)`, and *always*
falls through to the `msort/last` `@<` fallback. Selection has been by `@<` UID-order, never
by timestamp, for the branch's whole life. "Verified by manual dual-consult" read the comment's
intent, not the code's behavior.

**Reusable Prolog tripwire:** `aggregate_all(max(Key-Val), …, max(_-Witness))` — the common
argmax idiom — **evaluates `Key-Val` arithmetically** and throws on non-numeric (atom) keys. A
`catch/…fail` around it then silently degrades to whatever the fallback is. Witness both the
firing AND the fallback before trusting such a selector.

**Ruling (operator): `@<` is canonical; recency is the WRONG selector** — instances of one name
are parallel draws, not versions (determinism frontier), so there is no canonical-latest. Only
live correctness-bearing consumer of the selected fields is `orbit_operator.py`'s committer
terminal-projection orbit (via `cs_drift_terminal`); it needs determinism+stability, which
standard order of UID atoms supplies (never reads timestamps). Dead clause removed; `@<` is the
sole selector; in-code comment now carries the parallel-draws reason so the bug can't grow back.
Behavior-preserving on the live corpus (81 names / 81 `cs_story_uid` facts — branch never fires).
Test pins `@<` with bundle coherence + a recency-pin; positive control witnessed t1 RED under
reintroduced recency selection. (a) resolved; (b) pipeline-firing open, gated on a future
multi-instance load (OQ-17 pointer is stale — disposed). Commit `cfb5fa03`; `[GATE]` GREEN.

---

## 2026-06-25 — OQ-19 RESOLVED: drift-trajectory trigger thresholds made durable + fail-loud
**Files:** python/enhanced_report.py, python/tests/test_drift_trajectory_granularity.py, ISSUES.md
**Tier:** landed

Closed OQ-19 (temporal-shape trigger magic numbers). Single-file, behavior-preserving:
hoisted the 6 `build_drift_trajectory_section` thresholds (7 occurrences) into a named
`_DRIFT_*` constant block keyed to `_DRIFT_MEASUREMENT_GRANULARITY = 0.01`; Trigger A is
encoded *derived* (`4 * _DRIFT_MEASUREMENT_GRANULARITY == 0.04`, IEEE-754 byte-identical
to the literal — witnessed), B/C stay literals (empirically tuned, not granularity-
derived). Added `_series_granularity` guard that prepends `[CALIBRATION WARNING]` when a
rendered constraint's series are finer than the floor.

**Correction-to-the-OQ-premise (worth a cold read):** the original OQ-19 entry and the
plan both assumed "live data is 2-decimal today." FALSE as of this corpus — 4 constraints
(`longevity_mismatch`, `propagation_speed_asymmetry`, `protein_anabolic_resistance`,
`validation_judgment_separation`) carry **authored** (not projected) 3-decimal values.
None currently fire a trigger, so the guard is inert on rendered output (29 live sections,
0 warnings), but the feared finer-granularity regime is already partly present in authored
data — making the guard more valuable, not less. Witnesses (float kill-condition, grep
completeness 7→0, byte-identical per-trigger A/B/C diff vs HEAD, positive-control test) in
ISSUES.md OQ-19 resolution block.

**Promotion test:** history-only. A future agent editing the trajectory section now sees
named constants + an in-code guard + a granularity NOTE comment, so the silent-
miscalibration trap is structurally removed — no CLAUDE.md promotion needed.

---

## 2026-06-25 — OQ-182 C-null PASS: HAC structural families validated as MEANING-bearing (testsets/ leg)
**Files:** audits/2026-06-25_oq182_trajectory_revive/c_null_harness.pl, audits/2026-06-25_oq182_trajectory_revive/c_null_results.log, audits/2026-06-25_oq182_trajectory_revive/c_null_distribution.json, audits/2026-06-25_oq182_trajectory_revive/c_null_protocol_FROZEN.md, audits/2026-06-25_oq182_trajectory_revive/c2_domain_finding.md, ISSUES.md
**Tier:** landed

Spend-tier C-null leg of OQ-182 (plan `~/.claude/plans/bright-jumping-cocke.md`). Standalone control-first
Prolog harness in the audit dir; **no engine edits** (`config.pl trajectory_enabled` stays `0`; `git status`
shows only the audit dir + docs). Commentary-only invariant intact.

- **VERDICT: PASS — family product validated meaning-bearing.** RealSil = **0.161119** (97 clustered
  constraints, 11 families) > **P95(null) = −0.026436** over **200 per-component-independent shuffle draws**
  (0 degenerate). **0/200 null draws reach RealSil** — real lies beyond the *entire* null. TEETH PASS
  (null_median −0.0945 < RealSil; standardized gap **+5.01σ**). Null family-count centers at **15 vs real
  11** — the frozen doc's predicted false-FAIL-leaning direction, so the PASS is conservative. Reproducible
  under seed `20260625` (run-twice → identical P95; SWI 9.2.9; Python percentile cross-check matches every
  statistic).
- **Control-first, all pasted BEFORE the verdict and gating it:** INTERNAL-CHECK (Σ w_k·comp_k == engine
  `pair_dist`, max-diff 0.0), GROUPING-FIDELITY (`make_groups@identity` == engine `group_by_shift`, 26
  groups), FIDELITY (`P0 == RealPartition`, |S0−RealSil|=0), JOINT-TOOTHLESS (S_joint = RealSil to 1e-16,
  relabel-match=yes — the false-PASS the per-component design avoids, demonstrated), TIE-BREAK (overlay
  regime σ-pure). The per-component-vs-joint contrast is the teeth-witness.
- **MECHANISM CORRECTION (frozen quantities unchanged).** The frozen "Chimera surgery map" was
  mechanically wrong: `group_by_shift/2` keys the shift pre-grouping via
  `logical_fingerprint:fingerprint_shift/2` on the **constraint identity**, ignoring `trajectory_cached` —
  so a chimera `trajectory_cached` + `run_hierarchical_clustering/1` pins shift grouping to real boundaries
  regardless of σ_shift (toothless / false-PASS). The harness builds shift-groups itself (`make_groups/4`,
  keyed on `fingerprint_shift(C[σ_shift(i)])`) and reuses only `cluster_all_groups/2` + `assign_families/1`;
  the per-component shuffle is a pure index recombination over precomputed real component matrices. Erratum
  in `c_null_protocol_FROZEN.md`.
- **Scope:** families = safe + stable + **meaning-bearing**; **twins remain OPEN** (parallel report: 448
  twins / 4656 pairs, near-vacuous cross-domain gate; deferred to rebuild). Remaining OQ-182 legs: C0
  pipeline-diff corroboration, C-gen (haiku↔flash), kernel_v1 re-checkpoint, then the gate flip.

---

## 2026-06-25 — OQ-182 minted: revive + validate the dormant HAC trajectory-mining subsystem (cheap tier)
**Files:** prolog/context_profile_mining.pl, prolog/config.pl, prolog/isomorphism_engine.pl, prolog/constraint_bridge.pl, prolog/report_generator.pl, ISSUES.md, audits/2026-06-25_oq182_trajectory_revive/
**Tier:** landed

Cheap tier of the OQ-182 plan (`~/.claude/plans/fancy-splashing-pancake.md`). The plan was authored
naming "OQ-180"; that label was already taken by the OQ-51 build (commit `cef5dc6e`), so this work is
**OQ-182** (highest pre-existing was OQ-181). Audit dir: `audits/2026-06-25_oq182_trajectory_revive/`.

- **C-prov PASS (testsets/ leg, witnessed).** `prolog/context_profile_mining:trajectory_run/2` on
  `testsets/` (104 `corpus_constraint/1`; 97 yield trajectories → 11 families, 448 twins) leaves BOTH
  `classify_at_time_eps` and `classify_at_time_theater` globals **unset** — so the 2 passive
  `nb_getval` leaf reads (`drl_core.pl:306`, `boltzmann_compliance.pl:510`) fall back to authored
  `constraint_metric`, no imputed `BaseX=0.5`/`0.10` coupling. Excluded-constraint count = 0. Positive
  control (separate process): `classify_at_time/4` on an **on-grid** constraint+Time DOES set
  `classify_at_time_eps = eps(...,0.03)` — the probe is sensitive. **Note (OQ-178 trap, hit live):** the
  first positive-control draw at `Time=0` on an off-grid constraint returned `unknown` and bailed before
  the `nb_setval`; the control only proves sensitivity when fed a Time on the constraint's authored
  suppression grid. Witness: `c_prov_runtime.log`. **The same global-unset check re-runs on `kernel_v1`
  in the spend tier — C-prov gates both corpora, not just the narrow one.**
- **Cross-domain-twin fork verdict (Step 2, log-only, behavior-preserving).**
  `context_profile_mining:cross_domain_twins/3` is the **canonical** producer (live via
  `context_profile_report`, reachable). `isomorphism_engine.pl` is a **loaded-but-non-executing**
  Pattern-2 fork: loaded via `constraint_bridge.pl:11` + `report_generator.pl:31` (both in `stack.pl`),
  but all callers dead — `constraint_bridge:check_for_social_twins/2` (NOT in the export list at
  `constraint_bridge.pl:2–5`, never called), `report_generator:cross_domain_audit/0` (defined, never
  called), `isomorphism_report.pl` (NOT in `stack.pl`, unwired) → its `generate_cross_domain_index/1`
  has no live caller. Positive control: the grep DID find these sites, so a live caller would have
  surfaced. **NOT deleted** — deletion is a separate multi-file output-neutral cleanup (2 `use_module` +
  3 dead call sites) with its own diff-witness; mint as its own OQ if wanted. See `design_gaps.md` GAP-20.
- **Spend tier (C0/C1/C2/C3/C-null, gate flip) is gated behind an operator checkpoint** — not yet run.

## 2026-06-25 — OQ-51 main build: `unknown` is N/A on the canonical sheaf/H1 path (commits `f8ae0c9c` + `15cca7ed`)
**Files:** prolog/grothendieck_cohomology.pl, prolog/sheaf_analysis.pl, prolog/json_report.pl, prolog/product_site_export.pl, python/shared/schemas.py, python/shared/loader.py, python/w1_sheaf_join.py, python/enhanced_report.py, python/orbit_characterization.py, python/run_drift_mismatch.py, python/sweeps/epsilon_sensitivity.py, python/sweeps/range_sweep.py, python/sweeps/product_site_delta_sweep.py
**Tier:** tripwire

OQ-51's main build item landed (the cs_kernel_comparison site was f456896b; this is the
canonical path). `unknown` is N/A — not a disagreeing type, not a value that agrees with itself.

**Standing tripwires for a fresh agent:**
- **`h1_band` in `pipeline_output.json` is now NULLABLE.** null = UNDETERMINED (`<2` real seats —
  the obstruction is N/A, NOT 0). Any new reader MUST handle null, never `.get("h1_band", 0)` /
  `... or 0` — that silently reads undetermined/manifest as genuine. Use
  `shared.loader.h1_band_or_raise(entry, source)` (fails loud, distinguishing key-absent=stale
  artifact from null=undetermined). Same for the **product-site `"h1"`/`"h0"`** in
  `product_site_orbits.json` (separate file; `None > 0` CRASHES — null-guard it).
- **`sheaf_status` gains a 4th value `undetermined`** via TWO routes with distinct provenance in the
  sibling field `sheaf_undetermined_reason` (`insufficient_seats` | `uncomputable_height`). Route 2
  is **h1==0 AND undetermined** (height uncomputable) — so `undetermined ⟺ h1==null` is NOT an iff.
  The true partition (asserted by `w1_sheaf_join`): manifest⟺h1>0; genuine/fragile⟹h1==0;
  undetermined⟹h1∈{null,0}.
- **`arakelov_height/2` needs MaxEnt that a bare `[stack]` load does NOT populate** (computes for
  0/104 in a bare context; `arakelov_height.pl:16-18` "pipeline diagnostic only"). So a bare-context
  probe of route 2 (`uncomputable_height`) is an ARTIFACT — every h1=0 reads route-2. Route-2
  liveness is **pipeline-authoritative** (`pipeline_output.json`); in the live pipeline it is dormant
  (route 1=15, route 2=0). Any future route-2 census needs the arakelov-computable positive control
  (see `tests/test_sheaf_na.pl`). **A pipeline reorder that serializes `sheaf_status` before
  `maxent_run` would silently turn EVERY h1=0 constraint undetermined** — guarded by `w1_sheaf_join`
  Control 2b (dies if no h1=0 constraint is genuine/fragile); two-sided witnessed. Full mechanism +
  the ordering hazard: `docs/technical/sheaf_status_maxent_ordering.md`.

Witness: test_sheaf_na 10/10 + live route-1=15; dynamic suite 0 errors; pipeline exit 0 + mtime
advanced; schema gate green; diff 26 h1_band / 22 sheaf_status moves, 15 undetermined; w1
partition_ok; containment trips loud; 0 partition violations on testsets_haiku(960)/flash(960)/
kernel_v1(1106). Branch `oq51-sheaf-na-canonical`. Residual: OQ-180 (sibling `\=` + 3 audit-dir
silent sites), OQ-181 (per-site undetermined semantics for the 13 readers + `load_per_constraint`).

---

## 2026-06-25 — fix: OQ-57-class wrong-qualifier in the dormant trajectory-mining path (commit `fc9b4688`)
**Files:** prolog/context_profile_mining.pl, prolog/check_stack.pl
**Tier:** landed

Surfaced during the OQ-16 rename (rename-independent — byte-identical pre/post). `standard_contexts/1`
called `dirac_classification:standard_context/1`, removed 2026-06-02 (dirac kept `gauge_orbit/2`,
`preserved_under_context_shift/2` — both still valid — but deleted its local `standard_context/1`;
see `dirac_classification.pl:115`). Re-qualified to `drl_core:standard_context/1`, which retains the
identical 4-context generator (verified to enumerate the same 4 canonical contexts as
`constraint_indexing:site_contexts/1`). It was the **only** rotted call in the file.

**Witness:** the report generator (run_pipeline's exact load chain + `run_trajectory_report`) now
exits 0 with no existence/unknown-procedure errors, all 4 contexts processed, **135-line report
produced** (was crash → empty). Production path unchanged (`trajectory_enabled=0`); fix only bites
when enabled. **Why it sat unnoticed:** `context_profile_mining.pl` is NOT loaded by `[stack]` (only
in the separate trajectory chain), so `check_stack.pl`'s undefined-predicate scan never saw it.
**Gap closed (commit `a82d7ed0`):** `check_stack.pl` now loads the trajectory chain faithfully
(mirrors run_pipeline `_prolog_trajectory`) before `check/0`, so wrong-qualifier rot in
`context_profile_mining.pl`/`context_profile_report.pl` is now caught — positive-controlled
(reintroducing the bug makes check flag it), baseline unchanged (same 5 known undefineds). Honest
boundary recorded in-file: the OTHER standalone report scripts (abductive/orbit/fingerprint/…
report) remain uncovered — co-loading non-module scripts into one image cross-contaminates;
a faithful per-chain check needs a fresh process per chain (larger item, not done).

**Forward (not done here):** the crash is gone, but *validating* the now-runnable trajectory-mining
(HAC structural-family) output / deciding whether to revive the subsystem is the revive-or-gap design
call — OQ-91-adjacent (OQ-91 itself is the sibling `transition_paths`/repair-transition thread, a
distinct dormant module). No new defect OQ minted; the fix removed the sharp edge per the
fix-simple-errors ruling.

---

## 2026-06-25 — OQ-16 RESOLVED: temporal vocabulary rename pass (name-only, 5 renames, 3 commits)
**Files:** prolog/metric_drift_events.pl, prolog/metric_drift_report.pl, prolog/context_profile_mining.pl, prolog/context_profile_report.pl, prolog/network_dynamics.pl, prolog/stack.pl, prolog/drl_lifecycle.pl, prolog/transition_paths.pl, prolog/cs_pattern_detection.pl, prolog/cache_registry.pl, python/run_pipeline.py, scripts/pipeline_dashboard.sh, ISSUES.md
**Tier:** landed

Executed the deferred "drift"/"trajectory" rename pass — the words each named two
different concepts on opposite axes (metric/network drift ≠ CS commitment-drift;
observer-context "trajectory" ≠ CS commitment-trajectory). Name-only, no logic/threshold
moved. Five renames in three commits:

- `0a204af1` — predicate `detect_network_drift/3 → detect_network_contamination/3`
  (network_dynamics.pl + all qualified callers + the `drl_lifecycle` facade call).
- `1d861cee` — file+module renames `drift_events→metric_drift_events`,
  `drift_report→metric_drift_report`, `trajectory_mining→context_profile_mining`,
  `trajectory_report→context_profile_report` (file only, no module decl); imports/reexports/
  load order; `run_pipeline.py` + dashboard + `.legacy` output paths
  `trajectory_report.md → context_profile_report.md`.
- `1bcc07c5` — genuine code-pointer tokens across 15 live reference/implementation/design docs.

**Operator rulings:** `metric_*` over `dr_*` (no `dr_` scheme exists today; `cs_` is a concept
marker, not a file-prefix convention) — so `dr_` would be a lone scheme splitting the cluster.
One complete pass (sources + generated `.md` + genuine doc refs) so no half-renamed mismatch is
manufactured. **Left out of scope (logged, not missed):** JSON output field `drift_events`
(`json_report.pl`, python schemas), internal predicate `run_trajectory_report`, doc *filenames*,
and dated recon/essay docs (`recon_2_scope*.md`, `when_frame_isnt_foreground.md`) where the old
name is the subject of a historical narrative.

**Witness:** `[stack]` loads ok; `detect_network_contamination/3` present, `detect_network_drift/3`
absent; `[abductive_triggers]` loads through the reexport facade; `check_stack.pl` clean (positive
control for a missed qualifier); full `run_pipeline.py` exit 0 writing `context_profile_report.md`;
dashboard reads the renamed path. **Promotion test:** no tripwire — a missed reference fails
*loudly* at load (existence_error) or is caught by `check_stack.pl`, so this stays history, not a
promoted warning. **Side-finding (rename-independent — surfaced here, FIXED separately):**
`context_profile_mining.pl` called `dirac_classification:standard_context/1`, which
`dirac_classification.pl` deliberately removed (comment :115) — a pre-existing dangling call in the
production-disabled (`trajectory_enabled=0`) trajectory path, byte-identical pre/post rename, so not
an OQ-16 regression. Resolved by a concurrent instance (`fc9b4688`): re-qualified to
`drl_core:standard_context/1`, and `check_stack.pl` extended to load the trajectory chain so the
class is caught going forward (`a82d7ed0`). Authoritative entry: the standard_context fix entry above
(KNOWN_STATE 2026-06-25) + `swipl_load_path_and_probe_gotchas.md` §1 (loaded-image coverage boundary).

**Doc-scope refinement (commit `76eae0c1`, operator ruling 2026-06-25):** the 4 dated
recon/essay docs (`recon_2_scope.md`, `recon_2_scope_v2.md`, `when_frame_isnt_foreground.md`,
`commitment_systems/construction_over_inspection.md`) are NOT untouched — their **bodies are
preserved** as dated records, but each got a **per-doc end-note** pointing at the OQ-16 rename
table (only the renames appearing in that doc). Confirmed narrative-only first (no live
`see prolog/X` pointer) before preserving. **Final-grep exclusion (record so a future run reads
remaining hits as intentional-preserved, not a missed rename):** old tokens still legitimately
appear in (a) the JSON output field `drift_events` — `json_report.pl`, ~10 python files,
`report_sidecar_schema.json`, `diagnostic_integration_architecture.md:42`; (b) the 4 historical
docs above (body + end-note); (c) verbatim external review transcripts `docs/review/expansions.txt`
+ `docs/review/jaynesian-gemini.txt` (quoted `detect_network_drift/3`). A correct
"no dangling refs" grep excludes these basenames; every other old-token site is gone. **Note:**
this pass interleaved on `main` with a concurrent instance that committed the live docs (`1bcc07c5`)
and the close-out (`fb45c0e3`) — outcomes converged (it referenced these same hashes), but 6 claude
instances were running; multi-writer hazard per CLAUDE.md.

---

## 2026-06-25 — OQ-39 RESOLVED: scaffold rising-suppression gets a COMMENTARY verdict (rows 14–18 disposed)
**Files:** prolog/cs_pattern_detection.pl, prolog/tests/test_oq39_scaffold_escalation.pl, ISSUES.md
**Tier:** tripwire

OQ-39 row 14 (scaffold "suppression must decline over time", no engine enforcer) resolved **by
commentary, not gate-vs-drop** (operator ruling). Reclassifying a rising-suppression scaffold to
rope/tangled_rope would assert *coercion* the evidence doesn't show — it only shows the decline rule
is violated. New clause `cs_verdict(C, scaffold_suppression_escalating)` (commentary-grade,
annotate-only; flows to the `cs_verdicts` output field, touches no classification/override path)
fires when a constraint certifies `scaffold` at any standard context AND its authored
`suppression_requirement` *series* is rising (`drift_events:metric_trend`). **14 live constraints
fire** (witnessed; cross-checked against an independent inline probe — same 14).

**Cross-leg finding:** rising:falling ≈ 5–6:1 in every leg (testsets/ 13:2, haiku 53:7, flash 43:9
@ institutional). The two reconciled legs share one generation prompt → this rules out one model's
idiosyncrasy (NOT prompt-independence). Since the rule *is* a generation-prompt rule, the sharp
reading: the prompt's own "suppression declines" instruction is systematically not honored by
generation — which strengthens the commentary case. (A strict "require decline" gate would deny
18/20 institutional scaffolds; "deny on rising" 13–14/20 — both large reclassifications the ruling
rejects.) `metric_trend/3` reads the `measurement/5` series directly (earliest→latest delta); its
consumers do not route through `classify_at_time`, so the check is time-independent and **moot to
OQ-178's off-grid Time=0 wrinkle**.

Rows 15–18 closed: 15 (final-measurement==base_extractiveness) no validator, low-stakes,
positive-controlled absence; 16 (piton atrophy) enforcer exists (`coordination_dead/1` wired into
`classify_from_metrics/6`); 17 (Goodhart) leave diagnostic-only (`detect_metric_substitution/1`
report-path only); 18 (perspective-min) lives correctly at the linter eval surface, not an engine
enforcer.

**Tripwire — `cs_verdict/2` clause placement/cut gotcha.** Every existing `cs_verdict` clause ends
in `!`, harmless among themselves because each is gated on a DISTINCT single-valued `cs_pattern`
(mutually exclusive). A NEW clause gated on something ORTHOGONAL to `cs_pattern` (here
`dr_type=scaffold`) is NOT mutually exclusive: placed BELOW the family, an earlier clause's `!`
silently prunes it on a constraint that matches both; given a trailing `!`, it prunes the others.
**Rule: a new orthogonally-gated `cs_verdict` clause MUST be the FIRST clause and commit with
`once/1` (local cut over inner goals only — NO trailing `!`)**, leaving sibling clauses reachable so
`findall` gathers this verdict PLUS any `cs_pattern` verdict. Proven by the cut-regression control in
`tests/test_oq39_scaffold_escalation.pl` (a dual-verdict constraint carries BOTH). Mode note: the
clause needs C BOUND (it calls `dr_type(C,...)`); the production consumer (`json_report.pl:562`)
always binds C, but a `findall(C, cs_verdict(C, scaffold_suppression_escalating), _)` with C unbound
returns 0 — query by iterating `corpus_constraint/1`.

## 2026-06-25 — OQ-178/179 SUPERSEDED/RESOLVED: cs_kernel_divergence reverts to static `dr_type/3` (time-neutral)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, ISSUES.md
**Tier:** tripwire

`cs_kernel_divergence/4` and the `compare_kernel_readings/3` JOIN now classify with static
`dr_type/3` (time-neutral), mirroring `perspectival_incoherence` — reverting the interim OQ-178
latest-snapshot probe-fix (`9fde36c9`). Commit `5b069ae1`.

**Tripwire:** a `cs_*` cross-reading comparator uses static `dr_type/3` — its moving axis is
reading/perspective, NOT time. Do NOT wire it to the DR `measurement/5` series / `classify_at_time`:
that crosses the DR temporal element into a CS-layer predicate. *Latest*-snapshot specifically reads
a COLLAPSING constraint at its terminus (latest authored ε can be 0 → `unknown`; `unknown==unknown`
reads as agreement, masking real divergence). The CS lifecycle trajectory
(`cs_reference_frame`→`cs_drift_state`→`cs_drift_trajectory`) is a SEPARATE temporal element.
*(Promotion test: NOT promoted to CLAUDE.md — the `cs_kernel_registry.pl` header (lines ~14-23) now
carries this warning in-code, so a fresh editor sees it before touching the predicate; the
silent-mistake risk is covered at the edit site.)*

**Witness (probe == regenerated `json_report`):** live corpus n=97, `cs_kernel_divergence_count`
16→18, kernels 8→8. The +2 recovered pairs are both `visual_evidentiary_authority`
(`post_evidentiary` × `indexical_realism` / × `distributed_verification`), genuine type≠type
(`snare ≠ tangled_rope`/`naturalized`), zero unknown-pairings (OQ-37 artifact did NOT occur). Twin
corpora corroborate the direction: `testsets_haiku` 861→893 (+32, +3 kernels), `testsets_flash`
813→846 (+33, +4). `shinbutsu` (the interim audit's collapse exemplar) is now a SINGLETON live
reading → no live pair; the reversal stands on the principle. OQ-179 closed mis-premised; its genuine
DR-axis observation (sibling readings change DR-type across their own grids) re-homed to the DR
temporal subsystem (`drift_trajectory`/`temporal_residual`, OQ-110 family). OQ-105 BC-encoding fold
moot for this path (static `dr_type` never takes `max(T)`).

---

## 2026-06-25 — OQ-51 build-extension RESOLVED: `unknown` is N/A in cs_kernel_comparison (trichotomy + divergence enumeration)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/enhanced_report.py, prolog/tests/test_cs_kernel_registry.pl
**Tier:** landed

Applied OQ-51's N/A rule (`unknown` = not-agree, not-diverge) to the `cs_kernel_comparison`
surface — the site the original OQ-51 build never enumerated, surfaced by the OQ-178 audit
(all-`unknown` context was scored `agree(unknown)`, inflating robustness).

- **Verdict trichotomy** (`ctx_reading_verdict/2`): `agree(Type,NUnk)` / `diverge(TypeMap,NUnk)` /
  `undetermined(NReal,NUnk)`. Each carries `NUnk` = #unknown readings so abstention reads uniformly
  off the verdict (`verdict_unknown_count/2`). LENIENT (operator ruling): ≥2 real readings ⇒ verdict
  over the real ones; a lone unknown does NOT demote (strict = absence-as-presence reversed).
- **`cs_kernel_divergence/4`** and **`pair_reading_agreement/7`** now require BOTH types real before
  counting agree/diverge (shared `is_real_type/1`). Load-bearing for the join invariant
  (Σ DivergeN == #cs_kernel_divergence) — must not refactor back to bare `\=`. Jaccard = `null` when
  the pair has no comparable (both-real) context (was a misleading 1.0).
- **JSON**: `specific_context_count` → `divergent_context_count` (recomputed as `#diverge`, NOT
  relabelled NCtx−robust); new `undetermined_context_count`, `abstaining_context_count`
  (cross-cutting — a context can be agree AND abstaining; NOT a 4th partition cell), and
  `divergence_patterns` (deliverable ii: ENUMERATES the disagreement, keyed on the real-typed submap
  — abstention carried as sub-annotation, never in the key — capped at top 5 with a
  `divergence_patterns_truncated` notice). Partition: `robust + divergent + undetermined == total`.
- **Report** (`build_kernel_reading_section`) now renders the distribution + the divergence
  enumeration (`diverges: settler=snare / cultural=scaffold (117 contexts)`) instead of bare counts.

**Two silent footguns fixed:** (HOLE A) `write_jaccard_pair`'s `~6f` threw on a null Jaccard and
aborted the whole JSON write — now branches to literal `null`; (HOLE B) `enhanced_report`'s `:.3f`
threw on `None` — now renders `n/a`. (Arity fold A) `json_report.pl:2024`'s `agree(_)` would
SILENTLY fail-match the new `agree/2` token → RobustN=0; updated to arity-2.

**Witnesses (this commit):** unit suite 20/20 (incl. 6 synthetic N/A controls + join invariant);
dynamic suite 0 errors; pipeline exit 0, `pipeline_output.json` rewritten; partition invariant
9/9 kernels; `cs_kernel_divergence_count` 20→16, `cs_kernels_with_divergence` 9→8 (actinide's only
"divergence" was unknown-vs-real — now correctly 0 real divergences, 117 undetermined where the old
report falsely read 117 reading-specific); JSP report enumerates settler=snare/cultural=scaffold.
**Note:** robust_context_count can RISE on abstention-heavy real-agreement kernels (performance_legitimacy
21→147) — the lenient rule reclassifying real-agree-with-abstention from specific→robust; this is the
RULING applied (Blast-radius prose under-predicted the direction; the output is the authority).
**Note (dormant in serialized output — do not misread a no-op diff):** the all-unknown→`agree(unknown)`
robust-INFLATION case (the OQ-178 motivator) fires on **0 of the 9 serialized kernels** — each has 0
all-unknown contexts, so no serialized `robust_context_count` drops via this path; a `pipeline_output.json`
diff shows the inflation correction NOWHERE. It is witnessed by the synthetic control
`na_rule_all_unknown_is_undetermined` and in-predicate on 13 NON-serialized singleton kernels
(`doomsday_clock_metric` 120 all-unknown ctxs, `maat_order_principle` 126, `gita_kurukshetra` 76 …),
which the `L>=2` filter (`json_report.pl:1734`) excludes from output. The OQ-178 witnessed `robust 0→156`
required JSP's two readings to fail-close to `unknown`; live they are real-typed (snare/scaffold), so the
live serialized robust effect is the abstention-tolerant RISE (performance_legitimacy 21→147), not a drop.
Join invariant Σ DivergeN == #cs_kernel_divergence holds **9/9 live** serialized kernels (the plan's 42/42
was the `testsets_haiku` twin corpus, not this leg). **Note:** no live kernel currently has a zero-comparable
pair, so the null-Jaccard path is witnessed by the synthetic unit test + direct writer/guard probes, not live
data. Scope: only `cs_kernel_comparison`; the original OQ-51 `count_disagreeing_pairs`/`sheaf_status`/H1 sites remain
OQ-51's separate open item. Console drift: `cs_corpus_analysis.pl:110` divergence count drops
(expected). OQ-119 probes/exports see fewer divergences (expected).

## 2026-06-24 — OQ-37..41 census Pass 1: 2 strips landed; OQ-41 BaseX=0.5 is off-grid, not absence; OQ-178 minted
**Files:** prolog/data_validation.pl, prolog/drl_composition.pl, prolog/cs_kernel_registry.pl, ISSUES.md, CLAUDE.md, audits/2026-06-24_oq41_basex_t0/
**Tier:** correction-key

Implementing the OQ-37..41 census plan. **Landed (behavior-preserving, commit `1eacd2fc`):**
stripped the vacuous `resistance_to_change`-keyed piton sub-check in `validate_edge_cases/0`
(superseded by OQ-90) and `predict_transformation/3` (0 callers; helpers `linear_slope`/
`slope_accum` now orphaned → OQ-38 clause-pass candidates). Item-3 `inevitability` read was
already stripped in a prior session.

**Correction-key — the temporal path is LIVE, not dormant (overturns prior OQ-41 text):**
`classify_at_time` is consumed live by `cs_kernel_registry` (probes at **Time=0**, feeding
pipeline `validation.cs_kernel_*`), `temporal_residual`, `boltzmann_compliance`, `drl_core` —
NOT only via the dormant `constraint_history`/`snapshot_type`/`degradation_chain`. So the OQ-41
rows 24–25 `BaseX=0.5` site is live at Time=0. Fail-closing it (the OQ-44 reflex, attempted then
REVERTED) is output-changing (`cs_kernel_divergence_count` 17→16) and WRONG: all 15 affected
constraints author `base_extractiveness` as a temporal series at real years (none at the synthetic
Time=0) — 0/15 genuinely absent. The default is OFF-GRID PROBING, not absence; fail-closing erases
a real `snare`-vs-`scaffold` divergence (`jewish_sovereignty_palestine`). **Fix RESOLVED via
OQ-178 (2026-06-25, commit `9fde36c9`):** `cs_kernel_registry` now reads each reading at its LATEST
authored time (`reading_snapshot_time/2`), not the synthetic Time=0 — falsifier resolved (output
not time-aligned → per-reading-own-time). Witness: divergence count 17→20, JSP preserved, 0/15 still
off-grid, invariant 42/42, 32 readings re-based from authored ε. Single-snapshot is lossy (9/15
readings change type across grids) → trajectory successor **OQ-179**. OQ-39 row 14 reopened (same
premise); OQ-51 build-extension logged (cs_kernel_comparison counts unknown==unknown as agreement).
Audit: `audits/2026-06-24_oq41_basex_t0/`.

**Tripwire (reusable):** when witnessing behavior-preservation via a `pipeline_output.json` diff,
a `run_pipeline.py` whose **load-warning-gate aborts** (e.g. a `*/` inside a `/* */` Prolog comment)
exits non-zero and does NOT rewrite the output — so the diff reads FALSE-IDENTICAL against the
stale file. Always check exit code AND the output mtime changed before trusting a "byte-identical"
pipeline diff (Pattern 6).

---

## 2026-06-23 — OQ-15 RESOLVED (core): cross-axis taint guard LANDED, Phase 2 ruled policed-in-place
**Files:** prolog/check_axis_boundary.pl, prolog/axis_boundary_allowlist.txt, python/check_axis_boundary.py, python/run_pipeline.py, prolog/tests/axis_boundary_ctl_run1.pl, prolog/tests/axis_boundary_ctl_run2.pl, prolog/tests/axis_boundary_ctl_payload_widen.pl, prolog/tests/axis_boundary_ctl_nonbridge_seam.pl, scripts/gate.sh, ISSUES.md, docs/design/design_gaps.md
**Tier:** landed

Resolved the load-bearing half of OQ-15 (= v8 §8 item 1 / OQ-135 priority-1 artifact;
closes GAP-12). Commits `c6fe7edb` (Phase 0a/0b), `fd1ee561` (guard).

- **Phase 0a witnesses** (`audits/2026-06-23_oq15_crossaxis_witnesses/`, read-only):
  W1 MIXED (cs_drift_mismatch reaches observer machinery *transitively* via
  cs_is_metric_stable → grep is blind → guard load-bearing); W2 the `influences`
  bridge is the *unique committer→observer* dataflow (bucket-1 comparisons run the
  other direction); BC no runtime back-channel — re-witnessed engine-wide 2026-06-24
  (`bc_rewitness.txt`): non-vacuous probe (flags a planted cs_ assert) + complete
  assert-target enumeration → zero cs_ committer facts written at runtime. STATIC
  witness only ("found none," not a runtime snapshot-diff), and a SEPARATE surface
  from the guard (guard = static reads; writes = this enumeration). Corrected from the
  original inspection-only read (which swept only cs_*/drl_*). XR/SA confirmed. **constraint_bridge.pl `compute_veto_actors` is NOT cross-axis**
  (reads dr_type + authored `constraint_beneficiary` substrate, no cs_) — the plan's
  "reverse DR→CS read" hypothesis was *false*; NOT added to Files, NOT whitelisted.
- **The guard** (`check_axis_boundary.pl`): reachability over the LOADED call graph
  (clause/2, descends control constructs + meta-calls + **nested module qualifiers** —
  a missing-recursion blindness the positive controls caught before landing). Python
  harness diffs edges vs `axis_boundary_allowlist.txt` (load_warning_gate pattern,
  fail-closed); `--selftest` runs negative + 2 required controls (path-b payload widen,
  path-c non-influences seam — both fire). Wired into `scripts/gate.sh` (static check,
  no corpus). GATE GREEN; behavior-preserving (no engine file touched; guard absent
  from stack.pl/run_pipeline/corpus_loader load path).
- **Census beat the hand inventory:** 8 boundary edges; only 2 are observer-VERDICT
  reads (sanctioned `influences` bridge + bucket-3 `cs_kernel_id` exclusion → "exactly
  one forward bridge" confirmed in place). The other 6 are comparison/validation tooling
  (`axiom_diff`, `reading_diff`, `config_validation`) — modules OQ-15's `Files:` OMITTED.
- **W2 corrected (kind vs cardinality):** the relation-atom type system axis-segregates —
  `influences` (entailment, 38) read ONLY at the observer derivation; `forecloses` (47) /
  `coexists_with` (104) committer-modal, never cross. So single-bridge is principled-IN-KIND
  but "exactly one" is convention-not-theorem, guard-enforced-in-CARDINALITY. The earlier
  "principled" gloss asserted the conclusion W2 was scoped to test — dropped. Guard is
  corpus-INDEPENDENT (live/haiku/flash all → same 8 edges, byte-identical sets).
- **Phase 2 RULED policed-in-place (v8); core CLOSED (2026-06-24).** Operator's named reading:
  a green gate is sufficient; the boundary need not be source-legible today. The guard IS the
  resolution. **Synthesis (v7 named mediator) PRESERVED, not foreclosed** — v7 unbuilt-but-
  available; trigger = **a SECOND committer→observer bridge is proposed** (falsifiable,
  witness-tied; NOT "first legibility failure"), mechanically wired (such a bridge fires the
  guard RED → allowlist header → OQ-15 synthesis decision). The guard is now SOLE enforcement
  of a convention, so its two positive controls run in BOTH recurring gates (shown-firing):
  `scripts/gate.sh --selftest` AND `run_pipeline.py` (axis-boundary gate beside load-warning).
  Vocabulary migration remains human-gated under OQ-135. Bundled OQ-15 ↔ OQ-135.

---

## 2026-06-23 — OQ-06 RESOLVED: off-case fixtures witnessed for cs_drift_unacknowledged / cs_axiom_foreclosed
**Files:** prolog/cs_pattern_detection.pl, prolog/cs_axiom_engine.pl, prolog/narrative_ontology.pl, ISSUES.md
**Tier:** correction-key

All four off-case conjuncts now witnessed in BOTH directions (fires-when-it-should AND
stays-silent-when-it-should). Method: search all four real corpuses with a two-sided
planted control per off-bucket + per-corpus overlay fingerprint (Phase A), then a transient
matched-pair matrix (Phase C). Evidence: `audits/2026-06-23_oq06_offcase_fixtures/`.

Findings worth carrying:
- **Stale `Files:` corrected.** ISSUES.md OQ-06 pointed at `cs_drift_engine.pl` for the
  predicates; that file only *mentions* `cs_drift_unacknowledged/2` in a comment (lines
  34–35). The real definitions are `cs_pattern_detection.pl:412–416`
  (`cs_drift_unacknowledged/2`) and `cs_axiom_engine.pl:137–141` (`cs_axiom_foreclosed/2`).
- **`cs_axiom/3` is multifile-but-STATIC** in `narrative_ontology.pl` (NOT in the `:- dynamic`
  block, unlike `cs_drift_state/3` and `cs_axiom_grounding/3` which ARE dynamic). So
  `probe_harness:with_asserted` on `cs_axiom/3` throws `No permission to modify static
  procedure` — declare `dynamic(narrative_ontology:cs_axiom/3)` in the probe process first
  (does not change how readers see it; the process halts, no leak).
- **drift-C3 (Dir=stable + non-minor + unacknowledged) is a structural absence**, not a
  coverage gap: across all four corpuses, unacknowledged stable drifts are always minor and
  non-minor stable drifts are always acknowledged. The transient probe is its permanent
  witness — no synthetic fixture belongs in `testsets/` (THREE-LIVE-LEGS sparsity is intended).
- **Sequential multi-corpus scans must be one-corpus-per-process.** `load_all_testsets/0` is
  `corpus_loaded`-guarded (no-op after first load) and `consult` accumulates
  `narrative_ontology` facts — a one-process 4-corpus loop loads only corpus #1 and pollutes
  counts. (Already in CLAUDE.md Corpus Loading for the count-mismatch case; reinforced here.)

Promotion test: the stale-`Files:` correction is local to OQ-06 (now fixed in place, won't
re-mislead). The `cs_axiom/3` static-procedure gotcha is the candidate tripwire — but it is
narrow (only bites a probe that asserts `cs_axiom/3`) and fails LOUDLY (immediate permission
error, not silent), so it stays history here rather than promoting to an always-loaded section.

## 2026-06-23 — OQ-10 RESOLVED: reading-robustness as first-class report output (+ OQ-176 spawned)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/enhanced_report.py, prolog/tests/test_cs_kernel_registry.pl, ISSUES.md
**Tier:** landed

Added the summary/verdict layer OQ-10 needed. The comparison ENGINE already fired live
(`cs_kernel_divergence/4` + `write_kernel_comparison_entry` + `build_kernel_reading_section`);
the "no predicate/script/report section performs this comparison" premise was stale. New:
- `compare_kernel_readings/3` (cs_kernel_registry.pl, exported): per-context verdict profile over
  the SAME `classify_at_time/4` evaluations the divergence engine walks — a JOIN, not new compute
  (it makes FEWER classify_at_time calls than cs_kernel_divergence, which re-evals per pair).
  Invariant: Σ per-pair DivergeN == #cs_kernel_divergence solutions (166==166 on the live twin;
  unit test `compare_join_consistency_with_divergence_engine`, corpus-independent). **SUPERSEDED
  2026-06-25 by the OQ-51 trichotomy** — verdict tokens are now `agree(Type,NUnk)` /
  `diverge(TypeMap,NUnk)` / `undetermined(NReal,NUnk)`; see the 2026-06-25 entry.
- `pipeline_output.json` `validation.cs_kernel_comparison[].reading_robustness` object fields:
  `total_contexts`, `robust_context_count`, `divergent_context_count` (**renamed from
  `specific_context_count` 2026-06-25**), `undetermined_context_count`/`abstaining_context_count`/
  `divergence_patterns`/`divergence_patterns_truncated` (NEW 2026-06-25, OQ-51), `h1_band_robust`
  (true/false/null — null = fail-closed on missing H¹), `per_reading_h1[]`, `pairwise_jaccard[]`.
  Jaccard is CONTEXT-ALIGNED over presheaf section graphs (global-vocabulary Jaccard rejected —
  scores ~1 on type permutations); `null` when the pair has no comparable (both-real) context.
- `enhanced_report.build_kernel_reading_section` renders the robustness summary + Jaccard table.

Witness: `classify_corpus('testsets_haiku', …)` (full-pipeline load route) → twin
`end_of_life_decision_authority`, 156 ctx → 73 robust / 83 specific; H¹ all band 5; Jaccard
0.63/0.53/0.31. Two-sided control passed (known-divergence ctx→diverge; agree ctx→0 divergence
solutions). H¹ instance-blindness UNCHANGED (per-reading H¹ is a join: each reading is its own
`per_constraint` entry with its own `h1_band`). Commit `d2cb9bb7`.

OQ-176 spawned: `cohomological_obstruction/3` returns `H1=0` for an ABSENT constraint
(`orbit_vector/2` yields a uniform all-`unknown` vector) — Pattern-5 measured-flat-vs-didn't-look.
Latent for any consumer reading H¹=0 on an unvalidated id as "measured flat"; does not affect OQ-10
(readings always real). Engine-behavior change to a 6+-consumer core predicate → logged, not patched.

---

## 2026-06-23 — OQ-112 RESOLVED (close-out): arc is latent-hardening, structurally latent across all three live legs
**Files:** ISSUES.md, audits/2026-06-23_oq112_closeout/
**Tier:** landed

Combined witness pass closing OQ-112 (no engine `.pl` edits). Under a pre-registered **field-level**
bite-definition: only **item 1** touched live output on the 92 (13/92 abductive `agrees`→`unavailable`,
**headline-neutral**); items 2/4/7 latent-hardened; items 3/5/6/8 do **NOT** fire as live bites.

**The reusable finding (two tripwires for a future instance):**
1. **A guard-predicate count over-reports a Pattern-6 firing.** The v1 item-3 sweep said "6 of 92
   hit the absence branch"; the v2 **consumed-output reachability** pass showed those 6 short-circuit
   at `epistemic_access_check=false` → `purity_score: null` — the absence value never reaches a
   reader. Witness "does the absence value survive the upstream gate into a consumed field," not
   "does the guard fire."
2. **"Latent on the live 92" is not "latent engine-wide" until checked on a denser corpus.** The
   A6/C4c gates were re-checked on both live twins (`testsets_haiku` 960, `testsets_flash` 960,
   overlay-took witnessed): **0 live bites on all three legs.** The masking is **structural** —
   `epistemic_access_check` / the compliance-sufficiency guard require the same metric family the
   downstream absence-gate needs, so absence of the datum implies failure of the upstream gate (same
   mechanism as the claim-less maxent exclusion that makes items 2/4 latent). Archives NOT swept
   (declared scope boundary; retrospective-audit breadth, OQ-89 pattern).

Items 3/5/6/8 fix-shapes recorded in the writeup, **declared-not-landed** (latent-hardening judged
not to earn its spend pre-rebuild). The arc hardened against absence-defects but caught no live
user-facing defect — a reasonable stop under imminent rebuild, recorded as dual-status not papered over.

---

## 2026-06-23 — OQ-112 item 4 RESOLVED (Round 3, Commit 1 alone): maxent-local accessors fail-closed; Commits 2/3 falsified
**Files:** prolog/maxent_classifier.pl, docs/design/design_gaps.md, ISSUES.md, audits/2026-06-23_oq112_round3/
**Tier:** landed

The A3 metric-fallback-`0.0` idiom in the four maxent-local accessors
(`get_constraint_metrics/4`, `metric_value/3`, `get_constraint_metrics_indexed/5`,
`metric_value_indexed/4`) now returns the `unknown` sentinel on absence of
base_extractiveness / extractiveness_for_agent / theater instead of a fabricated `0.0`; the two
dead `;Supp=0.0` branches removed; `maxent_threshold_proximity/4` gained a `number/1` fail-closed
guard. **Blast radius is contained to `maxent_classifier.pl`** — Round-0 recon found the local
accessors have no cross-file consumers (the shared sources `base_extractiveness` etc. are
untouched; the hybrid fixes the *local accessor*, not the shared predicate).

**Live-unexercised on 92 (do not read as a live catch).** WA witness: 0 sentinels are produced
over the 86 claim-bearing constraints (all carry every metric), so every new else-branch is
unreached and genuine values are byte-identical to pre-edit. Item 4 is LATENT on 92, same as the
item-2 case.

**Round 0 falsified Commits 2 and 3 — they did NOT land** (the read pass killing the write,
escalated and re-ruled by operator):
- *Commit 2 (findall silent-drop) DROPPED.* The mechanism is a LOUD throw, not a silent drop:
  `sum_list` is OUTSIDE the findall and throws on `unknown`; the throw aborts precompute
  (`maxent_classifier.pl:897`) BEFORE `maxent_indexed_run_info` is asserted (`:905`), so item-2's
  completion gate already floors it. WC witnessed this end-to-end (constructed theater-absent claim
  constraint → throw → run_info absent → indexed void alert). Item-2 is NOT blind to it.
- *Commit 3 (boundary external-crash) DISSOLVED into Commit 1.* `maxent_boundary_analysis/3` has
  zero callers; `maxent_threshold_proximity`'s only live callers (`maxent_report.pl:211`,
  `maxent_diagnostic.pl:395`) are already `catch`-wrapped. The `number/1` guard is folded into the
  commit that introduces the `unknown` (hardening-at-point-of-introduction). `boundary_analysis`
  adjudicated unfinished-value (not cruft) → **GAP-19** logged (wire-it opportunity: per-constraint
  nearest-edge fragility view, the dual of the live per-boundary report).

**Tripwire candidate? NO** — the contained-blast-radius and the latent status are stable facts but
produce no *silent* mistake for a fresh agent before editing a file; they live as history here.
The general rule (maxent absence → `unknown` sentinel → item-2 gate) is already covered by the
AGENTS.md completion-witness invariant from item 2.

**Round-4 gate installed in the OQ-112 entry:** before any further round on items 3/5/6/8, point
to one verdict a user saw change across the arc (items 1/2/4/7) or declare it latent-hardening and
stop. Preliminary read: latent-hardening, pending that positive control.

**The cross-file diag-site idiom instances are NOT swept in** (deliberately): `constraint_indexing.pl:860/892/895/898`
and `invertibility_analysis.pl:111–115` carry the same `->;=0.0` idiom on the *shared* sources, but
each is outside the contained blast radius with its own consumers and its own live-bite/latent
question — a per-site adjudication deferred to the operator, not a blanket conversion.

---

## 2026-06-23 — OQ-112 item 7 RESOLVED → ROUND 2 COMPLETE: wasserstein incomparable-mass provenance tokens
**Files:** prolog/json_report.pl, python/shared/schemas.py, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

Last staged Round-2 piece (different surface from the item-2 gate). `json_report.pl:438–442` had
four per-context arms `(catch(measurement_layer:wasserstein_incomparable_mass(C,Ctx,WM),_,(WM=0.0))
-> true ; WM=0.0)` collapsing THREE states into `0.0`: genuine measured zero, no-distribution
(producer fails), thrown (producer throws). Replaced with `wm_token/3` (float | absent | errored)
+ `wm_emit/3` (serializes float | `null` | `"errored"`). Helper carries a **fourth-state guard**
(`var(M) -> Tok = errored`): a succeed-with-unbound-M would emit a malformed JSON hole; routed
fail-closed. That state is **unreachable through the real producer** — it is STATIC (cannot be
extended at runtime) and its only success path runs `extract_chain_probs/3`, whose terminal
`IncompMass is max(0.0,…)` always binds or throws — so the guard is defensive against a future
producer change. `schemas.py:228` inner-value contract widened **in-comment only** (the
`(…, dict, True)` tuple is unchanged; the validator never type-checked inner values, so mixed
float/null/"errored" passes).

**Output-changing at the schema → landed ALONE.** Witnesses (`audits/2026-06-22_oq112_round2/`):
- `item7_wm_token_controls.txt` — 4-state forced control, all PASS. genuine 0.0→`0.000000`;
  nonzero→`0.400000`; absent→`null`; errored→`"errored"`; unbound-M→`"errored"` (guard). The shipped
  `wm_token/3` clause is pasted via `clause/2` so the state-4 guard-decision control goal is
  diff-able against the shipped guard subterm; states 1–3 run the REAL shipped helper via a
  `probe_harness:with_overlay` of the dynamic `maxent_dist/3`.
- `item7_before_after_diff.txt` — item-7-ISOLATED diff (clean BEFORE regenerated at HEAD `a5593f7`
  with item-7 reverted, vs AFTER): **ZERO other top-level fields moved, ZERO wasserstein cell flips.**
  On the live 92: 86/92 fire the section as a dict (6/92 whole-field `null` = the unchanged outer
  transport-profile failure branch), **344/344 cells genuine float** incl. measured `0.0` correctly
  kept as float (NOT collapsed to null); absent/errored arms **0-firing**. So the fix is
  **output-identical on the live corpus** — contract widening is **forced-witnessed, live-UNEXERCISED**
  (the item-2 posture applied to a contract surface).
- `item7_schema_validation.txt` — 0 schema errors over the regenerated `pipeline_output.json`.

`0.0` stays a *legal measured value* here (unlike `N=0`), so emitting `null`/`"errored"` is a
consumer-CONTRACT change, not a value change. Realized in-repo numeric-reader set was **empty**
(grep bounded to in-repo: `w1_sheaf_join.py` reads other wasserstein fields; `audit3_synthesis.py`
parses a different predicate's source; `test_harness.pl` `catch(_,fail)`). Out-of-repo / notebook
float-readers are genuinely out of reach and **unwitnessed** — a per-context value read as a float
now gets `null`/`"errored"` where the state was absent/errored.

**ROUND 2 COMPLETE.** Dual-status (both true, the second NOT subsumed by COMPLETE): round-level
"Round 2 COMPLETE" AND gate-level "item-2 maxent completion gate live-fire UNEXERCISED on 92 (0/92
latency), live trigger named as falsifier" — COMPLETE ≠ gate-proven-live. item 4 (A3) → Round 3;
items 3,5,6,8 staged.

---

## 2026-06-23 — OQ-112 item 2 RESOLVED: completion-witness-or-fail-closed gate (maxent stages)
**Files:** prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/maxent_classifier.pl, AGENTS.md, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

Round 2 of OQ-112. A voided maxent stage previously read GREEN (probe_maxent → inconclusive,
dropped; the indexed stage is read by nothing in the verdict path = fully silent). Fix, three
commits: `d69d5d39` (Round 0 re-witness on 92 + witness-truth controls), `4ee4ce08` (the
**distinct** `maxent_indexed_run_info/3` completion fact — NOT shared with `maxent_run_info`,
because indexed needs a prior classical run so a shared fact couldn't distinguish "indexed done"
from "classical done, indexed voided"), `0ef5bf6d` (the gate: `maxent_attempted/1` markers +
`maxent_void_alerts/1` per-attempted-stage fail-closed in `verdict_join` + absorbers widened to
`( catch(G,_,fail) -> true ; true )` so a stage FAILURE continues the run). Severity moderate/
yellow (operator ruling — a void is absence-of-measurement, not measured-severe). Invariant
**promoted to AGENTS.md** ("completion-witness-or-fail-closed"); provenance here.

**Gate status — forced-witnessed, live-UNEXERCISED.** Matrix (`GATE.md`): COMPLETE→green no-op;
THROW-indexed & FAIL-indexed (the `:871-874` no-priors `failed_plain`, catch-blind) → yellow
void[indexed]; THROW-classical → yellow void[classical]; N0-legal (fact present, N=0) → green;
cross-term classical-present+indexed-void → void[classical=no, indexed=yes]. `LATENCY/92` = 0 of
92 voided → live-fire unexercised by construction. **Do not cite as "verified live on 92"** — the
live trigger is the first claim-bearing story missing `suppression_requirement` (count 0, W2),
re-checked via the item-4 reachability probe, NOT a re-run on today's 92. Deferred zero-legal
ruling: (B) defer, TWO falsifiers (zero-with-witness via W3; claim-less→claim-bearing via item-4
probe). Items remaining: 4 → Round 3; 3,5,6,7,8 staged (7 = wasserstein, lands alone, schema-level).

## 2026-06-22 — OQ-112 item-1 (C4a) RESOLVED: diagnostic_summary data-absence else-branches fail closed
**Files:** prolog/diagnostic_summary.pl, ISSUES.md, audits/2026-06-22_oq112_round1/
**Tier:** landed

Round 1 of OQ-112 (Pattern-6 census batch). Corpus pinned self-witnessing: **LIVE=92**
(membership emitted + manifest + negative control: bad `corpus_path`→`corpus_empty`,
`testsets_haiku`→960; consumer-predicate check: diagnostic path enumerates
`corpus_constraint/1` at `json_report.pl:64`). C4a = 13 `; Signal = agrees` else-branches in
`diagnostic_summary.pl`; member sort: **10 sound · 3 defects**. Discriminator: `agrees` is sound
after the probe predicate *succeeded* with a positive no-tension result (`none`/`[]`/`H1=0`/
no-override/good-zone), a defect when reached from the `catch(_,_,fail)` else (data-absence).
Fixed (commit `4e6cf6e9`): `:198`/`:212`/`:163` `agrees`→`unavailable` (dropped identically with
`inconclusive` at `classify_signals_acc:359–362`).

- **`:198` (`probe_abductive`) is the only LIVE site:** 13/92 constraints have no `abd_triggers`
  fact (producer `abductive_report.pl:401–404` enumerates only ≥1-hypothesis constraints; loader
  asserts no fact for the rest). Was counted as agreement; now dropped. **Output-changing at the
  agreements list, HEADLINE-NEUTRAL** — join verdict identical for all 92 (witness
  `probe_before.tsv`/`probe_after.tsv`; the join is driven by tensions, not the agreement count).
- **`:212` unreachable:** `constraint_signature/2` is total (metric-less id → `unknown` clause
  `:136`; metric-bearing → `classify_by_signature(_,_,ambiguous)` catch-all `:353`). 0 live
  firings; fixed as fail-closed hardening per the operator guardrail.
- **`:163` unreachable:** `classify_disagreement/7` is total over 5 shapes; `probe_maxent` handles
  all 5 by name. Fixed so a future 6th shape reports uninterpretable, not agree.

Tripwire (don't make this mistake): the Python enrich side already distinguishes file-absent
(`None`→unavailable) from cid-not-in-file (`[]`→measured-empty) at `enrich_pipeline_json.py:164–169`;
the Prolog consumer was the only site collapsing absence→agreement, and `abd_triggers/2` is
`:- dynamic`, so a *missing* `abductive_data.json` would leave the subsystem "available" and route
every constraint to `:198`→agrees (file-missing = universal agreement). Items 2–8 staged in ISSUES.md
with corpus-re-witness obligations (inherited 62/194-row verdicts are not standing on 92).

## 2026-06-22 — OQ-20 + OQ-174 RESOLVED: DR baseline code/data diff (PERTURBED, stable core)
**Files:** ISSUES.md, prolog/json_report.pl, prolog/drl_purity_network.pl, python/audits/oq20_strip_cs.py, python/audits/oq20_dr_diff.py, python/audits/oq20_make_rekey.py, python/audits/oq20_analyze.py, audits/2026-06-22_oq20_dr_baseline_diff/
**Tier:** correction-key

Corpus-fixed / code-varied diff of DR output, tag `v3-dev-baseline` (`3e75f90b`)
vs HEAD, via `run_json_report` only (bypasses the diverged `run_pipeline.py`).
Cells A/B (original_json), C/D (original_v6_csfree), E/F (kernel_v1 cs-strip);
all cells byte-identical across repeats (empty noise floor). Full method +
controls: `audits/2026-06-22_oq20_dr_baseline_diff/WRITEUP.md`.

**Arm 1 (OQ-20) = PERTURBED, replicated on both corpora.** Two type surfaces, one
moved: the **priority-cascade** classification is BYTE-STABLE (identical 13-field
zero-diff set incl. `claimed_type`, `classifications`, `base_extractiveness`,
`suppression`, `theater_ratio`, `victims`, `beneficiaries`, and the χ/ε/d/f_d
values), but the **MaxEnt `maxent_top_type` is NOT** (29% flips original_json,
**73% original_v6**, concentrated as `tangled_rope→snare` ≈2261 → minted OQ-175
to bisect that boundary move). Also changed: `signature` (~85%), MaxEnt
distribution. `gaps` list→null is **NOT a
regression** — it's OQ-109 B3's coverage-bit + the 2026-06-14 detect_gap_pattern
rebuild (null=didn't-look vs []=examined). Code-vs-noise attribution is
witnessed: the empty noise floor is positive-controlled (fresh-process repeats
independently recompute; warm in-process 2nd run byte-identical to cold), so it's
real, not a cache shadow — #5's non-determinism (session-overlay memos, Python
phases) is bypassed by the `run_json_report`-only path.

**Correction-key items (how to cite):**
- The original OQ-20 mechanism ("checkout tag, byte-diff") is CONFOUNDED — the
  tag swaps the corpus (reset 2026-06-05). Hold corpus fixed, vary only code.
- The per-constraint `id` relabeling (tag in-file id → HEAD filename base) is
  commit **`801390a5`** (`known_constraint/1`→`corpus_constraint/1`), **not** the
  UUID migration. Do not attribute on the ratio alone.
- **Tripwire:** running HEAD on a legacy/archive corpus whose **filename ≠ in-file
  constraint id** yields **null DR output for those stories** (HEAD enumerates the
  filename, queries facts under it, finds none — no error). 133/1151 in
  original_json; 0 in original_v6 and the live corpus (filename==internal there).
  Re-key by in-file `constraint_metric` subject before any cross-id comparison.

**Arm 2 → OQ-174 (Ω_C, RESOLVED — benign carve-out).** Stripping all `cs_*` from
kernel_v1 leaves the DR observer core fully detection-independent (Theorem 7
holds) EXCEPT `contamination_network` (180 stories incl. 28 cs-free neighbours),
where `constraint_neighbors/3` reads `cs_reading_relation` into `explicit` edges
(`drl_purity_network.pl:67,92,257`). Crux settled by substrate:
`cs_reading_relation` is an **authored corpus fact** (written into testsets,
never asserted by code — read-only in `once`/`\+` guards), so this is a
**shared-input dependency, not detection-dependence** — Theorem 7 (which forbids
detection output feeding detection) is intact. The "200 cs-free byte-identical"
negative control "fails" because the authored edge couples cs-free neighbours —
a feature, not a bug.

---

## 2026-06-21 — OQ-35 RESOLVED: wiring-gap census rows 1–6 adjudicated (cruft-vs-wire)
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/probe_oq35_field_counterfactual.pl, python/audits/oq35_field_counterfactual.py, prolog/narrative_ontology.pl, audits/2026-06-21_oq35_field_counterfactual/
**Tier:** correction-key

Adjudicated the 6 authored-field wiring gaps (`audits/2026-06-21_oq35_field_counterfactual/writeup.md`).

- **Rows 2–3 `accessibility_collapse`/`resistance` — RETAIN, load-bearing (census REVERSED).** The
  2026-05-31 census called them "cosmetic (T.1)"; that was NL-override-specific and superseded by the
  OQ-128/OQ-138 routing-sink conversion. Counterfactual probe over 5 corpora (full observation tuple
  `obs(dr_type, signatures, verdict, alerts, signature_grade)` — NOT `dr_type` alone, which shows a
  false 0-diff because the signatures these feed ROUTE post-OQ-138). Load-bearing in every presence>0
  corpus; null control clean=0 everywhere; positive control passes everywhere. Treatment diffs:
  testsets 55/92, haiku 691/960, flash 537/960, kernel_v1 26/44, original_v6 421/3380. **Citation
  caveat:** "cosmetic" must NOT be cited for these fields — the diff variable is the routing observable
  (signature/verdict/alerts/grade), not `dr_type`.
- **Row 1 `is_mandatrophy_resolved/1` — dead facts, STRIPPED (operator go).** The 2 facts + comment
  removed from `narrative_ontology.pl` (retirement note left); output-neutral, **diff-proven**
  (validation-suite output byte-identical bar `[ELAPSED]` jitter; pre-existing lycurgan interval warning
  unchanged). Zero goal-body/meta-call readers (grep). The only mandatrophy surface
  (`format_mandatrophy_gap/3`→`compute_chi_v6/6`) is independent of the facts → strip safe. That surface
  is itself dead on the live corpus (0 GAP lines; gate needs `constraint_classification/3`
  powerless≠institutional, 0 powerless facts live) → logged as a dangling consumer (`design_gaps.md` GAP-18).
- **Row 4 `cs_reference_frame/2` — RETAIN on the OQ-133 bet** (inert consumption: serialized at
  `json_report.pl:590`, no join). `design_gaps.md` GAP-17 + kill condition. **OQ-38 corrected:** its
  "confirmed dead `cs_reference_frame/2`" was stale (`:590` is a real read site).
- **Rows 5–6 `uke_scope.*`, `commentary.*` — by-design, no action.**

OQ-35 status open→resolved. No engine behavior changed (probe is pure evidence; the row-1 strip is
output-neutral, diff-proven).

## 2026-06-21 — OQ-173 RESOLVED: MaxEnt signature-override boost made seat-aware (OQ-138 maxent residual)
**Files:** prolog/maxent_classifier.pl, prolog/load_warning_allowlist.txt, ISSUES.md, docs/design/design_gaps.md, docs/technical/signature_detection_wiring.md, audits/2026-06-21_maxent_seat_aware/FINDINGS.md
**Tier:** landed

`apply_override_for_sig/3→/4`: `C` threaded from the single call site (maxent_classifier.pl:318); the
two converted signatures skip the MaxEnt boost at routed seats — `false_ci_rope` at
`signature_detection:fcr_routed/1`, `constructed_high_extraction` at `constructed_routed/1` (reused
verbatim, unbound-cascade keyed; `DistOut = DistIn` reverts the seat to its pre-override raw dist).
Non-converted clauses ignore `C` (byte-identical). Covers BOTH serialized surfaces (`maxent_top_type`/
classical `maxent_probs` and `maxent_indexed`) — both classify paths call the same
`apply_signature_override/3`.

**Witness** (`audits/2026-06-21_maxent_seat_aware/diff_witness.out`): exactly the 12 routed seats
(9 fcr + 3 constructed) revert to raw; **0** non-routed seats move on any maxent surface (negative-half
byte-clean via the raw-probs discriminator); **1** categorical flip — `shinbutsu` indexed top
tangled_rope→snare (the one genuinely-manufactured verdict); **0** `verdict_join` changes.
**Premise refinement (correction-key):** OQ-138 framed the residual as the boost flipping
`maxent_top` to tangled_rope; substrate shows the conditional ×3 boost **never flips a CLASSICAL top**
(positive control: only 2 corpus-wide flips, both non-converted UNCONDITIONAL overrides
`false_natural_law`/`coupling_invariant_rope`) — the manufacturing was classical-mass + the indexed
top, not a classical-argmax flip. 21-corpus generality sweep: `routed_STILL_boosted=0` everywhere,
non-converted boosts intact; `original_v5` PARTIAL (pre-existing `maxent_run` failure, stash-confirmed
NOT a regression — recorded as partial, not swept-clean). `validation_suite` 92/0/0; `check_stack`
baseline-clean; `gate.sh` GREEN. Incidental: renamed a pre-existing `[C2]` singleton → `_` and pruned
the now-stale `maxent_classifier.pl:852` load-warning allowlist line.

**Tripwire (promotion candidate — held to history, loud-not-silent):** when converting a future
signature override RECLASSIFY→ROUTE, the MaxEnt boost in `apply_override_for_sig` is a THIRD surface
to make seat-aware (after `dr_type` and the diagnostic consumers) — skip-guard it on the same
`*_routed/1` predicate. NOT promoted to CLAUDE.md: the omission fails loudly (the next conversion's
pipeline diff shows the routed seat still at the override target), and the recipe now lives in
`signature_detection_wiring.md §4`.

## 2026-06-21 — OQ-138 constructed-3 sub-part RESOLVED: claim-discriminant conversion (keeps #2's floor)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/CONSTRUCTED3_FINDINGS.md
**Tier:** landed

Routed the 3 live `constructed_high_extraction` unknown→snare seats to the honest abstain `unknown`. NEW
**claim discriminant** (mountain→severe, else→informational — victim doesn't distinguish, all 3 vic>0): a
mountain claim over high-extraction is the concealment, kept at severe, REPLACING the floor the manufactured
snare used to carry via `type_1_false_summit` (which now reads informational at dr_type=unknown). **Kill
condition MET:** #2 (institutional_trust_erosion, claimed mountain) keeps RED byte-identical (floor source
moved type_1→signature); #1/#3 route to yellow/commentary; 47 inert + all non-constructed byte-identical;
5-corpus `mountain-routed→severe` holds everywhere. Reused the seat-aware template (`constructed_routed/1`,
`converted_at_seat/2`, `seat_overrides/2`). **`constructed_routed`/`fcr_routed` keyed on the UNBOUND cascade
winner** — a bound-arg query trips on the detector even when shadowed (caught `superheavy_decay`, an FCR seat,
in constructed_routed; §1 gotcha; the fcr_routed fix was behavior-preserving). **Maxent residual confirmed
(operator's warning):** the boost (`maxent_classifier:341`) flips #1/#3's maxent_top→tangled_rope at the
pipeline surface (unlike FCR top=rope) — benign (headline yellow; #2 red via severe), seat-aware maxent (plumb
C) tracked as a shared GAP. validation_suite 92/0/0; check_stack clean. Full: CONSTRUCTED3_FINDINGS.md.

## 2026-06-21 — OQ-138 FCR-9 sub-part RESOLVED: false_ci_rope SEAT-AWARE conversion (template didn't transfer)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/diagnostic_summary.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md
**Tier:** landed

**The FSM template did NOT transfer directly — false_ci_rope is SEAT-SPLIT** (9 routed / 3 piton / 13 inert,
one signature). FSM had no inert/piton, so its signature-level mechanism worked; for FCR, signature-level
keying would flip the 13 inert seats' grade and (witnessed by ablation) disturb the 12 already-mismatched
piton+inert seats. Built **seat-aware**: type route (resolve_with_perspectival_check clause 3 else
tangled_rope→ModalType); `fcr_routed/1` keyed on the stable dispatch GATES + the dr_type OUTCOME (NOT a
`metric_based_type_indexed` proxy — that proxy diverged from the live ModalType on 2 haiku+4 flash seats,
**caught by the 5-corpus generality sweep** before shipping, then replaced by the outcome check which also
removed the dispatch-mirror fragility); `converted_at_seat/2` (signature-level FSM, seat-level FCR);
`seat_overrides/2` (abductive_helpers, exported) threaded through diagnostic_summary `probe_signature/3` + P1/P7
so the routed-9 are non-override (honest unmask) while piton/inert keep override semantics.

**Witness:** 9 seats route tangled_rope→scaffold/snare; 6 verdicts change (vic>0 correction/moderate, vic=0
commentary/informational, sig=AGREE — no spurious override_mismatch; milder than FSM, mostly yellow); piton-3
TYPES unchanged + 13 inert FCR + all non-FCR byte-identical. **Carve-out relaxed:** statutory_debt (piton)
shifts yellow→red via the corpus-relative maxent ENSEMBLE (entropy_flag) — type unchanged, OQ-90 not
relitigated (Position-A). 5-corpus invariants pass (routed∩piton=0, routed-still-tangled_rope=0,
piton-not-piton=0); validation_suite 92/0/0; check_stack clean. **Residual:** maxent FCR boost
(maxent_classifier:331) still signature-level (no C) — benign for the 9 (maxent top=rope), logged for
constructed (same shape at :341). Full: FCR9_FINDINGS.md.

## 2026-06-21 — OQ-138 FSM sub-part RESOLVED: false_summit_mountain converted RECLASSIFY→ROUTE; routed false-summits read RED
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/config.pl, ISSUES.md, AGENTS.md, docs/technical/signature_detection_wiring.md, audits/2026-06-21_oq138_fsm_route_conversion/
**Tier:** landed

**What landed.** `false_summit_mountain` no longer overwrites `dr_type` (config
`false_summit_override_target` default `tangled_rope→mountain`; existing hook neutralizes the overwrite
and stays an ablation lever; unknown-input clause `→unknown`, 0 live fires/unverified-in-commit).
Shared severity template: `signature_detection:converted_signature/1` + `signature_diagnostic_severity/3`;
`signature_grade/2`+`signature_severity/2` grade converted signatures on the victim discriminant
(`vic>0→moderate/correction`, `vic=0→informational/commentary`), NOT on the now-zero type delta
(`dr_claim_mismatch/4` precedent). FSM removed from `abductive_helpers:known_override_signature/1`+
`override_target/2` (else `probe_signature/3`/P1/P7 misfire post-revert).

**Two divergences from the plan (both witnessed, both material).** (1) Live corpus grew **57→92**; it now
has **3** FSM seats incl. one **vic>0** (`protein_anabolic_resistance`) — the kill condition is on live
main. (2) **The report-surface verdict goes yellow→RED, not the plan's expected green.** The override was
masking dirac(`second_class`)+cohomology(`fails_descent`)+abductive tensions by setting the type to
tangled_rope (where they are "expected"); reverting to mountain unmasks them as genuine contradictions.
`claimed_type=mountain` preserved (route ≠ reclassify). **Operator ruling 2026-06-21:** the engine adds
commentary, does not change classifications, and it is OK for diagnostics to render different verdicts —
**Position A** (let subsystems speak; red is honest) over Position B (suppress dirac/cohomology to force
green). The victim discriminant lives in the commentary layer (`signature_grade`/alert severity).
Evidence: **82 FSM seats across 5 corpora** (≈6,500 stories) ALL carry cohomology/dirac → 0 where the
discriminant would be headline-visible (the tensions are structural invariants of false summits).

**Witnesses.** Full-pipeline corpus diff = **only the 3 FSM seats change, 89 byte-identical**
(`PIPELINE_OLD.txt` vs `PIPELINE_NEW.txt`). `severity_floor/2` two-sided positive control discharged.
Trap (silent green) averted: headline RED; protein keeps `correction` via the discriminant despite zero
type-delta (a naive revert drops it to commentary). `validation_suite` 92/0/0; `check_stack`
baseline-clean; `test_contradiction_signatures` 5-fail is pre-existing CS-axis fixture (identical OLD vs
NEW, confirmed by stashed-build run). **Subtlety:** `constraint_signature/2` is a cut-cascade returning
ONE signature; a BOUND-arg query bypasses the cuts (the build uses the unbound form — correct).
OQ-138 stays **partial**: FCR(19)/constructed(41+)/CI-rope(4) OPEN with named witnesses, FNL deferred
(OQ-70). Full detail: `audits/2026-06-21_oq138_fsm_route_conversion/FINDINGS.md`.

## 2026-06-21 — OQ-119 RESOLVED: feeding moves the verdict layer, committer invariant (Theorem-7); + the cs_-facts generator tripwire
**Files:** ISSUES.md, agent/cohort_replicate_batch.py, agent/generate_kernel_corpus.py, python/audits/oq119_spend_driver.py, python/audits/oq119_analyze.py, prolog/export_oq119_corpus_join.pl, audits/2026-06-21_oq119/, audits/2026-06-21_oq119_gate0/
**Tier:** tripwire

**Tripwire (the reusable, silent-mistake fact):** the **single-story generation path**
(`cohort_replicate_batch.py` / `story_generator_base.build_prompt_parts`) authors **NO `cs_` facts** —
a regenerated single story has observer + temporal but **no `cs_kernel_id` / `cs_reading_relation`**
(witnessed: `audits/2026-06-13_oq117_within_arm_proxy/fed_arm/*.json`). The **committer/CS axis exists
only on the kernel-generation path** (`generate_kernel_corpus` no-scope / `c-orchestrator` scope, which
authors `cs_structure.reading_relations` → `generate_constraint_pl.py:666`). **Any fed/withheld or
perturbation experiment that needs the committer axis MUST use the kernel-regen path**, or it silently
measures ≤2.5 axes while looking complete (the exact vacuity OQ-119 forbids). Corollary: `GEN_MODEL` is
**Haiku**, which intermittently drops the schema-required `stakeholders[]` (OQ-149 `allOf[0]` gate fires
loud → coverage holes; a Haiku pass left 2/5 kernels at full coverage, Sonnet → 5/5). Override to Sonnet
for precision fed/withheld spends; the bulk-build Haiku default stays.

**Result (OQ-119, full detail in `audits/2026-06-21_oq119/WRITEUP.md`):** 96 Sonnet generations,
parties-fixed fed framing, per-axis `median(D_A) > max(F_A)` against the measured generation-noise floor,
observer de-weighted. **Feeding moves the DIAGNOSTIC VERDICT layer (4/5 kernels: false_natural_law
escalates commentary→correction +1 alert) and leaves the COMMITTER obstruction/divergence INVARIANT
(0/5 — Theorem-7 detection-independence holds, measured not assumed).** Observer + temporal-rate move
softly. The verdict move is substantially the claim-gated FNL path (semi-expected); the committer
invariance is the non-trivial result. Committer is generation-noisy (withheld redraws flip
real_closure↔licensed_plurality) → routed to OQ-149.

**Correction-key:** the schema's mountain no-parties `stakeholders` exemption (`allOf[0]`) is
**deliberate** (OQ-149 2026-06-19 `becd0f87` + OQ-83 Pattern-5 omit-vs-authored-empty) — do NOT "tighten"
it to rescue a weak generator; the fix is the model + a parties-fixed fed framing, not the schema.

## 2026-06-20 — OQ-71 depth-lineage: Phase A closes the design question (mitigated, no spend)
**Files:** ISSUES.md, docs/design/a_hypothesis_about_corpus_size.md, python/audits/oq71_a2_richness_alldims.py, audits/2026-06-04_oq71_depth_lineage/, python/build_lineage_seeds.py, agent/generate_kernel_corpus.py
**Tier:** correction-key

Ran the OQ-71 plan's **Phase A (zero-spend, read-only)**; it closed the design question and
demoted the spend, so OQ-71 → **mitigated** (not resolved; not the spend).

- **A0 (the feasibility gate, witnessed):** the kernel-nesting relationship **never reaches the
  Haiku generator**. `build_lineage_seeds.py:114–134` forks the generation `seeds`
  (→ `lineage_seeds.json`, fed to the model — **no `parent_kernel`/`level`**) from a separate
  `lineage.json` sidecar (parent/level, consumed only post-hoc by the fingerprint join). Generator
  prompt `generate_kernel_corpus.py:430–486` reads only flat seed fields; grep finds no
  `parent_kernel`/`lineage` read (`:104` comment "kernel lineage is carried separately"). Origin
  plan `~/.claude/plans/virtual-inventing-allen.md` confirms this was **deliberate** ("only seed
  authoring and output routing differ"; generator frozen) → mitigated, NOT
  inconclusive-by-construction. **Consequence:** the plan's breadth arm reading-(a) ("strip
  `parent_kernel`, regenerate") is a **provable no-op** — `depth − breadth ≈ 0` by construction —
  so branch 1 (depth-realized-at-generator) was never in the experiment and the instrument can't
  isolate it.
- **Two-path architecture (why no_scope is blind to nesting — by design; operator-flagged).** SCOPE
  path (`_scope_user_prompt`/orchestrator `_step_decompose`) hands the MODEL a topic and lets it
  CONSTRUCT the kernel; no_scope renders PRE-DECOMPOSED readings. Batch generation forces
  decompose-FIRST (can't SCOPE-construct inline across a batch) → per-reading prompt structurally
  blind to nesting; inherited by any breadth arm. The CONTROL's structure was itself
  model-SCOPE-constructed then harvested (`build_never_generated_seeds.py` pulls `is_contested_kernel`
  SCOPE manifests). So depth-vs-control at the structure level = Opus-designed nested tree vs the
  SCOPE model's flat decompositions → branch 2 ("author-identity") = *who constructed the kernel
  structure*, not just prose.
- **Correction-key — claim widths:** the 1.5× excess is **not generator-visible parent-nesting**;
  it is the authorship-bundle (Opus identity and/or lineage-structured authoring, undistinguished).
  Cite at THAT width — not "the excess IS authorship" (residual-elimination overclaim) and not
  "depth re-opens discovery." "Generator never saw depth" is too strong: `sibling_reading_ids`
  reaches the prompt and covaries with level (r=−0.366) — say "never saw **parent-nesting**,"
  co-channel bounded by the length-stratification control. The "156>118" line is **color**
  (non-matched-n, cross-regime), not evidence.
- **A2 (list-inflation closed, all 5 dims):** matched n=294, K=2000 — JOINT distinct-class excess
  +38.7 vs largest single-dim MARGINAL excess +2.8 (zone); depth uses *fewer* props/voids/actors
  values → new combinations, not proliferation; positive-controlled. Closes the prior 2-of-5-dims
  caveat. Witness: `audits/2026-06-04_oq71_depth_lineage/a2_richness_alldims_results.json`.
- **Watch-out (witnessed):** `outputs/completion_seeds/never_generated_seeds.json` **drifted
  2026-06-13** — missing 26 of the 300 frozen control ids, so it no longer reproduces the audit's
  length-2+ stratum (294→268). `control_membership.json` (the 300 ids) is the durable authority;
  A2 ran drift-immune on full frozen arms + the current stratum (same verdict both). Any OQ-71
  re-run keying on that seed file inherits the drift.
- **Graduation step (→ resolved, deferred):** Opus authors ~300 *flat* seeds, same frozen generator
  (origin plan reading-(b)) — splits author-identity from lineage-structure (the only live question
  once branch 1 is out of scope). Needs spend; declined this session; recorded in OQ-71 + §10.1 for
  a future instance.
- **Construct-validity gap → OQ-171 (minted this session).** §3's bounded-attractor claim is about
  the SCOPE construction path; OQ-71 falsified only *substrate-level* boundedness (Opus/no_scope),
  never the SCOPE path. Do NOT read mitigated as "§3 tested" — §3 stands within-regime. OQ-171
  registers the context-controlled batch-of-one design (vary inline-context, hold topics) and
  declines the naive small-batch proxy (inherits OQ-71's disjunction). May be non-constructible
  (A0 obstruction recurs); spend + pricing = operator seat.

## 2026-06-20 — OQ-69 research-frontier ledger DRAINED → OQ-154–170; OQ-69 closed
**Files:** ISSUES.md, issues/INDEX.md, issues/INDEX.json, CLAUDE.md, audits/2026-06-20_oq69_ledger_drain/
**Tier:** landed

OQ-69 was a backlog *ledger* (Ω_P), not a single question — it resolves by being **drained** (each
live item promoted to its own OQ), not by executing its contents. Drained the 16 still-live bullets
into **17 new OQs (OQ-154–170)** and closed OQ-69 `resolved` with a provenance map in its body. The
16→17 expansion: the engine-hardening bullet is three legs (OQ-154/155/156) and the cluster bullet
splits F/G (OQ-160 `gates` OQ-170). The prior check_stack item had already graduated → OQ-142–145.
**No engine code changed** — tracking restructure + index regen + doc-currency only.

Two operator rulings this session (both escalated, not self-resolved — genuine source conflicts in
the plan): (1) **cluster splits F/G** → OQ-170 `blocked_on OQ-160` added (the ledger's "Pkg F then
Pkg G after" is a real edge; the splitting rule + §5's BLOCKED-G witness outweighed the stale
"count=16"); (2) **priority scheme = distinct-within-band, bands overlap 1–10** (Higher 1–3 / Medium
1–5 / Lower 1–9). All 17 priorities are **provisional — operator to rule** (the declared seat).

Correction (Pattern-5 premise rot): the priority parser is **not** capped at 10 — regex
`^\*\*Priority:\*\*\s*(\d{1,2})\b` (omega_resolver.py:69) accepts 1–99; "1–10" in omega_resolver.md
is doc convention only. δ correction (OQ-162): the ledger's "δ not load-bearing" was the stale half —
witnessed perturbation probe shows δ is **live-but-zeroed** (wired `resolve_displacement →
D_eff=clamp(D+δ) → χ`, flips at δ:=0.3, but config default 0.0/uniform makes it inert as shipped).

Close-vs-keep-open ruled from code: `omega_resolver.py:244–258` authority set is all parsed OQs
(resolved included) → a resolved parent doesn't dangle; no inbound Deps edge points at OQ-69 →
**close** (not keep-open as a thin parent). Witnesses (all pasted at commit): `issues_status --check`
170/0, `omega check` 0 problems, `selftest` 10/10, `menu` arrival of 154–170 (156+170 BLOCKED, 168
BLOCKED-ON-YOU) + departure of OQ-69 **and control OQ-63** from WORKABLE (resolved items excluded),
`gate.sh` GREEN. Full writeup + δ probe: `audits/2026-06-20_oq69_ledger_drain/`.

## 2026-06-20 — OQ-58 cross-corpus census, non-gating linter wired, three-leg/beta corpus ruling
**Files:** python/run_pipeline.py, python/audits/reading_reference_linter.py, agent/generate_kernel_corpus.py, ISSUES.md, docs/design/design_gaps.md, CLAUDE.md, audits/2026-06-20_oq58_cross_corpus_incompleteness/
**Tier:** tripwire

Re-measured OQ-58 after the 2026-06-05 reset stale-ified its counts; wired the
referential-integrity linter as a non-gating `reading_linter` step in
`run_pipeline.py` `_phase_post_prolog` (writes `outputs/reading_reference_census.json`,
manifest+corpus_hash; `summarize()` added to the linter, behavior-preserving).
Witness: pipeline 47/47 OK in 10.4s, step "163 dangling → 158 missing / 66 kernels
(5 id≥2 defensible) — NON-GATING"; linter selftest PASS; gate GREEN.

**Census (read-only, `audits/2026-06-20_oq58_cross_corpus_incompleteness/census_driver.py`):**
LIVE testsets 92 files / 169 csr edges / 163 dangling / **93.5%**; testsets_haiku
960/2004/127/**3.7%**; testsets_flash 960/2008/101/**2.3%**; kernel_v1 1106/1774/94/**4.8%**.
LIVE 93.5% is a SPARSITY artifact (1.03 readings/kernel, 97% singletons), not a frontier.
GAP-07 bounded-attractor answer (split): rate bounded ~2-5% across lineages; defensible
id≥2 count ~40 reproducible WITHIN a lineage (haiku 39 ≈ flash 41, haiku∩flash 39), NOT
tri-lineage (kernel_v1 8; common core 1).

**Regime swap (git, corrects the planning note's direction):** the 06-13 rebuild pilots
BUILT testsets/ to 1000 files / 2.92 r/kern (reconciled multi-reading corpus); commit
`0ccc03cf` then moved it OUT to the twins (haiku/flash, byte-intact 960/960) and testsets/
reverted to a singleton working set (51 → 92). The "accidental clobber" fear is falsified.

**TRIPWIRE — three live legs, beta posture (operator ruling 2026-06-20; promoted to
CLAUDE.md Critical Distinctions "THREE LIVE LEGS, and the beta posture").** `testsets/` is
the live leg ON PURPOSE — a deliberately singleton topical working set to exercise the
engine while building it and surface live issues; `testsets_haiku/`+`testsets_flash/` are
the reconciled twins (comparison baseline). The singleton sparsity is INTENDED — do not
complete/flatten/rebuild testsets/ on sight. Currently ALPHA, working toward BETA: extract
maximum value from the current corpus so it earns its way to beta before any rebuild; a
fresh `testsets_*` rebuild comes only after
schema/wiring/enough-of-ISSUES.md are worked out (many OQs open → a ways off). A future
instance MAY suggest a rebuild when accumulated changes warrant it, not propose one lightly.
This resolves the OQ-58 corpus-identity flag (was `blocked_on_human`).

OQ-58 downgraded partial → mitigated, Priority 1 → 3; generation deferred (two backlogs
recorded: durable twin-reproducible 39; stream-relative live 5/3). Quarantine JSON
documented as a per-run artifact, not the live backlog (note at the writer + ISSUES).
Commits `1c5c97a7` (code), `9532ffe4` (docs).

---

## 2026-06-20 — grid-diet display: one-informative-line-when-absent + stale "unauthorable" fixed (OQ-93)
**Files:** prolog/report_generator.pl, prolog/data_repair.pl
**Tier:** landed

Two display fixes to the OQ-93 grid-provenance surface (OQ-93 is RESOLVED; grid is opt-in by story
focus, authored-or-absent — NOT a bug when 0/32). Consumer of these reports is a MODEL doing essay
synthesis; operator ruling 2026-06-20: it needs relevant outputs, not Prolog internals.

1. **One informative line when absent** (`report_generator.pl`, the report-body grid line). The
   ABSENCE is itself the signal — a story that could author a leveled coercion grid and didn't is
   not level-resolved-coercion focused. So on `authored+injected+imputed == 0` the body now prints a
   single plain line: `Leveled coercion grid: not authored (story not level-resolved-coercion
   focused); grid-dependent magnitude/coverage not computed - expected, not a gap (OQ-93)`.
   (Superseded the same-day "terse + [CONDITIONAL] token" form, commit 5c23830e — the operator ruled
   the model doesn't need the Prolog `[CONDITIONAL]` jargon; the plain text carries the same
   "ungrounded" meaning.) **OQ-98 ruling 1 preserved for PARTIAL grids** (0<authored<total still
   prints `[CONDITIONAL: grid authored X/Y]`); only the fully-absent case went plain. Grid stories
   print the full verbose line unchanged (witnessed: `sex_gender_category__identity_reading` →
   `authored 32/32`, Kappa 0.67, coverage 4/4). Surfaced in the .md via `run_prolog_report`.
2. **Stale message fixed** (`data_repair.pl:356` print + `:291` comment). Both claimed the grid is
   "unauthorable under the live generation schema" — false since OQ-93 resolved 2026-06-11 (3 live
   testsets author grids). `report_grid_provenance/1` is REACHABLE (`repair_interval/1`, used by
   scenario_manager/test_harness), so reworded, not deleted. Now: "opt-in by story focus
   (authored-or-absent; injection/imputation retired)".

Hinge witnessed (not assumed): `grid_provenance` reaches `pipeline_output.json` — 86/92 constraints
carry it in `verdict_join`, 0/32 stories show `{authored:0,…,absent:32,total:32}`. So trimming a
display surface cannot drop provenance; the machine-readable sink keeps it.

STILL OPEN (the bigger half): `assemble_report` embeds the FULL Prolog stdout into the model-facing
.md (witnessed: `header + prolog_output`), so a 0/32 story still carries ~12 grid-absent DEV-preamble
lines the model doesn't need (`[SHIM]`, `[REPAIR]`, `[OPEN] N/N grid components absent` ×8,
`[PROVENANCE]` ×2, `[WARN]`, `[INTENT] OPEN (no_gradient_data) [grid diet:…]`), plus the banner
`_grid_line` (`Grid: authored 0/32 …`). Decluttering these is content-removal from the model artifact
— pending operator go (show-before-delete). Sibling: `intent_engine.pl:75`.

---

## 2026-06-20 — OQ-56 + OQ-53 closed: canonical cross-kernel reading-stance vocabulary ruled
**Files:** python/orbit_operator.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

The last two open items of the kernel/reading-axis thread are closed.

**OQ-56 (Ω_P ruling — resolved).** Canonical cross-kernel reading-stance vocabulary = the two Tier-1
draw-robust keys, `observer_signature` (reading-unit, twin-agreement 0.722) + `obstruction_class`
(kernel-unit, 0.734). The six Tier-2 keys (incl. `seat_role_vector`, 0.245) are report-only,
model-relative. Made a **checked fact, not a memory** (Build Discipline Pattern 2):
`CANONICAL_VOCABULARY = {"observer_signature","obstruction_class"}` in `orbit_operator.py`, surfaced as
`canonical` on every orbit record (witness: `canonical=true` on exactly the 2 Tier-1 keys / false on the
6 others). Owned seat written first-person in `design_discipline.md` §0.1 (decline-not-refute the
seat-role-vector rival). **Kill condition recorded but NOT armed** (ISSUES OQ-56): a live downstream
consumer that *requires* `seat_role_vector` inside the canonical vocabulary to FUNCTION (not display)
reopens it as Option 2. None exists as of 2026-06-20 — witnessed two-pronged grep (named-key +
generic-`canonical`, pre- and post-`canonical`-stamp), each with an `observer_signature` positive
control. Detection is **manual, not automatic** — nothing trips if such a consumer is built later; the
condition is a documented reopen trigger re-evaluated by hand (re-run Step 0 grep), not a live tripwire.

**The headline Ω_E finding (recorded as the result, not buried).** OQ-56's motivating question — name the
semantic stances (naturalizing / coordination / power-revealing) comparably across kernels — has **no
draw-robust answer on this corpus**: reproducible keys are structural/coarse; the one semantically-aligned
key (`seat_role_vector`) is draw-fragile. The semantic-stance transpose is **foreclosed-as-draw-robust**,
model-relative only — an Ω_E, reopenable by a more reproducible extraction.

**OQ-53 (transpose leg — resolved, Branch 1 witnessed-live).** Within-kernel leg already satisfied (OQ-55
router). Transpose query — hold `observer_signature` fixed, sweep across kernels — runs live and finds
multi-kernel orbits: `constructed_high_extraction` spans **25 genuine multi-reading kernels**,
`false_ci_rope` spans **11** (positive control: 89 distinct kernels present, query detects 5 multi-kernel
orbits — not byte-identical to an empty read). `logical_fingerprint.pl` stays prefix-opaque by design, so
the close is (a-restricted).

Promotion test: this is a one-time ruling, not a silent-mistake-before-editing-X trap → history-grade, no
CLAUDE.md tripwire. The one durable do-not (`canonical` is a checked set, don't re-derive from `tier`)
lives at the code in `orbit_operator.py`'s docstring.

**Downstream consumer wired (enhanced_report.py).** The cross-kernel orbit artifact has no per-constraint
report consumer (and shouldn't — its product is the corpus-level transpose query). The one genuinely new
per-constraint datum is a **draw-robustness tag on the Signature line**: the report's `Signature:` IS
orbit_operator's `observer_signature` key, so it now reads `Signature: <label>  (canonical stance ·
twin-agreement 0.722)`, reusing `orbit_operator.KEY_META` as the single source (not a hardcoded number —
witnessed: flipping `canonical` in KEY_META flips the tag). It qualifies the *vocabulary's* draw-
reproducibility, not the specific value. Helper `_signature_robustness_tag()` in `enhanced_report.py`.

---

## 2026-06-20 — orbit regeneration wired into the pipeline (was a manual pre-step; OQ-29 follow-up)
**Files:** python/run_pipeline.py, python/sweeps/regenerate_orbits.py
**Tier:** landed

`run_pipeline.py` now runs `regenerate_orbits.py` as a sequential **Phase 1b** step (`regenerate_orbits`,
after `prep`, before the parallel Prolog phase and before `manifest_inject`). Previously orbit
regeneration was a MANUAL pre-pipeline step, and `manifest_inject`'s `check_orbits_corpus_hash` would
fail-closed if you forgot it (the recurring "product_site_orbits.json is stale: corpus_hash … != …"
error). Operator ruling 2026-06-20 (the regenerate-every-time vs on-demand tradeoff): regeneration is
cheap (~1.3s on the live corpus) and the manual-step friction wasn't worth the stale-orbits error, so
run it with the pipeline. The `manifest_inject` corpus_hash check is **kept as the fail-closed backstop**
(catches a regen that failed or was skipped) — the OQ-29 Thread-C guard is unchanged.

Sequential placement is deliberate: it must not race the shared `product_site_orbits.json` with the
parallel Phase-2 swipl analyses (serialization rule). Runs as a subprocess because the script
`sys.exit()`s on failure (a `SystemExit` that `_run_step` would not catch); non-zero exit → `RuntimeError`
→ recorded step error, with the manifest_inject guard still firing downstream. Caveat:
`regenerate_orbits.py` always exports the DEFAULT `testsets/` corpus (exactly what `manifest_inject`
checks); a non-default `classify_corpus` run is unchanged (pre-existing, not made worse). Witness:
pipeline now 0 errors (was 1 — `manifest_inject` stale), `regenerate_orbits ok [1.3s]`, total time ~8.8s
(unchanged — the regen replaced the error, not added to it).

## 2026-06-20 — within-kernel trifurcation router built + wired (OQ-55 resolved; OQ-53 within-kernel leg closed)
**Files:** prolog/cs_trifurcation.pl, prolog/json_report.pl, prolog/tests/test_cs_trifurcation.pl, prolog/stack.pl, ISSUES.md
**Tier:** landed

New module `cs_trifurcation.pl` (`cs_reading_trifurcation/3`) routes *why* a kernel's readings disagree
into the `debugging_philosophy.md` §6 trifurcation, **within-kernel only**. Dispatch on the authored
obstruction edge (`cs_kernel_obstruction_status/2`), refined by two computed within-kernel diagnostics:
`real_closure`→Type B (confirmed/edge_only via `cs_axiom_foreclosed`), `licensed_plurality`→Type C,
`untyped`+`cs_drift_unacknowledged`→Type A, `untyped`+no-drift→`unknown` (Pattern-5 fail-closed, NOT a
default), `singleton`→no verdict. Live consumers: (1) `reading_trifurcation` field in `json_report.pl`'s
`cs_kernel_comparison` (`scope:within_kernel` stamped inline; **commentary-grade**, never overrides
classification) — survives the enrich step into `enriched_pipeline.json`; (2) `enhanced_report.py`'s
`build_kernel_reading_section` renders a `Reading disagreement: <type> [within_kernel; obstruction=…, …]`
line in the human report (added 2026-06-20 follow-up — the field reached enriched_pipeline.json but was
unrendered; Pattern-1 second-wire closed). Wired into `stack.pl`.

**§6 mapping confirmed against the definitions** (not the table paraphrase): Type B = "impossible by
definition" = `forecloses`; Type C = stable coexisting frames = `coexists_with`; Type A = unmarked
mutation treated-as-stable = the `false` (unacknowledged) flag in the drift gap. Type A is the **sole
computed branch**; two layered controls hold obstruction at `untyped` and vary only the drift signal:
(1) two-twin (`tk_drift` vs `tk_nodrift`) — drift signal is the discriminator, not obstruction riding
along; (2) **single-bit** (`tk_drift` vs `tk_drift_ack`) — direction + magnitude held identical (checked
by in-test unification), only the `acknowledged` flag flips false→true, and the verdict flips
`type_a_drift`→`unknown`. Isolates the unacknowledged bit specifically (side-by-side pasted in the OQ-55
follow-up turn, 2026-06-20).

**Re-scope ruling (operator, 2026-06-20):** OQ-55 was `blocked_on OQ-56` — a *soft* block. The
within-kernel router needs no cross-kernel vocabulary; OQ-56 gates only OQ-53's transpose leg. Edge
dropped. **Re-scope witness = input-boundary trace:** every router input is gated by `cs_kernel_id(_,K)`,
so no cross-kernel fact enters the verdict (traced on `tk_drift`).

**Draw-robustness transfer caveat:** the 0.734 twin-agreement on the obstruction-class orbit was
measured *cross-kernel* (OQ-150). Its transfer to within-kernel use here is **inferred**, and is
discharged by the input-path trace (the router reads only per-kernel/per-member facts), NOT by that
number — the number describes a different (cross-kernel) measurement.

**Witnesses.** `test_cs_trifurcation.pl` 8/8 green (4 branches + singleton negative + two-twin
discriminator + cross-kernel-leak control). Live corpus (`run_pipeline.py`, all 9 multi-reading kernels
non-null): `type_a_drift`×5, `type_b_structure`×1 (`jewish_sovereignty_palestine`), `type_c_ambiguity`×2
(`press_reformation_causation`, `zero_mathematical_status`), `unknown`×1 (`polaris_document_status` —
fail-closed fires on real data). OQ-55 resolved; OQ-53 within-kernel leg closed, transpose leg stays
`blocked_on OQ-56`.

Note: the pipeline's `manifest_inject` step errors on `product_site_orbits.json` staleness (corpus_hash
mismatch, OQ-29) — pre-existing, orthogonal to this change (neither modified file references it).

## 2026-06-20 — kernel/reading orbit operator built + wired (OQ-150/OQ-53 Phase 3)
**Files:** python/orbit_operator.py, prolog/kernel_orbit_export.pl, python/run_pipeline.py, outputs/reading_orbits.json, outputs/kernel_orbits.json
**Tier:** landed

The cross-kernel orbit operator (commit `0c488468`). `orbit_operator.py` reads the canonical
`pipeline_output.json` (6 keys: observer-signature, terminal-observer/committer, apparatus,
seat-vector, grounding) + `kernel_obstruction.json` (the 2 keys not serialised in
pipeline_output: obstruction-class + grounding, produced by `kernel_orbit_export.pl`) → writes
`outputs/{reading,kernel}_orbits.json`. Wired into `run_pipeline.py`: `kernel_orbit_export` in
`_phase_post_prolog`, `orbit_operator` after `w1_sheaf_join` (dependency-ordered, non-critical).

**Two tripwires for a fresh agent:** (1) the operator's LIVE output is **sparse by design** — the
live corpus has ~3 multi-reading kernels, so `reading_orbits.json`/`kernel_orbits.json` on a live
run look near-trivial; the meaningful orbit populations are on the TWINS (run
`python3 python/orbit_operator.py --twin haiku`). Do NOT read sparse live orbits as a bug or as
"orbits don't form." (2) Per operator ruling 2026-06-20, only Tier-1 keys (observer-signature
0.722, obstruction-class 0.734) are declared draw-robust; Tier-2 keys carry their twin-agreement
number INLINE on every orbit record and are model-relative — do not cite a Tier-2 orbit membership
as a stable finding. Same-run guard: `orbit_operator` drops `kernel_obstruction.json` to
`source_missing` if its `n_constraints` ≠ the pipeline manifest (fail-closed; positive-controlled).

## 2026-06-20 — orbit-key declarability: judge against the extraction baseline, NOT the permutation null
**Files:** audits/2026-06-20_kernel_reading_orbits/, ISSUES.md
**Tier:** correction-key

OQ-150 cross-twin orbit measurement (8 keys, haiku/flash n=960; `phase1_orbit_keys.py` +
`phase1b_agreement.py`; controls pass — `claimed_type` 0.7208, K1 reproduces 2026-06-18 M3
0.134). **Citation correction:** a key clearing the permutation `band95` (`lo>band95`) means
*beats random labels*, NOT *draw-robust enough to declare as a vocabulary*. All 8 keys beat
chance; only 2 reproduce at the **extraction baseline (~0.72, the substrate's own
reproducibility — the natural floor)**: `kernel-obstruction-class` (0.734) and
`observer-signature` (0.722). The other 6 are above-chance but membership-fragile (0.13–0.57).
Judge orbit-key declarability against the baseline, not the null. The plan's `lo>band95` gate
under-operationalized the **reproducibility** filter the plan's own Context elected — applying
the baseline honors that election, it is not a retroactive switch.

Two substantive Ω_E findings: (1) **committer axis is fragile FINE but reproducible COARSE** —
apparatus/grounding model-relative (0.49/0.27) yet the 4-way obstruction verdict reproduces
(0.734); granularity governs declarability, not axis. (2) **apparatus orbit is gradient-orthogonal
to observer** (normalized MI 0.063, Theorem 7) — genuine second axis, keep separate. OQ-53
report-path witness: kernel is first-class in `cs_kernel_registry.pl` + `json_report.pl`
(`cs_kernel_comparison`), prefix-opaque only in `logical_fingerprint.pl`. Two operator picks
reserved (OQ-56 vocabulary; OQ-53 committer-transpose disposition); empty-menu kill did NOT fire.
Commits `b07e84f1`, `17dba90e`, `0fdc9d7a`.

## 2026-06-19 — the orbits-staleness warning is EXPECTED after every c-orchestrator run (not a bug)
**Files:** python/run_pipeline.py, python/sweeps/regenerate_orbits.py, agent/c-orchestrator.py
**Tier:** history

A c-orchestrator topic run grows `prolog/testsets/`, so `outputs/product_site_orbits.json`
(the perturbation-sweep baseline, regenerated only by `regenerate_orbits.py`) is stale by
construction the moment generation finishes. The `manifest_inject` step's
`check_orbits_corpus_hash` (`run_pipeline.py:1133`) then raises `RuntimeError: product_site_orbits.json
is stale: corpus_hash … != current …` — this is **non-critical**: `_run_step` catches it, the
manifest is already stamped (injected before the check), and the pipeline reports `42/43 steps OK`.
Do NOT re-diagnose this as a pipeline failure. The live classification path is unaffected — it
runs on `orbit_data.json`, which IS regenerated each pipeline run (`Matched orbit data for N/N`).
Only the sweeps (`perturb.py`, `product_site_delta_sweep.py`, …) consume the stale `product_site_orbits.json`;
run `python3 python/sweeps/regenerate_orbits.py` (atomic swipl export + hash stamp) before a sweep
that needs it. Operator ruling 2026-06-19: keep orbits DECOUPLED, regen on demand — deliberately
NOT wired into the orchestrator (the export is expensive and most topic runs never sweep). Lineage: OQ-29.

## 2026-06-19 — the engine's "H1" is a disagreement tally, not a cohomology rank (citation correction)
**Files:** prolog/grothendieck_cohomology.pl, ISSUES.md
**Tier:** correction-key

`cohomological_obstruction`'s H1 = `count_disagreeing_pairs` — by its own comment a *"Cech
1-cocycle proxy"*, the count of disagreeing context-pairs (range 0..6 = C(4,2)). It is NOT
dim H¹ / a Betti number. Witness: a role-gauge `[naturalized,snare,snare,snare]` gives tally 3,
but the first Betti number of that disagreement graph (star K₁,₃) is E−V+C = 3−4+1 = 0. **H⁰
(global section ⟺ all contexts agree) is legitimate; "H1" is a contextuality/disagreement count.**
Caught by a three-model review (the counterfeit-rigor register). Do not cite `H1` /
`contextuality_fraction` (=H1/6) / `sheaf_status` as cohomology results without that caveat — they
are a disagreement tally over a 4-point site with no overlapping cover. A real Čech H¹ needs a
nerve with overlaps (reading_diff's vantage alignment is the candidate). Lineage: OQ-151, OQ-51.

## 2026-06-19 — schema: conditional stakeholder-coverage gate (the false-negative root cause)
**Files:** schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md
**Tier:** landed

Diagnosis (OQ-149; audits/2026-06-18_oq56_*): 423/466 haiku no-stakeholder stories had authored
beneficiaries/victims (obvious parties) yet emitted no `constraint_stakeholder`. Root cause =
contradictory signals: the generation prompt prose marks stakeholders REQUIRED, but the schema
omitted it from `required` AND its field description said "optional alongside perspectives during
the A/B perturbation" — the proximate signal for structured generation, so the weaker model
(haiku) dropped it; flash wrote it on 75% of the same slots. Not truncation (six_questions, a
later field, survives more) and not surface-substitution (ben/vic co-occur with stakeholders).

Fix (commit `becd0f87`): CONDITIONAL `allOf` gate — if `base_properties.beneficiaries` or
`victims` non-empty → require `stakeholders` (minItems 1); a true mountain with no parties
(gravity) stays EXEMPT. Description rewritten to state the contract. **Forward-only** — gates
new/regenerated stories; existing corpus untouched (no consumer re-validates committed stories;
no test validates `json/`). Witnessed Draft7 (the pipeline's validator): example still validates,
example−stakeholders now CAUGHT, gravity exempt. **The prompt-prose reinforcement is the
operator's edit** (driving the c-orchestrator loop). Schema is the binding gate.

---

## 2026-06-19 — reading_diff un-stranded onto the live stakeholder-seat schema + stale test corpus
**Files:** prolog/reading_diff.pl, prolog/tests/test_reading_diff.pl, ISSUES.md
**Tier:** landed

`reading_diff:reading_cells/2` read only authored `constraint_classification/3`, which the
de-leak rebuild stopped authoring — every within-kernel pair on the live/twin corpus read a
vacuous `robustly_undersampled` (OQ-56 D1). Fix (commit `01cff6a7`): `reading_cells/2` now
UNIONS two cell-sources, mutually exclusive across corpora — authored
`constraint_classification/3` (archives) and `stakeholder_seats:stakeholder_context/3` +
`dr_type_for_stakeholder/3` (live; same `context/4` tuple, so alignment keys untouched).
Witnessed: haiku census 0/0/954 → 136 binocular / 111 fragile / 707 (now MEASURED) coverage
gaps; non-regressive on `kernel_v1` (0 stakeholders → clause inert; suite 10/10 pass with
archive overlaid). Twin both-stakeholder pair coverage: 26% haiku / 61% flash (model-asymmetric
stakeholder authoring — folds into OQ-149).

**Tripwire:** `prolog/tests/test_reading_diff.pl` fixtures are **pre-reset westphalia readings**
absent from the live `testsets/`; running it the documented way (default corpus) shows **7/10
FAILED** — a corpus-overlay artifact, NOT a reading_diff bug. Run it with the archive overlaid:
`swipl -g "asserta(config:param(corpus_path,'archives/datasets/kernel_v1')), [stack],
corpus_loader:load_all_testsets, [tests/test_reading_diff], run_tests(reading_diff), halt"`
→ 10/10 pass. (Stale-fixture repointing is unfiled — candidate OQ.)

---

## 2026-06-18 — OQ-147 crash floor + OQ-148: classifications regression (corpus-wide producer break)
**Files:** python/audits/sheaf_audit.py, python/audits/tests/test_sheaf_audit.py, ISSUES.md
**Tier:** landed

**OQ-147 (loud, resolved).** `sheaf_audit.py:515` raised `ZeroDivisionError` because its working set
(constraints with ≥2 of the 10 Tier-1 slice contexts) is empty. Fixed with one `insufficient =
(working_set_size == 0)` predicate reused on three surfaces (markdown early-return, console one-liner,
JSON null rates + `verdict: insufficient_data`); verdict single-sourced via `_verdict()` so JSON and
markdown can't drift, happy-path bands byte-identical to old 464–471. A naive `if n_total else 0.0`
was rejected — it sets `crossing_rate=0.0` → the `== 0.0 → "PRESERVED (zero crossings)"` branch,
making empty indistinguishable from measured-flat (Pattern 5/6). New fixture
`python/audits/tests/test_sheaf_audit.py` (4/4 PASS) pins the empty-case markdown + the
non-self-witnessing `_verdict` string swap. Witnesses: pre-fix crash at :515; post-fix exit 0, JSON
`crossing_rate: null`. Loud crash → stays history (no tripwire).

**OQ-148 (quiet, open — the real bug, candidate tripwire).** Root cause of the empty working set:
`outputs/pipeline_output.json` carries `classifications: []` for **all 80** constraints (2026-06-18),
but committed snapshots prove it populated on 2026-06-11 (46/48 @ 287 entries, 50/52 @ 312 entries) —
a **producer regression** in the intervening week (corpus also reset/regrew 48/52→80, so the break
may be in the data path, not a code commit — falsifier pre-registered in OQ-148). `classifications`
is a declared schema field (`shared/schemas.py:195`) referenced across ~40 python files; `sheaf_audit`
was the only one that crashed loudly. **The Pattern-5 risk is the quiet consumers that absorbed `[]`
into committed outputs reading as measurements** — this blast radius is OQ-148's spine and a
**candidate Critical-Distinctions tripwire** once the true consumer set is characterized. Pointer:
ISSUES.md OQ-147/OQ-148; commit at close.

## 2026-06-18 — OQ-146: orbits metadata-key landmine — single-source `load_orbits_constraints`
**Files:** python/shared/loader.py, python/oracle_gap_analysis.py, python/game_theory_nash.py, python/sweeps/product_site_delta_sweep.py, python/sweeps/structural_config_sensitivity.py, python/tests/alt_power_transform_test.py, python/tests/alt_power_transform_test_3k.py, ISSUES.md
**Tier:** landed

OQ-29 stamping put a top-level `corpus_hash` (a `str`) into `product_site_orbits.json`, a flat
`{id:{…,contexts}}` dict with no metadata namespace. Every consumer iterating top-level keys as
constraints crashed on it ("worked before" = un-stamped orbits had no such key). Census (`git grep
-ln product_site_orbits` + iteration-idiom grep; positive control re-found all 5 known exposures
**and** surfaced `structural_config_sensitivity.py:529`) → 6 exposed consumers. Fix: one fail-loud
predicate `shared.loader.load_orbits_constraints` — **partition-and-assert**: keep dict-with-`contexts`,
drop only allowlisted metadata (`_ORBITS_METADATA_KEYS={"corpus_hash"}`), **raise** on any
unclassifiable top-level key (no silent undercount). All 6 consumers repointed (inline `7b5801f0`
filter in oracle_gap replaced too). **Crash-over-drop ruled safe by producer construction:**
`product_site_export.pl:80–96` emits `"contexts"` unconditionally; key set is a static Cartesian
product (`constraint_indexing.pl:1052`) that never reads the corpus → every entry (live + every
archive) has `contexts`; a top-level entry lacking it can only be metadata/corruption.

**Tripwire (NOT promoted — distinct from a silent mistake; the failure is a loud crash):** anywhere
you iterate an orbits file as constraints, use `load_orbits_constraints`, never raw `json.load` +
`.items()`. When a new top-level metadata key is added, hand-bump `_ORBITS_METADATA_KEYS` AND the
hardcoded literal in the loader's set-equality test (the deliberately-unshared literal is the
tripwire proving the metadata set was consciously expanded). Witnesses: ISSUES.md OQ-146 (set-equality
75 vs raw 76; partition-assert raises naming `junk`; orbit_data.json no-op 75; per-consumer two-sided
all yield exactly 75; oracle_gap + game_theory_nash run end-to-end). Out of scope: `sheaf_audit.py:515`
ZeroDivisionError (corpus-size bug, not this class).

## 2026-06-18 — OQ-104: audit_citation_status.py built (standing checker, ungated)
**Files:** python/audit_citation_status.py, ISSUES.md, audits/2026-06-18_oq104_citation_checker/
**Tier:** landed

New `python/audit_citation_status.py` — sibling of `issues_status.py`/`known_state_status.py`;
verifies every path cited from `audits/*.md` exists-AND-tracked OR is allowlisted-ephemeral
(the fresh-clone invariant). **NOT in `scripts/gate.sh`** (ungated until FP rate is ruled).
Three WARN sublabels, three destinies: `untracked-pending` (`--promote-untracked`),
`missing-pending-M` (`--promote-missing`), `grammar-ambiguous` (never promotes). A gitignored
path inside the repo root is **never** allowlisted — it IS the OQ-104 signature.

Census: 1224 citations/85 dirs. **untracked-pending = 35 distinct, all `outputs/*`** — all
descriptive references to canonical regenerable outputs (schema docs, CLI defaults, command
lines), none the dangerous frozen-evidence class. **Operator ruling 2026-06-18:** leave
flagged, non-gating; copy-into-audit-dir inapplicable (outputs/ regenerated → faith-merge),
allowlist forbidden; `--promote-untracked` deferred. **missing-pending-M = 66 distinct** (drove
278 plan-upper-bound → 66; every survivor classified as relocation/illustrative/archive-shorthand/
deleted-output — no live broken citation). Controls: `controls.py` 23/23 (caught a `/etc/passwd`
field-list bug), `controls_run.sh` idempotence + rot-sensitivity (pass→flag on `git rm --cached`).
Promotion conditions + brace/glob + descriptive-outputs seats recorded as wiring triggers.
Evidence: `audits/2026-06-18_oq104_citation_checker/FINDINGS.md`. OQ-104 stays **open**.

## 2026-06-18 — OQ-29 RESOLVED: corpus_hash single-sourced; 14 producers stamp; consumers fail-closed
**Files:** python/corpus_hash.py, python/run_pipeline.py, python/enhanced_report.py, python/sweeps/perturb.py, python/sweeps/census_sweep.py, python/sweeps/persistence_sweep.py, python/axiom_reachability.py, python/sweeps/epsilon_sensitivity.py, python/audits/metric_audit.py, python/audits/sheaf_audit.py, AGENTS.md, ISSUES.md
**Tier:** landed

The corpus staleness fingerprint was a **Pattern-2 silent fork** — four byte-identical
`_compute_corpus_hash` copies (`perturb.py`, `run_pipeline.py`, `census_sweep.py`, + the
perturb-imported copies). The plan's census found 2; grep found the 3rd (`census_sweep.py`).
Consolidated into `python/corpus_hash.py` (`compute_corpus_hash` + fail-closed
`assert_corpus_current`); identity witness = every path `d2b3ec9429f1` on current `testsets/`.
Commits `b6aefb5a` (A), `4ab980ff` (B/C), `7b016978` (D).

- **10 producers self-stamp** (Thread B): the 9 plan-listed sweeps + `persistence_sweep` (a 10th
  the plan missed — it produces `persistence_results.json`, consumed by `enhanced_report`). Also
  fixed `persistence_sweep.py:32` standalone-import crash (`parents[2]`→`parents[1]`).
- **Consumer guards fail-closed** (Thread C): `run_pipeline.check_orbits_corpus_hash` upgraded
  presence-only → match (closes the residual: a stale-but-stamped orbits file used to pass);
  `enhanced_report.build_persistence_section` surfaces STALE/WARNING; `persistence_sweep` warns on a
  stale bifurcation input. Three-sided witness: match=pass, mismatch/absent=raise, no-file=pass.
- **Thread D, set-level discipline corrected the plan twice:** plan said "5 dead orphans, none
  cited." A set-level doc-citation probe (positive control: flags v3 + bifurcation) showed only 2
  are clean deletes (`config_sensitivity_results_test`, `structural_config_sensitivity_results_original`
  — deleted). Two others (`alt_power_transform_results_3k`, `test_battery_results`) have LIVE
  write-only test producers in `python/tests/` (no reader anywhere) → kept, excluded as a class
  (one runs vs the 3k ARCHIVE, so a testsets-keyed stamp would be wrong). One
  (`config_sensitivity_results_v3`) is doc-cited → kept + annotated. Pre-reset annotations added to
  `project_orientation.md`, `config_sensitivity_v3.md`, `CONFIG_SENSITIVITY.md`, **AGENTS.md** (the
  set-probe caught a third live-framed site the plan's "only two" missed).
- **Residual CLEARED (→ resolved):** the 4 scoped-out producers now stamp (`axiom_reachability`,
  `epsilon_sensitivity`, `metric_audit`, `sheaf_audit`; runtime control = `sheaf_audit_results.json`
  freshly stamped). The Fisher consumer (`enhanced_report.py:1903`) is guarded — stale/absent-hash
  `epsilon_sensitivity_results.json` surfaces STALE, never renders pre-reset numbers (four-sided
  witness). Audit-script ruling settled by probe (NOT defaulted): both load live
  `pipeline_output.json`/`orbit_data.json`, so a testsets-keyed stamp is the correct identity.
- **Two pre-existing bugs surfaced while exercising (NOT OQ-29, not fixed here):**
  `sheaf_audit.py:515` ZeroDivisionError (empty working set on the small post-reset corpus);
  `oracle_gap_analysis.py:143` `entry["contexts"]` indexed on a string.

**Promotion test:** the standing convention ("new producers stamp `corpus_hash` via
`corpus_hash.py`, never re-define the body; archive runs stamp the archive corpus") is promoted to
AGENTS.md (Config sensitivity sweep §); not CLAUDE.md (not a silent-mistake tripwire before editing a
named file — it's a build-time convention for NEW producers).

## 2026-06-18 — OQ-115 RESOLVED: abductive_helpers phantom under [stack] fixed; check_stack back to 4-finding baseline
**Files:** prolog/stack.pl, prolog/signature_detection.pl, prolog/check_stack.pl, ISSUES.md (OQ-115, OQ-142/143/144/145)
**Tier:** landed

Under bare `[stack]`, `signature_detection:signature_grade/2` (`signature_detection.pl:1624`)
called `abductive_helpers:known_override_signature/1` where the module was a phantom
(`current_module` TRUE, `module_property(_,file(_))` FAILS) → existence_error. The pipeline
was unaffected (loads it via json_report → diagnostic_summary), so the green B4 gauntlet hid
it; the OQ-98 alert path minted the reference after the 2026-06-04 baseline, making it the one
check_stack regression. **Fix:** `:- use_module(abductive_helpers, []).` in `stack.pl`
(`check_stack.pl:27` is `:- [stack].`, so the checker's image picks it up). **Option 1
rejected by evidence** — importing in signature_detection cycles tighter than the in-file
comment said: `abductive_helpers → maxent_classifier → signature_detection:constraint_signature/2`
(`maxent_classifier.pl:60`), plus the grothendieck→drl_core arm; the falsified `:1611-1617`
comment was corrected. **Witnesses (cold `[stack]`, corpus-free):** bite-call before → `THREW`
(`existence_error(procedure, abductive_helpers:known_override_signature/1)`); after →
`RETURNS`. check_stack after: no abductive line; 4 documented baseline findings.

**Class sweep (operator expansion):** partitioned all 4 remaining baseline findings, each with
its own pasted non-bite witness (none inherits baseline-trust). Discriminator = **phantom ×
guarded × reachable**: a reference bites only when target-absent at the call's load chain AND
unguarded AND reachable. OQ-115 was the only unguarded bite. `validation_suite:test_case/4` =
the guarded negative control (then-arm under `current_predicate/1`; else-arm doesn't reach it).
`data_repair:constraint_{beneficiary,victim}/2` = xref mis-attribution of `acc_has/2`'s
`narrative_ontology:Fact` goal-call into a dynamic/multifile target (`fails_clean`, not a
throw). `drift_events.pl:175` = a real latent OQ-57-class wrong-qualifier (`narrative_ontology:`
should be `domain_priors:`; the OQ-57 fix patched the sibling `:236`, missed `:175`) held off
only by being unreached. Tracked as **OQ-142** (parent) + **OQ-143/144/145** (the plan's
`142a/b/c`; renamed because the tracker label grammar is `OQ-\d+` — lettered sub-IDs are
invisible to `issues_status`/`omega_resolver`, witnessed). **Promotion test:** stays history —
the failure is a loud `existence_error`, not a silent miscompute.

**OQ-145 RESOLVED same session (the one code change of the sweep):** `drift_events.pl:175`
wrong qualifier `narrative_ontology:` → `domain_priors:` (mirrors the OQ-57 sibling fix at
`:236`). Reachability control-backed before landing: static unreached (probe 0, positive control
`drift_event`=19 fired), runtime-constructed path left explicitly unverified — fix correct
regardless. **Witness (cold `[stack]`, synthetic constraint extractiveness 0.05 / theater_ratio
0.80 to reach `:175`):** before → THREW `existence_error(procedure,
narrative_ontology:requires_active_enforcement/1)` in `context(drift_events:detect_is_piton/1)`;
after → `SUCCEEDED_CLEAN`. **check_stack baseline now 3** (was 4). OQ-143/144 remain annotate-only.

## 2026-06-18 — OQ-111 RESOLVED: dead data_repair omega bridge retired (zero-diff removal)
**Files:** prolog/data_repair.pl, ISSUES.md (OQ-111), docs/design/design_gaps.md (GAP-13)
**Tier:** landed
`bridge_omega_variables_pure/3` keyed its module lookup on the BARE interval id while testsets
declare facts in module `constraint_<id>` — so it always missed and imported zero omegas on every
report run (Pattern 6; OQ-99's wrong-module twin). RETIRED, not fixed: operator ruled
`prolog/archives/datasets/*` out of scope, closing the bridge's only genuine consumer (v3.4-legacy
UNPAIRED testsets; the live corpus is 100% paired and authored omegas already render via
`report_generator.pl:709`/`:776-794`). Removed the predicate + its `bridge_v34_data/2` call + the
now-dead `persist_single(omega_variable(...))` clause (tombstones in-file); also retired a secondary
defect (the /5 branch fabricated type `empirical` for a typeless 5-arity fact). Deferred capability
logged as GAP-13 with the re-introduction recipe. **Witness:** pre-removal probe on
`border_control_legitimacy__freedom_of_movement_primary` confirmed the no-op fired (bare_module FALSE
/ constraint_module TRUE / 5-arity present / 0 imported); removal is behavior-preserving — ZERO DIFF
on three omega-authoring reports across raw `run_scenario` + `enhanced_report.py`; dynamic suite GREEN
(80/0/0); [GATE] GREEN. No tripwire promoted (the bridge is gone; nothing silent remains).

## 2026-06-18 — OQ-48 recalibration-readiness audit: 0 thresholds recalibratable against the twins (all MODEL-CONFOUNDED)
**Files:** ISSUES.md (OQ-48), audits/2026-06-18_oq48_recalibration/, python/audits/oq48_threshold_distributions.py, python/audits/oq48_analyze.py, python/audits/oq48_triangulate_kernel_v1.py
**Tier:** landed

Read-only distribution-break audit of the 7 in-scope χ/ε/suppression classification cuts (config.pl,
691-corpus-provenanced) against the twins (`testsets_haiku`/`testsets_flash` = 960 each). Pre-registered
verdict rule (KDE antimode + bandwidth-robustness + lobe-mass + Dip; cross-twin agreement = validity gate).
**All 7 → MODEL-CONFOUNDED, 0 proposed values, no `config.pl` edit.** Every metric multimodal on both twins
(Dip p=0), but flash's antimodes fail bandwidth-robustness where their locations track haiku's ("soft
agreement, hard disagreement") → no DRIFTED candidate. Two cuts corroborated by haiku alone (`snare_chi_floor`
0.66≈0.666, `snare_epsilon_floor` 0.46≈0.484). Confounded kernel_v1 arm (1106, pre-reset/pre-de-leak,
corroboration-only, never pooled per OQ-26) cross-regime-corroborates `snare_epsilon_floor` (0.46); the rest
uncorroborated. Controls pass (LOADCOUNT 960/960/1106 via asserta; 0 unknowns; byte-identical re-run;
planted-gap recovered 0.4506). **OQ-48 stays open** — closure waits on corpus regeneration beyond the twins
(same-regime third corpus breaking the tie, or the live rebuild reaching the ~700-story Tier-4 bar).
Promotion test: NO — a result qualification, not a silent-edit tripwire; stays here. Provenance: twin TSV
sha256 haiku `7039d37b…`/flash `3c24b1d2…`, metric-code commit `0a629077`.

---

## 2026-06-18 — OQ-122 CLOSED: physics-RED fixed by OQ-128; FSM victim-gate DROPPED, discriminant handed to OQ-138
**Files:** ISSUES.md, prolog/drl_core.pl (witness only, no edit), outputs/pipeline_output.json (witness)
**Tier:** landed

Closed OQ-122. The control-inversion / physics-false-RED that filed it is FIXED by OQ-128's type_1
discrimination, NOT by the held FSM victim-gate: on live (commit `2172d55`, manifest 2026-06-18) both
`radiative_levitation_stratification` and `actinide_replenishment_mechanism_flat_control` read
`verdict_join.verdict=yellow`, `cap_applied:none`, `type_1_false_summit=informational` at every seat.
`false_summit_mountain` still fires (vic=0) but only adds a `signature_correction/moderate` alert while
`base_verdict` is independently yellow — so the gate's verdict benefit is now ≈0.

**FSM victim-gate (`oq122-fsm-victim-gate`, `ab1e9b26`) DROPPED — superseded by the engine-ROUTES-never-
RECLASSIFIES architecture (OQ-128).** The gate is a suppress-the-detector reclassification, the shape
OQ-128 removed; the branch's single-clause diff is recoverable at `ab1e9b26`. Its INSIGHT survives,
re-shaped for **OQ-138**: discriminate the FSM signature's severity (`vic=0→informational/route`,
`vic>0→moderate/floor`) — the exact analogue of the type_1 ε-split — with the pre-witnessed discriminant
(`testsets_flash` 18 vic=0 / 22 vic>0, `audits/2026-06-13_oq122_retype_discriminator/breadth_sweep_results.txt`)
handed to OQ-138 as its FSM-clause build spec. neutron_star/FCR stays under OQ-70. Branch to be deleted
after merge.

---

## 2026-06-17 — OQ-128 type_1 cap RULED + BUILT: discriminated severity (withhold high-ε snare, route low-ε artifact)
**Files:** prolog/drl_core.pl, ISSUES.md (OQ-128)
**Tier:** landed

The type_1 RED-cap ruling (a NEXT RULING after the sink). The type_1 `severe` alert was OVERLOADED:
it fired identically on (a) a mountain-claim the engine degrades to SNARE (high-ε real false summit, a
defect) and (b) degrading to rope/other (the arc's universal non-diagnostic degradation of genuine low-ε
mountains — the same artifact that made natural_law a free retirement). Witnessed clean ε gap in the
mountain-claimed population (snare-at-seat ε≥0.50, rope-at-seat ε≤0.25, nothing between, KILL=0 across six
corpora ~7000). Operator ruled **discriminated severity**: the `dr_claim_mismatch` type_1 clause
(`drl_core.pl`) is split — degrade→snare = `severe` (withhold, RED floor); degrade→other = `informational`
(routes via the sink, no headline floor). **Tripwire:** do NOT collapse it back to a single `severe` — that
re-overloads the alert and re-launders genuine math/physics mountains into RED. Acceptance witness: RED
389→102 across six corpora (287 RED→non-RED), all 10 v5 mountain-claimed snare-at-analytical STAY RED,
`dr_type` byte-identical. **KILL:** a future corpus authoring a mountain-claimed snare-at-analytical at
0.25<ε<0.50 breaks the clean gap → re-run the χ-decomposition. The `severe` of type_3/type_5 is untouched.

## 2026-06-17 — OQ-128 routing sink BUILT (engine ROUTES the author↔engine diff, never reclassifies)
**Files:** prolog/routing_sink.pl, prolog/signature_detection.pl, python/run_pipeline.py, python/enhanced_report.py, ISSUES.md (OQ-128), audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md
**Tier:** landed

The routing-sink design (ROUTING_SINK_DESIGN.md) was built. Three changes:
1. `:867` `resolve_modal_signature_conflict(_, natural_law, mountain)` RETIRED (tombstoned) in
   `signature_detection.pl` — the overwrite that manufactured mountain verdicts. The DETECTOR
   (`natural_law_signature` / `constraint_signature(C,natural_law)`) is LEFT INTACT as a socketed
   router input (unpowered: `HasAlternatives==false` is builder-unreachable). Witnessed behavior-neutral:
   `dr_type` byte-identical (288 rows), `dr_claim_mismatch` byte-identical (52 rows).
2. `prolog/routing_sink.pl` — per-SEAT `seat_diff/7` router. **Seven typed MECE addresses** (operator ruling:
   split, not a catch-all): §4's four (generation_gap / authoring_review / engine_exit_table_review / no_route)
   + `both_silent` / `engine_abstained` / `author_engine_divergence`. No `unrouted_residual`; each
   self-describing. Taps `dr_claim_mismatch/4` UNMODIFIED. Emits `outputs/routing_sink.json` with a coverage
   manifest. **Tripwire:** the leaf is per-SEAT — any predicate collapsing seats to one constraint verdict is
   the KILL condition (§9b.4), the aggregate-merge that recurred 3× in the arc.
3. Wired into `run_pipeline.py` Phase 2 (`routing_sink:run_routing_sink`). **Consumed by `enhanced_report.py`**
   — CONSTRAINT IDENTITY section renders each seat's address per-seat (after "Authored vs Computed").

Controls reproduced the arc's witness files exactly: thermo (clean uniform-mountain) →
`engine_exit_table_review` at moderate/institutional; topological (contested) → `generation_gap`
(moderate, the spec's literal example) + `author_engine_divergence` (institutional) + `authoring_review`
(analytical [mountain,rope]). Address-extension ruling RESOLVED 2026-06-17 (split into 7 typed addresses).
Next rulings (not built): type_1 RED-cap route-vs-adjudicate, FNL/FCR/FSM family, powering the detector
socket (§7).

## 2026-06-16 — Typed-absence corollary added to design canon + OQ-137 (reading-layer census)
**Files:** docs/design/design_discipline.md, ISSUES.md (OQ-137)
**Tier:** landed

Promoted the OQ-121 typed-absence convention from tooling notes to design canon:
`docs/design/design_discipline.md` §5 gains "Typed absence — a reading's silence is itself a
declaration" (corollary of S2/Corollary 2a). A reading an aggregate could consume must return a
typed token (`out_of_domain`/`absence`/measured), never fail silently — NOT "every predicate is
total" (genuinely relational lookups like `in_contention/3` correctly have no reading off-domain).
Templates: `constraint_signature/2`, `q6_cell/2`. **OQ-137 minted** to census the whole reading
layer against the convention (classify each aggregatable predicate total-on-domain / partial-by-
design / silently-failing-defect; fix defects; ideally a standing guard generalizing
`test_seat_totality.pl`). Scope discriminator + diagnostic positive-control requirement are in the OQ.

## 2026-06-16 — `census_sweep.py`: commentary census as a perturb measurement surface + denominator caveat
**Files:** python/sweeps/census_sweep.py, ISSUES.md (OQ-136)
**Tier:** correction-key

New tool pairing the perturb.py overlay method (retract/asserta a `config:param`, run a goal, diff vs
baseline) with the commentary census as the MEASUREMENT SURFACE. Diffs per-source bucket histograms +
`n_in_domain` / `coverage` / `prevalence`. Has a built-in positive control: the null perturbation
(re-apply the baseline value) MUST be inert, else it fails loud (overlay/parse bug). Commentary-grade,
so the sweep is pure observation (never feeds classification). `--param/--to` for one-offs, `--corpus`
to overlay a twin.

**Findings (live corpus, n=72):**
- **CORRECTION-KEY — a census RATE can move purely by domain-shrink.** `tangled_rope_chi_floor`
  0.35→0.85 raised extraction `prevalence` 0.060→0.067 (+12%) while `extraction_blindspot_fired` held
  at **3** — 5 extractive constraints fell out of the domain (`n_in_domain` 50→45). A single
  "prevalence" number reads this as a signal; it is a denominator artifact. **Rule: report raw `fired`
  + `n_in_domain` alongside any rate, or hold the domain fixed — esp. across config/schema-refit/corpus
  comparisons (the OQ-136 clustering test must use raw counts, not rates).**
- q6 `coverage` decomposes: `q6_unmeasured` (authoring) is config-INVARIANT; `q6_signature_unknown`
  (computational, dr_type→unknown) is config-VARIANT (8→10 under the same perturbation). Not one figure.
- The two census surfaces have ORTHOGONAL config-sensitivity: snare ε/χ-floor moves q6 (snare↔tangled,
  both extractive) but leaves extraction inert; only the extractive↔non-extractive boundary moves
  extraction. On this corpus the extractive domain's binding edge is the χ-floor, not the ε-floor.
- `config_validation` bounds the reachable sweep surface: single thresholds can't cross their neighbor
  (`snare_epsilon_floor`<`rope_epsilon_ceiling`; `tangled_rope_extraction_floor`<`…_ceil`). The tool
  records the rejection and continues. Witnesses: `audits/2026-06-16_census_sweep/`.

## 2026-06-16 — Partial-silent commentary predicates totalized (`consensus_provenance/2`, `seat_perceived_vs_real/4`) + OQ-136 minted
**Files:** prolog/stakeholder_seats.pl, prolog/tests/test_seat_totality.pl, ISSUES.md
**Tier:** landed

OQ-121 follow-up: the two remaining partial-silent R3 commentary predicates brought up to the
never-fail convention. Neither has any consumer outside the module (verified — no callers, no tests,
no negation-as-failure), so zero blast radius.
- `consensus_provenance/2`: was silent on `Ns=[]`; now TOTAL with explicit `no_agent_seats`
  (out-of-domain) and `seats_untyped` (absence). Live: plural 37 / no_agent_seats 21 / manufactured 8
  / unanimous 6 (Σ=72) — the 21 no_agent_seats silently failed before.
- `seat_perceived_vs_real/4`: was silent when the per-seat type couldn't derive on an existing seat;
  now returns `Computed = untyped`. Total over 370 live seats; `untyped` branch is a defensive guard
  (0 live triggers). Non-existent seat still correctly has no reading (domain boundary, not silence).
- Regression `prolog/tests/test_seat_totality.pl` 8/8; commentary_census 40/40, oq86 14/14 unaffected.
  Commentary-grade — not on the dr_type path. `mandatrophy_gap` is the last unconverted member.

**OQ-136 minted** (investigation): now that the census reports honestly, its absence/out-of-domain/
unnameable buckets are the first corpus measurements to interpret — 5 `extraction_unnameable`, 20
`q6_unmeasured`, 8 `q6_signature_unknown`, 21 `no_agent_seats`, 8 `manufactured_consensus_candidate`.
Pre-registered test: cluster by generation provenance/run-tag/topic ⇒ authoring artifact (generation
fix); spread + genuinely diffuse on hand-read ⇒ real category (keep/report). Witnesses:
`audits/2026-06-16_partial_silent_totalization/`.

## 2026-06-16 — OQ-121 RESOLVED: totalize the commentary family + domain-relative census coverage
**Files:** prolog/stakeholder_seats.pl, prolog/commentary_census.pl, prolog/tests/test_commentary_census.pl, python/run_pipeline.py, outputs/commentary_census.json
**Tier:** tripwire

A closer look at OQ-121 (operator asked) found a structural issue bigger than the missing coverage
ruling. **The engine already has a never-fail discipline** — correction-grade `constraint_signature/2`
(`signature_detection.pl:136`, explicit `unknown` fallback "instead of a default-fabricated verdict")
and `q6_crosscheck/3` (explicit absence buckets) always return an EXPLICIT token, never fail silently.
The rest of the R3 commentary family never got it: `extraction_reading/2` **failed silently**,
destroying the provenance bit at the source so no aggregate could reconstruct it (Pattern 6 in its
purest form).

**Built:**
- `stakeholder_seats:extraction_state/2` — TOTAL (mirrors `q6_cell/2`): every constraint reaches
  exactly one of `out_of_domain` / `extraction_clear` / `extraction_unnameable` / `extraction_fired(Es)`.
  `extraction_reading/2` now rides on `extraction_fired`, so its fire-or-silent report contract is
  UNCHANGED (oq86 14/14 green; report/sidecar output identical).
- `extraction_unnameable` (extractive ∧ no victim ∧ no nameable extractor) is its own bucket —
  **5 live constraints surfaced that the silent failure had hidden entirely.**
- `commentary_census.pl`: three bucket KINDS (out-of-domain / absence / measured), `coverage` is now
  **domain-relative** (`(n_in_domain − Σabsence)/n_in_domain`), `prevalence` (`fired/n_in_domain`) is a
  DISTINCT number. q6 unchanged (universal domain → 0.611); extraction `coverage 1.0`/50, `prevalence 0.06`.

**TRIPWIRE (the silent mistake a fresh agent makes):** when adding a new `commentary_cell/3` source to
the census, (1) make the per-constraint hook a TOTAL function (return an explicit out-of-domain/absence/
measured bucket — NEVER let it fail; a bare failure collapses out-of-domain, measured-clear, and absent
into one token); (2) declare its out-of-domain buckets — census `coverage` is DOMAIN-relative
(`n_in_domain = n_corpus − Σood`), NOT corpus-relative; a corpus-relative coverage silently claims
coverage of constraints the reading never applied to; (3) coverage ≠ prevalence ≠ corpus-fraction —
keep them separate; (4) a source ships a coverage ratio ONLY if `commentary_coverage_decidable/1` flags
its bucket sets ruled-complete. The full convention + rationale is in `commentary_census.pl`'s header.
`consensus_provenance/2` and `seat_perceived_vs_real/4` are still partial-silent but NOT census sources,
so not a live defect — bring them up to the total shape if/when censused.

Witnesses: `audits/2026-06-16_oq121_totalization/`; plunit 40/40; full resolution in ISSUES.md OQ-121.

## 2026-06-16 — OQ-134 RESOLVED: generic commentary-grade corpus census (`commentary_census.pl` + pipeline wire)
**Files:** prolog/commentary_census.pl, prolog/tests/test_commentary_census.pl, python/run_pipeline.py, outputs/commentary_census.json, outputs/commentary_census.md
**Tier:** landed

New read-only aggregator automating the by-hand q6 census as a kept-fresh pipeline artifact.
`prolog/commentary_census.pl`: a GENERIC commentary census (operator ruling — build the generic
exporter, not a q6-special one). Multifile `commentary_cell(+Source,+C,-Bucket)` hook (one clause
per source), `commentary_absence_bucket/2` (didn't-look buckets), `commentary_coverage_decidable/1`
(absence set RULED-complete → coverage ratio allowed), `commentary_census/2`, `run_commentary_census/0`.
Sources: `q6` (= `stakeholder_seats:q6_crosscheck/3`) and `extraction_reading` (= OQ-86, fired/silent).
`python/run_pipeline.py:_prolog_commentary_census` (Phase-2 task `commentary_census`,
`_PREAMBLE_MARKERS['commentary_census']`) parses the `CENSUS*` lines → `outputs/commentary_census.{json,md}`
with a corpus-identity manifest (n_constraints, corpus_hash, commit). Commentary-grade — own swipl
process, reads only, never on the classification path.

**Key design facts (carried so a fresh agent extending it stays honest):**
- **Sum invariant is the contract enforcer.** Census tallies via `findall` over the BUCKETS (not
  per-constraint `once`), so Python asserts `Σ buckets == n_corpus` AND `n_corpus > 0` per source.
  A non-deterministic `commentary_cell` over-counts (caught), a failing one under-counts (caught) —
  "exactly one bucket per (source, constraint)" is a CONSEQUENCE of the check, not a trusted property.
  The `n>0` clause closes the vacuous `0==0` that a forgot-to-load run would pass.
- **Coverage = "both sides MEASURED," not "landed in a named cell"** — so `q6_unclassified` counts as
  covered (q6 coverage=0.611=44/72; the 28 absent = `q6_unmeasured`(20)+`q6_signature_unknown`(8)).
- **`extraction_reading` coverage ships `null`/N/A, NOT a default 1.0** — whether `extraction_silent`
  is present-residual or didn't-look is UNRULED; a 1.0 we cannot defend is the exact Pattern-6 absence.
  Honesty wired structurally: a source ships a coverage ratio ONLY if `commentary_coverage_decidable/1`
  declares it (empty absence-set ≠ ruled-none). **[SUPERSEDED same day by OQ-121 — see below: extraction
  was totalized, coverage is now 1.0 over its 50-constraint domain, prevalence 0.06.]**
- **Absence buckets are load-bearing (fail-closed control):** pre-stakeholder archives
  (kernel_v1/v5/v6/sotu) route 100% to `q6_unmeasured`, ZERO named cells — the census never fabricates
  a verdict from absence. `q6_unclassified` is `0` on live but corpus-reachable on twins (haiku=1,
  flash=5) — the manifest's corpus identity makes the live `0` self-labeling, never hardcoded.

**Extension point:** a new commentary source is a one-clause `commentary_cell/3` add (+ source/absence/
decidability decls). Future-cheap family: `consensus_provenance/2`, `seat_perceived_vs_real/4`,
`mandatrophy_gap` — no open OQ requests them yet. Witnesses + raw output:
`audits/2026-06-16_oq134_commentary_census/`; full resolution in ISSUES.md OQ-134.

## 2026-06-16 — OQ-86 RESOLVED: `extraction_reading/2` R3 commentary (no-authored-victim blindspot)
**Files:** prolog/stakeholder_seats.pl, prolog/report_generator.pl, python/enhanced_report.py, prolog/tests/test_oq86_extraction_commentary.pl, prolog/data_repair.pl
**Tier:** tripwire

Shipped the OQ-86 reporting feature: `stakeholder_seats:extraction_reading/2` (+ `extractive_type/1`,
`authored_victim/1`), `report_generator:extraction_reading_line/1` (Section 7, beside the q6 crosscheck),
and the `extraction_reading` sidecar (`enhanced_report:extract_extraction_reading`). R3 commentary —
NEVER a classifier input. Fires on the blindspot shape: constraint-level `dr_type ∈ {snare,tangled_rope}`
∧ no **authored** victim ∧ ≥1 beneficiary-side agent seat; names the beneficiary-side seats, flags the
cost-bearer as prose-only. 24/24 plunit (positive + channel + 3 single-var negatives + bridge regression).

**TRIPWIRE (the silent mistake a fresh agent makes):** the data-repair bridge `data_repair.pl:153`
(OQ-93 shim-family) FABRICATES `constraint_victim(C, inferred_subject)` whenever E>0.46 ∧ S>0.40 and no
victim is authored — i.e. on the EXACT blindspot metric profile. So by report time the DB ALWAYS holds a
victim for the very case OQ-86 exists for; a naive `\+ constraint_victim(C,_)` guard is INERT on every
real report (Build Discipline P5/P6 — a fabricated success-shaped token fills the no-victim hole). Any
predicate that means "the STORY authored no victim" must exclude the `inferred_subject` sentinel
(`authored_victim/1` is the template). Witnessed: without the exclusion the end-to-end channel witness on
the blindspot fixture was silent; with it, the line + sidecar emit.

**Empirical (cross-corpus census, witnessed):** fires on 3 live testsets (plan predicted 0 — wrong),
10/960 testsets_haiku, 34/960 testsets_flash — ALL `tangled_rope`, ALL no authored victim. **0** across
kernel_v1/v5/v6/sotu (~5,377 stories): guard C fail-closes (those pre-stakeholder archives have 0 seat
facts; 62 kernel_v1 constraints pass guards A+B but cannot name extractors → silence, correct). EVERY real
firing is `tangled_rope` — omitting it from `extractive_type/1` would make the feature 100% inert on real
corpora (snare never fires outside the constructed fixture). Sets the table for OQ-134 (uniform sidecar shape).

## 2026-06-16 — Seat/orientation invariant audit + v8 "seat/gauge/orientation" design spec (engine votes one-seat)
**Files:** docs/design/v8_seat_gauge_orientation_design_spec.md, audits/2026-06-16_seat_invariant_vs_prolog/, docs/seat-theorem-v1.md, docs/deferential_realism_paper_v7.md
**Tier:** landed

Read-only seat/orientation invariant audit ran (REPORT.md + evidence; merges `c58611a8`/`864c961d`):
per-prediction verdicts P1–P9 + theory-killers, conditional-decision-tree synthesis (no net vote).
Headline R3 ("genuine second seat?") settled by a pre-registered **presentation-vs-structure** probe
(`evidence/probe_r3_presentation_vs_structure.pl`, merge `77e33bca`): `cs_pattern`/`cs_classify`
(cs_pattern_detection.pl:108–169) is a **pure function of authored presentation** (`cs_kernel_codification`
+ `cs_authority_grounding`), **blind to binding structure**; the `cs_verdict` false-X layer audits the
presentation against authored metric/beneficiary reality, **one-directionally**. → **engine votes ONE
seat**; the committer/CS axis is the **orientation (showing) face**, not a second content-seat. The R3
*declaration* is the operator's seat — evidence supplied, not ruled.

**v8 design spec** drafted through rev3 (`docs/design/v8_seat_gauge_orientation_design_spec.md`; merges
`403375e4`/`f6c22b81`/`1e81bc0f`): unifies `seat-theorem-v1` (law) + v7 (two-axis realization) + the CS
engine (mechanism) under **seat / gauge / orientation**; seat/face line drawn by **audit direction**;
standing invariant = a **transitive cross-axis taint property** (no committer field reaches observer
computation by *any* path except entailment-typed payload on the single forward `influences` bridge);
kill-condition = any other committer→observer *computation* path (reverse bridge / payload widening /
(B)-seam promotion). **Spec is a draft FOR REVIEW, pre-implementation.**

**TRIPWIRE (soft now; hard on v8 adoption):** v7's word **"seat" = v8's "gauge"** (an observer position);
v8's **"seat" = v7's ε-invariant content**. Reading "seat" across v7 and the v8 spec without the spec's §4
bridge table miscounts them as two content-seats (the error that produced the discarded two-seat hypothesis).

Two related docs added by the operator (web instance), **untracked**: `docs/one_seat_audited.md` (the
One-Seat *verification corollary*; superseded the two-seat draft) and `docs/provenance_is_not_proof.md`
(investigative essay — **NOT for commit** per the finished-essay convention; names a living person under a
defamation check → only *structural* claims may migrate to v8, with the intent-humility framing).

**NEXT STEP (needs an operator-authored OQ — `Priority:` is the operator's seat):** adopt v8 (rule the spec's
Q4 vocabulary) → a fresh CC instance plans implementation from spec §8 (priority-1 = promote the transitive
taint guard to a checkable **dataflow** guard with the two positive controls; then the low-stakes vocab
migration). Blocked on operator adoption + web review.

## 2026-06-16 — Orientation is a deferred Ω_E, NOT Ω_P (OQ-133 relabel) + verification-depth discipline
**Files:** ISSUES.md, docs/technical/build_discipline.md, CLAUDE.md
**Tier:** correction-key

**The relabel.** OQ-133 was filed `Ω-type: Ω_P (orientation)`. Resolved against
`docs/omega_variables.md`'s own definitions, that is wrong: Ω_P is a value judgment that differs
*legitimately across stakeholders* (resolved by those bearing the cost deciding); **orientation**
(a concealment's enclosure vs survival vs defense) is a **fact about the actor's actual stance** —
observers differ in *access*, not legitimately in *values* — whose named resolution operation is
*world-observation* (the longitudinal Cor-3 confrontation-response signature = the paradigm Ω_E
operation). So orientation is a **deferred Ω_E**, status: awaits the t0-anchor tier (OQ-133 itself).
**Why it was load-bearing, not taxonomic:** the Ω-type field routes resolution — `Ω_P` routes to
"someone bearing the cost declares it," which licenses the encloser to **self-certify as a defender
by fiat** (the concealment move blessed by the routing); `Ω_E` withholds that license, forcing the
verdict to be earned from the honor/reabsorb pattern. **Boundary (the Ω_E claim's falsifier):** the
signature tracks orientation only absent strategic gaming (a sophisticated encloser can *perform*
honoring, forging the longitudinal witness); under gaming it falls **outside the framework entirely**
(`omega_variables.md` Mechanism Boundaries exclude strategic gaming) — Ω_E in the non-gaming regime,
out-of-framework under gaming, **never Ω_P.** Do NOT collapse this with `contested_open` (rule 11),
which IS a genuine Ω_P/Ω_C (legitimate dispute about the founding problem; engine abstains): same
surface OPEN, opposite type/operation (route-to-deferred-measurement vs abstain-as-preference).

**The discipline added (CLAUDE.md synthesis-list (5) + `build_discipline.md` → *When to stop
verifying*).** "Verified enough" is a seat with no floor (`seat-theorem-v1.md` §8); the checkable
substitute is the conceal-an-open check: for each verdict/name, name a tier-available falsifier or
**downgrade to OPEN = route to a typed Ω**. This is `omega_variables.md`'s structural-convergence
stopping rule — but cite its **cost-benefit** line, not the stable-marriage terminus, because the
verification regress is *generative* (manufactures new dials), which the doc's Mechanism Boundary
excludes. Provenance: the `q6_crosscheck` review arc; the confident "Ω_P" was itself a concealed
open this check would have caught.

---

## 2026-06-16 — R5 Q6 synchronic crosscheck completed: `q6_crosscheck/3` replaces `zombie_piton_crosscheck/2`
**Files:** prolog/stakeholder_seats.pl, prolog/report_generator.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, python/linter.py, ISSUES.md
**Tier:** landed

`stakeholder_seats:zombie_piton_crosscheck/2` (single dead×piton cell) is GONE — replaced by
`q6_crosscheck(C, Cell, Daylight)`, the full status×computed-signature matrix. Commentary-grade
(NEVER overrides `dr_type`; sole caller `report_generator.pl:r5_zombie_crosscheck_line/1`, NOT in
json_report's per_constraint path so classification is byte-identical by construction). A fresh
agent grepping for the old name or the old `corroborated_zombie` verdict will not find them —
loud failure (predicate absent), so this is history, not a CLAUDE.md tripwire. Four non-verdict
buckets kept distinct: `q6_unmeasured` (authored absent) / `q6_signature_unknown` (computed
absent) / `q6_unclassified` (present, fell through — mountain/scaffold/naturalized × live/dead) /
out-of-domain → lint fail-loud. `q6_cell` is a mode-robust if-then-else (computes into a fresh var,
unifies last) — a multi-clause first-match with an unguarded catch-all let `q6_crosscheck(C,
q6_unclassified, _)` spuriously match all 71 (caught by its own positive control). Witness:
`dr_type/2` = `default_context` = analytical (constraint_indexing.pl:156–161); `q6_unclassified`
WITNESSED 0 on live corpus but reachable on twins (haiku=1, flash=5, all `live × mountain`).
Daylight axis (`founding_problem_corroboration_class/2`, authored atom, lint-gated) SHIPS INERT —
all stories `daylight(unstated)` until a bounded R5 backfill lands (OPEN graduation step). Audit:
`audits/2026-06-16_q6_crosscheck_completion/`. Tracking: OQ-83 follow-through; deferred diachronic
(confrontation-response) tier → OQ-133.

---

## 2026-06-16 — `python/paths.py` is the canonical path source (depth-agnostic); 3 absolute-path bugs fixed
**Files:** python/paths.py, python/domain_priors_expander.py, python/sweeps/range_sweep.py, python/tests/diff_cut_proof.py, AGENTS.md, ISSUES.md
**Tier:** tripwire

New code MUST import filesystem roots from `python/paths.py` (`REPO_ROOT, PROLOG_DIR, TESTSETS_DIR,
JSON_DIR, OUTPUTS, SCHEMAS, PROMPTS, ...`) — never re-derive with `Path(__file__).parents[N]`
(depth-fragile) or hardcode `/home/...`. Root detection walks up to the `pyproject.toml` marker
(depth-agnostic; survives worktrees where `.git` is a file, and tarball/CI). Nested scripts use the
byte-identical bootstrap in AGENTS.md §3 (same sentinel walk → copy-safe from any depth). Fixed the
3 files that hardcoded `/home/scott/...` (domain_priors_expander, sweeps/range_sweep,
tests/diff_cut_proof). Witnessed: paths.py resolves == the old hardcoded values; bootstrap finds the
same root from 6 depths. ~69 scripts still re-derive inline — migrating them + the
package-vs-`paths.py` decision is OQ-132 (held; do not bulk-migrate before A-vs-B is ruled).

## 2026-06-15 — OQ-131 Q1 (Ω_E) measured: 6-vs-4 observer site is consonant-suppressing, NOT a combinatorial artifact
**Files:** prolog/constraint_indexing.pl, prolog/config.pl, prolog/config_schema.pl, prolog/config_validation.pl, python/audits/oq131_six_observer_probe.py, audits/2026-06-15_oq131_six_observer/, ISSUES.md
**Tier:** landed

Added three **additive** observer site modes to `constraint_indexing:site_contexts_for_mode/2`
(commit `a06b5c7f`): `canonical_6` (canonical 4 + powerful/organized seats), `power_only_4`,
`power_only_6`. First-arg indexed, **no catch-all**, so `canonical`/`product` resolve byte-for-byte
as before. New seats are appended AFTER the canonical four ⇒ the 6 canonical observer-pairs stay
positional and the entire 4→6 H¹ delta is the 9 new pairs. **Tripwire:** the canonical-first
ordering is load-bearing — it is what makes the `(H¹₆−H¹₄)/9` headline conditioning valid and was
witnessed (9-pair basis PASS for every constraint, all three corpora). Don't reorder.

Seat bundles are **declared-revisable** `config.pl` params (`observer_bundle_powerful` =
logic.md:530 elite perspective; `observer_bundle_organized` revisable; `observer_baseline_tes` =
moderate canonical coords for the single-coordinate control). These are **compound** terms — I added
a `type_ok(compound, V)` clause to `config_validation.pl` and `compound` to the `config_schema.pl`
type vocabulary + three `param_spec`s; **every config param needs a schema spec or `[stack]` halts
at load** (witnessed: 3 "no schema spec" errors before I registered them).

**Finding (`audits/2026-06-15_oq131_six_observer/`, pre-registered):** observed `(H¹₆−H¹₄)/9` falls
BELOW the permutation band (N=1000, seed=20260615) on live (0.446 vs [0.741,0.825]), haiku (0.562 vs
[0.738,0.755]), flash (0.550 vs [0.754,0.775]) → **consonant-suppressing**; the new seats echo the
canonical four more than chance (`echoes_both` 82/69/62%). The combinatorial artifact is FALSIFIED.
Power-atom-driven (power_only ≈ headline), bundle-robust within the sweep envelope; twin model gap
0.012 on the 873 non-grid matched stratum (grid census 87/0). Exchangeability gate PASS (dr_type pure
fn of C). ISSUES.md OQ-131 stays `future` (Q2/Ω_C corpus-adoption deferred); Q1/Ω_E folded in.
**Scope walls:** H⁰/H¹ only (subobject-classifier on a larger site stays OPEN); finding is
seat-bundle-dependent, not "the 6-point cohomology of this corpus."

---

## 2026-06-15 — OQ-108 resolved: per-position witness coverage shipped; OQ-107 closed `future`; new `future` status token
**Files:** prolog/stakeholder_seats.pl, prolog/json_report.pl, python/tensions_ledger.py, python/issues_status.py, ISSUES.md
**Tier:** landed

Witness coverage over the **6-atom authoring power vocabulary** (powerless/moderate/powerful/
organized/institutional/analytical, `docs/logic.md:293`) — distinct from the **4-position
observer fingerprint** (`logical_fingerprint:fingerprint_shift/2`; `powerful`/`organized` have
π and canonical-d but no `standard_context_for_power`, hence no perspective column). New:
`stakeholder_seats:power_witness_count/3` + `power_witness_map/2` (reuse
`constraint_indexing:canonical_d_for_power/2` as the 6-atom enumerator, no forked list);
serialized as `perspective_witness` in `json_report.pl` (64/64 constraints); rendered in the
tensions ledger. A 0 = that perspective is inference-only, NOT measured-absent (Pattern 6: zeros
SHOWN). Witnessed: `geopolitical_settlement_competition` types `powerless=tangled_rope
moderate=snare` but authored `powerless=0 moderate=0` — argued-not-evidenced legs made visible.

Also: tensions ledger now SUPPRESSES the `grid coverage` line when fully absent (only 3/64 live
constraints author a grid; was noise on every block) — grid line prints only when
authored+injected+imputed>0; report `.md` generators deliberately unchanged (their grid lines are
load-bearing CONDITIONAL/OPEN captions + the OQ-98 always-print banner).

New status token **`future`** (operator ruling 2026-06-15): closes a REAL question deliberately
not slated for work but keeps it searchable + full-bodied; NOT in `omega_resolver`'s ACTIVE set,
so it drops out of the workable frontier; carries no resolution witness, so the rotted-witness
check skips it. Added to `issues_status.py` TOKENS + the ISSUES.md footer grammar +
`run_pipeline.py` comment. OQ-107 (survey-wave/external-instrument adapter) closed `future` —
operator does not see it getting done; OQ-108 was `blocked_on OQ-107` but the witness is the
authored stakeholder (no survey wave needed), so that dep was wrong and is dropped.

## 2026-06-14 — corpus omega soundness POC (OQ-130 scale arm): authored omegas 80% sound, NOT §8-class; identity is three orthogonal axes
**Files:** audits/2026-06-14_corpus_omega_soundness_poc/, ISSUES.md, docs/design/design_gaps.md, prolog/testsets_haiku/
**Tier:** correction-key
Ran §C soundness gate as a POC under a two-party independence protocol (sealed adjudicator held-sample
key committed `acc27d22` BEFORE a blind executor subagent ran probes 1–4; read-only over `testsets_haiku`,
no shared `outputs/` written, no `run_pipeline`). Results: (1) soundness 24/30 = 80% (Ω_E 86.7/Ω_C 75/Ω_P
66.7); content-templating LOW ⇒ the corpus artifact is **identity-overstatement, not fabrication** — so
the OQ-130 blocking precondition is discharged (the authored 4,430 are NOT §8-class; OQ-130 scale-arm
build is de-risked). (2) **Identity is three orthogonal axes, MEASURED:** KIND (signature/orbit) ⊥ topic
(`cs_kernel_id`) at ARI≈0 / same-kernel-cluster 7.65%≈chance — and `gauge_orbit`==`fingerprint-shift`
(one KIND organ, not two); frontier (omega question) ⊥ topic (suppression family spans 225–264 kernels).
(3) The unsound class = kernel-contest family = the same family driving the frontier collapse → dedup
organ and soundness gate are coupled. Fold-backs landed: **GAP-11** (frontier-identity organ missing;
embeddings the real instrument, lexical proxy a floor), OQ-130 (ranking gap named; §1b freshness key =
source content-hash not git HEAD). Caveats: probe 3 is a 30-omega sample (bounds, not proves); the one
held-sample disagreement (id 20) is a hybrid `omega_variables.md` itself leaves open. `issues_status
--check` green (129/0). **Citation rule:** cite "80% sound on a 30-omega sample," never "the corpus is
80% sound." **External adjudication (separate instance, `…poc/adjudication_external.md`):** probe-3
independence was within-instance (executor sealed its own held key); the first separate read corrects
id-20 → ≈77% (23/30), the only external look moving the rate *down*. Caveats it adds: probe 1b≡1a
(ONE KIND surface, not "the KIND organs are orthogonal"); the unsound mass = the kernel-contest family
whose noise-vs-legitimate-committer-Ω_P-frontier reading is **CONTESTED/OPEN** (Seat Theorem Cor 2b →
likely sound-but-mistyped, not restatement). Not §8-class; push pre-condition holds.

## 2026-06-14 — omega-resolver pilot validated on ISSUES.md (OQ-130 minted); §8 landed into OQ-129 OPEN-A
**Files:** python/omega_resolver.py, ISSUES.md, audits/2026-06-14_omega_resolver_pilot/, audits/2026-06-14_extraction_blindness_existential_label/
**Tier:** landed

Ran the omega-resolver memo's pilot (read-only catalog views over ISSUES.md prose + one authored
`Deps:` field; no `issues/` migration). New apparatus `python/omega_resolver.py`: loader / authority
control / SCC-condensation frontier view (§D) / checker / planted-fixture selftest (8/8 controls).
- **§8 re-witnessed** (not transcribed): `extraction_blindness` is an existential-labeling artifact —
  live 16/20 (80%) mirror, haiku 258/358 (72.1%), avg 2.73–2.85 types. Landed into OQ-129 OPEN-A.
  `audits/2026-06-14_extraction_blindness_existential_label/` (probe_mirror.pl reproduces).
- **§E verdict** (the only claim in doubt): frontier view vs independent naive cold-reader baseline →
  57 confirm, 7 contradict, 0 standoff; each contradict settled by an external fact (resolved-blocker
  status for OQ-37/41; own Ω_P type for OQ-03/56/58/69/82). Pilot success criterion met.
- **Model gap surfaced + fixed:** active Ω_E entries blocked on operator-spend-go/substrate are a human
  gate that is not an OQ edge → added relator `blocked_on_human <freetext>` (OQ-71/75/119).
- 16 `Deps:` edges authored by hand in ISSUES.md (values from prose, §1e). `issues_status --check`
  intact (129 parsed). OQ-130 minted for the corpus scale arm (gated on a §8-style omega-soundness
  spot-check before any agenda is trusted). `omega_resolver.py` is read-only, NOT a pipeline gate.

## 2026-06-14 — OQ-129: perspectival-gap feeder rewired onto authored stakeholder seats (was reading the retired constraint_classification)
**Files:** prolog/report_generator.pl, prolog/json_report.pl, ISSUES.md, audits/2026-06-14_omega_gap_reconstruction/
**Tier:** tripwire

`omega_from_gap/5` had been silently dead corpus-wide since the 2026-06-05 rebuild — not broken,
**stranded**: its feeder `report_generator:detect_gap_pattern/2` queried
`constraint_indexing:constraint_classification/3`, the pre-rebuild per-power-seat stored-type surface
that the rebuild retired (0 facts on live bar one engine demo). Rewired onto
`narrative_ontology:constraint_stakeholder/7` via the canonical seat path
`stakeholder_seats:dr_type_for_stakeholder/3` (per-`(C,Name)` d — escapes the same-power atom collapse;
chosen over the plan's inline `dr_type/3`, witnessed verdict-equivalent: both → gap=20). Gap = ≥2
distinct non-`unknown` seat types, fail-closed on <2. `omega_from_gap/5` is now **labeling** (computed
into fresh vars then unified, so a pre-bound pattern can't bypass the priority — same leak `dr_type/3`
guards): `extraction_blindness` → `omega_extraction_blindness_<C>` (critical), else
`general_type_mismatch` → `omega_perspectival_<C>`. `json_report.pl` gaps-array guard moved off the dead
`constraint_classification` to `report_generator:gap_coverage/1`.

**Tripwire for a fresh agent:** before touching gap/omega code, know that `detect_gap_pattern/2` reads
**authored stakeholder seats**, NOT `constraint_classification` (which is dead on the live corpus — a
probe over it returns 0 and looks like "no gaps" when it means "no facts"). Live: 20 GAP / 17 no_gap /
20 abstain; pipeline serializes 20 `omega_extraction_blindness_*`; check_stack clean, validation 0 errors.
OPEN-A..D (labeling finer-partition, abstainer deliberate-vs-hole, all-`unknown` seats, dedup) carried on
**OQ-129**. Gap-Ω prevalence inherits the OQ-70 authoring-convention caveat — do not cite gap counts as a
detection result. Witnesses: `audits/2026-06-14_omega_gap_reconstruction/`.

## 2026-06-14 — OQ-50 closed (explainer rebased on dr_type + type_3/type_5 per-context); OQ-74 core ruled reading-relative; OQ-122 fixture-blocker found STALE; OQ-128 minted
**Files:** prolog/report_generator.pl, prolog/drl_core.pl, ISSUES.md, docs/logic_extensions.md, audits/2026-06-14_oq122_fixture_triage/, audits/2026-06-14_oq49_remeasure/coord0_conjunction_positive_control.txt
**Tier:** landed

Closed OQ-50's two follow-ups (engineering, no design ruling): **OPEN-1** —
`forensic_explain_false_mountain/2` now headlines the post-signature `dr_type` ActualType (the
detector's own notion) with the suppression/extractiveness heuristic relabeled a non-headline
METRIC-LEVEL ANNOTATION; fail-closed `dr_type: unbound` guard, `dr_type/3` total over the reached
set (0/44 no-solution; comment forbids calcifying totality). **OPEN-2** — `type_3_snare_as_rope` /
`type_5_piton_as_snare` (`drl_core.pl:622,629`) lead with `standard_context(Context)` + drop the cut
(matching type_1): the unbound-Ctx trap is gone (type_3 live: 1 phantom-context solution → 4
standard-seat solutions; type_5 0→0). Full caller census clears the multiplicity falsifier (all
setof/findall/`\+`); `/3` legacy path single-solution preserved. Regression: contradiction-sig 5/12
identical to baseline, validation_suite 57/0.

**OQ-74 core RULED reading-relative** (operator, Ω_C/Ω_P): coordination_type is a seventh authored
field, the 55% sibling disagreement is signal; guard holds (no promotion into classify_from_metrics).
**OQ-49 hand-up limb MOOT** — the coord=0 clean-laundering subset is positive-controlled empty on both
twins via the *conjunction* probe (synthetic coord=0+asym row returned; coord+asym excluded), witness
in the oq49 audit dir.

**OQ-122 fixture-blocker is STALE (correction):** re-measured on live HEAD, the FSM victim-gate
(`oq122-fsm-victim-gate`, NOT merged) introduces **zero** new test failures — test_agent_beneficiary
baseline 20≡gate 20, test_contradiction_signatures 5≡5 (delta ∅ both). The "36 fixtures" fail from
2026-06-05 corpus drift (0/11 fsm_agent_mountains + maxwell absent), not the gate; gate's live effect
is a clean 2→0 on the vic=0 physics false-positives. The fixture-cost half of the hold rationale no
longer applies; hold now rests on OQ-128 (physics-RED). A 36-row triage is moot until fixtures are
rebuilt. Evidence: `audits/2026-06-14_oq122_fixture_triage/`. **OQ-128 minted** (mid-power-mountain→rope
power-scaling Ω_C, `drl_core.pl:605-613`). OQ-122 stays open; gate held for bundled landing.

---

## 2026-06-14 — OQ-116 split-closed: de-leak lint chokepoint (linter.py SSOT); MMC = non-collapsing seat divergence; SDZ → OQ-127
**Files:** python/linter.py, python/regenerate_stories.py, agent/cohort_zero_regen.py, python/tests/test_deleak_chokepoint.py, audits/2026-06-12_cohort_zero/pilot_witness.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** tripwire

OQ-116 resolution (operator ruling: *the linter is for the operator, not the engine; linting
stories would be orchestrated bias*). Threshold-coupled lint codes (`SCAFFOLD_DANGER_ZONE`,
`LOW_THEATER_RATIO`, `MOUNTAIN_METRIC_CONFLICT`) must never reach the authoring LLM —
de-leak-in-reverse (OQ-74). The set + strip now live in `linter.py` as the single source of
truth (`THRESHOLD_COUPLED_LINT`, `build_author_feedback`); `regenerate_stories.py` imports it
(was a Pattern-2 fork); `cohort_zero_regen.py` routed through it (latent — its feedback is
validate_json errors, not lint). MMC messages reworded: dropped the authoring imperative, framed
as a claim-vs-metric **seat divergence that need not collapse to one true type** (OQ-74 / seat
theorem) — NOT "the engine corrects the claim."

Engine witness (`audits/2026-06-14_oq116_mmc_engine_witness/`): all 9 live-corpus MMC firings run
through the engine — metric seat diverges from the mountain claim 9/9 (snare/rope/tangled_rope/
unknown), FNL fires only 1/9 (Boltzmann-gated). Corrects OQ-116's own premise: "FSM exists for
it" was WRONG (FSM needs ε ≤ 0.25; firings have ε > 0.25); the analog is the metric classifier
(primary) + FNL (secondary). `institutional_trust_erosion_c0` → snare, FNL=no. (Consistent with
the sibling 2026-06-14 entry: linter reads ε from the authored `domain_priors:base_extractiveness`
regex; the engine classifies on `constraint_metric` — different ε sources, which is *why* MMC is a
coarse proxy.) SDZ half (5/7 calibration) refiled as **OQ-127** (open); de-leak membership correct,
calibration is the open operator call.

**Promotion test (applied, two-pass):** a fresh agent could re-add a lint→prompt loop or
re-declare the tuple. But that mistake is now made **loud, not silent** — `test_deleak_chokepoint.py`
has a census tripwire that fails when a new module joins the {builds-prompt ∧ touches-lint} set,
and `design_discipline.md` §4a states the principle. Per the roll-off rule ("loud failures stay
history, not promoted"), this does **not** graduate to an always-loaded CLAUDE.md section — the
guard + §4a are the durable substrate. Kept here as `tripwire` provenance.

## 2026-06-14 — Engine reads ε from constraint_metric, NOT the testset's domain_priors:base_extractiveness (corrupt-test / ε-trace tripwire)
**Files:** prolog/drl_core.pl, prolog/constraint_data.pl, prolog/domain_priors.pl
**Tier:** tripwire

Surfaced while building the twin-comparison negative control (`audits/2026-06-13_twin_comparison/`):
corrupting a testset's `domain_priors:base_extractiveness(C, 0.68)` changed **nothing** in
classification; corrupting `narrative_ontology:constraint_metric(C, extractiveness, 0.68)` flipped
the signature and moved χ. The verified ε path for classification is:
`drl_core:base_extractiveness/2` (drl_core.pl:85) → `constraint_data:base_extractiveness/2`
(constraint_data.pl:11–13) → `config:param(extractiveness_metric_name, N)` →
`narrative_ontology:constraint_metric(C, N, V)` (N = `extractiveness`). The
`domain_priors:base_extractiveness/2` fact authored in a testset is a SEPARATE domain-prior path
the classifier does not read for corpus constraints (`drl_core:base_extractiveness(_,_):-fail` is
the domain_priors default, domain_priors.pl:33). **Silent-mistake guard:** anyone corrupt-testing
or tracing ε who edits `base_extractiveness` will see no effect and wrongly conclude ε is inert —
edit `constraint_metric(_, extractiveness, _)` (the authoritative source). This is the
"base_extractiveness bridge" the memory index references, now witnessed.

## 2026-06-14 — OQ-49 SPLIT-CLOSE: signature-override re-measure on live corpora; FNL collapse witnessed by source-attribution
**Files:** python/audits/oq49_override_remeasure.py, audits/2026-06-14_oq49_remeasure/, ISSUES.md, prolog/signature_detection.pl
**Tier:** landed

Plan `review-oq-49-in-issues-md-twinkly-mochi.md`. OQ-49's (a)/(b) laundering-vs-load-bearing
ruling was un-answerable as posed — the substrate is gone twice over: `testsets_3000` is a dead
corpus (reset 2026-06-05) and the FNL bait driver was deleted (OQ-70, `72ec2cdd`). Re-measured
read-only on the live corpora (`testsets` 57, `testsets_haiku` 960, `testsets_flash` 960) with a
reconstructed probe (the ad-hoc 2026-06-01 probe was never saved); resolved as a SPLIT-CLOSE under
OQ-74's seat frame. No engine/corpus write; no clause removed.

**The collapse witness is structural, not numeric:** every FNL firing on all three corpora tags
source-1 (`constraint_claim(_,mountain)`); zero source-2, zero unaccounted. Kill condition (any FNL
firing tagged neither = a third path) NOT triggered. The 827/1106 pre-reset bait firings are gone
*by construction*; the raw `1661 → ≤8` count drop is size-confounded (3380 vs 960) and is color,
not the witness. FNL override-effective is now 0/6/8 (was 1661). The override layer's dominant
effect on live is `false_ci_rope → tangled_rope` (override-effective 6/56/78, ~10× FNL→TR's 0/6/8),
not FNL. Inert on live: `:867` natural_law→mountain (0 firings) and `:877` FNL-unknown-fill (0).
Residual = the FULL FNL override-effective union (0/6/8 = 14 across both twins; snare→TR 0/4/4 +
scaffold→TR 0/2/4): **14/14 carry coord+asym**, coord=0 arm positive-controlled (fires elsewhere:
haiku 18 no_coord) → the clean-laundering coord=0 subset is EMPTY on live, escalation dissolves to
zero; the 14 are two-seat signal handed to OQ-74. Twin paired diff (generator-convention signal,
analogue-not-witness): 81 shared / 87 haiku-only / 100 flash-only override-effective ids.

**Citation qualifier (correction-key):** do NOT cite OQ-49's `testsets_3000` 1730/1661 numbers as a
live result — that corpus is dead and pre-OQ-70. The live re-measure is `audits/2026-06-14_oq49_remeasure/`.
OQ-49 status is now `resolved`; any witness-not-verdict engine change is OQ-74's gated pass, not OQ-49's.

## 2026-06-13 — Twin cross-model comparison harness + two generation-quality fixes (classify_corpus driver; Fix A axiom-status, Fix B sibling snap)
**Files:** python/run_pipeline.py, python/story_repair.py, agent/generate_kernel_corpus.py, python/audits/twin_comparison.py, audits/2026-06-13_twin_comparison/
**Tier:** landed

Plan `federated-toasting-sedgewick.md` implemented in four commits.

**Fix A (generation-quality, forward-only — does NOT alter the built twins, which
author zero out-of-enum statuses):** `generate_kernel_corpus.py` prompt now offers only
`holdable`/`overridden` (not `foreclosed`, which is engine-derived via
`cs_axiom_foreclosed/2`). `story_repair.py` coerces `contested→holdable`,
`foreclosed→holdable` (NOT `overridden`: that over-claims displacement unless a
`cs_axiom_contradiction` is authored, which repair cannot see — contradictions live in
the scope manifest / separate `_contradictions.pl`, so it takes the plan's safe-fallback
branch). Any OTHER out-of-enum value is COUNTED in `repair_stats` + reported to stderr +
coerced holdable; `process_batch_results` surfaces a nonzero count as an escalation line.

**Fix B:** `snap_sibling_id()` snaps a drifted `cs_reading_relation` sibling_id to
`<kernel>__<declared_sibling>` only on a UNIQUE confident match (exact, then unique
suffix-normalized) against the seed's `sibling_reading_ids`; ambiguous/unmatched stay
as-authored → quarantine (OQ-58), never wrong-snapped. Applied in `process_batch_results`
before `generate_pl` + JSON write.

**B1 — `classify_corpus(corpus_path, output_name, expected_model)` in run_pipeline.py:**
fresh-process driver classifying a NON-default corpus into its own manifest-bearing
output, WITHOUT running the full pipeline (no overwrite of shared outputs/ or tracked
validation_suite.pl) and never touching canonical pipeline_output.json. Single
deterministic corpus_path overlay (`retractall` default + `assertz` one clause).
Refuses on: zero-glob; load-incomplete (corpus_constraint != glob); model-swap (every
loaded story_provenance model prefix-matches expected_model, with #provenance==glob so
non-vacuous — a count CANNOT catch a name-identical haiku↔flash swap); stale raw;
seen!=classified. `expected_model=None` for mixed corpora. `build_manifest` gained a
`testsets_dir` param + stamps `corpus_path` ONLY for non-default corpora (no-arg manifest
byte-identical — witnessed).

**B-result (audits/2026-06-13_twin_comparison/):** haiku vs flash twins (960 each),
classified serially at one commit (8126231), joined over n=960 by twin_comparison.py
(N=1000 permutations, pre-registered H1/H2). **H1 (structural, per-field, no aggregate):
all 7 fields HOLD** — Wilson-95 lo > permute band95. Powerless seat most model-sensitive
(rate 0.397); institutional highest agreement (0.672) but narrowest chance margin.
Recurring signature lean `constructed_high_extraction`(haiku)↔`false_ci_rope`(flash) —
STRUCTURAL coding not detection (OQ-70). **H2 (continuous): the pre-registered drift test
(obs > band95) FAILED for all 5 fields → H2-drift FALSIFIED.** Observed Δ fell BELOW the
band for all 5 (consistent with continuous invariance), but the lower tail was
pre-registered only to be REPORTED, carries no pre-committed falsifier, and may be partly
ENTAILED by H1 (perspective_chi feeds the χ-classification) — so it is EXPLORATORY, needs
its own registered test, NOT a second confirmation. (Earlier draft over-claimed
"invariance fired"; corrected.) Forward work promoted to OQ-123 (powerless-seat
model-sensitivity), OQ-124 (the constructed_high_extraction↔false_ci_rope signature lean),
OQ-125 (the pre-registered H2-independence colocation test) — filed once the concurrent
OQ-122 writer finished, clearing the label-collision risk.

## 2026-06-13 — Branch cleanup: merged oq117-evidence-block into main; landed the China-legitimacy topic-run artifacts; gitignored *.pdf
**Files:** KNOWN_STATE.md, ISSUES.md (merge), .gitignore, prolog/testsets/{demographic_resource_allocation,livelihood_security_reading,performance_legitimacy_contradictions,performance_legitimacy_flat_control,property_sector_overhang,qualitative_development_reading,quantitative_growth_reading,techno_nationalist_reading}.pl, json/ (7 matching), essays/2026-06/captive_on_both_ends_v3.md
**Tier:** landed

`oq117-evidence-block` (8 OQ-117 audit/docs commits, never pushed) had diverged at `f3f347fe`
while `main` did the twin-corpus rebuild. Merged with `--no-ff`; the only conflict was
`KNOWN_STATE.md` (both branches prepended a dated section — resolved by keeping BOTH, the
twin-corpus and the essay-synthesis entries). ISSUES.md auto-merged; `issues_status.py --check`
passed (120 parsed, 0 malformed). Then committed the China-legitimacy c-orchestrator artifacts the
prior branch documented but never committed (8 testsets + 7 json + the v3 essay). Stale local
pipeline edits to `validation_suite.pl` and `cs_reading_relation_quarantine.json` were DISCARDED
(both are pipeline-regenerated, and main's rebuild had moved them on; the local copies were
pre-rebuild). `.gitignore` now excludes `*.pdf` on principle (already-tracked PDFs unaffected;
the 26MB GO-MAD.pdf and the other untracked `agent/analysis/originals/*.md` source articles were
LEFT in the tree, not committed). Branch deleted post-merge.

**NEXT STEP (not done — operator's call to run):** `python3 python/run_pipeline.py` so
`validation_suite.pl` + classifications pick up the 8 new testsets — they were committed
generate-only, so pipeline outputs are stale w.r.t. them until a run.

---

## 2026-06-13 — Two-model TWIN CORPUS: full never-generated rebuild (Haiku, 988) + Gemini Flash twin (971) reconciled into testsets_haiku/ + testsets_flash/ + testsets/ (branch corpus-rebuild-fresh, merged to main)
**Files:** agent/run_no_scope_gemini.py, agent/_pilot_ladder_strip.py, agent/generate_kernel_corpus.py, prolog/testsets_haiku/, prolog/testsets_flash/, prolog/testsets/, json_haiku/, json_flash/, prolog/beta_processed_flash.txt, ISSUES.md (OQ-75), CLAUDE.md (Corpus Loading)
**Tier:** landed

Branched `corpus-rebuild-fresh` off `main`, cherry-picked the five-defect provenance fix
(`2e3e1998`→`dc12bf5a`), and ran the full never-generated reading-seeds pool (1005 readings /
331 kernels — NOT the plan's remembered 304/101; manifest-pool growth, builder byte-identical)
through the fixed Anthropic/Haiku no-scope path in 8 chunks: **988/1005 generated, 17 named
failures**, n_constraints 5→993, ~$27 Haiku batch. Then generated the SAME pool with
**gemini-2.5-flash** via a faithful kernel-aware port (`agent/run_no_scope_gemini.py`: reuses
`build_cached_messages` + `process_batch_results` verbatim through an Anthropic-result-shaped
adapter; only the batch API/provider + destinations differ; `thinking_budget=0`): **971
generated, 34 failures**. Reconciled by filename → `testsets_haiku/` (960) and `testsets_flash/`
(960) are the INTERSECTION (set-equal, 0 mismatch either way — the controlled two-model
comparison set; JSON in `json_haiku/`/`json_flash/`); `testsets/` (44 = 28 Haiku-only + 11
Flash-only + 5 Sonnet baseline) is the standard location reserved for the c-orchestrator essay
corpus. All five provenance/robustness defects held at scale (993/993 then 960/960 provenance
facts, zero "Redefined static procedure"; Flash stamps `gemini-2.5-flash`). One grid-gate firing
all run (`dueling_disappearance_mechanism__contraction_reading`, pilot_04) — regenerated per the
increment-0 ruling, not waived.

Tripwires promoted to CLAUDE.md (Corpus Loading): **overlay `corpus_path` with `asserta` /
`retractall`-first, never plain `assertz`** — appends after config.pl's default and is silently
ignored (witnessed: loaded 44 instead of 960, no error). Residuals (ISSUES OQ-75, not blockers):
17 Haiku + 34 Flash readings to redraw, dominant cause the generation-side `status:'contested'`
enum violation (valid `holdable|overridden|foreclosed`); naming-drift quarantine class
(model mangles sibling-edge targets, all CAUGHT not crashed); run_pipeline's JSON_DIR is hardcoded
to `json/` so a twin-comparison harness must point its json source at the matching mirror.

---

## 2026-06-13 — Essay-synthesis read-site: report scalars over a propaganda-artifact testset are formalization-of-a-reading, not measurement; OQ-102(a)/OQ-103 are RESOLVED, not open
**Files:** outputs/constraint_reports/{demographic_resource_allocation,livelihood_security_reading,performance_legitimacy_flat_control,property_sector_overhang,qualitative_development_reading,quantitative_growth_reading,techno_nationalist_reading}_report.md, essays/2026-06/captive_on_both_ends_v3.md, docs/technical/build_discipline.md (Instrument-richness section), ISSUES.md (OQ-102, OQ-103)
**Tier:** correction-key

Claude-web synthesized `captive_on_both_ends_v3.md` from the seven China-legitimacy reports
generated 2026-06-13 00:12. Its substantive reading is **correct and report-witnessed**: every
report shows `grid authored 0/32 (absent 32)`, `[INDEX VACUOUS] … ZERO per-index checks ran (not a
clean pass)`, structural verdict `OPEN(no_gradient_data)`, and drift series flagged
`authored-as-PROJECTED (guesses, not observations)`. So the confident scalars (ε=0.42, χ=0.575,
purity=0.667, Boltzmann non-compliance, Wasserstein transport, theorems T2–T6) are a formalization
of **one analyst's reading of one translated press conference** — a regime self-presentation
artifact — not measurement of China. Treat them as a well-structured restatement of the
interpretation, never as evidence for it; the rhetoric's *structure* is anatomized, the *mechanism
it describes* is not. Durable rule promoted to `build_discipline.md` → *Instrument richness is gated
on substrate instrumentation* (read-site paragraph).

**Correction (the citation-staleness rung):** Claude-web cited **OQ-102(a)** (basis=projected
provenance) and **OQ-103** (contamination-edge provenance) as *open*. Both are **resolved** —
OQ-102 closed 2026-06-11, OQ-103 resolved 2026-06-12. The very flags the synthesizer relied on to
see the soft spots — the `basis=projected` drift tail and the `Provenance | Salience` edge columns
— **are those two fixes working**, not live gaps. Do not propagate "OQ-103 open / NOT CARRIED" into
substrate. The coupling-thesis check still stands: lean on a contamination edge only when its
`Provenance` column reads `authored` (livelihood↔{qualitative,quantitative,techno} edges are
`explicit | authored | 1.00`); a `corpus-derived` edge is corpus topology, not the story's claim.

**Open editorial next-step (not yet in substrate, the user's call):** the essay's "dominant Western
frame" contrast (beat-separated coverage, demographics-as-crisis, techno-nationalism-as-threat) was
characterized from general knowledge, not from a read of how the March 2026 conference was actually
covered. The whole "what isn't being said" claim rests on that contrast and would need a check
against real recent coverage before it is rigorous rather than gestural.

---

## 2026-06-13 — OQ-109 RESOLVED: replicate spend ran (15 draws, batch), σ/seat prediction FALSIFIED-AS-TESTED (Fisher p=0.649) → discharged to OQ-118 (draw-stability tracks field-construction-type, not the σ/seat line)
**Files:** agent/cohort_replicate_batch.py, python/cohort_stability.py, python/cohort_sigma_seat_eval.py, audits/2026-06-12_cohort_zero/, ISSUES.md (OQ-109 resolved, OQ-118 filed)
**Tier:** landed

Gated spend authorized + executed (batch `msgbatch_01UbfPq13BcHgJKxcsqK549i`, commit `dcfaea97`):
15 draws = 5 contested kernels (qwerty/free_market/total_war/printing_press/zero_as_number) × 3,
sonnet-4-5 @ temp 0.2, seeded from `prolog/kernel_seeds.json` through the FROZEN seed-spec
(title+domain+summary) so SIGMA_SEAT_PREDICTION (`5f2a626c`) applies. Runner reuses the batch
primitives (cache_control prefix, poll_batch) + cohort_zero_regen's source_desc/stamps; draws are
probe artifacts (replicate dir, none join the corpus). New stat instrument: self-contained Fisher
exact in cohort_sigma_seat_eval, validated vs scipy to 6 sig figs (4 cases) BEFORE use.

**σ/seat partition test FAILED its pre-registered falsifier:** 6 stories, 188 (field,story) cells,
47.87% prediction-consistent, **Fisher two-sided p=0.649 = NO SEPARATION**. The noise hypothesis
the prediction named as its own falsifier was NOT rejected. Operator ruling (split): ROBUST =
apparatus-presence mis-bucketing (boltzmann/network/interval 6/6 stable, predicted seat — no naming
confound, firmest) + the scoped null; CONFOUNDED-HELD (two halves, graduations) = cast/σ fields
(exact-match conflates fresh-cast vs renamed-cast → re-test with the already-built graded distance
metric) and verdict-stability (n=6 + temp 0.2 confound → temp-sweep or accept-as-confounded).
META-FINDING (the yield): draw-stability is an artifact of FIELD-CONSTRUCTION-TYPE (free-authored
cast vs schema-mandated/computed), not the σ/seat line — gates the corpus's analysis contract
(which fields a cross-story claim may trust). NOT noise-over-seat (confounded halves can't
adjudicate). Within-vs-between distance separates cleanly (within ~0.37 < between ~0.59;
printing_press d1-d3=0.543 reproduces the signature-identity witness's "one draw escapes").

**OQ-109 → resolved** (migration complete; σ/seat residual DISCHARGED to OQ-118, not answered —
the close note says discharge-to-successor explicitly). **OQ-118 filed** carrying the robust pair as
settled, the two confounded halves with graduation conditions, the escalate-don't-redraw discipline
(a graded re-test is a NEW pre-registered test, not a retrofit of `5f2a626c`), and reading_diff's
cohort-one carry. Process: settled empirical artifacts committed BEFORE escalating interpretation;
the theory ruling was the operator's, not stamped in auto mode.

## 2026-06-13 — OQ-109 Phase C analytical tail CLOSED to partial: population correction (Iran pair → separate cohort, n=7→n=5) + stability/σ-seat instruments wired & witnessed; two named residuals (gated σ/seat spend, cohort-one reading_diff)
**Files:** prolog/testsets/ (n=5 restored), prolog/archives/datasets/iran_essay_2026-06-11/, python/cohort_stability.py, python/cohort_sigma_seat_eval.py, ISSUES.md, audits/2026-06-12_cohort_zero/
**Tier:** landed

Phase C wire-only close (operator spend boundary = gate the replicate draws). Branch
`oq109-phasec-closeout`; WRITEUP `audits/2026-06-12_cohort_zero/WRITEUP.md`.

- **Step 0 population correction (RESOLVED, witnessed):** two untracked Iran-essay stories
  (`proxy_integration_narrative`, `strategic_victory_narrative`) were loading the live corpus at
  **n=7**. Different generation regime than cohort zero (sonnet-4 / temp 1.0 / `seeded_from=none`
  vs `_c0`'s sonnet-4-5 / temp 0.2 / archive-seeded) ⇒ NOT cohort-zero-homogeneous. Iran-count
  fork CLOSED positive-controlled (genuine 2-story essay: `tensions_ledger.md` + grep both return
  exactly two — possibility 2, not an interrupted-run fragment). Archived to
  `prolog/archives/datasets/iran_essay_2026-06-11/` (commit `d26d04a2`, byte-identity proven before
  live removal); corpus restored to clean **n=5** (pipeline manifest `2026-06-13T03:01:15Z`,
  `1f517a0`). NEVER mix into cohort-zero denominators.
- **Step 1 instruments (LANDED, commit `1f517a08`):** `cohort_stability.py` (per-field
  draw-stability + within-vs-between distance; **Pattern-5 absence-split** — agreement-in-absence
  reported separately, never as positive-stable; witnessed on `organization_floor`×3 + `--selftest`
  PASS) and `cohort_sigma_seat_eval.py` (parse-check reproduces the frozen `SIGMA_SEAT_PREDICTION.md`
  buckets with **zero drift**; population gate **REFUSES a verdict below 3 stories × 2 draws**,
  returns NO TEST at n=1 — operator ruling: a degenerate "insufficient power" number would be a
  counterfeit witness).
- **Two named residuals (status `partial`):** (1) σ/seat partition test awaits the GATED replicate
  spend (`agent/cohort_zero_regen.py --replicates <set>`, set chosen against the seat-side
  prediction fields; then re-run both instruments); (2) `reading_diff` re-point is COHORT-ONE —
  `constraint_stakeholder/7` is Unknown procedure on the corpus, so it has no live positive control
  (inert-proving-inert); deferred until a stakeholder-cell story lands. Homogeneity falsifier
  (item 6) threads to cohort two.

---

## 2026-06-12 — design_discipline v1.3: §9 recorded — engine's pipeline seat is discovery not justification; no-verdict-skips-adjudication; benign-constraint bias control independently re-derived
**Files:** docs/design/design_discipline.md, essays/2026-06/marked_to_market.md
**Tier:** landed

New §9 in `design_discipline.md` (v1.2 → v1.3) records a post-essay review comment (external
model on the `marked_to_market.md` run, relayed by operator) as design doctrine: (1) the engine
sits in the context of discovery, where miscalibration is nearly free because nothing is
load-bearing — its contribution is well-formed questions (anomaly seeds, omegas-as-kill-
conditions, theorems-as-lenses), not calibrated scores; (2) the standing condition is that no
verdict skips adjudication (engine→prose direct wiring = design drift); (3) the surviving risk
is systematic bias not random error (review checks facts, not distributions) — the proposed
benign-constraint control independently re-derives the doc's open item (b) false-positive-on-
high-trust probe, upgrading its standing; (4) convergence under component failure is the design
working, with the audit-of-audits lesson (right-verdict-wrong-mechanism is a finding one level
up; recursion terminates only where a stage holds the substrate). Wiring-state claims in §9 are
attributed to the review, not independently witnessed. Also removed a stray
`marked_to_market.md:Zone.Identifier` Windows download artifact from `essays/2026-06/`.

## 2026-06-12 — OQ-78 evidence pass: ε clustering two-layer; bin boundaries EQUAL config thresholds; circularity → OQ-117; THEN probe HALTED pre-spend — epsilon_bin channel DEAD at the generation interface (hypothesis is the live channel)
**Files:** prompts/uke_scope_v2_json.md, prompts/constraint_story_generation_prompt_json.md, prolog/config.pl, agent/story_generator_base.py, agent/generate_kernel_corpus.py, agent/c-orchestrator.py, ISSUES.md
**Tier:** correction-key

- The ε↔claimed_type correlation (kernel_v2_test2 n=60: snare 0.68–0.78, mountain 0.02–0.15,
  bands near-separable; recorded-bin conformance 15/15, "high"→0.68 in 8/13) is AUTHORING
  CONVENTION — never citable as a detection result (OQ-70-analog).
- OQ-78's "NOT a leak" status REVISED: the bin boundaries disclosed at
  `uke_scope_v2_json.md:292` include 0.10 and 0.30, EXACTLY `piton_epsilon_floor` (Rule Z) and
  `tangled_rope_epsilon_floor`; bin-conformant stories pre-satisfy those two gates by
  construction. 0.55 matches nothing — the rope/snare split (0.45/0.46) is NOT transmitted.
  Disclosure reaches the SCOPE bin-assigner only.
- Ruling RATIFIED same day (OQ-78 → partial): three-fate SPLIT — quantization half CLOSED
  working-as-designed (report ε at bin resolution, ~4 levels); idiom half (0.68 point mass, .x8
  rail) OPEN, re-baselined on cohort zero, graduation = bin-withdrawal probe; independence
  circularity → OQ-117, whose decouple-vs-document design call is SEQUENCED AFTER the probe
  (decision logic recorded in OQ-117 ahead of the run).
- Probe greenlit (15/arm), then HALTED PRE-SPEND in pre-flight recon (halt-and-escalate, not
  inline-amended): NO production path feeds epsilon_bin to the authoring model — unified
  backend / gkc kernel path / c-orchestrator inline all pass `Hypothesis type` only; sole bin
  consumers are two streamlit display lines; the prompt's mapping table is
  instruction-without-data. Historical numeric channel = the PRE-de-leak prompt's type-band
  table (config thresholds verbatim), scrubbed at b6c4e113 (2026-06-05) — every post-reset
  story authored with NO numeric ε instruction and NO bin token. Recorded uke_scope blocks
  are MODEL-FABRICATED (no writer in code; free-text tokens; fabricated dates) ⇒ the 15/15
  bin-conformance was self-labeling. Witnesses W1–W3:
  `audits/2026-06-12_oq78_dead_bin_channel/`. epsilon_bin = Pattern-1 dangling wire;
  disposition in OQ-117 (c), default NOT re-wire.
- Fate-2 graduation RE-ROUTED, zero marginal spend: OQ-109 Phase C regen = withheld arm on
  matched seeds by construction (seed spec is title/domain/summary only); kernel_v2_test2
  archive (n=60) = fed arm and comparator (cross-arm is the test; archive shares are labeled
  context: rail 86% n=91 / 77% n=60; exact-0.68 ~30%). Phase C withdraws MORE than hypothesis
  (full-manifest withdrawal): persistence ⇒ idiom a fortiori; collapse ⇒ hypothesis-vs-rest
  unresolved, finer hypothesis-only arm becomes the designed follow-up.
- Free-gate residual (operator amendment, in OQ-117) — SUPERSEDED same day with the mechanism
  correction: on the ε side ALL gates are numeric-instruction-free in the live pipeline; the
  fed side is the CLAIM (hypothesis-echo), uniformly. Re-weighting principle survives
  restated: discount divergence evidence by what the claim side was fed. OQ-117's live
  mechanism = hypothesis-feeding; boundary disclosure (0.10/0.30) reaches generation only
  indirectly (SCOPE co-authors bin + hypothesis; the hypothesis travels).
- Reconciliations: the 60th story is regime_change_structural_break (sole claimed piton,
  ε=0.28); the live cohort-zero corpus already band-breaks (institutional_trust_erosion_c0,
  claim=mountain ε=0.68 — the OQ-116 MOUNTAIN_METRIC_CONFLICT firing) while LANDING on the rail
  — pre-noted in OQ-78 as the probe's "partial" signature appearing unprompted (n=1,
  hypothesis-pointer), so it cannot be read back as confirmation after the run.
- Boundary-ancestry question (config-copying vs logic.md zones) ruled ARCHAEOLOGY — not chased;
  effect identical either way; ambient monitor is organic corpus growth.
- Direction-of-fix: no target-ε disclosure; no tightening bin boundaries toward thresholds.

## 2026-06-12 — OQ-106 RESOLVED: RETIRE ruled and landed — `structural_coercive_intent` top verdict deleted (range-dead, producerless, consumerless); capture-as-design ratified as piton intension with recorded kill condition; GAP-08 revival stays generic
**Files:** prolog/intent_engine.pl, prolog/config.pl, prolog/config_schema.pl, ISSUES.md, docs/design/design_gaps.md, audits/2026-06-12_oq106_retire/
**Tier:** landed

Worktree `oq106-retire` from `f3f1e99f`. Deciding pass added a third death to the audit's
two: the verdict token had NO consumer even if it fired — `report_generator.pl:22` imports
intent_engine `except([classify_interval/3])` and substitutes its own pattern-only
version; only reachable surface was a format line in validation output via test_harness.
"Unwired ≠ worthless" adjudication came out duplicate-except-the-conjunction (each
conjunct has a live constraint-level near-duplicate: κ-track gradient, agent_beneficiary +
FSM agency gate, authored suppression/resistance metrics, has_viable_alternatives).
Operator ruled retire via web-review option (i): capture-as-design is the piton intension
(`constraint_captured/1` carries designed/decayed; origin-intent not type-constitutive);
kill condition recorded in the OQ-106 close — a proxy/intuition split case arms GAP-08
revival; option (ii) (naming piton as standing candidate consumer) explicitly declined to
avoid an OQ-36 build-mid-baseline license misread. Deleted: the 4-condition clause,
`collect_intent_evidence/1`, `refine_confidence/3`, dead helpers, five params+specs
(config bijection check forces pairwise deletion). Preserved: lower verdicts, OQ-93 open()
passthrough, gradient-fact guard (control flow), intent_* tables + the OQ-43 fail-closed
NL gate. Witness (Pattern 3): full suite before/after byte-identical on substantive lines
(5 [INTENT] lines identical); warning-attribution residue positive-controlled as same-code
run-noise (two identical-code runs drift the same way). Rider: GAP-08's stale residual
paragraph (still described the NL gate as pass-open) updated to record the 2026-06-11
fail-closed ruling.

## 2026-06-12 — OQ-105 RESOLVED: operator ruled fork (a) ALONE; alignment rule landed (prompt + fail-closed validate_json gate); live exposure 0 after the cohort-zero swap retired all 11 hosts
**Files:** ISSUES.md, prompts/constraint_story_generation_prompt_json.md, python/generate_constraint_pl.py, audits/2026-06-12_oq105_alignment_gate/
**Tier:** landed

Worktree `oq105-alignment-rule` from `7ca48e0b`. Operator ratified (a)-alone — grid
alignment at generation, no read-side interpolation machinery — with two amendments: the
densification trade NAMED in the entry (unlabeled generation-side value assertion vs (b)'s
labeled interpolation; defense: model-authored-at-generation = same epistemics as any
authored point; the defect was code injecting endpoints post hoc), and a time-bound reopen.
Substrate moved under the ruling: the OQ-109 cohort-zero swap (`7ca48e0b`) retired ALL 11
OQ-105 hosts to kernel_v2_test2 — live misaligned rows re-derived to 0 (all 5 `_c0`
stories author one shared grid; series-less ones carry `suppression_profile(static)`).
The ratified time-bound ("regen the 11 within Phase C or by a named date") was discharged
by that retirement; its successor clock — rule enforced BEFORE cohort-one generation —
is met by this unit. Landed: prompt rule "One time grid per story" (union grid =
first-class authoring requirement: assert each value / thin to a shared sparser grid /
drop the series, never backfill; OQ-46 scalar-static path untouched) + `_grid_alignment_errors`
in `validate_json` (BOTH jsonschema and fallback paths; all generation drivers import it;
cannot fire on absence — <2 authored grids returns clean, the sanctioned case). Witnesses:
W1 synthetic misalignment fires; W2 5/5 live `_c0` JSONs clean (+ full-validate CLEAN
regression on 2); W3 gate over the 60 archived pre-cohort-zero JSONs flags EXACTLY the 11
known hosts, 0 false positives — extension equals the defect set. Scope note recorded:
the row-sweep's "19/23 robust" is robust relative to LINEAR INTERPOLATION ((b)'s payoff
enumeration), not ground truth. Reopen conditions for (b) in the entry (gate defect /
densification cost turns real on cohort one / Backed-blind consumer over a
misaligned-row corpus).

## 2026-06-11 — OQ-105 per-row sweep: PREDICTED bucket discharged — 4/23 misaligned rows timing-distorted, all one snare-floor mechanism; fork ruling still open
**Files:** ISSUES.md, audits/2026-06-11_oq105_row_sweep/
**Tier:** landed

Worktree `oq105-row-sweep` from `37ea069f`. Interpolation counterfactual over ALL 23
grid-misaligned suppression rows (62-file corpus): substituted scalar vs linear interpolation
of the constraint's own series through the same `classify_at_time_with_supp` clause path.
Controls: interp-identity 215/215 authored points exact; same-path re-derivation 0 failures;
enumeration census re-derives exactly the OQ-110 figures (23 rows / 11 constraints).
Default context: 3/23 rows diverge; all 156 product contexts: **4/23 rows** (181/3588 cells,
5.0%) — agenda_conditioning T=10, post_1998_convergence T=13,
technocratic_paradigm_vs_human_primacy T=9, truth_democracy_disinformation T=2 (non-default
contexts only). Every divergent cell is the one predicted mechanism: endpoint scalar ≥ snare
suppression floor (0.60), local series interpolates below → snare dated early
(sub=snare/interp=tangled_rope, no other type pair). 19/23 rows substitution-robust at every
context. Witness-bucket refinement: substantive_employment_reading T=9 (an original
flip-ON-substituted-row witness) is NOT timing-distorted — interpolated 0.62 also clears the
floor; flip-on-substituted-row was a weaker test than the interpolation counterfactual.
OQ-105 stays OPEN: the (a) grid-alignment-at-generation vs (b) labeled-interpolation-at-read
fork is the operator's; the sweep bounds (b)'s live-data payoff to exactly these 4 rows.
## 2026-06-12 — SIGNATURE-IDENTITY WITNESS: the engine types KINDS, not stories — naming-drift triple probed in fingerprint space; identity-by-signature ruled out for the Phase C regen; seeded_from + draw index added to cohort-zero provenance spec
**Files:** audits/2026-06-12_signature_identity_witness/, ISSUES.md, CLAUDE.md, prolog/logical_fingerprint.pl, agent/c-orchestrator.py
**Tier:** correction-key

Question (operator, after two instances proposed name-/inheritance-keyed identity across
the cohort-zero regen): can the math (fingerprint/orbit/Boltzmann) carry story identity
across generation draws, licensing meta-analysis despite LLM variability? Probe: the
kernel_v1 press/Reformation naming-drift triple (3 runs, 3 names, "same" reading) + 3
topic-distinct controls, pairwise `fingerprint_match/4` over all 7 dims
(`audits/2026-06-12_signature_identity_witness/`, raw output pasted). Result: draws 1&3
= 6/7 with IDENTICAL shift pattern; draw 2 = different mechanism class
(mountain/rope/rope/mountain vs tangled_rope/scaffold/scaffold/tangled_rope), sharing
NOTHING positive with its siblings (its 3/7 = agreement-in-absence: voids []=[], zone
negligible, coupling independent); all 9 between-pairs 0/7; BUT control pair
blockchain|neural_interface also 6/7. Verdict, both directions witnessed: same-material
draws can escape their kind; different-material stories can share one (by design —
fingerprints are domain-abstracted isomorphism classes). KIND-level meta-analysis over
draw-stable fields survives generation stochasticity (the apparatus's purpose — and on
this triple the CLASSIFICATION ITSELF, shift, was draw-unstable: type prevalence over
n=1 draws samples generation noise, consistent with OQ-26). STORY-level identity must be
authored forward (`seeded_from` at regen time), never recovered backward by matching —
signature-keying the trust_erosion exclusion list would have lost draw 2. OQ-109 item 4
updated: seeded_from + draw index schema-required for cohort zero; replicate probe
gains within-vs-between pairwise distances; stability table gates CLAIMS not generation.
Caveats: one triple, 3 controls, old-prompt regime (upper bound on drift); the funded
replicate probe is the calibration, this is data point zero. RULING APPENDED same day
(operator, citing docs/seat-theorem-v1.md): a category shift on redraw is the mechanism
working CORRECTLY — verdicts are seat-indexed, a redraw is a new seat, a classification
that could not shift would be contentless (Coupling Theorem); the analysis product is
SHAPE (hypothesis generation), not draw-invariant truth; determinism-as-desideratum is
part of the problem. Mechanical halves stand (no name/signature keying across regen;
seeded_from = provenance plumbing, no identity semantics); the "identity does not
survive" valence is WITHDRAWN — there was no seat-free identity to lose. Stability table
reads as an empirical sigma/seat partition (draw-stable = situation-fixed; draw-unstable
= seat-expressive), not a noise filter. WRITEUP addendum + ISSUES.md OQ-109 + CLAUDE.md
paragraph all amended with the ruling.

## 2026-06-12 — COHORT ZERO LIVE: pilot 7/7 generated, swap executed (live corpus = 5 _c0 stories; pre-cohort set retired to kernel_v2_test2); C-arm first live decisions witnessed; trio falsifier RESOLVED (filters on new regime); OQ-116 filed
**Files:** prolog/testsets/ (corpus swap), json/, prolog/guard_exclusions.pl, prolog/archives/datasets/kernel_v2_test2/, agent/cohort_zero_regen.py, ISSUES.md, audits/2026-06-12_cohort_zero/
**Tier:** landed

Operator rulings executed: pilot-only-for-continuity (5 continuity-critical seeds:
3 ruled stories + scale_ceiling + adjunctification; organization_floor ×3 replicate);
archive = kernel_v2_test2 (RENAMED from pre_cohort_zero_2026-06-12, not copied; manifest
carries both names + schema pin; transient symlink during in-flight rename, removed at
swap). Pilot: 7/7 driver-owned checks PASS; lint-only failures → OQ-116 (scaffold-zone
calibration; MOUNTAIN_METRIC_CONFLICT contradicts independence doctrine); operator ruled
swap-with-findings-recorded. Battery (battery_witnesses.out): trio FILTERS on cohort zero
(1/4 mountain-claims certify) — archive C≡claim-mountain was old-regime artifact;
demographic_skill_mismatch_c0 protected on own evidence; organization_floor_c0 examined
(redraw not NL-certified — ruled-IN = chain decides + we inspect); trust_erosion_c0
excluded AND chain-false (exclusion bite latent), redraw independently authored the
substantive-dissent shape (claim-mountain ε=0.68) from topic+summary alone;
corroborated_zombie none (flag armed); 12 failing JSONs dispositioned
archived-with-reason. Replicate datum: organization_floor ε=0.42 across all 3 draws
(against contaminated OQ-26 expectation, with frozen σ prediction; n=1 story, table
OPEN). Pipeline green at n=5 (manifest 2026-06-12T17:48:34Z). REMAINING OQ-109 TAIL:
reading_diff re-point (inert until then — no authored cells live), stability table
(needs cohort-one draws), σ/seat evaluation (frozen prediction 5f2a626c awaits table),
OQ-109 close-out.

## 2026-06-12 — DETERMINISM-FRONTIER ruling promoted to CLAUDE.md; Phase C removal commit (schema perspectives[]/mandatrophy_resolved OUT, provenance/8 REQUIRED incl. model+sampling); archive-before-removal executed; replicate probe folded into cohort zero
**Files:** CLAUDE.md, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/narrative_ontology.pl, prolog/guard_exclusions.pl, prolog/signature_detection.pl, prolog/stack.pl, agent/example_platform_commission.json, ISSUES.md, prolog/archives/datasets/pre_cohort_zero_2026-06-12/
**Tier:** landed

Operator ruling (via web-session analysis): "it's the LLM" is a hypothesis sitting where a
witness goes — three mechanisms produce same-material-different-results (generation
stochasticity / ensemble refit / fixed-input non-determinism), attributed by stage-hash
diff, never assumption. Record check WITNESSED all three in-repo: OQ-26 (ε
generated-not-invariant; Axiom 2 amended v6.13.1), press/Reformation 3-naming 9-file
triplet (kernel_v1), naming-drift siblings; the 57-story ensemble refit; OQ-112
order-dependency class + byte-identical same-code reruns (single commits only). Promoted
to CLAUDE.md Critical Distinctions: generation NEVER reproduces; committed JSON = the
checked determinism frontier; re-generated stories are NEW DRAWS never re-measurements.
Landed with it: GATE-0 exclusion (witnesses W1-W3, c_gate0_exclusion_witnesses.out);
archive-before-removal (pre_cohort_zero_2026-06-12: 62 pl + 60 json, schema-pinned at
046e0a40; ab_pilot_pair permanent per R4); schema removal + provenance/8 REQUIRED (model +
sampling_params per the ruling); compiler emits story_provenance/8, perspectives emission
retired delete-not-guard (reason left with the corpus); example carries honest
hand-authored provenance; witnesses W1-W5 (c_removal_commit_witnesses.out): example
PASS/compiles/lints clean, no-provenance fires, old-format invalid AS DESIGNED (archived
schema governs the archive). Replicate probe (3-5 stories x 3 draws, field-stability
table) folded into cohort zero — the table defines which fields n=1 meta-analysis may
compare. Remaining Phase C: regen driver + cohort zero (API spend), reading_diff
re-point, re-witness battery (C-arm + named pair + trio re-measure + corroborated_zombie
+ replicate probe), close-out.

## 2026-06-12 — OQ-114 RESOLVED: archive probe under frozen criterion → OUTCOME 3 (mixed) → operator ruled the live 3 SPLIT (2 in / trust_erosion out, kill conditions + fail-closed exclusion + named re-witness); rider: no-beneficiary conjunct WRONG
**Files:** ISSUES.md, audits/2026-06-12_oq114_archive_probe/
**Tier:** landed

Worktree oq114-archive-probe. Probe (criterion frozen at first commit c64f32a6): kernel_v1
41 mountain-claimed → both=32/Uonly=0/Conly=9/neither=0; v6 430 → 411/0/19/0; comparator
controls PASS both; include-semantics fix caught against the denominator before any
reading (archived duplicate facts multiplied bare findall). Structural finding: NL trio
filters NOTHING on archives — C ≡ claim-mountain there (live corpus authors the trio under
the stricter 2026-06-09 rule; archives cannot witness that). All 28 C-only inspected (≤25
per archive): instruments read all mountain-profiled (no snare-floor, ε≤0.18, low theater
except one deliberate piton); disagreements split duplicate-seat artifact (~6/9, ~8/19)
vs substantive distinct-seat dissent (thai_112 powerless-snare class) — BOTH shapes in
BOTH archives → outcome 3. Ruling: organization_floor + demographic_skill_mismatch IN
(first live C-arm decisions, named re-witness at Phase C); institutional_trust_erosion
OUT (substantive dissent × live FCR firing converging fail-open) with kill conditions
both directions and a FAIL-CLOSED per-story exclusion as the Phase C build item
(witnesses owed: excluded + two-sided control). Rider recorded in the entry: option 4's
no-beneficiary conjunct was WRONG, not over-restrictive (unanimous mountains declare
beneficiaries; the signal is FSM routing, not validity). Phase C now proceeds: extension
confirmed-as-amended → regen.

## 2026-06-12 — OQ-109 B4 gauntlet PASS against a pre-compiled expected-divergence manifest; Phase C ordering pinned (OQ-114 first); OQ-115 filed (check_stack divergence attributed pre-Phase-B)
**Files:** ISSUES.md, audits/2026-06-11_oq109_phase_b/B4_EXPECTED_DIVERGENCE_MANIFEST.md
**Tier:** landed

Manifest compiled BEFORE the run (operator: gauntlet = reconciliation against prediction,
not post-hoc explanation; unmanifested divergence blocks). Pipeline green; validation suite
EXCELLENT; plunit 14/14; check_stack = 4 baseline findings + 1 unmanifested →
investigated to attribution: abductive_helpers phantom-module under [stack]
(load-path-dependent, OQ-57 class; pipeline chain healthy via json_report →
diagnostic_summary; present at pre-Phase-B c22ec561, absent from the 2026-06-04 baseline
— OQ-98-era reference) → OQ-115, not Phase-B-attributable, does not block. Rows 1–10 all
reconciled (pipeline diff confined to the two A5 gaps nulls). corroborated_zombie
first-live-exercise flag carries into Phase C. **Phase B is COMPLETE.** Phase C ordering
pinned in the OQ-109 entry: OQ-114 ruling → C-arm extension confirmed → regen (no
dependency forces regen-first; archive probe rides corpus_path overlays).

## 2026-06-12 — OQ-109 B3: empty-table census CLOSED (A1–A6, B1–B3 all discharged); narrative_ontology A3/A4 detectors retired; linter migrated to agent-surface dispatch; gaps key carries coverage bit
**Files:** prolog/narrative_ontology.pl, python/linter.py, prolog/test_harness.pl, prolog/json_report.pl, prolog/report_generator.pl, python/shared/schemas.py, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Census closure table in b3_empty_table_census.md. Retirements (zero-consumer grep
positive-controlled, dead exemption legs, successors named): check_indexical_relativity,
validate_indexical_completeness, detect_omega(mandatrophy), count_unresolved_omegas,
detect_mandatrophy_omega — products live in R5 crosscheck / FSM / T17 / linter
role-coverage; has_mandatrophy_declaration KEPT (exported, R5-grounded clause). Linter:
MISSING_AGENT_SURFACE dispatch mirrors data_validation; perspectival minimums + variance
legacy-gated; ROLE_COVERAGE minimal two-sided policy (uniform-claim exemption carried);
UNRESOLVED_MANDATROPHY satisfied by authored founding_problem_status; Rule 18b validates
stakeholder_d_override when present. Witnesses: B2 example lints 5→0; corpus sweep 92→80
fully decomposed (7 mandatrophy cleared via genealogy, 2 correctly retained, 2 no-data
stories consolidate to accurate MISSING_AGENT_SURFACE). A2 validate_per_index logs
[INDEX VACUOUS] + ran-count (two-sided witness). A5 gaps: null=didn't-look vs
[]=measured-empty; python/shared/schemas.py gaps made nullable (the enrich validator
caught the null LOUDLY first — the chain working); output diff confined to the 2 no-cell
stories. A6 PERSPECTIVAL_GAPS carries ran-witness (137 incl. engine demos — now visible,
was absorbed). Remaining B3: NONE — next is the B4 gauntlet, then Phase C
(gated on B4; C-arm live-service note + OQ-114 ruling govern the guard there).

## 2026-06-12 — OQ-109 B3: R5 zombie consumer LANDED (A7 seam recovered, first consumer of zombie_piton_crosscheck/2); CLAUDE.md mandatrophy note retired per its own condition; presence gates + emission seam landed same day
**Files:** prolog/report_generator.pl, prolog/data_validation.pl, python/generate_constraint_pl.py, CLAUDE.md, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

R5 consumer: Section-7 mandatrophy surface EXTENDED with r5_zombie_crosscheck_line/1
consuming stakeholder_seats:zombie_piton_crosscheck/2 (Phase-A primitive, zero consumers
until now). Pre-registered witness shape held exactly: 6 live firings (4
authored_zombie_uncorroborated + 2 computed_piton_unflagged), one additive line per firing
report inside the existing section, quiet control (scale_ceiling) clean, pipeline JSON
untouched. corroborated_zombie = 0 on the corpus — witnessed ONLY by the overlay control
(dead+world_rearranges onto computed-piton regulatory_measurement_gap); the live diff is
NOT evidence for that bucket. CLAUDE.md mandatrophy tripwire RETIRED (its stated condition
— the R5 rewire landing with witnesses — met); residual: mandatrophy_resolved is STILL a
dangling schema field, Phase C retires it alongside perspectives[] (provenance KNOWN_STATE
2026-06-07 / OQ-83 A7). Same-day earlier units: presence gates
(agent_surface_present/1 dispatch, 5 two-sided controls) + census-B1 emission seam closed
two-sided (compiler gates invariance_check on perspectives presence). Gotchas: report
Section 7 prints only the subject constraint's crosscheck line per report (subject-scoped
like the OQ-99 scenarios); data_validation NOT loaded by [stack].

## 2026-06-12 — SPEC CORRECTION: unanimity bridge disjunction → conditional dispatch; extension change fully reverted (byte-identical witness); OQ-114 exposure window recorded; ensemble-decomposition practice note banked
**Files:** prolog/signature_detection.pl, ISSUES.md, docs/technical/build_discipline.md, audits/2026-06-11_oq109_phase_b/
**Tier:** correction-key

The 790bb009 bridge landed as old ∨ C — but C ⊇ old, so the union IS C's extension: the
3-story protection, the FCR un-fire, and the regulatory_measurement_gap yellow→red were
LIVE on main for the same-day window, pre-answering OQ-114 (operator: spec
under-specification — "ordered so the authored path decides" meant dispatch, was written
disjunction; executable miss here — the 9/62 extension witness was in hand and not read as
"the deferral didn't defer"). Fix: conditional dispatch (authored cells present → old
semantics verbatim; else nl_certification_chain). Witnesses: dispatch extension = old 6
exactly; seam control still passes via C arm; pipeline diff vs PRE-BRIDGE baseline
BYTE-IDENTICAL (b3_unanimity_dispatch_diff.out). OQ-114 carries the exposure-window note
(trace any consumer of pipeline output in the window). Banked as infrastructure:
build_discipline.md → "Extension-touching diffs decompose into direct targets vs ensemble
refit" (3 signature changes refit 57 stories' corpus-relative statistics; determinism
control is the standard companion) — required reading before B4 gauntlet / Phase C regen
diffs.

## 2026-06-12 — OQ-109 B3 unanimity guard RULED+LANDED: option-2 bridge (authored-cells ∨ nl_certification_chain); census A1 seam closed; OQ-113/OQ-114 filed; output-changing (3 targets + ensemble cascade)
**Files:** prolog/signature_detection.pl, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Both named criterion candidates FAILED the pinned gauntlet — computed-seat unanimity splits
mountain/rope on genuine NL profiles (metric path computes rope at mid-power seats on
ultra-low ε); natural_law_signature is unsatisfiable by construction (has_viable_alternatives
never returns false → pure_natural_law unreachable → OQ-113). Escalated; operator ruled
option-4-conditional → witness failed (C∧no-beneficiary retains 1/6) → option-2 bridge:
authored-cells arm first (DIES AT PHASE C, named retirement point in code comment) ∨
nl_certification_chain/1 (claim=mountain + emerges_naturally + NL collapse/resistance,
fail-closed, signature-layer-safe). Extension 9/62 = old 6 + 3 (OQ-114 adjudicates the 3;
all FSM-examinable). Output-changing commit landed alone: institutional_trust_erosion FCR
un-fired (→ coupling_invariant_rope, seats piton→rope); 57 non-target stories moved ONLY in
corpus-relative statistics (maxent/Wasserstein/Arakelov ensemble cascade; determinism
control: same-code rerun byte-identical); named non-target effects: 3 maxent_top_type
piton→rope + regulatory_measurement_gap headline verdict_join yellow→red. Gotchas worth
keeping: ε lives in domain_priors:base_extractiveness/2 (constraint_metric key is
`extractiveness` — a wrong-table bite-check read all-none before correction);
domain_priors:emerges_naturally is static+multifile (with_asserted cannot overlay it — use a
consulted scratch testset). Criterion-worked framing per operator: the pin rejected
everything offered; not grounds to loosen leg (1).

## 2026-06-11 — OQ-109 Phase B1+B2 LANDED: prompt cutover to stakeholder surface; new one-shot example (FNL statistics reset No. 2); schema/compiler perspectives-optionality (guard-not-delete)
**Files:** prompts/constraint_story_generation_prompt_json.md, agent/example_platform_commission.json, agent/story_generator_base.py, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Worktree `oq109-phase-b`. B1: P/T/E/S tuple + Indexed Classifications sections dropped
(1008→872 lines); d-derivation + ε-invariance KEPT trimmed (operator-approved: substrate-
general surface for the OQ-110 derived-d ruling, not four-tuple surface); suppression-
ambiguity omega + cyclical-measurement guidance relocated, not lost; stakeholders +
six-questions+R5 promoted to required. Witness: 11 tuple-vocabulary terms 0 post / >0 pre
(b1_vocab_grep_witness.out). B2: example = app_store_commission pilot (minimum-prevalence
pick 2.483, example_prevalence.out), hand-mutated per EXAMPLE_INHERITED_SIGNATURES.md —
THE FNL-reset discount list; EXAMPLE_PATH repointed off verification_bottleneck.json;
prompt working-example pointer off testsets/antifragility.json (OQ-47 leak source).
**Boundary pin: B2 changed optionality ONLY** — `perspectives` left the schema required
list and the compiler tolerates absence (.get, 3 sites; emission loop intact — existing
corpus compiles byte-identical, witnessed); property, $defs/Perspective, and emission stay
until Phase C Pattern-3 diffs. Known pre-B3 state: linter fires 4 perspective-era rules +
UNRESOLVED_MANDATROPHY on the example (b2_example_validation.out) — must clear at the B3
linter migration. Pre-existing: 12/60 live-paired JSONs fail schema validation in BOTH
pre/post states (b2_schema_failset_diff.out; 2026-06-09 strictening predates them) —
cross-check against the Phase C regen list. Mountain-claimed perspectives-free stories
would emit invariance_check over an empty authored table — B3 seam, noted.

## 2026-06-12 — OQ-103 RESOLVED: contamination-edge provenance made load-bearing + count-based salience floor at the read site
**Files:** ISSUES.md, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_contamination_provenance_salience.py
**Tier:** landed

Scope-corrected the OQ at close: the provenance bit was NOT absent — `constraint_neighbors/3`
tags each edge with `Source` → `json_report.pl` serializes `edge_type` → `enhanced_report.py`
already printed it. `edge_type == explicit` IS the story-authored-vs-corpus-derived bit. Defects
were (1) inert bit (no legend, equal interpretive weight) and (2) no salience floor.

Read-site fix (no engine classification change):
- `json_report.pl` `write_one_neighbor/6` now emits `shared_agent_count` per neighbor (distinct
  agents shared on the link type; null for explicit/inferred_coupling). It threads the subject `C`
  through `write_neighbor_array/4`→`write_neighbor_items/4`. `edge_strength = 0.3 × count`, so the
  count is the recalibration-proof salience input (don't back-derive from a literal 0.3).
- `enhanced_report.py` `build_contamination_network` gains Provenance/Salience columns + legend +
  `_edge_is_authored`/`_edge_is_salient` helpers; "primarily X" ranks salient edges only; explicit
  empty-above-floor sentence. Floor: authored always salient; derived agent edge salient iff
  count≥2; inferred_coupling (zero live coverage) falls back to strength≥0.6.

Witness: pipeline 2026-06-12T04:29:38Z n=62; 82/106 (77%) edges demote to low-salience; both filed
witnesses (reprogramming→digital_colonialism, trust→representation) render `corpus-derived|low`.
Unit test 5/5. Theorized dedup-mislabel checked on the one live overlap pair, NOT witnessed —
`edge_type` reliable. Back-propagation to existing essays declined (operator: fix-then-rebuild).
Synthesis enforcement stays OQ-101 (`tensions_ledger.py` can now consume the new fields). Commit
`ded4969d` (merge `1bb6e535`). No CLAUDE.md promotion: in-place OQ-103 comments + named helpers
are loud enough.

---

## 2026-06-11 — OQ-112 item-4 sentinel trace: verdict SILENT (three mechanisms); absorber-boundary class elevated to item 2; maxent_indexed_run order dependency found
**Files:** ISSUES.md, audits/2026-06-11_oq112_item4_sentinel_trace/, prolog/maxent_classifier.pl, prolog/json_report.pl
**Tier:** landed

Worktree `oq112-item4-trace` from `009c793a`. Driven-goal trace of the post-OQ-44 `unknown`
sentinel into maxent (read-only; probes + raw outputs in the audit dir). Confirmed: the
`; Supp = 0.0` branches (`maxent_classifier.pl:255/:761`) are dead; with profiles present both
LL paths throw `type_error(evaluable, unknown/0)` at `is/2` — loud in isolation — but (W8) the
only two absent-suppression constraints lack `constraint_claim` (drivers run 60/62; firing set
EMPTY on the live corpus), and every production boundary absorbs: `catch(_, true)` at
`json_report.pl:72/:76` + `trajectory_mining.pl:912` (vacuous success over a live throw,
W16); `catch(_, fail)` row drops (`maxent_report.pl:211`, `maxent_diagnostic.pl:395`); and
`maxent_threshold_proximity` absorbs UNCAUGHT via clause-failure-before-arithmetic (W12a — the
sink a catch-grep cannot see). Bonus: `maxent_indexed_run` quiet-fails standalone (hidden order
dependency on `maxent_run`, witnessed v3 vs v3b) — absorbed by the same json_report boundary.
OQ-112 re-ranked: widened absorber-boundary class (catch-true/catch-fail/clause-failure) is now
item 2. Tripwire for probe authors: the dynamic `maxent_profile/4` table is empty until
`maxent_run(Ctx)` runs in-process — sink probes that skip it get success-shaped LL=-10.0
(prior+bool) without touching the metric; witness profile-present before trusting a sink
result. Latent hazard: first claim-bearing story missing `suppression_requirement` silently
voids the whole maxent stage.

## 2026-06-11 — OQ-97 RESOLVED: Pattern-6 census executed (160/227/210 raw lines, 19 classes); 8 candidate classes filed as OQ-112; classification path clean
**Files:** ISSUES.md, audits/2026-06-11_oq97_pattern6_census/
**Tier:** landed

Worktree `oq97-pattern6-census` from `1bfd0b72`. Bounded grep census over 106 top-level
`prolog/*.pl` (denominator witness: subdir-load grep empty with 47-hit positive control on
stack.pl; scoped to STATIC load directives only — WRITEUP §7 residuals). Three shapes, raw
lists saved verbatim; all 7 pinned positive controls fired — and earned their keep: two Shape-C
grep iterations were rejected by the controls (bare-atom missed `pass(no_extraction_data)`;
no-comment-tolerance missed trailing-`%` defaults; Shape A regenerated with the same fix,
149→160). Class-based triage: 19 classes, file-don't-fix, zero engine edits. **No confirmed
candidate on the dr_type path** — drl_core.pl has zero Shape-A hits (the census itself
witnesses OQ-44 commit C's fix), and `signature_detection.pl:818/:905` tangled_rope branches
read as fired-signature override dispatch, not absence-defaults. 8 candidate classes → OQ-112
(top: diagnostic_summary agrees-on-absence probe signals, 13 sites, feeding the OQ-98 verdict
join as absence-of-alert). Census-surfaced interaction: post-OQ-44 `get_raw_suppression`
`unknown` sentinel makes `maxent_classifier.pl:255/:761` `; Supp = 0.0` dead branches and flows
an atom toward Gaussian-LL arithmetic (OQ-112 item 4). Row-26 tripwire strikes mapped by
content (purity_scoring :57→:58; coupling_factor :135, excess_extraction_factor :154);
`drl_fpn.pl:206` and `drl_boltzmann_analysis.pl:302` were NOT tripwired and stay candidates.

## 2026-06-11 — OQ-110 RESOLVED: residual join + pinned counterfactuals; operator ruled D-fork branch b NO-OPEN (derived-d stands); Backed deposit chain discharged
**Files:** ISSUES.md, python/audits/oq110_residual_join.py, audits/2026-06-11_oq110_residual_join/, prolog/temporal_residual.pl, prolog/drl_composition.pl, prolog/json_report.pl
**Tier:** landed

Worktree `oq110-residual-join`. Fresh pipeline at clean HEAD (manifest 2026-06-12T00:59:49Z,
`c22ec561`, n=62) — prior output was dirty-tree `25d6a637`; flip totals identical across runs.
(1.1) Backed end-to-end verified: controls A (fab_adjacent excluded via OQ-105
SuppBacked=false endpoint), B (backed flip present, deltas match JSON), C
(`with_retracted` eps@T2 removes flip, restore returns it) + full-corpus in-process identity
diff over 62 (comparator positive-controlled). The OQ-33 → OQ-46 → OQ-83 → OQ-110
Backed-verification deposit chain TERMINATES here. (1.2) Join: coverage both=11/62,
flips_only=23, stages_only=4, neither=24; 91 backed flips / 20 fab_adjacent; OQ-105
re-derived 23 rows/11 constraints (new host `institutional_trust_erosion`), 0 flips
on/adjacent. Committer moments are named atoms — presence-level join only. (1.3)
Pre-registered pins on all 91 flips × 2: 82 ε-explained / 9 supp-explained (ALL
snare-suppression-floor crossings at the analytical seat) / 0 genuinely unexplained; zero
third-type outcomes; identity-pin + expected-vanish controls pass. Aggregate v1's verdict
line keyed to the wrong bucket was corrected to the pinned ε-unexplained definition (buckets
untouched — implementation fix, not a criterion amendment). (1.4) Package escalated; operator
ruled branch b NO-OPEN with reopen condition: ≥1 backed flip surviving BOTH pins on a future
join. C1/C2 stubs persist; OQ-109 Phase C gate now reads "B complete" alone. Gotcha worth
keeping: `json_report.pl` is a NON-module script — its predicates live in `user`;
`json_report:write_temporal_residual/2` is an unknown-procedure error.

## 2026-06-11 — OQ-99 + OQ-100(a–c) RESOLVED: omega scenarios render authored protocols (subject-bound, fail-loud); report register coherence (qualified confidence labels, rival-P-graded disagreement, self-consistency header)
**Files:** prolog/report_generator.pl, python/enhanced_report.py, python/enrich_pipeline_json.py, agent/orchestrator.py, ISSUES.md
**Tier:** landed

Two output-changing commits (worktree oq99-omega-scenarios): `6b1092c0` (OQ-99),
`e9872538` (OQ-100 a–c). OQ-99: `generate_omega_resolution_scenarios/0→/1` takes the
report subject; `resolve_omega_source/3→/4` resolves omega_source → subject-binding →
fail-loud `unresolved_source` (never `Constraint: unknown`); authored 5-arity
`omega_variable` protocols (251 facts, 60/62 live testsets) now render per omega; catch-all
clause prevents mid-report abort. **Plan-correction worth keeping:** the 5-arity facts do
NOT land in module `user` — testsets declare `constraint_<id>` and the facts live there
(witnessed via wrong-guard first attempt: `current_predicate(user:omega_variable/5)`
failed silently and the generic template kept rendering; the module-keyed lookup also
disambiguates the 7 cross-file OID collisions). The 2 testsets without a module header
(`employment_boundary_contradictions`, `human_dignity_ai_governance_contradictions`)
author zero omega facts of any arity, so the no-5-arity path has no live instance — it
was witnessed by probe (typed template, bound constraint). Witnesses: scale_ceiling
before/after diff (4× unknown → 0); ai_governance gap omega still routes via omega_source;
probes A (unresolved [OPEN]) / B (catch-all on `empirical_v2`) / C (3-arity-only → typed
template); no-omega report byte-identical. OQ-100: labels `Pattern confidence
(categorical):` / `MaxEnt P(claimed):` (×2 sections — inventory sweep caught a 4th bare
label at the convergence section) / `MaxEnt bands (corpus):`; disagreement header graded
by rival P with cuts as `enrich_pipeline_json.py` constants (BAND_DEEP/BAND_MODERATE,
imported by enhanced_report.py; explicit None guard — bare comparison TypeErrors);
witnessed REJECTED at P=0.9969 (ai_governance_accountability), FAVORS RIVAL at P=0.5776
(institutional_trust_erosion), plurality + None via crafted entries (zero live <0.5
cases); `ONTOLOGICAL FRAUD DETECTION` → `DECLARED-TYPE vs OWN-ASSIGNED-METRICS
SELF-CONSISTENCY` (code grep zero outside archives). Legacy `agent/orchestrator.py:635`
regex updated to `MaxEnt P\(claimed\):` (groups unchanged, re.search witnessed). Engine
tests 10/10 + dynamic validation suite clean after each commit. OQ-100(d) subsumed by
OQ-101 ledger (partial-closure note in the OQ). Full-corpus report regeneration deferred
to the next `run_pipeline` (reports are re-derived artifacts). **Close-out residuals
(same day):** the wrong-module premise was swept repo-wide — single finding filed as
OQ-111 (`data_repair.pl` omega bridge guards on `current_module(IntervalID)`, imports 0;
probe-witnessed); the orchestrator regex match site was verified unchanged on a full
regenerated report (first match = convergence section line, before AND after the rename,
same value as `enriched_pipeline.json` entry confidence).

---

## 2026-06-11 — OQ-83 RESOLVED: measurement close-out; snapshot_type determinism guard; v7 §4.5 (A)/(B) census; OQ-109/OQ-110 filed
**Files:** ISSUES.md, prolog/transition_paths.pl, docs/deferential_realism_paper_v7.md, audits/2026-06-11_oq83_close/
**Tier:** landed

Operator-gated close of the stakeholder-layer migration's measurement question
(`audits/2026-06-11_oq83_close/`). **R4 ruled SATISFIED** (n=6 pilot diff = "produced and
preserved"; preservation witness 18 tracked pilot-arm JSONs — the plan's "20" reconciled as a
grep artifact catching 2 `phase_a_pilot_*` demos); corpus-scale census declined-with-reason
(structure pass named as what a re-open buys). **Ω_P transferred**, not answered: observer-axis
Type-B foreclosed (TWO_AXIS), committer C/B → OQ-87. **Classifier-sync item 5 resolved:**
nb_setval mechanism CONFIRMED at clinical T=0; milblogger T=18 graduates CLEAN (OQ-90/OQ-44
moved the piton path since the 2026-06-08 flag); NEW ε-sourcing mismatch
`challenge_as_commons_maintenance` T=5 (grid-misalignment class, no counted flip, unflagged).
Operator ruled determinism-fix-plus-document (counterfeit-witness rationale — a threading fix
would read as sync while the semantic ε-sourcing divergence remains): `snapshot_type/3` now
clears the classify_at_time nb-globals at entry (before/after witnesses + controls pasted;
`run_migration_tests` green; validation suite 0 warnings). The 2026-06-08 census substrate is
`archives/datasets/kernel_v2_test` (the then-live corpus, archived at `00c639da`) — overlay it
to reproduce. v7 §4.5 amended: one (A) data bridge (`influences`, drl_composition.pl:141) vs
≥3 (B) read-only seam diagnostics, all grep-witnessed live. Spin-offs: **OQ-109** (Phase B/C;
CLAUDE.md mandatrophy note retires there) and **OQ-110** (residual join + D-fork; inherits
consumer-side `Backed` verification). Phase-C calculus witnessed: live corpus 62 testsets,
47 with stakeholder facts / 49 with six-questions atoms → regen scope ≈ 13–15 stories.

## 2026-06-11 — Pew-typology review exchange landed: hedging-as-rigor dual, false-summit authoring discipline, OQ-107/OQ-108 filed, OQ-103 escalated
**Files:** docs/technical/build_discipline.md, CLAUDE.md, docs/design/design_discipline.md, ISSUES.md, prolog/testsets/institutional_trust_erosion.pl
**Tier:** landed

Operator review exchange over the Pew political-typology run (source:
`agent/analysis/originals/Pew_2026.5.10_political-typology_topline.txt`; four story files —
`institutional_trust_erosion`, `representation_legitimacy_gap`, `intra_party_fragmentation`,
`generational_value_divergence` — untracked in the main tree at landing time). What landed where:

- **Hedging-as-rigor (the under-confident dual)** → `build_discipline.md` → *Over-confident
  moves on the synthesis side* (new closing block) + a one-sentence tripwire as item (4) in the
  CLAUDE.md synthesis-side paragraph. "Held open" is earned only when no falsifier is
  specifiable; if a kill condition exists, commit and attach it. Trigger fires at generation
  time (drafting a both-readings passage), not at review. Corollaries recorded with it:
  claims-with-falsifiers-per-piece as the draft-time metric; weight reviewers' questions over
  their line edits when triaging. Instance: the "Counter-Reading, Held Open" section, written
  agnostic while the synthesis was available; an external reviewer's question forced the commit.
- **False-summit authoring discipline** → `design_discipline.md` §4: author testsets with the
  honest prior and let the engine fight it; never pre-conform claims to what classifies
  cleanly. Witness: `institutional_trust_erosion.pl:125` authored `constraint_claim(...,
  mountain)`, engine refused (false summit), and the refusal became the parent essay's spine.
  Includes the ontology-as-anomaly-detector point and the two-way essay↔engine loop.
- **OQ-107** (survey-wave witness adapter: instrument items → metrics; extends the OQ-102
  `measurement_basis/2` spine with a `witnessed` bucket; converts drift events from
  self-consistency checks into measurements) and **OQ-108** (per-position witness-coverage
  report; surveys sample powerless/moderate densely, institutional barely — flags which essay
  legs will be inference) filed in ISSUES.md.
- **OQ-103 escalated to load-bearing**: essays now make network claims; the
  trust↔representation `shared_victim` edge is the relocation thesis in graph form
  (`institutional_trust_erosion_report.md:142`), and it is corpus-topology, not story-authored
  (testset grep empty with positive control on `drl_purity_network.pl`).
- **"The mint"** (information regime as constraint — essay-generated hypothesis, first
  deliberate instance of the loop) queued as an OQ-69 ledger item.

## 2026-06-11 — OQ-90 RESOLVED: capture-keyed piton refinement in the FCR branch (piton un-darkened)
**Files:** prolog/signature_detection.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/signature_mapper.pl, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

`piton` was dark corpus-wide: a piton's real distributed extraction trips `appears_as_rope`, a
Boltzmann failure fires FCR before the profile fallback, so every piton was subsumed as
`false_ci_rope`. Built the refinement (audit: `audits/2026-06-11_oq90_piton_refinement/`; commits
`f2368073` substrate, `64448411` output-changing, `fc724ab2` retirement, `3a4e0209` prompt):

- `narrative_ontology.pl`: `uncaptured/1` (POSITIVE-authored `diffuse`, never NAF), `piton_candidate/1`
  (uncaptured ∧ `prohibitive` fixing_cost), `transient_neglect/1` (uncaptured ∧ `cheap`; diagnostic only).
- `signature_detection.pl`: `fcr_evidence/6→/7` capture-disposition field (evidence trail, populated at
  the constructor — does NOT gate); new `resolve_with_perspectival_check/4` clause between the
  dead-coordination piton clause and the generic FCR clause, guarded by `piton_candidate/1` +
  `config:param(piton_refinement_enabled, 1)`. **Invariant: `dr_signature` stays `false_ci_rope`; only
  `dr_type` becomes `piton`.** Retired the `Supp≤0.2` `piton_signature` dispatch + helper (atom-keyed
  handlers left with superseded comments).
- **TRIPWIRE — `piton_refinement_enabled` fires even when `fcr_override_enabled=0`** (separate axis,
  intentional). Dedicated kill-switch; do not fold into `fcr_override_enabled`.
- **TRIPWIRE — read "piton sparse" only WITH the upstream-shadow caveat:** 4 corpus piton_candidates,
  but only 2 reach FCR (the other 2 are CI_Rope-certified upstream — designed shadow, not a bug).
  `transient_neglect` cell is corpus-EMPTY (all live diffuse claims are prohibitive).
- Output delta (`piton_refinement_enabled` 0→1): exactly 2 rows `tangled_rope→piton`
  (`regulatory_measurement_gap`, `institutional_trust_erosion`); leak controls `organization_floor` +
  `reprogramming_safety_toxicity` stay `rope`. The plan pre-registered 1 row on a 48-testset snapshot;
  live corpus is 52 (4 untracked working-tree testsets feed the pipeline) — re-registered to 2 after
  the K=0 diffuse hand-audit was extended to `institutional_trust_erosion`. **Reproducibility flag:** a
  fresh clone at HEAD sees only 48 testsets (the 4 are untracked) → would reproduce a 1-row delta; the
  4 untracked testsets must be committed for the 2-row result to reproduce.
- Superseded-pending (not removed): `drl_core.pl:344,403` theater piton clauses; maxent piton
  `default_profile` (`maxent_classifier.pl:153–155`, theater-keyed, now stale vs the capture
  definition); `python/axiom_reachability.py:171,207` cascade replica models the removed clause.
- Unblocks OQ-37's `validate_edge_cases` resistance-keyed piton-check removal (successor now exists).

## 2026-06-11 — OQ-44 RESOLVED: fail-closed-on-absence ruled (statute for new gates, marker carve-out, common-law for existing); OQ-43 closed; thermal_dissipation_constraint un-certified
**Files:** prolog/signature_detection.pl, prolog/drl_core.pl, python/shared/schemas.py, ISSUES.md
**Tier:** landed

Operator ruling (witnesses: `audits/2026-06-11_oq44_policy_close/`; ruling text: ISSUES.md OQ-44
still-operative block). Grounded in converged practice — five fail-closed conversions, none
reverted — with the instance-counter satisfied as confirmation only. Statute: new/modified gates
fail closed on absence (`unknown`/OPEN on empty; pass carries its witness). Carve-out: absence →
authored provenance only via positive-control inference at authoring/compile time (the
`suppression_profile` precedent), never emptiness-inference at the read site. Existing gates:
common-law per-instance, prioritized by success-shapedness. Dispositions: (1)
`has_viable_alternatives` default `false`→`unknown` (commit `8b5a34b8`, output-changing) —
`thermal_dissipation_constraint` UN-CERTIFIED (natural_law→ambiguous; NL→mountain override
dropped, rope at moderate/institutional, verdict green→red perspectival_incoherence; all 277
diffs single-cause); (2) `get_raw_suppression` 0-default → `unknown` sentinel + `number/1` guard
at `classify_from_metrics` (commit `966d53c8`) — the witness CORRECTED the "never consumed"
pre-derivation: the two non-story `cs_axiom_contradiction` files exported the fabricated 0 and a
`fingerprint_voids` agreement computed on it (both now honest; `shared/schemas.py` suppression
nullable, null = no authored scalar); (3) report-layer 0.0 defaults CONFORMING as-is (print
MISSING). OQ-43 resolved in the same stroke, fifth-instance disposition recorded there.

## 2026-06-12 — First-contact gate C-range corrected: slot-count!=32 removed (partial grids are LEGAL); first misfire had halted the pipeline on an OQ-90 flip target
**Files:** python/grid_first_contact_gate.py, python/grid_audit_ledger.json
**Tier:** landed

The gate's C-range clause carried the BATCH addendum's full-grid mandate ("slot count != 32 =
battery failure") into the standing first-contact gate — but partial grids are operator-CONFIRMED
legal (no fraction threshold; consumer-named-levels decides sufficiency; the coverage read
reports OPEN where insufficient). First live-prompt opt-in story
(`institutional_trust_erosion`, Pew run, 12/32 all-valid points, endpoints correct, no dupes)
was excluded and run_pipeline HALTED — colliding with OQ-90, whose witnessed delta needed the
story. Corrected: C-range = value outside [0,1] OR duplicate slots (the genuinely
schema/compiler-unreachable shapes); C-flat now evaluates the slot-groups PRESENT (>= 2 levels
at a (metric,time); fires only if evaluable groups exist and all span < 0.05); partial grids
pass with a `coverage` field + prompt-compliance NOTE in the ledger (surfaced, never excluded).
Witness 6/6 (`audits/2026-06-12_gate_partial_fix/gate_partial_fix_witness.txt`): misfire story
passes as legal partial; C-range still bites on out-of-range + duplicate; ECHO/FLAT controls
unchanged; NEW control — partial-but-degenerate grid still fires C-flat. Pipeline exit 0 on the
62-corpus, story ledgered `coverage: 12/32`. OQ-90's two-row delta preserved.

## 2026-06-11 — OQ-93 FLIP RULED + EXECUTED: live prompt opt-in grid section; κ gate → first-contact gate; 10 batch stories promoted (corpus 48→58); two latent defects found by promotion
**Files:** prompts/constraint_story_generation_prompt_json.md, prompts/grid_batch_addendum.md, python/grid_first_contact_gate.py, python/grid_audit_ledger.json, python/run_pipeline.py, python/python_test_suite.py, prolog/data_repair.pl, prolog/validation_suite.pl, json/, prolog/testsets/
**Tier:** landed

Operator ruling: flip now; the one-time κ gate becomes FIRST-CONTACT — every grid-authoring
story is audited once (three indicators, per-story fail-closed) before any consumer read,
ledgered in `python/grid_audit_ledger.json` (seeded with the 10 gate-passed batch stories);
C-echo in any new story HALTS run_pipeline and demands the flip be reverted. Gate controls
4/4 (first_contact_gate_witness.txt). Promotion witnesses: exactly the 10 stories carry
authored 32/32 grids in pipeline output (flip_promotion_witness.txt); suite 58/58 green —
48 grid-absent honestly OPEN + 10 real increasing_coercion verdicts on authored data
(flip_promotion_suite.txt) — the first live-corpus grid consumption in the construct's
history. **TRIPWIRE — baselines re-pinned:** every standing 0-diff witness referenced the
pre-promotion substrate (the "143/143 byte-identical" compiler sweep = 143 json files, now
153; the phase-6 suite diff = 48-corpus, now 58); cite those witnesses as
of-their-substrate, re-run before reuse (staleness ladder). Two latent defects found by
first contact and fixed with witnesses:
1. `data_repair:grid_provenance` read measurement/5 with the interval ANONYMOUS —
   56/58 constraints read other stories' grid points as their own the moment ten grids
   coexisted in one KB (build-unit-1 leakage class; single-interval loads had masked it).
   Interval-scoped now; post-fix pipeline shows exactly the 10.
2. `python_test_suite.py`'s unanchored interval regex matched PROSE before facts — three
   phantom test_case IDs ('18' from "interval (18 months)", '0', 'from') ran green against
   scenario-manager-injected anchors while those stories' real intervals never got their
   suite pass (success-shaped miss). Regex anchored to the compiled fact form + fallback;
   59 test_cases all real IDs except the two genuinely interval-less contradiction files.
Spot-check witnesses added at operator flag: phase-6 diff mechanically traced (105/105
before-lines name the retired flag; 105/105 after-lines carry RETIRED wording; 22 ELAPSED =
all 232 lines); FSM number/1 guard two-sided control (sentinel reaches clause, FSM abstains
cleanly, unguarded comparison witnessed throwing).

## 2026-06-11 — OQ-93 grid migration LANDED end-to-end (stages A–D + coverage read + shim retirement); OQ-96/OQ-101/OQ-102 closed with it; intent sub-fork filed as OQ-106
**Files:** schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/coercion_projection.pl, prolog/pattern_analysis.pl, prolog/intent_engine.pl, prolog/report_generator.pl, prolog/signature_detection.pl, prolog/drift_report.pl, prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/scenario_manager.pl, prolog/data_repair.pl, prolog/data_verification.pl, prolog/domain_priors.pl, python/enhanced_report.py, python/run_pipeline.py, python/domain_priors.py, python/shared/schemas.py, python/tensions_ledger.py, agent/c-orchestrator.py, agent/generate_grid_batch.py, prompts/grid_batch_addendum.md
**Tier:** landed

Full audit package: `audits/2026-06-11_oq93_grid_migration/` (preregistration + per-stage
witness scripts/outputs). Worktree branch `oq93-grid-migration`, commits `bc41e8f4..` —
every stage carries its same-commit witness. Landed, in ruled order:
- **Stage A:** optional `coercion_grid` block (GridMetric/GridLevel enums DISJOINT from
  MeasurementMetric; `stakes_inflation` resurrected grid-side only); rider OQ-102(a)
  `basis` (observed|projected) on Measurement + grid points. 16/16 battery;
  143-file additivity sweep 0 deltas.
- **Stage B:** compiler emits sorted `*_grid_NN` measurement/5 facts (source_class
  authored); fail-loud integrity NOT bypassed by --no-validate: t0/tn == interval
  endpoints, time_point ∈ {t0,tn}, duplicate-slot REJECT (the contract licensing the
  once/1 cap in pattern_analysis). 143/143 byte-identical old-vs-new; constructed-
  duplicate control bit on both CLI paths. Rider: `measurement_basis/2` emission +
  `projected` bucket in `measurement_provenance` (meas_prov/5; json_report +
  shared/schemas carry the key).
- **Coverage read:** `system_gradient/4` carries coverage(Present, All); the `[]→0.0`
  fabricated default KILLED — empty reads FAIL → OPEN; `system_gradient_for/4` is the
  consumer-named-levels read; pattern/intent verdicts carry open(...) through (never
  mapped to stable). Two-sided witness: 8/32 one-level grid flips
  increasing_coercion→OPEN while all five probe stories hold exact pinned values; suite
  green with 48/48 [INTENT] OPEN.
- **Stage C:** grid-batch addendum (no worked value table — OQ-70 discipline) assembled
  with the live prompt at call time (no fork); N=10 batch (operator-ruled) generated;
  κ plausibility audit vs the operator-ruled split gate (C-echo zero-tolerance halt;
  C-flat/C-dir ≥2/10 escalate; per-story fail-closed exclusion): PASS 0/10 excluded.
  **Bug rider (the probe pattern repeating):** first audit read open(no_gradient_data)
  on ALL 10 — `time_point_in_interval` enumerated scalar-series times as gradient
  next-points; fixed with a compound(Metric) guard (grid times = grid-measurement
  times); probe stories had masked it (no scalar series).
- **Stage D:** `level_gradient_divergence/2` (rising-structural/falling-individual)
  wired POSITIVELY into FCR (new fcr_test_failure clause) + FSM (fsm_evidence/3,
  one-rung confidence bump; `open` on absence leaves pre-wiring values exactly) + the
  extraction-blindness omega (witnessed-process tail). OQ-94 sort respected (CI_Rope
  benignity gates untouched); `structural_coercive_intent` stays unwired (ruling (a) →
  OQ-106). Fire-on-migration: kappa `[CONDITIONAL: grid authored 16/32]` tag WITNESSED
  FIRING; moderate→yellow cap why-not recorded (0 correction-grade carriers on the
  48-corpus today).
- **Shim retirement (closes OQ-96):** `grid_shim_enabled` + injection/imputation/gate
  arms removed; `domain_registry.pl` regeneration + .gitignore fossil retired;
  domain_priors.py --output repo-relative; source_class buckets KEPT. Before/after
  full-suite diff: 0 unclassified lines (wording of the two retirement messages +
  [ELAPSED] noise only); per-class counts identical (FAIL 0/0, OPEN 513/513, SHIM
  48/48). NOTE: prereg said "0-diff"; actual = justified-wording-diff because the old
  messages named the retired flag — recorded here rather than silently absorbed.
- **OQ-102 closed:** (a) basis chain witnessed end-to-end (fixture → compiler →
  measurement_basis/2 → meas_prov(39,0,0,2,39) → ledger drift line); (b) drift
  severity joins its own confidence at the read site (`[warning | confidence: low]`
  witnessed live on agenda_conditioning) + projected caveat in the report trajectory
  section.
- **OQ-101 closed:** `python/tensions_ledger.py` (non-generative) replaces orchestrator
  step 6 (`_step_essay` REMOVED); 48/48 blocks witnessed on real pipeline output;
  fidelity spot-check vs two regenerated reports clean.

**PENDING OPERATOR (recorded, not self-resolved):** the live-prompt flip to
opt-in-by-story-focus — the N=10 PASS is necessary-not-sufficient by the operator's own
provision (supplemental batch optional); the 10 grid-batch stories sit in
`audits/2026-06-11_oq93_grid_migration/grid_batch/` (json+pl) pending a
promote-to-corpus decision with the flip ruling.

## 2026-06-11 — Backed semantics BUCKETED (follow-on to the OQ-46 close): compiler-stamped suppression_profile(static) sanction marker; OQ-105 filed; OQ-37 piton vacuous-green fixed
**Files:** prolog/drl_composition.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, prolog/data_validation.pl, prolog/testsets/thermal_dissipation_constraint.pl, ISSUES.md
**Tier:** landed

Same-day follow-on ruling to the OQ-46 close (evidence + witnesses:
`audits/2026-06-11_oq46_backed_reconciliation/`; commits `00040bb9`, `b0a0e380`, `609dbb47`).
The close left `Backed=false` on ALL scalar-supplied rows; the operator ruled **bucketed, keyed
on an explicit sanction, never emptiness-inference**: `suppression_profile(C, static)` is
compiler-stamped (`generate_constraint_pl.py` §8) only when the JSON authors other series but
deliberately omits suppression (positive-control absence); `classify_at_time` `SuppBacked` is
three-way — marker-sanctioned static scalar backs / grid-misalignment substitution excluded
(OQ-105) / **unmarked seriesless fails closed**. Decision witness: bucketed = 59 flips / 20
fab_adjacent unchanged (only `backed_times` rises, 7×4 contexts); blanket = 79/0, laundering
substitution-dated transitions into the OQ-83 D-fork flip count. Corpus-wide the scalar IS the
series endpoint (37/39 exact, pre-registered one-time query — 0 violations, so the equivalence-
lint question is closed-no-demonstrated-content) — which makes the misalignment substitution
ANTI-CAUSAL; it currently sets flip timing in 2 witnessed timelines
(`substantive_employment_reading` T=9, `post_1998_convergence` T=13; 1 checked-negative). The 7
seriesless testsets were recompiled from JSON (per-file diff = marker fact + decl only, zero
drift). Pipeline A/B: 30 diffs = 28 backed_times + 2 manifest, nothing else. Also: the
`data_validation` piton check joined over never-authored `resistance_to_change` and printed
"✓ No pitons detected" unconditionally — now prints a VACUOUS notice / joined-table sizes
(OQ-37 row updated; heuristic removal stays gated on OQ-90). Correction to the close-session
evidence: deletion-counterfactual phantom transitions surface via `temporal_residual`, not
`drift_trajectory` (raw series only).

## 2026-06-11 — OQ-46 RESOLVED: the classify_at_time scalar suppression fallback is SANCTIONED (operator ruling), not a retirable stopgap; OQ-46's premise contradicted the live generation prompt
**Files:** prolog/drl_composition.pl, docs/technical/classify_at_time_wiring.md, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

Read-only evidence pass + operator ruling (`audits/2026-06-11_oq46_close/`, branch
`oq46-ruling`). The OQ-46 retirement plan ("once the template authors a temporal
`suppression_requirement` series for every constraint, delete the scalar clause") rested on a
premise the prompt itself contradicts: since 2026-05-30 (commit `220739b8`, pre-reset)
`constraint_story_generation_prompt_json.md:457` instructs "Do NOT author
`suppression_requirement` measurements unless the story's narrative specifically tracks
enforcement-capacity change" — scalar-only is *deliberate authoring* for static-enforcement
stories, so the wait-state never terminates. Witnessed: 7/46 live stories scalar-only, all
prompt-conformant (physics/structural, supp 0.01–0.35, two 2026-06-09 batches incl. 3
regenerated under the required-metrics schema); 21 of 47 fallback rows are time-grid
misalignment inside 10 series-authoring constraints (series universality alone would not retire
the clause); deletion counterfactual flips 16/46 timelines (7 collapse to `[unknown]`, 9 gain
phantom `drift_trajectory` transitions); `snapshot_type`/`degradation_chain` have zero consumers
(positive-controlled grep), so the OQ-41 divergence concern is latent. **Operator ruled: accept
the prompt's design.** The read ladder (temporal at T → scalar-as-constant `Backed=false` →
fail-closed `unknown`) is permanent; no scalar/temporal equivalence check; Surface-3
temporal-suppression work gates on per-snapshot `Backed`, not corpus-wide series coverage.
Comment-only edits to `drl_composition.pl` (STOPGAP → sanctioned); wiring doc §1 re-ruled;
ISSUES.md OQ-46 compressed-on-close with the ruling block kept; cross-refs at OQ-33/OQ-40/OQ-41
updated. Side observation, same session: the two `*_contradictions` testset files are non-story
`cs_axiom_contradiction/2` records — they explain every "48 files / 46 classified" denominator gap.

## 2026-06-11 — Tripwire: the moderate→yellow verdict cap is confirmed-but-never-stressed; re-rule evidence arrives with the first correction-grade signature on a base-GREEN constraint
**Files:** prolog/diagnostic_summary.pl, prolog/signature_detection.pl
**Tier:** tripwire

At the OQ-98 close, severity=moderate for correction-grade signatures was confirmed only in
the sense that it changed nothing: all 13 correction carriers already had base ≥ yellow, so
zero moderate caps have ever shipped. The ruling has not been stressed. When the FIRST
correction-grade signature fires on a base-green constraint (corpus-content event, not
grid-gated), re-run the histogram gate
(`audits/2026-06-11_oq98_verdict_join/histogram_gate.pl`) and surface the transition to the
operator before trusting the new headline — that firing IS the re-rule evidence the
2026-06-11 ruling deferred to. Cross-listed in OQ-93's fire-on-migration witnesses (with the
kappa CONDITIONAL tail, the other dormant OQ-98 path).

## 2026-06-11 — OQ-98 RESOLVED: report headline verdict is now verdict_join (Prolog-side join over alerts + provenance, serialized with raw inputs); schema_version 1→2
**Files:** prolog/diagnostic_summary.pl, prolog/signature_detection.pl, prolog/json_report.pl, prolog/report_generator.pl, python/enhanced_report.py, python/run_pipeline.py, python/shared/schemas.py, ISSUES.md, audits/2026-06-11_oq98_verdict_join/
**Tier:** landed

Commits `e8ab707b` (plumbing, byte-identical pipeline witness) → `170db693` (pre-output
histogram gate) → `ce9a26ec` (output-changing, alone). `diagnostic_summary:verdict_join/3`
joins the base verdict with severity-floored alerts (`drl_core:dr_mismatch/3` + the new
`signature_detection:signature_grade/2`/`signature_severity/2`: correction-grade = override
signature that actually rewired the type, alerts at moderate; commentary never alerts) and
carries grid + measurement provenance (`data_repair:grid_provenance/2`, `source_class/2`).
Serialized in `json_report.pl` as a SIBLING of `diagnostic_verdict` (raw inputs alongside,
never instead); `enhanced_report.py` headlines `verdict_join.verdict`, prints BASE +
per-alert reconciliation when capped, ALWAYS prints the grid line, renders `[UNJOINED]` on
stale artifacts; sidecar verdict = joined. Corpus effect at close: 8/48 headlines changed
(6 green→red, 2 yellow→red, all severe claim-mismatch), zero moderate caps. P1 probe ruled
the grid question: BRANCH A — no diagnostic subsystem is grid-fed (0/48 changed under full
synthetic grids, positive control 46/46 `classify_interval`), so grid-diet lines carry
`[CONDITIONAL]` tags instead of gating the headline; revert to strict fail-closed if a
subsystem ever becomes grid-fed. Tripwire promoted to CLAUDE.md Architecture Invariants:
headline = `verdict_join.verdict`; `diagnostic_verdict.verdict` is a raw input, never a
headline. Witnesses W1–W4 + 2 falsifiers: `audits/2026-06-11_oq98_verdict_join/`.

## 2026-06-10 — OQ-95 resolved: constraint_neighbors/3 now fail-closed on phantom (zero-fact) constraints; giant_comp edges scoped to enumerated nodes; domain_registry throw hit independently (folded into OQ-96 at merge)
**Files:** prolog/drl_purity_network.pl, prolog/giant_component_analysis.pl, prolog/tests/test_phantom_neighbor_filter.pl, prolog/tests/test_forecloses_fpn_injection.pl, ISSUES.md, audits/2026-06-10_oq95_phantom_node_fix/writeup.md
**Tier:** landed

OQ-95's gating census found ALL five `constraint_neighbors/3` consumers (giant_comp, drl_fpn,
network_dynamics, json_report, drl_purity_network's own `bfs_path`/cascade walks) inheriting
phantom endpoints from 26 dangling authored `affects_constraint/2` facts, so the fix landed at
the shared source: `phantom_subject/1` (neither `constraint_claim/2` nor `constraint_metric/3`)
makes `constraint_neighbors/3` **symmetric fail-closed** — phantom endpoints are excluded and a
phantom *subject* returns `[]` (pre-fix the reverse-edge clause made phantoms traversable nodes;
`contamination_path` could route through a constraint that does not exist). Second layer:
`giant_component_analysis:precompute_edges_loop` scopes `assert_edge_canonical` to the enumerated
node set (`ord_memberchk`), making component > node-count impossible by construction.

Witnesses (`audits/2026-06-10_oq95_phantom_node_fix/`): live corpus largest component
118.9% → 56.8% (44→21 of 37); original_v6 259.9% → 89.2% (8,785→3,014 of 3,380); gc edges
75→49 = exactly the 26 dangling facts; post-fix phantom endpoint count 0 with firing positive
control; new 4-test suite `test_phantom_neighbor_filter.pl` (positive control + forward/reverse
exclusion + corpus census); `fpn_injection` 6/6; validation suite 39/39 exit 0; testset-embedded
threshold failures byte-identical before/after (9 pre-existing, unrelated).

**Contract change (the part a fresh agent could trip on):** the claim-OR-metric existence test
is NOT corpus membership — engine demos/probsets still pass — but a synthetic constraint
asserted by a test/probe now needs at least a `constraint_claim/2` to participate in the
network; `test_forecloses_fpn_injection` fixtures were updated for exactly this. Contamination
*values* never needed the fix (the `purity_score/2` `-1.0` sentinel already made phantoms
inert); the defect was purely topological. Generation-time fail-loud (option b) rejected:
dangling refs are an expected, separately-censused property of generated corpora
(`dangle_curve.py` OQ-58, `reading_reference_linter.py`).

Side-finding: hit the `domain_registry:domain_category/2` existence error independently in this
clean worktree — same defect the parallel session diagnosed deeper and fixed as **OQ-96** (module
deleted 2026-02-18; dead clauses removed; suite GREEN without the file). Three residue facts from
the independent path were folded into the OQ-96 entry at merge: the `.gitignore:8` fossil (stale
local copies mask the failure on long-lived checkouts), `run_pipeline.py:268` now regenerates a
file NOTHING consumes (Pattern-1 producer; retire with the shim flag), and
`python/domain_priors.py --output` defaults to an absolute path into the main checkout.
Note on the witness above: "validation suite 39/39 exit 0" was run pre-merge under the
stale-registry-file regime; re-witnessed post-merge under the shim-off regime (see merge commit).
## 2026-06-11 — OQ-33 RESOLVED: row-23 fail-close re-witnessed clean on live + kernel_v1; halt→disposition→control-gated clean re-scan; .gitignore unanchored-outputs tripwire found
**Files:** ISSUES.md, audits/2026-06-11_oq33_close/, prolog/drl_composition.pl, prolog/archives/pre_reset_outputs/, audits/2026-05-30_authoring_closure_fabricated_defaults/tripwire_fabricated_defaults_results.json, .gitignore
**Tier:** tripwire

Evidence pass for closing OQ-33 (plan retargeted from OQ-95). **The fix is sound on current
substrate:** live corpus (48 files/46 classified) 209 constraint×time rows = 162 temporal / 47
scalar-STOPGAP / **0 unknown-floor / 0 residual-0.5 anomalies**; kernel_v1 overlay (1,106 loaded,
path witnessed) 3,497 rows = 2,882/615/**0/0**; D2 `get_raw_suppression` else-branch 0/46. Every
census process ran its own positive controls (unknown-floor + STOPGAP synthetics; same-call-path
control for D2) before its zeros. `Backed=true` 161/162 temporal rows; the 1 false =
`techno_optimist_reading` t=5 (ε fallback, OQ-41 rows 24-25 scope).

**Correction-key (cite-discipline):** the `drl_composition.pl:191-197` comment figures
**471/562/91/0 are NOT kernel_v1 figures** — commit `b5ccee0d` (2026-06-02) measured them on a
562-testset working-tree state that was never archived (226 testsets tracked at that commit;
corpus reached 1,106 by the reset). kernel_v1 measures 934/1106 temporal, 172 scalar-only, 0
unknown. Do not cite 471/562 against any extant corpus; an exact-match expectation must pin the
substrate (corpus + commit), not just the figures.

**Close path:** evidence pass HALTED on the pre-registered Probe D condition — 4 pre-reset
artifacts live in `outputs/` (`pipeline_output.pre_agency_fix.json` manifest 2026-06-03;
`tripwire_fabricated_defaults_results.json`, the 2026-05-30 OQ-33 tripwire evidence cited from
gitignored `outputs/` by its audit; `schema_sieve/{analysis,features}.json` manifests 2026-06-04)
— escalated; operator ruled same day (archive / relocate-to-audit-dir / probe-then-archive /
delete scratch). Executed sha256-verified: archives at `prolog/archives/pre_reset_outputs/`,
tripwire JSON now inside its audit dir (citations fixed), 7 unparseable `scs_out_*.json` deleted.
Re-scan with in-run archive-side positive control (manifest ×3 + tripwire-content ×1 fired on
the relocated artifacts, THEN live scan): 1,055 JSONs, **NO HITS — witnessed-clean**. OQ-33 →
resolved (compressed); OQ-46 annotated with live coverage (the 2026-06-05 "20/20 universal"
template check did NOT hold — 7/46 live constraints are scalar-only); `drl_composition.pl:191`
comment re-stamped three-substrate/as-of-dated (comment-only; post-edit `[stack]` load witnessed).

**TRIPWIRE (RESOLVED same day, history kept) — `.gitignore:2` was an UNANCHORED `outputs/`:**
it silently swallowed ANY nested dir named `outputs` — a disposition commit dropped all four
archive files clean (witnessed; commit succeeded, files absent) until the archive dir was
renamed `pre_reset_outputs`, and `audits/2026-02-25_spectral_laplacian/outputs/` (25 evidence
files) had been gitignored since creation. Operator ruled: anchor, don't relocate. Landed as
commit `09390f0f`: rule anchored to `/outputs/`; pre-anchor survey of every nested outputs dir
(python/outputs empty; `prolog/archives/datasets/original_json/outputs/` 332 files/40M never
tracked → own ignore line, status-quo as a visible decision, track-or-not open); post-anchor
delta = exactly the 25 spectral files, plain `git add` sufficed (anchor-took-effect check).
Residual invariant (citations can dangle by other routes) filed as OQ-104.

## 2026-06-10 — External-review triage (two batches): OQ-98–103 filed; auto-essay synthesis ruled out (ledger replaces it); two topic runs committed under a live-witnessed gate
**Files:** ISSUES.md, audits/2026-06-10_external_review_vote_market/, audits/2026-06-10_external_review_xprize/, KNOWN_STATE.md, prolog/validation_suite.pl, agent/c-orchestrator.py
**Tier:** landed

Two external-review batches triaged against the reports/code/source (external output = hypothesis,
verified before any OQ). **Batch 1 (vote-market six, commit `2d54826c`):** 8 claims → OQ-98
(verdict banner is not a join — GREEN over a 0%-authored grid + alongside `! ALERT [severe]`;
`build_verdict_banner` reads only `diagnostic_verdict`), OQ-99 (omega generator prints
`Constraint: unknown`, `report_generator.pl:572-583`), OQ-100 (register incoherence: 3 "confidence"
meanings, HARD DISAGREEMENT at rival P=0.95, "ONTOLOGICAL FRAUD" overclaim; (d) severable),
+ notes on OQ-44 (resistance_to_change default `0.0` at `report_generator.pl:507`), OQ-93 (W1/purity
are arithmetic over the imputed grid, shim-era). **Batch 2 (XPrize three, commit `96113b05`):**
6 critiques → OQ-101, OQ-102, OQ-103, + OQ-94 cross-ref (who-bears vs who-benefits) + an OQ-98
framing line.

**Load-bearing ruling (operator, 2026-06-10): CUT orchestrator step 6 (the Sonnet auto-essay);
replace with a deterministic, non-generative tensions ledger (OQ-101).** The essay *form* collapses
plurality (the auto-essay announced *"converges on a single structural conclusion"*); `uke_think`
over-stated identically, so the defect is form-not-implementation and prompt guidance can't fix it.
The synthesis-fidelity discipline is NOT an OQ — it lives as a live-synthesis checklist in
`audits/2026-06-10_external_review_xprize/README.md`. Step 6 removal in `c-orchestrator.py` is
pending (OQ-101 build), not done this session.

Run-outputs gate: `run_dynamic_suite` re-run over the full 48-constraint corpus, exit 0 (witness:
`audits/2026-06-10_external_review_vote_market/gate_witness.txt`; positive control — reaches
test_case 48). `validation_suite.pl` auto-regen 39→48 (both runs' constraints) committed in
`2d54826c`. The earlier RED-gate-budget proposal was dropped (premise dissolved when OQ-96 went
GREEN before these commits). `essays/2026-06/who_owns_younger.md` left untracked (operator
finished-essay tree, not engine output). Staged plan: `~/.claude/plans/i-ran-an-article-merry-lagoon.md`.

## 2026-06-10 — OQ-92 RESOLVED: gain_flow receipt surface live end-to-end (schema→compiler→prompt→batch→gates); GAP-10 closed; OQ-90 Steps 2–4 unblocked
**Files:** ISSUES.md, docs/design/design_gaps.md, prompts/constraint_story_generation_prompt_json.md, prolog/narrative_ontology.pl, prolog/drl_core.pl, prolog/maxent_classifier.pl, prolog/signature_detection.pl, prolog/data_repair.pl, prolog/testsets/gfbatch1/, audits/2026-06-10_oq92_step3_preregistration/
**Tier:** landed

Stage C promoted stakeholders[] + six_questions + the receipt surface into the LIVE generation
prompt (additively — four-tuple arrays stay, OQ-83 R4 control arm intact; the live prompt had
carried NO stakeholder guidance, pilot-only). First batch (gfbatch1, 6 stories, run-tagged out
of the corpus glob): 6/6 author gain_flow + fixing_cost, 0 diffuse, referential integrity
clean end-to-end. Diffuse audit at K=0 against the pre-ruled criterion: **0/0 observed —
vacuous pass stated as vacuous**; 6/6 named-capture flagged authoring-convention-until-checked
(matters for OQ-90's piton side: a diffuse-starved corpus leaves piton_candidate unreachable —
check prevalence before reading a piton sweep as absence). Stage D:
`narrative_ontology:constraint_captured/1` (positive computation; absent/diffuse never block)
+ OQ-94 benignity gates rows 1–3 + maxent scaffold spec same-commit; two-sided controls all
landed (uncaptured→scaffold vs captured→rope; captured→pure_scaffold; CI_Rope deterministic
intervention with verified restore). Fabrication-ban grep witness in data_repair.pl. Suite
green; warning gate fired correctly on a deliberate maxent line-drift (allowlist updated
849→852). OQ-92 resolved with the Rulings block kept (operative); GAP-10 closed; OQ-90
Steps 2–4 now pure build on a real surface.

## 2026-06-10 — OQ-96 interim landed (shim OFF, suite green, warning gate wired) + OQ-93 viability probe: gradient cut-bug found and fixed; all pinned values exact post-fix; intent top verdict range-dead witnessed
**Files:** prolog/config.pl, prolog/config_schema.pl, prolog/scenario_manager.pl, prolog/data_repair.pl, prolog/data_verification.pl, prolog/domain_priors.pl, prolog/coercion_projection.pl, python/run_pipeline.py, python/load_warning_gate.py, prolog/load_warning_allowlist.txt, audits/2026-06-10_oq93_grid_viability_probe/
**Tier:** tripwire

**Standing behavior change:** `grid_shim_enabled=false` (config + schema spec) — the DR-AUDIT
grid shim is OFF by default: no injection, no imputation, the 32-point completeness gate
reports OPEN-and-witnessed instead of failing (or being satisfied by manufactured filler).
`[INTENT]` confidence on corpus stories now reads honest `low` (real 0/8), not manufactured
`high`. Set `true` only for archive replays of shim-era behavior. The dead `domain_registry`
references (module deleted 2026-02-18) are REMOVED — both clauses were throw-only for four
months (could never succeed), witnessed crashing the suite at TWO sites (repair imputation via
the Polaris story; `data_validation:127` once repair stopped crashing). Suite GREEN post-change
(0 errors/0 warnings, 47 [OPEN] witnessed-absence lines). **New pipeline gate:**
`python/load_warning_gate.py` + `prolog/load_warning_allowlist.txt` (4 known-benign records)
wired into run_pipeline beside the ISSUES gate — do NOT `grep -v Warning` over load output;
unexpected load warnings now abort the pipeline (negative control witnessed). **Tripwire for
anyone touching coercion_projection/pattern_analysis/intent_engine:** `system_gradient`'s
`[] → 0.0` fallback is a fabricated default — a failed gradient and a flat gradient emit the
same token; the OQ-93 probe witnessed an "(Optimized)" cut in `time_point_in_interval/2` that
made EVERY gradient ever computed fail into that 0.0 (stable-only basin = the cut, not data
starvation; one-char fix landed, corpus regression green). Probe verdict (preregistration
`e7e78a1b`, FINDINGS in the audit dir): post-fix ALL pinned values exact (G_sys ±0.588 etc.,
κ 5/5, all three pattern labels reached, first non-stable intent verdicts in the construct's
history); `structural_coercive_intent` RANGE-DEAD witnessed at the domain edge (max reachable
G_sys 0.98 < threshold 1.00 strict, with full hand-authored Conditions-2–4 evidence —
this probe authored those tables' first-ever facts). **Generalization (operator): the
`[] → 0.0` fallback is the success-shaped-default pattern — the cut was invisible precisely
because failure and "measured zero" were byte-identical at the read site; same channel-level
pathology as `grep -v Warning`, one layer down (suppressed-channel vs collapsed-value).
Ruling (a) recorded: intent top verdict RETIRE-OR-REDESIGN (sub-fork deferred); backward
contamination sweep WAIVED (forward only). Redundancy diff (REDUNDANCY_DIFF.md): zero by
DISJOINTNESS — κ-track's unique product is the level axis; bonus defect:
`coercion_vector`/`compute_completeness` interval-UNSCOPED (completeness=312.5 on loaded
corpus; single-story-safe only). Ruling (b) returns priced — then RULED keep-and-migrate
(named-consumer kind: the masking/naturalization verdict family; intent top verdict stays
retired; imputation killed permanently; sequence + κ-plausibility gate recorded in OQ-93).
Build unit 1 (interval scoping) landed: probe values unchanged exact, leakage healed
(312.5→0), suite green. **once/1 irony (operator flag): the slot-capping fix uses the same
first-solution-only mechanism as the cut bug it buried — sound ONLY under the
identical-by-contract premise, with the contract (duplicate slot authorship rejects loud)
enforced by the stage-2 compiler; once/1 is defense-in-depth, never primary semantics;
constructed-duplicate control queued to the stage-2 battery. Partial-grid threshold question
DISSOLVED on evidence: witnessed 8/32 one-level grid → G_sys=0.216 + increasing_coercion at
completeness 0.25 (findall absorbs missing levels — success-shaped absorption one aggregation
up); design answer = coverage-carrying G_sys + consumer-named-level requirements, confirm at
stage-2 prereg.**

## 2026-06-10 — OQ-94 read-site pass complete: rule sorted 12-file consumer surface; benignity-certification family escalated; prior 7-file census was head-truncated
**Files:** ISSUES.md, audits/2026-06-10_oq94_readsite_pass/READSITE_PASS.md, prolog/drl_core.pl, prolog/maxent_classifier.pl, prolog/signature_detection.pl, python/issues_status.py
**Tier:** correction-key

The OQ-94 per-site decision rule (ruled 2026-06-10) was applied to the full consumer surface.
**Census correction first:** the recorded "seven-consumer list" was `head -15`-truncated — the
untruncated census finds **12 files / 33 sites**, and the concealed ones were the most
load-bearing: `drl_core.pl:346` (scaffold clause) and `:373` (tangled_rope clause) in the
classification cascade itself, plus the `maxent_classifier` boolean_spec mirror and
`omega1_audit`. A probe-scope statement must name its output limits. **Sort result:** SOUND = the
four NL/FSM mountain-likeness gates (beneficiary presence already disqualifies; capture is
stronger evidence, same direction). FORBIDDEN = the tangled_rope cell (`drl_core:373` + maxent),
decay detection (`drift_events`, `transition_paths`), `separability_factor`, and two NAF-voids
(`logical_fingerprint:226,444`) that would FALSE-FIRE on captured constraints under a gate.
**ESCALATED (the one rule-unsorted family): benignity certification** — `drl_core:346` scaffold
clause (+ maxent scaffold spec) and `signature_detection:1019` CI_Rope gate ask "is this benign
coordination?", a third question; gate-on-not-captured there is plausibly correct (it is the
prototype's witnessed scaffold-push mechanism) but is the operator's call. Step-3 preregistration
carries TWO operator questions: diffuse tolerance + benignity-family ruling. Bonus finds:
`constraint_bridge.pl:96` is the first gain_flow-migration candidate;
`data_repair.pl:124-168` FABRICATES `constraint_beneficiary` from metrics on the DR-AUDIT path
(OQ-93 circularity). Estimator-classifier congruence: any `drl_core:346/:373` ruling must land in
maxent's boolean_spec table in the same change. Also this session: `issues_status.py` now fails
on duplicate OQ labels (pre-fix a duplicate entry was silently invisible — witnessed), and the
worktree rule is unconditional (CLAUDE.md). **Step-3 rulings landed (operator, same day): Q2
rows 1+3 GATE (scaffold clause + maxent mirror; pure_coordination subtype), row 2 deferred→
control RUN: synthetic vectors can't reach Boltzmann-gated signatures
(`inconclusive(insufficient_classifications)` — diagnosed), and the live-corpus existence check
witnessed CI_Rope ∧ beneficiary = 7/7 (gate runs entirely on beneficiary-bearers; captured-or-not
unknowable until gain_flow exists). Q1: K=0 on the observable, halt = Stage D only, N =
whole-batch-or-≥30 (convention), obviousness criterion pre-written, "0/N observed" never "clean".
Fabrication ban recorded (gain_flow never synthesized; data_repair.pl the named door). STAGES
A–C UNBLOCKED — schema → compiler → prompt per
`audits/2026-06-10_oq92_step3_preregistration/PREREGISTRATION.md`.** Row 2 then RULED GATE
(family gate-uniform; evidence-shape distinction preserved: row 1 misfire-witnessed, row 2
reachability-witnessed/misfire-pending-Stage-D — deferral would have inverted fail-closed).
**Stage A + Stage B LANDED same day** (schema fields + compiler emission + fail-loud
referential integrity + narrative_ontology declarations; witnesses in the prereg dir: 8/8
schema cases, two-sided additivity, 0-diff 134/134 old-vs-new, pilot branches incl. ghost-seat
REJECTED on both paths, swipl fact queryability). Standing fact with a number: **91/134
`json/` specs fail the CURRENT schema** — identical pre/post Stage A, the expected residue of
the 2026-06-09 required-fields tightening; latent (run_pipeline does not read `json/`; the
generator validates on entry) but a known surprise if old specs are recompiled or used as
fixtures. NEXT HUMAN GATE: the diffuse-audit "obvious capturing seat" criterion is written
BEFORE the first Stage-C batch is read (prereg Q1; operator-in-loop by design); Stage C prompt
work and everything else between is execution.

## 2026-06-10 — OQ-81 ruled SUPPRESS and wired: reading-typed wave-upstreams dropped at seed build; A/B finds verdict import in the gradable channel (theater_ratio), absorbed before the categorical
**Files:** agent/generate_kernel_corpus.py, agent/c-orchestrator.py, agent/story_generator_base.py, ISSUES.md, audits/2026-06-10_oq81_reading_upstream_recon/
**Tier:** landed

Full chain in `audits/2026-06-10_oq81_reading_upstream_recon/` (RECON → AB_PLAN pre-registered →
AB_RESULTS → WIREUP): recon established ZERO exposure to date (no story in any corpus was ever
generated under reading-verdict injection — pre-merge c-orch dropped readings, gkc --scope is
wave-free, post-merge live runs had no reading edges) and that the current SCOPE format emits
kernel-CONCEPT deps (21/21 dangling/inert), not reading deps. The A/B (3 arms × 3 reps, exact
pipeline params, injected verdict deliberately ≠ axis hypothesis): claimed_type held 9/9 snare,
but the three-line verdict block pulled authored theater_ratio 0.690→0.513 (zero range overlap;
kernel-substrate arm ≈ no-context arm). Operator reframe adopted as the closure language:
**verdict import occurred in the gradable channel and was absorbed before the categorical one**
— the categorical field is STICKY (anchored by the explicit hypothesis line), not safe; the R-arm
prose reasoning about theater is the positive control proving the injected verdict was read, so
the categorical null is real. Discovered en route: `axis_source_desc` already injects the
verdict-free kernel CSR into every supplementary-axis prompt — kernel substrate needed no new
wire; the fix-space collapsed to one bit. Wire: `_flat_seeds_from_manifest` drops reading-typed
deps from BOTH the seed's wave deps and the axis copy `upstream_context` reads (two read sites,
one filter point); same predicate in the serial escape hatch (code-read sync, NOT
payload-witnessed). Witness: germline byte-identical (8 flat injections preserved — §5.1 flat
design untouched); dutch+supp kernel capture 4/5 payloads identical, 5th loses exactly the three
verdict lines. Standing cautions (also in the compressed OQ-81 entry): (1) **injection channel
asymmetry** — categorical-stable / continuous-distorted is a general finding about context
injection (n=3, one axis: an instance, not an effect size); (2) the CSR line poisons
vocabulary-based leakage probes in ALL arms — key future leakage probes on tokens present ONLY
via the injected block.

## 2026-06-10 — OQ-77 closed: giant_comp SIGSEGV not serially reproducible (10/10 at exact crash size n=39; archives to n=3380) — concurrency artifact, operational rule promoted; OQ-95 filed (phantom network nodes)
**Files:** ISSUES.md, CLAUDE.md, prolog/giant_component_analysis.pl, prolog/drl_purity_network.pl, python/run_pipeline.py, audits/2026-06-10_oq77_serial_kill_condition/writeup.md
**Tier:** landed

OQ-77's pre-registered kill-condition executed (`audits/2026-06-10_oq77_serial_kill_condition/`):
serial 10/10 rc=0 at n=39 (the exact crash size; outputs byte-identical), 12/12 rc=0 under 12-way
co-residency, and serial archive runs at kernel_v1 n=1106 + original_v6 n=3380 ×3 (byte-identical
complete reports; 8,785-node component BFS). No serial recurrence ⇒ resolved as a concurrency
artifact per the kill-condition; mechanism inside the concurrent regime stays unidentified (pure
co-residency ruled out; mutating prep-interleave unsimulated; exact crashing corpus
unreconstructible). Operational rule promoted to CLAUDE.md Running the System: one pipeline at a
time against shared testsets/+outputs/ (within-pipeline parallelism fine). Reopen path: any
SERIAL segfault → kill-condition's "recurs serially" branch, this audit as baseline.

Side-finding → **OQ-95**: giant_comp's component BFS counts dangling `affects_constraint/2`
targets as nodes — 25 phantom atoms on the live corpus (component = 118.9% of network), ~2.6×
on original_v6 (259.9%). Node enumeration is corpus-scoped; edge discovery
(`drl_purity_network:constraint_neighbors/3`) is not. Probe positive-controlled against the
report's own edge count (75). Census other `constraint_neighbors/affects_constraint` consumers
before picking the fix point.

## 2026-06-10 — OQ-92 rulings recorded + step-2 gain-flow prototype PASSED 8/8: capture and fixing_cost separate on authored fields; step-3 surface build unblocked (OQ-92/OQ-90/GAP-10)
**Files:** ISSUES.md, docs/design/design_gaps.md, audits/2026-06-10_gain_flow_prototype/PREREGISTRATION.md, audits/2026-06-10_gain_flow_prototype/FINDINGS.md
**Tier:** landed

Operator rulings recorded (commit `4e04c2dc`, amendments landed BEFORE the rulings since recorded
rulings become precedent text): **(a)** build the authored gain-flow surface, prototype-first
(OQ-93 precedent); **(b)** ONE authoring surface, TWO distinct fields (gain_flow + fixing_cost),
justified on design grounds — the draft binary-bit argument ("one scalar can't encode two cuts")
was reviewed, found false as an information claim, and recorded as rejected in the OQ-92 Rulings
block to prevent re-citation. Tri-valued provenance design ruled: authored-gain-to-NAMED-seat /
explicit-`diffuse` / absent-fails-closed — with the trap named that NAF over authored fields is
authored-absence in disguise (uncaptured must be authored positively). Malformed-gain
(gain_flow → nonexistent seat) DECIDED to absorb into fail-closed at runtime, with a step-3
schema-rejection validation item so the absorption never hides a data error.

Step-2 prototype (preregistration committed `eb24a927` before the run): eight-control battery,
both fields hand-authored, prototype-only predicates, no production files. **Outcome 1 PASS,
8/8 as pre-registered.** Positive-control pairs held: 2↔7 (diffuse fires on the twin, making
absent's silence a witness) and 1↔8 (the `role_of/3` join fires on an existing seat, making the
malformed silence the absorption witness). Case 5 vs 4 (seat-identical, only `fixing_cost_class`
differs) **witnessed fixing_cost as load-bearing** — OQ-90's decisive pre-wiring control,
discharged. Under-claim holds: cases 1–6 are near-tautological as logic tests; the run witnesses
separation on these constructed cases, the join in both directions, and coherent authorability —
NOT corpus-range representability or generation-side honesty (that is the step-3 diffuse-audit
gate: hand-audit a pre-stated-size sample of generated `diffuse` claims with pre-stated tolerance
BEFORE the field drives classification — authored-diffuse is an authored universal negative with
no checkable witness, and OQ-70 is the template-convention precedent). Post-run promotions
(operator): the prototype's one production-engine touch — capturer seats computing **scaffold**
via `constraint_beneficiary/2` → `has_coordination_function/1` — homed as **OQ-94** (the same
fact-family will make opposite-direction calls once `seat_captures` wires into classification;
wide consumer surface incl. the Boltzmann/FCR coordination axis; collision structural since the
OQ-83 compiler derives constraint_beneficiary from role `beneficiary`); and the diffuse-gate
**tolerance/sample size RESERVED as an operator ruling at step-3 preregistration time**, not a
drafted default. Next forward move: OQ-92 step 3 = schema field + compiler emission + prompt
change per the OQ-83 Phase-A playbook — preregistration must carry both preconditions AND name
OQ-94 as known-interference.

## 2026-06-10 — OQ-57 re-witnessed post-reset: resolution holds; original behavioral witnesses were pre-reset/corpus-specific, now superseded by a corpus-independent positive control
**Files:** prolog/drift_events.pl, ISSUES.md, audits/2026-06-10_oq57_live_rewitness/FINDINGS.md
**Tier:** correction-key

OQ-57 (drift report threw on a missing `requires_active_enforcement/1` qualifier) was resolved
2026-06-04, but **all three behavioral witnesses ran on the corpus reset 2026-06-05** — they
describe constraints that no longer exist. Re-witnessed across live + archives:
- **Code fix durable** (`drift_events.pl:236`, `domain_priors:` qualifier). **Diagnostic positive
  control:** the pre-fix `narrative_ontology:` qualifier still throws `existence_error`, the fixed
  one resolves — the qualifier change is load-bearing, the probe is not vacuously clean.
- **Original emitter set reproduced exactly** on `kernel_v1` (1,106): `{kodashim_obligation__memorial_archival,
  statutory_debt_ceiling__constitutional_nullity_reading}` both fire CLEAN; `kodashim` →
  `evidence(extraction,0.08,theater,0.85)` byte-identical to the 2026-06-04 record.
- **Corpus-independent synthetic positive control** proves the clause fires when its guard is
  reached regardless of corpus content — the witness the original entry lacked.
- Full `drift_event/3` scan threw on **0 of 4,525** constraints across live(39)+kernel_v1(1,106)+
  original_v6(3,380); `run_dynamic_suite` live = 39/0/0.

**Tripwire carried:** the `internalized_piton` clause is currently **UNREACHED on the live
39-constraint corpus** (correct-but-dormant). A future "no drift throw on the live corpus" read
must not be mistaken for "exercised" — it is the Pattern-5 vacuous pass until a low-extraction/
high-theater constraint re-enters the rebuild. Not promoted (corpus-state-specific, self-resolving
as the rebuild grows); recorded so the next reader checks reachability before claiming exercised.

## 2026-06-09 — OQ-93 opened + mitigated: imputation shim diagnosed (unmigrated v3.4 grid contract) and made visible via three-bucket provenance threading
**Files:** prolog/data_repair.pl, prolog/scenario_manager.pl, prolog/test_harness.pl, prolog/intent_engine.pl, prolog/report_generator.pl, ISSUES.md, audits/2026-06-09_imputation_shim_census/census.md
**Tier:** landed

- **Class diagnosed (census: `audits/2026-06-09_imputation_shim_census/`).** The `[FIXED]
  Imputed 24–28 missing vectors` lines in every constraint report are an **unmigrated consumer
  contract**: the DR-AUDIT harness enforces the archived prompt-era 32-point leveled grid
  (incl. `stakes_inflation`, which greps to `prompts/archives/` only — positive control
  `suppression_requirement` fires in live schema+prompt), while the live schema's
  `MeasurementMetric` enum is `{theater_ratio, base_extractiveness, suppression_requirement}`,
  unleveled. **Empty intersection: 0/32 grid points authorable, ever, corpus-wide.** Sibling of
  the `mandatrophy_resolved` severance (OQ-83 A7, same JSON migration).
- **Blast radius:** shim fires only via `scenario_manager:load_and_run` (reports + validation
  suite); main pipeline / `pipeline_output.json` authored-fed. **MaxEnt confidences are
  authored-fed (scalar)** — the "0.95 over invented vectors" caveat was overstated; the
  fabrication-fed products are `[INTENT]` (only `stable` reachable; Confidence `high` derives
  from the imputer's own 8/8 completeness), the verification gate, and κ.
- **Phase 2 landed (visibility-only, witnessed):** `data_repair:grid_provenance/2` +
  three-bucket `[PROVENANCE]` line (authored / injected-0.5 `m_gen` / imputed `repair_m_*` —
  a binary split would launder injection into "authored", operator correction); stray-anchor
  `[WARN]` (injection hardcodes t=[0,10], ignoring the interval); diet flags on
  `[INTENT]`, report header Pattern/Confidence, and κ. Witnesses: report regen diff =
  provenance-lines-only (κ 0.39 and all classifications byte-identical); store-count probe
  matches `prov(0,4,28,0,32)` for transfer_gap_physics; `run_dynamic_suite` 0 errors /
  0 warnings after.
- **Unruled fork (OQ-93):** producer-side vs consumer-side migration completion. Adjudication
  constraint: every grid output ever produced was prior-flavored, so "unique product" is
  unanswerable from existing reports — "wire" requires a prototype with hand-authored grid data
  first.

---

## 2026-06-09 — OQ-80 + OQ-08 closed: generate-step token totals threaded (hard-0 retired); DR/CS Π-asymmetry annotated in both mismatch report layers
**Files:** agent/generate_kernel_corpus.py, agent/c-orchestrator.py, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_token_acc_threading.py
**Tier:** landed

- **OQ-80 resolved.** `process_batch_results` gained an optional `token_acc` mutable out-param
  (None = NOT measured, never 0; return signature intact for gkc CLI callers); usage summed at
  receipt (spend is real even when the story later fails parse/validation);
  `generate_from_manifests` forwards per wave; `_step_generate` now reports real token counts on
  the StepResult instead of the hard 0 + "unthreaded (OQ-80)" note. Witness:
  `python/tests/test_token_acc_threading.py` — summed-at-receipt-incl-parse-failures,
  errored-only→0 negative control, and None-path-unchanged all pass (2026-06-09).
- **OQ-08 resolved.** When `cs_drift_mismatch` fires, `json_report.pl` emits
  `cs_drift_mismatch_note` and `enhanced_report.py`'s kernel-reading section appends the note:
  Π-asymmetric by design — DR instance-blind at the fixed analytical context, CS context-free
  authored facts; cross-frame disagreement, not two answers to one question. Witnessed both
  directions on each layer (Prolog: kernel_test archive, firing UID note+parses / silent UID no
  note; Python: mock-pipeline, note iff mismatch). Eventual permanent home: the OQ-15 mediator.

---

## 2026-06-09 — Three doc-sync OQs closed with witnesses: OQ-07 (mismatch candidate runtime-probed SILENT, blocking conjunct named), OQ-28 (seat-theorem amendment provenance), OQ-14 (bridge unblessed; mediator is the decided join)
**Files:** ISSUES.md, docs/seat-theorem-v1.md, docs/design/two_axis_architecture_v7.md, prolog/cs_drift_mismatch.pl
**Tier:** landed

- **OQ-07 resolved.** `cs_drift_mismatch/2` runtime-probed for the hand-traced UID `72c8aa61…`
  on the only corpus carrying it (`archives/datasets/kernel_test`, 229 testsets; UIDs are
  per-generation surrogates — same-named archive copies differ). Positive control: 11
  corpus-wide firings on the same load. Candidate: SILENT; decomposition shows the
  foreclosure half HOLDS (`axiom_foreclosure_trajectory`) and `cs_is_metric_stable` FAILS —
  runtime falsified exactly the hand-trace's unverified metric-stability assumption.
  Verdict: architecturally-possible-but-not-this-case. Evidence:
  `audits/2026-06-09_oq07_mismatch_runtime_probe/` (probe.pl, probe_output.txt, WRITEUP.md).
- **OQ-28 resolved (option a, as the entry pre-ruled).** `docs/seat-theorem-v1.md` gained an
  "Amendment provenance" section naming the witness-asymmetry: the §3 correction is a
  result-claim carrying its run-witness (`test_forecloses_fpn_injection.pl`); the §5 and §8
  edits are scope-clarifications owing declaration, not run-grounding.
- **OQ-14 resolved.** `docs/design/two_axis_architecture_v7.md` amended (2026-06-09 section):
  the `influences` bridge is no longer the one blessed cross-axis join (16 cross-axis
  surfaces in 7 modules); the OQ-15 mediator layer is the decided-but-unbuilt join; three
  grep-enforceable invariants recorded; four stale claim-sites corrected in place.

---

## 2026-06-09 — Capture-cut discriminating control HALTED (Outcome 2): `has_computed_capturer` proxy false-positives; capture needs an authored gain-flow surface (OQ-92 / GAP-10, gates OQ-90)
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/stakeholder_seats.pl, prolog/constraint_indexing.pl
**Tier:** correction-key

Ran the pre-registered Step-1 control for OQ-90's proposed capture cut (`has_computed_capturer/1` =
beneficiary-side seat with favorable `dr_type_for_stakeholder`) against four seat-sets. **Outcome 2 →
HALT:** the cut fires TRUE on a *mild-favorable non-capturer* (two-part witness: candidate-set
membership TRUE *and* cut TRUE on a seat with no `constraint_beneficiary`) and on an uncaptured
designed DMV's agenda_setter. Root cause: χ (`extractiveness_for_agent_d/4`) is
**extraction-from-seat, not gain-to-seat**, and every beneficiary-side role gets low `d`
(`config.pl:156–160`) → favorable type regardless of receipt; the cut degenerates into "C has a
beneficiary-side-*role* seat at all." Bonus: `constraint_beneficiary/2` (the only authored signal
nearby) feeds `has_coordination_function/1` (`narrative_ontology.pl:303`) → pushes a capturer toward
*scaffold*, the wrong way. **Capture is not computed-representable from current signals;** needs an
authored gain-flow / receipt surface (OQ-92, GAP-10; proposed — for operator ruling — to possibly
unify with OQ-90's `fixing_cost` term, flagged not folded). OQ-90 Steps 2–4 (piton refinement,
`Supp ≤ 0.2` gate retirement) stay gated on OQ-92; the proxy is NOT shipped. Pre-registration written
before the run; favorable-set choice shown irrelevant (problem is upstream in d-derivation).
Witnesses: `audits/2026-06-09_capture_axis_cut_control/` (PREREGISTRATION.md, FINDINGS.md,
step1_capturer_cut_control.out, capturer_cut_control.pl).

**Promotion test:** no silent-mistake tripwire — this corrects how a *prior proposal* may be cited
(the capture cut is rejected, not pending), which is correction-key, not an always-loaded warning;
the χ-is-extraction-from-seat fact is already in the cut's own comment in `stakeholder_seats.pl:86–88`
and now in GAP-10/OQ-92. Stays history-side, not promoted to CLAUDE.md.

---

## 2026-06-10 — Cell controls (witnessed): snare is capture-blind (`Supp ≤ 0.2` is not the piton discriminator); coordination "non-rope" cases scatter (FSM vs FCR) — falsification-grade; rebuild held (OQ-90/OQ-91)
**Files:** prolog/signature_detection.pl, docs/repair_dynamics.md, ISSUES.md
**Tier:** correction-key

Three pre-registered cell controls settled two theory claims **before** they landed (the reason for
running them first: the design-axis 2×2 reasoning had called a table coherent that the witness then
falsified). Witnesses: `audits/2026-06-10_signature_liveness_crosscorpus/{dmv_cell_control.out,desirepath_cell_control.out}`.

- **DMV** (designed, enforced **Supp 0.5**, distributed extraction, **no concentrated capturer**) →
  **snare** (`constructed_high_extraction`). A high-suppression *uncaptured* extraction reads as
  snare ⇒ **`Supp ≤ 0.2` cannot be the piton discriminator** (live `piton_signature` gate +
  prior `repair_dynamics.md` §4 both corrected) and **snare is capture-blind**. Capture and
  suppression are **separable on this witness**; "orthogonal across the range" is the opened
  hypothesis, not established. Pre-registered criterion ("DMV in snare ⇒ axis is capture") met.
- **Coordination side scatters** (pre-registered: FCR ⇒ shared home/not-scatter; not-FCR ⇒ scatter):
  undesigned/emergent coordination (`emerges_naturally` + agent-beneficiaries, low ε) →
  **`false_summit_mountain`**; designed-but-unmaintained coordination → **`false_ci_rope`**. Distinct
  cells ⇒ **"emergent coordination = one cell / piton's mirror" FALSIFIED (scatter)**. Emergent
  coordination → **FSM**, but **home-vs-shadow is OPEN** — whether FSM correctly absorbs it or is the
  lossy place it lands for lack of a proper cell (subsumption vs under-naming, same shape as the piton
  question) is not settled by this probe; it showed the cell non-empty, not that FSM is the right home.

**What is NOT yet established:** "orthogonal across the range" (one witness gives separable, not
independent everywhere). **What is held for operator go (construction, not deletion):** the
capture×coordination rebuild, the per-seat-χ no-capturer detector, the `Supp ≤ 0.2` gate fix, the
environment/perturbation variant. Method note: each control was **pre-registered** before the bash
call so the result couldn't be narrated into agreement — the standing fix for axis-introduction (a
new/relabeled axis owes a pre-registered discriminating control; the DMV is the template; see
build_discipline "false-unification"/memory).

## 2026-06-10 — Piton: agenda_setter is a BETTER proxy (the fixer role), but extraction<fixing_cost stays uncheckable; build as computed false_ci_rope refinement — OPEN pending the fixing_cost control (OQ-90)
**Files:** prolog/signature_detection.pl, prolog/stakeholder_seats.pl, prompts/constraint_story_generation_prompt_json.md
**Tier:** correction-key

Resolves the piton arm of the cross-corpus dark-signature finding (this same day's entry). Two
in-conversation overreaches corrected, both from incomplete recon (the failure the *"witness before
claiming"* / *"unwired ≠ worthless"* disciplines warn about; operator's DMV worked-example + the
agenda_setter pointer were the positive controls):
1. **NOT "operationalization invalid / resistance-sign inverted."** A piton has HIGH resistance
   (people complain) — the gate's `resistance > 0.2` is correct. What's absent is the *fix*. The
   gate (low enforcement + resistance + theater + evolving) is a lossy *symptom-proxy* of the
   cost-asymmetry, not backwards.
2. **NOT "fully representable / no new design" either — the headline overshot (corrected after
   Claude-web push-back).** The fixer exists as **`agenda_setter`** (d=0.12) over distributed
   `payer`s (d=0.85), authored + populated (22/57) — but that encodes only **"the fixer isn't much
   hurt,"** NOT the comparison `extraction < fixing_cost`. The piton condition has two terms; the
   proxy carries one. It is lossy in both directions: **misses** a moderately-hurt fixer for whom
   fixing still isn't worth it (canonical collective-action piton, moderate d), and
   **false-positives** transient neglect (low-d fixer + a cheap fix nobody's done yet — not a piton).
   So `fixing_cost`/benefit-of-fixing is **potentially load-bearing, not deferred**; "representable
   via the stakeholder layer" is **OPEN**, gated on the cheap-fix-not-done positive control (OQ-90),
   not a finding. What actually improved across the thread was proxy quality (theater_ratio →
   stakeholder structure); the mechanism is still not directly checkable.

**Design (operator-ruled 2026-06-10; full spec + drafts in OQ-90):**
- Piton ⊂ `false_ci_rope`, refined **in-branch** (no cascade reorder; piton is FCR-shadowed because
  its low ε trips `appears_as_rope` and FCR fires at priority 2 before the profile fallback).
- Snare implies a capturing beneficiary → keep piton OUT of snare; the split turns on *capture*.
- **The no-capture test is COMPUTED (per-seat χ), never authored beneficiary-absence** — gating on
  "no beneficiary authored" would be a Pattern-5 regression and violate OQ-83 R3 (authored absence
  must not drive classification). Idiomatic here: `in_contention`/`consensus_provenance` are
  computed-not-authored.
- Prompt fix is **non-leaky**: guide authoring of `agenda_setter`/`payer` roles + the cost-asymmetry
  qualitatively; DROP the `theater_ratio ≥ 0.70` recitation (threshold-leakage = tuning-to-target,
  same class as the 0.5 default). Theater becomes an honest-if-present symptom, not the test.

**Tripwire:** when building OQ-90, verify the `chi_for_stakeholder/3` sign convention before writing
`seat_captures/1` (capturer = beneficiary-side seat whose computed χ shows real gain); positive-
control on a constructed DMV seat-set (piton) vs a capturing seat-set (snare) before wiring.

## 2026-06-10 — Cross-corpus signature-liveness sweep: 7/12 signatures LIVE, 5 dark everywhere; the fail-closed fix makes archive sweeps runnable (OQ-89)
**Files:** prolog/signature_detection.pl, prolog/corpus_loader.pl, audits/2026-06-10_signature_liveness_crosscorpus/
**Tier:** correction-key

Corrects the naive read "8 signatures don't fire on the live n=34 ⇒ dead." Ran the current
`signature_detection:constraint_signature/2` across four corpora via `corpus_path` overlay
(retract default → assert `archives/datasets/<x>` → `load_all_testsets`; non-recursive glob =
top-level only). **0 throws on all four** (live 34, kernel_v1 1106, original_v5 702, original_v6
3380; bucket sums equal loaded counts) — the 2026-06-09 fail-closed fix is what makes this safe:
old under-vectored stories abstain to `unknown` instead of throwing. Matrix + provenance:
`audits/2026-06-10_signature_liveness_crosscorpus/MATRIX.md`.

- **7/12 signatures fire somewhere** ⇒ LIVE: false_ci_rope, coupling_invariant_rope,
  constructed_high_extraction, **natural_law** (404 on v6 / 26 on kernel_v1 — zero on live),
  **false_summit_mountain** (kernel_v1+v6 — zero on live), **false_natural_law** (15 on v5 only).
  The three bolded were zero on live → resolved **live-but-narrow**, not dead.
- **5 DARK across all ~5,222 stories:** `coordination_scaffold`, `piton_signature`,
  `constructed_low_extraction`, `constructed_constraint`, `ambiguous`. Strongest cruft-candidates
  but NOT a verdict — per CLAUDE.md *"Unwired ≠ worthless"*, firing-anywhere is evidence feeding the
  value question, not the answer. Next discriminator: the reference-exemplar control
  (`constraint_instances.pl`: SI-units→scaffold, QWERTY→piton) + what each would detect. The three
  constructed_*/ambiguous are intermediate/fallback bands (corpus data lands in constructed_high or
  is overridden) → narrow-data, not proven dead-code.
- **Consistency checks:** `natural_law`=404 on original_v6 reproduces the OQ-43 "404 NL on
  testsets_3000" figure; `false_natural_law`=0 on kernel_v1 (despite OQ-70 recording FNL-dominance
  on its ancestors) corroborates that the OQ-70 bait-clause removal worked.
- **Caveat:** counts are liveness, NOT prevalence — archives are bait-era/ID-reuse and 67–81%
  abstain under current schema.

**Tripwire:** to sweep an archive, overlay `corpus_path` (retract the default `param/2` first — it's
dynamic, first solution wins) to `archives/datasets/<x>` and call `load_all_testsets`; the
non-recursive glob skips run-tag subdirs. Do NOT cite archive firing RATES as corpus content (OQ-70
bait, OQ-25 ID-reuse, schema-drift abstention).

## 2026-06-09 — `accessibility_collapse`/`resistance` now REQUIRED for all constraint types; `get_metric_average` fail-closes to `unknown` (was 0.5); 3 articles regenerated (OQ-89)
**Files:** prolog/signature_detection.pl, schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md, python/generate_constraint_pl.py, agent/c-orchestrator.py
**Tier:** landed

Root cause (audit `audits/2026-06-08_coordination_washing_clean_pass/`): generation never authored
`accessibility_collapse`/`resistance` for non-mountain constraints; `get_metric_average/3` defaulted
the missing vectors to **0.5**, which exceeds `snare_epsilon_floor` (0.46) — so an extraction-less
constraint fabricated `constructed_high_extraction` from no data, and the 0.5 fill was load-bearing
for the throw the audit removed.

**Landed (witnessed; evidence under the audit's `rebuild_evidence/`):**
- **Schema** (`constraint_story_schema.json`): `accessibility_collapse` + `resistance` added to
  `base_properties.required`; rejects each independently (V1 witnessed). `_basic_validate` fallback
  in `generate_constraint_pl.py` made consistent (else jsonschema-absent path silently skips them).
- **Prompt**: both promoted to Core-required-for-ALL-types with honest non-mountain guidance
  (mountains high collapse/low resistance; snares lower collapse/higher resistance). `emerges_naturally`
  stays mountain-specific.
- **Engine** (`signature_detection.pl`): `get_metric_average` empty branch `0.5` → `unknown`; added
  abstain clause `constraint_signature(C, unknown) :- \+ profile_metrics_authored(C), !`; `number/1`
  guards on `natural_law_signature`/`coordination_scaffold_signature`/`piton_signature`/
  `constructed_constraint_signature` + a `profile_numeric` gate on `signature_confidence` so absence
  **fails-closed (abstains), never throws**. Witness: 0 throws across the corpus + probes; the
  fully-vectored constraints classifiable pre-guard are byte-identical post-guard (anti-over-abstain
  control); under-authored constructed_high → `unknown`.
- **Regenerated** magnifica_humanitas, china_blue_collar, world_model3 via c-orchestrator
  (`DR_TEMPERATURE=0`, `--skip-search` — web search hung ~3.5min on the API in-env; research grounding
  doesn't affect metric authoring). All 16 regenerated *stories* author both metrics. **V5 deterministic
  substitution (`probe_harness:with_overlay/3`, caches auto-cleared): B(swap metrics→0.5)==C for all
  16** → the formerly-defaulted metrics do not move these (extraction/suppression-driven) verdicts;
  fix value is structural, not a verdict change.

**Tripwire / residuals (OQ-89):**
- **Full re-run RE-DECOMPOSES into different axes** — not "same stories +2 metrics." world3 went
  3→4 axes with only `proxy` overlapping; magnifica 11→6; china →5. Old testsets are **orphaned**,
  left in place (operator ruling 2026-06-09). 9 corpus members now abstain to `unknown`: 2 are
  `*_contradictions` axiom meta-files (not stories — correct), 7 are orphaned originals
  (e.g. `war_normalization_ai_weapons` superseded by `war_normalization_autonomous_weapons`). Corpus
  n=34 carries orphan+replacement duplicate coverage until a cleanup pass.
- **Legacy corpus not retro-fixed:** ~94/116 historical `json/` files still lack the two metrics;
  the schema requirement binds future generation only.
- **Class generalization deferred** (narrow-scope ruling): the neutral-default-crosses-threshold
  pathology (0.5 > floor) may recur for other `get_metric_average`-style defaults / metric-threshold
  pairs — see OQ-89, cross-ref OQ-43/44.

## 2026-06-08 — Flat router stably under-routes a COUPLED methodological kernel (World3); false-mountain (mountain→rope) is a candidate missed-kernel signal (OQ-88)
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, ISSUES.md
**Tier:** correction-key

First end-to-end `c-orchestrator.py` runs (kernel-first branch) audited against prior corpora and
essays. Routing discrimination works as designed: china wage-convergence + World3 → FLAT (no
`cs_structure`); magnifica AI encyclical → KERNEL (5 readings with `cs_structure.reading_relations` +
axioms; the seat layer reaches the essay — "Four Irreconcilable Frameworks", axiom contradictions,
foreclosure graph — which the pre-modification `magnifica_humanitas_ai_encyclical_original_run.md`
structurally could not produce). **But the flat path has a witnessed blind spot.** Comparing the
pipeline's `world3_recalibration_2024.md` to a thesis-driven web-Claude critique ("The Robustness Is
the Tell") surfaced a load-bearing seat — the policy REGIME (collapse is mountain-within-BAU,
rope-across-regimes; Stabilized-World = positive control) — that World3 never seated. Re-ran
`--dry-run --run-tag world3_kernel_probe` on the same source: **stably FLAT across 2 samples**
(`outputs/kernel_manifests/flat/…171605` and `…/world3_kernel_probe/…183123`, 0 readings). The
re-roll emitted the robustness fact itself as a standalone axis (`parameter_sensitivity_structural_robustness`)
AND `collapse_timing_credibility` as a separate axis but **never coupled them** (robustness ⊥
forecast), filing the regime as omegas (`omega_earth4all_paradigm_shift`,
`omega_belief_system_change_mechanism`). The gate decomposes a coupled kernel into independent axes +
epistemic omegas, dropping the coupling that makes it a kernel.

**Engine-vocabulary finding (the actionable one):** the mountain↔rope type-divergence IS the
necessity-vs-contingency kernel question. `collapse_mechanism_ambiguity` classified authored=mountain
→ computed=**rope**, conf 0.01, `type_1_false_summit` severe — the engine adjudicated the seat SCOPE
never built. Second witnessed instance same run: `demographic_skill_mismatch` (china, flat),
mountain→rope, conf 0.01. ⇒ **OQ-88**: flat-routed ∧ false-mountain = candidate kernel false-negative;
N=2 = positive control; a negative control sweep (don't blanket-fire on authoring-flinch
false-mountains) is REQUIRED before it auto-routes (else it repeats OQ-79's kernel-liberal
over-routing one level up). Also logged in the magnifica run (separate, not yet OQ'd): one of the 5
readings (`technocratic_paradigm_resistance`) carries 3 DANGLING `cs_reading_relation` edges to
`*_ai_governance` sibling-ids that were never generated (naming drift; OQ-58 integrity sweep is
skipped on the no-scope/kernel path) and duplicates the magisterial reading's axioms — the essay
silently treats it as 4 readings, but the broken 5th is in the corpus. Provenance: this analysis;
detector design + control requirement in ISSUES.md OQ-88.

## 2026-06-08 — Register OQ-83 committer-stage-time / observer-residual fields in pipeline schema
**Files:** python/shared/schemas.py, prolog/json_report.pl
**Tier:** landed

`json_report.pl` emits four OQ-83 fields per constraint — `cs_reference_frame`,
`cs_drift_moment`, `cs_drift_gap` (committer stage-time, commit ef5a9188) and
`temporal_residual` (Type-A observer residual, de3736a6) — but `PIPELINE_FIELDS` in
`python/shared/schemas.py` never listed them, so the drift detector printed
`[WARN] unexpected field: …` for every one on every constraint across every pipeline
tier (~280 lines/run; visible in the world3 orchestrator run). Added the four as
nullable declarations (str/str/dict/dict), grouped with their CS-UID siblings and the
temporal-trajectory block. Nullable ⇒ absence/null permitted, present values typed.

**Witness:** `PYTHONPATH=python` → `validate_pipeline_output` and
`validate_enriched_pipeline` on the on-disk artifacts both return **0 errors, 0
warnings** (was: 4 drift warnings × every constraint). No new type-validation errors.
Producer side (`json_report.pl`) unchanged — schema caught up to the emitter, not the
reverse.

## 2026-06-08 — make_brief: source-abstraction tool for oversized/refusing inputs (canonical llm_call; measured ingest ceiling; STOP-by-default refusal)
**Files:** agent/llm_call.py, agent/make_brief.py, agent/c-orchestrator.py
**Tier:** tripwire

Built a reusable brief tool so big/refusing source docs (spacex_s-1.txt 1.6 MB; the
PIIS vaccine paper that flat-refuses on Sonnet) can feed the orchestrator. Three pieces:

- **`agent/llm_call.py` — the ONE canonical Anthropic call path.** `get_client`,
  `call_with_retry`, `extract_text`, `count_tokens`, `context_window`, and
  `ModelCallError` (now carries `stop_reason`/`model`/`refusal_text`). `c-orchestrator.py`
  imports these; its `_call` is a thin wrapper. Consolidates the fix-#1 refusal detection
  (commit 7e85b261) into one spot so it cannot fork (Build Discipline pattern 2). NOTE: the
  orchestrator filename's hyphen blocks normal import — new callers import `agent.llm_call`,
  never the orchestrator.
- **`agent/make_brief.py` — NEUTRAL structural compression.** Emits MAIN IDEA / SOURCE'S OWN
  FRAMINGS / KEY FACTS / WHAT IS CONTESTED, and does NOT pre-partition into named READINGS
  (keeps primed SCOPE's kernel call un-anchored). Map-reduce over `SINGLE_PASS_BUDGET_CHARS`
  (~250 KB): Haiku maps chunks, Sonnet reduces. CLI: `python3 agent/make_brief.py f.txt`.
- **Orchestrator triggers (asymmetric, by design).** SIZE → auto-brief, but only when the
  topic exceeds the **MEASURED** ingest ceiling (`_ingest_decision`: window − step
  scaffolding − reserved − margin, min over research+decompose; **decompose binds** because
  the raw topic is packed only by research+decompose — generate works from the manifest).
  REFUSAL → **STOP by default** with a manual-route message (schema + scope prompt +
  build_prompt pointers); `--auto-bypass-refusal` is opt-in and logs the witness (refusal +
  the reframing that got it through), never a silent classifier bypass.

**Tripwires for a cold reader:**
1. **A brief is LOSSY — never feed one when the doc would fit whole.** The measured ceiling
   (~175K tok for decompose) deliberately sits far above the old asserted ~120 KB idea:
   witnessed spacex (~417K tok) briefs, but **magnifica (267 KB / ~69K tok) feeds WHOLE**
   (headroom +106K) — the old default would have needlessly briefed it (Phase-0: whole reads
   richer). Don't reintroduce a KB default below the measured ceiling.
2. **Neutral brief of a SINGLE-VOICE source under-routes to flat without research.** spacex
   S-1 is a prospectus (issuer voice only); the neutral brief faithfully says "no real
   contest… it is a prospectus." With `--skip-search` → SCOPE routes FLAT (8 risk axes,
   is_contested_kernel=None). WITH research grounding → recovers a contested kernel
   (`valuation_legitimacy`, 7 axes, readings dcf_fundamentalist/real_options_technologist/
   musk_cult_believer/governance_skeptic). **Research grounding is load-bearing for kernels
   from single-voice docs.** The hand-authored kernel-shaped `spacex_s-1_brief.md` (left
   untouched) imported external constituencies and routed kernel `dual_class_legitimacy` even
   without research — i.e. the two brief styles foreground DIFFERENT kernels (seat/framing-
   relativity), and a `--skip-search` manifest comparison is NOT apples-to-apples.

Verification (all witnessed this session): no-regression dry-run (no brief, manifest OK);
unit brief has no READINGS partition; measured ceiling (spacex trips, PIIS/magnifica fit);
map-reduce 44 chunks→6 KB brief, fidelity spot-checked against source (10:1 votes,
controlled-company, $41,311M deficit all present); PIIS default STOP prints manual route;
PIIS `--auto-bypass-refusal` succeeds on Haiku with logged before/after, fidelity confirmed
(DIOSynVax/S309/"not observed"/modest/baseline all in source).

## 2026-06-08 — Type-A snapshot floor + observer residual detector landed (time-aware d; ε-driven flips are NOT empty — 56/100)
**Files:** prolog/constraint_indexing.pl, prolog/drl_composition.pl, prolog/transition_paths.pl, prolog/temporal_residual.pl, prolog/json_report.pl, prolog/stack.pl, audits/2026-06-08_typea_template_extensibility/, docs/deferential_realism_paper_v7.md
**Tier:** landed

Pre-rebuild audit (`audits/2026-06-08_typea_template_extensibility/AUDIT.md`) then a **strict Tier-2,
schema-deferred** build of the Type-A (temporal) observer floor. The engine could express
classification drift over the authored timeline only through ε (both temporal classifiers read
time-varying ε from `measurement/5` but called `derive_directionality/3` with **no Time** — d
frozen). This build threads Time without authoring any time-indexed-d, and adds a read-only residual
detector.

**What landed (engine plumbing only; NO schema/template change):**
- `constraint_indexing.pl`: `derive_directionality_at/4` + deterministic `effective_time/3` (the C2
  frame_policy insertion point) + `:- dynamic time_indexed_directionality_source/4` (empty — the
  future C1 hook). Fail-closes to `derive_directionality/3`; **byte-identical on the current corpus**
  (no source facts).
- `drl_composition.pl`: `classify_at_time/5` surfacing `snap(D, Backed, Eps, Supp, Theater)` (the `/4`
  delegates; cs_kernel_registry + tests untouched). `Backed=false` flags the `:201` ε=0.5 fabrication
  and the STOPGAP scalar suppression — so phantom flips across real→fabricated transitions are
  excludable. Classification math unchanged.
- `transition_paths.pl`: `snapshot_type` `:130` swapped to `derive_directionality_at` (sync only, NO
  `backed` — it is default_context-only and nothing reads its backed).
- `temporal_residual.pl` (NEW, observer-only **category-B** seam diagnostic; reads NO `cs_`): per
  `(C,Context)` ran-witness (`times_examined`, `backed_times`) + flip composition — real flips only
  between adjacent `Backed=true` snapshots; type-changes touching a fabricated snapshot counted apart
  as `fabrication_adjacent_transitions` (a cross-metric hygiene counter, NOT signal). Emitted per
  constraint by `json_report.pl` (manifest-stamped via the single-writer pipeline).

**Finding (re-witness before citing): the residual is NOT empty on the current corpus.** 56/100
constraints show ≥1 backed flip; **155 counted flips** across the canonical contexts (e.g.
`ai_governance_accountability` at the analytical seat: scaffold→tangled_rope, t3→t6, Δε=0.05). Because
d is frozen on the current corpus (no time-indexed source), **every backed flip is observer-metric-
driven (ε/suppression/theater), not d-driven.** This contradicts the pre-build "expected empty" prior
and bears on the D-fork: substantial ε-driven flips at fixed role/d mean the cheap path produces
signal, so role-time-indexing (OQ-83 branch b) is NOT forced by emptiness.

**Bounds on the 155 (so it is not banked as an unqualified count):** |Δε| median 0.07, 120/155 > 0.05,
only 1/155 in the ≤0.02 jitter band → the flips track real ε movement, not boundary jitter; 150/155
flip-intervals sit on a fully-backed series. **Caveat (the classifier-sync OPEN, below): at the
default context — the only context with a second classifier — 2 of 52 counted flips touch a
snapshot_type-vs-classify_at_time disagreement point and are flagged classifier-sensitive for the
offline join.** Whether each flip is a genuine Type-A residual vs a committer-shadow is the OFFLINE
join — gated on the committer-time enrichment (see OQ-83 note).

**Verification (all 9 pass; audit dir has the recipes):** V1 pipeline byte-identical after stripping
the new block + manifest (no regression); V2 `derive_directionality_at` ≡ `derive_directionality` +
deterministic over 500 (C,T) pairs; **V3 — `test_snapshot_migration` green, but the named-test "sync"
is the WEAK claim: full `classify_at_time` ≡ `snapshot_type` is FALSE and was always false (3 unique
mismatch points at default context — the earlier "7" was metric-duplicated; my edit is sync-neutral,
witnessed on stashed code). The "two classifiers in sync" must-hold is OPEN, not passed. Contamination
join {3 mismatch}∩{52 default flips} = 2 flagged (clinical_deskilling_automation 0→2; milblogger
12→18). Likely cause [UNVERIFIED]: snapshot_type calls classify_from_metrics WITHOUT the `nb_setval`
temporal theater/eps state classify_at_time threads, so the piton/excess gates read stale/static.**
V4 residual reads d off
`/5` (0 second-derive); V5 real flip well-formed; V6 retracting an authored ε moves a real flip into
`fabrication_adjacent` and restores (guard fires); V8 no `cs_` in the module, imported only by
stack+json_report; V9 `git diff` touches only 5 engine files, no `schemas/`.

**Stale doc corrected (operator-flagged, substrate-confirmed):** v7 §4.5 "exactly one intentional
bridge … and nothing else" undercounts the cross-axis seam. The **(A) data bridge** is still exactly
one (`influences`→`detect_necessity_inheritance`); but **(B) read-only seam diagnostics** number ≥3
(`cs_drift_mismatch`, `cs_kernel_registry`→`classify_at_time`, `cs_pattern_detection`→
`constraint_signature`). Separation holds; the enumeration is wrong. (Audit §0; OQ-83 follow-on.)

**Follow-up (same day): committer stage-time enrichment LANDED** (OQ-83 follow-on #1). The CS block in
`json_report.pl` now emits `cs_reference_frame` (t0), `cs_drift_moment`+`cs_drift_gap` (t1) beside the
pre-existing `cs_drift_terminal` (t2) — 7 constraints carry it, null otherwise; no-regression empty
modulo the 3 new keys. Both temporal descriptors (observer integer-time + committer named-moment) are
now joinable per constraint `id`: the offline residual-subtraction join is unblocked; the
moment-to-integer reconciliation rule stays offline (not baked in-engine).

## 2026-06-08 — Observer-side temporal review: the DR "trajectory" is mostly dark; three "defects" dissolved; three deferrals are ONE coupled ruling gated on time-varying-d
**Files:** prolog/drl_composition.pl, prolog/transition_paths.pl, prolog/drift_events.pl, prolog/cs_kernel_registry.pl, ISSUES.md
**Tier:** correction-key

*[Merged late from worktree `sdm-temporal-records` on 2026-06-11. Superseded in part by later
entries: OQ-46's "12 scalar-only are GAPS" framing was overruled (scalar-as-constant SANCTIONED,
bucketed Backed, OQ-46 resolved 2026-06-11); OQ-83 RESOLVED 2026-06-11 with threads moved to
OQ-109/OQ-110; the time-varying-d D-fork was ruled NO-OPEN at OQ-110 (derived-d stands). The
OQ-41 rows 24–25 correction and the dormancy findings remain current as of the merge.]*

Pre-rebuild review of how the observer (DR) hub handles temporal declarations
(`narrative_ontology:measurement/5` series; Time = relative integer step, not calendar year).
Two hubs: **CS = discrete snapshots t0–t3** (straightforward); **DR/observer = a trajectory**
(per-time re-classification). Read against `docs/debugging_philosophy.md` Type A (frame-fixing).
Started as "fix three temporal defects pre-rebuild"; the substrate dissolved all three. No
engine-logic changed — records-only corrections on existing OQs. Witnesses (read-only, swipl
`[stack]+ensure_corpus_loaded`, N=100):

- **Coverage re-witnessed; prior 471/562 was pre-reset kernel_v1, STALE.** Live AS OF
  2026-06-08: temporal `suppression_requirement` **88/100**, `base_extractiveness` 100/100,
  `theater_ratio` 100/100; **12** scalar-only (STOPGAP), **0** unknown. Corrected in OQ-46 and
  the `drl_composition.pl:174–198` comment. Re-witness on corpus growth.
- **The 12 scalar-only are asymmetric-authoring GAPS, not scalar-by-design** — all 12 carry
  baseE+theater temporal series; only suppression's is missing. Completing it would not
  fabricate motion (no synchronic constraints in the set). Per-story "is suppression flat by
  design for any one" deferred to template/rebuild (authoring judgment, not engine fact). OQ-46.
- **`BaseX=0.5` (`drl_composition.pl:201`) is REACHABLE-BUT-LOCKED, not latent** — OQ-41 rows
  24–25's "extractiveness required-authored" reason is stale. Branch would fire at 11 (C,T)
  cells (e.g. `attribution_erosion-3`), **all non-zero-time (3,5,8,10,16,19), 0 at t=0**. The
  only live caller (`cs_kernel_registry`) classifies at t=0; non-zero times reached only via
  the dormant `constraint_history` sweep → not live. Corrected in OQ-41.
- **The DR trajectory classifier is DORMANT (positive-controlled).** Same consumer-probe finds
  `classify_at_time`'s consumer (`cs_kernel_registry.pl:66–67`) but **none** for
  `constraint_history`/`snapshot_type`/`degradation_chain`; their entry points
  (`transformation_detected`/`canonical_transformation`/`transformation_type`/
  `predict_transformation`) have **zero callers anywhere**. So the "fork" + nb_setval-asymmetry
  "defects" live in code nothing runs; the `measurement/5` series is authored/live but the
  thing that turns it into a classified trajectory is wired to nothing. Live temporal consumers:
  `classify_at_time` at t=0 only, and qualitative drift via `drift_report` (test/lifecycle, NOT
  `run_pipeline.py`).
- **The fail-closed-vs-impute choice is the deferred OQ-44 once-for-class ruling** — the three
  "fixes" were per-site moves on a class decision; recorded as class members (BaseX, snapshot_type
  defaults) under OQ-44, not fixed per-site.

**Coupling (operator ruling, due before the rebuild template is fixed).** Three deferred
temporal threads are **ONE ruling with three faces, gated on the time-varying-d decision**, not
three independent OQs: (a) **time-varying-d** (OQ-83 deferred Ω); (b) **revive-or-gap the dormant
trajectory classifier**; (c) **rebuild temporal-authoring density** (author dense series at
all?). Coupled because the trajectory classifier is dormant **and** would freeze directionality
even if revived: `derive_directionality/3` is not time-indexed and beneficiary/victim are
static-only (0 temporal beneficiary/victim/directionality facts live OR in archives), so
`check_capture_between/3` launders a role-shift it cannot see into an ε-magnitude event.
Reviving (b) is worth it only if (a) is in scope; authoring dense series (c) only if (b) will
consume them. Substrate finding + coupling recorded on OQ-83.

**Meta-pattern worth flagging at the rebuild.** This is the third consecutive deep-read this
session to resolve to "this doesn't need doing" (step-4b `in_contention` feeds nothing; OQ-85
disentangling info absent from substrate; these temporal fixes dormant/locked). The live
load-bearing surface is smaller than the activity around it — the engine carries more dormant /
dangling machinery than live. The **rebuild is the decision point for carry-forward vs. shed**:
regenerating dense temporal series feeds a trajectory classifier nothing consumes, so the
revive-or-gap of the observer trajectory is not tidy-up — it is whether the rebuild's authoring
cost is feeding a dark wire.

## 2026-06-07 — Stakeholder-layer migration Pass-1 audit: computed path ignores authored perspectives (controlled null); straitjacket witnessed; mandatrophy surface is a dangling wire
**Files:** prolog/constraint_indexing.pl, prolog/drl_core.pl, prolog/constraint_data.pl, prolog/probe_harness.pl, prolog/inferred_coupling_protocol.pl, prolog/drl_purity_network.pl, prolog/reading_diff.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, schemas/constraint_story_schema.json, audits/2026-06-07_stakeholder_layer_migration/
**Tier:** landed

Full report + evidence: `audits/2026-06-07_stakeholder_layer_migration/` (AUDIT.md leads with the
keystone). Tracker: ISSUES.md OQ-83 (rulings of record R1–R5, R4 reversed-from-consensus,
sequencing, deferred Ωs). Verdicts, each witnessed in AUDIT.md:

- **A1 keystone (controlled null):** flipping an authored `constraint_classification/3` fact
  (snare→mountain) leaves dr_type/χ/signature/H¹ byte-identical over canonical-4 + product-156
  (162/162 lines); the ε-overlay control on the same story moved EVERY register (120/160 type
  flips, 160/160 χ, sig false_ci_rope→constructed_high_extraction, H1 3→5). The computed
  classification path does not read authored perspectives — the stakeholder layer is an additive
  refactor on the engine side.
- **A2:** d keys on (power atom × beneficiary/victim EXISTENCE booleans × exit) — removing either
  single victim leaves d=0.5 untouched; removing all moves d to 0.46; the atom-keyed override
  moves every same-atom agent together. Two-powerful-agents collapse confirmed.
- **A6 guard asymmetry (silent-mistake warning):** the intra-kernel filter on `shared_agent_link`
  exists at `drl_purity_network.pl:96–98` but NOT at `inferred_coupling_protocol.pl:218–222` —
  same-kernel shared agent names DO enter `run_coupling_protocol`'s edge set. Any cross-reading
  stable-name convention must ride a NEW predicate or add the guard at the second site first.
- **A7 dangling wire:** schema `base_properties.mandatrophy_resolved` has ZERO compiler emissions
  (only `mandatrophy_analysis` commentary prose is emitted); `has_mandatrophy_declaration/1` reads
  `attribute(C, lifecycle, mandatrophy)` = 0 facts corpus-wide; `is_mandatrophy_resolved/1` = 2
  hardcoded archived-corpus facts. Authoring a `mandatrophy_resolved` value today does NOTHING.
  R5's genealogy consumer rewires this (OQ-83), not a third surface. Promoted to CLAUDE.md
  Critical Distinctions (operator, same day); retire that note when the rewire lands.
  **Abandonment reason git-witnessed same day:** emission never existed in any version — engine
  consumers entered at `6f997d71` (hand-authored era), the schema boolean at `3641ae71`
  (JSON-template migration) whose compiler only ever emitted `mandatrophy_analysis` prose. A
  dropped seam at the format migration, not a gameability wall; R5 inherits no hidden wall.
  A6's guard asymmetry split out as its own engine-hygiene item → OQ-84 (operator, same day).
- Probe infra (cost two failed runs): `probe_harness` is NOT loaded by `[stack]` (explicit
  `use_module` required); `domain_priors:base_extractiveness/2` is STATIC (retract throws) and is
  not on the ε read path — overlays target `narrative_ontology:constraint_metric/3` (the chain is
  drl_core.pl:84 → constraint_data.pl:11–13 → constraint_metric).
- A4 role-alignment: 85.0% (1046/1230) middle band → proceed + residue ledger
  (`a4_residue_ledger.md`): contender 6.3% (dial-set backgrounds contention), ritual_operator
  1.9%, dual_role, non_agent. Cuts 90/70 operator-declared, revisable against the ledger.
- **Phase A step 1 LANDED (same day):** `schemas/constraint_story_schema.json` gains OPTIONAL
  `stakeholders[]` (five-role declared dial-set; contender ruled out — contention is derived,
  relational; per-stakeholder agent-hood gate; name rule per OQ-84) + `six_questions` (Q3/Q4/Q5 +
  R5 genealogy, mismatch-consumer-only, provenance structurally required). Pattern-5
  authored-empty conditional enforced. Witness: `phase_a_schema_witness.py` 7/7 — pre-migration
  story still validates (additive), stakeholder story validates, four negative controls each bite
  at the intended guard; validated with Draft7Validator (the pipeline's actual validator —
  installed jsonschema has no Draft202012).
- **Phase A step 2 LANDED (same day): compiler emission closes the window.**
  `generate_constraint_pl.py` emits `constraint_stakeholder/7` (+ secondary_role/non_agent
  facts), `disappearance_verdict/2`, `founding_problem_status/2`, and role-derived
  beneficiary/victim (agent-gated; excluded derives NOTHING — R3; deduped, duplicate facts would
  inflate victim critical-mass counts). Witnesses: 0/100 old-vs-new diff; pilot with all five
  derivation branches; lint clean; swipl-loads. ALL witnesses re-ran against the post-fix file
  (fix → pilot recompiled to disk → branch greps → diff re-run → lint+load), not just the diff.
  **Bug caught pre-commit, and WHICH probe caught it is the lesson: the additivity diff (0/100,
  the strongest automated check) was STRUCTURALLY BLIND to it — no corpus story has
  six_questions, so the corrupted path never executed on the corpus and "0/100 differ" was a true
  statement about a probe that could not reach the defect. Only the pilot's per-branch greps — a
  positive control aimed at the path the corpus cannot yet exercise — could see it. PROBE
  PLACEMENT RULE: every compiler feature with no corpus coverage has this same blind spot; pair
  it with a per-branch pilot probe on the uncovered path, and never read a green aggregate diff
  as covering paths the corpus doesn't contain.** The trap itself (generic to generate_pl): a
  local named `lines` shadows the `emit` closure's accumulator and silently discards all prior
  output while reporting success — silent-on-success, nothing downstream complains; comment at
  the site. **A6 sub-gate inside step 2 — clean, but read its scope precisely:** 0 engine
  consumers of new predicates (control fires), 0 new cross-constraint shared atoms, guard sites
  untouched — this clears the DERIVED-name half only (derived names reuse existing naming +
  dedupe). The bespoke AUTHORED stakeholder names (the population the 504/25/38 baseline was
  about) arrive with step-3 projection, against the still-unguarded
  `inferred_coupling_protocol.pl:218–222` (OQ-84). **A6 is closed for derived facts and REOPENS
  at step-3 projection — the guard lands before-or-with the projection, same pass, never
  after.** Step 3 (engine layer) is the next forward move; until then stakeholder/7 + the two
  atoms are produced-awaiting-consumer (named hold), while derived beneficiary/victim are
  consumed immediately by existing d/FSM machinery.
- **Phase A step 3 LANDED (same day): engine layer; mechanism witnessed (scoped as plumbing —
  the experiment is step 4).** Delegation refactor `extractiveness_for_agent/3` →
  `extractiveness_for_agent_d/4` witnessed BYTE-IDENTICAL on the A1 harness (162 rows,
  canonical-4 + product-156); `dr_type_with_d/4` (skips resolve_coalition_power — caller owns d);
  `stakeholder_seats.pl` per-(C,Name) layer (role-d config params = DECLARED fitness-chosen seat,
  config.pl comment; all outputs commentary-grade); narrative_ontology decls (the five
  stakeholder predicates are dynamic — but **`cs_kernel_id/2` is STATIC: assert throws; consult
  a temp multifile file as the overlay tool**); R5 zombie clause (second
  `has_mandatrophy_declaration` clause over the two authored atoms, mismatch-only). OQ-84 guard
  added in the same pass (bug branch git-witnessed: coupling module frozen 2026-02-18,
  pre-kernels; live 72=72 no-op, synthetic same-kernel 1→0). Mechanism witness
  (`step3_mechanism.txt`): same substrate, atom-keyed all-solutions `[0.15]`/one type vs
  name-keyed 0.12/0.85 split, causally traced via payer-param overlay (only payer seats moved;
  restore verified); control story no-split. Untested this pass (deliberate mobile-isolation):
  exit-mod arms beyond trapped(+0.05), the d clamp. Validation suite 0 warnings post-change.
- **Phase A step 4 — 4b gate fired RENAMED-NOT-ESCAPED (same day); 4c NOT run; STOP, operator's
  to act on (OQ-85 filed).** Pilot stakeholder prompt (neutral, witnessed) + constant-scaffold
  adapter + scaffold-leak witness (PASS both axes — type and tuple inert, positive control
  fires) + 6 topics pinned-before-gen, model pinned gemini-2.5-pro. Across all 3 contention
  topics the headline antagonists never land same-power+opposed-role: streaming & hospital
  authored both as agenda_setter at the same power (institutional) — opposition only in prose;
  app_store opposed-role but power-atom-drifted. `in_contention` (same-atom AND opposed-role)
  fires on neither headline shape (but IS live — fired on 3 non-headline/non-contention/mountain
  pairs = positive control). Dual cause: generation (gemini renders co-equal contention as two
  agenda_setters) + vocabulary (the d-split only separates agenda_setter/beneficiary-vs-payer,
  so opposed co-administrators are invisible — A4 contender-residue with evidence). Bears on the
  A4 derive-don't-author ruling. Phase B + 4c + the 2×2 model Ω gated until OQ-85 ruled.
  Evidence: `audits/.../STEP4_4b_RENAMED_NOT_ESCAPED.md` + 6 `*.stakeholder.json`. No live-corpus
  writes; four-tuple prompt untouched.
- **OQ-85 RESOLVED same day — silence-is-correct; the 4b gate was guarding a non-problem; 4c
  unblocked.** Read-only decomposition audit (`audits/.../OQ85_DECOMPOSITION_AUDIT.md`).
  Load-bearing witness: **`in_contention` feeds NO classifier** (grep: zero consumers;
  dr_type/classify_from_metrics/signature read neither it nor `constraint_stakeholder`) — it is
  annotation, so its silence on co-equal antagonists cannot be a classification blind spot.
  General result (not corpus-contingent): the constructed no-anchor worst case `oq85_blindspot`
  (two co-equals both agenda_setter, zero victims) still computes `snare` — type is metric-driven,
  correct without the pairwise relation even when no powerless anchor exists. Corroboration (not
  load-bearing): both real stories carry a powerless anchor making the rivalry second-order. The
  4b renamed-not-escaped did NOT find a straitjacket gap — co-equal contention is outside
  `in_contention`'s job; the straitjacket was escaped for everything it exists to detect.
  Right-sizing: a consumer grep would have settled this at the 4b gate, three turns earlier.
  Residual filed standalone as OQ-86 (pairwise who-extracts commentary; not in the migration).
  **4c (cross-framing census / Ω_E / Type-C/B) is the unblocked next move.**
- **4c RAN (same day, PILOT n=6); report `STEP4C_PARTITION.md`; presented not ruled.** Per-topic
  partition (bin-blind evaluability = (a) same-object + (b) (HasB,HasV) profile; ε-pinned): 2
  survived, 2 flipped, 2 unevaluable. Headline type survived in all 4 evaluable (snare). **Both
  flips dissolved to a resolution artifact by per-flip scrutiny:** all-metric-pin control showed
  not-metric-drift; mechanism = victim COUNT × critical_mass_threshold(=3) via
  resolve_coalition_power at the powerless seat (flips = stake 3 victims vs four 2; hospital 3-v-3
  survived). **Criterion finding (next corpus-scale run, pre-register):** (b) incomplete — orbit
  reads victim count via coalition, not just the boolean; extend (b) to count-same-side-of-
  threshold. **Robust separate signal:** claim-layer framing effect — stakeholder claims `rope`,
  four-tuple claims `tangled_rope` (3/3 contention), engine corrects both to snare (claim moves,
  computed type doesn't). Engine has no framing-sensitive classification layer (orbit =
  f(metrics, victim-count); perspectives ignored per A1). Type-C/B not settled at n=6 → corpus
  run + 2×2 model Ω. **Swallowing-trap recurred:** the all-metric-pin control's first run
  returned empty==empty and falsely read "identical/metric-drift"; caught, re-run, corrected
  result was the OPPOSITE — a diff-of-two-empties is a false pass, not a witness.
- **Committer-axis thread BANKED/PARKED (2026-06-08); cold-read entry
  `audits/2026-06-07_stakeholder_layer_migration/COMMITTER_THREAD_HANDOFF.md`; tracker OQ-87
  (partial).** Banked verdicts: two-axis architecture real (observer orbit framing-blind incl. to
  cs_structure; committer axis separate structure-sensitive surface → observer-axis Type-B
  architecturally foreclosed); CA-1 committer field partition confirmed (framing-invariant,
  content-sensitive); CA-3 kernel_v1 diverge-A 74 is ~89% one drift convention (saturation), NOT
  load-bearing (per-item cause witness — banked the standing rule *a gating count needs its
  composition in the same pass*, build_discipline.md); Step 0 observer claim-drift MODEL-STABLE
  (rope/tangled_rope reproduces 3/3 under Sonnet); pilot Steps 1/1b/matched — kernel_v1
  husk-saturation is reading-set + magnitude-authoring, NOT a Haiku prior, NOT removable by the
  Haiku→Sonnet bump (the MATCHED run — same manifests, vary only GEN model — overturned the
  unmatched Step 1b "Sonnet de-saturates"; ack-false is reading-set-dominated 49–92%, substantial
  robustly 62–88%). Detection-independence existence proof UNPROVEN; next move is a fresh-decision
  LARGER de-leaked study (not a model swap), + CA-2 for committer C/B. Run-tagged pilot stories
  (`prolog/testsets/pilot_*`, `json/pilot_*`) untracked, glob-isolated, disposable. ≈284 gen calls.

---

## 2026-06-06 — Kernel-first router: `_step_decompose` now uses the PRIMED scope prompt (construction-as-classifier)
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, outputs/kernel_first_phase0/PHASE0_READOUT.md
**Tier:** tripwire

`c-orchestrator._step_decompose` no longer builds the unprimed §3-independence prompt. It now calls
gkc `_scope_user_prompt({"human_readable": topic, "summary": ""}, research_context, self.axes)` —
the PRIMED prompt that asks the kernel question ("contested kernel? emit READINGS; else flat + collapse
omega"). This closes OQ-79 mechanism-2 (flat-miss: the old path never asked, silently flattening
genuine kernels — magnifica → 12 flat axes). Downstream is unchanged: `_step_generate` →
`generate_from_manifests` already handles kernel manifests (readings + the AUTO forced-flat control
from `flatten_manifests` lines 343–359 = the construction pair).

**TRIPWIRES (silent mistakes a fresh agent would make):**
1. **Do NOT revert `_step_decompose` to the unprimed "select every axis that survives §3" prompt.**
   It looks like the "normal" SCOPE call; reverting silently re-breaks kernel routing (the magnifica
   failure). The primed prompt is the single source in gkc `_scope_user_prompt` — both front-ends
   share it; do not fork a second copy (Build-Discipline Pattern 2).
2. **A kernel-positive (`is_contested_kernel=true`) means "admits a foundational construction,"
   dominance UNJUDGED — NOT "this topic IS a dominant/certified kernel."** The primed verdict is
   KERNEL-LIBERAL (Phase 0: routes to kernel whenever a foundational reading is constructible =
   contentful, `docs/seat-theorem-v1.md`; flat only when σ settles it). Loud means-disputes
   (nuclear-climate, reading-wars) route kernel. A downstream count / Tier headline / essay that
   reads the kernel set as "N genuine axiom-level contests" commits the seat-theorem no-seat pose
   (asserts a seat-free dominance ranking, which §6 forbids). Kernels accrue UNCURATED by operator
   ruling (2026-06-06, LIBERAL); a *seated* dominance stage is permitted but DEFERRED (design against
   a witnessed pile). See the promoted line in CLAUDE.md Critical Distinctions.

Witnessed (`--dry-run --skip-search` via the front-end): magnifica → `is_contested_kernel=true`
(3 readings) where the unprimed path flattened it; flat topic → `is_contested_kernel=false`
(reasoned rejection). Phase 0 + widen evidence + ruling: `outputs/kernel_first_phase0/PHASE0_READOUT.md`.
A3 grounding-leg DROPPED (Phase 0: wrong instrument — over-routed readings have real constituencies).

## 2026-06-06 — Generation-backend unification: c-orchestrator routed through the shared backend; the kernel-dropping fork DELETED
**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, agent/story_generator_base.py, python/audits/capture_generation_payloads*.py
**Tier:** landed

The silent fork (Build-Discipline Pattern 2) where c-orchestrator's flat-only generator silently
dropped recognized kernel readings (OQ-79 mech-1) is healed by DELETION. New shared backend
`generate_kernel_corpus.generate_from_manifests` is the single manifest->corpus path: seed-type
dispatch (flat -> c-orch framing via the moved `axis_source_desc`/`upstream_context` in
story_generator_base; reading/flat_control -> gkc `build_cached_messages`), c-orch's wave loop ported,
request defaults caller-supplied (sonnet/string-system for c-orch, haiku/list for gkc). c-orch's
`_step_generate` now calls it; the forked `_step_generate_batch` (44 ins / 255 del) + delegators +
dead imports are gone (grep 0). Serial escape hatch kept (self-contained inline source_desc, named
legacy duplication). OQ-79 guard demoted to a defensive assertion (no ledger; C4 co-mingling gone).

**Witness ladder (all in commits 0f61517c, 099066c4, a7d56a14, ed2ec212):**
- P0 old==old byte-identical across TWO COLD processes, FULL params (model/system/max_tokens) —
  the deterministic target is real.
- W1/W2 new==old byte-identical on 3 flat topics incl. germline (5-wave); re-confirmed AFTER the
  splice — the wiring that delivered kernels did not perturb the flat path.
- P3 LIVE: Zionism (frozen 222814 manifest) — the 3 readings the flat path dropped now land with
  cs_kernel_id; reading classifies tangled_rope/snare/rope/snare across seats.
- P4 mechanism: synthetic reading-upstream manifest — supplementary axis waves AFTER its reading
  with the reading's claimed_type injected (wave FIRES; appropriateness = OQ-81, NOT witnessed).
- The deterministic witness caught a real seed-building dup bug (readings in both axes[] and
  generation_sequence) BEFORE any live kernel run — fixed, germline still byte-identical.

**TRIPWIRE — partial unification:** gkc's `--scope` entry point STILL runs its own (working,
wave-free) kernel generation; it is NOT yet routed through generate_from_manifests (OQ-82). So two
generation implementations coexist — the BUG is gone (both handle kernels) but the literal one-path
goal + gkc-gains-waves remains. Do not assume gkc --scope already waves. New OQs: OQ-80 (generate-step
token totals unthreaded = NOT MEASURED, reports 0), OQ-81 (readings-as-wave-upstream appropriateness),
OQ-82 (the gkc --scope rewire). OQ-76 (never-recognized flat-miss) still uncovered.

## 2026-06-05 — Pre-build ruling session executed: OQ-70/64/63 ruled and landed, intent_* declared GAP-08, perturbation-principle §1.1 added
**Files:** prolog/signature_detection.pl, prolog/constraint_indexing.pl, prolog/narrative_ontology.pl, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prompts/constraint_story_generation_prompt_json.md, docs/design/design_gaps.md, docs/the_perturbation_principle.md
**Tier:** landed

Operator ruled the three pre-build items in one session, all on one principle (now written into
`the_perturbation_principle.md` §1.1): wherever two layers disagree about what an authored thing
means, the authored layer's definition is authoritative — the computed layer must never consume
what the author did not assert.
- **OQ-70-A as the CLASS** (`72ec2cdd`): claimed_natural source 2 + appears_as_rope's sibling
  removed — no signature may read a single authored perspective as a story-level claim. Live-20
  witness: FCR 16→5, FNL 3→1; positive control manpower_exhaustion_trap still fires FNL via
  source 1. Signature prevalence is a claims statistic from rebuild story 1.
- **OQ-64-A** (`e5fbc2e8`): `vindicated_propositions` schema array → `constraint_vindicates/2`
  (feeds NO metric/gate); beneficiaries are actors only; six witnesses incl. negative control.
- **OQ-63-A** (`28f2dfc8`): d-derivation consumes `agent_beneficiary`. ZERO-DIFF cutover
  (80/80 constraint×seat rows identical) + guard positive control (registry non-agent refused).
- **Item 2** (`f618c1f1`): intent_* = design_gaps GAP-08 (declared absence). Verification found
  the residual points PASS-OPEN: `has_viable_alternatives` defaults false on the empty table and
  NL certification REQUIRES false — OQ-43 fifth instance, fail-close deliberately not made
  (would un-certify all NL until intent is fed or the gate re-sourced; needs its own ruling).
- §1.1 added to `the_perturbation_principle.md` (operator-authored): the perturbable object is
  the authored story; the purpose is holding the seats without collapsing into one view or a
  view sub specie aeternitatis; every view is a view, even the God's-eye one.

## 2026-06-05 — CORPUS RESET: live testsets/ rebuilt from scratch under the de-leaked pipeline; all previous corpora consolidated to prolog/archives/datasets/
**Files:** prolog/testsets/, prolog/archives/datasets/, CLAUDE.md, AGENTS.md
**Tier:** tripwire

Operator reorganization (by hand; git-recorded as 13,532 renames in commit `29889e50`):
live pre-reset corpus (1,106 stories + stage1_probe/flatctl_probe/lineage_probe_01 run-tags)
→ `prolog/archives/datasets/kernel_v1/`; testsets_3000 (3,380 chimera-era) → `original_v6/`;
testsets_sotu (189) → `sotu/`; gaptests/recon_2/ab_test → dated `audits/` dirs;
commitment_corpus + fix → root `archives/`. New `prolog/testsets/` seeded with the first three
post-de-leak topic runs (20 stories). **Follow-up (same day, commit `1a0acfb8`): `json/` reset to
match — 4,410 pre-reset story specs + 21 pre-reset subdirs archived to
`prolog/archives/datasets/kernel_v1_json/`; `json/` now holds exactly the live stories.** **Tripwires:** (1) ALL pre-2026-06-05 empirical findings
(OQ-70 FNL stats, OQ-71 lineage, 55% coordination disagreement, sweep baselines, KNOWN_STATE
witnesses) were measured on `kernel_v1` or its ancestors — re-witness on the live corpus before
citing against it; retrospective audits overlay `corpus_path` to the archive dir. (2) run_pipeline
reports n_sotu=0 (graceful); sotu analyses must overlay the archive path. (3) The first-pass
new-vs-old comparison (this session): 3/20 new stories claim mountain and ALL fire
type_1_false_summit (incl. claimed-mountain ε=0.85 `manpower_exhaustion_trap`, unauthorable
pre-de-leak); old 0.58 ε-anchor gone, new 0.68 idiom (11/20, not band-linked — Stage-2 watch);
claimed-type diversity 5 types/run vs old tangled-dominance; seat-agreement 26/80 new vs 7/12 old
(old comparison biased: old pipeline steered claims into modal types).

## 2026-06-05 — c-orchestrator batch generation (dependency waves); repair de-fanged; report highlights authored-vs-computed divergence
**Files:** agent/c-orchestrator.py, agent/story_generator_base.py, python/story_repair.py, python/enhanced_report.py
**Tier:** landed

With the axis cap removed, 6–8 sequential Sonnet calls became the per-topic long pole.
`_step_generate` now dispatches to a BATCH path by default (`--serial-generate` /
`DR_SERIAL_GENERATE=1` keeps the legacy loop with its LLM retry-with-feedback): each §5.1
dependency WAVE is one Anthropic batch (50% cheaper; static prefix cache-controlled; `poll_batch`
reused from generate_kernel_corpus — no pattern fork); upstream claimed_type context flows
between waves; failed upstreams unblock dependents (no deadlock). `build_prompt` refactored into
`build_prompt_parts` (static/dynamic split) with a byte-parity witness (old vs new identical,
both arg shapes). Offline simulation witness (fake client): correct wave partitioning
(a/c/e → b → d), upstream context injected, cache_control present, 5/5 saved, tokens summed.
**Operator ruling folded in: generated stories are NOT linted at generation time and the
authored side is never "fixed" — divergence is read downstream.** Two enforcement changes:
(1) `story_repair.py` no longer fabricates `mandatrophy_resolved` from an extractiveness
threshold (band-keyed fabricated default writing an authored field; its schema conditional died
with the de-leak) — witnessed: repair leaves claim/metrics untouched, high-ε story without the
field validates; (2) `enhanced_report.py` CONSTRAINT IDENTITY now renders an explicit
"Authored vs Computed: DIVERGES at n/m seats — …(divergence is signal, not defect)" line in
both branches (witnessed both directions). The batch path contains zero lint calls (grep = 0).

## 2026-06-05 — Generate-both landed: forced-flat control on every kernel, mechanical alignment key flat_control_of/2 (OQ-76 mitigated)
**Files:** agent/generate_kernel_corpus.py, python/generate_constraint_pl.py, prolog/testsets/flatctl_probe/, ISSUES.md
**Tier:** landed

Operator ruling: generate-both promoted to PRIMARY fix for the stochastic kernel/flat gate —
the recognizer becomes REDUNDANT (every kernel gets a flat construction unconditionally) rather
than trusted; stratification and the kernel-bias hedge both routed through the broken detector.
Implementation: `flatten_manifests` auto-emits `<kernel_id>_flat_control` seed per kernel
(substrate = `kernel_description`; the reading set is NEVER shown to the flat author);
compiler emits `narrative_ontology:flat_control_of/2` from ephemeral `_flat_control_of`,
OUTSIDE the cs_structure gate; flat controls carry no `cs_kernel_id`/`cs_reading_relation`
(not pseudo-readings — kernel stats and OQ-58 sweep untouched); stamp_kernel_linkage extended
(separate counter, mismatch guard, no-cs exception). ASYMMETRIC BY DESIGN: flat-on-every-kernel
only; never kernel-on-every-flat. Witnesses: compiler emission + negative control; seed/prompt
independence on a real K1 manifest (no reading ids leak); E2E run-tag `flatctl_probe` — first
construction-pair diff via the key: computed dr_type construction-ROBUST (tangled_rope ×4 seats
both constructions), authored layer divergent (snare ε=0.65 vs tangled_rope ε=0.48).
Stage-2 residue: the readout stratum (OQ-76 Remaining). Interim kernel-bias hedge superseded.
Writeup + probe + seed: `audits/2026-06-05_flat_control_generate_both/`.

## 2026-06-05 — K1 kernel-gate replication: real topic-classed boundary band; under-firing misses against explicit §1.3-K criteria (OQ-76 filed; Stage-2 condition)
**Files:** python/audits/kernel_gate_replication_probe.py, prompts/uke_scope_v2_json.md, ISSUES.md
**Tier:** landed

Promoted from the count probe's side-observation by operator review: the kernel/flat gate routes
the SAME contested substrate (T5 manifests diffed — identical contestation as kernel readings vs
flat axis) onto the axiom vs observer axis, and a flat-miss destroys the axiom axis irrecoverably.
K3 hand-adjudication first (free): gig classification AND content moderation both pass all three
§1.3-K criteria → flat takes are gate MISSES, not definitional ambiguity. K1 (k=8 × 5 topics,
40/40 calls, pre-registered INVALIDATION conditions — personhood control <7/8 would have removed
the thin-band diagnosis from the menu): controls 0/8 and 8/8 (instrument valid); affirmative
action 8/8; gig 5/8; content moderation 3/8. Band is real and topic-classed (famous moral kernels
stable; statutory/regulatory contests near coin-flip); noise localized to the binary gate
(conditional reading counts perfectly stable: 4/3/3). Dispositions recorded in OQ-76, not built:
interim hedge = bias gate toward kernel on band topics (fail toward the recoverable side);
candidate fix = generate both representations (construction-pair diff is §7.1 signal); K2
phrasing-sensitivity probe licensed as cause-diagnosis. Stage-2 (OQ-75) now carries the routing
condition. Writeup + 40 manifests + driver: `audits/2026-06-05_kernel_gate_replication/`.

## 2026-06-05 — SCOPE count-distribution probe: 7-7-7 was coincidence + run noise, NOT an implicit target (OQ-75 watch resolved)
**Files:** python/audits/scope_count_distribution_probe.py, prompts/uke_scope_v2_json.md, agent/c-orchestrator.py, ISSUES.md
**Tier:** landed

Two-arm (current vs pre-`d179423d` SCOPE prompt — the lens instruction IS in the decomposition
system prompt, `c-orchestrator.py:177,421`, so one arm couldn't name a FAIL's lever), 8-topic
richness-spanning battery, 16/16 calls, pre-registered signatures including the upper-tier
masked-target sub-criterion (T4–T7 must spread among THEMSELVES; a binary floor rescuing global
range = FAIL). Result: selected counts 3→11 tracking richness; upper tiers spread (A: 5/6/6/11,
B: 5/7/6/9); deferrals fire (six non-zero cells — §4 triage visibly works, including deferring
an axis that overlapped another); replicate noise ±1; arms agree; T7's 11 axes shown
pairwise-distinct (1 borderline composite). Bridge replicate: gig-economy 7 (original) → 5
(re-run) — the original uniformity was mid-richness coincidence + temp-0.2 run noise. Stage-2
(OQ-75) is NOT gated on a SCOPE-framing fix; axis-count distribution at scale is a readout, not
a gate. Side observation (recorded, not gating): kernel-recognition is itself noisy — T5
decomposed as a kernel in one arm only. Writeup + 16 raw manifests + driver:
`audits/2026-06-05_scope_count_distribution/`.

## 2026-06-05 — Generation-pipeline de-leak: schema/prompt/feedback boundaries no longer hand the author the engine's bands (audit brief F1–F9)
**Files:** schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md, prompts/uke_scope_v2_json.md, python/linter.py, python/regenerate_stories.py, python/generate_constraint_pl.py, agent/c-orchestrator.py, agent/orchestrator.py, agent/uke_narrative_orchestrator.py, agent/story_generator_base.py, agent/generate_kernel_corpus.py, docs/logic_extensions.md, docs/technical/generation_path_resolution.md
**Tier:** landed

The authored-claim-vs-computed-type diff is the research signal (`the_perturbation_principle.md`);
the pipeline was handing the authoring LLM the engine's decision boundaries, collapsing it.
**Binding leak was the SCHEMA, not the prompt:** `allOf` conditionals tied `claimed_type` to numeric
bands AND the schema text ships verbatim in the generator prompt (`story_generator_base.py:28`,
`build_prompt`), with validation a retry-until-valid gate — a claimed-mountain/high-ε story (the
false summit the engine exists to catch) was literally unauthorable. Commits, each with same-turn
witnesses:
- `29cd45d4` linter coordination_type 4→6 (286 false INVALID_COORDINATION_TYPE cleared, corpus lint
  1821→1535, delta exactly 286; positive control still fires) + canonical 6-value table with
  offset-active/floor-inactive asymmetry → `docs/logic_extensions.md`.
- `9f2d050a` schema de-leak (user ruling: bands AND ε>0.46/0.70 triggers; allOf 9→6; structural
  conditionals kept; measurements/omegas unconditionally encouraged). Witness: synthetic
  claimed-mountain/ε=0.6 REJECTED before → AUTHORABLE after; tangled-without-victims still rejected.
- `b6c4e113` prompt de-leak, maximal scrub (qualitative type criteria; χ/sigmoid/f(d)/σ tables →
  prose, d∈[0,1] semantics kept for overrides; NL-profile 0.85/0.15 → presence-with-honest-values;
  worked-example ε anchors removed; epsilon_bin hand-off dropped in all three orchestrators).
  **Closing witness at the real interface:** assembled `build_prompt` payload, band-near-type hits
  19→0 and threshold-comparisons 28→0, both greps firing on the pre-change payload.
- `7ad86c5a` axes cap → optional ceiling (`--axes` default None in c-orchestrator + gkc;
  SCOPE "THREE IS THE BUDGET" → distinctness-is-the-budget; §4 = ranking/ordering only).
  No-cap witness on 3 topics: **uniform 7-7-7 axes, 0 deferred** — axes 4+ are NOT near-duplicates
  (distinct deltas/observables; contingency gate did not fire) but the uniformity suggests a new
  implicit count target; re-check distribution at Stage 2 (OQ-75).
- `07f7b1c0` regenerate_stories filters THRESHOLD_COUPLED lint codes (SCAFFOLD_DANGER_ZONE,
  LOW_THEATER_RATIO, MOUNTAIN_METRIC_CONFLICT) at the build_user_prompt choke point — covers BOTH
  channels (known_errors from lint_errors.txt + retry_errors). Witness: tripping story's lint shows
  the code, built prompt doesn't, MISSING_NL_PROFILE passes through. Rules stay as offline
  diagnostics: their firing rate IS the claim-vs-metric divergence readout.
- `d179423d` lens-diversity SCOPE instruction — **SEPARATE CHANGE VARIABLE** (user ruling):
  attribute reading-set shifts to this commit, not the de-leak, in Stage-2 readouts.

Engine-side verification (no engine changes): authored type lands as
`narrative_ontology:constraint_claim/2`, read ONLY by diff detectors (`drl_core.pl:566
dr_claim_mismatch/4`, `claimed_natural`); `dr_type/3` computes from metrics; no fallback returns
the claim (brief F8 moot). Probe controls: clean corpus mountain (`axiom_of_choice_consequence`)
reads claim=computed=mountain ×4 seats, no mismatch; synthetic false summit compiles and fires
`type_1_false_summit-severe` (computed tangled_rope at institutional, unknown elsewhere — OQ-37
surface). Stage-2 rebuild is OQ-75 (gated on operator go). New OQs: 72 (axiom alignment key), 73
(cross-frame probe), 74 (coordination_type kernel-vs-reading ruling; 55% = 158/286 re-witnessed).
NOT swept (recorded residuals): coordination offset/floor table in the prompt (engine cost params,
not classification bands); qualitative f(d)/χ direction-of-effect mentions; schema-validation error
messages outside c-orchestrator are unsanitized (harmless post-de-leak: the schema no longer
carries band values to echo). Known limitation (pre-existing): c-orchestrator `_step_generate`
resolves only `manifest["axes"]` — kernel-reading entries skip (witnessed twice); kernel topics go
through `generate_kernel_corpus.py`.

**Schema relocated (operator-ruled, same day): canonical schema now lives at
`schemas/constraint_story_schema.json`** (moved out of `python/`; the stale
`agent/data/constraint_story_schema.json` orphan — 158-line diff, loaded by nothing — deleted).
All loaders updated and witnessed (generate_constraint_pl `_load_schema` relative default,
regenerate_stories `SCHEMA_PATH`, story_generator_base, orchestrator, uke_narrative_orchestrator;
validate_constraint_story delegates to generate_constraint_pl); `DR_SCHEMA` env override
unchanged; assembled-payload band grep re-run post-move: still 0. Live docs updated
(`docs/technical/generation_path_resolution.md`, AGENTS.md Rule 3b, commitment_corpus/ROLLOUT.md,
apply_schema_patch docstring); archived papers/handoffs keep the old path (point-in-time
convention, audits/README).

## 2026-06-04 — OQ-71 depth-lineage probe: SCALE RUN COMPLETE — H1/H3 falsified beyond noise (boundedness is within-regime only)
**Files:** prolog/testsets/lineage_probe_01/, docs/design/a_hypothesis_about_corpus_size.md, ISSUES.md
**Tier:** correction-key

[Compressed 2026-07-05; full text in git history.] The 438-story depth-lineage arm minted
distinct 5-dim structural classes at ~1.5× the same-generator breadth control at every matched n.
**Citation discipline: falsifies UNCONDITIONAL boundedness only — depth was confounded with seed
authorship; do not cite as depth-specific discovery until OQ-71's authorship-controlled arm runs.**
Full record: ISSUES.md OQ-71 (partial); finding in `a_hypothesis_about_corpus_size.md` §10.

## 2026-06-04 — OQ-71 depth-lineage probe: machinery pilot (generator run-tag routing, fingerprint probe validated by exact reproduction)
**Files:** agent/generate_kernel_corpus.py, python/lineage_fingerprint_probe.py, audits/2026-06-04_oq71_depth_lineage/
**Tier:** tripwire

[Compressed 2026-07-05; warnings retained; full text in git history.]
- **The regression gate for the no-scope request path is REQUEST-PAYLOAD identity, not story
  bytes** (generation is stochastic) — stubbed-client capture harness in
  `audits/2026-06-04_oq71_depth_lineage/gate2_capture.py`; re-gate any edit the same way.
- **`validate_reading_relation_integrity` writes its quarantine to the FLAT path**
  (`prolog/testsets/cs_reading_relation_quarantine.json`) even on run-tagged dirs — a run-tagged
  sweep silently clobbers a flat-corpus quarantine.
- `python/lineage_fingerprint_probe.py` is a validated six-dim fingerprint dumper (reproduced the
  v5 dump exactly; salvaged originals + md5s in the audit dir / OQ-71).

## 2026-06-04 — Probe/loading infrastructure hardening (gotchas → utilities; two commits)
**Files:** prolog/corpus_loader.pl, prolog/cache_registry.pl, prolog/probe_harness.pl, prolog/check_stack.pl, prolog/json_report.pl, python/run_pipeline.py
**Tier:** tripwire

[Compressed 2026-07-05; most warnings promoted → CLAUDE.md Corpus Loading / Running the System;
full text in git history.] Commits `1460e873` (behavior-preserving) + `801390a5`
(output-affecting): cwd-independent corpus loading, `corpus_empty` throw, `corpus_constraint/1`
registry, `cache_registry:clear_all_caches/0`, `probe_harness` overlay utilities, manifest
single-writer convention. **check_stack BASELINE (cited by CLAUDE.md; UPDATED 2026-06-18,
engine-only): 3 undefined-predicate references** — `data_repair:constraint_beneficiary/2`
(:134, :174), `data_repair:constraint_victim/2` (:147), `validation_suite:test_case/4`
(test_harness.pl:26) — plus load warnings; findings beyond this list = regressions; each tracked
with a non-bite witness under OQ-142 (OQ-143/OQ-144 annotate-only). Not a pipeline gate while
the baseline is non-empty.

## 2026-06-04 — OQ-65 detector-bait census COMPLETE: bait=2 (no new), omega-routed=75, 6/10 firings expectation-authored
**Files:** python/audits/oq65_bait_census.py, audits/2026-06-04_oq65_bait_census/
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Per-file census (7 channels, 10-assertion
self-test, blind decoys): explicit_bait 2/1106 (no new — OQ-63 scope qualifier CLOSED);
omega_routed 75 (6.8%); expectation-authored union 87 (7.9%); 6/10 FSM firings
expectation-authored. Method corrections (truncating omega regex; dual-anchor windows) recorded
in the audit dir. Artifacts: `audits/2026-06-04_oq65_bait_census/`; memory
`project_oq65_bait_census`; OQ-65 mitigated.

## 2026-06-04 — Audit corpus consolidated into `audits/<YYYY-MM-DD>_<slug>/` (location mandate)
**Files:** audits/, python/audits/false_ci_rope_audit.py, python/audits/scaffold_piton_gate_audit.py, python/audits/bc_coupling_audit.py
**Tier:** tripwire

[Compressed 2026-07-05; mandate promoted → CLAUDE.md Audit Methodology; move map + conventions
→ `audits/README.md`; full text in git history.] 22 subdirectories consolidated from docs/,
root packages, and gitignored outputs/. Convention: `outputs/` = live workspace, `audits/` =
dated archive. Consumers of `outputs/bc_coupling_audit.json` need
`python/audits/bc_coupling_audit.py` run first on a fresh clone.

## 2026-06-04 — FNL prevalence is template-bait-confounded (OQ-70): mechanism witnessed end-to-end, counterfactual run
**Files:** prolog/signature_detection.pl, agent/verification_bottleneck.json, audits/2026-06-04_fnl_bait_confound/
**Tier:** tripwire

[Compressed 2026-07-05; warning promoted → CLAUDE.md Critical Distinctions (OQ-70 block, resolved
2026-06-05); full text in git history + ISSUES.md OQ-70.] The 827/1106 FNL era rode
`claimed_natural/2` source 2 reading ANY authored mountain perspective as a naturality claim
(counterfactual: retraction → FNL→FCR 809, zero mass to genuine NL/CI_rope). Probe evidence:
`audits/2026-06-04_fnl_bait_confound/`. The `catholic_church_1200` demo-exclusion rule is also
in Critical Distinctions.

## 2026-06-04 — sheaf_status provenance traced end-to-end; arakelov_threshold now emitted + cited
**Files:** prolog/json_report.pl, prolog/arakelov_height.pl, prolog/sheaf_analysis.pl, python/enhanced_report.py
**Tier:** tripwire

[Compressed 2026-07-05; the "don't patch sheaf_analysis piecemeal, OQ-51 moves consumers
together" warning is SUPERSEDED by OQ-51's resolution (2026-06-25, promoted → CLAUDE.md
Architecture Invariants h1_band block); full text in git history.] Chain coherent (H¹, heights,
sheaf_status on one site); `arakelov_threshold` (corpus p75) now emitted as
`diagnostic.arakelov_threshold` and cited by enhanced_report on height-dependent regimes;
witnessed against independent recompute.

## 2026-06-04 — Schema drift fixed: `sheaf_status` added to `PIPELINE_FIELDS` (schemas.py)
**Files:** python/shared/schemas.py, prolog/json_report.pl
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Producer-side emission (205a8187) had no
validator-side whitelist entry → 1107 warnings/run (Pattern 1 in miniature: additive-to-producer
requires same-change schema sync). Fixed + enum check; witnessed clean run + positive controls.

## 2026-06-04 — Engine/shadow split anatomy (debt-ceiling probe): confidence-0 is wiring-determined for victim-less FSM hosts; filed on OQ-65/OQ-66
**Files:** prolog/maxent_classifier.pl, prolog/config.pl, prolog/signature_detection.pl
**Tier:** correction-key

[Compressed 2026-07-05; full text in git history.] For every victim-less FSM host,
engine=tangled_rope vs shadow p(tangled_rope)≈0 is structurally guaranteed (the shadow's
tangled_rope requires `has_asymmetric_extraction` ← `constraint_victim/2`) — confidence≈0 is
WIRING, not per-item calibration evidence. Residual signal is the shadow's TOP type. Recorded
as OQ-65 evidence; debt-ceiling scope-out at OQ-66.

## 2026-06-04 — Tracking-surface consolidation: AGENDA.md, AUDIT.md, TODO.md deleted; ISSUES.md is the single tracker
**Files:** ISSUES.md, CLAUDE.md, AGENTS.md, README.md
**Tier:** landed

[Compressed 2026-07-05; rule promoted → CLAUDE.md End-of-Session (single-tracker, Pattern 2);
full text in git history, deleted files last at `a1140d0d`.] Item-by-item substrate review
before deletion; still-live items became OQ-67/68/69.

## 2026-06-04 — Ledger sweep: five trivial OQs closed (11, 12, 13, 24, 42)
**Files:** ISSUES.md, prolog/config.pl, prolog/drl_purity_network.pl
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Four were already done in substrate with
stale ledger entries; OQ-24 needed one comment. Lesson (kept): "open" in ISSUES.md is a claim
about the substrate that can go stale in BOTH directions — a closure sweep needs the same
witness discipline as a fix.

## 2026-06-04 — OQ-57 RESOLVED: drift_events.pl:230 wrong-module qualifier (one-token fix, land-alone)
**Files:** prolog/drift_events.pl, prolog/json_report.pl
**Tier:** tripwire

[Compressed 2026-07-05; mechanism promoted → CLAUDE.md (wrong-qualifier/load-path) +
`swipl_load_path_and_probe_gotchas.md`; full story in ISSUES.md OQ-57.] Wrong-qualifier bugs
can throw in the REPL and silently work in the pipeline (json_report.pl is a non-module file;
imports land in `user`) — diagnose on the consumer's exact load path.

## 2026-06-04 — OQ-63 diagnostic run: directionality's beneficiary read measured (read-only; no engine change)
**Files:** prolog/constraint_indexing.pl, ISSUES.md
**Tier:** correction-key

[Compressed 2026-07-05; full evidence in ISSUES.md OQ-63; morphology rule promoted → CLAUDE.md
Cross-Sibling Disambiguation + OQ-64.] The d→χ mis-derivation on proposition-kind beneficiary
values is REAL but χ-immaterial today (all |Δχ| ≤ 0.022, zero band crossings; suffix-probe
population known to undercount). Escalation ruled AGENT on in-file witness; OQ-63 →
"consumer working correctly."

## 2026-06-03 — FSM agency gate: agent_beneficiary/2 two-site narrowing (maxwell un-stripped; one-row manifest diff, derived then confirmed)
**Files:** prolog/narrative_ontology.pl, prolog/signature_detection.pl, prolog/tests/test_agent_beneficiary.pl
**Tier:** tripwire

[Compressed 2026-07-05; the TWO-GATE non-agent-registry principle is documented at the registry
itself (narrative_ontology.pl) and in memory `project_fsm_agency_gate`; full text in git
history.] `non_agent_beneficiary/1` registry (2 ruled entries; unlisted = agent, fail-open) +
`agent_beneficiary/2`; FSM gate + `count_power_beneficiaries/2` narrowed. Manifest diff exactly
1 row (maxwell → mountain×4/natural_law), derived pre-write then confirmed. Guard tests in
`prolog/tests/test_agent_beneficiary.pl` incl. the :287 inertness tripwire (fails loudly when
the OQ-66 deferral goes stale). Gotcha (kept): `setup_call_cleanup/3` defers cleanup while the
goal holds choicepoints — wrap the goal in `once/1`.

## 2026-06-03 — Purity audit: structural_purity/2 was dead (bound-probe bug, now fixed); correction key for purity readings
**Files:** prolog/signature_detection.pl, prolog/boltzmann_compliance.pl, prolog/purity_scoring.pl, docs/logic_extensions.md
**Tier:** tripwire

[Compressed 2026-07-05; the bound-arg warning is a comment at the `epistemic_access_check/2`
definition (boltzmann_compliance.pl) — promoted to substrate; full audit
`audits/2026-06-03_purity/`.] `structural_purity/2` returned `inconclusive` unconditionally for
its whole life (bound-probe `epistemic_access_check(C, false)` satisfied by the catch-all);
fixed to unbound + `Access == false`; post-fix 96.6% contaminated, 0 scalar scores moved.
Open findings: OQ-60 (absence-reward), OQ-61 (purity restates type composition), OQ-62 (band
vocabulary fork — do not auto-unify).

## 2026-06-03 — never-generated kernels generated (300/304); corpus 803→1103
**Files:** agent/generate_kernel_corpus.py, agent/build_never_generated_seeds.py, prolog/validation_suite.pl
**Tier:** tripwire

[Compressed 2026-07-05; both warnings live in the memory index
(`reference_no_scope_skips_integrity_sweep`, `reference_validation_suite_autogenerated`);
corpus superseded by the 2026-06-05 reset; full text in git history.] The ~102 never-generated
kernels were naming drift, not missing content; generated per the sibling-kernels-are-distinct
ruling (commit `64cc249a`). Warnings (kept): **no-scope mode does NOT run the OQ-58
reading-relation integrity sweep** — run it manually after any no-scope batch; **a modified
`validation_suite.pl` after a pipeline run is expected regeneration, not a hand edit.**

## 2026-06-03 — `reading_diff.pl`: the cyclopean disparity operator (OQ-59 disposition)
**Files:** prolog/reading_diff.pl, prolog/axiom_diff.pl, prolog/stack.pl, prolog/reading_diff_census.pl
**Tier:** tripwire

[Compressed 2026-07-05; invariants carried in memory `project_reading_diff_operator` +
`feedback_verdict_omits_seat` and in-module docs; census pre-reset; full text in git history.]
OQ-59 ruled preserve-and-diff, not merge. Invariants (kept): authored-cells-only (never the
computed export); regime is pair × key with an order-independent stability verdict; counts over
vantage-groups, not pairs; `weighted` keys throw on `reading_diff/6`. Census (pre-reset corpus):
53.7% key_fragile. Axiom layer: 0/935 reading-pairs share an axiom NAME — `exact_name`
structurally all-blind; don't compare `cs_axiom_status` across readings; don't bake
`axiom_concept` (superseded 2026-07-02: OQ-72 baked a RATIFIED registry —
`axiom_concept_registry.pl`, see AGENTS.md Rule 3c). westphalia/westphalian are distinct
sibling kernels, not a spelling dup.

## 2026-06-02 — Reading-reference linter + the "complete kernels, not patch edges" finding
**Files:** python/audits/reading_reference_linter.py
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] Linter (reporter, 3 rules, each with a
synthetic positive control): the dangling-edge problem was kernel-COMPLETENESS (119 missing
readings across 69 kernels), not edge-patching → OQ-58. `affects_constraint` targets may be
abstract nodes — its "dangling" refs are NOT an integrity signal. R3 over-flags by design.

## 2026-06-02 — Reading-axis structural obstruction built + cs_reading_relation name-form repair
**Files:** prolog/cs_kernel_registry.pl, agent/generate_constraint_pl.py, agent/generate_kernel_corpus.py, prolog/cs_corpus_analysis.pl, prolog/json_report.pl
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] Built `cs_kernel_obstruction/4` (committer
H¹ analog, observer-blind, fail-closed on untyped pairs). Repaired 86 short-form
`cs_reading_relation` targets → canonical `<kernel>__<short>` across 47 files (predicted-delta
control passed exactly); generator now canonicalizes at emit (`generate_constraint_pl.py:482`)
+ hard-fail integrity check with quarantine (OQ-58 policy: attach-or-quarantine, NO auto-rewrite
tier, NO plausible-form tier). **Tripwire (kept): consumers stay EXACT-MATCH; do NOT add a
read-time short→full resolver — it re-hides the defect.**

## 2026-06-02 — Coupling liveness profile wired into per-constraint JSON (seat structure, not just verdict)
**Files:** prolog/boltzmann_compliance.pl, prolog/json_report.pl, python/query.py, python/enhanced_report.py
**Tier:** landed

[Compressed 2026-07-05; full text in git history.] `coupling.scope_violations` /
`power_violations` / `live_index` now emitted per constraint (violation logic single-sourced in
`coupling_violation_components/5`); score path byte-identical pre/post (773/773). Framing:
`live_index=none` is Mountain-consistent (seat-free verdict), not a pathology flag.

## 2026-06-02 — Toy corpus finished 769/770; generator repair + 3 robustness fixes
**Files:** agent/generate_kernel_corpus.py, python/story_repair.py, prompts/constraint_story_generation_prompt_json.md
**Tier:** tripwire

[Compressed 2026-07-05; toy corpus superseded by the 2026-06-05 reset; generator fixes live on;
full text in git history.] Fixes: `overwrite=True` in the no-scope path (**tripwire, kept: do
not revert to skip-on-exists — the ladder `beta_processed.txt` is the idempotence source, and
`json/` held a stale pre-rebuild corpus**); `poll_batch` transient-error retry; plain-seed
summaries capped ≤500; `python/story_repair.py` canonical deterministic repair (never touches
conditional `allOf/then` bounds — clamping would fabricate).

## 2026-06-02 — `sheaf_status` now persisted (W1×sheaf join built); orbit provenance is a sidecar
**Files:** prolog/json_report.pl, python/run_pipeline.py, python/w1_sheaf_join.py, prolog/sheaf_analysis.pl
**Tier:** tripwire

[Compressed 2026-07-05; the sidecar rule is GAP-03 in `design_gaps.md` (promoted) and the
freshness rule is Build Discipline Pattern 1; full text in git history.] `sheaf_status` emitted
per constraint; `orbit_data.manifest.json` sidecar asserts same-run. Warnings (kept):
**`orbit_data.json` provenance lives in the SIDECAR — do not inject a `"manifest"` key in-file**
(7 consumers iterate it with bare `.items()`); **`sheaf_status` recomputed on a bare `[stack]`
(no maxent run) is VACUOUS** — heights degenerate, fragile count reads 0.

## 2026-06-02 — Dirac Axis-1 (`derived_from/3`) removed → design gap; `gauge_fixed/3` straggler fixed
**Files:** prolog/dirac_classification.pl, docs/design/design_gaps.md
**Tier:** tripwire

[Compressed 2026-07-05; warning promoted → GAP-01 in `design_gaps.md` (cited by CLAUDE.md Design
intent); full text in git history.] `derived_from/3` had zero producers corpus-wide (Pattern 5:
every constraint read `primary` via the `\+` cut) — removed; do NOT re-add unfed.
`gauge_fixed/3` straggler migrated off deleted `standard_context/1`. `full_dirac_report/3`
still has no consumers (candidate for the same treatment).

## 2026-06-02 — False-summit forensic detector repaired (was vacuous) + two report bugs + stale comment
**Files:** prolog/drl_core.pl, prolog/report_generator.pl, prolog/drl_composition.pl
**Tier:** tripwire

[Compressed 2026-07-05; the dr_claim_mismatch region now carries the OQ-128 severity split +
in-code rationale (drl_core.pl:625-644); follow-ups OQ-50; full text in git history.]
`dr_claim_mismatch(_,_,type_1_false_summit,_)` had NEVER functioned (`is_mountain(C,Ctx,fail)`
satisfied by the unconditional catch-all clause; the report queried a nonexistent atom on top —
doubly dormant, Pattern 5). Fixed: negate post-signature `dr_type/3`, enumerate contexts, no
cut. **Warnings (kept): do not "simplify" back to `is_mountain` (pre-signature — flags genuine
mountains at mid-power contexts) and do not re-add the cut.** Sibling type_3/type_5 clauses
silently no-op on unbound Context (OQ-50).

## 2026-06-02 — Removed superseded observer-axis husk (saturation_floor) — commit ef92a61d
**Files:** prolog/drl_composition.pl, python/enrich_pipeline_json.py, python/enhanced_report.py, python/run_pipeline.py, python/shared/schemas.py
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] **If you are looking for `--- HUSK
SIGNATURE ---` / `saturation_floor` / `husk_metrics`: deleted deliberately (commit `ef92a61d`),
do not re-add.** Two husks existed; only the observer-axis draft (zero engine consumers,
superseded within 4h of landing) was removed — **the CS husk (`cs_terminal_attractor(...,
husk)` + 9 consumers) is live and design-endorsed; the §5.11 "husk 57" count is that one.**
The real underlying finding (static ε understates series peak, 70/499 one-sided) was never
opened as an OQ.

## 2026-06-01 — Corpus rebuild pipeline built + validated on N=1 (decompose → no-scope gen)
**Files:** agent/generate_kernel_corpus.py, python/merge_kernels.py, python/partition_probe.py
**Tier:** tripwire

[Compressed 2026-07-05; modes + recipes promoted → `docs/technical/bulk_corpus_generation.md`;
full text in git history.] Three CLI modes (no-scope default / --decompose / --scope);
collision-proof naming; 3× retry. **Warnings (kept): `story_uid` is ALWAYS overwritten with a
fresh uuid4 (`generate_kernel_corpus.py` — do NOT revert to `setdefault`; Haiku copies the
example's placeholder and duplicates halt the corpus);** reading ids >64 chars are skipped
fail-loud (batch custom_id limit). Probe: the v5 archive is observer-axis (0 committer kernels
/ 99, 74% positive control) — kernels come from authored kernel files, archive supplies plain
seeds.

## 2026-06-01 — Corpus rebuild Phase 0: old corpora archived, `testsets/` emptied
**Files:** prolog/testsets/, prolog/archives/, python/sweeps/range_sweep.py
**Tier:** tripwire

[Compressed 2026-07-05; superseded by the archive map in CLAUDE.md Critical Distinctions (the
archives have since moved to `prolog/archives/datasets/<name>/`); full text in git history.]
Start of the rebuild: old corpora archived (v5 = 3,380, v6 = 229), fresh empty `testsets/`;
4 hardcoded `testsets_3000` overlays retargeted with a positive control.

## 2026-06-01 — `signature_detection.pl`: honest `unknown` now SURFACES (override removed, OQ-37)
**Files:** prolog/signature_detection.pl, python/sweeps/regenerate_orbits.py, python/enhanced_report.py
**Tier:** tripwire

[Compressed 2026-07-05; warning carried in memory index (`project_oq37_unknown_surfaces`); full
text in git history.] Commit `c90c5482`: FNL/FCR overrides no longer launder honest `unknown`
into tangled_rope (guards at :738 and :685). **Warnings (kept): do NOT reinstate "never
preserve unknown" — removed by ruling; `unknown` surfacing is load-bearing for OQ-37.** Also:
perturb.py's staleness guard checks only the testsets hash, NOT engine state — after an engine
edit that changes classifications, regenerate `product_site_orbits.json` manually or
stability-band comparisons silently read a stale baseline. `coordination_type_offset` is
per-constraint, not perturb-sweepable — keep it out of `_WITNESSED_PARAMS`.

## 2026-05-31 — Surface-2 primitive built; lock hypothesis witnessed (lever was misnamed)
**Files:** python/sweeps/surface2_lock_sweep.py, prolog/boltzmann_compliance.pl, prolog/signature_detection.pl
**Tier:** correction-key

[Compressed 2026-07-05; carried in memory `project_surface2_lock_primitive`; full text in git
history.] The handoff/OQ-30 lock lever was wrong: `boltzmann_floor_*` moves excess but not the
lock; the gate is `boltzmann_compliant` via `boltzmann_coupling_threshold` (+
`coordination_type_offset`), which flips 48/56 load-bearing locked readings (floor flips 5/96).
Floor hypothesis FALSIFIED, coupling-threshold WITNESSED. Row-level witness tier:
structure-closed + regenerable from `outputs/surface2_lock_sweep_results.json` (`db66cc53`),
not pasted.

## 2026-05-31 — Commit A: row-23 fail-close in `drl_composition.pl` `classify_at_time` (OQ-41)
**Files:** prolog/drl_composition.pl
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] `classify_at_time/4` no longer fabricates
`Supp=0.5`: temporal series → authored scalar → `unknown` (scalar fallback, not literal
`unknown` — 650/650 no-series rows carried an authored scalar; returning `unknown` would
discard authored data). 268 rows corrected. **Warning (kept): the scalar clause is a labeled
STOPGAP — retired by OQ-46 (temporal series authoring), gated behind OQ-47; do not harden or
build an equivalence check on it.** Downstream audit: one live consumer
(`cs_kernel_divergence`), persisted counts invariant; the per-context divergence set grew +642
(real divergence the fabricated 0.5 homogenized).

## 2026-05-31 — Commit B LANDED (behavior-preserving batch behind Commit A)
**Files:** prolog/signature_detection.pl, prolog/constraint_bridge.pl, python/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md
**Tier:** tripwire

[Compressed 2026-07-05; full text in git history.] B1 NL-gate fail-close (reads authored
`constraint_beneficiary`, not the empty `intent_power_change` join; live NL certs 5→2 — a
correct decline, not a regression); B2–B4 dead-clause/schema strips. Deferred with reasons
(kept): **`psych_bridge` is a dead UNLOADED module — remove/revive deliberately, don't strip
its reads (OQ-38 family); `resistance_to_change` is NOT a free strip (live report paths);
`python/constraint_story_schema.json` (canonical) vs `agent/data/constraint_story_schema.json`
is an unreconciled Pattern-2 fork** — B4 edited only the canonical one.

## 2026-05-31 — Legacy bullets imported from CLAUDE.md (2026-05-28 → 2026-05-31 items)
**Files:** prolog/product_site_export.pl, prolog/config_validation.pl, python/sweeps/perturb.py, python/sweeps/demotion_pass.py, python/enhanced_report.py, agent/generate_kernel_corpus.py, prolog/signature_detection.pl, prolog/drl_composition.pl
**Tier:** history

[Compressed 2026-07-05; full text in git history.] Verbatim import of the CLAUDE.md Known State
section at the 2026-05-31 split. Everything here has a later home: the LCO-critical cut →
CLAUDE.md Architecture Invariants + OQ-02; the OQ-25 ε-coherence load guard →
`config_validation_wiring.md`; kernel-linkage join → memory `project_kernel_linkage_join` +
bulk runbook; perturb()/stability-band/191-param sweep → memory (`project_sweep_primitive`,
`project_stability_band`, `project_perturbable_parameter_surface`) + OQ-29/OQ-30; bound-probe
Pattern 3 → `build_discipline.md` + `signature_detection_wiring.md`; OQ-43 satisfy-on-absence →
CLAUDE.md Build Discipline #5; the NL-gate "diagnostic-decline ≠ classification-changing"
correction and the "demotion_pass.py is engine-blind — route verification through perturb.py,
not its static buckets" caveat live in git history and OQ-30/OQ-33.
