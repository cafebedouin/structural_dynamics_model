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

## 2026-08-06 — [correction-key] OQ-264 RESOLVED (standard-only): per-reading redraw stability is file-structure-dependent (0.33–1.00); pooled share does NOT repair churn (denominator artifact); k=3-unanimous presence standard minted

**Files:** ISSUES.md, CLAUDE.md, audits/2026-08-06_oq264_kredraw_variance/, audits/2026-08-03_kritik_ingest/WRITEUP.md, python/audits/oq264_idiom_share.py
**Tier:** correction-key

Full record `audits/2026-08-06_oq264_kredraw_variance/WRITEUP.md`. Pre-registered
(PROPOSAL `fd58d3a1` before scoring; calls `0a28d7ca` before mapping `e4c293d4`) blinded
pooled scoring of the six free kritik manifests, then an operator-review correction pass
(`241ec42d`/`13999d9c`), then the sole spend: AT Fiat k=3 same-input redraws
(`ac2650ae`/`b418b632`, ≈101K tok, corpus untouched every run). **How results may be
cited:** (1) the share gate's PASS(sens1) is on record but SUPERSEDED in meaning — its
entire 0.25 range fell between numerator-identical draws (TAG=3/6 vs 3/4), i.e.
denominator churn at fixed judgment, with the perverse direction fewer-readings→higher-
share; cite the specification finding, not the pass. (2) Reproduce-rates now span
2/6–3/6 (Cap 340K arsenal), 4/6–5/6 (Biopower 103K), **6/6 ×3 (AT Fiat 34K single-voice
— the 2026-08-05 "no Arm-0 measurement" rider closed, expectation inverted)**: no global
churn floor exists; never cite one number as "the" floor. (3) Presence claims need k=3
unanimous same-input redraws (1–2/3 = observation); names are never identity (kernel ids
churned at reproduce-rate 1.0). (4) 0/n control agreement is a binomial bound (0/6 →
95% UB 0.393), never "zero variance." (5) Plan-rule recalibration precedent: the rev-1
sensitivity modifier was rejected in Phase A by its own quantization simulation
(CALIBRATION.txt witnesses both rules) — gate rules must pass the stable-null
simulation before numbers commit. Propagated: OQ-259 items 2–3 unblocked (item 3 k=3
concrete; AT Fiat cannot serve it), Amendment 6 on the 2026-08-03 WRITEUP (plan said
"Amendment 5"; one already existed), CLAUDE.md Generation-is-stochastic block updated.

---

## 2026-08-06 — [landed] Monthly consolidation pass (2026-08): KNOWN_STATE roll-off 129 entries compressed; residues declared

**Files:** KNOWN_STATE.md, CLAUDE.md, ISSUES.md
**Tier:** landed

Roll-off: 129 landed/history entries in the 2026-06-05..2026-07-06 due window compressed
in place (7,906→5,310 lines, `4e0efd5b`; headers/Files/Tier verbatim, pointers kept;
checker 261/0 green; 51 tripwire/correction-key entries in window deliberately left —
they need the promotion test, next pass). ISSUES compress-on-close CHECK ran: ~130
closed entries >14 lines (worst OQ-138/153/62/219); bulk compression deferred with the
still-operative-ruling exemption noted. Memory dir verified consistent (103 files, 0
orphans, no merge candidates). CLAUDE.md: dates advanced (next pass ≥2026-09-06),
residues declared in the review section, OQ-264 churn-floor tripwire promoted into the
"Generation is stochastic" block (promotion-test hit from the 2026-08-05 finding).

---

## 2026-08-05 — [correction-key] OQ-259 item 1: emphasis discriminator HALTED by its pre-registered Arm-0 gate — per-reading presence is NOT redraw-stable within a file; "replicate-stable (f)" is cross-file only

**Files:** python/audits/emphasis_extract.py, audits/2026-08-05_oq259_emphasis_discriminator/, agent/analysis/originals/k_files/, audits/2026-08-03_kritik_ingest/SCORING.md
**Tier:** correction-key

Full apparatus built and witnessed before the gate: pre-registration addendum
`c4785da7` (per-file thresholds; quantified HALT reproduce-rate < 2/3; Arm-2
selection rule + seed 259; commit-order blinding protocol) committed BEFORE any run;
`python/audits/emphasis_extract.py` (raw-string docx splice — ET re-serialization
breaks pandoc image extraction; byte-copy rezip; scramble mode) + three
emphasis-aware conversions committed `b8a44661`, strip-restore BYTE-EXACT vs the
`1bd57a84` baselines, marker pairs exactly as predicted (AT Fiat 208/175, Biopower
829/1014, Cap 2535/2589). Arm 0 (same-input re-runs ×2 per payload file, inputs
md5-pinned, `70c458f9`): **Cap K reproduce-rate 3/6 then 2/6 — HALT fired (< 2/3 in
both; the second Cap re-run minted NO contested kernel at all); Biopower passed 4/6 +
5/6. Read-through churn control: 3 of the 4 discriminator-target readings churned at
byte-identical input** (world_system, growth_process, coalition_governmentality;
only the insurance deferred axis stable 2/2) → Cap K P1 effective n=0, Biopower n=1.
Arm 1/Arm 2 NOT run; ~884K input tok spent (4 of ≤8 calls); corpus untouched every
run (listing-diff witness). **Citation consequences:** (1) the 2026-08-03 "(f)
partial recovery, replicate-stable" verdict is CROSS-FILE stability — do NOT cite it
as within-file redraw stability of any individual reading; (2) SCORING.md's one
CONTESTABLE hit (`reformist_iatrogenic`) churned 0/2 — treat that baseline row as
draw-fragile; (3) any future per-reading-presence design on ~100K–340K-token inputs
needs a churn arm sized to the effect or a churn-robust observable (idiom SHARE over
pooled redraws). **Ruling landed same day (option 1, extended): item 1 CLOSED — the
Arm-0 churn floor IS the finding; options 2/3 ruled dead (n=1 anecdote / instrument
failure).** Sampling check witnessed with the ruling: Sonnet-5 decompose runs with
temperature OMITTED by design (`llm_call.py:112`; 400 on non-default) and the API has
no seed — churn is the production regime's own magnitude, not an unpinned knob.
Program-wide propagation minted as **OQ-264** (single-draw per-reading findings carry
unquantified error bars; k-redraw variance-floor standard; fewer-files × more-draws;
Cap K out of scope for per-reading measurement); OQ-259 items 2–3 blocked_on OQ-264;
item 3's genre-flag standard restated to appearance-across-k-redraws. Extractor +
conversions stay ready for an OQ-264-compliant redesign.

---

## 2026-08-04 — [correction-key] OQ-258 discriminator: referent ambiguity did NOT own the channel-legibility finding; reader-position survives; ε referent now fixed in the contract

**Files:** prompts/constraint_story_generation_prompt_json.md, prompts/constraint_story_generation_prompt.md, schemas/constraint_story_schema.json, audits/2026-08-03_oq258_referent_discriminator/, audits/2026-07-27_cross_author_epsilon_probe/RESULTS_legibility_coding.md
**Tier:** correction-key

Null+fix discriminator over the 18 top-spread tacit/none_apparent items × 4 legs
(pre-registered `74e74e35` BEFORE spend; contract fix `685ed7cf`). Old-contract redraw (Arm B)
mean spread 0.4633 vs referent-fixed (Arm A) 0.5167, Wilcoxon p=0.328 — pinned row 3:
**A ≈ B with B elevated. The 2026-07-27 channel-legibility finding may now be cited WITHOUT
the OQ-258 degeneracy caveat, and the channel-conditional reliability caveat (2026-07-27
entry) HARDENS from "artifact-or-real, undecided" to "survives its first real test."**
The phenomenon replicates on redraw (B retains ~77% of baseline elevation, no
regression-to-mean collapse). Named witness failed informatively: haiku re-authored ε=0.00
on `animal_status__abolitionist_reading` under the fixed referent, rationale explicitly
refusing the quantitative question; kimi FLIPPED 0.82→0.02 under the fix, scoring the
reading-as-constraint itself ("the constraint itself blocks rather than extracts") —
the live ambiguity is CONSTRAINT IDENTITY in kernel-reading stories (standing arrangement
vs reading-as-constraint), upstream of the (a)/(b) referent choice OQ-258 posited; tracked
as OQ-263 (three-valued declared-referent-field fallback). The referent-(b) contract fix
STANDS regardless (ruling fixes the rebuild's contract): kernel-reading ε names the standing
arrangement under contest, assessed by the reading's own lights (OQ-26 untouched). Do not
expect prompt-side referent language ALONE to force cross-author ε agreement — witnessed
insufficient. Kimi Moonshot batch stalled 0/18 ~8h → declared sync fallback used (identical
sampling params). OQ-258 resolved; evidence archived under the audit dir (`generated/`),
baseline legs md5-unchanged throughout.

---

## 2026-08-03 — [landed] Kritik ingest probe: arsenal-format K-files score (f) partial recovery replicate-stable; AT Fiat K graduated (7 stories, n=225)

**Files:** audits/2026-08-03_kritik_ingest/, agent/analysis/originals/k_files/, agent/c-orchestrator.py, agent/decompose_manifests/flat/, prolog/testsets/
**Tier:** landed

Pre-registered probe (`PROPOSAL.md` committed `1bd57a84` before any run) on whether SCOPE
recovers coherent structure from debate-camp card files. Three dry-runs + fresh emotives
control, ALL `--skip-search` (Cap K NW measured 339,501 tok — largest witnessed SCOPE
ingest — vs the ~187.9k research cap; uniform flags required). Verdict per the
pre-registered rule: **both arsenal replicates (f) partial recovery, replicate-stable** —
coherent, precision/recall pass (b)'s bars, idiom MIXED (SCOPE scaffolds subjects/stances
on the editorial block layer but populates readings from the card literature, minting
definitional-contest kernels plus ~2 pure read-through readings per file no block names).
Emphasis ruling (A): all claims are properties of emphasis-blind ingestion (pandoc drops
highlight/font-size = the read/unread layer; bold survives), never of the format —
extractor re-run is the named discriminator (OQ-259). **[CORRECTED same-day, operator
review]** AT Fiat K produced a 6-reading grounds-contest kernel; the first draft of this
entry read that as "single-voice did NOT flat-route" against the KNOWN_STATE 2026-06-08
under-routing tripwire — wrong premise: the file is single-STANCE but multi-voice (six
attributed authors), so the single-voice tripwire was never applicable and no-flat-route
is expected behavior (no tripwire-regression reading is licensed; WRITEUP.md Correction 2).
The confound also splits directionally (WRITEUP Correction 1): the scaffold half
(precision despite 10× tag-layer dilution) is a-fortiori STRONGER than stated; the
read-through half is confound-predicted and is what OQ-259 can kill. Principal finding
(WRITEUP Amendment 4, INFERRED from the scoring tables, symmetric across both
replicates): **SCOPE's reading granularity sits at the theoretical position** —
position-staking sections surface as readings regardless of side; Link/Impact machinery
absorbs into the parent reading's `expected_structural_delta` (present one level down,
not lost). Every strict recall miss was this altitude conflation in the predicted
denominator, so read the (f) label with the WRITEUP's verdict qualifier (nearer (b) than
the bare label; classification unchanged — the pre-registered rule stands). Granularity
prediction registered on OQ-259 (machinery stays absorbed under emphasis-aware
ingestion), independent of the tag-idiom prediction. Phase 3: AT Fiat K
full run on the frozen manifest →
7 stories (`69db90a1`), pipeline 49/49, n_constraints 217→225 (+1 untracked
`fiat_efficacy_kernel_contradictions.pl`, standing convention), ε-referent uniform over
the defended practice (weak OQ-258 evidence — wrong specimen class, logged on the OQ).
Side-finding OQ-260: `_step_commit` manifest staging mislabels relative in-repo
`--manifest-file` paths as "outside repo" and silently skips staging
(c-orchestrator.py:965). Scoring + witnesses: `SCORING.md`/`WRITEUP.md` in the audit dir.

## 2026-07-27 — [landed] Cross-author ε probe: 4-leg divergence sorts by channel legibility, not topic heat; twins never ε-harmonized; haiku exact-0.00 is authored

**Files:** audits/2026-07-27_cross_author_epsilon_probe/, prolog/testsets_haiku/, prolog/testsets_flash/, prolog/testsets_kimi/, prolog/testsets_sonnet/
**Tier:** landed

Commits `90de6e91` (probe) / `bcba5d4d`+`be76062c` (pre-registration, committed BEFORE coding).
Part 1: over the 957 readings shared by all four twin legs, ε was never harmonized at
reconciliation (haiku~flash 3.9% identical, mean |Δε| 0.105); author-level means differ
systematically (kimi 0.589 > haiku 0.565 > flash 0.508 > sonnet 0.490); only 1.0% of readings
carry identical ε 4-ways. Exact-0.00 ε is an authored value, not a null (haiku 14/960 with
in-file justification; sonnet 0). Part 2 (blind coding, 7 subagents, seed-pool substrate,
controls 7/8): top-vs-bottom spread deciles sort by channel legibility (Fisher p=0.023,
predicted direction), NOT topic heat (p=0.27, reversed); biggest post hoc asymmetry is
referent-weak items (`none_apparent` 8 vs 1). Instrument note: per-story ε on tacit or
referent-weak constraints is low-reliability across authors — treat cross-leg ε agreement as
channel-conditional. Establishes variance, NOT directional bias (needs a non-LLM reference
leg; operator ruled 2026-07-27 no human ε leg — reader-profile plan steps 2–3 dead unless
revived). MECHANISM is degenerate (reader-position vs ε-referent ambiguity in the generation
contract) — see **OQ-258** (witnessed contract defect + pre-specified discriminator); do not
cite the channel sort as reader-profile evidence until it discriminates. Full method,
caveats, per-item codes: the audit dir.

---

## 2026-07-25 — [landed] OQ-216 stage-2 contract guard redesigned: header-proxy → content-level; floating_city false-negative corrected

**Files:** agent/uke_narrative_orchestrator.py, python/tests/test_stage2_contract_extraction.py, ISSUES.md
**Tier:** landed

Commit `4878df78`; evidence `audits/2026-07-25_oq216_contract_extractor_redesign/`; full
correction block on ISSUES OQ-216. The 2026-07-13 guard checked header-string-at-position — a
proxy that failed both directions: blocked four drifted-but-complete Sonnet-5 stage-2 outputs
(prometheus ×2, quellcrist, ergodocity), and on `the_floating_city_xixi_1784000706` passed while
over-capturing to EOF — stages 9/10 consumed an 18,266-byte blob as "the contract" and the story
shipped (run-dir `invariant_contract_output.md` is the witness). Redesign
(`_extract_invariant_contract_checked`): canonical + drifted headings accepted; bound at next
same-or-higher heading; EOF-termination always fails (SECTION 0 mandated first); negative
(no SECTION 1/2/OMEGA LOG) + positive (invariant/falsifier/substrate/inhabitation,
token-censused over 13 good blocks; `break` deliberately excluded) content assertions.
Witnesses: 7/7 fixtures; rotation_seven old==new byte-identical; floating_city 18,049→1,693ch;
prometheus 0→2,159ch; fresh-draw set 3/4 pass (the fail is the no-block shape where fail-loud is
correct). Standing: drift is Sonnet-5-endemic on floor-primary sources (3/5 prometheus draws);
OQ-219 clause amplifies, does not cause (ergodocity predates it) — extractor+content-guard is
the load-bearing layer, prompt fixes are hygiene.

## 2026-07-25 — [landed] OQ-254 RESOLVED: Q-provenance wired (join key, self-stamp, tracked manifests, standing readout); headline corrected on close

**Files:** agent/c-orchestrator.py, agent/generate_kernel_corpus.py, python/generate_constraint_pl.py, schemas/constraint_story_schema.json, python/run_pipeline.py, python/q_provenance_readout.py, agent/decompose_manifests/, ISSUES.md
**Tier:** landed

Audit `audits/2026-07-25_oq254_q_provenance/`; commits `01d503aa`/`f1436bd4`/`2d7432a0`/
`7f29bfea`/`c200fcd2`. The OQ's headline was FALSE as written (marked on the entry, second
v8-prose inference corrected by code contact): the Q-choice was richly declared
(selection_reason 2596/2598 axes, deferral_reason 1022/1022, kernel verdict 486/515 over the
515-manifest census) but unreachable — gitignored, unstamped, unjoined (Pattern 6). Landed:
(1) `generation_run_id` = manifest filename stem, minted at decompose, threaded through all
THREE scope-manifest write paths (c-orch `_persist_manifest`, gkc batch decompose, legacy
`--scope`) into story provenance → `epsilon_provenance/5` arg 4 (schema-optional field —
never required; `'none'` = declared pre-wiring stratum, NEVER a defect and never backfilled);
(2) manifest `_provenance` self-stamp; (3) manifests now write to tracked
`agent/decompose_manifests/<run_tag|flat|decompose>/`, the 515 pre-existing ones archived at
`archive_pre_2026-07-25/` (archive-not-read-surface; readout token
`joined_archive_not_authoritative`, never `joined`/`unreachable`); (4) standing
`q_provenance_readout.py` + run_pipeline Phase 9d (planted two-sided controls every run;
behavior-preservation witnessed: per_constraint byte-equal over md5-frozen corpus). Close
records WIRED not JOINED: all 205 live stories `no_run_id_authored`; e2e join graduates at
the next operator topic run. `_step_commit` extension landed after operator review, amended:
manifest stages only on join-key match (filename stem == every committed story's
`generation_run_id`); all non-staged outcomes recorded in `StepResult.data`. Follow-up
minted: OQ-256 (§3 foreclosure as structure; waits on first exercise of OQ-255's
hand-enumerated branch).

---

## 2026-07-25 — [landed] Seat-theorem v2.5 ADOPTED (OQ-253 ruled): Q enters the formalism; interrogative type exemption struck at all three sites
**Files:** docs/seat-theorem-v1.md, docs/deferential_realism_paper_v8.md, docs/the-few-seats-worth-choosing-v2.md, ISSUES.md
**Tier:** landed

Operator ruling (option 1 + two riders), commit `fdc502ec`. seat-theorem v2.4→v2.5: §1(3)
signature V = 𝔙_Q (completed to 𝔙_{Q,Π} in §8, which now states the chain Q → Π → σ →
liveness); §7 seat is the pair (Q, Π); §6.2 type exemption replaced by graded seat-cost
(foreclosure-set, enumerable) with the guard requirement promoted to the v1.1 gate — carried
WITH its scope inline as a **standing probe** (rider 1: the gate encodes the claim the
discriminator tests; one run, engine tokens, n=199 — a future discriminator run that lands the
other way weakens the gate back toward admission). v8 §6.3 tracks the priced form. The essay's
site-3 sentence ("commits to nothing the world could refute" — its strongest form) was REWRITTEN
with an authored replacement paragraph + fourth-pass note (rider 2), not struck. Sweep witness:
`grep "commits to nothing"` over docs hits only the two strike-records quoting the dead claim.
Tripwire for future doc cites: **the interrogative exemption is dead law** — do not quote
"a question commits to nothing..." from memory of pre-v2.5 text; questions price low, not zero.
Ruling record: ISSUES OQ-253 (resolved, compressed).

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
   (`:440-448`). Fails closed (dormant, not defective). **Same-day correction (audit §8): do NOT
   read this as "the engine has no genealogical/drift channel."** The live genealogy surfaces
   are `founding_problem_status/2` (authored 164/199 live-leg: 89 contested/14 dead/61 live) and
   `disappearance_verdict/2` (165/199), with the drift conjunction wired at
   `narrative_ontology.pl:168-170` (`has_mandatrophy_declaration ← fps(dead) ∧
   dv(world_rearranges)`). A future probe hunting "drift verdict" via `coordination_vitality`
   alone repeats this session's channel misidentification — check fps/dv first (two-axis lesson:
   check both surfaces before "engine has no X layer").

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

OQ-213(a) RESOLVED: `twin_comparison.py` graduated to N-general (all-pairs guards, per-pair salted RNG, `analyse_agreement_nway` with missingness carried); all three legs re-classified in ONE serialized batch at HEAD `1169170` (legs had straddled `bbf5c92`/`ea8ed72`); 3-leg intersection n=957 — the 3 missing ids are exactly the treaty/legal seeds (residual (b)), not a behavior change.
Witnesses V1–V4 (split behavior-preservation, partition closure, delta-trace, ingestion): `audits/2026-07-06_oq213a_twin_sonnet_leg/`. Leg JSONs are at HEAD and regenerable — cite BOTH corpus and commit. (b) stays open kill-conditioned; interpretation rides OQ-123/124.

## 2026-07-05 — THIRD model-twin leg built: `testsets_sonnet/` (claude-sonnet-5, 1001 stories) — matched triple 957/960; unblocks the 3-model divergence OQs
**Files:** agent/run_no_scope_sonnet.py, prolog/testsets_sonnet/, json_sonnet/, prolog/beta_processed_sonnet.txt, prolog/testsets/, json/, python/audits/twin_comparison.py, ISSUES.md
**Tier:** landed

Third matched twin built over the SAME 1005-seed pool (`prolog/kernels/rebuild_2026-06-13/never_generated_seeds.json`): `testsets_sonnet/` = 1001 claude-sonnet-5 stories via `agent/run_no_scope_sonnet.py` (byte-identical prompt, thinking DISABLED, twin recipe `bulk_corpus_generation.md` §6); matched triple sonnet∩haiku∩flash = 957/960; `outputs/pipeline_output.sonnet.json` passed all four classify_corpus refusals; ~$48 spend.
5 genuinely-new extras merged into `testsets/` (130→135); the 39 collisions are the twins' own extras, NOT overwritten. 4 persistent Sonnet-specific schema-fail treaty/legal seeds + the marginals-only wiring gap → OQ-213; unblocks the 3-model divergence OQs (OQ-123/124/149/211/212).

## 2026-07-04 — OQ-88 MITIGATED: false-mountain detector sweep (positive WITNESSED N=2; D′ discriminator SATURATES; Ω_P auto-route ruling handed to operator)
**Files:** python/audits/oq88_false_mountain_detector.py, ISSUES.md, audits/2026-07-04_oq88_false_mountain_detector/
**Tier:** landed

OQ-88 MITIGATED: detector D (flat-routed ∧ engine-false-mountain) positive WITNESSED N=2 (cell 1); the pre-registered D′ regime-omega discriminator SATURATES (D′≡D; replicates 69/69 corpus-family-wide) → verdict per the pre-registered cells: gate-signal CANDIDATE, not a witnessed gate. Ω_P RULED (operator): review-prompt light seat, NO auto-route.
kernel_v1 fresh classify at `e438723b`; twins at `8a529c73` — D fires on ZERO twin stories by construction (100% kernel-linked); new instrument: in-file `cs_kernel_id` is a second Layer-A routing source the manifest walk misses; positive control = N=1 per engine regime (`411db0e`/`23b7faa`); pre-router archive stories are D-INAPPLICABLE, not D-negative.
Full record + TWINS_ADDENDUM.md + KERNEL_V1_ADDENDUM.md + FINDINGS.md post-review amendments: `audits/2026-07-04_oq88_false_mountain_detector/`.

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

OQ-140 RESOLVED (no engine edits; commits `e90bf3db` Phase 0/1, `9d7baf07` Phase 2): partialling the mechanical confound BEFORE decomposing re-ranks the population (confound-free 56/277 = 20.2%; pre-confound lead `tangled_rope→snare` dissolves; surviving lead `rope→scaffold`). One operator-ruled kind — `naturalization-over-claim (rope→scaffold correction)` [Ω_E] — reproduces on BOTH twins; Ω_C reading 3/3 twin-confirmed; one UNRESOLVED Ω_P prose-signal parked in OQ-211(d).
Scope pin: kind name + counts valid only at `route_address/5` HEAD `7762b2c0` (OQ-211 carries it, bundled_with OQ-138). Controls: emit-independence byte-agreement 277/277, D-ladder 49, mountain 0-count w/ positive control. Residuals → OQ-211. Evidence: `audits/2026-07-04_oq140_divergence_characterization/`.

---

## 2026-07-04 — Drone-report audit (Claude-web critique): d-header fixed, signature wording softened, OQ-209/210 minted, regulatory_lag H¹ fracture witnessed ROBUST
**Files:** python/enhanced_report.py, prolog/signature_detection.pl, ISSUES.md
**Tier:** landed

Drone-report critique triaged: FIXED the factually-false d-comparability header (`enhanced_report.py:356` — the common d path is AUTHORED via `derive_directionality/3` precedence, `constraint_indexing.pl:408`; only the fallback is a config lookup, so same-seat cross-constraint d is NOT apples-to-apples); FIXED `coupling_invariant_rope` overclaim (`signature_detection.pl:769,772`, display-only; = OQ-210 resolved); OQ-209 minted (single-constraint reports render corpus-scope metrics as success-shaped defaults; Pattern-6, bundled_with OQ-97).
Falsifier run: regulatory_lag H¹=4 is ROBUST — the 2+2 powerless≡institutional ≠ moderate≡analytical structure survives ε∈[0.50,0.90] and d_offset∈[−0.15,+0.20]. Caveats: ε/d authored (OQ-102a), Fisher/persistence STALE (OQ-29); probe scripts scratchpad-only.

---

## 2026-07-04 — OQ-193 report-surface build: giant_comp provenance split (pooled + cross-kernel stratum)
**Files:** prolog/giant_component_analysis.pl, python/run_pipeline.py, python/enhanced_report.py
**Tier:** landed

OQ-193 report-surface build (RULED (c)) landed at ZERO engine-behavior change (`per_constraint` sha256-identical): giant_comp `## Provenance split (OQ-193)` md section + same-run `giant_component_analysis.raw.json` co-product + manifest sidecar stamped ONLY on status==ok, and enhanced_report per-constraint NETWORK POSITION section with same-run guard. Strip method = retract-recompute, dead-last, never restored.
Witnessed (testsets): 68 sibling edges stripped; pooled giant 12/72 → stratum 9/95; `same_kernel_edges_surviving=0` (cross_kernel label HONEST); node set = 119 extractiveness-bearing subset of the 128-corpus. Tripwire: giant_comp intermittently hits the 900s Phase-2 timeout (OQ-182-class co-residency, not a regression) and degrades cleanly — check that before suspecting the code. Frozen probe (not edited): `audits/2026-07-02_oq193_giant_comp_ruling/probe_giant_ripple.pl`.

---

## 2026-07-04 — OQ-75(b) grain precursor probe: throw LARGE, cell-count non-monotone under coarsening (statistic-spec inputs)
**Files:** python/audits/oq75b_grain_probe.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Pre-registered grain arms over the tranche-1 registry (10 kernels, 42 pairs, both legs; `audits/2026-07-04_oq75b_grain_probe/`): one refinement step throws LARGE (cells 47→21, contradiction-pair co-slotting 3/3→0/3); coarsen-max grows alignment mass but the raw cell count FELL 47→42 — a cell/vantage-count invariance statistic reads coarsening with the WRONG SIGN; verdicts grain-labile both directions (key_fragile 26→38→12).
Constraints recorded in OQ-75's ruled sub-item (grain normalization load-bearing; no raw-count statistic; contradiction-pair reads refinement-brittle). Controls fired (overlay-took-effect, known-changer, A0==OQ-72 sweep 47==47); canonical registry never edited. Stage 1 proper NOT discharged — statistic unbuilt.

## 2026-07-04 — OQ-72 consumer wiring: axiom concept alignment section in tensions_ledger (three-valued coverage); baker emits tranche-kernel facts
**Files:** python/tensions_ledger.py, python/axiom_concept_bake.py, prolog/axiom_concept_registry.pl
**Tier:** landed

Operator-directed post-close wiring: `tensions_ledger.py` appends a kernel-level "Axiom concept alignment" section (fresh swipl compute each run; a disparity cell = a tension by construction) with THREE-VALUED per-kernel coverage never collapsed (RATIFIED / NOT-YET-RATIFIED — blind BY DESIGN, GAP-24 / single-reading named); fails LOUD on swipl error; in-run TWO-SIDED join control closes the CLEAN-EMPTY hole (halt(3); both arms witnessed; the control's own falsifier caught a format/2-vs-format/3 bug in its first version).
Baker also emits `axiom_diff:axiom_concept_tranche_kernel/1` (coverage provenance travels in the registry; regen byte-identical + C6 refusal re-run). Mixed-scope and full-128 runs pasted in-session 2026-07-04; new tension surfaced immediately (moral_causation_locus disparity [deontological]|[instrumental]).

## 2026-07-04 — OQ-72 resolved: ratified concept key for the axiom axis (pilot); axiom_concept_registry born; westphalia tests re-frozen
**Files:** prolog/axiom_concept_registry.pl, python/axiom_concept_bake.py, prolog/stack.pl, prolog/tests/test_axiom_diff.pl, prolog/axiom_diff.pl, ISSUES.md, docs/the_perturbation_principle.md, docs/design/design_gaps.md
**Tier:** landed

OQ-72 closed at the scoped altitude "mechanism demonstrated" (mixed 10-kernel pilot; `audits/2026-07-03_oq72_concept_key_pilot/` WRITEUP.md has the control table): `prolog/axiom_concept_registry.pl` is the NEW CANONICAL populator of `axiom_diff:axiom_concept/2` (71 ratified facts, tranche 1), loaded from stack.pl; regenerate ONLY via `python/axiom_concept_bake.py` (fail-closed on unratified rows). All six pre-registered controls passed; false-merge 0/71.
Standing cautions: the registry is NAME-keyed (applies on ANY leg); `cs_axiom_contradiction` is not universally same-subject; the key makes the axiom axis RATIFIED-legible, not discovered (§7.1 amendment, `docs/the_perturbation_principle.md`). Also fixed en route: westphalia tests in `tests/test_axiom_diff.pl` were silently unrunnable-green since the 2026-06-20 regime swap (now fixture-local), and their blanket retractall would have wiped the baked registry (now scoped; post-run 71 witnessed). SCOPE-time slot emission = GAP-24; scale-up = separate spend-go (recipe in the OQ-72 resolution).

## 2026-07-03 — OQ-03 RESOLVED: operator declared DR's own seat (extraction-seeking skepticism); 03b mooted; self-application run snapshotted
**Files:** ISSUES.md, audits/2026-07-03_oq03_self_application/
**Tier:** landed

OQ-03 RESOLVED by operator ruling: 03b MOOTED — where DR sits is the declared seat itself, not a redraw-measurable fact. Declaration (operative text in ISSUES.md OQ-03): DR is a variety of philosophical skepticism seated to look for extraction everywhere — a lens, not the truth (`docs/seat-theorem-v1.md`, `docs/commitment_systems/*`, `docs/debugging_philosophy.md`; known limit `essays/2026-06/the_same_paper.md`).
Datum: operator ran `docs/deferential_realism_paper_v8.md` through c-orchestrator same day (5 stories, commit `72ab7663`, manifest n=128) — seat-indexed plurality, kernel siblings diverged; single LLM draw, illustrative seated datum only, never "DR is X". Ledger + reports snapshotted: `audits/2026-07-03_oq03_self_application/`.

---

## 2026-07-03 — OQ-205 RESOLVED: ε declaration discipline BUILT (11 units, Controls P/S green through the recurring gate)
**Files:** prolog/constraint_indexing.pl, prolog/boltzmann_compliance.pl, prolog/narrative_ontology.pl, prolog/data_validation.pl, prolog/json_report.pl, prolog/reading_registry.pl, prolog/tests/test_epsilon_declaration.pl, prolog/tests/fixtures/eps_controls/, python/generate_constraint_pl.py, python/run_pipeline.py, python/enrich_pipeline_json.py, python/enhanced_report.py, python/sweeps/epsilon_stability.py, python/epsilon_authorship_readout.py, docs/design/epsilon_declaration_discipline.md, docs/deferential_realism_paper_v8.md, ISSUES.md, audits/2026-07-03_oq205_build/
**Tier:** landed

Build U1–U11 landed same-day as the spec (commits `e9041905`…close; unit→commit map + transcripts: `audits/2026-07-03_oq205_build/README.md`); all five §9 graduation criteria met, OQ-205 resolved. Both §3 fabrication fallbacks DEAD (fail-closed; the first U2 cut emitting computed-looking `scope_violations: 0` was REJECTED as Pattern 6); no-backfill ruling recorded (pre-build corpus = declared loud-null stratum, `"none_authored"`); new recurring gates `_prolog_epsilon_declaration_gate` + ε-stability sweep, deliberate-break controls witnessed for both.
Sweep tripwire: `drl_core:base_extractiveness/2` is multifile STATIC — took-effect guards must `once/1` the read (an unpinned guard "passed" under the shadow). Corpus finding: `unstable_off_grid` is the largest flag class on every leg (43/110 live, 452/1106 kernel_v1) — ε-sensitivity is mostly NOT threshold proximity; routed to OQ-78/OQ-48; standing readout `python/epsilon_authorship_readout.py` (pipeline Phase 9c) reproduces the census exactly.

---

## 2026-07-03 — OQ-205 spec landed: ε declaration discipline (provenance + stability), read-only census with control PASS
**Files:** docs/design/epsilon_declaration_discipline.md, docs/design/design_discipline.md, ISSUES.md, audits/2026-07-03_oq205_epsilon_census/
**Tier:** landed

Spec-only session: `docs/design/epsilon_declaration_discipline.md` authored (disambiguation vs DP-001/OQ-26 — never title anything "ε invariance"; `epsilon_provenance/5` R2; read-site table anchored `6c59615e`; stability protocol r=0.02 R3 with two kill conditions; R4 commentary-grade); OQ-205 → partial; design_discipline §7 cross-pointer same-commit.
Census (`audits/2026-07-03_oq205_epsilon_census/`, 4 legs, planted control PASS): flash authors ε exactly ON thresholds 218/960; the (0.45,0.46) interval is EMPTY on all legs; OQ-78 re-baseline 41.8% (46/110), last-digit rail model-specific. Recon corrections: threshold set includes `mountain_extractiveness_max` 0.25; SECOND fabrication fallback found at `boltzmann_compliance.pl:248–252` (BaseEps=0.5) beside `constraint_indexing.pl:902–903`; every story authors ε TWICE (silent-fork surface — spec §3 requires equality-check or declared canonical).
Same-day ratification: R2–R4 RATIFIED with the three-site equality check + two-class stability flag (`on_threshold_grid` vs `near_threshold`) amendments; R4 gained its promotion trigger; audit-dir tracking witnessed (8 files in `a2a87dc5`).

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

OQ-126 RESOLVED (`ee51cdff`): the drift terminal now carries its authored-ack provenance as witness-not-verdict — new fields at every terminal surface incl. the no-CS-UID default branch (missed on the first edited run, 30/119; test w3 pins it): `cs_drift_terminal_basis` + `cs_drift_ack_witness`, with `confrontation_path: "none_exists"` a NO-PATH sentinel (OQ-107 `future`), NOT "checked, none found"; enhanced_report renders the terminal conditional (decoration kill-condition control).
RED control witnessed both ways (24/24 restored; test_cs_trifurcation 19/19, OQ-55 twin untouched); clean-vs-edited diff n=119 additive-only; twins n=960×2, 0 missing/unfaithful. Item (c): external-anchoring tier ladder promoted into `design_discipline.md` §10. Ω_P core (honor/reabsorb seated, never engine-certifiable) closed DECLARED, not solved; stale OQ-74 cross-ref corrected (resolved 2026-06-14).

## 2026-07-02 — OQ-195 RESOLVED: general-n H¹ gap spectrum proven at every cardinality; stakeholder frame makes it the live law; OQ-207 minted
**Files:** docs/h1_gap_spectrum_general_n.md, python/audits/oq195_h1_spectrum_check.py, prolog/tests/test_h1_spectrum.pl, prolog/grothendieck_cohomology.pl, docs/deferential_realism_paper_v8.md, docs/deferential_realism_paper_v7.md, docs/deferential_realism_paper_v6.13.1.md, ISSUES.md, audits/2026-07-02_oq195_general_n_gap/
**Tier:** landed

OQ-195 RESOLVED: new proof doc `docs/h1_gap_spectrum_general_n.md` (commit `5d052990` + close) — min nonzero H¹ = n−1 at every cardinality, exact band decomposition, unconditional band-floor lemma, inter-band gap iff n ≥ j+3+C(j+1,2), type-token bound T=7; LIVE via the stakeholder frame (3–12 seats/story on the live legs). Verified under pre-registered BLOCKING criteria n≤40 with PER-BAND bookkeeping (a union-only check cannot discriminate — the unconstrained classifier ran as control, bands mismatch 38/39); engine witness `test_h1_spectrum.pl` 23/23.
Propagated: v8 §3.4/§9.6/Appendix, v7 dated amendment, v6.13.1 changelog, `grothendieck_cohomology.pl` comments (behavior-preserving). Line-drift correction-key: cite the stale-range flag by predicate header, never `grothendieck_cohomology.pl:158`. OQ-207 minted (stakeholder-frame H¹ build; `consensus_provenance/2` its H¹=0 special case). Evidence: `audits/2026-07-02_oq195_general_n_gap/`.

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

OQ-135 RESOLVED: v8 adopted wholesale (operator ruling) in four phased commits — `4ea2c2d5` (v8 paper: entry point, canonical seat/gauge/orientation vocabulary, §5.4 bridge table), `16143c15` (review-response Appendix, Perplexity point only), `7c4cca6f` (README rewrite, claims re-witnessed), `64a44514` (CLAUDE.md refresh); Phase-4 close incl. dead-hash note (`fd1ee561` does not resolve) and fresh-agent self-containedness control 7/7.
Near-fork DECLARED, not resolved (Pattern 2): `docs/v8/foundations/` is source material; `docs/` + `config.pl` stay canonical (8 byte-identical copies, 4 STALE snapshots, which-v6.9 unresolved; the seven-category framing incl. "Naturalized" is historical — live taxonomy is six types + naturalized as cascade outcome, v8 §3.3; contradiction noted at core_v4.3.md:46,117).
Mojibake REPAIRED (operator-ruled): `docs/logic.md` 1,791 + `docs/logic_thresholds.md` 172 sequences — the Feb-2026 repair was PARTIAL and persisted continuously (per-revision counts flat since ≥2026-02-15; NOT a recent regression); method = per-run cp1252 round-trip + 5 hand mappings, 5 positive controls (scratchpad `moji_fix.py`); residual audit zero. The `⤠` (U+2920) rope-gate bypass symbol flagged as likely ancient corruption of `⊤` — cosmetic, not decided.

---

## 2026-07-02 — OQ-137 RESOLVED (reading registry + totality suite + pipeline gate + sweep fixes); OQ-136 evidence in (haiku/contradictions authoring artifact vs genuine mcc)
**Files:** prolog/reading_registry.pl, prolog/tests/test_reading_totality.pl, prolog/commentary_census.pl, prolog/signature_detection.pl, prolog/report_generator.pl, prolog/cs_drift_engine.pl, prolog/cs_axiom_engine.pl, prolog/tests/test_cs_drift_engine.pl, python/run_pipeline.py, python/audits/oq136_bucket_provenance.py, audits/2026-07-02_oq136_census_bucket_provenance/, audits/2026-07-02_oq137_reading_totality/
**Tier:** landed

OQ-137 RESOLVED (slice `a81d4c83`/`2453b922`; close `486756fe`/`ed851eb7`+gate): `reading_registry.pl` (`aggregatable_reading/3` + `census_source_backing/2`) + registry-driven `test_reading_totality.pl` now open `run_pipeline._phase_prolog` as a sequential fail-fast gate (wiring control: planted broken entry → red, clean → green, per_constraint byte-identical). Defects fixed: explain_signature missing `unknown` clause (silent report truncation; planted witness 0/110→111/111), cs_terminal_attractor overlapping rows, cs_has_axioms/+C→+UID doc keys; test_cs_drift_engine rebuilt (RED since the reset), 11/11.
Tripwires: `[C]-m:g(...)` / `V^m:g(...)` templates parse WRONG (`:` is priority 600) — parenthesize `(m:g(...))`; the first sweep passed VACUOUSLY until planted controls caught it. Register any new aggregate-consumable reading predicate in `reading_registry.pl` same-change (opt-in).
OQ-136 evidence in (PROPOSAL frozen `0ba48b4c` before the join; execution `2b66dedc`): q6_unmeasured + no_agent_seats cluster = ONE haiku/contradictions generation-path artifact (p_holm=8e-4); mcc hand-read 8/9 genuine. Rulings executed → OQ-202/OQ-203/OQ-204 minted; R3 one-legged caveat kept; `no_agent_seats` out-of-domain RATIFIED; OQ-136 resolved. Evidence: `audits/2026-07-02_oq136_census_bucket_provenance/` + `audits/2026-07-02_oq137_reading_totality/`.

## 2026-07-02 — Cross-leg check: OQ-52 replicates member-level; OQ-45's phenomenon recurs via DISJOINT members (draw-variance); live-leg hidden-winner exists
**Files:** audits/2026-07-01_oq45_oq52_hidden_winners/, prolog/testsets_haiku/temple_sacrifice_commitment__performance_only.pl
**Tier:** landed

Cross-leg check of the 2026-07-01 closes (B5 of `audits/2026-07-01_oq45_oq52_hidden_winners/`): OQ-52's authored-channel finding replicates member-level 100% on every live leg (haiku 113/113, flash 83/83, live 8/8); OQ-45's phenomenon RECURS via DISJOINT members — expected draw-variance (OQ-26), no member-level replication claimed; one hidden-winner on a LIVE leg (`prolog/testsets_haiku/temple_sacrifice_commitment__performance_only.pl`). kernel_v1 NL population 26 matches the 2026-06-10 matrix (aggregate control PASS); all dispatch controls PASS.
CITATION AMBIGUITY WITNESSED: "HEAD yields strict=235" was the HEAD engine on kernel_v1, not a canonical-corpus count — when citing counts across classify_corpus runs, name BOTH corpus and code state (rule promoted to CLAUDE.md Running the System).


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
Row-26 five-site expansion at HEAD `27afde7a` (behavior-preserving; gate GREEN, validation 0 errors): `drl_fpn:197` is a sentinel pass-through CARVED OUT of row-26 (prior entry conflated it with `:206`); `covering:490`'s 0.5 is a presence guard (the off-grid trigger-class had zero members); `covering:490`/`gap:120`/`omega1:102` = DORMANT/LOCKED (OQ-44 once-for-class); `drl_fpn:206 Immunity=0.5` = NEUTRAL-by-corpus (0 natural fires on testsets(119)+kernel_v1(1106), positive control fires; sink diagnostic-only).
OQ-40 RESOLVED: rows 19–20 split RULED-INTENDED, lifted into `two_axis_architecture_v7.md` §"Representation grounding". Row-22 → OQ-201 minted: `compute_temporal_stability` reads the scalar store, not `measurement/5`; 107/110 and 934/1106 reach-the-gate constraints author an ignored temporal series; >1 scalar level = 0 on both corpora → variance path dead, gate is a degenerate presence-check (positive control catches a known series). Evidence: `audits/2026-07-01_oq41_row26_expansion/`.

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

Graduation witness for the OQ-197 chain — counts reproduced from substrate, not the doc: kernel_v1 canonical-varying = 944 exactly (stakeholder_facts=0); twin detector_calibration net-new = 43/53 and net-new ∩ stakeholders-present ∩ detect_gap_pattern-fails = 29/41 exactly. Case (i): the 944 read undetermined(no_seats) under source (a), never silent 0. Case (ii) REFINEMENT: the 29/41 split three-valued (haiku 4 undetermined + 25 no_gap; flash 12 + 29) — the doc's "uniformly insufficient" premise was imprecise; none silent 0. Negative controls same-run both cases.
OQ-197 fix witnessed end-to-end; only (5) R4 recompute remained (held on the detector_calibration proposal ruling). Evidence: `audits/2026-07-01_oq197_acceptance_controls/`.

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

Branch `oq197-three-valued-gap-operability`, commit `b616e625`: `report_generator:gap_status/2` → gap | no_gap | undetermined(no_seats|single_seat|single_power_position), closing the Pattern-6 collapse in the gap detector; source-parameterized (`gap_seat_source/1`, default stakeholder) so the (a)/(b) ruling is a one-line change; `detect_gap_pattern/2` firing UNCHANGED (57=57 diff-empty); gap_status total/deterministic 119/119 (gap=57 no_gap=32 undetermined=30); 9 two-sided plunit controls pass; 0 new corpus-suite failures.
Finding: `tensions_ledger.py:131` is a SIXTH consumer with its own bug — it computes index-mismatch from `perspectives` and counts `unknown` as a diverging value (needs repointing to gap_status or an unknown filter). Full 6-site consumer map + sequencing: ISSUES.md OQ-197 (Progress 2026-07-01).

---

## 2026-06-30 — detector self-assessment: Slice A (author×engine cross-tab) LANDED; Slice B (calibration omega) proposal awaiting ruling
**Files:** prolog/routing_sink.pl, docs/design/detector_calibration_omega_proposal.md, outputs/routing_sink.json
**Tier:** landed

From the Elias-Thorne report review, the "is the detector calibrated" question split three ways. Slice A LANDED (`routing_sink.pl`, commit `f6921ac1`): `author_engine_crosstab(_summary)` added to `routing_sink.json` — (authored_type × engine_type) confusion cross-tab over per-seat `seat_diff`; hard label SEAT-AGREEMENT, NOT calibration (divergence_rate 0.77 is a two-seat disagreement rate, never a detector FP rate); positive control reconciled (diagonal 91==no_route; 396+36+44=476=119×4; an unbound-key Pattern-5 vacuous guard caught pre-ship, nonvar/2 added).
Slice B PROPOSAL awaiting ruling (`docs/design/detector_calibration_omega_proposal.md`, commit `c4864999`): a `detector_calibration` omega the engine MINTS OPEN but never closes, typed as an Ω_E + Ω_P PAIR; R1–R4 are the operator's seat, NOT wired/fired. (C) auto-closing the verdict = category error (no ground truth; seat theorem).

## 2026-06-30 — perspective_chi d/f_d fork fixed (resolved-context derivation); report frame added
**Files:** prolog/constraint_indexing.pl, prolog/json_report.pl, python/enhanced_report.py
**Tier:** landed

Fixed the d/f_d fork (`6d1df7d1`): `write_one_perspective_chi` derived d/f_d on the UNRESOLVED canonical power atom while chi coalition-resolves internally — 40/119 live constraints had a `powerless` row where chi ≠ ε·f_d·σ (surfaced by web-Claude; both its hypotheses falsified — d is observer-position-keyed, `constraint_indexing.pl:478-487 power_role_heuristic/4`). Fix: factored `constraint_indexing:agent_resolved_directionality/4`, used by BOTH the chi path and the JSON writer; behavior-preserving (0 type/chi changes; forked rows 40→0/440).
Also `5e5830df`: "HOW TO READ THIS REPORT" frame prepended to `enhanced_report.build_header` (seats surface; divergence is the finding; RED = authored direction, OQ-187). Tripwire: any NEW consumer reporting d/f(d) beside chi must derive via `agent_resolved_directionality/4`, never `derive_directionality/3` on the raw canonical context — else the fork reopens silently.

## 2026-06-30 — OQ-38 RESOLVED: reproducible orphan-xref tool built; four calibration orphans stripped; OQ-196 minted
**Files:** prolog/orphan_xref.pl, python/audits/oq38_orphan_sweep.py, prolog/drl_composition.pl, prolog/utils.pl, ISSUES.md, AGENTS.md, audits/2026-06-30_oq38_orphan_xref/
**Tier:** landed

OQ-38 RESOLVED: reproducible tool-native funnel replaced the discredited 2026-05-31 grep sweep — `prolog/orphan_xref.pl` (library(prolog_xref); diagnostic, NOT a pipeline gate; conservative caller matching) + driver `python/audits/oq38_orphan_sweep.py` (self-exclusion gotcha witnessed + fixed). Funnel over 121 sources: 614 exports (grep undercounted by 86), 201 STATIC_ORPHAN, 29 dynamic-masked, M=170 real-orphan upper bound. Stage-1 hard gate: `cs_reference_frame/2` LIVE (the OQ-35 adversarial case); `non_monotonic_trajectory/2` LIVE in `metric_drift_report.pl` (stale `drift_report.pl:164` cite corrected in ISSUES.md).
Four calibration orphans stripped (commits `736783e4`, `6a3acf1d`; tool `c9be12ca`) — behavior-preserving (load gate exit 0; validation suite byte-identical; per_constraint sha256 `d9c85bec…` unchanged, mtime advanced). Cascade: `safe_get_category/3` newly orphaned → OQ-196 minted (value-adjudicate the M=170), NOT stripped. Writeup: `audits/2026-06-30_oq38_orphan_xref/WRITEUP.md`.

## 2026-06-30 — OQ-37 RESOLVED (read-but-unauthored metric census re-dispositioned); GAP-23 minted
**Files:** ISSUES.md, docs/design/design_gaps.md, prolog/data_validation.pl, python/generate_constraint_pl.py
**Tier:** landed

OQ-37 RESOLVED at its root: all six read-but-unauthored `constraint_metric` names trace to the fixed compiler emit set (`generate_constraint_pl.py:608-635`); authoritative cross-corpus census — all 6 are 0 facts on testsets/haiku/flash/kernel_v1 = 3,142 stories, controls fire on every leg (`audits/2026-06-30_oq37_census_redispose/`). Dispositions routed (OQ-93 grids; χ-partition closed `3ab3ace4`; helpers → OQ-38; supp/ε-floor → OQ-48); the two genuine deferred capability livens (`sunset_time`, `internalization_depth` + never-loaded `psych_bridge`) → GAP-23 (priced, operator-seat).
One behavior-preserving edit (`5b7a8b95`): dropped never-authored `resistance_to_change` from the `data_validation.pl` extreme-value monitor (provably byte-identical, validation-channel only). Correction-key (OQ-64 instance): `resistance` ≠ `resistance_to_change` — distinct referents (`grid_first_contact_gate.py:48`); the proposed `metric_drift_events.pl:174,247` repoint was DECLINED (`safe_metric/3` fails silently, repoint buys zero behavior while baking a wrong-metric identification) — liven both detector inputs together (GAP-23) or leave dark; never repoint by name-stem.

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

OQ-23/OQ-24 RESOLVED: the positive-controlled canary (`prolog/tests/test_coexists_fpn_canary.pl`) FALSIFIED the premise it was built to backstop — the `coexists_with` "zero contamination by definition" exclusion was ALREADY VIOLATED on every populated leg (testsets/ 2, haiku 178, flash 361, kernel_v1 662) via the authored `affects_constraint` side channel (the DP-001 ε-linkage instruction; forecloses leaked likewise); only FPN `effective_purity` and the coupling baseline reach a shipped product.
FIX: same-kernel-donor guard as first clause of `compute_edge_contamination/7` — contamination-LOCAL by design, giant_comp topology deliberately untouched so OQ-193 (sibling-strip collapses the giant 334→70 on kernel_v1 — unsettled Ω_C ruling) can be ruled on its own evidence; coupling-baseline ship noted in HOLD_FINDINGS. Witnessed: leaked 2→0 live (forecloses 1→0), cross-leg post-fix leaked=0, connectivity zero-change control, plunit regression gate GREEN. Tripwires file-local in `drl_purity_network.pl` (do NOT extend the guard into `constraint_neighbors_existing/2` without OQ-193). Evidence: `audits/2026-06-29_oq23_coexists_fpn_canary/`.

## 2026-06-27 — OQ-124/OQ-149 committer-axis convention control: A=SIGNAL, B=CONVENTION, C=OPEN
**Files:** ISSUES.md, prolog/signature_detection.pl, python/story_repair.py, agent/run_no_scope_gemini.py
**Tier:** landed

Ran the OQ-70-style bait-confound control on the three cross-model-divergent fields, per-field pre-registered (`audits/2026-06-27_oq124_oq149_committer_convention_control/`); twins re-classified at one commit `bbf5c92` (on-disk outputs had straddled 20fab78/8126231); positive controls held. Verdicts: Field A (CHE↔FCR signature fork) = SIGNAL — ~13:1 asymmetric, dominant lean a continuous extraction-magnitude difference (ext Spearman 0.86, flash systematically lower; two-sided `with_retracted` control discharged) → signature lean carries a model index (v8 §3/OQ-72). Field B (`cs_reading_relation`) = CONVENTION — fails to covary with settled substrate on disagreeing slots → needs a provenance bucket (precedent `becd0f87`). Field C (`overridden` 51-vs-4) = OPEN-pending-instrumentation.
Enrichment: `overridden` is coercion-invariant (missing `cs_axiom_status` KeyErrors generation, `generate_constraint_pl.py:672` — NOT silently defaulted; the `contested/foreclosed→holdable` remap `story_repair.py:89-90` IS silent — needs raw pre-repair capture via `story_repair._normalize_axiom_status` cid logging). Third-model spend warranted (A=signal), operator-gated.
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

Flipped `config.pl:571 trajectory_enabled` 0→1. Root cause of the intermittent flag=1 stall: concurrency memory pressure — the O(N²) `trajectory` (HAC) stage co-resident with O(N²) `giant_comp` in the 4-worker Phase-2 pool (NOT a giant_comp bug; OQ-77). Fix (surgical, Python-only): `run_pipeline._phase_prolog` pulls `trajectory` out of the parallel tasks and runs it sequentially after `_run_parallel`; the 11 remaining real stages stay parallel; order correctness-irrelevant (C0: `context_profile_report.md` has no downstream consumer).
Witnessed (`audits/2026-06-27_oq182_trajectory_serialization/`): ps/RSS sampler captures PRE-FIX co-residency vs CURED disjoint windows; N=10 liveness battery 10/10 GREEN; freshness positive control PASS; C0 zero classification diff; 300s timeout held (≥175× margin); `validate_config` PASS at flag=1. Tripwire promoted to CLAUDE.md (Running the System): never re-fold trajectory into the parallel tasks list.

---

## 2026-06-26 — OQ-91 resolved: commentary-grade repair-transition detector + report surface
**Files:** prolog/transition_paths.pl, prolog/json_report.pl, python/enhanced_report.py, docs/repair_dynamics.md, ISSUES.md
**Tier:** landed

OQ-91 resolved: new `repair_transition/4` in `transition_paths.pl` — the upward dual of the 8 decay heads (transitive closure of decay edges read backwards, `unknown` excluded; reuses `degradation_chain/3`), 4th arg a named op (maintain/splice/replace rope line-ops; scaffold_struck). COMMENTARY-GRADE — must never feed `classify_from_metrics/6`, the signature layer, or `verdict_join`. Serialized as the `repair_transitions` per-constraint field (`json_report.pl`, hermetic globals wrapper), rendered by `enhanced_report.build_repair_section` (silent on decay-only = honest absence). Doc: `docs/repair_dynamics.md`.
Witnessed (`audits/2026-06-26_oq91_repair/`): B1-scan non-empty (testsets/ 2, kernel_v1 30, incl. multi-step homoousios/versailles); B4 invariant PASS (classification fields byte-identical). Bug found+fixed: `repair_op` clause selection must key on from/to/pre, not a bound 4th arg. No promotion (wiring repair into classification would fail LOUD).

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

Operator ruled: gate the OQ-104 danger class by REGENERABILITY. `audit_citation_status.py:classify()` splits untracked cited paths into `untracked-frozen-evidence` (non-`outputs/`; GATING intrinsic ERROR) vs `untracked-regenerable` (top-level `outputs/`; non-gating WARN); `scripts/gate.sh` gains the 7th check `audit cites` (`--check` exits 1 iff frozen-evidence non-empty or parse problems). Controls 23/23 → 25/25 incl. a matched-pair isolating the prefix as the deciding variable; witnessed RED-on-frozen / GREEN-on-removal; full gate 7/7 GREEN (all 39 untracked paths under `outputs/`).
Scope (do not over-read "resolved"): one of two origin routes mechanized; two residuals stay non-gating with kill conditions in ISSUES.md OQ-104 (a typo'd path lands `missing-pending-M`; a frozen artifact parked under `outputs/` reads regenerable). Controls: `audits/2026-06-18_oq104_citation_checker/controls.py` + `controls_run.sh`.

## 2026-06-26 — GAP-04/OQ-53 increment: cross-kernel reading-stance transpose (fingerprint_shift spine)
**Files:** prolog/cs_kernel_registry.pl, prolog/json_report.pl, python/cross_kernel_stance_report.py, prolog/tests/test_cs_kernel_registry.pl, docs/design/design_gaps.md, ISSUES.md
**Tier:** landed

Built the reading-stance transpose GAP-04 named absent (OQ-53's close had deferred it): `cs_kernel_registry.pl` gains `declared_stance/2` (THE SEAT — hand-declared cohort table), `reading_stance/2` (declared-only authority; morphology never a fallback), `stance_cohort/2`, `cross_kernel_stance_profile/2` (per-position majority consensus, convergent/divergent partition, verdict WITH cohort provenance) + report/JSON export; `json_report.pl` now serializes `fingerprint_shift` per per_constraint entry (0 pre-change → 104 post-run); consumer `python/cross_kernel_stance_report.py` → `outputs/cross_kernel_stance.{json,md}`.
Cohort DECLARED, not derived (Seat-Theorem Cor 2b): morphology unreliable both ways on the 7-member abolition cohort (exact-stem 4/7; substring over-admits a *rejection* of abolitionism). Witnessed both twins: abolition convergent 5/7 on BOTH (draw-stable); deterrence flips across twins (seat-expressive); read the split as a σ/seat partition. Pinned by 5 corpus-free `transpose_*` tests in `test_cs_kernel_registry.pl`; the pre-existing `divergence_silent_at_observed_agreement_context` failure is documented archive-draw fragility, not this change. Provenance: ISSUES.md OQ-53 addendum, GAP-04 status.

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

OQ-21(a) RESOLVED (`cfb5fa03`): the documented pick-latest-by-`cs_created_at` path in `write_per_constraint_entry/4` was DEAD — `aggregate_all(max(T-U),…)` evaluates `T-U` arithmetically, throws on atom UIDs, swallowed by `catch/…fail`, so selection was always the `@<` msort/last fallback for the branch's whole life. Operator RULED `@<` canonical (instances are parallel draws, not versions — no canonical-latest; sole live consumer `orbit_operator.py` needs determinism, not timestamps); dead clause removed; test pins `@<` + bundle coherence, positive control witnessed RED under reintroduced recency.
Reusable tripwire: the `aggregate_all(max(Key-Val))` argmax idiom evaluates Key-Val arithmetically and throws on non-numeric keys — a surrounding catch silently degrades to the fallback; witness BOTH arms. (b) left open gated on a future multi-instance load (OQ-17 pointer stale — disposed); `[GATE]` GREEN.

---

## 2026-06-25 — OQ-19 RESOLVED: drift-trajectory trigger thresholds made durable + fail-loud
**Files:** python/enhanced_report.py, python/tests/test_drift_trajectory_granularity.py, ISSUES.md
**Tier:** landed

OQ-19 RESOLVED (single-file, behavior-preserving): the 6 `build_drift_trajectory_section` thresholds hoisted into a named `_DRIFT_*` block keyed to `_DRIFT_MEASUREMENT_GRANULARITY = 0.01` (Trigger A derived, IEEE-754 byte-identical to the literal — witnessed); `_series_granularity` guard prepends `[CALIBRATION WARNING]` on finer-than-floor series (positive-control test `python/tests/test_drift_trajectory_granularity.py`).
Premise correction worth a cold read: "live data is 2-decimal" was FALSE — 4 constraints already author 3-decimal values, so the feared finer-granularity regime is partly present (guard currently inert: 29 rendered sections, 0 warnings). Witnesses (float kill-condition, grep 7→0, per-trigger A/B/C diff, positive control) in the ISSUES.md OQ-19 resolution block. History-only; no promotion.

---

## 2026-06-25 — OQ-182 C-null PASS: HAC structural families validated as MEANING-bearing (testsets/ leg)
**Files:** audits/2026-06-25_oq182_trajectory_revive/c_null_harness.pl, audits/2026-06-25_oq182_trajectory_revive/c_null_results.log, audits/2026-06-25_oq182_trajectory_revive/c_null_distribution.json, audits/2026-06-25_oq182_trajectory_revive/c_null_protocol_FROZEN.md, audits/2026-06-25_oq182_trajectory_revive/c2_domain_finding.md, ISSUES.md
**Tier:** landed

C-null PASS — HAC structural families validated MEANING-bearing on the testsets/ leg (no engine edits; `trajectory_enabled` stays 0; plan `~/.claude/plans/bright-jumping-cocke.md`): RealSil 0.161119 (97 constraints, 11 families) > P95(null) −0.026436 over 200 per-component-independent shuffle draws; 0/200 null draws reach real; TEETH PASS (+5.01σ); null centers 15 families vs real 11 (conservative direction); reproducible under seed `20260625`. All controls pasted BEFORE the verdict (INTERNAL-CHECK, GROUPING-FIDELITY, FIDELITY, JOINT-TOOTHLESS — demonstrating the false-PASS the per-component design avoids — TIE-BREAK).
MECHANISM CORRECTION (frozen quantities unchanged): the frozen "Chimera surgery map" was mechanically wrong — `group_by_shift/2` keys on constraint identity, ignoring `trajectory_cached`, so the harness builds shift-groups itself; erratum recorded in `c_null_protocol_FROZEN.md`. Scope: twins remain OPEN (near-vacuous cross-domain gate; deferred to rebuild); remaining legs C0/C-gen/kernel_v1 then the gate flip. Evidence: `audits/2026-06-25_oq182_trajectory_revive/` (c_null_harness.pl, c_null_results.log, c2_domain_finding.md).

---

## 2026-06-25 — OQ-182 minted: revive + validate the dormant HAC trajectory-mining subsystem (cheap tier)
**Files:** prolog/context_profile_mining.pl, prolog/config.pl, prolog/isomorphism_engine.pl, prolog/constraint_bridge.pl, prolog/report_generator.pl, ISSUES.md, audits/2026-06-25_oq182_trajectory_revive/
**Tier:** landed

OQ-182 minted — cheap tier of the trajectory-revive plan (`~/.claude/plans/fancy-splashing-pancake.md`; the plan's "OQ-180" label was already taken by the OQ-51 build, commit `cef5dc6e`). C-prov PASS on testsets/ (witnessed, `c_prov_runtime.log`): `trajectory_run/2` (97 trajectories → 11 families, 448 twins) leaves both `classify_at_time_*` globals UNSET — no imputed BaseX coupling; positive control sensitive, but note the OQ-178 trap hit live: the control only proves sensitivity when fed a Time ON the constraint's authored grid. C-prov re-runs on kernel_v1 in the spend tier.
Fork verdict: `context_profile_mining:cross_domain_twins/3` is canonical; `isomorphism_engine.pl` is a loaded-but-non-executing Pattern-2 fork (all callers dead; positive-controlled grep) — NOT deleted, see `design_gaps.md` GAP-20. Spend tier (C0/C1/C2/C3/C-null, gate flip) operator-gated. Audit: `audits/2026-06-25_oq182_trajectory_revive/`.

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

Fixed the OQ-57-class rotted qualifier in the dormant trajectory-mining path (commit `fc9b4688`, surfaced during the OQ-16 rename, rename-independent): `standard_contexts/1` called `dirac_classification:standard_context/1`, deleted 2026-06-02 (`dirac_classification.pl:115`) — re-qualified to `drl_core:standard_context/1` (identical 4-context generator, verified). Witness: the report generator on run_pipeline's exact load chain now exits 0 producing a 135-line report (was crash → empty); production unchanged (`trajectory_enabled=0`).
Why unnoticed: `context_profile_mining.pl` is NOT loaded by `[stack]`, so `check_stack.pl` never saw it. Gap closed (commit `a82d7ed0`): check_stack now loads the trajectory chain faithfully — positive-controlled, baseline unchanged (same 5 known undefineds); honest boundary recorded in-file (other standalone report chains remain uncovered). Validating/reviving the subsystem became the OQ-182 arc.

---

## 2026-06-25 — OQ-16 RESOLVED: temporal vocabulary rename pass (name-only, 5 renames, 3 commits)
**Files:** prolog/metric_drift_events.pl, prolog/metric_drift_report.pl, prolog/context_profile_mining.pl, prolog/context_profile_report.pl, prolog/network_dynamics.pl, prolog/stack.pl, prolog/drl_lifecycle.pl, prolog/transition_paths.pl, prolog/cs_pattern_detection.pl, prolog/cache_registry.pl, python/run_pipeline.py, scripts/pipeline_dashboard.sh, ISSUES.md
**Tier:** landed

OQ-16 RESOLVED: name-only rename pass (no logic/threshold moved), 5 renames in 3 commits — `0a204af1` (`detect_network_drift/3` → `detect_network_contamination/3`), `1d861cee` (file+module renames drift_events→metric_drift_events, drift_report→metric_drift_report, trajectory_mining→context_profile_mining, trajectory_report→context_profile_report + output path `context_profile_report.md`), `1bcc07c5` (doc code-pointer tokens); doc-scope refinement `76eae0c1` (4 dated recon/essay docs keep bodies as dated records + per-doc end-notes); close-out `fb45c0e3`. Operator ruling: `metric_*` over `dr_*`.
Deliberately out of scope (logged): JSON output field `drift_events`, internal `run_trajectory_report`, doc filenames. Final-grep exclusion list recorded (full entry in git history): remaining old-token hits in the drift_events JSON surface, the 4 historical docs, and verbatim review transcripts are intentional-preserved, not missed renames. Witness: `[stack]` loads, check_stack clean, full run_pipeline exit 0 writing the renamed path. Side-finding (fixed separately, `fc9b4688`/`a82d7ed0`): the dangling `dirac_classification:standard_context/1` call — see the entry above + `swipl_load_path_and_probe_gotchas.md` §1. Pass interleaved with concurrent instances on main; outcomes converged (multi-writer hazard per CLAUDE.md).

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

Applied OQ-51's N/A rule (`unknown` = not-agree, not-diverge) to `cs_kernel_comparison` — the site the original build never enumerated (OQ-178 audit: all-unknown contexts scored `agree(unknown)`, inflating robustness). Verdict trichotomy `ctx_reading_verdict/2` agree/diverge/undetermined each carrying NUnk; LENIENT operator ruling (≥2 real readings ⇒ verdict over the reals); `cs_kernel_divergence/4` + `pair_reading_agreement/7` require BOTH types real via shared `is_real_type/1` — load-bearing for the join invariant Σ DivergeN == #cs_kernel_divergence, never refactor back to bare `\=`; Jaccard = null when no both-real context; JSON gains divergent/undetermined/abstaining_context_count + `divergence_patterns`; report renders the enumeration.
Witnessed: unit suite 20/20 (6 synthetic N/A controls + join invariant), partition invariant 9/9, `cs_kernel_divergence_count` 20→16 (actinide's only "divergence" was unknown-vs-real). Two silent footguns fixed (`~6f` threw on null Jaccard aborting the whole JSON write; `agree(_)` arity fail-match → RobustN=0).
Do not misread a no-op diff: the OQ-178 all-unknown inflation case fires on 0 serialized kernels (witnessed by the synthetic control + 13 non-serialized singletons excluded by the `L>=2` filter, `json_report.pl:1734`); the live serialized effect is the abstention-tolerant RISE (performance_legitimacy robust 21→147 — the ruling applied). Join invariant holds 9/9 live (the plan's 42/42 was the haiku twin). Scope: this surface only; the original OQ-51 `count_disagreeing_pairs`/`sheaf_status`/H1 sites stayed OQ-51's separate item. Console/OQ-119 count drops expected.

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

OQ-15 core RESOLVED (closes GAP-12; v8 §8 item 1 / OQ-135 priority-1): static cross-axis reachability
guard (`check_axis_boundary.pl` + `axis_boundary_allowlist.txt`) wired into `scripts/gate.sh` + `run_pipeline.py`;
8 boundary edges censused, exactly one committer→observer `influences` bridge confirmed; Phase 2 RULED
policed-in-place, v7 synthesis PRESERVED (trigger: a SECOND bridge fires the guard RED). Commits `c6fe7edb`
(Phase 0a/0b) + `fd1ee561` (guard); witnesses `audits/2026-06-23_oq15_crossaxis_witnesses/` (incl.
`bc_rewitness.txt`). Bundled OQ-15 ↔ OQ-135.

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

OQ-10 RESOLVED: `compare_kernel_readings/3` + `reading_robustness` object in `pipeline_output.json` +
`enhanced_report` kernel-reading section; witnessed on twin `end_of_life_decision_authority` (156 ctx → 73
robust / 83 specific, Jaccard 0.63/0.53/0.31; two-sided control passed). Commit `d2cb9bb7`. Verdict tokens
and field names SUPERSEDED 2026-06-25 by the OQ-51 trichotomy (see that entry). OQ-176 spawned
(`cohomological_obstruction/3` returns H1=0 for an ABSENT constraint — Pattern-5, logged not patched).

---

## 2026-06-23 — OQ-112 RESOLVED (close-out): arc is latent-hardening, structurally latent across all three live legs
**Files:** ISSUES.md, audits/2026-06-23_oq112_closeout/
**Tier:** landed

OQ-112 RESOLVED (close-out, no engine edits): only item 1 touched live output (13/92 abductive
`agrees`→`unavailable`, headline-neutral); items 2/4/7 latent-hardened; items 3/5/6/8 declared-not-landed
(fix-shapes recorded). Masking is STRUCTURAL — 0 live bites on all three legs (testsets/haiku/flash);
archives not swept (declared boundary, OQ-89 pattern). Two reusable tripwires (guard-count over-reports a
Pattern-6 firing; latent-on-92 ≠ latent engine-wide) in `audits/2026-06-23_oq112_closeout/`.

---

## 2026-06-23 — OQ-112 item 4 RESOLVED (Round 3, Commit 1 alone): maxent-local accessors fail-closed; Commits 2/3 falsified
**Files:** prolog/maxent_classifier.pl, docs/design/design_gaps.md, ISSUES.md, audits/2026-06-23_oq112_round3/
**Tier:** landed

OQ-112 item 4 RESOLVED (Commit 1 alone): the four maxent-local accessors return `unknown` on metric absence
instead of fabricated `0.0` (+ `number/1` guard in `maxent_threshold_proximity/4`); blast radius contained to
`maxent_classifier.pl`; LATENT on 92 (0 sentinels live — not a live catch). Round 0 falsified Commit 2 (loud
throw; item-2 gate already floors it) and Commit 3 (dissolved into 1; `maxent_boundary_analysis` → GAP-19 in
`design_gaps.md`). Evidence: `audits/2026-06-23_oq112_round3/`; Round-4 gate installed in the ISSUES OQ-112 entry.

---

## 2026-06-23 — OQ-112 item 7 RESOLVED → ROUND 2 COMPLETE: wasserstein incomparable-mass provenance tokens
**Files:** prolog/json_report.pl, python/shared/schemas.py, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

OQ-112 item 7 RESOLVED → ROUND 2 COMPLETE: `json_report.pl:438–442` three-states-into-`0.0` collapse replaced
by `wm_token/3`/`wm_emit/3` (float | `null` | `"errored"`, + unbound-M fourth-state guard); output-identical on
the live 92 (344/344 cells genuine float; absent/errored arms 0-firing) — contract widening forced-witnessed,
live-UNEXERCISED; `schemas.py:228` widened in-comment only; out-of-repo float readers unwitnessed. Witnesses:
`audits/2026-06-22_oq112_round2/` (4-state controls; item-7-isolated diff at HEAD `a5593f7`; schema validation 0 errors).

---

## 2026-06-23 — OQ-112 item 2 RESOLVED: completion-witness-or-fail-closed gate (maxent stages)
**Files:** prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/maxent_classifier.pl, AGENTS.md, ISSUES.md, audits/2026-06-22_oq112_round2/
**Tier:** landed

OQ-112 item 2 RESOLVED: completion-witness-or-fail-closed gate for maxent stages — distinct
`maxent_indexed_run_info/3` completion fact, `maxent_attempted/1` + `maxent_void_alerts/1` fail-closed in
`verdict_join` (yellow/moderate, operator ruling), absorbers widened so a stage FAILURE continues the run.
Commits `d69d5d39`/`4ee4ce08`/`0ef5bf6d`; matrix + witnesses `audits/2026-06-22_oq112_round2/` (GATE.md).
Forced-witnessed, live-UNEXERCISED (0/92 voided) — do not cite as "verified live on 92"; two falsifiers named.
Invariant promoted to AGENTS.md ("completion-witness-or-fail-closed").

## 2026-06-22 — OQ-112 item-1 (C4a) RESOLVED: diagnostic_summary data-absence else-branches fail closed
**Files:** prolog/diagnostic_summary.pl, ISSUES.md, audits/2026-06-22_oq112_round1/
**Tier:** landed

OQ-112 item-1 (C4a) RESOLVED: the 13 `; Signal = agrees` else-branches in `diagnostic_summary.pl` sorted 10
sound / 3 defects; `:198`/`:212`/`:163` `agrees`→`unavailable` (commit `4e6cf6e9`). Only `:198`
(probe_abductive) is LIVE — 13/92 constraints with no `abd_triggers` fact no longer count as agreement;
output-changing at the agreements list, HEADLINE-NEUTRAL (witness `probe_before.tsv`/`probe_after.tsv`,
`audits/2026-06-22_oq112_round1/`). Tripwire kept: a missing `abductive_data.json` used to read as universal
agreement (Python side already split absence at `enrich_pipeline_json.py:164–169`). Items 2–8 staged in ISSUES.md.

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

OQ-173 RESOLVED: `apply_override_for_sig/3→/4` (maxent_classifier.pl:318) made seat-aware — routed seats
(`fcr_routed/1`, `constructed_routed/1`) skip the MaxEnt boost. Witness (`audits/2026-06-21_maxent_seat_aware/`
diff_witness.out + FINDINGS.md): exactly the 12 routed seats revert to raw, 0 non-routed move, 1 categorical
flip (`shinbutsu`), 0 `verdict_join` changes; 21-corpus sweep `routed_STILL_boosted=0` (original_v5 PARTIAL,
pre-existing failure). Premise refined: the ×3 boost never flips a CLASSICAL top — manufacturing was
classical-mass + the indexed top. Future-conversion recipe: `signature_detection_wiring.md` §4.

## 2026-06-21 — OQ-138 constructed-3 sub-part RESOLVED: claim-discriminant conversion (keeps #2's floor)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/CONSTRUCTED3_FINDINGS.md
**Tier:** landed

OQ-138 constructed-3 RESOLVED: the 3 live `constructed_high_extraction` unknown→snare seats routed to honest
`unknown`; NEW claim discriminant (mountain claim→severe, else→informational) keeps #2's RED floor — kill
condition MET (institutional_trust_erosion byte-identical RED; 47 inert + non-constructed byte-identical;
5-corpus mountain-routed→severe holds). `constructed_routed`/`fcr_routed` keyed on the UNBOUND cascade winner
(caught `superheavy_decay`). Maxent residual (`maxent_classifier:341` boost flips #1/#3 maxent_top) benign,
tracked as shared GAP. validation_suite 92/0/0. Full: `audits/2026-06-21_oq138_fsm_route_conversion/CONSTRUCTED3_FINDINGS.md`.

## 2026-06-21 — OQ-138 FCR-9 sub-part RESOLVED: false_ci_rope SEAT-AWARE conversion (template didn't transfer)
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/diagnostic_summary.pl, ISSUES.md, AGENTS.md, audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md
**Tier:** landed

OQ-138 FCR-9 RESOLVED: `false_ci_rope` is SEAT-SPLIT (9 routed / 3 piton / 13 inert) so the FSM signature-level
template did NOT transfer — built seat-aware: `fcr_routed/1` keyed on dispatch gates + the dr_type OUTCOME
(the metric proxy diverged on 2 haiku + 4 flash seats, caught by the 5-corpus sweep); `seat_overrides/2`
threaded through diagnostic_summary. 9 seats route tangled_rope→scaffold/snare (6 verdicts change); piton-3 +
13 inert + non-FCR byte-identical; statutory_debt yellow→red via the maxent ensemble (Position-A, OQ-90 not
relitigated). Residual: maxent FCR boost (maxent_classifier:331) signature-level, logged. Full:
`audits/2026-06-21_oq138_fsm_route_conversion/FCR9_FINDINGS.md`.

## 2026-06-21 — OQ-138 FSM sub-part RESOLVED: false_summit_mountain converted RECLASSIFY→ROUTE; routed false-summits read RED
**Files:** prolog/signature_detection.pl, prolog/abductive_helpers.pl, prolog/config.pl, ISSUES.md, AGENTS.md, docs/technical/signature_detection_wiring.md, audits/2026-06-21_oq138_fsm_route_conversion/
**Tier:** landed

OQ-138 FSM RESOLVED: `false_summit_mountain` no longer overwrites `dr_type` (config
`false_summit_override_target`; victim-discriminant severity via `converted_signature/1` +
`signature_diagnostic_severity/3`; FSM removed from `known_override_signature/1`/`override_target/2`).
Full-pipeline diff: only the 3 live FSM seats change, 89 byte-identical (`PIPELINE_OLD.txt`/`PIPELINE_NEW.txt`);
verdict goes yellow→RED, ruled Position A (red is honest — 82 FSM seats across 5 corpora all carry the unmasked
dirac/cohomology tensions); protein_anabolic_resistance keeps `correction` via the discriminant. OQ-138 stays
partial (FCR/constructed/CI-rope OPEN; FNL deferred OQ-70). Full: `audits/2026-06-21_oq138_fsm_route_conversion/FINDINGS.md`.

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

OQ-69 (a backlog ledger, Ω_P) DRAINED into 17 new OQs (OQ-154–170; OQ-170 `blocked_on` OQ-160; prior
check_stack item already → OQ-142–145) and closed resolved with a provenance map; no engine code changed.
Two operator rulings (cluster F/G split; distinct-within-band priorities, all 17 provisional). Corrections:
priority parser accepts 1–99 (omega_resolver.py:69); δ (OQ-162) is live-but-zeroed, not unwired; close-vs-keep
ruled from `omega_resolver.py:244–258`. Witnesses (issues_status 170/0, omega check + selftest 10/10, menu
arrival/departure incl. control OQ-63, gate GREEN) + δ probe: `audits/2026-06-20_oq69_ledger_drain/`.

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

Two OQ-93 grid-display fixes: fully-absent grids (0/32) now print ONE plain informative line (supersedes the
same-day `[CONDITIONAL]`-token form, commit `5c23830e`; OQ-98 ruling 1 preserved for PARTIAL grids), and the
stale `data_repair.pl:356`/`:291` "unauthorable" message reworded (grid is opt-in by story focus). Hinge
witnessed: `grid_provenance` reaches `pipeline_output.json` (86/92), so display trimming cannot drop
provenance. STILL OPEN: `assemble_report` embeds ~12 grid-absent DEV-preamble lines in the model-facing .md —
decluttering pending operator go (sibling: `intent_engine.pl:75`).

---

## 2026-06-20 — OQ-56 + OQ-53 closed: canonical cross-kernel reading-stance vocabulary ruled
**Files:** python/orbit_operator.py, docs/design/design_discipline.md, ISSUES.md
**Tier:** landed

OQ-56 RESOLVED (Ω_P ruling): canonical cross-kernel vocabulary = the two Tier-1 draw-robust keys
`observer_signature` (0.722) + `obstruction_class` (0.734), made a checked fact (`CANONICAL_VOCABULARY` +
per-record `canonical` flag in `orbit_operator.py`); kill condition recorded NOT armed (manual reopen check);
seat owned in `design_discipline.md` §0.1. Headline Ω_E: the semantic-stance transpose is
foreclosed-as-draw-robust (`seat_role_vector` 0.245, model-relative). OQ-53 transpose leg resolved
(a-restricted): `constructed_high_extraction` spans 25 multi-reading kernels, `false_ci_rope` 11. Consumer
wired: the report `Signature:` line carries the canonical/twin-agreement tag via `orbit_operator.KEY_META`
(`enhanced_report.py:_signature_robustness_tag`).

---

## 2026-06-20 — orbit regeneration wired into the pipeline (was a manual pre-step; OQ-29 follow-up)
**Files:** python/run_pipeline.py, python/sweeps/regenerate_orbits.py
**Tier:** landed

`regenerate_orbits.py` wired into `run_pipeline.py` as sequential Phase 1b (operator ruling 2026-06-20:
regen every run — ~1.3s beats the recurring stale-orbits error); `manifest_inject`'s corpus_hash check kept as
the fail-closed backstop (OQ-29 Thread-C guard unchanged). Subprocess (the script `sys.exit()`s); sequential
to avoid racing the parallel Phase-2 analyses. Witness: pipeline 0 errors (was 1), `regenerate_orbits ok
[1.3s]`. Caveat: always exports the DEFAULT `testsets/` corpus.

## 2026-06-20 — within-kernel trifurcation router built + wired (OQ-55 resolved; OQ-53 within-kernel leg closed)
**Files:** prolog/cs_trifurcation.pl, prolog/json_report.pl, prolog/tests/test_cs_trifurcation.pl, prolog/stack.pl, ISSUES.md
**Tier:** landed

OQ-55 RESOLVED / OQ-53 within-kernel leg closed: new `cs_trifurcation.pl` (`cs_reading_trifurcation/3`)
routes within-kernel reading disagreement into the `debugging_philosophy.md` §6 trifurcation (Type A/B/C;
`unknown` fail-closed; singleton no-verdict); commentary-grade, serialized as `reading_trifurcation` in
`cs_kernel_comparison` and rendered by `enhanced_report.py`. Witnesses: `test_cs_trifurcation.pl` 8/8 (incl.
single-bit drift-ack discriminator + cross-kernel-leak control); live corpus all 9 multi-reading kernels
non-null (type_a×5, type_b×1, type_c×2, unknown×1). Re-scope ruling: the OQ-56 edge dropped (input-boundary
trace is the witness); transpose leg stays `blocked_on OQ-56`. Pre-existing `manifest_inject` staleness error
orthogonal (OQ-29).

## 2026-06-20 — kernel/reading orbit operator built + wired (OQ-150/OQ-53 Phase 3)
**Files:** python/orbit_operator.py, prolog/kernel_orbit_export.pl, python/run_pipeline.py, outputs/reading_orbits.json, outputs/kernel_orbits.json
**Tier:** landed

Cross-kernel orbit operator built + wired (commit `0c488468`): `orbit_operator.py` joins
`pipeline_output.json` (6 keys) + `kernel_obstruction.json` (from `kernel_orbit_export.pl`) →
`outputs/{reading,kernel}_orbits.json`; wired into `run_pipeline.py` dependency-ordered, non-critical, with a
same-run fail-closed n_constraints guard (positive-controlled). Two tripwires: live output is sparse BY DESIGN
(~3 multi-reading kernels — use `--twin haiku` for real orbit populations); only Tier-1 keys
(observer-signature 0.722, obstruction-class 0.734) are draw-robust — never cite a Tier-2 orbit membership as
a stable finding.

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

The `manifest_inject` `check_orbits_corpus_hash` staleness error (`run_pipeline.py:1133`) is EXPECTED after
every c-orchestrator run — non-critical (42/43 steps OK), live classification unaffected (runs on
`orbit_data.json`). Run `python3 python/sweeps/regenerate_orbits.py` before any sweep needing
`product_site_orbits.json`. Operator ruling 2026-06-19: orbits DECOUPLED, regen on demand. Lineage: OQ-29.
(Superseded 2026-06-20: regeneration was wired into the pipeline as Phase 1b — see that entry.)

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

OQ-149 root cause fixed: 423/466 haiku no-stakeholder stories had authored parties — the schema omitted
`stakeholders` from `required` and its description called it optional, contradicting the prompt prose
(diagnosis: `audits/2026-06-18_oq56_*`). Fix (commit `becd0f87`): CONDITIONAL `allOf` gate — non-empty
beneficiaries/victims ⇒ require `stakeholders` (minItems 1); a true no-party mountain stays EXEMPT.
Forward-only (existing corpus untouched); witnessed under Draft7 (example passes, example−stakeholders caught,
gravity exempt). Prompt-prose reinforcement is the operator's edit; the schema is the binding gate.

---

## 2026-06-19 — reading_diff un-stranded onto the live stakeholder-seat schema + stale test corpus
**Files:** prolog/reading_diff.pl, prolog/tests/test_reading_diff.pl, ISSUES.md
**Tier:** landed

reading_diff un-stranded (commit `01cff6a7`): `reading_cells/2` now UNIONS authored
`constraint_classification/3` (archives) with the live stakeholder-seat path (`stakeholder_context/3` +
`dr_type_for_stakeholder/3`) — haiku census went 0/0/954 vacuous → 136 binocular / 111 fragile / 707 MEASURED
coverage gaps; non-regressive on kernel_v1 (suite 10/10 with archive overlaid). Twin both-stakeholder pair
coverage 26% haiku / 61% flash → folds into OQ-149. Tripwire: `tests/test_reading_diff.pl` fixtures are
pre-reset westphalia — 7/10 FAIL on the default corpus; overlay `archives/datasets/kernel_v1` → 10/10
(stale-fixture repoint unfiled, candidate OQ). Filed under OQ-56 D1.

---

## 2026-06-18 — OQ-147 crash floor + OQ-148: classifications regression (corpus-wide producer break)
**Files:** python/audits/sheaf_audit.py, python/audits/tests/test_sheaf_audit.py, ISSUES.md
**Tier:** landed

OQ-147 RESOLVED (loud): `sheaf_audit.py:515` ZeroDivisionError on an empty working set fixed with one
`insufficient` predicate reused on three surfaces (JSON null rates + `verdict: insufficient_data`; the naive
0.0 fallback rejected as Pattern 5/6); fixture `python/audits/tests/test_sheaf_audit.py` 4/4 PASS; witnesses
pre-fix crash / post-fix exit 0. OQ-148 OPEN (the real bug): `pipeline_output.json` carries
`classifications: []` for all 80 constraints vs populated 2026-06-11 snapshots — a producer regression; the
quiet-consumer blast radius (~40 python files read the field) is OQ-148's spine and a candidate
Critical-Distinctions tripwire. Pointer: ISSUES.md OQ-147/OQ-148.

## 2026-06-18 — OQ-146: orbits metadata-key landmine — single-source `load_orbits_constraints`
**Files:** python/shared/loader.py, python/oracle_gap_analysis.py, python/game_theory_nash.py, python/sweeps/product_site_delta_sweep.py, python/sweeps/structural_config_sensitivity.py, python/tests/alt_power_transform_test.py, python/tests/alt_power_transform_test_3k.py, ISSUES.md
**Tier:** landed

OQ-146 RESOLVED: the OQ-29 top-level `corpus_hash` stamp crashed every consumer iterating
`product_site_orbits.json` keys as constraints (census with positive control → 6 exposed consumers). Fix: one
fail-loud `shared.loader.load_orbits_constraints` (partition-and-assert; `_ORBITS_METADATA_KEYS`; raises on any
unclassifiable top-level key); all 6 repointed (incl. the inline `7b5801f0` oracle_gap filter). Crash-over-drop
safe by producer construction (`product_site_export.pl:80–96` emits `contexts` unconditionally). Rule: never
raw `json.load`+`.items()` on orbits files; bump the metadata set AND the loader's hardcoded literal together.
Witnesses in ISSUES.md OQ-146; `sheaf_audit.py:515` out of scope (different class).

## 2026-06-18 — OQ-104: audit_citation_status.py built (standing checker, ungated)
**Files:** python/audit_citation_status.py, ISSUES.md, audits/2026-06-18_oq104_citation_checker/
**Tier:** landed

Built `python/audit_citation_status.py` (sibling of `issues_status.py`/`known_state_status.py`; NOT in
`scripts/gate.sh` until FP rate ruled): verifies audit-cited paths exist-and-tracked or allowlisted-ephemeral;
three WARN sublabels with distinct promote flags; a gitignored in-repo path is never allowlisted (it IS the
OQ-104 signature). Census: 1224 citations / 85 dirs; untracked-pending=35 (all `outputs/*`, operator ruled
leave-flagged non-gating); missing-pending-M=66, no live broken citation. Controls 23/23 + idempotence/
rot-sensitivity. Evidence: `audits/2026-06-18_oq104_citation_checker/FINDINGS.md`. OQ-104 stays open.

## 2026-06-18 — OQ-29 RESOLVED: corpus_hash single-sourced; 14 producers stamp; consumers fail-closed
**Files:** python/corpus_hash.py, python/run_pipeline.py, python/enhanced_report.py, python/sweeps/perturb.py, python/sweeps/census_sweep.py, python/sweeps/persistence_sweep.py, python/axiom_reachability.py, python/sweeps/epsilon_sensitivity.py, python/audits/metric_audit.py, python/audits/sheaf_audit.py, AGENTS.md, ISSUES.md
**Tier:** landed

OQ-29 RESOLVED: four silent-fork `_compute_corpus_hash` copies (Pattern 2) consolidated into
`python/corpus_hash.py` (identity witness: every path `d2b3ec9429f1`); 14 producers stamp (incl.
`persistence_sweep` + its `parents[2]`→`parents[1]` fix, and the 4 formerly scoped-out audit scripts);
consumers fail-closed (orbits presence→match upgrade; persistence/Fisher STALE surfacing, four-sided witness).
Commits `b6aefb5a`/`4ab980ff`/`7b016978`. Thread-D set-probe corrected the plan twice (only 2 clean deletes;
2 live write-only test producers kept). Convention promoted to AGENTS.md. Two pre-existing bugs surfaced, not
fixed here: `sheaf_audit.py:515`, `oracle_gap_analysis.py:143`.

## 2026-06-18 — OQ-115 RESOLVED: abductive_helpers phantom under [stack] fixed; check_stack back to 4-finding baseline
**Files:** prolog/stack.pl, prolog/signature_detection.pl, prolog/check_stack.pl, ISSUES.md (OQ-115, OQ-142/143/144/145)
**Tier:** landed

OQ-115 RESOLVED: the `abductive_helpers` phantom under bare `[stack]` (existence_error from
`signature_detection.pl:1624`) fixed by `:- use_module(abductive_helpers, []).` in `stack.pl`; Option 1
rejected by evidence (tighter import cycle than the in-file comment claimed; comment corrected). Class sweep
partitioned the remaining baseline findings (phantom × guarded × reachable) → OQ-142 parent +
OQ-143/144/145. OQ-145 RESOLVED same session: `drift_events.pl:175` wrong qualifier
`narrative_ontology:`→`domain_priors:` (witnessed THREW→SUCCEEDED_CLEAN, control-backed reachability).
check_stack baseline now 3 (was 4); OQ-143/144 remain annotate-only.

## 2026-06-18 — OQ-111 RESOLVED: dead data_repair omega bridge retired (zero-diff removal)
**Files:** prolog/data_repair.pl, ISSUES.md (OQ-111), docs/design/design_gaps.md (GAP-13)
**Tier:** landed

OQ-111 RESOLVED: `bridge_omega_variables_pure/3` keyed on the bare interval id vs `constraint_<id>` modules —
always imported zero omegas (Pattern 6; OQ-99's wrong-module twin). RETIRED, not fixed (operator ruled
archives out of scope; live corpus 100% paired, omegas render via `report_generator.pl:709`/`:776-794`);
also removed `bridge_v34_data/2` call, the dead persist clause, and the /5 fabricated-`empirical` defect.
Deferred capability → GAP-13 with re-introduction recipe. Witness: pre-removal no-op probe + ZERO DIFF on
three omega-authoring reports; dynamic suite 80/0/0; [GATE] GREEN.

## 2026-06-18 — OQ-48 recalibration-readiness audit: 0 thresholds recalibratable against the twins (all MODEL-CONFOUNDED)
**Files:** ISSUES.md (OQ-48), audits/2026-06-18_oq48_recalibration/, python/audits/oq48_threshold_distributions.py, python/audits/oq48_analyze.py, python/audits/oq48_triangulate_kernel_v1.py
**Tier:** landed

Read-only distribution-break audit of the 7 in-scope χ/ε/suppression cuts against the twins (960 each),
pre-registered verdict rule: **all 7 → MODEL-CONFOUNDED, 0 proposed values, no `config.pl` edit** (flash
antimodes fail bandwidth-robustness while tracking haiku's). Haiku alone corroborates `snare_chi_floor`
(0.66≈0.666) + `snare_epsilon_floor` (0.46≈0.484; the latter also cross-regime via the kernel_v1 arm).
Controls pass (LOADCOUNT 960/960/1106, byte-identical re-run, planted-gap 0.4506). OQ-48 stays open pending
a same-regime third corpus or the ~700-story live rebuild. Evidence: `audits/2026-06-18_oq48_recalibration/`;
twin TSV sha256 haiku `7039d37b…` / flash `3c24b1d2…`, metric-code commit `0a629077`.

---

## 2026-06-18 — OQ-122 CLOSED: physics-RED fixed by OQ-128; FSM victim-gate DROPPED, discriminant handed to OQ-138
**Files:** ISSUES.md, prolog/drl_core.pl (witness only, no edit), outputs/pipeline_output.json (witness)
**Tier:** landed

OQ-122 CLOSED: the physics false-RED is FIXED by OQ-128's type_1 discrimination — live witness at commit
`2172d55`: both physics controls read `verdict_join.verdict=yellow`, `cap_applied:none`, type_1 informational
at every seat. The held FSM victim-gate branch (`oq122-fsm-victim-gate`, `ab1e9b26`) DROPPED — superseded by
engine-ROUTES-never-RECLASSIFIES (OQ-128); its insight (vic=0→informational / vic>0→moderate) handed to
OQ-138 with the pre-witnessed discriminant
(`audits/2026-06-13_oq122_retype_discriminator/breadth_sweep_results.txt`). neutron_star/FCR stays under OQ-70.

---

## 2026-06-17 — OQ-128 type_1 cap RULED + BUILT: discriminated severity (withhold high-ε snare, route low-ε artifact)
**Files:** prolog/drl_core.pl, ISSUES.md (OQ-128)
**Tier:** landed

OQ-128 type_1 cap RULED + BUILT: the overloaded `severe` split in `drl_core.pl` — degrade→snare = `severe`
(RED floor, withhold), degrade→other = `informational` (routes via the sink); rests on a witnessed clean ε gap
(mountain-claimed snare-at-seat ε≥0.50 vs rope ε≤0.25, KILL=0 across six corpora ~7000). Acceptance: RED
389→102, all 10 v5 mountain-claimed snares STAY RED, `dr_type` byte-identical; type_3/type_5 `severe`
untouched. Tripwire: do NOT re-collapse to a single `severe` (re-launders genuine math/physics mountains into
RED). KILL: a mountain-claimed snare-at-analytical at 0.25<ε<0.50 → re-run the χ-decomposition.

## 2026-06-17 — OQ-128 routing sink BUILT (engine ROUTES the author↔engine diff, never reclassifies)
**Files:** prolog/routing_sink.pl, prolog/signature_detection.pl, python/run_pipeline.py, python/enhanced_report.py, ISSUES.md (OQ-128), audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md
**Tier:** landed

OQ-128 routing sink BUILT (`audits/2026-06-17_mountain_authoring_sweep/ROUTING_SINK_DESIGN.md`): (1)
`signature_detection.pl:867` natural_law→mountain overwrite RETIRED, detector left intact as a socketed router
input (behavior-neutral: `dr_type` + `dr_claim_mismatch` byte-identical); (2) `routing_sink.pl` per-SEAT
`seat_diff/7` router with seven typed MECE addresses → `outputs/routing_sink.json`; (3) wired into
`run_pipeline.py` Phase 2, rendered by `enhanced_report.py`. Tripwire: the leaf is per-SEAT — any collapse of
seats to one constraint verdict is the KILL (§9b.4). Controls reproduced the arc's witness files (thermo →
`engine_exit_table_review`; topological → `generation_gap` + `author_engine_divergence` + `authoring_review`).

## 2026-06-16 — Typed-absence corollary added to design canon + OQ-137 (reading-layer census)
**Files:** docs/design/design_discipline.md, ISSUES.md (OQ-137)
**Tier:** landed

Promoted the OQ-121 typed-absence convention to design canon: `design_discipline.md` §5 "Typed absence — a
reading's silence is itself a declaration" (aggregate-consumable readings return a typed token, never fail
silently; NOT "every predicate is total"). Templates: `constraint_signature/2`, `q6_cell/2`. OQ-137 minted to
census the whole reading layer against the convention (scope discriminator + positive-control requirement in the OQ).

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

OQ-121 follow-up: the two remaining partial-silent R3 commentary predicates totalized (zero blast radius — no
external consumers). `consensus_provenance/2` gains `no_agent_seats`/`seats_untyped`; live plural 37 /
no_agent_seats 21 / manufactured 8 / unanimous 6 (Σ=72). `seat_perceived_vs_real/4` returns
`Computed = untyped` (0 live triggers, 370 seats total). Regression `test_seat_totality.pl` 8/8;
commentary_census 40/40; oq86 14/14. OQ-136 minted (interpret the honest absence buckets; pre-registered
provenance-clustering test). Witnesses: `audits/2026-06-16_partial_silent_totalization/`.

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

OQ-134 RESOLVED: generic commentary-grade census — `commentary_census.pl` (multifile `commentary_cell/3` hook,
`commentary_absence_bucket/2`, `commentary_coverage_decidable/1`; sources q6 + extraction_reading) wired into
`run_pipeline.py` → `outputs/commentary_census.{json,md}` with a corpus-identity manifest. Design facts: the
sum invariant (Σ buckets == n_corpus ∧ n>0) is the contract enforcer; coverage = both-sides-MEASURED;
extraction coverage shipped null [SUPERSEDED same day by OQ-121 totalization → 1.0 over its 50-constraint
domain, prevalence 0.06]; absence buckets fail-closed (archives 100% `q6_unmeasured`). Extension = one
`commentary_cell/3` clause. Witnesses: `audits/2026-06-16_oq134_commentary_census/`; resolution in ISSUES.md OQ-134.

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

Read-only seat/orientation audit (`audits/2026-06-16_seat_invariant_vs_prolog/`, merges `c58611a8`/`864c961d`;
R3 probe merge `77e33bca`): `cs_pattern`/`cs_classify` is a pure function of authored presentation, audited
one-directionally → the engine votes ONE seat; the committer/CS axis is the orientation (showing) face, not a
second content-seat (R3 declaration = operator's seat). v8 design spec drafted rev3
(`docs/design/v8_seat_gauge_orientation_design_spec.md`, merges `403375e4`/`f6c22b81`/`1e81bc0f`):
seat/gauge/orientation + the transitive cross-axis taint invariant; DRAFT pre-implementation, adoption blocked
on operator. Tripwire (hard on v8 adoption): v7 "seat" = v8 "gauge" — use the spec's §4 bridge table. Operator
docs untracked: `docs/one_seat_audited.md`; `docs/provenance_is_not_proof.md` (NOT for commit).

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

`zombie_piton_crosscheck/2` GONE — replaced by `q6_crosscheck(C, Cell, Daylight)`, the full status×signature
matrix; commentary-grade (sole caller `report_generator.pl`; classification byte-identical by construction);
four non-verdict buckets kept distinct; `q6_cell` is a mode-robust if-then-else (an unguarded catch-all had
spuriously matched all 71 — caught by its own positive control). `q6_unclassified` witnessed 0 live, reachable
on twins (haiku=1, flash=5). Daylight axis (`founding_problem_corroboration_class/2`) SHIPS INERT pending a
bounded R5 backfill (OPEN graduation step). Audit: `audits/2026-06-16_q6_crosscheck_completion/`; tracking
OQ-83; deferred diachronic tier → OQ-133.

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

Three ADDITIVE observer site modes added to `site_contexts_for_mode/2` (`canonical_6`/`power_only_4`/
`power_only_6`, commit `a06b5c7f`; canonical/product byte-identical; canonical-first ordering is LOAD-BEARING
for the `(H¹₆−H¹₄)/9` conditioning — don't reorder). Finding (pre-registered,
`audits/2026-06-15_oq131_six_observer/`): the observed delta falls BELOW the permutation band on live/haiku/
flash → the new seats are consonant-suppressing; the combinatorial artifact is FALSIFIED; power-atom-driven,
bundle-robust. Config gotcha: every param needs a `config_schema.pl` spec or `[stack]` halts at load
(3 witnessed). OQ-131 stays `future` (Q2/Ω_C corpus-adoption deferred); scope walls: H⁰/H¹ only, seat-bundle-dependent.

---

## 2026-06-15 — OQ-108 resolved: per-position witness coverage shipped; OQ-107 closed `future`; new `future` status token
**Files:** prolog/stakeholder_seats.pl, prolog/json_report.pl, python/tensions_ledger.py, python/issues_status.py, ISSUES.md
**Tier:** landed

OQ-108 RESOLVED: per-position witness coverage over the 6-atom power vocabulary
(`power_witness_count/3`/`power_witness_map/2`, reusing `canonical_d_for_power/2` as enumerator; serialized as
`perspective_witness` 64/64; rendered in the tensions ledger — a 0 = inference-only, Pattern-6 zeros SHOWN;
witness: `geopolitical_settlement_competition`). Tensions ledger now suppresses the grid line when fully
absent (report .md generators deliberately unchanged). New status token `future` (operator ruling 2026-06-15)
added to `issues_status.py` + the ISSUES.md footer grammar; OQ-107 closed `future` and its wrong
`blocked_on` dep on OQ-108 dropped.

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

Omega-resolver pilot validated: `python/omega_resolver.py` (loader / authority control / SCC frontier /
checker / selftest 8/8), read-only, NOT a pipeline gate. §8 re-witnessed — `extraction_blindness` is an
existential-labeling artifact (live 16/20=80%, haiku 258/358=72.1%) → landed into OQ-129 OPEN-A
(`audits/2026-06-14_extraction_blindness_existential_label/`). §E verdict: 57 confirm / 7 contradict /
0 standoff, each contradict settled by an external fact — pilot criterion met. `blocked_on_human` relator
added; 16 `Deps:` edges authored; `issues_status --check` intact (129). OQ-130 minted (corpus scale arm,
gated on an omega-soundness spot-check). Evidence: `audits/2026-06-14_omega_resolver_pilot/`.

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

OQ-50 closed: `forensic_explain_false_mountain/2` headlines the post-signature `dr_type` (heuristic demoted to
a metric-level annotation; fail-closed unbound guard); `type_3`/`type_5` (`drl_core.pl:622,629`) lead with
`standard_context` + cut dropped (unbound-Ctx trap gone; caller census clears the multiplicity falsifier;
validation_suite 57/0). OQ-74 core RULED reading-relative (coordination_type a seventh authored field; the 55%
sibling disagreement is signal); OQ-49 hand-up limb MOOT (coord=0 subset positive-controlled empty —
`audits/2026-06-14_oq49_remeasure/coord0_conjunction_positive_control.txt`). OQ-122 fixture-blocker found
STALE (gate adds zero new failures; evidence `audits/2026-06-14_oq122_fixture_triage/`); OQ-128 minted; OQ-122 stays open.

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

OQ-49 SPLIT-CLOSE (plan `review-oq-49-in-issues-md-twinkly-mochi.md`): the (a)/(b) ruling was un-answerable as
posed (testsets_3000 dead since the reset; FNL bait driver deleted, OQ-70 `72ec2cdd`); re-measured read-only
on live corpora. Collapse witness is STRUCTURAL: every FNL firing tags source-1, zero source-2/unaccounted
(kill condition not triggered); FNL override-effective 0/6/8; the dominant live override is
`false_ci_rope→tangled_rope` (~10×); the 14-firing residual all carry coord+asym (coord=0 laundering subset
EMPTY, positive-controlled) → handed to OQ-74. Citation qualifier: never cite the testsets_3000 1730/1661
numbers as live. Evidence: `audits/2026-06-14_oq49_remeasure/`; OQ-49 resolved.

## 2026-06-13 — Twin cross-model comparison harness + two generation-quality fixes (classify_corpus driver; Fix A axiom-status, Fix B sibling snap)
**Files:** python/run_pipeline.py, python/story_repair.py, agent/generate_kernel_corpus.py, python/audits/twin_comparison.py, audits/2026-06-13_twin_comparison/
**Tier:** landed

Plan `federated-toasting-sedgewick.md` landed in four commits: Fix A (prompt offers only
`holdable`/`overridden`; `story_repair.py` coerces out-of-enum, counts + escalates — forward-only, twins
unaffected); Fix B (`snap_sibling_id()` unique-confident snap, else quarantine per OQ-58, never
wrong-snapped); B1 `classify_corpus(corpus_path, output_name, expected_model)` in `run_pipeline.py`
(non-default-corpus driver with zero-glob / load-complete / model-fingerprint / stale-raw refusals; canonical
outputs untouched). B-result (`audits/2026-06-13_twin_comparison/`, twins classified at commit `8126231`,
N=1000 permutations): H1 structural — all 7 fields HOLD; H2 drift FALSIFIED (below-band result EXPLORATORY
only, earlier over-claim corrected). Forward work → OQ-123/OQ-124/OQ-125.

## 2026-06-13 — Branch cleanup: merged oq117-evidence-block into main; landed the China-legitimacy topic-run artifacts; gitignored *.pdf
**Files:** KNOWN_STATE.md, ISSUES.md (merge), .gitignore, prolog/testsets/{demographic_resource_allocation,livelihood_security_reading,performance_legitimacy_contradictions,performance_legitimacy_flat_control,property_sector_overhang,qualitative_development_reading,quantitative_growth_reading,techno_nationalist_reading}.pl, json/ (7 matching), essays/2026-06/captive_on_both_ends_v3.md
**Tier:** landed

Merged `oq117-evidence-block` (diverged at `f3f347fe`) into main `--no-ff`; only conflict KNOWN_STATE.md (both
dated sections kept); `issues_status.py --check` 120/0. Committed the China-legitimacy topic-run artifacts
(8 testsets + 7 json + `essays/2026-06/captive_on_both_ends_v3.md`); discarded stale local edits to the two
pipeline-regenerated files; `.gitignore` now excludes `*.pdf`. Branch deleted post-merge. NEXT STEP (operator's
call): run `python3 python/run_pipeline.py` — the 8 testsets were committed generate-only, pipeline outputs
stale w.r.t. them until a run.

---

## 2026-06-13 — Two-model TWIN CORPUS: full never-generated rebuild (Haiku, 988) + Gemini Flash twin (971) reconciled into testsets_haiku/ + testsets_flash/ + testsets/ (branch corpus-rebuild-fresh, merged to main)
**Files:** agent/run_no_scope_gemini.py, agent/_pilot_ladder_strip.py, agent/generate_kernel_corpus.py, prolog/testsets_haiku/, prolog/testsets_flash/, prolog/testsets/, json_haiku/, json_flash/, prolog/beta_processed_flash.txt, ISSUES.md (OQ-75), CLAUDE.md (Corpus Loading)
**Tier:** landed

On branch `corpus-rebuild-fresh` (five-defect provenance fix cherry-picked `2e3e1998`→`dc12bf5a`): full
never-generated pool (1005 readings / 331 kernels — not the remembered 304/101) → Haiku 988/1005 (~$27 batch)
+ Gemini Flash twin 971 via `agent/run_no_scope_gemini.py` (faithful port, adapter-shaped, thinking_budget=0);
reconciled INTERSECTION = `testsets_haiku/` (960) + `testsets_flash/` (960); `testsets/` (44) reserved for the
orchestrator corpus. All five provenance defects held at scale; one grid-gate firing regenerated, not waived.
Tripwire promoted to CLAUDE.md Corpus Loading: overlay `corpus_path` with `asserta`/`retractall`-first, never
plain `assertz` (silently ignored — witnessed 44-vs-960). Residuals (ISSUES OQ-75): 17+34 redraws, dominant
cause the `status:'contested'` enum violation; run_pipeline's JSON_DIR hardcoded to `json/`.

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

OQ-109 RESOLVED: gated replicate spend executed (15 draws = 5 contested kernels × 3, sonnet-4-5 @ temp 0.2,
batch `msgbatch_01UbfPq13BcHgJKxcsqK549i`, commit `dcfaea97`; frozen seed-spec so SIGMA_SEAT_PREDICTION
`5f2a626c` applies; Fisher instrument validated vs scipy before use). σ/seat partition FAILED its
pre-registered falsifier: 188 cells, 47.87% consistent, Fisher two-sided p=0.649 = NO SEPARATION. Operator
split ruling: ROBUST (apparatus-presence bucketing + scoped null) vs CONFOUNDED-HELD (cast/σ exact-match;
verdict-stability n=6/temp confound) with graduations. META-FINDING: draw-stability tracks
FIELD-CONSTRUCTION-TYPE, not the σ/seat line → discharged to OQ-118 (filed). Evidence: `audits/2026-06-12_cohort_zero/`.

## 2026-06-13 — OQ-109 Phase C analytical tail CLOSED to partial: population correction (Iran pair → separate cohort, n=7→n=5) + stability/σ-seat instruments wired & witnessed; two named residuals (gated σ/seat spend, cohort-one reading_diff)
**Files:** prolog/testsets/ (n=5 restored), prolog/archives/datasets/iran_essay_2026-06-11/, python/cohort_stability.py, python/cohort_sigma_seat_eval.py, ISSUES.md, audits/2026-06-12_cohort_zero/
**Tier:** landed

Phase C wire-only close (branch `oq109-phasec-closeout`; WRITEUP `audits/2026-06-12_cohort_zero/WRITEUP.md`).
Population corrected: the two untracked Iran-essay stories (different generation regime, NOT
cohort-zero-homogeneous) archived to `prolog/archives/datasets/iran_essay_2026-06-11/` (commit `d26d04a2`,
byte-identity proven), corpus restored n=7→5 (manifest `1f517a0`); Iran-count fork closed positive-controlled.
Instruments landed (commit `1f517a08`): `cohort_stability.py` (Pattern-5 absence-split; selftest PASS) +
`cohort_sigma_seat_eval.py` (zero-drift parse of the frozen prediction; REFUSES verdicts below 3 stories × 2
draws). Two named residuals (status partial): the gated σ/seat replicate spend; cohort-one `reading_diff`
re-point (no live positive control until a stakeholder-cell story lands).

---

## 2026-06-12 — design_discipline v1.3: §9 recorded — engine's pipeline seat is discovery not justification; no-verdict-skips-adjudication; benign-constraint bias control independently re-derived
**Files:** docs/design/design_discipline.md, essays/2026-06/marked_to_market.md
**Tier:** landed

§9 recorded in design_discipline.md (v1.2→v1.3): the engine sits in the context of discovery —
contribution is well-formed questions, not calibrated scores; standing condition: no verdict skips
adjudication; surviving risk is systematic bias (proposed benign-constraint control independently
re-derives open item (b)). Wiring-state claims in §9 attributed to the review, not independently
witnessed; stray marked_to_market.md:Zone.Identifier artifact removed from essays/2026-06/.

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

RETIRE ruled + landed: `structural_coercive_intent` top verdict deleted (range-dead, producerless,
and consumerless — report_generator.pl:22 imports intent_engine except classify_interval/3);
capture-as-design ratified as the piton intension (constraint_captured/1), kill condition recorded
in the OQ-106 close arming GAP-08 revival; option (ii) declined (OQ-36 misread risk). Witness
(Pattern 3): full suite before/after byte-identical on substantive lines (5 [INTENT] lines); GAP-08
stale residual paragraph updated to the 2026-06-11 fail-closed ruling. Worktree oq106-retire from
`f3f1e99f`; evidence audits/2026-06-12_oq106_retire/.

## 2026-06-12 — OQ-105 RESOLVED: operator ruled fork (a) ALONE; alignment rule landed (prompt + fail-closed validate_json gate); live exposure 0 after the cohort-zero swap retired all 11 hosts
**Files:** ISSUES.md, prompts/constraint_story_generation_prompt_json.md, python/generate_constraint_pl.py, audits/2026-06-12_oq105_alignment_gate/
**Tier:** landed

Operator ruled fork (a) ALONE — grid alignment at generation: prompt rule "One time grid per story"
+ fail-closed `_grid_alignment_errors` in validate_json (both jsonschema and fallback paths); the
cohort-zero swap (`7ca48e0b`) retired all 11 hosts → live misaligned rows 0. Witnesses: W1 synthetic
misalignment fires; W2 5/5 live _c0 JSONs clean; W3 gate over the 60 archived JSONs flags EXACTLY
the 11 known hosts, 0 false positives. Reopen conditions for (b) recorded in the OQ; worktree
oq105-alignment-rule; evidence audits/2026-06-12_oq105_alignment_gate/.

## 2026-06-11 — OQ-105 per-row sweep: PREDICTED bucket discharged — 4/23 misaligned rows timing-distorted, all one snare-floor mechanism; fork ruling still open
**Files:** ISSUES.md, audits/2026-06-11_oq105_row_sweep/
**Tier:** landed

Interpolation counterfactual over all 23 grid-misaligned suppression rows: 4/23 rows (181/3588
cells, 5.0%) timing-distorted, all the one predicted mechanism (endpoint scalar ≥ snare suppression
floor 0.60, interp below → snare dated early); 19/23 substitution-robust at every context; OQ-105
(a)/(b) fork left open, (b)'s live payoff bounded to the 4 rows. Worktree oq105-row-sweep from
`37ea069f`; evidence audits/2026-06-11_oq105_row_sweep/.
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

Pilot 7/7; swap executed — live corpus = 5 _c0 stories, pre-cohort set retired to
prolog/archives/datasets/kernel_v2_test2/ (renamed from pre_cohort_zero_2026-06-12, manifest carries
both names). Trio falsifier RESOLVED: filters on the new regime (1/4 mountain-claims certify);
trust_erosion_c0 excluded AND chain-false; C-arm first live decisions witnessed
(battery_witnesses.out); OQ-116 filed (scaffold-zone calibration, MOUNTAIN_METRIC_CONFLICT).
Pipeline green n=5 (manifest 2026-06-12T17:48:34Z); remaining OQ-109 tail: reading_diff re-point,
stability table, σ/seat eval (frozen prediction `5f2a626c`), close-out. Evidence
audits/2026-06-12_cohort_zero/.

## 2026-06-12 — DETERMINISM-FRONTIER ruling promoted to CLAUDE.md; Phase C removal commit (schema perspectives[]/mandatrophy_resolved OUT, provenance/8 REQUIRED incl. model+sampling); archive-before-removal executed; replicate probe folded into cohort zero
**Files:** CLAUDE.md, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/narrative_ontology.pl, prolog/guard_exclusions.pl, prolog/signature_detection.pl, prolog/stack.pl, agent/example_platform_commission.json, ISSUES.md, prolog/archives/datasets/pre_cohort_zero_2026-06-12/
**Tier:** landed

Ruling promoted to CLAUDE.md Critical Distinctions: generation NEVER reproduces; committed JSON is
the CHECKED determinism frontier; regens are NEW DRAWS; attribute same-material-different-results by
stage-hash diff, never assumption (record check witnessed OQ-26, the press/Reformation triplet,
OQ-112 class). Phase C removal commit: perspectives[]/mandatrophy_resolved out of schema,
provenance/8 REQUIRED (model + sampling_params); archive-before-removal executed
(prolog/archives/datasets/pre_cohort_zero_2026-06-12/: 62 pl + 60 json, schema-pinned at
`046e0a40`); replicate probe folded into cohort zero. Witnesses W1–W5
(c_removal_commit_witnesses.out) + GATE-0 W1–W3 (c_gate0_exclusion_witnesses.out).

## 2026-06-12 — OQ-114 RESOLVED: archive probe under frozen criterion → OUTCOME 3 (mixed) → operator ruled the live 3 SPLIT (2 in / trust_erosion out, kill conditions + fail-closed exclusion + named re-witness); rider: no-beneficiary conjunct WRONG
**Files:** ISSUES.md, audits/2026-06-12_oq114_archive_probe/
**Tier:** landed

Archive probe under frozen criterion (`c64f32a6`): kernel_v1 41 / v6 430 mountain-claimed → OUTCOME
3 (both duplicate-seat artifact and substantive distinct-seat dissent in both archives; NL trio
filters NOTHING on archives — C ≡ claim-mountain there). Operator ruled the live 3 SPLIT:
organization_floor + demographic_skill_mismatch IN (named re-witness at Phase C);
institutional_trust_erosion OUT with kill conditions both directions + a FAIL-CLOSED per-story
exclusion as the Phase C build item. Rider: option 4's no-beneficiary conjunct was WRONG, not over-
restrictive. Evidence audits/2026-06-12_oq114_archive_probe/.

## 2026-06-12 — OQ-109 B4 gauntlet PASS against a pre-compiled expected-divergence manifest; Phase C ordering pinned (OQ-114 first); OQ-115 filed (check_stack divergence attributed pre-Phase-B)
**Files:** ISSUES.md, audits/2026-06-11_oq109_phase_b/B4_EXPECTED_DIVERGENCE_MANIFEST.md
**Tier:** landed

Gauntlet PASS against the pre-compiled manifest
(audits/2026-06-11_oq109_phase_b/B4_EXPECTED_DIVERGENCE_MANIFEST.md): pipeline green, plunit 14/14,
rows 1–10 reconciled; the one unmanifested check_stack finding (abductive_helpers phantom-module
under [stack], OQ-57 class, present at pre-Phase-B `c22ec561`) attributed → OQ-115, not Phase-B-
attributable, non-blocking. Phase B COMPLETE; Phase C ordering pinned: OQ-114 ruling → C-arm
extension confirmed → regen.

## 2026-06-12 — OQ-109 B3: empty-table census CLOSED (A1–A6, B1–B3 all discharged); narrative_ontology A3/A4 detectors retired; linter migrated to agent-surface dispatch; gaps key carries coverage bit
**Files:** prolog/narrative_ontology.pl, python/linter.py, prolog/test_harness.pl, prolog/json_report.pl, prolog/report_generator.pl, python/shared/schemas.py, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

Census A1–A6/B1–B3 all discharged (closure table b3_empty_table_census.md,
audits/2026-06-11_oq109_phase_b/): five zero-consumer narrative_ontology detectors retired (grep
positive-controlled, successors named; has_mandatrophy_declaration KEPT); linter migrated to agent-
surface dispatch (B2 example lints 5→0; corpus sweep 92→80 fully decomposed); A5 gaps made nullable
(null=didn't-look vs []=measured-empty; the enrich validator caught it loudly first); A2/A6 carry
ran-witnesses. Remaining B3: none — next is B4, then Phase C.

## 2026-06-12 — OQ-109 B3: R5 zombie consumer LANDED (A7 seam recovered, first consumer of zombie_piton_crosscheck/2); CLAUDE.md mandatrophy note retired per its own condition; presence gates + emission seam landed same day
**Files:** prolog/report_generator.pl, prolog/data_validation.pl, python/generate_constraint_pl.py, CLAUDE.md, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

r5_zombie_crosscheck_line/1 landed as the FIRST consumer of
stakeholder_seats:zombie_piton_crosscheck/2; pre-registered witness shape held exactly (6 live
firings, quiet control clean, pipeline JSON untouched; corroborated_zombie=0 witnessed ONLY by the
overlay control). CLAUDE.md mandatrophy tripwire RETIRED per its own condition; presence gates (5
two-sided controls) + census-B1 emission seam landed same day. Residual: mandatrophy_resolved still
a dangling schema field until Phase C. Gotchas: Section 7 is subject-scoped; data_validation NOT
loaded by [stack]. Evidence audits/2026-06-11_oq109_phase_b/.

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

Both named criterion candidates FAILED the pinned gauntlet (natural_law_signature unsatisfiable by
construction → OQ-113); operator-ruled option-2 bridge landed: authored-cells arm first (dies at
Phase C, named retirement point) ∨ fail-closed nl_certification_chain/1. Extension 9/62 = old 6 + 3
(OQ-114 adjudicates the 3). Output-changing commit alone: institutional_trust_erosion FCR un-fired
(→ coupling_invariant_rope), 3 maxent_top_type piton→rope, regulatory_measurement_gap verdict_join
yellow→red; 57-story ensemble cascade determinism-controlled (same-code rerun byte-identical).
Gotchas: ε lives in domain_priors:base_extractiveness/2; emerges_naturally is static+multifile
(overlay via consulted scratch testset). Evidence audits/2026-06-11_oq109_phase_b/.

## 2026-06-11 — OQ-109 Phase B1+B2 LANDED: prompt cutover to stakeholder surface; new one-shot example (FNL statistics reset No. 2); schema/compiler perspectives-optionality (guard-not-delete)
**Files:** prompts/constraint_story_generation_prompt_json.md, agent/example_platform_commission.json, agent/story_generator_base.py, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, ISSUES.md, audits/2026-06-11_oq109_phase_b/
**Tier:** landed

B1: prompt cutover to the stakeholder surface (P/T/E/S tuple + Indexed Classifications dropped,
1008→872 lines; 11 tuple terms 0 post — b1_vocab_grep_witness.out). B2: new one-shot example
app_store_commission (FNL statistics reset No. 2; minimum-prevalence pick, example_prevalence.out;
mutated per EXAMPLE_INHERITED_SIGNATURES.md); B2 changed perspectives OPTIONALITY only (guard-not-
delete; existing corpus compiles byte-identical). Known pre-B3: linter fires on the example
(b2_example_validation.out); 12/60 live-paired JSONs fail schema in BOTH states
(b2_schema_failset_diff.out). Worktree oq109-phase-b; evidence audits/2026-06-11_oq109_phase_b/.

## 2026-06-12 — OQ-103 RESOLVED: contamination-edge provenance made load-bearing + count-based salience floor at the read site
**Files:** ISSUES.md, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_contamination_provenance_salience.py
**Tier:** landed

Scope-corrected at close: the provenance bit already existed (`edge_type == explicit` is the
authored-vs-derived bit); defects were an inert bit + no salience floor. Read-site fix: json_report
emits shared_agent_count per neighbor; enhanced_report gains Provenance/Salience columns + legend +
floor (authored always salient; derived agent edge iff count≥2; inferred_coupling strength≥0.6).
Witness: pipeline n=62, 82/106 (77%) edges demote to low-salience; unit test 5/5. Commit `ded4969d`
(merge `1bb6e535`); synthesis enforcement stays OQ-101.

---

## 2026-06-11 — OQ-112 item-4 sentinel trace: verdict SILENT (three mechanisms); absorber-boundary class elevated to item 2; maxent_indexed_run order dependency found
**Files:** ISSUES.md, audits/2026-06-11_oq112_item4_sentinel_trace/, prolog/maxent_classifier.pl, prolog/json_report.pl
**Tier:** landed

Verdict SILENT via three absorber mechanisms: catch-true (json_report.pl:72/:76,
trajectory_mining.pl:912), catch-fail row drops (maxent_report.pl:211, maxent_diagnostic.pl:395),
and clause-failure-before-arithmetic (W12a — the sink a catch-grep cannot see); firing set EMPTY on
the live corpus. Absorber-boundary class elevated to OQ-112 item 2; maxent_indexed_run hidden order
dependency on maxent_run found. Tripwire: maxent_profile/4 is empty until maxent_run(Ctx) runs in-
process — witness profile-present before trusting a sink result. Worktree oq112-item4-trace from
`009c793a`; evidence audits/2026-06-11_oq112_item4_sentinel_trace/.

## 2026-06-11 — OQ-97 RESOLVED: Pattern-6 census executed (160/227/210 raw lines, 19 classes); 8 candidate classes filed as OQ-112; classification path clean
**Files:** ISSUES.md, audits/2026-06-11_oq97_pattern6_census/
**Tier:** landed

Bounded grep census over 106 top-level prolog/*.pl (160/227/210 raw lines, 19 classes; all 7 pinned
positive controls fired — two grep iterations rejected by them). No confirmed candidate on the
dr_type path (drl_core.pl zero Shape-A hits — the census itself witnesses the OQ-44 commit-C fix); 8
candidate classes filed as OQ-112 (top: diagnostic_summary absence-of-alert, 13 sites, feeding the
OQ-98 join; item 4 = the unknown sentinel into maxent). Worktree oq97-pattern6-census from
`1bfd0b72`; evidence audits/2026-06-11_oq97_pattern6_census/.

## 2026-06-11 — OQ-110 RESOLVED: residual join + pinned counterfactuals; operator ruled D-fork branch b NO-OPEN (derived-d stands); Backed deposit chain discharged
**Files:** ISSUES.md, python/audits/oq110_residual_join.py, audits/2026-06-11_oq110_residual_join/, prolog/temporal_residual.pl, prolog/drl_composition.pl, prolog/json_report.pl
**Tier:** landed

Backed verified end-to-end — the OQ-33 → OQ-46 → OQ-83 → OQ-110 deposit chain TERMINATES; join
coverage both=11/62, flips_only=23, stages_only=4; all 91 backed flips pinned: 82 ε-explained / 9
supp-explained / 0 genuinely unexplained. Operator ruled D-fork branch b NO-OPEN (derived-d stands;
reopen = a backed flip surviving BOTH pins on a future join). Fresh pipeline manifest
2026-06-12T00:59:49Z at `c22ec561` (prior dirty-tree `25d6a637`; flip totals identical). Gotcha:
json_report.pl is a NON-module script — predicates live in user. Evidence
audits/2026-06-11_oq110_residual_join/.

## 2026-06-11 — OQ-99 + OQ-100(a–c) RESOLVED: omega scenarios render authored protocols (subject-bound, fail-loud); report register coherence (qualified confidence labels, rival-P-graded disagreement, self-consistency header)
**Files:** prolog/report_generator.pl, python/enhanced_report.py, python/enrich_pipeline_json.py, agent/orchestrator.py, ISSUES.md
**Tier:** landed

OQ-99: omega scenarios render authored 5-arity protocols, subject-bound + fail-loud
unresolved_source (never Constraint: unknown); plan-correction kept — the facts live in
constraint_<id> modules, NOT user. OQ-100(a–c): qualified confidence labels, rival-P-graded
disagreement header (BAND_DEEP/BAND_MODERATE constants in enrich_pipeline_json.py), fraud header →
DECLARED-TYPE self-consistency; orchestrator regex updated and verified. Commits `6b1092c0` +
`e9872538` (worktree oq99-omega-scenarios); wrong-module sweep filed one finding as OQ-111;
OQ-100(d) subsumed by OQ-101.

---

## 2026-06-11 — OQ-83 RESOLVED: measurement close-out; snapshot_type determinism guard; v7 §4.5 (A)/(B) census; OQ-109/OQ-110 filed
**Files:** ISSUES.md, prolog/transition_paths.pl, docs/deferential_realism_paper_v7.md, audits/2026-06-11_oq83_close/
**Tier:** landed

Operator-gated measurement close-out: R4 ruled SATISFIED (n=6 pilot diff = produced-and-preserved);
Ω_P transferred, not answered (observer Type-B foreclosed per TWO_AXIS; committer C/B → OQ-87).
Classifier-sync item 5: snapshot_type/3 now clears the classify_at_time nb-globals at entry
(determinism-fix-plus-document; witnesses + controls pasted, suite 0 warnings); new ε-sourcing
mismatch challenge_as_commons_maintenance T=5 recorded. v7 §4.5 amended: one (A) data bridge vs ≥3
(B) seam diagnostics. Spin-offs OQ-109 + OQ-110; census substrate = archives/datasets/kernel_v2_test
(archived at `00c639da`). Evidence audits/2026-06-11_oq83_close/.

## 2026-06-11 — Pew-typology review exchange landed: hedging-as-rigor dual, false-summit authoring discipline, OQ-107/OQ-108 filed, OQ-103 escalated
**Files:** docs/technical/build_discipline.md, CLAUDE.md, docs/design/design_discipline.md, ISSUES.md, prolog/testsets/institutional_trust_erosion.pl
**Tier:** landed

Landed: hedging-as-rigor (the under-confident dual) → build_discipline.md + CLAUDE.md synthesis item
(4); false-summit authoring discipline → design_discipline.md §4 (witness:
institutional_trust_erosion.pl:125 authored constraint_claim mountain, engine refused — the refusal
became the essay's spine); OQ-107 (survey-wave witness adapter) + OQ-108 (per-position witness-
coverage report) filed; OQ-103 escalated to load-bearing (trust↔representation shared_victim edge is
corpus-topology, institutional_trust_erosion_report.md:142); "the mint" queued as an OQ-69 ledger
item. Source: agent/analysis/originals/Pew_2026.5.10_political-typology_topline.txt.

## 2026-06-11 — OQ-90 RESOLVED: capture-keyed piton refinement in the FCR branch (piton un-darkened)
**Files:** prolog/signature_detection.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/signature_mapper.pl, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

Piton un-darkened: capture-keyed refinement in the FCR branch — piton_candidate/1 (uncaptured ∧
prohibitive fixing_cost) gates a new resolve_with_perspectival_check clause; dr_signature stays
false_ci_rope, only dr_type becomes piton; `piton_refinement_enabled` is a SEPARATE axis from
fcr_override_enabled (dedicated kill-switch). Output delta 0→1: exactly 2 rows tangled_rope→piton
(regulatory_measurement_gap, institutional_trust_erosion); the 4 untracked working-tree testsets
must be committed for the 2-row result to reproduce. Superseded-pending: drl_core.pl:344,403 theater
piton clauses, maxent_classifier.pl:153–155 default_profile, axiom_reachability.py:171,207. Commits
`f2368073`/`64448411`/`fc724ab2`/`3a4e0209`; audit audits/2026-06-11_oq90_piton_refinement/.

## 2026-06-11 — OQ-44 RESOLVED: fail-closed-on-absence ruled (statute for new gates, marker carve-out, common-law for existing); OQ-43 closed; thermal_dissipation_constraint un-certified
**Files:** prolog/signature_detection.pl, prolog/drl_core.pl, python/shared/schemas.py, ISSUES.md
**Tier:** landed

Ruled: STATUTE — new/modified gates fail closed on absence; carve-out — absence→authored provenance
only via positive-control inference at authoring/compile time (suppression_profile precedent), never
emptiness-inference at the read site; existing gates common-law per-instance. Dispositions:
has_viable_alternatives false→unknown (`8b5a34b8`, output-changing — thermal_dissipation_constraint
UN-CERTIFIED, verdict green→red, all 277 diffs single-cause); get_raw_suppression 0→unknown sentinel
+ number/1 guard (`966d53c8`; shared/schemas.py suppression nullable); report-layer 0.0 defaults
conforming as-is. OQ-43 resolved in the same stroke. Witnesses audits/2026-06-11_oq44_policy_close/.

## 2026-06-12 — First-contact gate C-range corrected: slot-count!=32 removed (partial grids are LEGAL); first misfire had halted the pipeline on an OQ-90 flip target
**Files:** python/grid_first_contact_gate.py, python/grid_audit_ledger.json
**Tier:** landed

slot-count!=32 removed from the standing gate (the BATCH addendum's full-grid mandate had leaked in;
partial grids are operator-confirmed LEGAL): C-range = value outside [0,1] OR duplicate slots;
C-flat evaluates the slot-groups present; partial grids pass with a coverage field + prompt-
compliance NOTE. The misfire had HALTED run_pipeline on institutional_trust_erosion (Pew run, 12/32
all-valid — an OQ-90 flip target); now passes as legal partial, OQ-90's two-row delta preserved;
pipeline exit 0 on the 62-corpus. Witness 6/6:
audits/2026-06-12_gate_partial_fix/gate_partial_fix_witness.txt.

## 2026-06-11 — OQ-93 FLIP RULED + EXECUTED: live prompt opt-in grid section; κ gate → first-contact gate; 10 batch stories promoted (corpus 48→58); two latent defects found by promotion
**Files:** prompts/constraint_story_generation_prompt_json.md, prompts/grid_batch_addendum.md, python/grid_first_contact_gate.py, python/grid_audit_ledger.json, python/run_pipeline.py, python/python_test_suite.py, prolog/data_repair.pl, prolog/validation_suite.pl, json/, prolog/testsets/
**Tier:** landed

Flip ruled + executed: live prompt opt-in grid section; the one-time κ gate became the standing
FIRST-CONTACT gate (per-story fail-closed, ledgered in python/grid_audit_ledger.json; C-echo halts
run_pipeline); 10 batch stories promoted, corpus 48→58 — first live-corpus grid consumption
(witnesses first_contact_gate_witness.txt, flip_promotion_witness.txt, flip_promotion_suite.txt).
Two latent defects found by promotion and fixed: data_repair:grid_provenance read measurement/5 with
the interval ANONYMOUS (56/58 cross-reads once ten grids coexisted; now interval-scoped) and
python_test_suite's unanchored interval regex matched prose (phantom test IDs; now anchored to the
compiled fact form). TRIPWIRE: every pre-promotion 0-diff baseline is of-its-substrate (143→153
json, 48→58 corpus) — re-run before reuse.

## 2026-06-11 — OQ-93 grid migration LANDED end-to-end (stages A–D + coverage read + shim retirement); OQ-96/OQ-101/OQ-102 closed with it; intent sub-fork filed as OQ-106
**Files:** schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prolog/coercion_projection.pl, prolog/pattern_analysis.pl, prolog/intent_engine.pl, prolog/report_generator.pl, prolog/signature_detection.pl, prolog/drift_report.pl, prolog/diagnostic_summary.pl, prolog/json_report.pl, prolog/narrative_ontology.pl, prolog/config.pl, prolog/config_schema.pl, prolog/scenario_manager.pl, prolog/data_repair.pl, prolog/data_verification.pl, prolog/domain_priors.pl, python/enhanced_report.py, python/run_pipeline.py, python/domain_priors.py, python/shared/schemas.py, python/tensions_ledger.py, agent/c-orchestrator.py, agent/generate_grid_batch.py, prompts/grid_batch_addendum.md
**Tier:** landed

Stages A–D landed end-to-end: schema coercion_grid + basis rider; compiler *_grid_NN emission with
fail-loud integrity (143/143 byte-identical); coverage read — system_gradient's []→0.0 fabricated
default KILLED, empty reads → OPEN; Stage C N=10 batch PASS 0/10 excluded (gradient compound-guard
bug fixed en route); Stage D level_gradient_divergence wired into FCR/FSM + extraction-blindness
omega; shim retirement closes OQ-96 (per-class counts identical, justified-wording diff recorded).
OQ-102 closed (basis chain + drift-severity confidence) and OQ-101 closed (tensions_ledger.py
replaces _step_essay); intent sub-fork → OQ-106. Pending-at-entry: the live-prompt flip (batch
parked in audits/2026-06-11_oq93_grid_migration/grid_batch/). Audit package
audits/2026-06-11_oq93_grid_migration/; branch oq93-grid-migration, commits `bc41e8f4..`.

## 2026-06-11 — Backed semantics BUCKETED (follow-on to the OQ-46 close): compiler-stamped suppression_profile(static) sanction marker; OQ-105 filed; OQ-37 piton vacuous-green fixed
**Files:** prolog/drl_composition.pl, prolog/narrative_ontology.pl, python/generate_constraint_pl.py, prolog/data_validation.pl, prolog/testsets/thermal_dissipation_constraint.pl, ISSUES.md
**Tier:** landed

SuppBacked bucketed, keyed on the compiler-stamped suppression_profile(C, static) sanction marker
(never emptiness-inference): marker-sanctioned scalar backs / grid-misalignment substitution
excluded (→ OQ-105) / unmarked seriesless fails closed. Decision witness: bucketed = 59 flips / 20
fab_adjacent vs blanket 79/0 (laundering); scalar==series-endpoint 37/39 (0 violations); pipeline
A/B 30 diffs = 28 backed_times + 2 manifest. Also fixed the OQ-37 piton vacuous-green (unconditional
"No pitons detected" → VACUOUS notice + joined-table sizes). Commits
`00040bb9`/`b0a0e380`/`609dbb47`; evidence audits/2026-06-11_oq46_backed_reconciliation/.

## 2026-06-11 — OQ-46 RESOLVED: the classify_at_time scalar suppression fallback is SANCTIONED (operator ruling), not a retirable stopgap; OQ-46's premise contradicted the live generation prompt
**Files:** prolog/drl_composition.pl, docs/technical/classify_at_time_wiring.md, prompts/constraint_story_generation_prompt_json.md, ISSUES.md
**Tier:** landed

Operator ruled: accept the prompt's design — since `220739b8` the prompt
(constraint_story_generation_prompt_json.md:457) deliberately authors scalar-only suppression for
static-enforcement stories, so the retirement premise never terminates. Read ladder permanent:
temporal at T → scalar-as-constant Backed=false → fail-closed unknown; deletion counterfactual would
flip 16/46 timelines; snapshot_type/degradation_chain have zero consumers (positive-controlled).
Comment-only edits (STOPGAP → sanctioned); wiring doc §1 re-ruled; OQ-33/OQ-40/OQ-41 cross-refs
updated; the two *_contradictions files explain the 48/46 denominator gap. Evidence
audits/2026-06-11_oq46_close/ (branch oq46-ruling).

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

Headline = diagnostic_summary:verdict_join/3 (base verdict + severity-floored alerts via
signature_grade/severity + grid/measurement provenance), serialized as a SIBLING of
diagnostic_verdict; enhanced_report headlines the join, prints per-alert reconciliation, renders
[UNJOINED] on stale artifacts; schema_version 1→2. Corpus effect: 8/48 headlines changed (6
green→red, 2 yellow→red, all severe claim-mismatch), zero moderate caps; P1 probe ruled BRANCH A —
no diagnostic subsystem is grid-fed, so grid-diet lines carry [CONDITIONAL] tags. Tripwire promoted
to CLAUDE.md Architecture Invariants. Commits `e8ab707b` → `170db693` → `ce9a26ec`; witnesses W1–W4
+ 2 falsifiers audits/2026-06-11_oq98_verdict_join/.

## 2026-06-10 — OQ-95 resolved: constraint_neighbors/3 now fail-closed on phantom (zero-fact) constraints; giant_comp edges scoped to enumerated nodes; domain_registry throw hit independently (folded into OQ-96 at merge)
**Files:** prolog/drl_purity_network.pl, prolog/giant_component_analysis.pl, prolog/tests/test_phantom_neighbor_filter.pl, prolog/tests/test_forecloses_fpn_injection.pl, ISSUES.md, audits/2026-06-10_oq95_phantom_node_fix/writeup.md
**Tier:** landed

constraint_neighbors/3 made symmetric fail-closed on phantoms via phantom_subject/1 (neither
constraint_claim/2 nor constraint_metric/3); giant_comp edges scoped to the enumerated node set
(component > node-count impossible by construction). Witnesses: largest component 118.9%→56.8% live,
259.9%→89.2% on original_v6; gc edges 75→49 = exactly the 26 dangling affects_constraint/2 facts;
test_phantom_neighbor_filter.pl 4/4; fpn_injection 6/6; suite 39/39 (re-witnessed post-merge).
Contract change: a synthetic test constraint now needs a constraint_claim/2 to join the network.
domain_registry throw hit independently — folded into OQ-96 at merge (.gitignore:8 fossil,
run_pipeline.py:268 Pattern-1 producer, domain_priors.py --output absolute default). Evidence
audits/2026-06-10_oq95_phantom_node_fix/writeup.md.
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

Two batches triaged (external output = hypothesis, verified first): batch 1 (vote-market,
`2d54826c`) → OQ-98/99/100 + OQ-44/OQ-93 notes; batch 2 (XPrize, `96113b05`) → OQ-101/102/103 +
OQ-94 cross-ref. Load-bearing ruling: CUT orchestrator step 6 (Sonnet auto-essay) — the essay FORM
collapses plurality; deterministic tensions ledger replaces it (OQ-101); synthesis-fidelity
checklist lives in audits/2026-06-10_external_review_xprize/README.md. Runs committed under a live-
witnessed gate (run_dynamic_suite over 48, exit 0 —
audits/2026-06-10_external_review_vote_market/gate_witness.txt); essays/2026-06/who_owns_younger.md
left untracked; staged plan ~/.claude/plans/i-ran-an-article-merry-lagoon.md.

## 2026-06-10 — OQ-92 RESOLVED: gain_flow receipt surface live end-to-end (schema→compiler→prompt→batch→gates); GAP-10 closed; OQ-90 Steps 2–4 unblocked
**Files:** ISSUES.md, docs/design/design_gaps.md, prompts/constraint_story_generation_prompt_json.md, prolog/narrative_ontology.pl, prolog/drl_core.pl, prolog/maxent_classifier.pl, prolog/signature_detection.pl, prolog/data_repair.pl, prolog/testsets/gfbatch1/, audits/2026-06-10_oq92_step3_preregistration/
**Tier:** landed

gain_flow receipt surface live end-to-end: Stage C promoted stakeholders[] + six_questions + the
receipt surface into the LIVE generation prompt (additive; OQ-83 R4 control arm intact); first batch
gfbatch1 6/6 author gain_flow + fixing_cost, 0 diffuse, referential integrity clean; diffuse audit
0/0 observed — vacuous pass stated as vacuous. Stage D: constraint_captured/1 + OQ-94 benignity
gates rows 1–3 + maxent scaffold spec, two-sided controls all landed; fabrication-ban grep witnessed
in data_repair.pl; warning gate fired on a deliberate drift (allowlist 849→852). GAP-10 closed;
OQ-90 Steps 2–4 unblocked. Prereg audits/2026-06-10_oq92_step3_preregistration/.

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

Ruled SUPPRESS and wired: _flat_seeds_from_manifest drops reading-typed deps at both read sites (+
same predicate in the serial escape hatch, code-read sync only). Recon: ZERO historical exposure.
A/B (3 arms × 3 reps, injected verdict ≠ hypothesis): claimed_type held 9/9 snare, but the injected
verdict pulled authored theater_ratio 0.690→0.513 — verdict import in the GRADABLE channel, absorbed
before the categorical (categorical sticky, not safe; R-arm prose = positive control). Standing
cautions: injection-channel asymmetry is an instance (n=3), not an effect size; the CSR line poisons
vocabulary-based leakage probes in ALL arms. Full chain
audits/2026-06-10_oq81_reading_upstream_recon/ (RECON → AB_PLAN → AB_RESULTS → WIREUP).

## 2026-06-10 — OQ-77 closed: giant_comp SIGSEGV not serially reproducible (10/10 at exact crash size n=39; archives to n=3380) — concurrency artifact, operational rule promoted; OQ-95 filed (phantom network nodes)
**Files:** ISSUES.md, CLAUDE.md, prolog/giant_component_analysis.pl, prolog/drl_purity_network.pl, python/run_pipeline.py, audits/2026-06-10_oq77_serial_kill_condition/writeup.md
**Tier:** landed

Pre-registered kill condition executed: serial 10/10 rc=0 at the exact crash size n=39 (byte-
identical), 12/12 under 12-way co-residency, serial archive runs at kernel_v1 n=1106 + original_v6
n=3380 ×3 byte-identical → resolved as a concurrency artifact; one-pipeline-at-a-time rule promoted
to CLAUDE.md Running the System. Reopen path: any SERIAL segfault. Side-finding filed as OQ-95:
phantom network nodes (25 phantom atoms live, component 118.9%; ~2.6× on original_v6). Evidence
audits/2026-06-10_oq77_serial_kill_condition/writeup.md.

## 2026-06-10 — OQ-92 rulings recorded + step-2 gain-flow prototype PASSED 8/8: capture and fixing_cost separate on authored fields; step-3 surface build unblocked (OQ-92/OQ-90/GAP-10)
**Files:** ISSUES.md, docs/design/design_gaps.md, audits/2026-06-10_gain_flow_prototype/PREREGISTRATION.md, audits/2026-06-10_gain_flow_prototype/FINDINGS.md
**Tier:** landed

Rulings recorded (`4e04c2dc`): (a) build the authored gain-flow surface prototype-first; (b) ONE
authoring surface, TWO fields (gain_flow + fixing_cost; the binary-bit argument recorded as REJECTED
to prevent re-citation); tri-valued provenance authored-gain-to-NAMED-seat / explicit-diffuse /
absent-fails-closed; malformed-gain absorbs into fail-closed with a schema-rejection validation
item. Step-2 prototype (prereg `eb24a927`, eight-control battery): Outcome 1 PASS 8/8 — case 5 vs 4
witnessed fixing_cost load-bearing, OQ-90's decisive pre-wiring control discharged. Promotions:
scaffold-push collision homed as OQ-94; diffuse-gate tolerance/sample RESERVED for operator at
step-3 prereg. Evidence audits/2026-06-10_gain_flow_prototype/ (PREREGISTRATION.md, FINDINGS.md).

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

Shim diagnosed as an unmigrated v3.4 grid contract: empty intersection — 0/32 grid points
authorable, ever, corpus-wide; fires only via scenario_manager:load_and_run; fabrication-fed
products are [INTENT] / verification gate / κ (MaxEnt is authored-fed). Phase 2 visibility landed
(witnessed): three-bucket [PROVENANCE] line — authored / injected-0.5 / imputed (a binary split
would launder injection into authored, operator correction) — plus stray-anchor [WARN] and diet
flags; report regen diff = provenance-lines-only; run_dynamic_suite 0 errors / 0 warnings. Producer-
vs consumer-side migration left as the unruled OQ-93 fork. Census
audits/2026-06-09_imputation_shim_census/census.md.

---

## 2026-06-09 — OQ-80 + OQ-08 closed: generate-step token totals threaded (hard-0 retired); DR/CS Π-asymmetry annotated in both mismatch report layers
**Files:** agent/generate_kernel_corpus.py, agent/c-orchestrator.py, prolog/json_report.pl, python/enhanced_report.py, python/tests/test_token_acc_threading.py
**Tier:** landed

OQ-80: token totals threaded via an optional token_acc out-param (None = NOT measured, never 0;
summed at receipt incl. parse failures); the hard 0 + "unthreaded" note retired. Witness:
python/tests/test_token_acc_threading.py (all three cases pass). OQ-08: cs_drift_mismatch_note
emitted (json_report.pl) + rendered (enhanced_report.py) — Π-asymmetric by design (DR instance-blind
at the fixed analytical context, CS context-free authored facts); witnessed both directions on each
layer; eventual permanent home = the OQ-15 mediator.

---

## 2026-06-09 — Three doc-sync OQs closed with witnesses: OQ-07 (mismatch candidate runtime-probed SILENT, blocking conjunct named), OQ-28 (seat-theorem amendment provenance), OQ-14 (bridge unblessed; mediator is the decided join)
**Files:** ISSUES.md, docs/seat-theorem-v1.md, docs/design/two_axis_architecture_v7.md, prolog/cs_drift_mismatch.pl
**Tier:** landed

OQ-07: candidate UID runtime-probed SILENT on archives/datasets/kernel_test (positive control: 11
corpus-wide firings); cs_is_metric_stable FAILS — the blocking conjunct named; verdict
architecturally-possible-but-not-this-case (audits/2026-06-09_oq07_mismatch_runtime_probe/). OQ-28:
docs/seat-theorem-v1.md Amendment-provenance section (witness asymmetry: §3 run-grounded via
test_forecloses_fpn_injection.pl; §5/§8 scope-declarations). OQ-14:
docs/design/two_axis_architecture_v7.md amended — the influences bridge unblessed (16 cross-axis
surfaces in 7 modules); the OQ-15 mediator is the decided-but-unbuilt join; three grep-enforceable
invariants recorded.

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

Root cause (audits/2026-06-08_coordination_washing_clean_pass/): the metrics were never authored for
non-mountains and get_metric_average defaulted to 0.5 > snare_epsilon_floor 0.46 — fabricating
constructed_high_extraction from no data. Landed: schema + prompt REQUIRE both metrics for ALL types
(fallback validator made consistent); engine fail-closes — 0.5→unknown, abstain clause, number/1
guards (0 throws; anti-over-abstain control byte-identical); 3 articles regenerated
(world3/magnifica/china; V5 substitution B==C for all 16 — fix is structural, not verdict-changing).
Residuals (OQ-89): full re-runs RE-DECOMPOSE into different axes (orphans left in place per operator
ruling; 9 corpus members abstain to unknown); ~94/116 legacy json/ lack the metrics; class
generalization deferred (cross-ref OQ-43/44).

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

The four OQ-83 emissions — cs_reference_frame, cs_drift_moment, cs_drift_gap (commit ef5a9188) and
temporal_residual (de3736a6) — added to PIPELINE_FIELDS as nullable declarations, ending ~280 [WARN]
unexpected-field lines per run. Witness: validate_pipeline_output + validate_enriched_pipeline both
0 errors / 0 warnings; producer side (json_report.pl) unchanged — schema caught up to the emitter,
not the reverse.

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

Strict Tier-2 schema-deferred build: derive_directionality_at/4 + effective_time/3 (+ empty C1
hook), classify_at_time/5 surfacing snap(D, Backed, Eps, Supp, Theater), snapshot_type sync, and NEW
temporal_residual.pl (read-only category-B seam diagnostic, emitted per constraint by json_report).
Finding: the residual is NOT empty — 56/100 constraints, 155 counted flips, all observer-metric-
driven at frozen d (|Δε| median 0.07; bears on the D-fork: emptiness does not force branch b). V1–V9
verifications pass; V3 caveat: full classify_at_time ≡ snapshot_type is FALSE (3 mismatch points at
default context, sync OPEN; 2/52 flips flagged classifier-sensitive). v7 §4.5 corrected (one (A)
data bridge vs ≥3 (B) seam diagnostics); same-day committer stage-time enrichment landed
(cs_reference_frame/cs_drift_moment/cs_drift_gap beside cs_drift_terminal). Audit
audits/2026-06-08_typea_template_extensibility/.

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

A1 keystone (controlled null): the computed classification path ignores authored perspectives —
flipping an authored classification is byte-identical 162/162 while the ε-overlay control moves
every register → the stakeholder layer is an additive refactor. A7: mandatrophy surface is a
dangling wire (zero compiler emissions; abandonment git-witnessed at `6f997d71`/`3641ae71`); A6
guard asymmetry split out as OQ-84 (guard landed with step 3). Phase A steps 1–3 + 4b/4c all
landed/ran same day: schema stakeholders[] + six_questions, compiler constraint_stakeholder/7 +
role-derived beneficiary/victim, engine seat layer (extractiveness_for_agent_d/4 byte-identical); 4b
fired RENAMED-NOT-ESCAPED → OQ-85 filed then RESOLVED silence-is-correct (in_contention feeds no
classifier; residual → OQ-86); 4c pilot n=6 — both flips a victim-count × critical_mass_threshold
resolution artifact; claim-layer framing effect the robust separate signal. Committer thread
banked/parked → OQ-87 (COMMITTER_THREAD_HANDOFF.md). Full report + evidence
audits/2026-06-07_stakeholder_layer_migration/ (AUDIT.md, STEP4_4b_RENAMED_NOT_ESCAPED.md,
OQ85_DECOMPOSITION_AUDIT.md, STEP4C_PARTITION.md); tracker OQ-83 rulings R1–R5.

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

The kernel-dropping fork (OQ-79 mech-1) healed by DELETION:
generate_kernel_corpus.generate_from_manifests is the single manifest→corpus path (seed-type
dispatch; c-orch _step_generate calls it; forked _step_generate_batch + delegators + dead imports
removed, grep 0; serial escape hatch kept). Witness ladder P0/W1/W2/P3/P4 in commits `0f61517c`,
`099066c4`, `a7d56a14`, `ed2ec212`: flat path byte-identical across cold processes and after the
splice; Zionism readings land with cs_kernel_id; seed-dup bug caught pre-live. TRIPWIRE: gkc --scope
still runs its own kernel generation (OQ-82); new OQ-80 (token totals) + OQ-81 (reading-upstream
appropriateness); OQ-76 still uncovered.

## 2026-06-05 — Pre-build ruling session executed: OQ-70/64/63 ruled and landed, intent_* declared GAP-08, perturbation-principle §1.1 added
**Files:** prolog/signature_detection.pl, prolog/constraint_indexing.pl, prolog/narrative_ontology.pl, schemas/constraint_story_schema.json, python/generate_constraint_pl.py, prompts/constraint_story_generation_prompt_json.md, docs/design/design_gaps.md, docs/the_perturbation_principle.md
**Tier:** landed

One principle (the_perturbation_principle.md §1.1, operator-authored): the authored layer's
definition is authoritative — the computed layer must never consume what the author did not assert.
OQ-70-A ruled as the CLASS (`72ec2cdd`: no signature reads a single authored perspective as a story-
level claim; live-20 FCR 16→5, FNL 3→1, positive control manpower_exhaustion_trap still fires);
OQ-64-A (`e5fbc2e8`: vindicated_propositions → constraint_vindicates/2, feeds no metric/gate);
OQ-63-A (`28f2dfc8`: d consumes agent_beneficiary, zero-diff 80/80 cutover + guard positive
control); intent_* declared design_gaps GAP-08 (`f618c1f1`) — residual pass-open noted as the OQ-43
fifth instance, fail-close deliberately deferred to its own ruling.

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

_step_generate dispatches to a BATCH path by default (one Anthropic batch per §5.1 dependency wave;
--serial-generate / DR_SERIAL_GENERATE=1 keeps the legacy loop); build_prompt → build_prompt_parts
with a byte-parity witness; offline simulation witnessed wave partitioning, upstream context
injection, cache_control, 5/5 saved. Operator ruling folded in: stories are NOT linted at generation
and the authored side is never "fixed" — story_repair.py no longer fabricates mandatrophy_resolved
(witnessed) and enhanced_report.py renders the explicit Authored-vs-Computed divergence line
(witnessed both directions); batch path contains zero lint calls (grep = 0).

## 2026-06-05 — Generate-both landed: forced-flat control on every kernel, mechanical alignment key flat_control_of/2 (OQ-76 mitigated)
**Files:** agent/generate_kernel_corpus.py, python/generate_constraint_pl.py, prolog/testsets/flatctl_probe/, ISSUES.md
**Tier:** landed

Generate-both promoted to PRIMARY fix for the stochastic kernel/flat gate: flatten_manifests auto-
emits a <kernel_id>_flat_control seed per kernel (reading set never shown to the flat author);
compiler emits narrative_ontology:flat_control_of/2 outside the cs_structure gate (flat controls
carry no cs_kernel_id/cs_reading_relation); ASYMMETRIC by design (flat-on-every-kernel only).
Witnesses: compiler emission + negative control; seed/prompt independence on a real K1 manifest; E2E
run-tag flatctl_probe — first construction-pair diff: computed dr_type construction-ROBUST
(tangled_rope ×4 seats), authored layer divergent (snare ε=0.65 vs tangled_rope ε=0.48). Stage-2
residue: the readout stratum (OQ-76 Remaining). Writeup + probe + seed
audits/2026-06-05_flat_control_generate_both/.

## 2026-06-05 — K1 kernel-gate replication: real topic-classed boundary band; under-firing misses against explicit §1.3-K criteria (OQ-76 filed; Stage-2 condition)
**Files:** python/audits/kernel_gate_replication_probe.py, prompts/uke_scope_v2_json.md, ISSUES.md
**Tier:** landed

K3 hand-adjudication: gig classification and content moderation both pass all three §1.3-K criteria
→ flat takes are gate MISSES, not definitional ambiguity. K1 (k=8 × 5 topics, 40/40 calls, pre-
registered invalidation conditions): controls 0/8 and 8/8 (instrument valid); affirmative action
8/8, gig 5/8, content moderation 3/8 — the boundary band is real and topic-classed; noise localized
to the binary gate (conditional reading counts stable 4/3/3). Dispositions recorded in OQ-76
(interim kernel-bias hedge; generate-both candidate fix; K2 licensed); Stage-2 (OQ-75) carries the
routing condition. Writeup + 40 manifests + driver audits/2026-06-05_kernel_gate_replication/.

## 2026-06-05 — SCOPE count-distribution probe: 7-7-7 was coincidence + run noise, NOT an implicit target (OQ-75 watch resolved)
**Files:** python/audits/scope_count_distribution_probe.py, prompts/uke_scope_v2_json.md, agent/c-orchestrator.py, ISSUES.md
**Tier:** landed

Two-arm (current vs pre-`d179423d` SCOPE prompt) 8-topic battery, 16/16 calls, pre-registered
signatures incl. the masked-target sub-criterion: selected counts 3→11 track richness; upper tiers
spread among themselves (A: 5/6/6/11, B: 5/7/6/9); deferrals fire; replicate noise ±1; arms agree;
bridge replicate gig-economy 7→5 — the original 7-7-7 uniformity was mid-richness coincidence +
temp-0.2 run noise. Stage-2 (OQ-75) NOT gated on a SCOPE-framing fix; axis-count distribution at
scale is a readout, not a gate. Side observation: kernel-recognition itself noisy (T5 kernel in one
arm only). Writeup + 16 raw manifests + driver audits/2026-06-05_scope_count_distribution/.

## 2026-06-05 — Generation-pipeline de-leak: schema/prompt/feedback boundaries no longer hand the author the engine's bands (audit brief F1–F9)
**Files:** schemas/constraint_story_schema.json, prompts/constraint_story_generation_prompt_json.md, prompts/uke_scope_v2_json.md, python/linter.py, python/regenerate_stories.py, python/generate_constraint_pl.py, agent/c-orchestrator.py, agent/orchestrator.py, agent/uke_narrative_orchestrator.py, agent/story_generator_base.py, agent/generate_kernel_corpus.py, docs/logic_extensions.md, docs/technical/generation_path_resolution.md
**Tier:** landed

Binding leak was the SCHEMA, not the prompt: allOf conditionals tied claimed_type to numeric bands
and shipped verbatim in build_prompt — a claimed-mountain/high-ε story was literally unauthorable.
Landed with same-turn witnesses: `29cd45d4` (linter coordination_type 4→6, 286 false codes cleared;
canonical table → docs/logic_extensions.md), `9f2d050a` (schema de-leak; false summit authorable
after), `b6c4e113` (prompt maximal scrub; assembled-payload greps 19→0 and 28→0), `7ad86c5a` (axes
cap → optional ceiling; 7-7-7 uniformity → OQ-75 watch), `07f7b1c0` (regenerate_stories filters
THRESHOLD_COUPLED lint codes at the choke point), `d179423d` (lens-diversity SCOPE instruction —
SEPARATE change variable). Engine reads the claim only via diff detectors (drl_core.pl:566
dr_claim_mismatch/4; probe controls incl. synthetic false summit firing type_1_false_summit-severe);
new OQ-72/73/74 (55% = 158/286 re-witnessed). Canonical schema relocated same day to
schemas/constraint_story_schema.json (stale agent/data orphan deleted; all loaders witnessed;
docs/technical/generation_path_resolution.md + AGENTS.md updated; band grep re-run post-move: 0).

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
