# OQ-326 Phase 2 — the census: which overlay sites throw under the strict harness

**Executed:** 2026-08-21 · **OQ:** OQ-326 (parent OQ-302, resolved 2026-08-19)
**Code state:** HEAD `8efd8be7` · **Corpus:** live leg, loaded via `corpus_loader:load_all_testsets`
at evaluation time (see *Residues* — the corpus is a moving denominator for this question)
**Fired:** live

**Verdict (scoped):** the census is **complete for the mechanical axis and blocked on three
operator rulings before migration**. Of **56 real call sites over 28 files**, the strict harness
would throw at **20**. But two of the six checks do not survive contact with the substrate as
written — **clause 4 is unreachable by construction** once the binding leak it ships beside is
fixed, and **check 2 applied to the assert side would reject 7 legitimate sites**. And one site
does not merely *would*-throw: it throws **today**, and leaks state when it does.

---

## Evidence map

| artifact | what it is |
|---|---|
| `PROPOSAL.md` | what runs and what each verdict means, written before execution |
| `classify_sites.py` | static arm; reuses Phase 1's balanced-paren arg scan, adds a comment mask + shape tagging |
| `call_sites_raw.txt` | Phase-1 extractor output at HEAD, verbatim (byte-identical site set to 2026-08-19) |
| `sites_classified.tsv` | 57 matches × {file, line, kind, shape, retract side, assert side} |
| `template_eval.pl` / `template_eval_raw.txt` | mechanical arm: 23 predicates × 5 checks, + two-sided controls |
| `clause4_reachability.pl` / `evidence_clause4.txt` | clause-4 reachability across 4 configurations, + its two-sided control |
| `binding_leak_fixture.pl` / `evidence_binding_leak.txt` | S13, reproduced outside the repo |
| `oq109_permission_error_witness.pl` / `evidence_oq109_leak.txt` | the live throw-and-leak at a committed site |

---

## 1. The instrument, and a false-positive shape Phase 1 did not record

Phase 1's census §1 records one false-positive shape (functor-proximity grep catching
**goal-position** predicates). There is a second: **the extractor does not mask comments**. It
matched `probe_harness:with_retracted` inside the prose header of
`audits/2026-06-11_oq110_residual_join/backed_semantic_probe.pl:20`, where the text *describes*
Control C. That one phantom is the whole of the off-by-one between the published figure and its
own raw evidence.

**Reconciliation.** Phase 1 publishes *44 call sites, 27 files*. Its own
`overlay_template_census_raw.txt` holds **57** `kind=` matches over **28** files — as does my
re-take, which is **byte-identical** to it (`diff` clean; a reproduction, and a control on the
extractor). Correct figures: **57 matches, of which 1 is a comment phantom ⇒ 56 real sites over
28 files.** Neither 44 nor 27 reconciles with either reading; they are a transcription error, not
a different method. The load-bearing Phase-1 figures (13 templates, rule-bearing count) are
re-derived below rather than cited.

## 2. Shape census — 56 real sites

| shape | n | meaning |
|---|---|---|
| `STATIC_TEMPLATE` | 36 | retract side is a literal term list |
| `RUNTIME_VARIABLE` | 8 | template/fact list passed as a variable — not statically decidable; resolved by reading the binding |
| `BARE_ASSERT` | 7 | `with_asserted/2`, empty template list by construction |
| `DECLARED_ZERO` | 5 | retract side literally `[]` |
| `COMMENT_FALSE_POSITIVE` | 1 | excluded |

## 3. Mechanical arm — 23 predicates × 5 checks

Full table: `template_eval_raw.txt`. Two-sided controls fired on all five checks (dynamism, rule
detector, resolvability, empty snapshot) — see §7 for the one I had to repair.

**Rule-bearing retract templates at HEAD: 3, not 1.**

| template | rules | facts | dynamic |
|---|---|---|---|
| `constraint_indexing:constraint_classification/3` | 8 | **0** | yes |
| `drl_core:base_extractiveness/2` | 2 | 1 | no |
| `constraint_data:base_extractiveness/2` | 1 | 0 | no |

Phase 1 reported *1 of 13*. The two `base_extractiveness/2` arms are new to this census.

**My first-pass table over-reported throws, and the plan predicted exactly why.** Four verdicts
were artifacts of *my* evaluation chain, not properties of the sites:

- `narrative_ontology:cs_authority_grounding/2` reads STATIC under `[stack]`+corpus ⇒ I scored 3
  sites `throws_immutable`. Both consuming probes **declare it dynamic before corpus load**, and
  say why in-file: *"so the static-procedure retract refusal does not block the CS-axis arm"*
  (`probe_seat_test.pl:17-19`, `probe_r3_presentation_vs_structure.pl:20-22`). **Clean.**
- `narrative_ontology:cs_axiom/3` reads STATIC; `audits/2026-06-23_oq06_offcase_fixtures/probe.pl:47`
  calls `dynamic(narrative_ontology:cs_axiom/3)` at runtime. **Clean, conditional on execution order.**
- `maxent_classifier:maxent_dist/3` reads `facts=0` because **MaxEnt is unfitted under `[stack]`**
  (OQ-66) — a known soft-failure, not a site property.

This is the plan's *"run under the load chain the probe itself declares"* requirement earning its
place. It is recorded because the next re-runner will hit it first.

## 4. The throw set — 20 of 56 sites

| class | throw | n | files |
|---|---|---|---|
| 4′ `reach_undecidable` (**by construction**) | no template covers the fact | **15** | 8 |
| 3 `partial` (rule-bearing template) | `probe_overlay_partial` | 2 | 2 |
| 5 `immutable` (static assert target) | `probe_overlay_immutable` | 1 | 1 |
| 1 `empty` (zero-fact template today) | `probe_overlay_empty` | 2 | 1 |

The 4′ class breaks down as 7 bare `with_asserted/2` + 5 runtime-variable asserts with empty
template lists + 3 `with_overlay([], [...], G)`. **Every one throws unconditionally, before any
site-specific analysis** — they share one already-ruled migration (`reach_undeclared`). At 15 of
56 (27%) across 8 files this is **F11's report-before-wrappers threshold**: a mechanically
determined class of that size is a re-scoping question, not a migration.

## 5. Axis 1 — self-witnessed, read site by site

Recorded with the deciding line, per R4. A site with no line I can point at is `n`.

| site | self-witnessed | deciding line |
|---|---|---|
| `prolog/probe_oq190_edge_admission.pl:94,97` | **y** | `:101` — `PresN =:= 0 -> Adm='n/a', Why=source_absent_on_this_corpus`, plus a control arm at `:104` catching `probe_broken_control_also_zero`. This probe **already implements OQ-326's distinction by hand.** |
| `audits/2026-06-07_.../a1_probe.pl:77` | **y** | `:41-43` prints `AUTHORED_PERSP` per authored fact; `AUDIT.md` records the install as a `< snare` / `> mountain` diff |
| `audits/2026-06-23_oq06_offcase_fixtures/probe.pl:32,38` | **y** (via `expect_fired` only) | `:38-41` — no install ⇒ Goal fails ⇒ prints FAIL. Its sibling `expect_silent` (`:31-35`) is **not** self-witnessing: a no-op overlay makes `\+ Goal` succeed and prints PASS. The pair is two-sided only because `expect_fired` exists. |
| `audits/2026-06-23_oq06_offcase_fixtures/search.pl:115,117` | **y** | `:118` — `Sens =:= Base+1`; a no-op overlay gives `Base =:= Base+1`, which fails ⇒ FAIL |
| `audits/2026-06-11_oq110_residual_join/backed_semantic_probe.pl:106` | **y** | `:111-113` Control C fails loudly if the flip survives retraction |
| `audits/2026-06-11_oq109_phase_b/unanimity_adjudication_probe.pl:66` | **n** | — |

## 6. The one site that does not *would*-throw — it throws today, and leaks

`audits/2026-06-11_oq109_phase_b/unanimity_adjudication_probe.pl:66` asserts a 7-fact list whose
last element is `domain_priors:emerges_naturally(oq109_seam_nl)`. That predicate is **static**
(21 facts) and the probe declares nothing dynamic — it loads only `:- [stack]`. Run at HEAD:

```
THREW: error(permission_error(modify,static_procedure,domain_priors:emerges_naturally/1),
             context(system:assertz/1,_276))
--- post-state (cleanup should have removed ALL of these) ---
  *** LEAKED: constraint_metric(oq109_seam_nl,...) PERSISTS
  *** LEAKED: constraint_claim(oq109_seam_nl,...) PERSISTS
```

**This is R2's state-corruption hazard, reachable at a committed site, at HEAD.** The throw comes
from `apply_overlay/2`, which runs inside `setup_call_cleanup/3`'s **Setup**; Setup throwing means
Cleanup never registers, so the facts asserted before the failing one persist for the rest of the
session and every later goal runs against a mutated program with no indication.

Two consequences. **Check 5 is validated by a naturally-arising positive control** — a real defect
at a real site, nobody authored it to be found, and check 5 is exactly what converts it into a
pre-mutation refusal. And the probe's **published finding is in question**: this is OQ-109 Phase B
Test 2, the *seam positive control*. Its intent is **not derivable from the committed artifact** as
a declared zero, so per R5 it gets **no wrapper** — it needs an OQ against the published finding,
which is the operator's judgment, not mine.

## 7. My own instrument owed a repair, and a control

The first pass of the check-4 shadow control came back `CONTROL BROKEN` — I had written a test
whose expectation and body disagreed. Repairing it produced the finding in §8. Recorded because an
introduced control is itself a claim, and this one was wrong on first writing.

## 8. Clause 4 is unreachable — and that is a finding about the pre-registered criterion

Under the ruled precedence **2 → 3 → 1 → 4 → 5**, clause 4 (`probe_overlay_shadowed`) cannot fire:

- `snapshot/2` collects **every** fact unifying with the template (`findall` over
  `clause(M:T, true)`), and `apply_overlay/2` retracts all of them ⇒ **no fact clause matching the
  template survives**.
- The only surviving clauses that can match the template are **rule** clauses — and clause 3 fires
  first by ruling.
- Therefore clause 4's precondition is precisely the condition clause 3 has already rejected.

Tested across four configurations (`evidence_clause4.txt`): every fact is either uncovered (⇒ 4′)
or covered by a template whose facts were all retracted (⇒ clean). **Never clause 4.**

**Two-sided control, because an absence owes one.** The test *can* fire — under the binding leak:

```
POSITIVE (leak present):  snapshot narrowed to [p(a,1)]; survivors at template shape [p(a,9),p(a,2)]
                          *** clause4 FIRES: p(a,9) shadows p(a,2)
                          once(p(a,X)) selects X=9  -- replacement UNREACHABLE
NEGATIVE (leak fixed):    snapshot [p(a,1),p(a,9)]; survivors [] ; clause4 DECLINES
                          once(p(a,Y)) selects Y=2  -- replacement reachable
```

So **clause 4's only reachable path is the binding leak that Phase 3 fixes in the same change.**
Fixing the leak closes it. Implemented literally, clause 4 becomes a check that cannot fail —
which is the Pattern-5 shape (*a gate passes because its precondition is never met*) installed
inside the harness built to close Pattern 6. This is the plan's stopping condition #5 and only the
operator may amend the criterion.

## 9. Check 2 must not apply to the assert side

F7 extends checks 2 and 5 to `Facts`. **Check 5: correct** — verified, a static assert target
throws `permission_error(modify, static_procedure, …)`, which is §6's live defect. **Check 2:
wrong.** `assertz` into an *undefined* predicate is legal and creates it dynamic:

```
assertz into UNDEFINED predicate: OK, created
  and it is now DYNAMIC
```

That is the ordinary fixture-planting idiom, and 7 committed sites use it —
`drl_composition:constraint_data/2` (×5, the OQ-67 chi-retirement controls) and
`agent_index/2` (×2), both undefined until asserted. Executed as written, F7 would throw
`probe_overlay_unresolvable` — *"always a defect, no escape"* — on all 7. F7's stated rationale
justifies only its check-5 half; the check-2 half was carried along with it.

---

## Residues

- **Axis 2 answers "would throw if re-run today", which for historic sites conflates a probe defect
  with corpus drift.** `a1_probe.pl:77` is the witness: its overlay demonstrably **installed** in
  June (`AUDIT.md`'s `< snare` / `> mountain` diff), yet today
  `constraint_indexing:constraint_classification/3` has **0 fact clauses** corpus-wide — 258 live
  testsets declare it multifile and none author it — so a re-run would throw `probe_overlay_empty`.
  The site is clean; the corpus moved. **A retrofit wrapper written on today's reading would encode
  a corpus fact as a probe property.** Not resolvable by re-reading: the June corpus is gone.
- **Per-instance binding risk is not retroactively closable.** A generalized template matching >0
  facts does not prove each runtime instantiation matched. Phase 3 closes this forward only.
- **Clause 3's naturally-arising positive rests on one predicate.** `boltzmann_invariant_mountain/2`
  is still the only rule-bearing *and* static template; the census adds two rule-bearing templates
  (`base_extractiveness/2` ×2) but both are static too, so no **dynamic** rule-bearing predicate
  exists. If that predicate is retired, clause 3 falls back to a planted fixture, which licenses
  only *"authored drift gets rejected."*
- **The check-order freedom is flag-conditional.** Verified: `protect_static_code = false` (SWI
  default, unset in-repo). Were it true, clause 3's detector and `snapshot/2` would raise an
  unattributable `permission_error` on a static target and check 5 would become forced-early.
- **Structural install ≠ semantic effect.** The checks prove clauses moved and that the replacement
  is reachable at the declared query shape; they do not prove the observable changed.
- **A concurrent writer was live throughout.** The plan predicted the code-path collision
  (`scripts/gate.sh`, R6) and not the **ledger** one: run-id `2026-08-21-1` was already claimed in
  `.claude/skills/plan-review/RUNS.md` by the OQ-306 session, so this run is filed as
  `2026-08-21-2`. Two structural collisions from one day of concurrency, one of them unforeseen.
  `CLAUDE.md`'s pinned `with_overlay` bullet drifted 331→332 mid-verification.
