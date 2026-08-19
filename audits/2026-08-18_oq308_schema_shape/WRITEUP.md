# OQ-308 — the narrative_ontology namespace gets a shape register, anchored on the declaration set rather than on the corpus-schema rows

**Executed:** 2026-08-18
**OQ:** OQ-308 (resolved). Spawns OQ-321, OQ-322, OQ-323, OQ-324. Touches OQ-43, OQ-307,
OQ-66, OQ-68, OQ-306. Related: OQ-60, OQ-255.
**Verdict:** `prolog/schema_shape.txt` now carries one row per member of the **repo-wide
resolved declaration set (63)** — every `(name, arity)` declared `:- multifile` or `:- dynamic`
into `narrative_ontology` by any non-tests module — with `DECL=` as a list, a leg-total `LEGS=`,
an arity-total `ARG<i>=`, and a required `DISPOSITION=` for dead members. Four arms (E–H) ride
`--check`, which now scans all five legs; `--full` is retired. `multifile` still governs clause
accumulation and not shape, but a wrong-arity qualified call, a leg that quietly lost its
writers, an authored value outside a closed vocabulary, and a declaration nobody rowed are now
each a gate RED instead of a silent failure.
**Substrate:** no pipeline run, no engine `.pl` modified. Read-only over 180 engine files and
4,205 story files across five legs; the only writes are the checker, the allowlist, `gate.sh`
and the new register. Open `d0caef57` → close in `audit_log.md`.
**Manifest:** not cited — nothing here reads `pipeline_output.json`. Corpus size is stated as
file counts per leg (279/960/960/1005/1001), measured at open.
**Fired:** live — arm H flags a real in-tree site; two defects in this pass's own new code were
caught by controls rather than by reading; an existing control was found DISARMED and repaired;
and three of the plan's counts were corrected by measurement.

---

## Step 0 was the pass's only falsifier, and what "stop" would have meant

**Read this first if you are here because something disagrees.**

The whole register design rests on one rule, correction (b) in the plan:

> a `ROLE=corpus-schema` row exists **iff** the `(name, arity)` is written as a qualified
> clause head by some story file — keyed name/arity throughout.

That rule is a **replacement**. The first version of it was wrong: it predicted 18
disagreements of 63, 14 of which were correct existing rows, and writing the prediction down
before running is what caught it. Correction (b) is the second attempt.

So step 0 is not a smoke test and it is not one check among many. **It is the only thing in
this pass that could have falsified the design rather than a row.** A non-empty disagreement
set on the 40 would have meant the rule was wrong *for the second time* — and the register,
the derived-view framing, arm E's derivation half and the allowlist rewording all sit on top
of it. The instruction in the plan was to stop, and stopping would have meant: **the
foundation is broken, not this row.** The correct response then is to re-derive the rule from
the substrate and re-run step 0 — never to adjust a row until the disagreement set empties,
because every row is downstream of the rule and a row-level repair would make the rule
unfalsifiable through its own register.

It did not fire. Measured over all five legs, asserted on the **sets** and not the counts
(`evidence/step0_derivation.txt`, committed at `c2aa6a67` before any authoring):

```
rows  (ROLE=corpus-schema @ narrative_ontology) : 40
heads (qualified heads, all five legs)          : 40
DISAGREEMENT SET (row but no head): 0
DISAGREEMENT SET (head but no row): 0
=> TOTAL DISAGREEMENTS ON THE 40: 0

REGISTER (repo-wide resolved declaration set): 63
register members that are ROWLESS: 23
intent_fact/4 in register? True    intent_fact/4 has an allowlist row of ANY role? False
multi-module declared members: 15
```

Every clause of the prediction held, including the composition of the 15 multi-module members
(10 `scenario_manager`, 4 `cs_axiom_engine`, 1 `data_repair`).

**If you are reading this in six months because you found a disagreement:** you are not
looking at a bad row. You are looking at a broken foundation. Correction (b) has failed twice,
the register's anchor is the wrong anchor, and arm E's derivation half is enforcing a rule the
substrate does not obey. Go back to the rule.

## Control quality, stated at two altitudes rather than blurred into one

Arm E has two checks, and the strong one does **not** cover the weak one.

**Anchor resolution — naturally arising, two-sided.** The repo-wide anchor finds 63 members.
The anchor an author would plausibly *write* — `narrative_ontology.pl`, the namespace owner's
own file — finds 57 and misses six: `measurement/2` and `intent_fact/4`
(`scenario_manager.pl`); `cs_authority_grounding/2`, `cs_interpretation_layer_present/1`,
`cs_kernel_codification/2` (`cs_pattern_detection.pl`); `cs_kernel_id/2`
(`cs_kernel_registry.pl`). The repo-wide anchor **declines on nothing** — named-scan minus
repo-wide is empty at every widening. Neither side was authored to be found.
(`evidence/control_armE_anchor.txt`)

**Derivation — fixtures only, and it cannot be otherwise.** Step 0 predicted zero
disagreements and measured zero, so there is no naturally-arising positive to draw on. That is
not an effort gap somebody could close later: **the rule was derived from the substrate it now
checks, so an in-tree positive is unavailable by construction, not by accident.** Any case
that would serve as a natural positive is, by the rule's own construction, a case the rule was
built to exclude. It is the same circularity as arm F's declared sets, which were transcribed
from the corpus they now police.

This is stated rather than left implicit because **arm E's other half has a genuinely natural
two-sided control**, and the two sit side by side. Unstated, the contrast reads as an oversight
in the derivation half — as though somebody found a natural control for the anchor and simply
did not bother for the derivation. It is not an oversight; it is a structural limit of a check
whose rule and whose substrate are the same object. What the derivation half ships on is
both-directions fixture rejection — a row whose writers are gone, and a written head with no
row (`evidence/control_derivation.txt`) — which is arm F's altitude, and is labelled that way
in the code, in the commit, and here.

## What landed

| | |
|---|---|
| `prolog/schema_shape.txt` | 63 rows, 162 declared argument positions, 315 leg assignments |
| arms E–H | closure + derivation + disposition; value conformance; leg census; arity resolution |
| `--check` | now all five legs, 16.7s; `--full` retired with a note |
| selftest | 23 → 50 |

**54 of 162 argument positions are ENFORCED** (22 closed sets, 24 `text`, 8 `number`); the
other 108 are `open`/`cid`/`atom`/`compound` and are **documentation only**. The file and the
arm docstrings both say which is which, because an unenforced token in a gated file reads as a
checked one. Arm F is non-vacuous: all 54 enforced positions harvested at least one value, and
60,642 distinct authored values were compared (`evidence/armF_coverage.txt`).

**Arms F and G are drift ratchets, not specifications.** Both were transcribed from the corpus
as it stands. A green F or G means the schema has not changed unnoticed — not that it is right.
Arm G has a discrimination record, which proves the arm *fires*, not that the declarations are
correct. Said in both docstrings.

## Controls (all pasted in `evidence/`)

| Arm | Grade | Fires on | Declines on |
|---|---|---|---|
| E anchor | naturally arising, two-sided | 57-member named scan misses 6 | repo-wide misses nothing |
| E closure | fixture, two-sided | member without a row; row without a member | the true register |
| E derivation | **fixture only, by construction** | row whose writers are gone; head with no row | the true 40 |
| E disposition | fixture + live | zero/zero without the marking; marked-but-live | the 5 true dead members |
| F | **authored decoy only** | planted `bogus_decoy`, naming value *and* file | removal; leg md5 identical |
| G | naturally arising, two-sided | `all:nonempty` names exactly the 4 twin legs; `testsets:empty` names 28 files | the true 28/0/0/0/0 |
| H | naturally arising **both sides** | 1 of 1,080 references (a 2026-06-23 fixture) | the other 1,079 |

Arm H's natural case is a *no-arity* case, so an authored decoy adds the arity sensitivity it
does not exercise: a planted `constraint_victim/3` is flagged against the resolved `[2]`.

## Findings

**1. `intent_fact/4` — the member nothing watched.** Declared `:- dynamic
narrative_ontology:intent_fact/4` in `scenario_manager.pl` alone: in no `narrative_ontology`
block, in no allowlist row, zero writers, zero readers. This is the concrete justification for
scanning repo-wide rather than over named modules, and it is why the totality claim is the
escape-removal guarantee rather than a nicety.

**2. A goal in a clause body was being counted as a clause head** — 810 occurrences across 270
files, all `constraint_metric/3`, all inside plunit test bodies. Arm C never noticed because it
asks only whether the *pair* appears and that predicate has 20,895 genuine heads; the key set
and every per-file count are unchanged. **Arm F would have noticed**: it harvested the Prolog
variable `ExtMetricName` as an authored value of argument 2. A parse reused by a second
consumer has to mean what its name says, and this one did not — the miscount was invisible at
the altitude of its only consumer. Fixed before arm F was built on it.

**3. The arity scanner was blind to brackets.** `adjacent_pairs([], [])` scanned as arity 0 —
a predicate recorded at an arity it does not have, which is the fabricated-census-row shape
this checker's own docstring warns about. 32 of 29,447 engine open-parens; 17 phantom `/0`
entries in `defined_preds`. **Latent, not live**: no reported set moves, but those phantoms are
load-bearing for `closure_arity`, whose first branch is `if (pred, 0) in pool: return 0`.

**4. A control was found DISARMED, not failing.** The clause-start guard in finding 2 removed
the only discriminating element from the existing naive-parser fixture, which then compared two
parsers that agreed. It would have stayed green while testing nothing — *a control that stops
discriminating looks exactly like a control that passes*. Repaired, re-checked to actually
differ, and its label corrected: it read "mis-parses the multi-line fact", but the multi-line
fact parses identically under both strippers; the block comment was always its whole content.

**5. Two defects in this pass's own new code, both caught by controls.** Arm F named the
predicate's *first head* rather than the file holding the offending value — sending the reader
to innocent code, the same defect `strip_comments()` preserves line numbers to avoid. And the
`DISPOSITION` reader probe counted a `:- dynamic` **declaration** as a reader, reporting
`intent_fact/4` as live — which would have retired the requirement on precisely the member the
register exists to surface. Neither was visible by reading the code.

**6. A written-but-unread stratum of 6.** `constraint_vindicates/2`, `cs_axiom_status/2`,
`cs_created_at/2`, `fixing_cost_class/2`, `story_seed/3` and `flat_control_of/2` are authored by
stories and read by no Prolog engine file; their only Python mentions are the emitter
(`generate_constraint_pl.py`) and this checker. **Probe scope limit stated:** the reader probe
scans Prolog engine files, so a Python consumer would not count — the Python side was checked by
hand, not by the arm. Reported, not acted on: *unwired ≠ worthless* governs removal.

## Three plan claims corrected by measurement

- **The readerless stratum is five, not three.** The plan named `coupling_profile/2`,
  `input_vector/2`, `theater_ratio/2`; measured, `attribute/3` and `intent_fact/4` join them.
  Verified with a positive control — the same probe fires on `update_authority` and declines on
  all five (`evidence/zero_written_readers.txt`).
- **The anchor delta is six members from three modules**, not five including
  `cs_axiom_engine.pl`. Those four CS axiom predicates are *also* declared in
  `narrative_ontology.pl`, so excluding that module loses DECL sites and **zero members** — it
  cannot be part of a member delta. The escape is wider than the plan estimated.
- **Arm H's two predicted arity skews do not exist as skews.** `constraint_claim/3` and
  `measurement/2` are both genuinely declared, so an arity check never sees them. Arm H's single
  live finding is a different fact — a reference to a predicate that resolves at *no* arity — so
  the exemption registry is named `SCHEMA_ARITY_EXEMPT`, not the plan's `SCHEMA_ARITY_ALIASES`.
  Using the planned name for a different thing would have been a fork.

Two further plan items did not reproduce: the `--full` timing ("4.3s / 36s, the 1.4s/13s comment
is stale") measured 1.35s/14.4s at open, so the existing comment was nearly right; and the
`PATH=` prefix said to be moot was already absent.

## Residues → OQs

1. **OQ-321** — per-consumer absence-safety across the zero-written stratum. 18 declared
   predicates are live-read and zero-written. Arm G asserts the emptiness is *expected* and
   nothing about whether each reader distinguishes absent from zero.
   `founding_problem_corroboration_class/2`'s consumer handles it explicitly
   (`stakeholder_seats.pl:605`, checked); **the other 17 were not audited** and each now carries
   an `all:empty` row that reads as inspected-and-fine. This is the largest finding here.
2. **OQ-322** — the `theater_ratio/2` fork. Zero/zero, while the quantity lives under three other
   spellings. Deleting the declaration leaves three.
3. **OQ-323** — `scenario_manager`'s shape-inventing declarations (`measurement/2`,
   `intent_fact/4`) plus the 10 redundant redeclarations and the four other dead members, as one
   adjudication.
4. **OQ-324** — static ≠ loaded. `load_testset_list/3` skips-and-continues and the default
   `run_pipeline` path has no `Loaded == glob_count` assertion. Above the fix-on-sight threshold.
5. **Not an OQ** — arms F and G are drift ratchets. Recorded in both docstrings, where they
   execute, rather than in a ledger.

## Evidence map

| File | What it holds |
|---|---|
| `step0_derivation.txt` | the falsifier: 0 disagreements, register 63, 23 rowless, 15 multi-module |
| `baseline_check.txt`, `baseline_full.txt` | pre-change output, for commit 1's byte-identical acceptance |
| `commit1_setdiff.txt` | set-level identity: heads map, 737 bypass sites, module tables, 29,447 open-parens |
| `bracket_fix_witness.txt`, `bracket_fix_sets.txt` | the 32 corrected parses; 17 phantom `/0` removed |
| `head_vs_goal.txt` | 810 body goals miscounted as heads; 0 arm-C counts moved |
| `armF_coverage.txt` | 54/54 enforced positions harvested; 60,642 values compared |
| `armH_preview.txt` | arm H's resolution set and its single finding |
| `control_armE.txt`, `control_armE_anchor.txt`, `control_armF.txt`, `control_armG.txt`, `control_armH.txt`, `control_derivation.txt` | the per-arm two-sided controls |
| `zero_written_readers.txt` | the zero/zero set, with the plan reconciliation |
| `audit_log.md` | open/close HEAD stamps and the concurrent-writer comparison |
