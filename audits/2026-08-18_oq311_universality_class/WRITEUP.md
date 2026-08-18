# OQ-311 Item 1 — §2.3's type-concentration claim is WITHDRAWN as unwitnessed; range-robustness survives with a re-pointed tracked witness

**Executed:** 2026-08-18
**OQ:** OQ-311 (Item 1 landed; Item 2 pre-registered and UNFUNDED). Related: OQ-01, OQ-05, OQ-22.
**Verdict:** `docs/observers_not_humans_v6.md` §2.3 fused two claims. **Robustness to functional
form survives** — Jaccard 0.697–0.833 over six forms, witness re-pointed to the tracked
`python/alt_power_transform_results_3k.json`. **The type-concentration claim is withdrawn** —
"+0.21 in tangled_rope (N=2,245) vs +0.014 in snare+rope (N=1,169)", the 14.6× figure, and
"concentrates in a single constraint family" — because its named witness could never have
contained numbers of that shape, and the two subset sizes are arithmetically impossible.
**Withdrawn as unwitnessed, not refuted:** the concentration may be true, and whether it does is
OQ-311 Item 2, pre-registered here and unfunded. H0's scope is OPEN.
**Substrate:** **no pipeline run.** One read-only corpus **load check** against
`prolog/archives/datasets/original_v6` (measured `corpus_constraint/1` = **3,380**), plus
git-history and tracked-artifact reads at HEAD `d9687381` → close `ec860a2e`. No engine behavior
changed; no Jaccard sweep was run — that is Item 2 spend.
**Fired:** live — a published headline is withdrawn, its two propagation sites amended, and a
dead corpus path that made `range_sweep.py` throw `corpus_empty` at HEAD is repaired.
**Evidence map:**
- `audit_log.md` — HEAD stamp pair, the declared commit-ordering deviation, prereg md5.
- `PREREGISTRATION.md` — Item 2, authored in full, **frozen and unfunded** (md5 `3f53bb8e…`).
- `evidence/rename_r100.txt` — 3,380/3,380 `prolog_v5` → `original_v6` renames are R100
  (byte-identical); witnesses that `original_v6` **is** the §2.3 corpus, and that the old path is gone.
- `evidence/range_sweep_output_keys.txt` — **ground (i)**: the script's whole output surface, plus
  the finding that *no version ever* stratified; carries its own positive control and a correction
  to the plan's recon.
- `evidence/arithmetic.txt` — **ground (ii)**: the three corpus counts, the 3,414 contradiction, and
  the resolved V5 branch.
- `evidence/tracked_witness.txt` — **what survives**: `git ls-files` hit, `"total": 3380`, and the
  six variant Jaccards reproducing 0.697–0.833.
- `evidence/load_check.txt` — the V5 measurement **with its decline control** (repaired path exit 0
  / pre-repair path exit 2, `corpus_empty`).
- `evidence/propagation_sweep.txt` — pre-edit baseline, the BRE-vs-ERE control, the coverage relation.
- `evidence/feasibility_crosstab.py` / `.tsv` / `.txt` — **DESIGN-FEASIBILITY ONLY, NOT A RESULT**:
  proxy corpus, proxy code state, no Jaccard measured. Includes the tie-rule sensitivity sweep.

**Prior art (grepped `build_discipline.md`, same pass as the finding):** **no hit** for this
mechanism — `never satisfiable` 0, `range_sweep` 0, `unwitnessed` 0, `arithmetically impossible` 0;
the single `unstratified` hit (:2365) is about stratifying a *draw*, unrelated. The grep's own
control fires (`Pattern 4` → 8). Nearest kin is **Pattern 8 (recap-as-witness substitution)**, of
which this is the **published** analogue: there, a prose "done" stands in for a pasted run; here, a
*citation to a file* stands in for evidence the named producer could not emit. Both are the spine —
an absence wearing a success-shaped token. Not minted as a new pattern; recorded as an instance.

---

## 1. What was found

OQ-311 filed §2.3 as a doc-currency chore plus a live stratification question. Recon changed the
framing, and execution changed it again in three places.

### 1.1 The range-robustness half survives, and OQ-311's residue bullet was half wrong

OQ-311 records "the witnesses are gone" for both files §2.3 names. One of them is **tracked**:

    $ git ls-files python/alt_power_transform_results_3k.json
    python/alt_power_transform_results_3k.json

Its seven entries (the sigmoid baseline at Jaccard 1.0 by definition, plus six variants) give
**min 0.6966, max 0.8327** — the published "0.697–0.833" — with `"total": 3380` uniform across all
seven. The `outputs/` path §2.3 cites is gitignored and gone; the fix is a re-point, not a
withdrawal.

### 1.2 GROUND (i) — the type-concentration citation was never satisfiable

§2.3 cites `outputs/range_sweep_results.json` for the per-type numbers. That file is produced by
`python/sweeps/range_sweep.py`, whose entire persisted output is:

    results_out = {
        'arm_a_jacs': ..., 'arm_b_jacs': ...,
        'mean_ab_gap': ..., 'max_span_drop_a': ..., 'max_span_drop_b': ...,
    }

`jaccard_stats(base_set, var_set)` takes two **whole** presheaf id sets from `load_presheaf_set`
and returns one scalar. **No code path subsets either argument by type or by any geometric
condition** — and this is not a fact about HEAD only: the dict is byte-identical across **all four
commits** in the file's history. The citation could not have been satisfied at any point in time.

**A caveat that had to be recorded rather than smoothed over.** A naive grep for `rope|snare` in
that file **hits** (:96, :118–135), so "the script never mentions the types" would have been false.
Those hits are a per-**variant** f(d) profile table: for each transformation variant it computes
χ_min/χ_max from that *variant's* L/U at a fixed ε=0.70 and labels which gates the variant's own χ
range spans (`rope->TR->snare`, `*** STARVED ***`). That is a property of the function, printed to
stdout, computed from **no corpus data at all**. The precise claim — no partition of a
per-constraint id set — is the one that survives contact with the file.

**Ground (i) is sufficient on its own** and stands regardless of every count in §1.3.

### 1.3 GROUND (ii) — the subset sizes are arithmetically impossible (corroborating)

2,245 + 1,169 = **3,414**, exceeding every corpus in play:

| accounting | value |
|---|---|
| `original_v6` `.pl` files on disk | 3,380 |
| results JSON's own per-variant `"total"` | 3,380 |
| **measured** `corpus_constraint/1` | **3,380** |
| corpus of the tracked `tangled_rope_sign_flip.md` | 3,314 |

Independently of any total, that same tracked census gives **rope 55 + snare 571 = 626**, against a
published 1,169 — a factor of 1.87. That corroborator appeals to no corpus size at all.

**The V5 pre-declared branch resolved.** The block asserts 3,380 as a *constraint* count while the
disk figure is a *file* count, so the plan required this be **measured, not asserted**, with the
branch declared in advance (`N < 3,414` → ground (ii) survives; `N ≥ 3,414` → drop it, verdict
unchanged). Measured **N = 3,380** → branch 1. **No divergence to record**: the measured constraint
count equals both the file count and the JSON's own total, so the identity the arithmetic rides on
is now witnessed rather than assumed. That is the informative outcome the "do not assert a value"
instruction existed to preserve.

### 1.4 The corpus is fully recoverable, and the sweep could not run at HEAD

`prolog/archives/prolog_v5` → `prolog/archives/datasets/original_v6` is a **byte-identical rename in
all 3,380 cases** (3,380 R100 lines, 3,380 of 3,380). `range_sweep.py` still named the dead path, so
it threw `corpus_empty`. Repaired (commit `5d548413`), with the discrimination shown both ways:
repaired path → exit 0, "Loaded 3380 testsets successfully"; pre-repair path → exit 2,
`corpus_empty(.../archives/prolog_v5/*.pl)`.

---

## 2. Three corrections to the plan's own recon

Recorded because a plan is not a witness, and each of these would otherwise have been published as
verified.

1. **`range_sweep.py` first appears at `cdfbe999` (2026-05-29), not `ae10e7ea`.** `ae10e7ea`
   (2026-06-02) is where the now-dead `archives/prolog_v5` path was *introduced*, during the corpus
   rebuild. The postdating claim **survives** — v6's §2.3 commit is `e5f805ab` (2026-05-28), one day
   earlier — and was additionally guarded against a false read: `cdfbe999` is titled "Repo reorg,
   initial" but is **not** the repo root (218 commits precede it; root is `41db1d0b`, 2026-01-12), so
   "first appears there" is a real authoring date, not an artifact.
2. **The "no stratification of any kind" phrasing was too strong** (§1.2). The corrected claim is
   narrower and fully witnessed.
3. **The feasibility cell counts depend on a free choice the plan did not declare** — the modal-type
   tie rule. See §3.

---

## 3. Item 2 feasibility — a PROXY, and what it does and does not establish

`evidence/feasibility_crosstab.py` cross-tabs χ-span-crosses-zero × modal-type-is-`tangled_rope` over
the committed OQ-22 census TSVs. **It is not a result.** Wrong corpus (never touches `original_v6`),
wrong code state (TSVs from `bbbf2c6`, 2026-06-28, before OQ-67 drained the legacy χ path), possibly
wrong condition (uses zero-crossing, which is one of the two readings the prereg must first
disambiguate), and **no Jaccard is computed at all**. Cell occupancy is the entire output.

| leg | n_scored | cond∧TR | **cond∧¬TR** | **¬cond∧TR** | ¬cond∧¬TR | both off-diagonals |
|---|---|---|---|---|---|---|
| testsets | 94 | 7 | 77 | 0 | 10 | no |
| testsets_haiku | 885 | 168 | 571 | 40 | 106 | **yes** |
| testsets_flash | 890 | 115 | 773 | 0 | 2 | no (degenerate) |
| kernel_v1 | 871 | 398 | 228 | 150 | 95 | **yes** |

**The tie-rule finding.** "Modal type" needs a rule for ties, and a tie rule is a free choice. If the
verdict moved with it, the verdict would be an artifact of the choice — so it was swept rather than
assumed: 3 tie rules × 2 type columns = 6 settings per leg. **The counts move; the occupancy verdict
does not.** `both_offdiag_populated` is STABLE across all six settings on every one of the four legs.
The `first`-occurrence rule reproduces the plan's recon figures exactly (kernel_v1 433/155, haiku
616/48, flash 0), which identifies which rule that recon used and explains the divergence from the
counts above (which exclude ties).

**What this licenses:** the Item 2 design is *feasible in principle* — the discriminating cells are
populatable on at least some legs. **What it does not license:** any claim about `original_v6`
occupancy, which is unmeasured. If `original_v6` is flash-like (TF empty), Item 2 is UNANSWERABLE on
that corpus — a legitimate terminal result the prereg pre-commits to (§3, §4 there).

---

## 4. Verification

| step | result |
|---|---|
| **V1** `issues_status.py --check` | see §5 residue — run with the ISSUES.md commit |
| **V2** `omega_resolver.py check` + `index` | see §5 residue |
| **V3** `audit_writeup_gate.py --check` | this dir's header + `Fired:` + reserved `PREREGISTRATION.md` |
| **V4** `./scripts/gate.sh` | full gate, `python env` row read first |
| **V5** load check, **measured** | `N = 3380` → branch `N < 3,414` → ground (ii) survives; **discriminates** (exit 0 vs exit 2 + `corpus_empty`) |
| **V6** propagation sweep | below |
| **V7** readback | withdrawn numbers **present and marked**, not deleted: `14.6`×7, `1,169`×8, `+0.21`×6, `+0.014`×6 occurrences; 19 struck spans; 21 dated markers |

### V6 — the propagation sweep and its own control

**V6a. `-E` is mandatory, and the sweep witnesses its own discrimination.** The pattern uses
alternation; under BRE (grep's default) `|` is a **literal**, so the whole pattern matches nothing
and the sweep returns clean — banking a false negative across every file. Same pattern, same file:

    BRE: 0     ERE: 7

The plan predicted this demo would read 0 vs 4; **measured 0 vs 7**, and the measured value is what
is recorded. The demonstration's point is unchanged. `/usr/bin/grep` is pinned throughout, not bare
`grep`.

**V6b. Gates, against the pre-declared table.**

| file | baseline | gate | result |
|---|---|---|---|
| `docs/observers_not_humans_v6.md` | 7 | **property gate** + direction "> 7" | **15** — pass |
| `docs/lawvere_glossary.md` | 1 | **count gate 0** | **0** — pass |
| `v2`–`v5` | — | excluded (point-in-time, not corrected) | — |
| all other `docs/`, `essays/` | 0 | **count gate 0** | **0** — pass |

**The property gate, checked per line rather than in aggregate.** All 15 hit lines: 9 carry a dated
`2026-08-18` marker on the line itself; the other 6 (`:104,110,121,124,147,149`) are **inside** the
correction block (`:89–153`). No hit falls outside both.

**This gate caught real unfinished work.** After the first edit pass, six stale sites still stood
unmarked — the concentration sentence and bypass pin in "What Changed in v6" (`:15`, `:19`), the
trailing `Witness:` sentence still naming the never-satisfiable file (`:159`), the bypass pin in the
conditional-assumption subsection (`:193`), the dead `open_questions.md` pointer (`:199`), and the
bypass pin in the front-matter note (`:5`). A count gate would have read "15 > 7, pass" and shipped
all six. The property gate is why they were found.

**Why v6 gets a property and not a count gate:** the marking principle *requires* the block to quote
the withdrawn sentences, restate +0.21/+0.014/14.6×, name the never-satisfiable witness, and state
both pointer repairs. Every one of those is a new pattern match, so the count necessarily rises. A
count gate would fail against its own baseline, and the obvious way to make it pass is to delete the
quotes — exactly what the correction forbids. Count gates are meaningful only where the expected
value is zero.

**V6c. Scope and exclusions.** **`2,245` is deliberately NOT in the pattern.**
`when_consensus_isnt_coherence.md:29` and `docs/results/tangled_rope_sign_flip.md` cite it
legitimately from the tracked census (`tangled_rope` = 2245, 67.7% of 3,314); it is a witnessed
number, not part of the withdrawn claim. Sweeping on it would emit noise a future reader would
re-chase. The discriminating token is `1,169`, which is unique to the withdrawn claim.
`when_consensus_isnt_coherence.md:29` was verified to need **no change**.

**Coverage relation** (recorded because a negative whose verifying command has *narrower* scope than
the claim is not reproducible): the plan records a named negative — "`deferential_realism_paper`
v7/v8/v6.13.1 return 0 hits". The sweep above does not name those files; it sweeps `docs/` and
`essays/` whole. That is a **superset** of the named negative iff every such file is under `docs/`.
Verified: **24** tracked `deferential_realism_paper*` files, **0** outside `docs/` (one nested at
`docs/v8/foundations/`). The repo-wide sweep therefore reproduces and subsumes the named negative.

---

## 5. Residue — what changed in substrate, and the next forward move

**Landed.**
- `python/sweeps/range_sweep.py` — dead corpus path repaired; two site markers (the path move, and
  that this script is not the source of any per-type number). Commit `5d548413`.
- `docs/observers_not_humans_v6.md` — §2.3 correction block (`:89–153`); **both** revision notes
  (`:5`, `:430`) marked inline *and* carrying an appended dated withdrawal; six further stale sites
  marked. Withdrawn text left standing throughout.
- `docs/project_orientation.md` — the universality-class summary line now names it as a *proposed*
  framing with its concentration evidence withdrawn and open at OQ-311.
- `docs/lawvere_glossary.md` — independent stale pin (`metric_based_type_indexed/3` at `:356`, now
  `:532`) repaired and re-cited **by predicate**. Kept deliberately separate from the withdrawal:
  different predicate, different claim, no conflation. It independently motivates the
  cite-by-predicate rule — OQ-22 pinned this same predicate at `:479–483` on 2026-06-28.

**Not done, deliberately.** No corpus overlay run, no Jaccard sweep, no stratified-sweep tool, no
§5.5 cross-class protocol rewrite. **Item 2 stays OPEN with its prereg authored and its spend-go
unclaimed.**

**Next forward move (Item 2), and what it is gated behind.** Gated on an **operator spend-go**.
The first task is not the run: it is resolving the §1 conflation in `PREREGISTRATION.md` — "Hub 1
spans the snare gate" (χ span crosses `snare_chi_floor`) and "institutional below `d_zero`,
powerless above" (χ span crosses **zero**) are **not the same set**, and OQ-311 glosses both as "the
gate-spanning condition". Then the tool from §5 must be **built** — no script in the repo computes a
stratified Jaccard, and none ever did, so this is construction, not a re-run. Item 1 was executed
prereg-first precisely so the rebuild is not silently judged by whether it lands near the withdrawn
+0.21/+0.014.
