# OQ-120 Phase 0 — the rope/snare ε boundary is not a free decision boundary, and the gate still did not close it

**Executed:** 2026-08-23
**OQ:** OQ-120 (re-specified, stays **open**); mints OQ-350
**Verdict:** Over 18 live legs (17,104 stories) and `kernel_v1`, at 0.01-rail + 1e-4 resolution:
`rope_epsilon_ceiling` is the decisive ε gate at scale (**1936** live transitions) while
`snare_epsilon_floor` is decisive **once in 122,031**, non-replicating across its own model's
redraw triple, and the FT pair `{rope, snare}` the boundary is named for is observed **0 times**.
The named boundary does not exist as filed. **The frozen gate scored G1b, not G0** — 1 ≠ 0 — so
OR-1's closing trigger never reached and the entry is re-specified rather than resolved.
**Substrate:** `outputs/pipeline_output.json` manifest `pipeline_run_at 2026-08-21T16:36:35Z`,
`n_constraints 285`, `n_stories 258`, `code_commit_short 885151b`, `code_dirty true`,
`schema_version 3`. Sweep at OPEN HEAD `f88c8c3c`.
**Fired:** live
**Evidence map:**

| artifact | what it holds |
|---|---|
| `PREREGISTRATION.md` | the three definitions and three branches, frozen before any sweep code existed; md5 `b181e1a2a9cd42b86d190be09f61d400` |
| `audit_log.md` | OPEN/CLOSE HEAD stamps, prereg md5 above the first result line, S8 baseline at both ends |
| `substrate_check.md` | S1–S13 verified first-hand, with the five findings about the plan |
| `eps_transition_map.py` | the sweep fork (not an edit of the wired `epsilon_stability.py`) |
| `build_strata.py` | per-**story** model stratum from each story's own `story_provenance/8` |
| `run_all_legs.sh` | the driver, with the before/after file-count pair per leg |
| `analyze.py` | Step C: N_eps/N_reach/N_rail + the MOVED/DECISIVE scoring |
| `gate_readout.md` | the branch determination, both scorings, controls, the operator ruling |
| `gate_numbers.txt`, `gate_readout.json` | the numbers and every classified transition row |
| `raw/` | 19 per-corpus sweep JSONs, per-leg stdout/stderr, `sweep_log.txt`, `strata.json` |
| `raw_PREFIX_double_emission/` | the superseded first sweep, kept as a free negative control |
| `threshold_boundary_relabel.md` | `:591`/`:593` label evidence — **no commit**, question left open |
| `gate_open_baseline.txt`, `gate_close.txt` | observed gate at both stamps |

---

## What was measured

ε perturbed in-memory per story on the 0.01 rail (101 points) ∪ bracket triples at δ=1e-4 around
{0.10, 0.25, 0.30, 0.35, 0.45, 0.46, 0.60, 0.66} ∪ adaptive bisection to 1e-4 — 117 grid points ×
4 canonical seats, restored after every probe with a verified restore and a `once/1`-pinned
took-effect guard. Per probe point per seat: ε, d, f(d), σ, χ, suppression, `coalition_fired`, and
**16 independently-evaluated gate bits** derived by reading `classify_from_metrics/6` clause by
clause (the plan estimated 13; the derived set is the witness). Per located transition: the SET of
bits that changed.

**Integrity.** No leg moved during its sweep (before/after `.pl` counts identical on all 19; a
concurrent instance was generating `testsets_nemotron` throughout and the count-pair guard was
what would have caught it). 0 took-effect guard failures, 0 restore failures across 18,210 stories.
The only `NO_EPS` is `testsets`'s 27 `axiom_contradiction` non-story members.

## The result

```
N_eps 9351   N_reach 9344   N_rail 9191        (cells; duplication-invariant)
qualifying transitions 9853  (+178 with an `unknown` endpoint, reported separately)
all located transitions, live legs        122031
MT-invariant / FT-only among qualifying     4602 = 46.7%

                       MOVED   DECISIVE
snare_epsilon_floor     4717          1
rope_epsilon_ceiling    6571       1936
```

**MOVED vs DECISIVE.** Crossing 0.46 flips the `snare_epsilon_floor` bit *by construction*, so a
gate-attribution count answers "did the bit change", not "did the gate decide". DECISIVE adds: the
type that gate's own clause produces is an MT endpoint. The operator ruled **decisive** on
2026-08-23, **having seen both scorings** — post-hoc specification, labelled as such in
`gate_readout.md`, with both tables kept so a cold reader can re-derive under either reading and
see the branch change if they do.

**The one decisive case:** `testsets_haiku3`,
`equal_protection_kernel__antisubordination_reading`, analytical seat, ε 0.4599→0.4600,
`tangled_rope → snare`, χ 0.680434→0.680582 (χ is not what moved), sole changed gate
`snare_epsilon_floor`. It appears in **1 of 3** same-model redraws of the same seed — 0 transitions
at ≈0.46 on `testsets_haiku` and `haiku2` — and haiku carries the noisiest floor in OQ-347's table
(65% seat-vector churn with ε pinned). A draw artifact is fully consistent. `kernel_v1`: 0 decisive
in 10,215.

**What actually happens at 0.46** is the mechanism recon predicted: `resolve_coalition_power/3`
upgrades `powerless → organized`, d drops, and **χ falls as ε rises**. The modal outcome is
`tangled_rope → naturalized` (4373 of 4717). The rope/snare split is dominated by χ and by a step
that moves d, not by the ε gate.

**Nearly half the effect is invisible to MT.** 4602 of 9853 qualifying transitions (46.7%) leave the
raw metric type unchanged and move only the signature-resolved type — the
`authority_vacuum_incommensurability` shape, far more prevalent than that single prior witness
suggested.

## Why G1b and not G0

G0 requires **none** attributing to `snare_epsilon_floor`; one does. G2 requires a
`snare_epsilon_floor` transition whose FT pair is exactly `{rope, snare}`; zero are. So G1, subtype
**G1b** (the transition exists but on a single leg). The operator's ruling states the principle
plainly: *the gate was frozen precisely so that a result which looks like G0 to the people who
wanted G0 cannot be scored as G0, and 1 ≠ 0.* Substantively both operator and executor read the
single case as a draw artifact. That reading does not get to override the frozen number.

Consequences: `[G1-D]` governs — Step D wrote `threshold_boundary_relabel.md` and landed **no
commit**; OQ-120 stays open and is re-specified; OR-1's closing trigger (G0) never reached.

**Controls.** C1 PASS ×19 (natural carrier, synthetic fallback never needed), C2 PASS ×19
(transition vanishes at floor 0.90, restore verified), C3 PASS ×19, C4 PASS ×10 /
precondition-absent ×8 / carrier-absent ×1, **0 FAIL**. C1 fired and C2 declined on every corpus, so
the prereg's precondition for interpreting the branches is met.

## The backstop census (branch-independent, filed as OQ-350)

`dr_claim_mismatch/4` has four clause heads over three mismatch types; **four of the six claimed
types have no clause in existence**. Re-derived from `pipeline_output.json`: mountain 14/21 (66.7%),
rope 2/52 (3.8%), snare 0/25, tangled_rope 0/143, scaffold 0/13, piton 0/4. `type_5_piton_as_snare`
fires 0× (OQ-307). **`naturalized` has no clause** is kept as a shape distinct from **low flag
rates** — a weak backstop and an absent one are different defects and must not collapse into one
fix. OR-2 was already ruled (new entry), so this is OQ-350, Priority 3 deliberately (inheriting
OQ-120's 1 would launder a verdict-surface change through a line filed as inert).

## Framing verdict on "lower-stakes... not gating anything"

**Revised, not reaffirmed.** The entry filed itself lower-stakes while carrying **Priority 1**, the
resolver's highest sort key; those do not sit together and the entry never said which governs. The
stakes turn out to live in the backstop gap, not the ε boundary — and the ε boundary is precisely
the part that measured inert. The "lower-stakes" reasoning rested on the dangerous concealment being
the mountain shape OQ-117 covered; the census shows four claimed types with no concealment surface
at all, which is wider than the rope/snare framing implies. Whether to rewrite the entry's own
framing is **OR-6**, the operator's seat, left open.

---

## Process findings

### 1. The plan's own correction was wrong on the merits, in the same error class as the original

**This is the strongest result in the report and it is about the apparatus, not the engine.**

The plan's Step D prescribed, under G0, relabelling `:593` from `rope, tangled_rope` to
**`rope, naturalized`**. The derivation was clean: `rope` needs χ ≤ 0.35, `tangled_rope` needs
χ > 0.35 *strict*, so raising ε past 0.45 at a rope-qualifying χ cannot reach `tangled_rope`; it
reaches `naturalized`. Correct about what the cascade *permits*.

**The witness says that pair is not modal.** Among 1936 `rope_epsilon_ceiling`-DECISIVE
transitions, `piton → rope` (1188) outruns `rope → naturalized` (617) roughly two to one, and the
modal **MT** pair is `rope → rope` (1163) — MT does not move at all and the change is
signature-layer. The χ-side movement that accompanies the ε crossing puts most decisive transitions
on the `piton` boundary, which no amount of reading the cascade would have told you.

So: **the replacement label the plan would have committed under G0 was itself wrong.** That is the
second time in this arc a confidently-derived label prediction failed against the engine's own
witness. The first was `:591`'s original `rope <-> snare` — which is what put the phrase "rope/snare
boundary" into OQ-120's premise in 2026-06-13 and generated this entire investigation. Same error
class, one layer down, **committed by the correction rather than by the original**.

This is stronger evidence for gating output-changing relabels behind a witness than anything the
plan argued in prose, because it is the plan's own arithmetic failing under exactly the discipline
it recommended. Reasoning from the cascade to a label is the move that produced the original error,
and it produced a second one under review conditions built to catch exactly that.

### 2. Eight instances of one drift shape, six of them introduced by fixes

Planning took six blind-review rounds and surfaced **eight instances of a single shape**: a
multi-branch gate's scope boundary restated in slightly different words at each site. **Two were in
the original draft; six were introduced by fixes for earlier instances** — five by cancelling an
action meant to survive, one by adding a commitment nobody had ruled on. Patching instances of the
drift surface generated more instances than it removed, until the surface itself was removed:
`[G1-D]` defines the boundary once, makes every other site a pointer, and forbids paraphrase **and**
addition. (Its first draft forbade only paraphrase; the eighth instance arrived as an addition.)
The counting was itself corrected in review, an earlier draft undercounting by exactly the newest
instance of the pattern it documents.

**At execution `[G1-D]` held.** G1b fired, and the single-definition boundary was unambiguous about
what Step D does and does not do. The construction worked.

### 3. Two defects in this audit's own instruments, caught before publishing a number

1. **Double emission.** `emit_transition`'s seat filter was a bare disjunction
   `( MT0 \== MT1 ; FT0 \== FT1 )` inside `forall/2`, which yields once per succeeding branch: a
   seat whose MT *and* FT both changed emitted twice, one whose MT was invariant emitted once.
   **Differential, not uniform** — `testsets` went 3228 → 1863 rows, not → 1614 — so every
   transition-level count was wrong *and* the MT-invariant share was understated relative to
   everything else. All 19 corpora re-swept. The cell counts `N_eps`/`N_reach`/`N_rail` are
   duplication-invariant and identical across both runs; that is a **consistency** check, not a
   discrimination check, and is not offered as evidence the fix was unnecessary.
2. **C4 reported an absent precondition as FAIL.** The carrier's suppression is a per-leg redraw
   property (haiku 0.35 / haiku2 0.28 / haiku3 0.42, but flash 0.60 / kimi 0.60). On 8 legs it sits
   at or above the snare floor and the control cannot run; the code printed `FAIL declined 0/55`.
   An absent precondition is a SKIP. Fixed.

The pre-fix output is preserved at `raw_PREFIX_double_emission/` with its own README, per
*build_discipline* → *when a defect is found, its before-commit is a free negative control*.

### 4. Findings about the plan

- **S8 is false as written.** Pristine `f88c8c3c` is GREEN; this audit's Step A turns two rows red
  (`audit writeup`, `apparatus`) because the new dir lacked `WRITEUP.md`. Settled with a two-sided
  control — restoring `INVESTIGATIONS.md` to HEAD and moving the dir aside returns both to green —
  so the reds are this audit's, **not** the concurrent leg generation's. The `apparatus` line's
  `NO DECLINE EVER RECORDED` clause prints in the GREEN state too and is *not* the red cause;
  reading it as one would have misattributed a pre-existing condition.
- **G0's test and G0's prose disagree**, and **G1's subtypes are not total** over G1's outcome
  space (under *moved*, the result is G1 but neither G1a nor G1b). Both are now ruled.
- **The recon manifest is stale** (`14:30:09Z`/`d7b4d4f` cited; on disk `16:36:35Z`/`885151b`).
  `n_constraints 285` matches and S11 re-derived identically.
- Minor: S4's T2 pin drifted `:459-462` → `:424-427`; S6 has an unnamed near-miss
  (`gap_diagnostic.pl`'s unrelated `threshold_boundary/**3**`); OQ-347 step 4's situation-fixed core
  does not exist yet, so the prereg's preferred population was declared absent, not substituted.
- **Not** findings, checked and withdrawn: the models-vs-legs denominator and the 19-corpus scope
  were both the plan's own 2026-08-23 revision (`plan:342`, `plan:270` + S5's list naming
  `haiku3`), carried into the frozen prereg — not substitutions made at scoring time.

## Non-authoring-facing

OQ-78 ruling 3 binds on this phase's **output**. `gate_readout.json` and `raw/` contain exact ε
values at which types flip — the numeric disclosure that ruling forbids reaching an authoring stage.
They must never feed a prompt, a seed file, or `epsilon_bin`, which stays a dangling wire on
purpose: re-wiring it would manufacture the concordance this OQ measures.
