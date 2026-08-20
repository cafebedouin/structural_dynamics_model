# OQ-310 — the rule survives and gains its first independence-screened witness, but its binary does not describe its own population

**Executed:** 2026-08-20
**OQ:** OQ-310 (registers the falsifier for `amnesiac_institution_v0_6.md` §7.4's
invariant-versus-value rule). Inputs from OQ-328 (`22a8b5b3`), which landed the V04 rows and
deferred the instance-12 call here.

**Verdict (scoped to this repository's gate rows and audit-local integrity lines, at HEAD
`58039b6a`, 2026-08-20):** **Neither half of the registered falsifier fires**, and the rule
gains **one** non-retrodictive, independence-screened witness — the `apparatus` gate row going
RED on a malformed `Fired:` bit on 2026-08-19, by an arm authored 2026-08-10, eight days before
the rule existed. **But the rule's binary does not partition its own population**, and it fails
in the *opposite* direction from the one the pass predicted: 18 of 26 gate rows are
invariant-asserting and only **one** is a clean value check, so the sentence *"the repository
runs twenty-two gate rows, most of them value checks, and the rule says every one of them is
blind to this class"* is **false of the artifact it describes**. The population is
invariant-dominated. A third failure mode the binary has no cell for arrived from the record:
an invariant-asserting instrument that **false-fired**.

**Scope this is NOT:** not a claim about gates in general, not a measured defect rate, and not a
claim that the rule is true — one witness at natural-catch grade, over a population whose median
row-exposure is **7 days**, is a witness, not a confirmation.

**Substrate:** no pipeline run, no engine change, no API spend. Read-only against the repo plus
this audit's own files. Gate GREEN at open (26 rows) and at close, both pasted below.
`amnesiac_carriage_check.py`'s expectation set byte-identical to `22a8b5b3` at open.

**Fired:** live — the rule gains its first non-retrodictive witness; the Phase-1 prior's central
prediction is falsified by the Phase-2 measurement; a defect is found in a gate row's own output
(`omega_resolver.py:1040` prints a hardcoded `10/10` against 9 controls); §7.4's table gains a
twelfth instance and its property is restated; and this pass's own enumeration instrument was
caught excluding the rule's only positive, by a positive control asserted before the enumeration
was pinned.

---

## 1. What the falsifier returned

| | result |
|---|---|
| **(A)** a value-checking row that HAS caught an apparatus self-instance | **DOES NOT FIRE** |
| **(B)** an invariant-asserting row that MISSED one | **DOES NOT FIRE** — declared null, power stated |

**Falsifier A.** Both standing candidates sat on the `apparatus` row, and both fail — for
**different** reasons, which is the frozen screens doing work rather than one screen absorbing
both:

- **34/33 feedback cap** — `screen 0` eliminates it. A cap breach is a **budget breach, not a
  defect**: nothing was wrong, there were too many entries, and a threshold said so. The channel
  is a gated exchange, so exceeding it is the mechanism operating.
- **malformed `Fired:` bit** — screen 0 **admits** it as a defect, but the catching arm is
  **not** a value check. `check_catch_bits` partitions *parseable / present-but-unparseable /
  absent* by construction (`FIRED_ANY_RE` is the strict prefix of `FIRED_RE`), so a malformed bit
  cannot satisfy it by any route. It is a catch by an invariant.

No other value-checking row, forward or retrospective, is recorded as having caught an apparatus
self-instance. What value checks did do in the window is **miss** one: `checks.sh` row 3 asserted
the near end of a pointer pair and nothing checked the far end — *"delete or renumber §III and
both markers dangle with the gate green."*

**Falsifier B is a declared null with power stated, not a clean negative.** Three properties of
the population limit it: median row-exposure **7 days** (8 rows ≤ 3 days; 695 row-days total);
jurisdiction is cleanly partitioned, so no in-scope miss was available (checked, not assumed);
and **6 of 26 rows assert nothing about the substrate at all**. Its discriminating control is
real — the search found a defect *in* an invariant-shaped instrument (§4) — so the null is not a
didn't-look; what it found was a false fire rather than a miss.

## 2. The witness, and why it clears the bar §7.4 set

`audits/2026-08-19_oq285_mode3_measurement_arm/audit_log.md:40-49`. OQ-285's own writeup carried
`**Fired:** **latent** — …`; the bit was **present and unparseable**; the `apparatus` gate row
went RED and was, in that audit's own words, *"the only thing that noticed."*

| | §7.4's rejected candidate | this witness |
|---|---|---|
| instrument authored | 2026-08-18 — **same day** as the rule | 2026-08-10 — **8 days before** |
| incident occurred | in the pass that produced the rule | 2026-08-19 — **1 day after** |
| author could have had the rule in view | yes | **no — it did not exist** |
| evidence grade | — | natural catch on an unrelated defect (top of the frozen ladder) |

**§7.4's rejection reasoning answers a different question than OQ-310's frozen screen 3 asks.**
§7.4 asked *did the rule PREDICT something?*; screen 3 asks *is the catch EVIDENTIALLY
INDEPENDENT of the rule?* Under screen 3 — frozen before any incident was read — predating the
rule is precisely what makes a catch independent, not what disqualifies it.

**Scoped honestly: this is an invariant over the bit's GRAMMAR, not over its TRUTH.** A
well-formed bit carrying the wrong token satisfies the arm. Reported at that altitude.

Two further forward-window catches (`dangle_count.sh`'s control arm; `standing_check.sh`'s
discrimination selftest) are **invariant-asserting and consistent with the rule**, but were
authored 2026-08-20 by an author who on the same day wrote about §7.4 directly. Per screening
rule 3 they are **compliance data in their own column, never witnesses**. Tallies are reported
separately and never pooled — `forward_window.md`.

## 3. The binary does not describe its own population — and it fails the other way

`classification.tsv`, frozen before any incident history was read
(md5 `c6604cc6bd0625253788045179a6035e`):

| stratum | invariant | mixed | value | n |
|---|---|---|---|---|
| **1** — exit-coded gate rows | **18 (69%)** | 7 | **1** | 26 |
| **2** — printed integrity lines in executable artifacts | 23 (16%) | 1 | **117 (83%)** | 141 |

**The two strata have opposite shape distributions**, which nothing in §7.4 anticipates.

**The Phase-1 prior's central prediction is falsified.** The prior (md5
`8f2380b58824641d6afefc2bf3f5b3ca`) put mixed at ~13 of 22 and predicted the binary "may not
partition" because most rows are mixed. Measured: mixed is 7, invariant is 18. The prior's
*other* pinned prediction — value-leaning ≤ 3 of 26 — is confirmed at 1. Three of the four named
hard calls moved off *undecidable* to **invariant** on the frozen criterion *"the derived
artifact must regenerate identically"* (`omega index`, `claim cites`, `oq277 freeze`); the fourth
(`bound selector`) stayed **mixed**, because the assertion is set closure and the detector is a
regex and both frozen criteria genuinely apply at different layers.

**So the does-not-partition outcome lands, but inverted.** §7.4 prices its falsifier's cheapness
against *"twenty-two gate rows, most of them value checks."* That premise is false: exactly one
gate row is a clean value check, and it is `paper carriage` — **the checker written for §7.4**.

**`paper carriage` is the pass's sharpest single finding.** `amnesiac_carriage_check.py:2` titles
itself *"an INVARIANT-asserting carriage check"*; `:12-14` of the **same docstring** says it
*"asserts, per enumerated site, the expected number of hits"* — a plain description of a value
check; `:171` implements `ok = actual == expected` against hardcoded integers. The contradiction
is inside one docstring, ten lines apart, not between docstring and code.

**Six of 26 rows assert nothing about the substrate.** `python env st`, `omega selftest`,
`claim cites st`, `axis boundary`, `cli selftest`, `tripwire hook` are instrument-only. The
`axis boundary` case is the one worth a tripwire: the **gate row runs `--selftest` only**
(`gate.sh:126`), so the live reachability sweep — the thing that would catch a new cross-axis
read — is **not in the gate**. The row proves the detector discriminates; it does not look at the
substrate.

## 4. A third cell the binary has no room for: invariant assertions that FALSE-FIRE

`audits/2026-08-19_oq242_absence_semantics/WRITEUP.md`, "Instrument note": the frozen-corpus
md5-of-md5s fingerprint hashes `md5sum`'s output **lines**, which contain path strings, so a
relative-vs-absolute mismatch *"produced a spurious 'corpus moved' reading during this audit
before being identified."*

That instrument is invariant-shaped — *the corpus must be byte-identical across the diff pair* —
and it emitted a **false positive**. §7.4's rule is about detection (catch / miss) and has no
cell for this. It arrived from the record, not from argument, and it is also the discriminating
control for §1's falsifier-B null: the search is demonstrably not blind to invariant instruments.

## 5. Screen 0's calibration: eliminates 0 of 11

`screen0_calibration.md`, run **after** the Phase 2 freeze and **before** Phase 4 opened, per the
operator's execution correction. Applied to the eleven §7.4 derivation instances — material it
was not fitted to — screen 0 **admits all eleven**. Every one is either a claim that failed to
match the artifact it described, or a structural property broken; not one is a budget or
threshold breach.

**The pre-registered *eliminates one or more* outcome is scored NEGATIVE and reported as such:**
§7.4's eleven-instance base is **not** inflated with non-defects, and the "ten of eleven"
property needs no restatement over a surviving denominator. The declared seat failure (screen 0
was drawn with both candidates in view) stands as declared — and is now discharged empirically
rather than by assertion: a screen that admits 11 of 11 cases it was not built on, while
excluding the cap breach, is discriminating on its stated criterion.

## 6. Instance 12: ADMITTED

§7.4 states its own gate-row population twice with different values ("twenty-one", then
"twenty-two" seventeen lines later) against a live count of 26 — inside the registration of the
test built to give the rule its first witness.

**Ruled: it becomes instance 12.** Reasons, in order of weight:

1. It passes screens 0 and 2 cleanly. Excluding it would need a criterion that was not frozen.
2. §7.4 already shows how it keeps a non-member out — row 19's unit-06 leak stayed out of the
   table because it is a **catch, not a self-instance**. This one *is* a self-instance.
3. The "it only adds a confirming miss, not a test" objection is about **evidential value**, not
   **membership**. The table enumerates observed instances; it is not a table of tests.
   Dropping a genuine member because it does not help is the selection the paper warns about.

The property is restated **eleven of twelve**. The `7.4 numbered rows` gate row goes 11 → 12 —
an edit to a value-checking row that is itself under test — landed per §8.

## 7. A defect found in a gate row's own output, and not fixed in this commit

`python/omega_resolver.py:1040` prints `selftest: all positive controls fired (10/10)` as a
**hardcoded literal**. `selftest()` carries **9** numbered controls (`:531-:605`), and nothing
derives the banner from them. A gate row's own output publishes a count that disagrees with the
artifact it describes — the exact shape §7.4's instances are made of.

Per the Phase-1 close discipline, its fix lands in its **own dated commit after scoring**, with
the Phase 2 classification md5 beside it, so it provably could not have moved the result. The
count is repeated in `CLAUDE.md` and `docs/technical/omega_resolver.md`.

## 8. A live event inside this pass's own instrument

The **first** version of the stratum-2 enumeration command silently excluded
`freeze_frame.sh:105` — `partition_check`, **the rule's only positive** — because its token set
required `== [0-9]` and `partition_check` compares against a shell variable. The instrument built
to enumerate the population dropped the one member the population exists to retain.

It was caught by **asserting a known member before pinning the command**, not by reading the
output. Recorded in `PREREGISTRATION.md` §1 at the time, and repeated here because this pass is
about instruments that fail silently and it is one.

(Also collected: **there are two `partition_check` lines**, at
`2026-08-10_oq277_rq2_crosscoding/frame/freeze_frame.sh:53` and
`2026-08-18_appendix_b_discharge/frame/freeze_frame.sh:105`. §7.4 cites one.)

## 9. Plants: NOT RUN

Phase 5 was pre-registered as conditional on Phases 3–4 being inconclusive. They are not: A
returns a screened negative on both candidates, B returns a null with stated power and a real
discriminating control, and the forward window produced a natural-grade witness. Running plants
would have added floor-grade evidence to a question already answered above that grade. **Not
run, and declared — not silently skipped.**

## 10. What lands

- §7.4: instance 12 added; property restated *eleven of twelve*; the twenty-one/twenty-two
  premise corrected to what the walk measured (**invariant-dominated**, not "most of them value
  checks"); the rule gains its tested-at-OQ-310 line with the witness and its grade; the
  false-fire cell declared as unnamed.
- `amnesiac_carriage_check.py`: `7.4 numbered rows` 11 → 12, own commit, md5 beside it.
- `build_discipline.md`: **state an invariant AND exit on it** — the strata-disagreement finding,
  in the form the evidence actually took (§11).
- `ISSUES.md`: OQ-310 → `resolved`.
- `KNOWN_STATE.md`: the `axis boundary` selftest-only tripwire; the omega banner correction key.

## 11. The strata-disagreement outcome landed INVERTED, and that is the finding

The pre-registered row read: *stratum 1 zero catches + stratum 2 `partition_check` catches ⇒ the
rule survives as **legibility, not enforcement***.

What happened is the mirror image. **Stratum 1 produced the witness** — an invariant that
*exited non-zero* and stopped a commit. **Stratum 2's `partition_check` remains a non-exiting
`echo`** that printed `186 == 185` and needed a person to read it. Both are invariants; only one
was unmissable.

So the promotion is not *legibility instead of enforcement*. It is: **state an invariant AND
exit on it — the invariant is what makes the failure legible, and the exit code is what makes it
unmissable.** The pre-registered cell reached the right destination by a route it had backwards,
which is worth recording as a fact about the prediction, not smoothed over.

## Evidence map

| artifact | what it is |
|---|---|
| `PREREGISTRATION.md` | Phase 1: population pin both strata, per-row exposure table, frozen criteria, the four screens, the outcome table. md5 `49988021c27e0b46063f5c830d971637` |
| `evidence/prior.tsv` | the Phase 1 PRIOR, pinned as an artifact. md5 `8f2380b58824641d6afefc2bf3f5b3ca` |
| `evidence/phase1_freeze.md5` | the Phase 1 stamp |
| `evidence/s2_enum.sh` | the re-runnable stratum-2 enumeration command |
| `evidence/s2_candidate_lines.txt` | its 228 candidate lines |
| `evidence/s2_stage1.tsv` | stage 1: integrity line or not |
| `evidence/s2_classification.tsv` | stage 2: machine shape verdicts |
| `evidence/s2_hand_corrections.tsv` | every hand correction, with what was NOT hand-reviewed declared |
| `classification.tsv` | **stratum 1, all 26 rows**: shape, target, docstring-disagrees, the verbatim deciding line, prior, prior-vs-verdict. md5 `c6604cc6bd0625253788045179a6035e` |
| `evidence/phase2_freeze.md5` | the Phase 2 stamp |
| `screen0_calibration.md` | screen 0 over the eleven §7.4 instances — eliminates 0 of 11 |
| `forward_window.md` | the primary arm: W1–W7, screened, tallies never pooled |
| `incidents.md` | the two standing candidates adjudicated; the pre-window record; falsifier B's power |
| `gate_open.txt` / `gate_close.txt` | the gate at both ends |
