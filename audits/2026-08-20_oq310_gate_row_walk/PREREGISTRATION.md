# OQ-310 — Preregistration: gate-row walk against the invariant-versus-value rule

**Written:** 2026-08-20, at HEAD `58039b6a`, **before any incident history was consulted.**
**Scope:** read-only against the repo. No corpus run, no engine change, no API spend.

## 0. The rule under test

`docs/amnesiac_institution/amnesiac_institution_v0_6.md` §7.4, minted 2026-08-18:

> A gate detects an apparatus self-instance if and only if it asserts an invariant the defect
> cannot satisfy; a gate that checks a value against an expectation does not.

Registered falsifier (OQ-310), either half of which kills or narrows it:

- **(A)** a *value-checking* row that HAS caught an apparatus self-instance, or
- **(B)** an *invariant-asserting* row that MISSED one.

## 1. Population pin

### Stratum 1 — exit-coded rows in `scripts/gate.sh`

HEAD `58039b6a`. `md5(scripts/gate.sh) = 3ad2911661e76dfe9383ec74d94fd1e4`.
`/usr/bin/grep -c '^run ' scripts/gate.sh` -> **26**.

**Exposure is attributed PER ROW, not per checker** — 26 rows over 22 checker files
(`omega check`/`omega selftest`/`omega index` share one file; `python env`/`python env st` share
one; `claim cites`/`claim cites st` share one). A row's exposure starts at the commit that
introduced its `run "<name>"` line, recovered by
`git log -S'run "<name>"' --diff-filter=AM --reverse -- scripts/gate.sh | head -1`.

| # | row | checker | row added | exposure-days at 2026-08-20 |
|---|-----|---------|-----------|------|
| 1 | python env | python/python_env_check.py | 2026-08-18 | 2 |
| 2 | python env st | python/python_env_check.py | 2026-08-18 | 2 |
| 3 | issues_status | python/issues_status.py | 2026-06-14 | 67 |
| 4 | omega check | python/omega_resolver.py | 2026-06-14 | 67 |
| 5 | omega selftest | python/omega_resolver.py | 2026-06-14 | 67 |
| 6 | omega index | python/omega_resolver.py | 2026-06-18 | 63 |
| 7 | spec enums | python/spec_enum_check.py | 2026-08-06 | 14 |
| 8 | doc patterns | python/doc_pattern_check.py | 2026-08-14 | 6 |
| 9 | bound selector | python/bound_selector_check.py | 2026-08-17 | 3 |
| 10 | dispatch head | python/dispatch_head_check.py | 2026-08-17 | 3 |
| 11 | codewalk caller | python/codewalk_caller_check.py | 2026-08-18 | 2 |
| 12 | displaced cites | python/pattern_citation_check.py | 2026-08-17 | 3 |
| 13 | module bounds | python/module_boundary_check.py | 2026-08-18 | 2 |
| 14 | claim cites | python/claim_cite_check.py | 2026-08-13 | 7 |
| 15 | claim cites st | python/claim_cite_check.py | 2026-08-13 | 7 |
| 16 | known_state | python/known_state_status.py | 2026-06-14 | 67 |
| 17 | axis boundary | python/check_axis_boundary.py | 2026-06-23 | 58 |
| 18 | audit cites | python/audit_citation_status.py | 2026-06-26 | 55 |
| 19 | paper carriage | python/amnesiac_carriage_check.py | 2026-08-18 | 2 |
| 20 | audit writeup | python/audit_writeup_gate.py | 2026-08-06 | 14 |
| 21 | apparatus | python/apparatus_instrument.py | 2026-08-10 | 10 |
| 22 | gap surfaces | python/check_gap_status_surfaces.py | 2026-07-01 | 50 |
| 23 | sunset | python/sunset_check.py | 2026-08-19 | 1 |
| 24 | cli selftest | python/cli.py | 2026-06-27 | 54 |
| 25 | tripwire hook | python/pretooluse_tripwires.py | 2026-08-13 | 7 |
| 26 | oq277 freeze | python/audits/oq277_build_prereg.py | 2026-08-11 | 9 |

Row-exposure total **695 row-days**; median row **7 days**. Eight rows are <= 3 days old.
The 22 checker files total **9,273 lines**.

### Stratum 2 — printed integrity lines in executable artifacts

Per the 2026-08-20 operator ruling: printed integrity lines in **executable** artifacts
(manifests, drivers, audit frames). **Bound: not prose claims in documents.**

Enumerated by a stated, re-runnable command, stored at `evidence/s2_enum.sh`:

```
find audits -type f \( -name '*.sh' -o -name '*.py' \) -print0 \
 | xargs -0 /usr/bin/grep -nE '^[[:space:]]*(print\(|echo )' \
 | /usr/bin/grep -E '(PASS|FAIL|MISMATCH|mismatch|GREEN|RED|OK|expected|EXPECT|==|!=)' \
 | sort
```

Yields **228 candidate lines across 89 files** (`evidence/s2_candidate_lines.txt`).
This is a *candidate* set. Phase 2 classifies each candidate into
{is an integrity line, is not} **and** its shape, so the denominator is two-stage and
recoverable next pass.

**Positive control on the enumeration command, run before pinning it.** The command must return
`audits/2026-08-18_appendix_b_discharge/frame/freeze_frame.sh:105` — `partition_check`, the
rule's single positive. **The first version of this command did not**, because its token set
required `== [0-9]` and `partition_check` compares against a shell variable. The instrument
built to enumerate the population silently excluded the one member the population exists to
retain. Recorded here as a Phase-1 event and carried into the writeup as a datum in its own
right: an apparatus self-instance committed *by this pass, inside this pass's own enumeration
instrument*, caught by asserting a known member rather than by reading the output.

## 2. The PRIOR (pinned, not a result)

A shape pass over the 22 checkers already exists in the approved plan. It cannot be unseen by
the executor, so it is recorded here as an artifact and stamped, and Phase 2 measures *against*
it rather than pretending it does not exist. **This pass has no blind, and must not claim one.**

Prior, as stated in the plan:

- roughly **9 clean invariant**, **~13 mixed**, **~2 value-leaning**;
- **four calls genuinely undecidable** — `omega index --check` and `oq277_build_prereg`'s
  pre-freeze arm ("the stored artifact must equal a fresh derivation" is value-shaped by the
  letter and invariant-shaped by intent); `claim_cite_check`'s digest pin; `bound_selector_check`,
  where the *assertion* is set closure but the *detector* is a regex;
- `amnesiac_carriage_check.py:2` declares itself "an **INVARIANT-asserting** carriage check"
  while `:171` implements `ok = actual == expected` against hardcoded integers;
- `partition_check` is a non-exiting `echo` inside a manifest heredoc, read by a person;
- therefore **almost nothing is cleanly value-checking**, so falsifier A has a tiny population —
  itself a narrowing of the rule.

Prior counts are recorded per item in `evidence/prior.tsv`, frozen with this document.

## 3. Classification criteria (applicable by a cold reader)

- **invariant-asserting** — asserts a structural property the substrate cannot satisfy while
  defective: partition/set-equality, totality, per-index agreement, IFF closure, dispatch shape,
  "the derived artifact must regenerate identically."
- **value-checking** — compares an output against an expected value, count, pattern or regex.
- **mixed** — a **first-class verdict, never a tie-break.** Given the prior's ~13, the honest
  outcome may be that the rule's binary does not partition its own population.

**Source decides; the docstring is a separate column.** The verdict comes from the
implementation. *docstring-disagrees* is recorded as its own column and is itself a finding.

## 4. Screening rules — pre-committed, applied in order

### (0) Is the incident a defect at all?

The rule concerns *defects*: a claim failing to match the artifact it describes, or a structural
property broken. **A budget or threshold breach is not a defect** — nothing is wrong, there is
too much of something, and a threshold says so. That is the gate doing its declared job.
Admitting such firings would inflate falsifier A with instances that are not instances.

Stated in general terms only; **no candidate's verdict is pre-rendered here.**

**Declared: screen 0 was drawn with both standing falsifier-A candidates in view.** That is the
seat failure one level up — a criterion authored by someone who can see which incidents it will
admit. The screen is believed correct on its merits, but correctness reached that way is
declared, not assumed clean.

**Calibration, and when it runs.** Screen 0 is calibrated against material it was not fitted to:
it is run over the eleven §7.4 derivation instances. **That run happens after the Phase 2
classification md5 freeze and before Phase 4 opens** — not at Phase 1. The eleven instances are
incident records naming what caught each one or failed to; reading them at Phase 1 would hand
the Phase 2 classifier exactly the knowledge Phase 2 exists to exclude, for the eleven most
anchoring cases available. Nothing is lost by the move: screen 0's *text* is stamped here at
Phase 1, and the anti-fitting property comes from that stamp, not from the run's position.

- *Eliminates none* -> consistent with the rule's own evidential base; the fitting worry is
  discharged empirically rather than by assertion.
- *Eliminates one or more* -> **§7.4's eleven-instance base is itself inflated with non-defects.**
  A finding about the paper substantially larger than anything else in this pass. Recorded as a
  first-class outcome, not a footnote to the screen.

### (1) Construction is not a catch — narrowly defined

A row minted in response to a *specific already-known defect instance D*, going red on **that
same D**, is construction. Going red on a *later, new* instance of the class it guards **is a
genuine catch.** (The broad version voids every catch, since every gate catches what it guards.)

### (2) Self-instance vs substrate-instance

The rule concerns defects in a control, gate, checker, manifest or selftest. A row catching a
corpus/engine/doc-content defect is evidence about neither side.

### (3) Independence — ASYMMETRIC, and the asymmetry is load-bearing

Per catch, record (a) whether the catching instrument **predates 2026-08-18**, and (b) whether
its author was demonstrably working from the rule (commit message and the instrument's own
docstring). Then:

- **Confirmations are gated by independence.** A catch consistent with the rule, by an
  instrument built under it, is a **compliance** datum in its own column — never a witness for
  the rule's truth.
- **Falsifications are EXEMPT.** A value-checking row catching an apparatus self-instance fires
  falsifier A regardless of what its author knew. An author who believed §7.4 would not have
  built a value check *expecting* it to catch this class, so authorial knowledge makes the
  finding **stronger**, not weaker.

Symmetric screening would be fatal: in a solo repo every instrument built after 2026-08-18 is
presumptively "built under the rule," so a symmetric rule makes the primary arm structurally
incapable of returning anything but `partial`.

## 5. Evidence grading

natural catch on an unrelated defect **>** before-commit pair (fires at N, declines at N-1)
**>** plant-and-restore. Plants are the floor and are reported at that altitude.

## 6. Outcome table — every cell written before the run

**Outcomes are scored independently. They are NOT mutually exclusive and any combination can
land.** Screen 0 eliminating derivation instances can co-occur with does-not-partition, and both
can co-occur with a null falsifier result. Each row below is evaluated on its own evidence.

| outcome | what lands |
|---|---|
| **Rule survives** — falsifier A and B both return empty on screened evidence | §7.4 keeps its falsifier and gains "tested at OQ-310, n witnesses at grade G, independence-screened"; promote the construction rule to `build_discipline.md`. `resolved`. |
| **Falsifier A fires** — a value-checking row caught a screened apparatus self-instance | Amend §7.4's sentence and its "ten of eleven" property, and the OQ-309 writeup's closing round. `resolved` with the killing witness. |
| **Falsifier B fires** — an invariant-asserting row missed a screened apparatus self-instance | Same amendment path, with the missing witness. `resolved`. |
| **Strata disagree** — stratum 1 zero catches, stratum 2 `partition_check` catches | Rule survives as **legibility, not enforcement**: §7.4 restated, and `build_discipline.md` gets *state an invariant **and exit on it***. `resolved`. |
| **Does not partition** — the mixed column is large (prior puts ~13 of 22) | §7.4's binary is replaced by a graded statement. A narrowing, not a null. |
| **Screen 0 eliminates §7.4 derivation instances** | First-class finding: the eleven-instance base is inflated with non-defects; the "ten of eleven" property is restated over the surviving denominator. **Independent of the falsifier's verdict; reported even if the walk is inconclusive.** |
| **Never-fired column dominates** | Report as an exposure fact, not a rule fact — graded in row-days from the table in section 1. A row with 1-3 days of exposure and no incident is evidence about neither side. |
| **Inconclusive / every forward catch scores as compliance** | §7.4 gains a sentence on why the record could not decide it and what would. `partial`, graduation step named. |

## 7. Phase order, frozen

0. Population intact — gate GREEN, 26 rows, carriage expectation set unmoved. **[DONE]**
1. This document + `evidence/prior.tsv`, md5-stamped. **No incident history.**
2. Classification of both strata against the pinned prior -> `classification.tsv`, md5-frozen.
3. **Screen 0 calibration over the eleven §7.4 derivation instances** — after the Phase 2
   freeze, before Phase 4.
4. Forward window (2026-08-19/20) — the primary arm -> `forward_window.md`.
5. Retrospective walk, including the two `apparatus`-row candidates -> `incidents.md`.
6. Plants, only if 4-5 are inconclusive.
7. `WRITEUP.md`, ISSUES/KNOWN_STATE, outcome-dependent paper edits.

**Phase 6 close discipline, stated now so it is not improvised under pressure.** If the walk
finds a genuine defect in a stratum-1 row, "GREEN at close" and "report the finding" pull against
each other. Resolution — the same one the plan already uses for instance 12: **land the fix in
its own dated commit with the Phase 2 classification md5 pasted beside it, after scoring is
complete.** The fix is never folded into the scoring commit, and scoring is never delayed to keep
the gate green.

**The instance-12 call is made last**, after classification is frozen and all scoring is
complete. If admitted, `7.4 numbered rows: 11` must go to 12 or the gate is red at close — an
edit to a value-checking row that is itself under test. That edit lands explicitly: dated, in its
own commit, with the Phase 2 classification md5 pasted beside it. If the call is *not* to admit,
that is recorded too — a deferred instance and an unmade edit are different states, and only one
leaves an obligation.
