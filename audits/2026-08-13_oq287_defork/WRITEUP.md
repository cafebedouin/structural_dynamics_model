# OQ-287 Pass A — de-forking the derivation

**Executed:** 2026-08-13 – 2026-08-14.
**OQ:** OQ-287 (Two live papers claim the same contribution).
**Verdict (scoped):** the derivation fork between `amnesiac_institution_v0_6.md` §2 and
`concealment_without_a_concealer_v0_4.md` is **closed for the derivation itself** — v0.6 now cites
and does not carry it. **Two limbs remain open**, so OQ-287 closes `mitigated`, not `resolved`: the
extraction (Pass B) and the §2.8/§2.9 apparatus redirect.
**Fired:** live
**Manifest:** no pipeline run; this pass touches documents and apparatus only. Code state at
execution: `73727587` → `96db0124`. Digests computed against `CWC` at `e995f978`+ and re-verified
at each landing.

---

## What was done

| step | landed | witness |
|---|---|---|
| A0 coverage diff | `fb0cbb86` | `COVERAGE_DIFF.md` — 31 mapped units, each with a concealment anchor or a preservation destination |
| A1 stale claims | `fb0cbb86` | `checks.sh row1` + 6 controls |
| E5 row repair | `e995f978` | every Appendix A row matches its header arity; 15/15 labels resolve once |
| C1 upstream + digest tool | `b880c217` | 15 pre-existing digests verified unmoved |
| A2 vacation | `974649e0` | `checks.sh row1 + row3`, exit 0 |
| `claim_cite_check` | `2c3139a3` | 10 controls; wired into `scripts/gate.sh` |
| A3 re-pointing | `7ecb56b3` | `checks.sh row4` green **by conforming**, twice red first |
| A4 + A5 | `96db0124` | `checks.sh row2` (9 assertions); all four rows green |

**Pass A is complete, and Pass B is SPECIFIED rather than promised** — `EXTRACTION_PROMPT.md` is
written and executes later, which was the plan's mitigation for its own named risk (that the
intermediate state becomes permanent). Stopping at a written specification rather than an authored
draft is deliberate: the next session can be adversarial about it.

## Evidence map

| artifact | what it holds |
|---|---|
| `COVERAGE_DIFF.md` | the A0 mapping table; findings 1–4. **Point-in-time; its reference counts are pre-A2 and superseded** — banner at the top says so |
| `A2_DRAFT_section2.md` | the reviewed §2 replacement, revisions 1→2. Its pins are **superseded** and wrapped in `PIN-RECORD` sentinels — a record of a past state, not a claim about a present one |
| `checks.sh` | **all four rows** of the verification table, executable, with 6 selftest controls |
| `claim_digest.sh` | **the definition** of a claim digest. Carries the whole-row rationale and the declared stopping point |
| `EXTRACTION_PROMPT.md` | **Pass B's receiver's prompt — written, not executed.** Gates (incl. OQ-278 as a checked gate), destination, salvage map, the sub-item redirect table, the acceptance condition **with its deletion test**, IV's candidate material with a scope bound, the unguarded residuals, and the receiver's licence to refuse |
| `A3_MAPPING_RULE.md` | what a re-pointed reference must say, fixed BEFORE the re-pointing so row 4 checks conformance rather than non-absence |
| `../../python/claim_cite_check.py` | the gate-wired checker. Carries the corpus-scope rationale and the aptness residual |

## Findings

**1. The coverage call came out partly inverted from the plan's expectation.** §2.4's
three-instance table is COVERED (concealment §5.4 carries the same three, expanded to prose);
§2.2's three ordinary examples are residue only 2-of-3; §2.7 is confirmed residue and is the
largest preserved block. Detail: `COVERAGE_DIFF.md` §2.

**2. A third stale claim, at four sites, two of which survive the vacation.** v0.6 carried the
**pre-narrowing** `A4` — "asserts content while carrying no standpoint" — which concealment §3.5
calls "the most contestable move in v0.1" and replaced with **warrant transfer**. Found at `:228`
(§0 table), `:550` (§2.5, vacated), `:2165` (§13, A4's step), `:2581` (Appendix D.1, a record of
the v0.1 claim — left alone deliberately). The §0 row's kill condition was *already* the
warrant-transfer form: the narrowing had been half-absorbed and stopped. Same label, changed
claim — the exact case the digest scheme exists for, found in the document before the scheme
existed.

**3. Concealment's Preface undercounted its own claim table**, and `E5`'s row was short a cell so
its kill condition rendered under *Support*. Both repaired before any digest pinned them. The
Preface now states that Appendix A is authoritative for the label set **and that this sentence has
already been stale once**.

**4. At least twelve false absences were produced by this pass's own instruments — and the ones that matter
are the ones that told me something I had not predicted.**

Each is a measurement whose *own framing* was not part of the query, returning an inventory that
read complete because every item in it belonged. Full table in `COVERAGE_DIFF.md` finding 4.

**The split that matters, because an instrument that confirms is not evidence the way an
instrument that discovers is:**

| | count | instances |
|---|---|---|
| **DISCOVERED** — the red light carried information I did not already have | 10 | `grep -v` dropping the one uneditable external reference; a §2-scoped sweep missing three surviving `A4` sites; a line-wrapped phrase read as absent; the sentinel-rationale comment registering as an occurrence of the string it explained; arm E's first draft passing for the wrong reason; `sha256sum` of an absent row returning the empty-string hash; one prose recipe implemented two incompatible ways in one turn; arm E's probe surviving *unwrapped* in §0 after §2.2 was vacated; `claim_cite_check` firing on its own selftest fixtures; arm 5 conflating "clean" with "checked nothing" |
| **CONFIRMED** — I had already predicted the state | 2 | row 1's containment check going vacuous after A2 (predicted when A2 was designed); the C1 quotation comparison splitting on a line break (the third instance of a class already known) |

**One sub-class deserves its own name, because a reviewer cannot catch it by reading the changed
lines: CORRECT CODE THAT BECAME INCORRECT WHEN ITS SUBSTRATE'S SEMANTICS MOVED UNDERNEATH IT, WITH
NO DIFF AT THE FAILURE SITE.** Three instances. (i) `rowN; exit $?` was right while `rowN` was a stub
whose return value *was* the verdict, and wrong the moment the row became real — no line edited.
(ii) `fail=$sfail` at the end of `selftest` was right while `selftest` ran alone, and silently
overwrote three earlier rows' verdicts once `all` accumulated — no line edited. (iii) `E5`'s row was
well-formed until the table around it defined a fourth column; the trailing cell then meant something
else — no diff at the row. In all three the changed thing was *meaning*, and every reviewable diff was
elsewhere.

Ten of twelve discovered. **And five of the ten were committed inside instruments built to catch
the earlier ones** — the digest tool's first draft, the checker's fixtures, two arm revisions, the
recipe. The repair generated fresh instances in its own apparatus, repeatedly, within one session.

**The count is a FLOOR, not a measurement.** Twelve is what the instruments and I noticed, drawn
from the same population the claim is about, by the same party the claim is about. No one enumerated
the total number of measurement acts in this pass, so there is no denominator and no rate — and a
thirteenth instance nobody caught is exactly what the finding predicts and exactly what this method
cannot see. Cite as *"at least twelve, self-observed, undenominated"*; never as a rate, and never as
a comparison against the nine of §7.4, which were counted differently.

**Why this is evidence and not hygiene.** It is a dated, prospective, twelve-instance witness for
v0.6 §2.9(b)'s claim that the remedy is **not self-terminating** — *declaring Π produces a new
artifact with a new Π*. §2.9(b) currently supports that claim with the nine §7.4 instances, which
are retrospective. **And the catch mechanism matches §7.4.1 exactly: not one was caught by a gate
reading its own output green.** Every one was caught by comparing a claimed value against the
artifact it described — by the operator, by a re-run, or by a control doing that comparison
mechanically.

**What actually terminated all twelve, stated plainly, because it is the finding.** **Not one was
caught by a gate reading its own output green.** Ten were caught by someone — the operator, a re-run,
or a control doing it mechanically — **comparing a claimed value against the artifact it described.**
That is §7.4.1's finding re-derived independently: a fresh arc, a different route, dated, prospective,
and with an adversarial second party in the loop that the earlier arc did not have. The nine
retrospective instances of §7.4 and these twelve are two independent arrivals at the same result.

**Carried forward:** this is the strongest available argument that §2.9 can be made **load-bearing**
in the practice paper's section IV rather than appended, which is Pass B's pre-registered
acceptance condition. To be named in `EXTRACTION_PROMPT.md` as candidate material for IV.

**5. The scheme paid for itself inside a day, and the fifteen that did NOT move are half the result.**

On 2026-08-14, A5 corrected `E1`'s **Owed** cell upstream: it read *"Prediction 1"* as pending when
that prediction had been run on 2026-08-13 and **withdrawn as a test of E1** — the measure turned out
to be selection rather than detection (OQ-293). Same claim text, changed row, no diff at the failure
site.

`E1`'s digest moved `884ea0b6 → 911a4db5`; `claim_cite_check` went red on **six citing sites**. Each
was **re-read, not bumped** (per-site verdicts: `A3_MAPPING_RULE.md` §8). All six survived — and one
*improved*, because being forced back to the text turned a gesture (*"which that paper marks as
unevenly supported"*) into the specific fact (*"whose one prediction was run and withdrawn as a test
of it"*).

**The other fifteen digests recomputed identical**, and that is not a footnote: **a scheme that fires
on everything is indistinguishable from one that fires on nothing.** The discrimination is the pair —
one row moved and exactly its dependents fired; fifteen rows did not move and nothing else did. That
is the argument for pinning *content* rather than *addresses*, demonstrated on this pass's own work
within a day of building it, and it is the one result here that a reader can check without repo
access by reading the two commits.

**6. The instrument regress is declared stopped**, per v0.6 §7.6, in `claim_digest.sh`'s header.
The tempting next move — a checker for the harness that checks the checker — is instance thirteen,
not the fix for twelve. What terminated all twelve was a party comparing a claimed value against an
artifact. **Falsifier, at the current tier:** *if a stale pin reaches a citing document undetected —
a pin that reads green while its row has moved — the stopping point was called too early and the
next instrument is owed.* (Finding 5 is the first evidence *for* the stop: the one row that did move
was caught.) `claim_cite_check`'s repo-wide corpus is what keeps that falsifier
well-formed; under a `docs/`-only scope it would be unfalsifiable for exactly the documents where
it is most likely to fire.

## Declared residuals

**None of these three is checkable, and each is a thing a future reader most needs and is least
likely to reconstruct.** They are the reason Pass A closes `mitigated`.

**R1 — `COVERAGE_DIFF.md`'s coverage calls are unverified, and pre-`C1`.** A0 decided, by hand, which
concealment claim each vacated v0.6 unit maps to. A3 then made that table the authority for ~11
semantic decisions. It was re-checked on 2026-08-14 (`COVERAGE_DIFF.md` §5) — but, carried verbatim
because a dated re-check otherwise reads as validation of the whole table:

> The re-check verifies the anchors, not the coverage calls — a row marked COVERED in A0 that was wrong then is still wrong, and nothing in this pass would catch it.

**R2 — `claim_cite_check` is blind to aptness, and the blindness is ANTI-GUARDED.** A green gate
over 60+ resolving citations reads as verification of the citation *set*; it verifies one relation
and is silent on the one carrying the meaning, so a reader who trusts it checks **less** than one
facing no instrument. A success-shaped token where aptness review would go — this pass's own
signature, produced by this pass's own apparatus, on the surface it cannot see. Mitigation carried
into `EXTRACTION_PROMPT.md` §8: every new citation owes a one-line note saying which claim it leans
on and why that row rather than a sibling. Not machine-checkable; it makes aptness *reviewable*,
which is §7.4.1's shape — not a better instrument, a second party comparing a claim against the
artifact. It verifies that a pin matches its row, never that
the row is the right one to cite at that site. A citation aimed at `A2` where the argument needs `A4`
reads green forever and stays green through every future narrowing of either. Its first real test was
the `§2.3` split (3 structural `A3` / 3 behavioural `E1`) and the discovery that two of the three `E1`
sites are **vocabulary borrows** rather than assertions — a distinction the checker cannot see and
that survives only because it is written into the prose and into `A3_MAPPING_RULE.md` §6(c).

**R3 — finding 4's count is a floor drawn from the population it describes.** Twelve is what the
instruments and I noticed, by the same party, in the same pass, with no denominator. A thirteenth
that nobody caught is exactly what the finding predicts and exactly what the method cannot see.
Never cite it as a rate, and never against §7.4's nine, which were counted differently.

**R4 — the intermediate state is live and one limb has no owner.** v0.6 is hollowed at §2 and
declared-temporary at §2.8/§2.9; the practice paper does not exist. See the two limbs in ISSUES
OQ-287 — the apparatus redirect is contingent on Pass B and guards an **already-sent** external
citation, so it carries its own review date rather than waiting on a paper that may be deferred.

**R5 — the section-only citation class is unguarded by construction.** `CWC` §5.1/§5.4/§9.1/§3.2 have
no Appendix A row, so references to them cannot be digested. Counted, never checked, and deliberately
not fixed by minting rows — that would be the instrument reshaping the substrate to fit itself.
Reproduce with `python3 python/claim_cite_check.py --list --unpinnable`.
