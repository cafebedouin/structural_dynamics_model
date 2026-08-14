# OQ-287 Pass A — de-forking the derivation

**Executed:** 2026-08-13.
**OQ:** OQ-287 (Two live papers claim the same contribution).
**Verdict (scoped):** the derivation fork between `amnesiac_institution_v0_6.md` §2 and
`concealment_without_a_concealer_v0_4.md` is **closed for the derivation itself** — v0.6 now cites
and does not carry it. **Two limbs remain open**, so OQ-287 closes `mitigated`, not `resolved`: the
extraction (Pass B) and the §2.8/§2.9 apparatus redirect.
**Fired:** live
**Manifest:** no pipeline run; this pass touches documents and apparatus only. Code state at
execution: `73727587` → `974649e0`. Digests computed against `CWC` at `e995f978`+ and re-verified
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
| `claim_cite_check` | this commit | 9 controls; wired into `scripts/gate.sh` |

**Still open:** A3 (re-point **23** refs — the plan's 33 was measured pre-A2; 10 of them lived
inside the vacated subsections and left with them, see `A3_MAPPING_RULE.md` §1), A4 (§13), A5 (canonicity markers + `CWC` Preface), A6
(record), Pass B (`EXTRACTION_PROMPT.md`).

## Evidence map

| artifact | what it holds |
|---|---|
| `COVERAGE_DIFF.md` | the A0 mapping table; findings 1–4; the measurement set A3's positive half needs |
| `A2_DRAFT_section2.md` | the reviewed §2 replacement, revisions 1→2. Its pins are **superseded** and wrapped in `PIN-RECORD` sentinels — a record of a past state, not a claim about a present one |
| `checks.sh` | rows 1 and 3 of the verification table, executable, with 6 controls. Rows 2/4 declared and exit 3 |
| `claim_digest.sh` | **the definition** of a claim digest. Carries the whole-row rationale and the declared stopping point |
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

**Carried forward:** this is the strongest available argument that §2.9 can be made **load-bearing**
in the practice paper's section IV rather than appended, which is Pass B's pre-registered
acceptance condition. To be named in `EXTRACTION_PROMPT.md` as candidate material for IV.

**5. The instrument regress is declared stopped**, per v0.6 §7.6, in `claim_digest.sh`'s header.
The tempting next move — a checker for the harness that checks the checker — is instance thirteen,
not the fix for twelve. What terminated all twelve was a party comparing a claimed value against an
artifact. **Falsifier, at the current tier:** *if a stale pin reaches a citing document undetected —
a pin that reads green while its row has moved — the stopping point was called too early and the
next instrument is owed.* `claim_cite_check`'s repo-wide corpus is what keeps that falsifier
well-formed; under a `docs/`-only scope it would be unfalsifiable for exactly the documents where
it is most likely to fire.

## Declared residuals

- **Aptness is unchecked.** `claim_cite_check` verifies that a pin matches its row, never that the
  row is the right one to cite at that site. A citation aimed at `A2` where the argument needs `A4`
  reads green forever. Recorded in the checker's header. The partial mitigation is
  `COVERAGE_DIFF.md`'s mapping table — **re-checked against post-`C1` concealment on 2026-08-14**
  before A3 consumed it (`COVERAGE_DIFF.md` §5: every anchor still resolves; two rows now have a
  better target in `C1`). The re-check verifies the anchors, **not the coverage calls themselves**:
  a row marked COVERED in A0 that was wrong then is still wrong now, and nothing in this pass
  would catch it.
- **The intermediate state is live.** v0.6 is hollowed and declared-temporary at §2.8/§2.9; the
  practice paper does not exist yet. Tracked as OQ-287's two limbs.
- **`C1` is byte-identical in both papers until A2's vacation propagates** — closed as of `974649e0`.
