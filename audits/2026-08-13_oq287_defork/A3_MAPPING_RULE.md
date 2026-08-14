# A3 mapping rule — written BEFORE the re-pointing, so row 4 checks conformance, not non-absence

**Executed:** 2026-08-14. **OQ:** OQ-287, step A3.
**Status:** EXECUTED 2026-08-14. §§1–5 are the rule as written *before* the edit; §6 records what
actually happened, including where the edit deviated from the rule.

A check written after the edit it verifies is fitted to that edit. This file fixes what a
re-pointed reference *should* say, so `checks.sh row4`'s positive half can assert **conformance to
a rule** rather than the mere absence of a `§2.[1-7]` string — which is satisfied by deleting a
reference as readily as by re-pointing it.

---

## 1. The population is 23, not 33 — re-measured, and reconciled

The plan's 33 (4/7/7/3/7/4/1) was measured **before** A2 rewrote §2 from 275 lines to 92. Ten of
the 33 were cross-references *inside* §2.1–2.7 and left with the subsections they lived in.

| | count |
|---|---|
| recorded pre-A2 (plan, `COVERAGE_DIFF.md` §4) | 33 |
| of those, inside the vacated range → gone with it | 10 |
| outside the vacated range → **the real A3 population** | **23** |
| plus the vacation notice's own `§2.1–§2.7` self-mention (`:380`) | 2 |
| **live `§2.[1-7]` occurrences today** | **25** |

Reconciliation is exact: 23 + 2 = 25. Verified against `fb0cbb86` (pre-A2) and HEAD.

**`:380` MUST NOT BE RE-POINTED.** The notice names what it vacated; that is the visible gap doing
its job. Row 4 must special-case it, and must fail if it disappears — a "completed" A3 that
silently re-pointed the notice's own text would read identical to a correct one under a
count-based check.

Bare `§2` refs: recorded 27, live **25** (two also lived inside the vacated range).

## 2. The mapping rule

### 2.1 Sourced references (23) — by what the citing sentence is *doing*, not by which number it names

| former | the citing sentence is about | maps to |
|---|---|---|
| §2.1 | working-set finitude; *"unbounded retention is not memory; it is a pile"* | `CWC:A1` (§3.1 carries the sentence verbatim) |
| §2.2 | the framing is a live parameter / not entailed / a summary cited without its scope | `CWC:A2` |
| §2.2 | **the P3-conditionality and the weaker reading available to a direct realist** | `CWC:A2` + `CWC` §3.2 — concealment states an explicit **weak form** v0.6 never had, so these refs land on a *better* target than they had |
| **§2.3** | **STRUCTURAL: no procedure operating on the compression can enumerate omissions** | `CWC:A3` |
| **§2.3** | **BEHAVIOURAL: parties default to recognition and enumerate only when forced** | `CWC:E1` |
| §2.4 | frame-completeness / *"every item in it belonged"* | `CWC` §5.4 — **unlabelled**, see §3 below |
| §2.5 | absence/presence collapse; one mechanism at N layers | `CWC:C1` |
| §2.5 | the no-seat pose produced structurally, nobody posing | `CWC` §5.1 |
| §2.6 | the two carriage rules; remedy is artifact discipline not honesty norm | `CWC:P1` + `CWC` §9.1 |
| §2.6 | one of the three worked repository phrasings | **`§2.A`** (preserved in v0.6, not concealment's) |
| §2.7 | anything | **`§2.D`** (preserved entire in v0.6) |

**§2.3 is the load-bearing disambiguation and the aptness surface `claim_cite_check` cannot see.**
v0.6's §2.3 merged two claims that concealment keeps apart: `A3` is a statement about what a
*procedure* can do, `E1` is a statement about what *parties* actually do — analytic vs empirical,
with different discharge conditions. A ref sent to `A3` where the sentence is about behaviour reads
green forever. Classified per-site in §4.

### 2.2 Bare `§2` refs (25) — three destinations, and the default is the notice

| the citing sentence | maps to |
|---|---|
| cites §2 for the **derivation or its conclusion** ("§2's spine", "the chain in §2", "in §2's terms") | the **vacation notice**, i.e. rewrite to name `CWC:C1` or the specific claim — never bare `§2` |
| cites §2 for a **repository instance or phrasing** preserved there | `§2.A`–`§2.D` as applicable |
| cites §2 **as a document location** ("§2 (rebuilt as derivation; §2.8 replaced)" in Appendix D; the canonicity marker's "this paper's §2") | **LEFT ALONE** — §2 still exists; these are historical or structural references to the section, not to the derivation |

**Default when genuinely ambiguous: the notice.** Rationale — the notice is the one destination that
is correct-but-imprecise rather than wrong: it names the conclusion and points at the pinned claims,
so a reader lands one hop from the right row. A `CWC:` pin chosen wrongly is *silently* wrong, and is
the failure mode this whole scheme has no guard for.

### 2.3 Form of a re-pointed reference

- A reference to a concealment **claim** carries a pin: `CWC:A3@fe7890db`. Never a bare label.
- A reference to a concealment **section that has no claim label** (§5.1, §5.4, §9.1, §3.2) is
  written as `` `CWC` §5.4 `` — **unpinned, and that is a declared hole**, see §3.
- A reference to preserved v0.6 material is a plain `§2.A`–`§2.D`.
- No re-pointed reference may be *deleted* to satisfy the check. Row 4 asserts the total is
  conserved.

## 3. Declared hole: unlabelled section references cannot be pinned

`CWC` §5.1, §5.4, §9.1 and §3.2 carry no Appendix A row, so `claim_cite_check` cannot guard
references to them: they resolve by section number, which is exactly the *label resolution* the
plan rejected as insufficient because it "passes silently through a row whose content changed."

Roughly 5 of the 23 land here (§2.4's two, §2.5's no-seat-pose refs, §2.2's weak-form refs).

**Not fixed in A3, and the reason is a rule this pass established:** minting Appendix A rows for
§5.1/§5.4/§9.1 would be authoring claims into the upstream paper *in order to make my citations
checkable* — the instrument shaping the substrate to fit itself. `C1` was different: it was a
conclusion the paper already argued and the notice needed to quote. Recorded as a residual for the
operator rather than taken.

## 4. Per-site classification of the 23

The §2.3 refs, which are the only genuinely contested ones:

| site | citing text | class | target |
|---|---|---|---|
| `:624` | "the one operation §2.3 says **cannot find the gap**" | structural | `CWC:A3` |
| `:646` | "on §2.3's logic that makes shape-with-lost-detail the more hazardous amnesia" | structural | `CWC:A3` |
| `:711` | "the party best positioned to notice is the one §2.3 says **cannot**" | structural | `CWC:A3` |
| `:1212` | "AbsenceBench is the empirical form of §2.3's central asymmetry" | **behavioural** — AbsenceBench is `E1`'s cited evidence | `CWC:E1` |
| `:1389` | "in §2.3's vocabulary, every rescue was a **forced enumeration**" | **behavioural** | `CWC:E1` |
| `:1525` | "recognition standing in for enumeration (§2.3)" | **behavioural** | `CWC:E1` |

The remainder, by rule §2.1: `:1622` → `A1`; `:226`, `:356`, `:1349`, `:1927`, `:2208`, `:2488` →
`A2`; `:1969` → `A2` + §3.2; `:743`, `:1884` → `CWC` §5.4 (unpinned); `:69`, `:2012` → `CWC` §5.1
(unpinned); `:886`, `:1148`, `:1739` → `C1`; `:1183` → `P1` + §9.1; `:1709` → `§2.A`.

Count: 3 `A3` + 3 `E1` + 1 `A1` + 7 `A2` + 2 §5.4 + 2 §5.1 + 3 `C1` + 1 `P1` + 1 `§2.A` = **23**.

## 5. What row 4 asserts

1. `§2.[1-7]` occurrences == **2**, and both are at the vacation notice's self-mention.
2. Re-pointed count == **23**, matched against this file's per-site table — **by site**, not by
   total, so a deletion plus a spurious addition cannot net out.
3. Every `CWC:` pin introduced by A3 resolves (delegated to `claim_cite_check`).
4. The §2.3 split is **3 structural / 3 behavioural**; a re-point that sends all six to one target
   fires, because that is the aptness error this rule exists to prevent and the only one a
   conformance check can catch.
5. Bare `§2` refs: each of the 25 accounted for as notice / `2.A`–`2.D` / left-alone, and the
   left-alone set enumerated here rather than counted.


---

## 6. Executed 2026-08-14 — deviations from this rule, recorded rather than absorbed

A3 landed. Row 4 is green. Two things came out differently from the projection above, and both are
written here rather than fixed by moving a threshold — the rule was written first precisely so that
a mismatch is information about the edit, not an inconvenience in the check.

**(a) The pinned-count gap was a real defect, not a bad projection.** Row 4 wanted ≥34 pins and saw
33. The missing pin was at `:1969`, which §4 classifies `A2` + §3.2 and which I first rendered as a
§3.2 section reference alone — dropping the pin the rule required. Worse, the replacement had
severed a sentence: the original ran *"…the weaker reading in §2.2's penultimate / paragraph, which
is sufficient…"* across a line break, and my substitution consumed the first half and orphaned
*"paragraph, which is sufficient for Parts II–IV and less interesting."* The bullet also still said
*"§2 binds readers"* when §2 no longer holds the derivation. Rewritten whole; the count reached 34
by conforming to the rule, never by relaxing it.

**(b) Section-only references are 6, not the projected 5.** The extra one is at the §0 `A2` row's
kill condition, which now carries **both** a pin and a `CWC` §3.2 pointer — the pin for the row, the
pointer for the weak form that makes the P3 caveat survivable. That is a better citation than the
projection, and it grew the declared-unpinnable class by one. The class is counted by
`python3 python/claim_cite_check.py --list --unpinnable`, which reports `docs/` and apparatus
populations separately; **cite that command, not this number.**

**(c) Two of the three `E1` sites are VOCABULARY BORROWS, and are now written to say so.** `E1` is
*empirical* — supported unevenly, and its owed Prediction 1 was attempted on 2026-08-13 and
**withdrawn as a test of E1**. A reference re-pointed from v0.6's merged §2.3 inherits that discharge
condition, which the original sentence never carried:

| site | what it does | how it is written |
|---|---|---|
| `:1212` AbsenceBench | genuinely cites model evidence, and already scopes itself (*"the theory needs only the weaker, substrate-independent version"*) | plain `CWC:E1@911a4db5` — apt as-is |
| `:1389` the nine rescues | borrows the recognition/enumeration **distinction** to describe nine instances this paper witnessed; its warrant is those instances, not `E1`'s generalization | *"In the vocabulary of `CWC:E1@…` — borrowed for its distinction, not leaned on for its generalization, which that paper marks as unevenly supported"* |
| `:1525` prove-before-you-replace | **definitional**: classifies one practice as recognition; asserts nothing about what parties typically do | *"(`CWC:E1@…`'s distinction; the classification here is definitional, not a claim about what parties typically do)"* |

This is the aptness residual arriving at its first real test. Row 4 confirms the label is present and
**cannot see any of the above** — the distinction between citing a claim and borrowing its vocabulary
is exactly the semantic relation `claim_cite_check` is blind to. The guard is that it is written down.

**(d) Row 4's own matcher had a one-sided framing.** Its window looked only *forward* from each
anchor and reported two correctly re-pointed sites as unpinned, because a citation reads as naturally
before its anchor as after (*"On `CWC:A3@…`'s logic that makes…"*). Widened to 220 before / 160 after.
The first widening used a bidirectional `grep -o` glob that backtracked catastrophically over the
250 KB single-line normalized text and hung past two minutes; the window is now extracted by index
arithmetic in python. Same semantics, no pathological matcher.


## 7. The bare-`§2` half, executed

25 bare `§2` references existed post-A2. **16 re-pointed, 9 left alone**, per rule §2.2 row 3.

Left alone — enumerated, not counted, because a count of 9 is satisfied by any nine:

| site | why it stays |
|---|---|
| canonicity marker ×2 | A5 rewrites the marker whole |
| construction note ×2 | a historical record of the v0.5→v0.6 build; rewriting it would falsify the record |
| §13 "The chain in §2" | A4's step, and must land in the **narrowed** form |
| Appendix D ×3 | a record of what past versions claimed |
| **"…which §2 cites and this paper instantiates"** | **created by A3 itself.** The rule projected 8 left-alone; the executed count is 9, because re-pointing the "Three terms" paragraph produced a new, correct, in-class reference: §2 genuinely *is* the location that cites the derivation. Recorded rather than forced back to 8. |

Row 4's arm 5 matches each of the nine against this list by content, so a stray bare `§2` fires by
name even though the total would still read 9.

**Final counts, and cite the commands rather than these figures** — they are true at 2026-08-14 and
the instruments are the authority:

| quantity | value | command |
|---|---|---|
| pinned `CWC:` citations in v0.6 | 47 | `checks.sh row4` |
| live citations repo-wide, all resolving | — | `python3 python/claim_cite_check.py --check` |
| section-only (unpinnable), `docs/` vs apparatus | 6 / — | `python3 python/claim_cite_check.py --list --unpinnable` |
| `§2.[1-7]` survivors | 2 (the notice) | `checks.sh row4` |
| bare `§2` survivors | 9 (enumerated above) | `checks.sh row4` |

*(47 exceeds the projected 34 because the bare-`§2` pass added 13 more pins than the rule's
arithmetic covered — rule §5 assertion 2 counted only the 23 sourced references. The threshold is a
floor, and it held.)*


## 8. The first live digest fire, 2026-08-14 — and the re-read it forced

A5 corrected `E1`'s **Owed** cell upstream: it read *"Prediction 1"* as though that prediction were
pending, when Prediction 1 had been run on 2026-08-13 and **withdrawn as a test of E1** (§11; ISSUES
OQ-293). Same claim text, changed row — the E5 shape a third time, in the document this pass made
canonical.

`E1`'s digest moved `884ea0b6 → 911a4db5` and `claim_cite_check` went red on **six sites**, five in
v0.6 and one in this file. That is the whole design working: a row's meaning changed, and every
document relying on it was made to look again.

**The re-read, per site — a digest fire demands a re-read, not a bump:**

| site | does it still hold? |
|---|---|
| v0.6 `:389` vacation notice, "the operational asymmetry at `E1`" | **yes** — cites the asymmetry, not E1's support level |
| v0.6 `:814` "In `E1`'s terms… a forced enumeration" | **yes** — vocabulary borrow |
| v0.6 `:1213` AbsenceBench as "the empirical form of `E1`'s central asymmetry" | **yes**, and it was already the most exposed: it cites E1 *as empirical*. The sentence's own hedge — *"the theory needs only the weaker, substrate-independent version"* — is what carries it |
| v0.6 `:1390` the nine rescues | **yes, and improved.** Its caveat now names the specific fact rather than gesturing: *"whose one prediction was run and withdrawn as a test of it"* |
| v0.6 `:1526` prove-before-you-replace | **yes** — definitional |
| this file `:159` | **yes** — the classification table's own citation |

All six survived and were re-pinned to `911a4db5`. **Nothing else moved**: the other fifteen digests
were recomputed before and after and are unchanged, so this was an edit to one row, not a cascade.
