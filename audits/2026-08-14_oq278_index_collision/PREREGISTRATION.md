# PRE-REGISTRATION — OQ-278 index-collision rulings (R1a / R1b / R2 / R4)

**Registered:** 2026-08-14, at HEAD `62922f29`, **BEFORE** the archaeology sweep of Step 3 ran
in this session.
**OQ:** OQ-278
**Rules on:** which mechanisms are members of the build-discipline defect taxonomy, and which
index each holds.
**Registered by:** implementing instance. **Ruled by:** the operator — every branch below
terminates in a ruling, none of them is self-executing.

---

## Why this is pre-registered rather than ruled from the plan

The `d0c3c5fb` precedent (the P3 failure-shape sweep) worked because its branches were written
by the operator before the search ran. The plan that produced this audit already summarizes the
evidence **with a direction**; a ruling made against that summary would not be independent of
the account. `amnesiac_institution_v0_6.md` §2.9 was careful to preserve exactly that
independence for the P3 demotion, and the same care is owed here.

So: branches now, evidence in `WRITEUP.md`, ruling after.

---

## The discriminating criterion — taken from the file, not invented here

`docs/technical/build_discipline.md:769-772`:

> Pattern 4 invents a *value* and feeds it to a downstream computation; Pattern 5 lets *absence
> itself* pass a *condition*. … Pattern 4's tell is a catch-all clause binding a constant;
> Pattern 5's tell is a comparison or quantifier whose driving table is empty.

Generalized: **distinct tell + distinct corrective ⇒ distinct member.**

## The three dispositions — the option set includes the one this repo actually practices

The plan's original option set was binary (peer member / demote). It omitted the third, which is
the local convention:

3. **A named, mechanism-bearing sub-instance under an existing pattern, with no index of its
   own.** `CM-P5`/`BD-P5` carries four of these (the OQ-178 dual; dead-by-range, OQ-113; the
   `[fail]`-mode absence gate, OQ-137) and `BD-P6` carries one — each with mechanism text, a
   dated exemplar, a diagnostic and a rule.

So "carries more operational evidence than any published member" does **not** entail "gets an
index." By local convention it entails "gets a worked section," which both claimants already
have. Every branch below must be read against all three dispositions.

---

## TWO CORRECTIONS TO THE PLAN'S EVIDENCE, recorded before the branches rest on it

Both were found while re-deriving pins for this pre-registration. Recording them here rather
than adopting the plan's figures silently, because R1b's strength is stated as a contrast
between these two counts.

**(i) `BD-P4` has NINE internal index cross-reference lines, not seven.** The plan lists
`:769, :771, :772, :901, :1102, :1902, :2112`. Also present, verified at HEAD `62922f29`:

```
$ /usr/bin/grep -nE 'Pattern[- ]4' docs/technical/build_discipline.md
686:## Pattern 4: Fabricated default — ...        <- the heading itself
769: 771: 772:  (the P4/P5 sibling paragraph)
890:defaults-on-empty get the Pattern-4 treatment ...      <- MISSING from the plan's list
901:**Relationship to the other patterns:** Pattern 4 is the value-level case ...
1102: 1902: 2112:
1759:This is the Pattern-4/6 shape (a green exit conceals the absence) ...  <- MISSING
```

`:890` is what the plan describes separately as "Pattern 6 citing the Pattern-4 treatment"; it
is an index cross-reference and belongs in the count. **Consequence: the renumbering footprint
priced below is larger than the plan priced it.**

**(ii) `BD-P3` has ONE internal index cross-reference, not zero.** The plan asserts "**Zero**
internal index cross-references (vs P4's 7). Cited by index nowhere in its own defining
document." That is false as stated:

```
1015:- the bound Pattern-3 probe reported **432** `natural_law` constraints — a result set
     produced by a query that **silently failed to dispatch** the lock clauses
```

— inside *Every diagnostic needs a positive control*. It is a genuine citation-by-index to the
mechanism, with a dated instance behind it (the 432-vs-404 `natural_law` count).

**The asymmetry survives the correction but is 1-vs-9, not 0-vs-7, and R1b must be argued on
the corrected figure.** A false-absence claim ("cited by index nowhere") inside a
pre-registration that rules on membership would be this taxonomy's own §5 firing on the
document that adjudicates it.

---

## R1a — is `fabricated-default` a member in its own right?

**Evidence standing at registration** (all verified at HEAD `62922f29`):
- mechanism text + dated exemplar (OQ-33) + tripwire diagnostic + blast-radius analysis;
- `:901` "Pattern 4 is the value-level case" — an **axis position**, not an instance;
- `BD-P6` cites "the Pattern-4 treatment" (`:890`) as a primitive it *composes* — the inverse
  of subsumption;
- 9 internal index cross-references (corrected count above);
- the frozen prereg measured `P4` coverage = 1 (`PREREGISTRATION.md:78`).

**Branches:**

| if | then |
|---|---|
| **A1** — its tell and its corrective are distinct from every published member's (the `:769-772` criterion applied outward, not only against P5) | **peer member.** It keeps a worked section and takes an index. Which index is R2. |
| **A2** — some published member's corrective already subsumes it | **sub-instance** under that member, no index. The `:769-772` sibling paragraph is retained and re-pointed. |
| **A3** — it names a discipline (a thing one does) rather than a defect shape (a way systems fail silently) | **demote** to a witness rule, as index 3 was. |

**Registered expectation: A1.** Stated so it can be wrong. The `:769-772` paragraph is an
explicit, authored distinction against the nearest neighbour, and `BD-P6` composing it is
evidence no published member subsumes it.

## R1b — is `bound-probe-bypasses-clause-order` a member in its own right?

**Materially weaker than R1a, and stated separately so that no joint recommendation hides
it** — that is the granularity error the plan criticizes in the paper's §5.2.

**Evidence standing at registration:**
- 1 internal index cross-reference (corrected), vs 9 for `BD-P4`;
- ~~**zero measured coverage** in the frozen prereg (`P3 = 0`, `PREREGISTRATION.md:78`) **with no
  account.**~~ **STRUCK 2026-08-14 (AMENDMENT 2 §A2.1) — THIS BULLET WAS FALSE.** The figure has
  an account, written four lines below it in the artifact cited: `no members — uncalibrated`.
  Reading a per-pattern (iii′) figure as a finding is named a **pre-registered error** by that
  same freeze (`:244` row 6). Struck from every consequent; not evidence either way;
- its corrective (`:645`, query unbound + post-filter) is genuinely distinct from the
  positive-control discipline, so it is **not subsumed**;
- but its honest description is a **Prolog-specific instrument defect** in that family — the
  spine table (`:2596`) lists its read site as "the `findall` result/count," which is the
  diagnostic layer's own read site.

**Branches: — ⚠️ VOIDED 2026-08-14 by AMENDMENT 1 (operator). Superseded by R1b′ below; kept
verbatim because a silently rewritten branch table is worse than a voided one.**

| if | then |
|---|---|
| **B1** — the zero coverage is explained by the corpus (audit dirs under-sample Prolog probe defects) AND its tell is distinct at the taxonomy's altitude, not only at the instrument's | **peer member**, index per R2. |
| **B2** — the zero coverage is unexplained, and its tell is a *language-specific instance* of "the probe didn't actually look" | **sub-instance** under *Every diagnostic needs a positive control* — which is where `:1015`, its one cross-reference, already sits. |
| **B3** — as A3 | **demote.** |

**Registered expectation: B2.** Stated so it can be wrong. Its single index citation is
*already* inside the positive-control section; the spine table already routes its read site to
the diagnostic layer. **If B2 lands, index 3 stays vacant in both documents and R2 simplifies
to a two-body problem.**

**Explicitly NOT a branch condition: "one claimant is left, so it wins by default."** OQ-278
flags that inference and leaves it un-acted. Sole occupancy is not membership evidence.

## R2 — who holds index 4?

**Branch on R1a.** If R1a returns A2 or A3, index 4 is uncontested and reads
`recap-as-witness`; R2 is discharged without a ruling. If R1a returns A1, two members need
addresses and the following ground applies.

**The ground, stated at its real strength.** `audits/2026-08-10_oq277_rq2_crosscoding/PREREGISTRATION.md:356`
publishes `P4 — Recap-as-witness substitution.` inside an artifact that is **md5-frozen
(`4118f64e`) and gate-enforced** (`scripts/gate.sh:45`, `run "oq277 freeze"`); altering it
breaks the freeze and every result behind it loses its pre-registration.

*Correction to the plan's characterization, which called this "a hard ground … not a churn
judgment":* the freeze does not **forbid** a renumbering. It makes one site **permanently
unrepairable** — the artifact would go on expressing the old numbering forever, correctly, as a
point-in-time record of what the out-of-harness coder was shown. That is stronger than churn and
weaker than a bar, and the ruling should be made on the accurate version.

> **⚠️ THIS CORRECTION WAS ITSELF AN OVERCORRECTION — operator, 2026-08-14.** "Permanently
> unrepairable" reads as stronger than it is. The frozen prereg is **point-in-time, the same
> class as the 228 JSON payloads and `FINDINGS.md:23`** — and Step 4 already rules that
> point-in-time artifacts are repaired **by the label set, not in place**. That is the
> mechanism, and `LABEL_SET.tsv` being mechanism-keyed is exactly what makes it work. So the
> frozen site is an *ordinary* member of a class this audit already has a repair for, and
> calling it unique inflated it into a ground it is not. **R2's real ground is the cost
> asymmetry — priced in AMENDMENT 1 below.**

| if | then |
|---|---|
| **C1** — the unrepairable frozen site outweighs `BD-P4`'s 9 in-document cross-references | index 4 = `recap-as-witness`; `fabricated-default` renumbers to the next free index. |
| **C2** — the reverse | index 4 = `fabricated-default`; `recap-as-witness` renumbers. The frozen artifact is annotated **externally** (never edited) with a pointer to this audit. |
| **C3** — neither, and the namespace freeze is made permanent | **no bare index is ever written**; `CM-P4`/`BD-P4` become the standing citation form and index 4 is deliberately never disambiguated. Step 0's interim convention is promoted rather than lifted. |

**Registered expectation: C3 if R1a=A1, else discharged.** Stated so it can be wrong. C3 is the
only branch under which the frozen artifact, the 9 cross-references, and the ~100 historical
citations are all simultaneously correct without a single edit — and OQ-278 already lists
"give every index a namespace so a bare 'Pattern 4' cannot be written" as a candidate shape.

*C1's condition text is superseded by the priced asymmetry in AMENDMENT 1; the three
dispositions C1/C2/C3 stand unchanged.*

## R4 — does the paper extend?

**Falls out of R1a/R1b, and is not independently ruled.** `amnesiac_institution_v0_6.md` §5.1
publishes the list and §5.2 explicitly declines index 4.

- Any branch adding a member ⇒ §5.1's table and §5.2's declining paragraph are both amended in
  the same change, and the amendment names this audit.
- Any branch adding none ⇒ §5.2's declination is replaced by the ruling, table unchanged.
- **Interacts with OQ-287.** The paper is under active revision by a peer; R4 is executed only
  after confirming no in-flight edit to §5.1/§5.2 (`git log -1 --format=%H -- docs/…`).

---

## PRICED FOOTPRINT of a renumbering — enumerated here, not in a single Step-5 clause

Worst case (R1a=A1 **and** R1b=B1, both peers, index 3 staying vacant ⇒ the set becomes
`1, 2, —, 4, 5, 6, 7, 8`). Sites that go wrong, at HEAD `62922f29`:

| # | site | count | note |
|---|---|---|---|
| 1 | `build_discipline.md` internal `Pattern 4` cross-refs | **9 lines** | corrected count; `:769,771,772,890,901,1102,1759,1902,2112` |
| 2 | `build_discipline.md` internal `Pattern 3` cross-ref | 1 line | `:1015` |
| 3 | `build_discipline.md` headings | 2 | `:601`, `:686` |
| 4 | `build_discipline.md` spine table | 2 rows | `:2596-2603` — **and its prose says "The five patterns", already wrong (omits P6)** |
| 5 | `CLAUDE.md` numbered list | 2 entries | `:506`, `:528` (post-Step-0 line numbers shift) |
| 6 | `CLAUDE.md` cardinality claim | 1 | `:158` "five live, index 3 vacated" |
| 7 | `README.md` cardinality claim | 1 | `:170` "the six defect patterns" — **already wrong since 2026-08-11** |
| 8 | paper, 6 versions | ≥6 | `amnesiac_institution.md:186` … `v0_6.md:1067`; only the current version is amended, earlier ones are point-in-time |
| 9 | `design_discipline.md` | 3 | `:464`, `:600`, `:710` |
| 10 | `KNOWN_STATE.md` | ~10 | index citations |
| 11 | frozen prereg | 1 | **READ, NEVER WRITTEN** — see R2 |
| 12 | 228 machine-generated JSON payloads (oq277 audit) | 228 | point-in-time, not retro-edited |
| 13 | the Step 3 label set | **0** | keyed on `mechanism_slug`, index survives only as `raw_text_as_found` — **valid under every branch** |

**~26 live editable sites** (rows 1-10 excluding point-in-time), plus 2 already-wrong cardinality
claims that a renumbering must fix regardless of which branch lands.

Under **C3** the footprint collapses to rows 6, 7 and 4's prose — the three claims that are
wrong *today*, independent of any ruling.

---

## What is NOT pre-registered here

- **R3 is dropped as a separate question.** It is not independent of R1 ("does Pattern 3 name
  bound-probe" presupposes bound-probe is a member), and it buys almost nothing: pre-2026-08-11
  citations stay ambiguous whatever is ruled, and forward citations are handled by Step 0's
  namespacing without ruling it at all.
- **R5 is dissolved.** Namespacing needs no prohibition gate — a gate forbidding bare
  `Pattern N` would run >50% false positives (`prolog/diagnostic_summary.pl:374`'s independent
  `P1`-`P10` conflict catalog, essay headings, recon flags, `Priority:` levels, and a Prolog
  variable named `P3`; of 146 raw `Pattern 3` hits only ~68 are this taxonomy) and would have to
  be diff-scoped, which no `gate.sh` row is.
- **The slug pick** at `build_discipline.md:601` — the heading now carries **both** strings
  (`## Pattern 3 — Bound-probe bypasses clause-order (query-binding-bypasses-cut)`), so no pick
  is forced. If R1b demotes, it dissolves entirely.

---

# AMENDMENT 1 — 2026-08-14 (operator review of the ruling request)

Appended, never rewritten: the original R1b branch table is marked VOID **in place** and kept
verbatim above. A pre-registration that silently rewrites its own branches after seeing the
evidence has spent the discipline it exists to protect.

## A1.0 — The mechanical question, answered from the text

> *Does the branch-condition text cite the zero, or only the expectation note?*

**Neither: the branch conditions cite a DIFFERENT zero, and it is true.** B1 and B2 both
condition on **"the zero coverage"** — the frozen prereg's *measured* `P3 = 0`
(`audits/2026-08-10_oq277_rq2_crosscoding/PREREGISTRATION.md:78`), which is verified and
unaffected by the correction. The false claim ("cited by index nowhere in its own defining
document") came from the **plan**, and this pre-registration **corrected it before the branches
were written** (§(ii), lines 74–86), stating: *"R1b must be argued on the corrected figure."*

The corrected figure of **1** then appears inside B2's own consequent — *"`:1015`, its one
cross-reference"* — so even the registered expectation argues **from** the 1, not from a zero.
Its claim was that the single citation is *located inside* the positive-control section, which is
an argument only available if one exists.

**So the branch is not void on this ground, and the pre-registration discipline is intact.**

## A1.1 — But the branch IS defective, on the operator's second ground — and worse than diagnosed

The operator's charge: B2's condition imports a **generality criterion** ("its tell is a
*language-specific instance*") that appears nowhere in `build_discipline.md`, so applying it
would be inventing the discriminator at ruling time.

**Correct — and it is not confined to B2. B1 carries the same criterion**, inverted: *"its tell
is distinct at the taxonomy's altitude, not only at the instrument's."* Both branches are built
on an unregistered discriminator, so the defect is the **whole R1b table**, not one row.

**Two checks, run rather than assumed:**

**(a) No membership-generality criterion exists in the file.** Every `altitude`/`general` hit is
about *claim scope* (`:186` "control the claim at the altitude it's made") or *where an instance
sits* (`:855` value/channel/aggregation altitudes) — never about whether a mechanism qualifies as
a member. `:4` runs the other way: *"This is not general [software advice]."*

**(b) A PUBLISHED MEMBER IS ALREADY LANGUAGE-SPECIFIC IN ITS TELLS — this is decisive.**
`Pattern 5`'s canonical tells at `:735–737` are ``Count == 0``, ``Supp =< 0.05``, and
``forall(P, Q)`` *"vacuously true when `P`'s table is empty"* — Prolog. `Pattern 6`'s three
instances include `grep -v Warning` — shell.

**So generality is demonstrably NOT a membership criterion in practice.** Registering it now
would require justifying why it disqualifies bound-probe while leaving Pattern 5 — a much
heavier lift than the original expectation implied, and one nobody has attempted.

## A1.2 — R1b′ (re-registered)

**Stated criterion only**, `:769–772` — *distinct tell + distinct corrective ⇒ distinct member*:

| if | then |
|---|---|
| **B1′** — bound-probe has a distinct tell and a distinct corrective | **peer member**, index per R2 |
| **B2′** — some published member's corrective subsumes it | **sub-instance**, no index |
| **B3′** — it names a discipline, not a defect shape | **demote** |

**Recorded, not registered as an expectation — the evidence for B1′ was already in the record
before this amendment and stating a preference now would be the thing this amendment exists to
prevent:** §R1b's own evidence list says its corrective *"is genuinely distinct from the
positive-control discipline, so it is **not subsumed**."* Under the stated criterion, with
generality removed and no ground left for B2′, **B1′ is the only branch with evidence behind
it.** The operator's framing — *"either bound-probe is a peer member under the stated criterion,
or you state and register the generality criterion first and re-derive"* — is therefore a real
fork only if someone takes the second path.

**The second path stays open and is specified**, so declining it is a choice rather than an
oversight: a generality criterion must be **named, written into `build_discipline.md` as a
membership rule, and reconciled with Pattern 5's Prolog tells** before it can bear on R1b. That
is a change to the taxonomy's constitution, not a ruling within it.

**Unchanged from the original registration, and it still binds:** *"one claimant is left, so it
wins by default"* is explicitly NOT a branch condition. Sole occupancy is not membership
evidence.

## A1.3 — R2's ground, corrected and PRICED

The frozen prereg is point-in-time (see the note at R2 above), so it is repaired by the label
set like every other point-in-time artifact. **R2's real ground is the cost asymmetry**, priced
at HEAD `6e7df53f`:

| | **C1** — index 4 = `recap-as-witness` (fabricated-default moves) | **C2** — index 4 = `fabricated-default` (recap-as-witness moves) |
|---|---|---|
| `build_discipline.md` internal cross-refs | **9 break** (`:769,771,772,890,901,1102,1759,1902,2112`) | 0 |
| `build_discipline.md` heading | 1 (`:686`) | 0 |
| `build_discipline.md` spine table row | 1 (`:2599`) | 0 |
| `CLAUDE.md` numbered entry | 0 | 1 |
| paper — current version amended | 1 (`v0_6.md`) | 1 |
| paper — 5 earlier versions | point-in-time, not edited | point-in-time, not edited |
| frozen prereg | point-in-time, label-set repair | point-in-time, label-set repair |
| **live editable sites** | **11** | **2** |

**The asymmetry is ~5:1 against moving fabricated-default off index 4**, and it is concentrated
in a single file, which makes it a one-change edit rather than a sweep. Against that, C1's only
remaining argument is that `CLAUDE.md` is the always-loaded file and the paper published its
list six times.

**No expectation registered for R2.** It branches on R1b′, which is unruled, and the priced
table is exactly the input a ruling should be made against — registering a preference beside the
price would put a thumb on it.

## A1.4 — R1a: RULED

**Operator, 2026-08-14: A1 — `fabricated-default` is a PEER MEMBER.** It passes the file's own
stated criterion at `:769–772` (distinct tell, distinct corrective), and the corrected evidence
strengthens rather than disturbs the case: 9 internal cross-references (not 7), mechanism text,
the dated OQ-33 exemplar, a tripwire diagnostic, `:901`'s axis position, and Pattern 6 composing
*"the Pattern-4 treatment"* as a primitive — the inverse of subsumption.

**Consequence:** R2 is live (two members need addresses) and blocked on R1b′.

## A1.5 — Status after this amendment

| ruling | state |
|---|---|
| **R1a** | **RULED — A1, peer member** |
| **R1b′** | re-registered, **UNRULED** |
| **R2** | live, ground corrected and priced, **UNRULED** — blocked on R1b′ |
| **R4** | falls out, waits |

Steps 4 and 5 remain blocked. Nothing in this amendment is a repair.

---

# AMENDMENT 2 — 2026-08-14 (R1b′ ruled; the zero struck as inadmissible)

## A2.1 — THE ZERO IS STRUCK, and my evidence bullet asserting it was FALSE

The operator asked whether the power floor was cleared before the frozen prereg's `P3 = 0` does
any work. **The answer is that the question never arises, and the figure is barred on stronger
grounds than being underpowered.**

**(a) The pass that the power floor governs never ran.** The power block belongs to the
fork-residue row, which was retired with OQ-277's cross-coding (`ISSUES.md:11217`: *"the honest
RQ2 result now that the cross-coding is retired"*). The `P3 = 0` I cited is not from that pass at
all — it is **(iii′) unit coverage**.

**(b) That figure is PRE-REGISTERED AS INADMISSIBLE, by name, four lines below itself.**
`audits/2026-08-10_oq277_rq2_crosscoding/PREREGISTRATION.md:78–84`:

> **(iii′) coverage: … P3 = 0.** Pre-registered before any number exists: **a pattern with zero
> members contributes NOTHING to the row.** Not read as agreement, not counted in the
> denominator, not reported as "no disagreement observed." **P3's row entry is `no members —
> uncalibrated`.** … whole-row agreement is reportable, **per-pattern agreement is not**, and any
> sentence reading a per-pattern (iii′) figure as a finding is a [pre-registered error]

and `:244` row 6 lists it in the freeze's explicit table of pre-registered errors: *"(iii′) at
n=10 with P3 uncalibrated — calibration data, not a verdict; per-pattern figures are a
pre-registered error."*

**(c) So my R1b evidence bullet — "zero measured coverage … WITH NO ACCOUNT" — was false.** It
has an account, and the account is written in the artifact I cited: `no members — uncalibrated`.
I read a per-pattern (iii′) figure as a finding, which that document names as an error in
advance.

**The shape of my mistake, recorded because it is a live instance of a declared residual.** I
cited a *frozen, gate-enforced* artifact — and the freeze guarantees the text is unaltered, not
that my reading of it is apt. `claim_cite_check.py` declares exactly this gap: *"THIS CHECKER
VERIFIES THAT A PIN MATCHES ITS ROW. IT CANNOT VERIFY THAT THE ROW IS THE RIGHT ONE TO CITE AT
THAT SITE … The mechanical relation is guarded; the semantic one is not."* The freeze's
authority made the figure feel load-bearing; the same document four lines down forbade the
reading.

**The zero is struck from every consequent in R1b′.** It is not evidence for or against
membership.

## A2.2 — R1b′ RULED: B1′, PEER MEMBER

The criterion is conjunctive — distinct tell **AND** distinct corrective — and the operator is
right that only the corrective half had been verified. The tell half, run now.

**The file answers it itself, in the spine table, which gives them SEPARATE ROWS with different
holes, different tokens and different read sites** (`build_discipline.md:2599`, `:2602`):

```
| 3 | Bound-probe bypasses cut | the lock clause never dispatched | a solution came back → "it's in the class" | the `findall` result/count |
| — | (diagnostic layer)      | the probe didn't actually look   | a clean/empty result → "nothing there"     | the analyst reading the result |
```

**The success-shaped tokens are of OPPOSITE POLARITY**, which is the decisive fact:

| | bound-probe | positive-control discipline |
|---|---|---|
| token | **a solution came back** — an over-count (432 against a true 404) | **a clean/empty result** — an under-read |
| failure direction | false POSITIVE membership | false NEGATIVE / unexamined absence |
| tell is visible | **statically**, in the source: *"any `findall`/`forall` over a cut-ordered predicate with the selecting argument bound is suspect"* (`:648`) | **only dynamically**: *"a clean read is byte-identical to a read that didn't look"* (`:1007`) — you cannot tell from the output; you must run a known-positive |
| read site | the `findall` result/count | the analyst reading the result |
| corrective (already verified) | query unbound + post-filter (`:645`) | pair the probe with a case it must flag |

A defect you can find by *reading the call site* and a defect you can find *only by running a
control* are not the same tell at any altitude.

**The one contrary datum, weighed rather than ignored.** `:1015` files the bound probe as one of
"four instances, all the same shape" inside the positive-control section. But that section states
its shape as the **spine** — *"absence and 'looked and found absence' collapse to the same token
at the read site, exactly as in the five patterns."* The spine is shared by **every** member by
construction; if co-membership in it decided the question, all six would collapse into it. It
cannot discriminate, so it does not.

**Ruling: distinct tell + distinct corrective ⇒ distinct member. R1b′ = B1′ — `bound-probe` is a
PEER MEMBER.**

**Inadmissible and not used:** the 1-vs-9 citation asymmetry (evidence about how heavily a member
is *cited*, not whether it *is* one — and a lagging measure of the very ambiguity this OQ
exists to fix); the struck zero (A2.1); generality (AMENDMENT 1 §A1.1).

## A2.3 — INDEX = 7. Index 3 stays vacant in both documents.

Operator, 2026-08-14. `CLAUDE.md:506` — *"Index deliberately left EMPTY; do not reuse or
renumber"* — is a standing instruction on the always-loaded file, and the demote ruling that
vacated 3 is a fortnight old. Refilling 3 with bound-probe would countermand it. **Bound-probe
takes a fresh index 7; 3 stays vacant in both documents.**

## A2.4 — R2's ground: READ-SITE WEIGHT, not the edit count

Operator correction: **the 5:1 is an edit count, and edit counts are not the right weight.** Nine
of the eleven sites are internal to `build_discipline.md`, a detail doc read on demand; the two
on the other side include `CLAUDE.md`, which every instance reads on load.

> Moving recap-as-witness is two edits but changes what every future instance reads first;
> keeping fabricated-default at 4 is nine edits in one file, in one change, mechanically
> checkable by the new checker.

**The asymmetry runs in opposite directions on the two measures**, and only read-site weight is
about what readers actually encounter. **Operator lean: C2 — `fabricated-default` keeps index 4**,
on read-site weight. Recorded as a lean; **R2 is not formally ruled here.**

Consequent set if C2 lands with R1b′ = B1′:

```
1 produced-but-not-consumed   2 one-canonical-thing-became-two   3 — VACANT (never reused)
4 fabricated-default          5 absence-satisfies-the-gate       6 success-shaped-absorption
7 bound-probe-bypasses-clause-order                              8 recap-as-witness-substitution
```

`recap-as-witness` moving to 8 is the two-edit side, and it is the side that touches the
always-loaded file — which is exactly why the ground is read-site weight and not the count.

## A2.5 — Status

| ruling | state |
|---|---|
| **R1a** | **RULED — A1, peer member** |
| **R1b′** | **RULED — B1′, peer member; index 7** |
| **R2** | ground corrected to read-site weight; operator lean **C2**; **not formally ruled** |
| **R4** | falls out of R1a + R1b′ — the paper gains two members and a vacant 3; applied at Step 5 |

Steps 4 and 5 remain blocked on R2. Nothing here is a repair.

---

# AMENDMENT 3 — 2026-08-17 (R2 RULED; R4 applied; the entry closes)

*Appended, never rewritten. Numbered 3 because AMENDMENT 2 already exists — the execution plan
called this one "AMENDMENT 2", which was written against the pre-`c06bcb26` state of this file.
Recorded rather than silently renumbered.*

## A3.1 — R2 RULED: C2

**`fabricated-default` keeps index 4; `recap-as-witness` renumbers to 8.** The operator lean in
A2.4 is now the ruling, on the ground A2.4 corrected it to.

**GROUND: read-site weight.** Nine of the eleven publishing sites are internal to
`build_discipline.md`, read on demand; the two on the other side include `CLAUDE.md`, read by
every instance on load. **The 5:1 edit-count measure is explicitly NOT the ground** — it was
corrected away in A1.3/A2.4 and reinstating it here would undo that correction.

**Two corroborations, filed as corroboration and NOT as criteria.** Recording either as a
membership criterion would repeat the R1b defect diagnosed in A1.1 — inventing the discriminator
at ruling time:

1. `fabricated-default` is the member with worked mechanism text and `recap-as-witness` has none
   anywhere, so C2 does not put the specified member at the higher index.
2. C2 makes `audits/2026-06-10_oq93_grid_viability_probe/FINDINGS.md:23` **retroactively correct**
   rather than a repair target — and on execution the whole §4.4 wrong-label class emptied the
   same way, all four rows re-adjudicated in context.

Neither is a membership criterion written down in `build_discipline.md`.

## A3.2 — SYMMETRY: unified indices, asymmetric depth

Every member occupies the same index in **both** documents; a document may carry a member as a
**pointer entry** rather than a worked section. Index-only symmetry was rejected because its cost
is measured, not hypothetical: under index-only, C2 leaves `CLAUDE.md` with index 3 vacant *and*
index 4 occupied only in the detail doc — which is the configuration that produced the 2026-08-17
bound-selector incident, where the rule was published in one document, named-but-not-taught in the
other, and an executor ran the exact query the worked example warns about.

**Binding condition on every pointer entry: state the TELL, not the member.** A pointer reading
`4 — fabricated default (see build_discipline.md:686)` degrades to index-only with extra steps —
precisely what the pre-repair "bound-probe" mention was, and it was indistinguishable from absence
at the read site. The form generalised is the interim BD-P3 repair: state the shape, why it
misfires, the rule, and the asymmetry that says when you need not re-run.

## A3.3 — R4 APPLIED

`amnesiac_institution_v0_6.md` §5.1 amended to the 7-member / 8-slot set with two new dated
instances; §5.2's declination of index 4 replaced by new **§5.2.1**, which names this audit
directory and states the delay's cost as measured. Receiver check re-run immediately before the
edit (no reachable agents; the sole pin into the paper is below the amended region; every hunk at
or below line 886 is line-count-preserving) rather than relied on from the plan.

## A3.4 — What the execution added that was NOT pre-registered

Stated because unregistered additions are exactly what a pre-registration exists to expose:

1. **A commit 0.** Executing R1b′ strands the citations that resolved *correctly* to `BD-P3` — a
   second stale class, created by this ruling rather than by the 2026-08-11 one. Its population was
   declared **before** the move, because mechanism recovery is what distinguishes those citations
   and it gets harder once index 3 resolves to `bound-probe` in neither document. Not renumbering
   without sweeping is the whole content of it: the vacating did that and it is this entry's third
   self-inflicted instance.
2. **Both members relocated in `CLAUDE.md`, not just renumbered in place.** The priced footprint
   said "the `:598` block gains index 7"; leaving it after the spine paragraph would have put
   members 7 and 8 *after* the summary that generalises over them. They were moved to sit after
   member 6 so the list reads 1–8 contiguously.
3. **Two checker controls re-forced through a synthetic manifest.** With `DECLARED_COLLISIONS` and
   `DECLARED_SPINE_LAG` retired to empty, the `UNDECLARED RESOLUTION` shapes can no longer be
   forced by mutating a document. Dropping them would have retired two controls for a reason
   unrelated to whether their code paths work.
4. **The sweep's second positive control re-anchored.** It was pinned to `design_gaps.md` — a site
   on its own instrument's repair list — and went red the day the repair landed. All three positives
   now sit on artifacts nothing is licensed to edit, one per recoverable mechanism.
5. **The census reconciled by row identity and driven to a fixed point.** Registered as a check;
   what it found was not registered — the published 750 was stale *at its own commit* (`fd73ec9e`
   held 755), not by later drift.

## A3.5 — Status

| ruling | state |
|---|---|
| **R1a** | RULED — A1, peer member (AMENDMENT 1) |
| **R1b′** | RULED — B1′, peer member, index 7 (AMENDMENT 2); **executed 2026-08-17** |
| **R2** | **RULED — C2, on read-site weight** |
| **R4** | **APPLIED** |

Steps 4 and 5 are executed. OQ-278 closes; OQ-294 and OQ-287 are unblocked.
