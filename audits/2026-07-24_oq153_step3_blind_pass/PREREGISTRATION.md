# OQ-153 step 3 — blind authoring pass: PRE-REGISTRATION (sealed before the pass)

Sealed 2026-07-24, before any text is handed to the subagent. Scores the Kill A/B/C verdicts
against a split, predictions, and selection procedure fixed in advance. Primary source leg:
`testsets_sonnet`. Field: `update_authority ∈ {licensed_revisable, frozen, absent_diffuse}` (+ the
authoring-record token `unauthored`, which is NOT an engine value and NOT `absent_diffuse`).

## The sample (15 items, 15 distinct kernels, all sonnet)

Columns: stratum (in = `naturalized∧forecloses`), amending institution, **institution basis**
(TEXT = read off the constraint text / EXT = supplied from world knowledge), non-exemplar (does the
rubric NOT pre-decide it), my **predicted** value, predictable-from-name.

| # | Str | Kernel | Amending institution | Inst basis | Non-exmp | Prediction | Pred? |
|---|---|---|---|---|---|---|---|
| 1 | in | `commerce_clause_scope` | US Constitution (Art. V) | EXT | no | licensed_revisable | ✓ |
| 2 | out | `equal_protection_clause` | US Constitution (Art. V) | EXT | no | licensed_revisable | ✓ |
| 3 | in | `bitcoin_consensus_kernel` | Bitcoin (BIP + node adoption) | TEXT | yes | absent_diffuse? | ? |
| 4 | out | `bitcoin_whitepaper_purpose` | Bitcoin whitepaper (fixed doc) | TEXT | yes | frozen | ✓ |
| 5 | in | `article_9_war_renunciation` | Japanese Constitution (Art. 96) | EXT | no | licensed_revisable | ✓ |
| 6 | in | `biblical_authority` | Church magisterium | TEXT | no | licensed_revisable? | ? |
| 7 | out | `biblical_source_text` | Academic textual criticism | EXT | — | absent_diffuse? | ? |
| 8 | in | `balfour_mandate_instruments` | Historical mandate (superseded) | EXT | yes | frozen/unauthored? | ? |
| 9 | in | `salic_prohibition` | Salic succession law | TEXT | yes | frozen | ✓ (selected-for) |
| 10 | in | `eternal_marriage_covenant` | Marriage-covenant doctrine | TEXT | yes | frozen | ✓ (selected-for) |
| 11 | out | `church_turing_thesis` | Math/CS (thesis, no owner) | TEXT | yes | absent_diffuse | ? |
| 12 | out | `notability_guidelines` | Wikipedia community (RfC) | TEXT | yes | licensed_revisable/absent_diffuse? | ? |
| 13 | out | `legitimacy_of_practice_standardization` | Practice-standardization body | TEXT | yes | absent_diffuse? | ? |
| 14 | out | `refugee_convention_text` | 1951 Convention (protocol) | EXT | — | licensed_revisable | ✓ |
| 15 | out | `licensing_statute_mandate` | Legislature/statute | TEXT | no | licensed_revisable | ✓ |

7 in / 8 out. ~13 distinct amending institutions (US Constitution and Bitcoin repeat as deliberate
cross-stratum pairs). **Predictable ≈ 6/15; ≈ 9 carry information.**

## Kill A — reframed (honest about selection)

**Items 9 and 10 were drawn BECAUSE I predict them `frozen`** — selection on the outcome variable
(the de-exemplarization pass had cut the closed-canon `frozen` supply, so unselected the sample would
have fired Kill A on `frozen` for construction reasons, routing to the option-2 abort the operator
overruled). Therefore Kill A can **no longer** be read as "the corpus varies in authority structure."
Reframed question: **is the field assignable across its full range when the range is present?** The
corpus was never a random sample of institutions; this is the question that matters.

- Rule (tightened): each enum value must appear on **≥2 DISTINCT amending institutions**, not ≥2 rows.
- Expected supply: `frozen` on ≥3 (Bitcoin whitepaper, Salic, marriage covenant, maybe Balfour);
  `licensed_revisable` on ≥3 (US Const, Japan Const, treaty, legislature); `absent_diffuse` on ≥2
  (math/CS, Wikipedia, academic criticism, practice standardization).

## Kill B — the rubric-recall caveat and the Q0/Q1 clause

Even de-exemplarized, items 1/2/5/14/15 sit near rubric examples and will read cleanly; the
**information is in the ~9 uncertain items.** Kill B asks whether a call is justified from its quote
on institutional grounds alone.

**Q0/Q1 clause (pre-registered):** a Q0-vs-Q1 confusion visible in the reason texts is a **rubric
defect, not a Kill B failure.** Worked example: `church_turing_thesis` HAS an identifiable kernel (the
thesis) with no owner → Q1 → `absent_diffuse`; mis-routing it to Q0 → `unauthored` is a boundary the
rubric under-specifies (I wobbled it myself). If a Q0/Q1 confusion appears on **>1 item**, the fix is
**amending Q0**, not aborting Leg B.

## Kill C — answerable from the primary 15 (free), replicates are a separate test

Two same-institution cross-stratum pairs sit in the sample:
- **US Constitution (Art. V):** #1 `commerce_clause_scope` (in) vs #2 `equal_protection_clause` (out).
- **Bitcoin:** #3 `bitcoin_consensus_kernel` (in) vs #4 `bitcoin_whitepaper_purpose` (out).

If a pair returns the **same** value across the stratum split, `frozen ⟺ naturalized∧forecloses` is
falsified by construction — Kill C is answered without the replicate set.

**Bitcoin caveat (pre-registered):** a frozen whitepaper vs BIP-revisable consensus rules is a
**genuine** difference in amending authority. A split on the Bitcoin pair is a real discrimination,
**NOT** inconsistency, and must not be scored as a Kill-C failure.

## The institution mapping is a second unblinded judgment

The "amending institution" column is assigned by me (basis marked TEXT/EXT above) and is load-bearing
for the ≥2-institutions rule. **Watch item:** the biblical pair (#6 Church magisterium / #7 Academic
textual criticism) — same referent-family, two institutions assigned; that split is what keeps them
out of the collision count. If they are one institution, `licensed_revisable`/`absent_diffuse` each
lose one institution of support.

## Selection queries (verbatim — the selection procedure is part of the record)

**Stratum** (in ⟺ both hold), computed on each item's sonnet text:
```prolog
nat_seat(C)     :- stakeholder_seats:dr_type_for_stakeholder(C, _, naturalized).
forecloses_c(C) :- narrative_ontology:cs_story_uid(C,UID),
                   narrative_ontology:cs_reading_relation(UID,_,forecloses).
in_stratum(C)   :- nat_seat(C), forecloses_c(C).
```
**Non-exemplar register grep** (surfaced #11/12/13 and confirmed #3):
```
grep -iE "procurement|notability|architectural|bureaucrat|standard|guideline|protocol|
convention|governance|benchmark|de_facto|inertia|regulat|zoning|accounting|editorial|
peer_review|licensing|certif"   over the sonnet in/out pools, excluding _flat_control/_contradictions/_c0
```
**Frozen register grep** (surfaced #9/#10 — selection-on-outcome, see Kill A):
```
ls testsets_sonnet | grep -iE "ossif|closed|final|perpetu|unalterab|entrench|superseded|
defunct|immutab|sealed|canon|fixed|frozen|sola_scriptura|last_word|settled|dead_letter|abrogat"
```

## Other pre-registered notes

- **Test–retest asymmetry:** replicate/pair **disagreement is dispositive** (the field fails if one
  kernel/institution gets two values from two coherent texts); **agreement is weak** evidence (the
  subagent may recognize the institution across texts — recognition, not reading). Cannot be
  prevented; stated rather than claimed away.
- **3-readings/kernel is a template, not a fact:** sonnet 1001 CIDs / 331 kernels / 1 standalone ≈ 3
  readings per kernel by generation design; "all kernels multi-reading" carries no corpus information.
- **Model-dependence:** in-stratum rate (husk conditions 3+4) runs 11.6% (testsets) → 34% (kimi) by
  authoring model alone — recorded in OQ-153 as a data point for OQ-228; any five-condition husk rate
  is substantially model-dependent.

## Blindness protocol

The subagent receives: the 15 constraint **texts** (authored situation/narrative) in arbitrary order
+ the rubric's **decision procedure, values, boundary cases, and evidence rule ONLY** (no intro/
rationale, no domain labels, no stratum, no institution map, no Kill flags, no OQ/plan context). It
returns per item: value (or `unauthored` + reason) + one authority-text quote. **The exact prompt
handed over is recorded** as the blindness witness. Kill A/B/C verdicts are escalated to the operator
UNRULED.

## Replicate set (~5) — sealed as an addendum AFTER this file, BEFORE the pass

Cross-leg same-kernel opposite-stratum authorings (e.g. `constitutional_text_authority`: out in
sonnet, in in testsets), pulled from the other leg's text, shuffled in unmarked. Tests same-kernel /
different-model reliability (distinct from the primary-15 Kill C). To be listed in
`PREREGISTRATION_replicates.md`.

---

## ADDENDUM 1 — appended 2026-07-24, AFTER the seal (not a rewrite)

Primary pre-registration above sealed at commit **`21496a1c`** (git history is rewritable; this hash
pins the sealed object). This addendum corrects one selection flaw and hardens the pass.

### A1.1 Frozen-swap: immutability-claim → institutional-superseded (collinearity fix)

Items 9 (`salic_prohibition__immutable_mandate_reading`) and 10
(`eternal_marriage_covenant__immutable_commandment_reading`) are **REMOVED**. They were selected by
immutability *language*, and **a reading asserting immutability IS a naturalization claim** — so
`frozen` sourced that way is collinear with the naturalized signature in substance while Kill C passes
in form: condition 5 (frozen authority) would become a restatement of condition 3 (naturalization),
the exact laundering the leg exists to prevent, entering underneath the falsifier. It is also
reading-the-reading: a doctrine's immutability claim is one party's claim, not the institution's
authority structure (an eternal-covenant doctrine can be held by a church that demonstrably amends).

**Replaced with institutional-`frozen`** (superseded instrument, no successor amending body — `frozen`
for institutional-structure reasons, not asserted ones), both in-stratum, 7/8 preserved:
- 9′ `bretton_woods_treaty_substrate__neoliberal_convertibility` — Bretton Woods system (superseded 1971).
- 10′ `versailles_reparations_clauses__punitive_liability_reading` — Treaty of Versailles (defunct/repudiated).

### A1.2 Supply-only items — excluded from the Kill B denominator

Items 9′ and 10′ are **SELECTED-FOR-`frozen`** (on superseded structure). Mark them **supply-only**:
they exist to populate the value, and cannot test assignability when the answer (superseded / no
successor body) is in the text. **Exclude 9′ and 10′ from the Kill B denominator.** (Item 8
`balfour_mandate_instruments` was already in the sample and its `frozen` prediction is uncertain — it
is NOT supply-only.)

### A1.3 Remaining Kill A exposure (stated honestly)

- `frozen` is now **protected by construction** (9′, 10′ selected-for + `bitcoin_whitepaper` out).
- `licensed_revisable` is well supplied (US Const ×2, Japan Const, refugee convention, legislature).
- **`absent_diffuse` rests ENTIRELY on unpredicted calls** (`church_turing`, `notability`,
  `biblical_source_text`, `legitimacy_of_practice_standardization`, `bitcoin_consensus`) with
  **nothing selected for it.** If Kill A fires on `absent_diffuse`, that is the **honest** version and
  reads differently from the `frozen` case, which is now unfireable by design.

### A1.4 Subagent input hardening

- **Strip CIDs and reading labels from the handed texts** — a filename/label like
  `immutable_mandate_reading` hands over the answer. The subagent sees only the authored
  situation/narrative prose.
- **Record per item whether the text contains an explicit unalterability phrase** ("shall not be
  altered", "immutable", "eternal", "closed", "final") — the covariate that separates "the field is
  assignable off institutional structure" from "the text said so."

### A1.5 Final 15 (superseding the table's items 9/10)

in: `commerce_clause_scope`, `bitcoin_consensus_kernel`, `article_9_war_renunciation`,
`biblical_authority`, `balfour_mandate_instruments`, `bretton_woods_treaty_substrate__neoliberal_convertibility`,
`versailles_reparations_clauses__punitive_liability_reading`.
out: `equal_protection_clause`, `bitcoin_whitepaper_purpose`, `biblical_source_text`,
`church_turing_thesis`, `notability_guidelines`, `legitimacy_of_practice_standardization`,
`refugee_convention_text`, `licensing_statute_mandate`.

---

## ADDENDUM 2 — replicate set (sealed 2026-07-24, before the pass)

5 cross-leg same-kernel opposite-stratum authorings, shuffled UNMARKED into the same pass. Tests
same-kernel / different-authoring-model reliability (test–retest); disagreement dispositive, agreement
weak (§ Other notes). A replicate returning a DIFFERENT value from its primary pairing ⇒ the field
reads prose, not institution ⇒ fails regardless of quote quality.

| R | Item (other leg) | Leg | Stratum(leg) | Pairs with primary (sonnet) | Institution |
|---|---|---|---|---|---|
| R1 | `commerce_clause_scope__broad_effects_test` | flash | out | #1 (in) — SAME reading | US Constitution |
| R2 | `biblical_authority__tradition_scripture_reading` | haiku | out | #6 (in) — SAME reading | Church magisterium |
| R3 | `bretton_woods_treaty_substrate__neoliberal_convertibility` | flash | out | #9′ (in) — SAME reading | Bretton Woods |
| R4 | `church_turing_thesis__physical_claim_reading` | haiku | in | #11 (out) — same kernel | Math/CS |
| R5 | `equal_protection_clause__remedial_reading` | kimi | in | #2 (out) — same kernel | US Constitution |

Pass = 20 items (15 primary + 5 replicate), 15 distinct kernels. Each replicate's value is expected
STABLE vs its primary (same institution); a split is the failure signal — EXCEPT where the Bitcoin
caveat's logic applies (a genuinely different sub-instrument), which does not arise in R1–R5.
