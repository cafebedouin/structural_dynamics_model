# Paris 2026: Scoring the Weakened-Snare Matrix

*Post-mortem of "Paris 2026 Municipal Election: Falsification Matrix" (registered
pre-election; repo copy `blog/2026-05_or_before/paris_2026_falsification_matrix.md`).
Scored 2026-08-18 against the matrix's own thresholds. This matrix carried explicit
scenario probabilities (60/30/10) — the only pre-v2.2 register that did.
Vocabulary: the source matrix uses "Noose" for the type the framework now calls
**snare**; this document uses snare throughout, including where source claims are
restated, per that mapping.*

---

## Resolution data

First round, March 15, 2026 (turnout 58.89%):

| List | % |
|---|---|
| Grégoire (PS-LE-PCF-PP…) | 37.98 |
| Dati (LR-MoDem-UDI-PR) | 25.46 |
| Chikirou (LFI-PG-POI) | 11.72 |
| Bournazel (HOR-RE…) | 11.34 |
| Knafo (Reconquête) | 10.40 |
| Mariani (RN…) | 1.61 |

Second round, March 22 (turnout 61.60%): Bournazel merged his list into Dati's;
Knafo withdrew. Grégoire 50.52%, Dati 41.52%, Chikirou 7.96%. **Seats (163):
Grégoire 103, Dati 51, Chikirou 9.** Grégoire elected mayor with a single-list
majority — no coalition partners required.

Source: [2026 Paris municipal election](https://en.wikipedia.org/wiki/2026_Paris_municipal_election).

## Row-level scoring

First round (Date-4 table):

| Metric | Predicted | Threshold | Observed | Verdict |
|---|---|---|---|---|
| Lists >10% | 4–5 | fail if ≤3 | **5** | ✅ CONFIRMED |
| Bournazel | 13–17% | fail if <11% | 11.34% | ⚠️ partial band (11–13) — neither predicted nor falsified |
| Top-two margin | 4–10 pts | fail if <2 | 12.52 | missed HIGH — gate only policed the low side |
| 3rd-place distance from 2nd | 6–8 pts | fail if >10 | 13.74 | 🔴 FIRED ("wasted-vote cliff") |
| Turnout | 45–52% | fail if >55% | 58.89% | 🔴 FIRED (polarization mobilization) |
| Geographic Gini | <.38 | fail if >.42 | never computed | UNRESOLVABLE |
| Far-right combined (Knafo+Mariani) | 12–16% | fail if >22% | 12.01% | ✅ |

Second round / seats (Date-5 table):

| Metric | Predicted | Threshold | Observed | Verdict |
|---|---|---|---|---|
| Winner's seats | 55–70 | fail if >73 | **103** | 🔴 FIRED decisively |
| Lists winning seats | 5–7 | fail if ≤4 | **3** | 🔴 FIRED |
| Winning margin | 50–53% | fail if >54% | 50.52% | ✅ |
| Coalition size | 3+ parties | fail if winner+1 ≥82 seats | winner alone: 103 | 🔴 FIRED |
| Bournazel voter split | <70/30 | fail if >75% to one pole | list formally merged into Dati's | 🔴 fired structurally (merger = 100% list alignment; individual flows untracked) |
| Post-election stability | complex negotiation | fail if majority within 48h | immediate majority | 🔴 |

Compound bands, by their own grammar: STRONG FALSIFICATION needs 3+ of its 6 named
conditions — 2 fired firm (seats >73; ≤4 lists seated), 2 resolved negative
(Bournazel ≥11; 5 lists >10%), 2 UNRESOLVABLE (compression velocity — no weekly
series was collected; Gini — never computed). PARTIAL needs 2 — only 1 held
(Bournazel 11–13). **Neither band is formally reached, because the outcome did
something the bands didn't anticipate: it split by level.**

Scenario probabilities: the 60% base case is falsified on its seat-side conjuncts
(coalition-required, 55–70 seats); the 10% null is falsified on its ballot-side
conjuncts (Bournazel <10, 3 lists). Reality took the base case's first round and the
null's second round.

## The finding: the snare relocated

**Ballot level — weakened-snare predictions confirmed.** Five lists over 10%
(Colombia: two poles at a combined 84.65%). Centrist at 11.34% (Colombia's
Fajardo: 4.26%). Far-right fragmented, not consolidated. The psychological
compression that a full snare exerts *before* round one demonstrably did not
operate: multi-polarity survived to the ballot. The Colombia–Paris paired test
discriminated, and in the predicted direction.

**Seat level — full-snare outcomes anyway.** A 37.98% first-round list took 103 of
163 seats (63.2%), three lists seated, zero coalition partners needed, majority
immediate. The mechanism: inter-round list mergers (Bournazel→Dati; Knafo
withdrawal) plus the 25% bonus computed on second-round shares reconstructs
hegemonic allocation downstream of a genuinely multi-polar first round.

Typed lesson (L3): **compression relocates across levels rather than dying.**
Weakening the psychological compression channel (pre-round-one wasted-vote fear)
moved the compression to mechanical channels (inter-round consolidation, seat
mathematics). A snare-breaking reform must be judged at the terminal allocation,
not the ballot. — Readers of the repo's build-discipline doc will recognize this as
the *relocating confound* pattern, appearing in the world rather than in a
verification harness: closing one channel relocates the mechanism; iterate until it
lands nowhere or the landing is declared.

## Hypothesis updates

**H1 (mechanism-hit / magnitude-timid) takes its first counterexample.** Claim 5 —
"reduced majority bonus prevents hegemonic control" — was a *mechanism* miss, flat
wrong: the matrix never modeled inter-round mergers or bonus-on-second-round math.
(Magnitude-timidity also recurs: Grégoire predicted 32–37, actual 37.98; turnout
predicted 45–52, actual 58.89 — the named dynamics again ran stronger than priced.)
H1 stands at n=2 mixed: Colombia purely magnitude-side, Paris containing one
genuine mechanism miss at the layer the theory hadn't modeled.

**H2 (type-transfer) gets its first positive datum.** The full-snare /
weakened-snare type distinction predicted a real, observed differential at the
ballot level across two countries eleven weeks apart. The typing was predictive —
and incomplete: "snare" needs a level index (psychological vs. mechanical), because
the reform converted one without converting the other.

## Protocol defects (feeding uke_write / uke_score)

1. **Compound bands assume coherent movement.** The STRONG/PARTIAL/CONFIRM bands
   mix ballot-level and seat-level conditions, so a level-split outcome reaches no
   band and the matrix's own verdict grammar goes silent exactly when the result is
   most informative. Bands should be stratified by level.
2. **Rows die without a collection executor.** Compression velocity was the
   matrix's own "core test" and Gini its polarization instrument; both are
   UNRESOLVABLE because the weekly series and arrondissement computation were
   specified but never assigned to anyone. A register row without a named collector
   is a wish. (v2.2's named-resolver rule addresses the resolution side; this is
   the collection side.)
3. **One-sided gates, again.** The top-two-margin gate policed hypercompression
   (<2) and was silent on the hegemonic direction (12.52) — the same defect as
   Colombia's Fajardo floor, opposite side.

## Verdict

By its own grammar: no band reached — superseded by a level-split reading that is
sharper than any band: **reform broke the snare in voters' heads and left it intact
in the seat mathematics.** The theory's typing discriminated (H2 positive); its
claim that the reform converts Snare to Rope is falsified at the governance level;
the register format needs level-stratified bands and named collectors.

**Fired: live** — four threshold rows fired, the 60%-confidence scenario lost its
seat-side conjuncts, one standing hypothesis (H1) took a counterexample, and one
typed lesson (L3) was minted.
