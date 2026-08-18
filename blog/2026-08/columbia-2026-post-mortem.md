# Colombia 2026: Scoring the Falsification Matrix

*Post-mortem of "Colombia 2026 Election: Falsification Matrix" (registered 2026-01-26,
building on "A Constraint Story: Why Colombia's Election Defies Standard Forecasting,"
2026-01-17). Scored 2026-08-18 against the matrix's own pre-registered thresholds.
Mechanical resolution first; interpretation separated and marked.*

---

## Resolution data

First round, May 31, 2026 (turnout 57.89%):

| Candidate | % |
|---|---|
| Abelardo de la Espriella (ind.) | 43.75 |
| Iván Cepeda (Historic Pact) | 40.90 |
| Paloma Valencia (Democratic Centre) | 6.92 |
| Sergio Fajardo (Dignity & Commitment) | 4.26 |
| Claudia López (ind.) | 0.95 |
| All others + blank | 3.22 |

Runoff, June 21, 2026 (turnout 63.60%): De la Espriella 49.66%, Cepeda 48.70% —
margin 0.96 points, ~252,000 votes of ~26 million.

March 8 Gran Consulta: Valencia 3,236,286 votes; Oviedo 1,255,510 (second);
primary participation therefore ≥4.49M.

Sources: Registraduría results as compiled by Wikipedia
([2026 Colombian presidential election](https://en.wikipedia.org/wiki/2026_Colombian_presidential_election));
AS/COA runoff analysis.

## Row-level scoring (the matrix's own thresholds)

| # | Claim | Own threshold | Observed | Verdict |
|---|---|---|---|---|
| 1 | Noose: bipolar compression of undecideds | fail if undecided >12% by May 15 | 7.7% by mid-Feb; ~0 at vote | **CONFIRMED** |
| 2 | Institutional collapse on the right | fail if primary winner >26% nationally; fail if primary turnout >4M | Valencia 6.92% (no fire); turnout ≥4.49M (**fired**) | **CONFIRMED, one misattributed fire** (see below) |
| 3 | Centrist strangulation (Fajardo terminal ~9–10%) | fail if Fajardo >15% (Feb) / >13% (Apr); non-polar combined >22% (May) | Fajardo 4.26%; non-top-two 15.35% | **CONFIRMED** — overshot its own prediction by 2× |
| 4 | Left Rope bounded at ~35% | fail if Cepeda >40%; confirm if never >38% | 40.90% first round; 48.70% runoff | **FALSIFIED** at its own line |
| 5 | Unstable bipolar attractor | confirm: top two within 1–4 pts (May 31); fail if runoff winner >55% | 2.85 pts; winner 49.66% | **CONFIRMED**; Feb red flag stands (De la Espriella 32.1% vs fail-line 28%) |

Cumulative, per the matrix's own scheme: **2 red flags plus one ambiguous fire →
"theory needs revision"** — neither robust nor fundamentally wrong.

For context on claim 5: late-January prediction markets implied a 14.5-point
first-round separation (46 / 31.5). The matrix said 1–4 points. Actual: 2.85.
The runoff prose prediction ("50–52% winner facing deeply hostile opposition")
resolved at 49.66% — within rounding of the band, exactly the substance.

## Mechanism vs. magnitude (the uke_write v2.2 split, applied retroactively)

**Mechanism questions: ~5/5.** Every load-bearing joint the structural read named was
the right joint — the runoff system as compressor, institutional-right collapse,
center strangulation, the bipolar terminal state, a sub-majority-legitimacy winner.

**Magnitude/rate questions: ~1/4.** De la Espriella's consolidation rate (predicted
16–20% in February; observed 32.1%), Cepeda's ceiling (35 → 40.9), Fajardo's floor
(9–10 → 4.26), Valencia's level (<3 → 6.92).

## Three extractions the tally alone does not show

### 1. The falsified claim was killed by the confirmed ones *(INFERRED — marked)*

The rows were scored independently, but the claims were not independent. The ~35%
ceiling on Cepeda binds only if non-left anti-Espriella voters have somewhere else to
park. Claims 1 and 3 assert they do not — and both confirmed *more severely than
modeled* (Fajardo at 4.26% against a predicted 9–10% terminal). Full compression
transfers the strangled middle's mass to the poles; the Noose's over-performance
entails the Rope-ceiling's failure. The runoff is consistent: of ~5.6M votes added
between rounds, Cepeda took roughly 54% and closed the gap from 2.85 to 0.96.

The alternative reading — the ceiling was simply a bad prior on left base size —
remains live. The discriminating datum would be where Cepeda's overage came from
(geographic/demographic vote-flow), unresolved here; hence INFERRED, not ruled.

If the coupling reading holds, the transferable rule is typed, not Colombian:

> **A capacity ceiling claimed on a pole is conditional on incomplete compression.
> Never register a static ceiling alongside an active compression mechanism without
> an interaction row.**

### 2. A threshold can resolve TRUE while its pre-written interpretation is false

The claim-2 line "primary turnout >4M = right-wing base energized; institutional Rope
still functional" fired (≥4.49M) — and its meaning-sentence was wrong. Valencia took
3.24M primary votes and then 1.64M general-election votes: the institutional vehicle's
*own primary electorate* defected to the scaffold candidate. The trigger measured
energy; the interpretation asserted where the energy would flow.

Protocol lesson: pre-registered interpretations are themselves claims and need their
own falsification route. The scoring rubric needs a sibling to UNRESOLVABLE:
**RESOLVED-BUT-MISATTRIBUTED**. Related: claim 3's thresholds were one-sided, so a 2×
magnitude miss in the *confirming* direction scored as a clean hit — falsification
gates should be two-sided wherever the theory also implies a floor.

### 3. The error signature is coherent — and it is not fragility bias

All four magnitude misses point the same way: **the named mechanism operating more
strongly than priced.** Compression harder, re-coordination faster, pole absorption
bigger. The structural read identifies joints correctly and then prices them timidly.
This is a pre-registerable hypothesis for the next matrix — and it runs contrary to
the fragility-bias hypothesis pre-registered elsewhere (ISSUES OQ-229). One case;
n=1; logged as a hypothesis, not a finding.

## The February revision, honored

The matrix's own mid-course note (Feb 16) already extracted the fifth lesson: the
essay "underspecified the rate of right-wing re-coordination around non-institutional
nodes... De la Espriella's movement built a new Scaffold that rapidly hardened into a
functional Rope." Typed: **under institutional vacuum plus active compression,
replacement coordination does not proceed at institutional timescales — the
compression energy drives scaffold-hardening.** The France 2027 essay is already
applying this (Bardella as structural vacancy), which makes it the lesson's first
transfer test.

## Verdict

Needs revision, per its own scheme — and the revisions are identifiable and typed:
(1) ceilings are conditional on compression completeness (composition, not
independence); (2) re-coordination rate under vacuum is fast, not institutional;
(3) the register format needs interaction rows, two-sided gates, and separately
scoreable interpretations. The mechanism layer of the read survived contact with
reality outright, against a market baseline that had the shape badly wrong.

**Fired: live** — one claim flipped at its own threshold, one control fired with a
false interpretation attached, and a pre-registered hypothesis took a hit.
