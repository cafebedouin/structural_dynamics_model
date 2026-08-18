# FORECAST.md — Forecasting Record and Lessons Ledger

**What this file is.** The accumulating record of forecasting exercises and what they
taught — the layer that individual post-mortems cannot provide, because error
signatures and type-level lessons exist only *across* cases. The method lives in
`agent/uke_write_v2.2.md` (§1.6 Forecast Register, §6.1 format) and
`agent/uke_score_v0.1.md`; this file holds performance, standing hypotheses, and
transferable lessons. Single tracking surface for forecasting outcomes — per-case
post-mortems go in `blog/YYYY-MM/`, one row lands here. Related open question:
ISSUES OQ-229 (essay-archive outcome scoring; Forecast Register instrument).

**Reader and decision.** The operator (or an instance), before *registering* the
next matrix: which hypotheses are live, which lessons apply, what the protocol's
known defects are. If a planned register contradicts a lesson below, either follow
the lesson or say why not — silently ignoring the ledger is how it stops earning
its place. When a register is written with this file in context, stamp it: one line
naming which lessons/hypotheses were applied (e.g. "written under H1 correction;
L1 interaction rows"), so scoring can attribute corrections rather than re-test an
already-corrected bias.

**⛔ SCORING INSTANCES: STOP.** This file is for the *writing* pass only. Scoring
is row-only by contract (`agent/uke_score_v0.1.md`; uke_write v2.2 §1.6) — a scorer
holding this ledger's standing hypotheses and expected error directions can drift
toward resolving ambiguous rows in hypothesis-confirming directions, and that
contamination cannot be undone by trying to ignore what you have read. If you have
read any part of this file and are now asked to score a register or matrix, do not
proceed and do not attempt to self-blind: **stop, tell the operator this instance
is ledger-contaminated for scoring, and ask them to start a fresh instance that is
given only the register rows and the scoring rubric.** Refusing here is the correct
completion of the task, and the refusal should be reported at the volume of a
completion. (Sequence: writer reads this file → scorer reads rows only → this file
is updated *after* scoring.)

---

## The record

| Exercise | Registered | Resolved | Scored | Headline |
|---|---|---|---|---|
| GJ Open 2019 (74 questions) | 2018–2019 | 2019–2020 | 2020-01-05 | Brier .23; accuracy −.05 vs crowd median |
| GJ Open 2020 (114 questions) | 2020 | 2020–2021 | 2021-03-15 | Brier .32; accuracy −.03 vs crowd median |
| Colombia 2026 election matrix (5 claims) | 2026-01-26 | 2026-06-21 | 2026-08-18 | "Needs revision" per own scheme; mechanism ~5/5, magnitude ~1/4 |
| Paris 2026 municipal matrix (5 claims) | pre-2026-03 | 2026-03-22 | 2026-08-18 | LEVEL-SPLIT: ballot-level snare broken (confirmed), seat-level snare intact (4 rows fired); H2 positive, H1 counterexample |
| Bangladesh lock-in analysis (4 claims) | 2026-02-09 | partial | 2026-08-18 interim | P1–P2 confirmed (charter margin cleared its own narrowness line by 0.26); P3 open to 2027-08–2028-02, P4 open to 2027-02 |
| U.S. farm wages decomposition | 2026-08-03 | ~2027 | OPEN | Two kill conditions; resolvers: OEWS May-2027 release, H-2A 2027 disclosure data |
| France 2027 election essay | 2026 | 2027 | OPEN | Cordon-sanitaire-as-piton; Bardella-as-structural-vacancy (first transfer of L2) |
| China debt-deleveraging call | 2021-05-27 | ~2031 | OPEN (legacy) | Decade-horizon macro call, pre-DR; revisited 2021-09-22 |
| Cryptocurrency price forecast | 2021-09-27 | resolved | UNSCORED (legacy) | Pre-DR mechanical-model exercise; low priority |

Accuracy convention (GJ Open rows): own mean Brier minus crowd-median mean Brier;
negative = beat the crowd. Both years beat the median on average; the edge narrowed
in 2020 (−.05 → −.03) on a harder, more pandemic-shaped question mix.

Artifacts: Colombia — matrix + essay in `blog/2026-05_or_before/`
(`columbia_falsification_matrix.md`, `columbian_election_2026_essay.md`), post-mortem
`blog/2026-08/columbia-2026-post-mortem.md`, published at cafebedouin.org
(2026-01-17, 2026-01-26). Paris — matrix
`blog/2026-05_or_before/paris_2026_falsification_matrix.md`, post-mortem
`blog/2026-08/paris-2026-post-mortem.md`. Bangladesh — cafebedouin.org 2026-02-09
("How Bangladesh's Interim Government Is Locking In Constitutional Change…"),
interim scoring `blog/2026-08/bangladesh-2026-interim-scoring.md` (a
commitment-systems prediction registered before the CS sketch paper; diarize P4 for
2027-02 and P3 for 2027-08–2028-02).
France — `blog/2026-05_or_before/france_election_2027_essay.md`. Farm wages —
cafebedouin.org 2026-08-03 ("U.S. Farm Wages: The Decomposition Nobody Has Run").
China / crypto — cafebedouin.org 2021-05-27, 2021-09-22, 2021-09-27.
GJ Open reviews were standalone PDFs (not retained in-repo); their performance
numbers and complete lessons are preserved below.

Vocabulary note: the January-2026 matrices use "Noose" for the type the framework
now calls **snare**; this ledger and the 2026-08 post-mortems use snare throughout.

---

## Standing hypotheses (pre-registered, falsifiable)

**H1 — Mechanism-hit / magnitude-timid (registered 2026-08-18; n=3, mixed).** The
structural read identifies the right load-bearing joints and then under-prices their
intensity: misses cluster in the direction of *the named mechanism operating more
strongly than modeled*. Evidence for: Colombia (all four magnitude misses point that
way); Bangladesh P1 ("clear plurality" → two-thirds supermajority — the named
consolidation under-priced again); Paris's magnitude rows (Grégoire 37.98 vs 32–37
predicted; turnout 58.89 vs 45–52). **Counterexample:** Paris claim 5 ("reduced
majority bonus prevents hegemonic control") was a clean *mechanism* miss — the
matrix never modeled inter-round mergers or bonus-on-second-round-share math. So:
magnitude-timidity recurs in every scored case, but "mechanism always hits" is
already false at the layer the theory hasn't modeled. Sibling hypothesis: OQ-229's
pre-registered *fragility bias* — still no scored case supports it. **Test:** tag
every future register row `fragility`/`stability` per v2.2 and read the
error-direction distribution.

**H2 — Type-transfer (the load-bearing one).** Do constraint types carry forecast
profiles at all — does a lesson learned on a "snare" in one country apply to the
next thing classified as a snare? The Colombia→Paris pair was the designed
two-condition test (same topology theory, majority bonus 50%→25%, opposite
predictions). **First verdict (2026-08-18): positive at the ballot level.** The
full/weakened-snare distinction predicted a real observed differential across two
countries eleven weeks apart — Paris kept 5 lists >10% and its centrist at 11.34%
where Colombia compressed to two poles at 84.65% with the centrist at 4.26%. The
typing is predictive AND incomplete: the reform converted psychological compression
without converting mechanical compression (see L3) — "snare" needs a level index.
France 2027 is the second test (L2 transfer, registered in the essay itself);
Bangladesh P3 is the first commitment-systems-axis test (entrenchment→hollowing,
window 2027-08–2028-02).

---

## Typed lessons (constraint-taxonomy level)

**L1 — Ceilings are conditional on incomplete compression** *(Colombia 2026;
INFERRED — coupling reading not yet discriminated from bad-prior reading).* A
capacity ceiling claimed on a pole binds only while the compressed middle has
somewhere else to park; full compression transfers the strangled middle's mass to
the poles and breaks static ceilings. Register rule: never register a static ceiling
alongside an active compression mechanism without an interaction row. Discriminating
datum still owed: vote-flow analysis of Cepeda's first-round overage.
**Next test:** any future matrix pairing a compression claim with a bound claim.

**L2 — Scaffold→rope hardening is fast under vacuum + compression** *(Colombia
2026, from the matrix's own Feb 16 revision note).* When an institutional pole
collapses inside an active compression field, replacement coordination does not
proceed at institutional timescales — the compression energy drives rapid
scaffold-hardening (De la Espriella: 16–20% predicted February share, 32.1%
observed). **First transfer in flight:** France 2027 essay's Bardella-as-
structural-vacancy read. Score at the 2027 resolution.

**L3 — Compression relocates across levels rather than dying** *(Paris 2026;
RULED at the ballot/seat split — the level-split is in the official results, not an
interpretation).* Weakening one compression channel (psychological, pre-round-one)
moved the compression to downstream mechanical channels (inter-round list mergers,
25%-bonus-on-second-round-share seat math): a 37.98% first-round list took 63.2% of
seats with zero coalition partners. Judge snare-breaking reforms at the terminal
allocation, never the ballot. This is the *relocating confound* pattern
(`build_discipline.md`) appearing in the world rather than in a verification
harness. **Next test:** any future claim that a rule change converts Snare→Rope —
the register must carry rows at every level the extraction can relocate to.

---

## Protocol defects found by execution (feed into uke_write / uke_score revisions)

1. **Independent rows on coupled claims.** The Colombia register scored claims 1/3/4
   independently; the theory coupled them (L1). Registers over multi-constraint
   systems need explicit interaction rows.
2. **One-sided falsification gates.** Fajardo's 2× overshoot of his predicted floor
   scored as a clean hit because only the upper direction falsified. Where the
   theory implies a floor and a ceiling, gate both sides.
3. **RESOLVED-BUT-MISATTRIBUTED.** A threshold can fire while its pre-written
   interpretation is false (Colombia claim 2: primary turnout >4M fired; "Rope
   still functional" was wrong — the primary electorate defected to the scaffold
   candidate). Pre-registered interpretations are claims; the scoring rubric needs
   this outcome as a first-class sibling of UNRESOLVABLE.
4. **Terminal checkpoints can be near-tautological.** "Undecideds collapse by
   election day" is true in every election; the skill lived in the *rate*
   checkpoints (Feb/Apr). Put the falsification weight on rate and intermediate
   dates, not the terminal state.
5. **Compound verdict bands assume coherent movement** (Paris). The
   STRONG/PARTIAL/CONFIRM bands mixed ballot-level and seat-level conditions, so
   the level-split outcome reached no band — the verdict grammar went silent
   exactly when the result was most informative. Stratify bands by level.
6. **A register row without a named collector is a wish** (Paris). Compression
   velocity was the matrix's own "core test" and Gini its polarization
   instrument; both died UNRESOLVABLE because the weekly series and the
   arrondissement computation were specified but never assigned. v2.2 names
   resolvers; name collectors too.
7. **Thresholds without probabilities can't measure skill-over-baseline.** The
   Colombia matrix (pre-v2.2 idiom) is scoreable on its own terms but yields no
   `p_essay` − `p_baseline` skill number; the informal baseline comparison (markets
   implied a 14.5-pt separation; matrix said 1–4; actual 2.85) had to be
   reconstructed after the fact. v2.2's mandatory probability pairs fix this; hold
   future registers to it.

---

## General forecasting lessons (GJ Open era, pre-DR)

Preserved from the 2019/2020 review documents (source PDFs not retained). These are
the operator's own distillations; they predate the constraint framework and remain
the base-layer craft under any register.

**2019 (74 questions; Brier .23, accuracy −.05):**
- Base rates: bills passing U.S. Congress ~1–3%; U.S. state legislatures <10% up to
  ~50% in Western states; international legislatures behave more like state
  legislatures than Congress — build in a 15–30% cushion.
- The politics of countries you do not live in are even harder to forecast — hedge.
- People's expressed desires in media may not reflect what they really think.
- Base rates from small samples (e.g., Special Counsel investigation lengths) need
  big cushions.
- Regulation slows fundamentally new technology; new regulations, like laws, are
  slow to develop. There is a difference between technically possible and a product
  launch of that capability.
- Be careful going low or against the crowd on a popular franchise.
- "This time is different / this will remake the landscape" happens, but rarely —
  bet against the rare occurrence absent a very good reason.
- Questions resolving on opinions of individuals or small groups need a significant
  hedge; subjective preferences (award panels) resist predictive models.
- Know the technical details — laws, rules, procedures are often crucial.
- Know when you aren't bringing anything useful and the result is random chance;
  make sure you have a probabilistic edge on every question, even when riding the
  crowd on a risky call.
- Media figures entering politics get a boost from para-social relationships.
- Don't exclusively follow others — do your own research. Luck counts too.

**2020 (114 questions; Brier .32, accuracy −.03):**
- Moderate your forecast, particularly in uncertain environments.
- Base rates beat special circumstances, ~75/25; in politics, base rates often beat
  polls; when off base rate, look for reversion to the mean.
- If a question resolves around people, don't assume the logical answer is the
  right one; where institutional processes you don't understand are in play, be
  conservative.
- Mechanical/historical price scripts beat almost everyone's stock-price forecasts;
  don't forecast markets outside mechanical probabilities.
- Know the players and how their involvement changes the base rate; don't make
  extreme predictions against an incumbent/experienced politician. Bet the
  favorite.
- Famine/deflation-type declarations are low-probability by construction (the
  question is really about the number of such questions) — go ≤10%.
- Look for the thing you haven't accounted for (e.g., a stock split); sometimes a
  partial counts (symbolic Hajj).
- Do the work on neglected question classes (award shows with handicapping sites);
  some question types raise your Brier even when played well — budget for it.
- Get ahead of where the news is pointing.
- If a question has an exponential curve, put an exponent on your estimate — you
  are likely projecting a line.

---

## How to add an entry

1. **Register** forecasts in the essay per uke_write v2.2 §1.6/§6.1 (probability
   pairs, direction tags, self-contained rows, absolute dates, named resolvers).
   Check the register against the typed lessons and protocol-defect list above.
2. **Checkpoint** at the register's dates; log mid-course revisions in place (the
   Colombia Feb-16 note is the model — it carried the L2 lesson before resolution).
3. **Score** on resolution with `agent/uke_score_v0.1.md`, mechanism and magnitude
   separately. Record UNRESOLVABLE and RESOLVED-BUT-MISATTRIBUTED rows as protocol
   defects here, not as repaired interpretations.
4. **Post-mortem** in `blog/YYYY-MM/` when the case taught something a reader
   can't get by re-scoring the rows (coupling structure, error direction,
   protocol defects). A tally alone does not need a post-mortem.
5. **Ledger:** add the row to *The record*; update or falsify the standing
   hypotheses; promote any typed lesson with its status (RULED / INFERRED) and its
   next test. A lesson with no nameable next test goes to the case post-mortem,
   not here.

---

*Created 2026-08-18 (operator direction; drafted by Claude). Colombia scoring
witness: `blog/2026-08/columbia-2026-post-mortem.md`.*
