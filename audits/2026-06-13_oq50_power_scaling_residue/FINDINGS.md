# OQ-50 power-scaling residue — census, the dead-restoration mechanism, and the joint witness

**Date:** 2026-06-13. **Branch:** `oq50-power-scaling-residue` (worktree `../wt-oq50-residue`, **NOT merged**).
**Subject:** why the two pristine physics controls `radiative_levitation_stratification` and
`actinide_replenishment_mechanism_flat_control` fire `type_1_false_summit` ×2 at the mid power seats,
and what disposition turns them GREEN without collateral.
**Corpora:** `testsets` (57), `testsets_flash` (960), `testsets_haiku` (960) — overlay-took-effect
witnessed per run (`[corpus] Loaded N`), N matched the file count each time.
**Evidence:** `census_probe.pl` (+`census_*.txt`), `phase2_probe.pl` (+`phase2_*.txt`),
`discriminator_probe.pl` (+`discriminator_*.txt`), `delta_control_probe.pl` (+`delta_control_output.txt`),
`joint_witness.pl` (+`joint_witness_output.txt`). All probes snapshot/restore via `probe_harness`,
clear caches per phase, and carry positive controls.

---

## TL;DR for the operator's Ω_C ruling

1. **The drl_core.pl:605–612 comment is STALE.** It says the metric layer degrades every mountain-claimer
   at the mid seats *"that the signature layer then restores in dr_type for genuine mountains."* The
   restoration **has not fired on any constraint, on any of the three live corpora** (Control 1 empty
   ×3; `natural_law_signature` passes = 0 ×3). It died when the OQ-44 fail-close set `HasAlternatives`'s
   default to `unknown` and **no authoring surface for `false` was ever built (GAP-08)** — so the
   `HasAlternatives == false` clause of `natural_law_signature` is *unsatisfiable corpus-wide*. The
   residue is therefore **not specific to physics**: every mountain-claimer degrades at the mid seats and
   none is restored.

2. **No metric relaxation can isolate the two physics cases.** `radiative`/`actinide` are *metrically
   identical* to social naturalization claims (`price_formation_kernel__naturalist_reading`,
   money-emergence, zero-as-discovered, animal-as-property) — same ε≈0, vic=0, `emerges_naturally`,
   same pass on accessibility/suppression/resistance/stability. The delta-exactly-two control shows
   **every** metric relaxation over-restores on both clean twins (the canonical false positive
   `price_formation__naturalist` shows up in *every* over-set). Disposition A "**relax** the failing
   `natural_law` condition" is **REFUTED by witness.**

3. **The only clean separator is authored: `HasAlternatives`.** Physics had no alternative; markets/money/
   property *were* choices. The drafted fix **feeds** that discriminator (a config-gated, default-off
   `no_viable_alternatives/1` authoring surface) instead of relaxing the gate. Scoped per authored
   constraint ⇒ delta-exactly-two **holds by construction**.

4. **The two physics controls are OVERDETERMINED — both-required, witnessed.** GREEN only when **both**
   the authored-`HasAlternatives` leg **and** the funded-science beneficiary leg are on. Neither alone
   clears it. **Correction to the plan's framing:** the second leg is *not* the FSM victim-gate — it is
   `natural_law`'s own `BeneficiaryCount == 0`, which the FSM victim-gate does **not** satisfy (it only
   swaps the *flavor* of RED from `false_summit_mountain`→`ambiguous`). See §5.

5. **Operator's Ω_C call, now well-posed:** (a) accept an authored `no_viable_alternatives` surface as the
   GAP-08 fill? and (b) rule on whether *funded-science beneficiaries* disqualify a `natural_law`
   (`BeneficiaryCount == 0`). Both are needed for radiative/actinide to go GREEN. The draft implements
   only (a), config-gated default-off; (b) is left to the operator (it is the OQ-122 beneficiary question).

---

## Phase 1 — Census (read-only). Three controls, all witnessed.

Per-corpus, every mountain-claimer with ε, victim/beneficiary/agent-beneficiary counts, the six
`natural_law` profile fields with per-field pass/fail, `constraint_signature`, `is_mountain` (metric)
and `dr_type` (final) at the 4 standard seats, `type_1` firing, and a partition label.

- **Control 3 (premise check) PASS on all 3 corpora.** No mountain-claimer holds `is_mountain=mountain`
  at the moderate or institutional seat — the drl_core.pl:605–612 *degradation* premise is intact, so
  Phase 2 was licensed to proceed (this was a declared STOP-AND-REPORT fork; it did not trip).
- **Control 2 PASS on all 3.** Victim-bearing claimers depart mountain (the `type_1` firing path is live
  and detectable).
- **Control 1 EMPTY on all 3.** *No* constraint reaches `dr_type=mountain` at all four seats anywhere —
  i.e. mountain-restoration never fires. Per the plan this is reported **against Control 1** (the
  detection is positively validated by the Phase 2(d) unit control below), not asserted bare.

Mechanism located: the mid-seat degradation is **not** a χ effect on the mountain branch (that branch
ignores χ). It is the `effective_immutability` table (`constraint_indexing.pl:195–227`):
`(biographical, mobile)→rope` and `(generational, arbitrage)→rope`. An agent with *exit options* reads
even a genuine immovable as escapable "rope." The `natural_law` override exists precisely to bypass this
("you cannot exit physics"), but it is dead (Phase 2).

## Phase 2 — Analysis (evidence-only). `phase2_*.txt`.

- **(a) Restoration is dead corpus-wide.** `natural_law_signature` passes **0** constraints on each
  corpus. Per-field FAIL tally over mountain-claimers: `HasAlternatives` (`ha`) fails **11/11, 104/104,
  72/72**; `TemporalStability` fails 0. The gate is blocked **solely** by the unauthorable `ha==false`.
- **(b) The failing condition, beneficiary-free, is the SINGLETON `{HasAlternatives}`.** radiative/actinide
  baseline fail-set `{bc, ha}`; with all beneficiaries retracted, fail-set collapses to `{ha}`.
- **(c) The residue is victim-independent.** vic=0 physics cases degrade at the mid seats (scaffold/rope)
  exactly as vic>0 social cases do — pure power-scaling/immutability, not concealment.
- **(d) UNIT POSITIVE CONTROL — restoration logic is LIVE.** A synthetic `profile(0.92,0.02,0.04,0,false,
  stable,_)` PASSES `natural_law_signature` and `resolve_modal_signature_conflict(rope, natural_law, R)`
  yields `R=mountain`; the same profile with `HasAlternatives=unknown` FAILS. ⇒ the corpus-wide absence
  is real (the probe sees restoration when it happens; it simply never happens), and the blocker is
  exactly `HasAlternatives`.

## Phase 2.5 — The discriminator question, witnessed. `discriminator_*.txt`, `delta_control_output.txt`.

The restoration-eligible set (passes accessibility/suppression/resistance/stability) is large on the
twins — **62 in flash, 30 in haiku** — and contains many vic=0, ε≈0 social naturalization readings that
are *metrically indistinguishable* from radiative: `price_formation_kernel__naturalist_reading` (ε=0.0,
vic=0, emerges_naturally), `zero_as_number_entry`, `animal_moral_status__property`, money-emergence,
`technological_determinism` readings, `temple_sacrifice` readings.

**Delta-exactly-two control** (does any metric relaxation restore EXACTLY `{radiative, actinide}`?):

| relaxation | testsets | testsets_flash | testsets_haiku |
|---|---|---|---|
| `R_ha` (drop `ha`, keep `bc==0`) | n=0, fails | n=5 over-restores | n=7 over-restores |
| `R_ha_bc` (drop `ha` & `bc`) | n=3 over (+demographic) | **n=62 over** | n=30 over |
| `R_scoped` (+ ε≤0.05 & vic=0) | n=2 **holds** | **n=22 over** | n=9 over |

The only "holds" is `R_scoped` on the 57-story `testsets` — a **small-corpus measured-empty artifact**
(the identical trap that the OQ-122 turn-2 "swap≡remove" fell into; the corpus simply contains no other
pristine case). On both clean twins every relaxation over-restores, and `price_formation__naturalist`
appears in **every** over-set. **⇒ no metric scope isolates physics from social naturalization.**

## Phase 3 — Draft fix (config-gated, default-off) + JOINT WITNESS. `joint_witness_output.txt`.

**Drafted fix (`git diff` on the branch, 4 files, +29 lines, all default-off):**
- `narrative_ontology.pl` — declare `no_viable_alternatives/1` (dynamic + multifile): authored "this
  constraint had no viable alternative" (the GAP-08 surface).
- `signature_detection.pl` — `has_viable_alternatives(C, false) :- config:param(oq50_alt_authoring,1),
  narrative_ontology:no_viable_alternatives(C), !.` inserted **after** the `true` clause, **before** the
  `unknown` fallback. Absence still fails closed to `unknown` (does **not** reopen the OQ-44
  pass-on-absence defect).
- `config.pl` / `config_schema.pl` — `param(oq50_alt_authoring, 0)` (+ schema spec). Default 0 ⇒ current
  behavior exactly.

**Joint witness over `{radiative, actinide}` (no_viable_alternatives authored only for those two):**

| cell | result | type1 | signature | seats |
|---|---|---|---|---|
| {HA off, BENEF off} (baseline) | **RED** | 4 | false_summit_mountain | TR/scaffold/scaffold/TR |
| {HA on, BENEF off} | **RED** | 4 | false_summit_mountain | TR/scaffold/scaffold/TR |
| {HA off, BENEF on} | **RED** | 2 | ambiguous | mtn/rope/rope/mtn |
| **{HA on, BENEF on}** | **GREEN** | 0 | **natural_law** | **mtn/mtn/mtn/mtn** |

⇒ **both-required** (the standing bet), witnessed. **Delta-exactly-two HOLDS:** with both legs on and the
flag authored only for the two, the corpus-wide `natural_law`-pass set = **exactly `{radiative,
actinide}`**. The authored discriminator achieves what no metric relaxation could (Phase 2.5).

- HA leg = `oq50_alt_authoring=1` + authored `no_viable_alternatives` ⇒ `has_viable_alternatives=false`
  ⇒ clears `ha`.
- BENEF leg = `constraint_beneficiary` retracted for the two (overlay standing in for the funded-science
  ruling) ⇒ `agent_beneficiary` empties ⇒ clears `bc==0`.

**Regression (fixture impact) = ZERO.** Default-off, the new `has_viable_alternatives` clause never
fires, so behavior is logically unchanged. Witnessed: `test_contradiction_signatures` 12 passed / 5
failed and `test_agent_beneficiary` 53 passed / 31 failed are **identical on the branch and on main**
(b0fa5bc6). Those pre-existing failures are not introduced by this change and are out of scope.

## §5 — Correction to the plan's joint-witness framing (the FSM victim-gate is NOT the second leg)

The plan paired "OQ-50 fix" with the **FSM victim-gate**. Witnessed correction: the victim-gate operates
on `false_summit_mountain`; exempting radiative there only changes its signature
`false_summit_mountain → ambiguous` (the {HA off, BENEF on}-shaped cell: `type1=2`, mtn/rope/rope/mtn —
**still RED**). It does **not** satisfy `natural_law`'s own `BeneficiaryCount == 0`, which radiative
fails with 3 agent beneficiaries. Restoration requires zeroing `bc` *at the natural_law gate*. So the two
genuinely-required levers are **(1) authored `HasAlternatives=false`** and **(2) a ruling that
funded-science beneficiaries do not disqualify a `natural_law`** — not the FSM victim-gate. The
`{HA on, BENEF off}=RED` cell is the witness that HA-authoring alone is insufficient.

---

## Dispositions for the operator's Ω_C ruling

- **A. Relax the failing `natural_law` condition — REFUTED.** Delta-exactly-two over-restores on both
  twins; `price_formation__naturalist` and other naturalization claims would be (mis)certified as natural
  law. Do not.
- **A′. FEED the gate (the drafted fix).** Config-gated authored `no_viable_alternatives` surface (GAP-08
  fill). Restores *exactly* the constraints a human authors as alternativeless; delta-exactly-two holds.
  Requires three things to land radiative/actinide GREEN: (i) accept the surface [drafted, default-off],
  (ii) author the flag for genuine-no-alternative constraints, (iii) **the beneficiary ruling (B′ below)**.
- **B. Metric `is_mountain` fix** — same over-restoration problem as A (metrically inseparable). Broadest
  blast radius. Not recommended.
- **B′. Funded-science beneficiary ruling (the bc leg, = OQ-122's question).** Should agent beneficiaries
  on an otherwise-pristine, no-alternative natural law count against `BeneficiaryCount == 0`? Witnessed
  necessary for GREEN. Deliberately **not coded** — it is the operator's Ω_C call. (Note: the existing
  `oq122-fsm-victim-gate` branch does **not** resolve this — see §5.)
- **C. Detector-level (`type_1` declines on no-victim)** — the FCR/superheavy control (prior OQ-122 audit)
  established the no-victim principle does **not** extend to the cap layer. Out.
- **D. Residue correct for the social mid-power cases; physics exemption routes through A′/B′.** Coherent,
  and it is what the evidence supports: mid-seat `rope` *is* the correct reading for a contested
  naturalization claim; the physics exemption is the *authored* `no_viable_alternatives` route, not a gate
  relaxation. ("Accept RED for radiative/actinide" remains NOT a live option — they are pristine, vic=0,
  nothing to conceal; the OQ-122 ruling stands.)

## Out of scope / guards

- OQ-50 OPEN-1 (explainer/detector coherence) and OPEN-2 (bound-Context sibling-clause) — separate OQs.
- The social/contested mountain-claimers must stay `rope`; the delta-exactly-two control enforces this and
  REFUTES any metric relaxation that would move them.
- `neutron_star_bombardment_reading` (`false_ci_rope`, vic=0) — OQ-70 bait-confound thread, not here.
- The pre-existing `test_agent_beneficiary` (31) / `test_contradiction_signatures` (5) failures on main —
  not introduced here; the FSM-fixture migration is the OQ-122 thread.
- **No merge.** The branch holds the default-off draft; the Ω_C ruling (A′ acceptance + B′) is the operator's.

## ADDENDUM (2026-06-13, turn 2) — six-questions lens corrects the disposition framing

The original §"Dispositions" framed the two GREEN-blockers as two free Ω_C choices. Pointing
`docs/six_questions.md` back at the engine sharpens both and reclassifies one:

- `natural_law_signature` is the **Question-5** machinery (`six_questions.md:43`, "if this disappeared,
  would the world rearrange?"). `HasAlternatives==false` is its purest condition; AccessCollapse/
  Suppression/Resistance/TemporalStability are legitimate Q5 contingency-measures.
- **`BeneficiaryCount==0` is a Question-1 (extraction) condition welded into the Q5 verdict.**
  `six_questions.md:55` rules them orthogonal verbatim — *"Size measures contingency, not extraction."*
  The Q5 entity is the **stakeholder** (`:51`, "whose arrangements depend on the rule"); the clean
  stakeholder-marker is the **payer/victim**, not the beneficiary, because beneficiaries can be *external*
  (the grant-funded scientist gains but has no stake; `:49`, "no human had a stake in gravity"). So the
  OQ-50 NL gate and the FSM gate share **one bug — gate on beneficiary — and one fix: read
  `constraint_victim` (the payer)**. One principle, two sites; the mechanism under the turn-one FSM
  victim-gate.

Reclassified handoff:
- **A′ / the `no_viable_alternatives` surface — principle FORCED, not a choice.** `logic.md:695` lists
  "no viable alternatives" as a *required* NL condition the engine can never satisfy (no false-input path)
  ⇒ a **defect against the spec**. Only the authoring criterion (who marks the flag, by what test) is
  escalable. (a) was over-escalated at the head.
- **B′ / the beneficiary leg — a SPEC-vs-SPEC CONTRADICTION, not a design preference.** `logic.md:678` and
  `:695` mandate **"zero beneficiaries"** for NL; the code faithfully implements logic.md. `six_questions.md:55`
  contradicts it (payer, not beneficiary, is the Q5 marker). Two spec docs disagree; the engine sides with
  logic.md. **Proposed resolution: cut the weld, read `constraint_victim` as the stakeholder marker.**
  Ruling = amend `logic.md:678/695` (which operationalized Q5 with a broken beneficiary proxy) or keep it.
  This is a contradiction-between-sources escalation (the operator's call) — it is NOT self-resolvable by
  declaring `six_questions` the winner over the named formal spec, and it is NOT the "free design choice"
  the original writeup called it.

## ADDENDUM (2026-06-16, turn 3) — the MISSING control for B′: payer-read delta-exactly-two across 7 corpora

The turn-2 ADDENDUM and the joint_witness gave the **HA leg** a delta-exactly-two control (the one
that REFUTED disposition A). The **payer-read leg (B′)** — swap natural_law's `BeneficiaryCount==0`
for `\+ constraint_victim(C,_)` — never got the analog. The joint_witness GREEN cell **faked** the bc
leg by *retracting* `constraint_beneficiary` on the two hand-picked cases (FINDINGS §3, line 138): it
witnessed "zero bc on {radiative,actinide} ⇒ GREEN," **not** "read the payer ⇒ these two and nothing
else." `payer_read_control.pl` (+`_output.txt`, `_archives.txt`) supplies the analog: it measures the
**ungated** stakeholder set (`metric_nl` & ha-dropped) under each stakeholder condition, characterising
what each admits on its own merits.

**Positive control (self-validating):** this probe's `R_ha` (bc==0) column reproduces
`delta_control_probe.pl`'s `R_ha` **exactly** — n=0/5/7 on testsets/flash/haiku — so its enumeration is
verified and the new `R_payer` column is trustworthy. `perturb.py` is **not** the instrument for this:
it sweeps numeric config params via `product_site_export`; the bc→victim swap is a gate-predicate change,
not a param. The Prolog delta-control harness is the right tool.

| corpus | mountain-claimers | R_ha (bc==0) | R_payer (vic==0) | Δ | excess character |
|---|---|---|---|---|---|
| `testsets` (57, live) | 11 | 0 | **2 = {radiative,actinide}** | +2 | small-corpus artifact (only phys2 is vic0) |
| `testsets_flash` (960, live twin) | 104 | 5 | **29** | +24 | **social naturalization (heavy)** |
| `testsets_haiku` (960, live twin) | 72 | 7 | **23** | +16 | **social naturalization (heavy)** |
| `kernel_v1` (1106, archive) | — | 30 | 40 | +10 | math claims + 2 social (animal_property, tech_determinism) |
| `original_v5` (702, archive) | — | 0 | 0 | 0 | measured-empty (no metric_nl passer) |
| `original_v6` (3380, archive) | — | 412 | 426 | +14 | math-naturalization dominated |
| `testsets_sotu` (189) | — | 0 | 0 | 0 | measured-empty |

**Pre-registered branch FIRED ("weakens B′"):**

1. **`R_payer ≥ R_ha` on every corpus (7/7); strictly greater on all 5 non-empty.** The payer-read is
   **never the tighter gate** — it admits strictly more mountain-claimers than the beneficiary rule it
   would replace, everywhere measured (~6,400 stories). The premise that made payer-read attractive ("bc
   is a leaky Q1 proxy; victim is the clean Q5 marker") does **not** yield a cleaner gate.
2. **On the de-leaked LIVE twins the excess is dominated by social naturalization** — flash newly-admits
   `price_formation__naturalist`, `digital_money_emergence`, `zero_as_number`, `animal_property`, money-
   emergence; haiku adds `press_reformation__technological_determinism`, `qwerty_persistence`,
   `market_as_natural_default`, the `total_war` contraction readings. Exactly the class metric relaxation
   pulled in and was refuted for. The live rebuild is where this matters most.
3. **The only "holds exactly {radiative,actinide}" is on the 57-story `testsets`** — the *identical*
   small-corpus measured-empty artifact the FINDINGS already used to refute `R_scoped` (lines 104–108).
   By the audit's own logic it is not evidence of tightness.

**Honest scope boundary (does NOT overclaim):** this measures the *ungated* stakeholder condition. OQ-126
*as specified keeps `ha==false` gating*, so in the gated regime the social twins remain blocked (no
authored `no_viable_alternatives`) and OQ-126's behavior-preservation + delta-exactly-two claims **still
hold** — the isolation is done by the HA leg. What the control kills is the **justification** for B′: the
victim-read contributes **zero discriminating power** and is **strictly looser** than the rule it replaces;
100% of the GREEN-cell isolation came from the HA leg plus hand-retracting two beneficiaries, never from
the payer-read. "Not persuaded" is vindicated by witness.

**This does NOT settle B′.** The remaining hinge is the governance call the substrate cannot adjudicate:
**which spec doc governs — `logic.md` (named formal spec, zero-beneficiary NL) or `six_questions.md`
(payer is the Q5 marker)?** That is the operator's Ω_C seat. The witnessable half is now closed and it
says the payer-read buys no isolation; the doc-authority half remains open and is not self-resolvable.

## Next step (landed in substrate)

ISSUES.md OQ-50 carries the residue thread with this audit as evidence; OQ-122 graduation step (b)
is witnessed (joint diff = both-required). Forward move is the operator's ruling, now split:
- **A′ (the `no_viable_alternatives` HA-leg surface)** — rulable now: spec-forced GAP-08 fill, default-off,
  regression-zero. Open sub-decision is only the **authoring criterion** (who marks the flag, by what test);
  "delta-exactly-two holds" witnessed a 2-element hand-authored set, **not** a criterion — do not read it as
  the criterion being validated.
- **B′ (payer-read / beneficiary-vs-victim)** — the witnessable half is now CLOSED against it (this addendum:
  payer-read is the looser gate, leaks to social naturalization on the live twins, isolation is all HA leg).
  What remains is the pure **`logic.md` vs `six_questions.md` doc-authority** governance call — operator only.
If A′ is ruled in, landing requires: author `no_viable_alternatives` for the genuine cases, then re-run
`joint_witness.pl` + `delta_control_probe.pl` + `payer_read_control.pl` across all three live corpora.
