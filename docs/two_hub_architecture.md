# Two-Hub Classification Architecture

**Purpose:** Architectural note explaining why the DR classification system has two independent sources of perspectival variation, why they should not be unified, what this structure reveals about where indexicality lives in the system, and what empirical verification against the corpus confirms about these architectural claims.

**Audience:** Anyone reading the codebase for the first time, or anyone asking why classification differences across observer positions arise from two separate mechanisms rather than one.

---

## The Two Hubs

Every classification in DR flows through `classify_from_metrics/6`. But the observer-dependent inputs to that function come from two independent subsystems that share no internal logic.

**Hub 1: Power-Scaled Extraction (the sigmoid).** The directionality derivation chain — `derive_directionality/3` → `sigmoid_f/2` → `extractiveness_for_agent/3` — maps observer position to an experienced extraction value χ. This is a continuous transformation: small changes in power position produce small changes in χ. The mechanism is structural. An observer's relationship to the constraint (beneficiary, victim, neither) determines a directionality value d, and the sigmoid amplifies or suppresses base extraction accordingly. All threshold-based classification differences (snare vs. tangled rope vs. rope) are downstream of this hub.

**Hub 2: Effective Immutability (the lookup table).** The `effective_immutability_for_context/2` table maps (TimeHorizon, ExitOptions) pairs to a perceived mutability level. This is a discrete function: 18 hard-coded facts that produce categorical outputs (mountain, rope, or neither). No sigmoid, no continuous transformation. The mechanism is perceptual. An observer with biographical time horizon and no exit options perceives a constraint as immutable; an observer with generational time horizon and systemic exit perceives the same constraint as mutable. The mountain gate and the rope gate in `classify_from_metrics/6` both check this table independently of χ.

These two hubs operate on different dimensions of the observer's position. Hub 1 asks: *how much extraction does this observer experience?* Hub 2 asks: *does this observer perceive the constraint as changeable?* A constraint can produce different classifications across observers purely through Hub 1 (different χ values crossing different thresholds), purely through Hub 2 (different immutability perceptions opening or closing the mountain/rope gates), or through both simultaneously.

---

## Why They Should Not Be Unified

The natural instinct is to fold Hub 2 into Hub 1 — to replace the discrete lookup table with a continuous function and absorb immutability perception into the sigmoid architecture. This would produce a single, unified source of perspectival variation. It would also be wrong, for three reasons.

**They measure different things.** Hub 1 measures how power position transforms the experience of extraction — a structural-economic relationship between observer and constraint. Hub 2 measures how temporal horizon and escape options transform the perception of mutability — a structural-epistemic relationship between observer and constraint. These are genuinely independent dimensions. An observer can experience high extraction (Hub 1 says snare) while perceiving the constraint as mutable (Hub 2 says not-mountain). This combination — high extraction, perceived mutability — is exactly the tangled rope, which is the most common constraint type in the corpus. Collapsing the two dimensions would lose the ability to distinguish "I'm being extracted from and I can't leave" (snare) from "I'm being extracted from but I could leave" (tangled rope) from "I'm not being extracted from and I can't leave" (mountain).

**They have different mathematical characters.** Hub 1 is continuous, differentiable, and admits sensitivity analysis. Small perturbations to d produce small perturbations to χ, and the system's sensitivity sweep confirms 87% of parameters are inert at ±25%. Hub 2 is discrete and categorical — there is no meaningful "halfway between mountain and rope" immutability perception. Forcing a continuous parameterization onto an inherently discrete judgment would introduce false precision. The immutability table's coarseness is a feature: it reflects the fact that mutability perception really does come in categorical blocks (I can change this / I cannot change this), not in continuous gradients.

**They interact productively at specific points.** The mountain gate in `classify_from_metrics/6` requires BOTH low χ (Hub 1) AND `effective_immutability = mountain` (Hub 2). The `snare_immutability_check/1` predicate crosses between the two hubs: it asks whether ANY standard context sees rope-level mutability, which is a Hub 2 query used as a gate within what is otherwise a Hub 1-driven classification path. These interaction points are where the system's most interesting diagnostic information emerges. If Hub 1 says "low extraction" but Hub 2 says "immutable," the constraint is a genuine mountain. If Hub 1 says "high extraction" but Hub 2 says "immutable," the constraint is a false mountain — actively enforced extraction that an observer perceives as natural law. The false mountain is only detectable because the two hubs are independent. A unified hub would blend the signals and lose the diagnostic.

**Verified:** The mountain gate's BaseEps check prevents the most pathological hub-conflicts entirely — no constraint in the corpus is simultaneously mountain-by-immutability and snare-by-extraction. Where hub-conflicts do arise (15 constraints where the immutability table flips between mountain and non-mountain across contexts), they cluster at exactly H¹ = 4, forming a distinct cohomological band that corresponds to Hub 2's discrete transition. See "Hub-Conflict as Cohomological Band" below.

---

## Where Indexicality Lives

The evaluation that prompted this note applied Henry Yuen's "fully quantum" complexity framing to the codebase and produced a finding worth preserving here: DR has a well-defined observer-independent substrate. The raw metrics — base extractiveness ε, suppression σ, theater ratio τ, the boolean structural features — are not indexed. They represent the constraint's structural properties as parsed from evidence. Classification is what introduces observer-dependence.

This means the system is not analogous to quantum superposition (where no observer-independent description exists). It is closer to a classical parametric family: fixed structural data, indexed by observer position, producing different classifications through deterministic transformation. The "quantum" element — the irreducibly observer-dependent component — lives in exactly one place: `derive_directionality/3`. This function maps observer position to a directionality value d, and everything downstream is deterministic machinery.

More precisely, indexicality enters the system through two channels, corresponding to the two hubs:

- **Hub 1 channel:** `derive_directionality/3` produces d from the observer's structural relationship to the constraint (beneficiary/victim/neither + power level). This is the primary indexical input.
- **Hub 2 channel:** The (TimeHorizon, ExitOptions) pair in the observer's context determines which row of the immutability table applies. This is the secondary indexical input.

Everything else in the classification pipeline — the sigmoid, the thresholds, the structural signatures, the MaxEnt likelihoods — is either observer-independent data or deterministic transformation of indexed inputs. The system's observer-dependence is fully localized to these two input channels.

---

## The Classical Oracle Test

The MaxEnt classifier operates as an independent "classical" analysis path. It computes a probability distribution over constraint types using only the raw, non-indexed metrics (ε, σ, τ). When MaxEnt agrees with the indexed deterministic classifier (`dr_type/3`), the constraint's classification is recoverable from observer-independent data alone — no indexing needed. When they disagree (`maxent_disagreement/3`), the constraint's classification requires indexical information that the raw metrics cannot provide.

This is structurally equivalent to a question in quantum complexity theory: can a classical oracle (with access to all non-indexed structural data) recover the same answer as the indexed computation? The `maxent_disagreement/3` predicate operationalizes this test. The indexed MaxEnt variant (`maxent_indexed_run/2`) extends it by computing what MaxEnt produces when given power-scaled χ instead of raw ε, allowing direct comparison of the classical and indexed probability distributions.

**Verified result (February 2026):** The classical oracle cannot always recover the indexed result. Eight constraints show divergence > 0.05 between classical and indexed MaxEnt, with the maximum at 0.63. All eight have H¹ > 0 — 100% correlation between probabilistic divergence and cohomological obstruction. Where MaxEnt detects that indexing changes the probability distribution, the cohomological formalism independently confirms that observer-disagreement is structurally present.

**The reverse direction is the more important finding.** Only 8 of 790 constraints with H¹ > 0 show high MaxEnt divergence — 1.0%. The cohomological formalism detects 100× more observer-dependence than the classical oracle can see. This has a precise interpretation: most observer-dependence in the corpus produces categorical classification shifts (mountain → scaffold, rope → tangled rope) without producing large probabilistic shifts in the MaxEnt distribution. The Gaussian likelihoods are broad enough that the raw metrics are compatible with multiple types, so shifting from ε to χ doesn't move the probability mass much — but it does cross the deterministic thresholds.

The classical oracle fails not by getting wrong answers but by failing to detect that answers differ across indices. It thinks the classification is ambiguous either way. The indexed classifier knows it is different. This is a stronger version of Yuen's claim than a simple "the oracle gets it wrong" — the oracle cannot even detect that there is a problem to solve in 99% of cases.

---

## The Snare Irreducibility Result

One finding from the cohomological analysis deserves special emphasis in this architectural context. Zero snares appear in H⁰ (global sections) out of 212 observer-independent constraints in the corpus. This means no constraint classified as a snare from any observer position is classified as a snare from every observer position.

This is a strong result. It means snare classification is irreducible to any single observer position. There is no "base" classification of a snare that all observers agree on. From the institutional position, the extraction is suppressed below the snare threshold by the power modifier, and the constraint appears as coordination (rope) or mixed (tangled rope). The snare is only visible from positions where the power modifier amplifies extraction above the threshold.

Architecturally, this confirms that Hub 1 (power-scaled extraction) is doing essential, non-eliminable work for an entire constraint category. Without indexing, snares do not exist as a classification — they collapse into other types. This is the strongest evidence in the system that indexed classification is not a refinement of non-indexed analysis but a categorically different operation for at least one constraint type.

The parallel finding for mountains is different. Mountains DO appear in H⁰ — some constraints are classified as mountains from every observer position. This makes architectural sense: a constraint that emerges naturally and has no extraction (ε ≈ 0) will classify as mountain regardless of power-scaling, because the sigmoid has nothing to amplify. Mountains are the one category where the classical oracle can always recover the indexed result, because there is no extraction for indexing to transform.

**Verified (February 2026):** Of 114 constraints preserved as mountain across all contexts, 25 have base extractiveness below 0.05. All 25 show exactly zero MaxEnt divergence between classical and indexed runs. The sigmoid does no work on genuine mountains. Their extraction is so low that power-scaling is flat — there is nothing for indexing to transform. This confirms that mountains and snares occupy opposite poles of the indexicality spectrum: mountains are fully classical (recoverable from non-indexed data), snares are fully indexed (invisible without observer position).

---

## Hub-Conflict as Cohomological Band

The two-hub architecture generates a specific class of constraints where Hub 1 and Hub 2 pull classification in different directions. These hub-conflict constraints are the clearest test of whether the two hubs are genuinely independent sources of perspectival variation.

**Verified result (February 2026):** Fifteen constraints show hub-conflict — classified as mountain at one context (Hub 2's immutability table locks them) but as scaffold or rope at another context (Hub 2 releases the lock). All fifteen sit at exactly H¹ = 4 in the cohomological formalism. The pattern is uniform: powerless sees mountain (biographical time horizon, no exit options → immutability table returns mountain), moderate sees scaffold (generational time horizon, constrained exit → immutability table releases). Every case.

No Type A conflicts exist — no constraint is simultaneously classified as mountain by the immutability table and at snare-level extraction by the sigmoid. The mountain gate's BaseEps check prevents this: it requires low base extraction as a precondition for the mountain classification, so the sigmoid never has enough raw material to push χ into snare territory for a constraint that passes the mountain gate. The two hubs are coupled at this boundary by `snare_immutability_check/1`, which prevents the most pathological conflicts from arising.

The H¹ = 4 clustering is not a correlation — it is an identity. The cohomological band H¹ = 4 *is* the band where Hub 2's discrete flip between mountain and non-mountain drives classification change. This means the cohomological formalism has decomposed the system's observer-dependence into bands that correspond to specific architectural mechanisms: H¹ = 3 for Hub 1-driven divergence (where power-scaling crosses thresholds), H¹ = 4 for Hub 2-driven divergence (where the immutability table flips). The mathematics is tracking the architecture.

---

## Two Kinds of Locality: Epistemic Restriction and Frame-Dependence

The system contains two independent phenomena that both look like "observer-dependence" but are architecturally distinct. The verification queries revealed this by testing the new `observer_accessible/3` restriction operator against the existing `gauge_fixed/3` frame-dependence detector.

**Epistemic restriction** is what `observer_accessible/3` and `classify_from_restricted/3` measure. An observer at a given power position cannot see all structural features of a constraint. A powerless observer experiences extraction but cannot measure the theater ratio. An institutional observer sees beneficiary identity but may not perceive the suppression mechanism as suppressive. When classification from restricted data differs from classification with full data, the observer's epistemic limitations are producing a different — and from their position, rationally justified — classification.

**Frame-dependence** is what `gauge_fixed/3` measures. An observer's structural frame (beneficiary, victim, neither) determines how the sigmoid processes extraction. Two observers can see the same data and still classify differently because the sigmoid transforms identical raw metrics through different directionality values. The frame is not an epistemic limitation — it is a structural relationship between observer and constraint that changes how extraction is experienced.

**Verified result (February 2026):** These two phenomena are nearly disjoint. Of 910 (constraint, context) pairs showing restricted-view divergence and 1,231 pairs showing gauge-fixedness, only 15 overlap — 1.6% in one direction, 1.2% in the other.

The mismatch samples clarify the distinction. Restricted-view divergences cluster in two patterns: `rope → piton` at institutional context (the observer can't see the theater ratio that distinguishes genuine coordination from performative coordination) and `tangled_rope → snare` at powerless context (the observer can't see the coordination function that distinguishes mixed constraints from pure extraction). These are information-loss errors — the observer lacks data that would change their classification.

Meanwhile, gauge_fixed constraints are ones where the observer has a structural frame that prevents them from seeing the constraint "from outside." An institutional observer gauge-fixed on a constraint they benefit from will classify it as coordination even with full access to the structural data, because the sigmoid suppresses experienced extraction below the threshold. This is not missing data — it is the correct classification from that structural position.

The quantum complexity framing names this distinction precisely. Epistemic restriction is having a reduced density matrix — less data than the full system contains. Frame-dependence is having a different measurement basis — processing the same data through a different structural relationship. These are independent phenomena in quantum mechanics and they turn out to be independent phenomena in DR. A unified "locality" concept would conflate them. The system is richer for keeping them separate.

**Practical consequence:** The two diagnostic tools are complementary, not redundant. `observer_accessible/3` catches cases where an observer's limited information produces misclassification — these are the epistemic traps where snares look like mountains because the observer can't see the enforcement mechanism. `gauge_fixed/3` catches cases where an observer's structural position produces a classification that is correct for them but different from what other positions see — these are the perspectival gaps that drive the "extraction always has a cover story" finding.

---

## Verified Predictions

This section records the four predictions generated by the original architectural note and their outcomes against the corpus.

**1. MaxEnt divergence clusters at H¹ > 0. — CONFIRMED.** All 8 high-divergence constraints have H¹ > 0 (all at H¹ = 3). Reverse direction: only 1.0% of H¹ > 0 constraints show high divergence. The cohomological formalism detects 100× more observer-dependence than the probabilistic formalism. See "The Classical Oracle Test" section above.

**2. Hub-conflict constraints have high H¹. — CONFIRMED (with correction).** Original prediction: H¹ ≥ 5 or 6. Actual result: all 15 hub-conflict constraints sit at H¹ = 4, not 5-6. The prediction was directionally correct (hub-conflicts do cluster at elevated H¹) but the specific band was lower than predicted. H¹ = 4 corresponds to exactly one classification change driven by Hub 2's immutability flip — the discrete table switching between mountain and non-mountain at the powerless/moderate boundary. See "Hub-Conflict as Cohomological Band" section above.

**3. Restricted-view divergence matches gauge_fixed. — DISCONFIRMED.** Bidirectional overlap: 1.6% and 1.2%. The two diagnostics measure nearly disjoint phenomena — epistemic restriction vs. frame-dependence. This disconfirmation is more informative than confirmation would have been: it reveals that the system has two independent kinds of observer-dependent classification error, not one. See "Two Kinds of Locality" section above.

**4. Mountain stability under indexing. — CONFIRMED.** All 25 genuine mountains (H⁰, ε < 0.05) show exactly zero MaxEnt divergence. The sigmoid produces no effect on near-zero extraction. No bugs in the sanity gates. See "The Snare Irreducibility Result" section above.

---

## Summary

The classification system has two independent sources of observer-dependent variation. They measure different structural relationships (experienced extraction vs. perceived mutability), operate with different mathematical characters (continuous vs. discrete), and interact at specific points where their independence produces diagnostic value. Indexicality is fully localized to two input channels: directionality derivation for Hub 1, temporal-escape context for Hub 2. Everything else is either observer-independent data or deterministic machinery.

Verification against the 1,033-constraint corpus confirmed three predictions and productively disconfirmed one:

- The classical oracle (MaxEnt on raw metrics) detects only 1% of the observer-dependence that the cohomological formalism sees. It fails not by getting wrong answers but by being unable to detect that answers differ across indices.
- Hub-conflict constraints — where the two hubs pull classification in opposite directions — cluster at exactly H¹ = 4, establishing an identity between architectural mechanism and cohomological band.
- Genuine mountains show zero MaxEnt divergence under indexing, confirming that mountains and snares occupy opposite poles of the indexicality spectrum.
- Epistemic restriction (what an observer can see) and frame-dependence (how an observer processes what they see) are nearly disjoint phenomena, not a single "locality" concept. The system has two independent kinds of observer-dependent classification error, which the new `observer_accessible/3` and existing `gauge_fixed/3` diagnostics catch complementarily.

The strongest architectural finding is the asymmetry in what indexed classification adds. For mountains, indexing adds nothing — they are fully recoverable from non-indexed data. For snares, indexing is essential — they do not exist as a classification without observer position. Between these poles, the system's six constraint types arrange along an indexicality gradient that the cohomological bands (H¹ = 0 through H¹ = 6) quantify with architectural precision.

---

*Architectural note for the Deferential Realism framework.*
*Prompted by structural parallels with Yuen (2026) on fully quantum complexity theory.*
*Original note: February 2026. Updated with verification results: February 2026.*
