 ---
  Invariant-First Meta-Classification: Index Logic Analysis

  Empirical Ground Truth

  Before theorizing, I traced the actual classification mechanism through every code path. The findings are non-obvious
  and in some cases contradict naive expectations.

  The Classification Pipeline

  dr_type(C, Context, Type) :=
      1. metric_based_type_indexed(C, Context, MetricType)
         → compute χ = ε × π(P)
         → classify_from_metrics(C, ε, χ, Supp, Context, MetricType)
      2. integrate_signature_with_modal(C, MetricType, Type)
         → if MetricType = unknown AND signature = constructed_constraint → tangled_rope
         → if MetricType = unknown AND signature = coordination_scaffold → rope
         → if signature = natural_law → mountain (override everything)

  The signature system is not a tiebreaker. It is a structurally distinct classification mechanism that activates
  precisely when metric thresholds fail. This creates two distinct classification regimes operating in the same system.

  ---
  1. WHO (Power Position)

  What varies within WHO

  The effective extraction χ = ε × π(P), where:
  ┌───────────────┬──────┬────────────────────────────────┐
  │     Power     │  π   │             Effect             │
  ├───────────────┼──────┼────────────────────────────────┤
  │ powerless     │ 1.5  │ Extraction amplified 50%       │
  ├───────────────┼──────┼────────────────────────────────┤
  │ moderate      │ 1.0  │ Baseline                       │
  ├───────────────┼──────┼────────────────────────────────┤
  │ powerful      │ 0.6  │ Extraction reduced 40%         │
  ├───────────────┼──────┼────────────────────────────────┤
  │ organized     │ 0.4  │ Collective burden-sharing      │
  ├───────────────┼──────┼────────────────────────────────┤
  │ institutional │ -0.2 │ Net beneficiary                │
  ├───────────────┼──────┼────────────────────────────────┤
  │ analytical    │ 1.0  │ Observer baseline (= moderate) │
  └───────────────┴──────┴────────────────────────────────┘
  What remains invariant across all WHO values

  1. ε (base extraction) — the structural extraction property of the constraint itself
  2. Suppression score — structural, explicitly documented as unscaled by WHO
  3. All structural properties — coordination, asymmetric, enforcement, sunset
  4. Theater ratio — structural
  5. Classification threshold boundaries — from config.pl, WHO-independent
  6. Immutability perception — determined by WHEN × WHERE, not WHO
  7. Structural signatures — the 7-feature profile is WHO-invariant
  8. Measurement/drift data — the actual temporal trajectory

  How WHO invariance shapes classification

  WHO creates a one-dimensional projection of a multi-dimensional constraint space onto the chi axis. The same
  constraint maps to different chi values for different agents. Because classification boundaries are fixed in
  chi-space, the constraint crosses different thresholds depending on WHO is looking.

  But the critical finding is: WHO doesn't just shift WHICH type — it shifts which classification mechanism is active.

  The Phase Transition at π = -0.2

  For any positive ε:
  - Institutional chi = ε × (-0.2) < 0
  - Snare requires χ ≥ 0.66 → structurally unreachable
  - Tangled Rope requires χ ≥ 0.40 → structurally unreachable
  - Rope requires χ ≤ 0.35 ✓ but also ε ≤ 0.15 → fails for high-extraction constraints

  This means for any high-extraction constraint, institutional agents CANNOT be classified via metric thresholds.
  Classification falls to unknown → structural signature → typically tangled_rope (via constructed_constraint signature,
   which triggers for any constraint with suppression > 0.2).

  The institutional agent does not perceive snares. They are modeled as net beneficiaries of the constraint structure.
  When they see extraction, they see it as theater (piton, if theater ≥ 0.70) or as coordinated complexity
  (tangled_rope, via signature). This is not a bug — it is the formal encoding of epistemic position.

  The Trapped Ceiling at π = 1.5

  For powerless agents, χ = 1.5ε. For the standard powerless context (biographical, trapped):
  - effective_immutability = mountain
  - Mountain requires ε ≤ 0.15 AND suppression ≤ 0.05
  - Snare requires immutability = rope → structurally unreachable for powerless

  So powerless agents cannot see snares metrically either — but for the opposite reason. Institutions can't because chi
  is too low. Powerless can't because immutability is locked to mountain by their trapped exit condition. The metric
  system sees powerless agents as either seeing mountains (low-ε) or nothing classifiable (high-ε → signature fallback).

  The powerless agent does not perceive snares. Their experiential world contains only mountains (immutable) and
  tangled_rope (via signature when ε and suppression exceed mountain ceilings). A snare is a snare precisely because the
   trapped agent CANNOT name it as such — they experience it as an immutable feature of reality or as an
  incomprehensible tangle.

  WHO interaction with other indices

  WHO × WHERE: Power determines exit feasibility. But in the current architecture, exit_options are specified directly
  in the context tuple, not derived from power. The interaction occurs at the immutability level: a powerless agent with
   trapped exit has BOTH amplified extraction (π = 1.5) AND mountain immutability. These are independent mechanisms that
   compound.

  WHO × WHEN: Power moderately interacts with time. Institutional agents typically operate at generational+ horizons,
  powerless at biographical. But this is a convention of context construction, not a formal coupling — the system allows
   any (WHO, WHEN) pair.

  WHO × HOW MUCH: Currently no interaction. Spatial_scope is validated but never consumed by any classification
  predicate. This is the missing index.

  Where WHO invariance breaks

  1. Dynamic Coalition: When enough powerless victims organize, their effective power upgrades from π=1.5 to π=0.4. This
   is a discrete jump in the classification mechanism — a phase transition caused by collective action. The constraint
  didn't change, but the agent's position in the invariant structure shifted.
  2. The Signature Fallback: WHO-invariance of structural signatures means that when the metric system fails (which it
  does for both powerless AND institutional), the signature system produces identical outputs. This is why the dominant
  shift pattern is (TR,TR,TR,TR) — 297 of 689 classifiable constraints show the SAME type at all power levels. The
  signature system erases WHO-variation for constraints that live outside metric-reachable space.
  3. The π = -0.2 Semantic Discontinuity: The modifier transitions from positive (extraction perceived) to negative (net
   benefit) between organized (0.4) and institutional (-0.2). This is not a smooth gradient — there is no power level
  between 0.4 and -0.2. The classification mechanism changes qualitatively at this boundary.

  WHO dissonance analysis

  Low dissonance across WHO (pattern TR,TR,TR,TR): The constraint lives in signature space for all power levels.
  Classification is WHO-invariant because the metric system is uniformly irrelevant. These 297 constraints have a stable
   structural reality that all agents agree on — but they agree because none of them can resolve the constraint
  metrically.

  High dissonance across WHO (pattern TR,Sn,Pi,Sn): Maximum disagreement. Moderate and analytical agents see a snare
  (pure extraction). Institutional agents see a piton (theatrical maintenance). Powerless agents see tangled_rope
  (incomprehensible tangle). This is structural gaslighting: the constraint IS extractive, but the extraction is
  invisible to institutions (who benefit) and unnameable to the powerless (who are trapped).

  The "Truth at the Extremes" pattern (TR,R,R,TR): Powerless and analytical agree (both see tangled_rope), while
  moderate and institutional see rope. The extremes of power — those with the least and those with the most analytical
  distance — perceive the entanglement that middle-power agents miss. This is not a coincidence of π values; it's a
  structural consequence of the fact that both powerless (via trapped immutability → signature fallback) and analytical
  (via π=1.0 with civilizational scope) escape the "moderate comfort zone" where rope classification is metrically easy.

  ---
  2. WHEN (Time Horizon)

  What varies within WHEN

  The perception of immutability and the visibility of drift.
  ┌────────────────┬──────────────────────────────────┬───────────────────────────────────┐
  │  Time Horizon  │       Immutability Effect        │         Drift Visibility          │
  ├────────────────┼──────────────────────────────────┼───────────────────────────────────┤
  │ immediate      │ Mountain for trapped/constrained │ None — snapshot only              │
  ├────────────────┼──────────────────────────────────┼───────────────────────────────────┤
  │ biographical   │ Mountain for trapped/constrained │ Single lifecycle                  │
  ├────────────────┼──────────────────────────────────┼───────────────────────────────────┤
  │ generational   │ Mountain ONLY for trapped        │ Cross-generation patterns visible │
  ├────────────────┼──────────────────────────────────┼───────────────────────────────────┤
  │ historical     │ Rope for ALL exit conditions     │ Long-arc drift visible            │
  ├────────────────┼──────────────────────────────────┼───────────────────────────────────┤
  │ civilizational │ Mountain AND Rope (dual access)  │ Full trajectory visible           │
  └────────────────┴──────────────────────────────────┴───────────────────────────────────┘
  What remains invariant across all WHEN values

  1. The constraint's actual lifecycle trajectory — its measurement data doesn't change
  2. χ — effective extraction is WHEN-independent (χ = ε × π, no time component)
  3. Suppression — structural, not temporal
  4. Structural properties — enforcement, coordination, asymmetric, sunset
  5. Theater ratio — current value, not horizon-dependent
  6. Structural signature — the 7-feature profile

  How WHEN invariance shapes classification

  WHEN does NOT affect extraction perception. It affects which classification categories are accessible by controlling
  the immutability gate.

  The Mountain and Snare clauses in classify_from_metrics require specific immutability values:
  - Mountain requires effective_immutability(T, E, mountain) — the agent must perceive the constraint as potentially
  immutable
  - Snare requires effective_immutability(T, E, rope) — the agent must perceive the constraint as potentially changeable

  This creates a binary partition of the type space based on WHEN × WHERE:

  - Short horizon + trapped: Can see mountains, cannot see snares. The trapped agent in the immediate perceives
  constraints as facts of nature.
  - Long horizon + mobile: Can see snares, cannot see mountains. The mobile agent across generations perceives
  everything as changeable.
  - Civilizational + analytical: Can see BOTH. This is the only WHEN × WHERE combination with access to the complete
  type space.

  WHEN interaction with other indices

  WHEN × WHERE: These two indices are formally coupled through the immutability function. They should be analyzed as a
  single 2D surface, not independently. The immutability surface defines the classification possibility space.

  WHEN × HOW MUCH: Time horizon determines which drift patterns are visible. Short horizons hide metric_substitution
  (Goodhart drift). Long horizons reveal extraction_accumulation. The interaction is that larger scope amplifies the
  amount of drift data available — but currently HOW MUCH has no classification effect, so this interaction is latent.

  WHEN × WHO: The conventional pairing (powerless → biographical, institutional → generational) amplifies the WHO
  effect. But the formal system allows any pairing. An "institutional immediate" context would produce unusual results —
   institutional power with mountain immutability. The classification system permits this, but no standard context
  generates it.

  Where WHEN invariance breaks

  1. Horizon-Lifecycle Mismatch: A civilizational time horizon viewing a quarterly business constraint will see its
  ENTIRE lifecycle collapsed into a moment. Drift becomes invisible not because the horizon is too short, but because
  the constraint's cycle is too fast relative to the horizon. The constraint appears as a statistical smear, not an
  evolving structure. Current system does not model this — all measurements are treated identically regardless of time
  horizon.
  2. The Generational Inflection: At generational horizon, the immutability function shifts: trapped goes from mountain
  (biographical) to mountain (generational), BUT constrained shifts from mountain to rope. This is the formal encoding
  of "what one generation cannot escape, the next might organize against." The transition from constrained-mountain to
  constrained-rope happens at exactly the generational boundary. This is where Tangled Ropes become visible — long
  enough to see the coordination function, short enough that the extraction hasn't been naturalized.
  3. The Historical Flattening: At historical horizon, ALL exit conditions produce rope immutability. Nothing is
  perceived as immutable. This means the mountain classification is unreachable at historical+ (except for
  civilizational-analytical, which has the dual clause). For an analyst with a historical horizon, there are no
  mountains — everything is changeable given enough time. This may cause systematic under-detection of genuine physical
  constraints (mountains of physics) when viewed at historical scale.

  WHEN drift implications

  - Pitonization is invisible at short horizons: Theater ratio drift requires comparing initial and final states. An
  immediate-horizon agent sees only the current theater ratio, not the trend. They cannot detect that a Rope has become
  a Piton.
  - Extraction accumulation is invisible at biographical horizon: If extraction grows at 0.5% per generation, a
  biographical agent sees a stable 0.50 extraction. A generational agent sees 0.50 → 0.55 → 0.60 — the drift from
  tangled_rope toward snare. A civilizational agent sees the full 0.30 → 0.80 arc — the original rope becoming a snare.

  ---
  3. WHERE (Exit Options)

  What varies within WHERE

  The perception of constraint escapability, which feeds directly into immutability.
  ┌──────────────┬──────────────────────────────┬─────────────────────────────────────────────────────┐
  │ Exit Options │             Role             │                 Immutability Effect                 │
  ├──────────────┼──────────────────────────────┼─────────────────────────────────────────────────────┤
  │ trapped      │ No escape path visible       │ Mountain at short/medium horizons                   │
  ├──────────────┼──────────────────────────────┼─────────────────────────────────────────────────────┤
  │ constrained  │ Escape exists but costly     │ Mountain at short, Rope at generational+            │
  ├──────────────┼──────────────────────────────┼─────────────────────────────────────────────────────┤
  │ mobile       │ Can leave with moderate cost │ Rope at all horizons                                │
  ├──────────────┼──────────────────────────────┼─────────────────────────────────────────────────────┤
  │ arbitrage    │ Can exploit between systems  │ Rope at all horizons                                │
  ├──────────────┼──────────────────────────────┼─────────────────────────────────────────────────────┤
  │ analytical   │ Abstract/observer position   │ Special: dual (mountain AND rope) at civilizational │
  └──────────────┴──────────────────────────────┴─────────────────────────────────────────────────────┘
  What remains invariant across all WHERE values

  1. ε — base extraction is WHERE-independent
  2. χ — effective extraction is WHERE-independent (no exit modifier)
  3. Suppression — structural
  4. All structural properties
  5. Theater ratio
  6. Structural signatures
  7. All temporal measurement data

  How WHERE invariance shapes classification

  WHERE controls which side of the immutability gate an agent stands on. The monotonic exit-cost gradient maps to a
  binary immutability outcome — mountain or rope — with the transition point varying by WHEN.

  The critical asymmetry: WHERE affects Mountain and Snare accessibility, but it does NOT affect Scaffold, Tangled Rope,
   Piton, or the signature fallback. Those types do not check immutability. This means WHERE's classification effect is
  concentrated on the Mountain-Snare axis.

  The invariant principle: exit determines capture. An agent who cannot exit perceives the constraint as a feature of
  reality. An agent who can exit perceives it as a choice. The constraint itself is invariant — the agent's relationship
   to escape changes.

  WHERE interaction with other indices

  WHERE × WHEN: Already analyzed — these are formally coupled through immutability. The key interaction: WHERE
  determines the immutability gradient's sensitivity to WHEN. A trapped agent needs civilizational-analytical scope to
  access rope immutability. A mobile agent gets rope immutability at any horizon.

  WHERE × WHO: Power determines exit feasibility in practice, but in the formal system, they are specified
  independently. A "powerless + arbitrage" context is permitted but represents an unusual structural position — a
  low-power agent who can nevertheless exit (perhaps through geographic mobility, or digital exit from a platform). The
  system will produce meaningful results because π and immutability are independent mechanisms.

  Where WHERE invariance breaks

  1. Illusory Exit: The formal system treats exit as a given. But exit can be structurally illusory — a platform that
  formally allows you to leave but holds your data, network, and professional identity hostage. The constraint appears
  as mobile-exit (rope immutability) but functionally operates as trapped-exit (mountain immutability). This breaks
  WHERE invariance because the agent's STATED exit position doesn't match their EFFECTIVE exit position. The system
  currently has no mechanism to detect illusory exit.
  2. Exit as Extraction: Some constraints make exit itself extractive — leaving costs more than staying. Non-compete
  agreements, golden handcuffs, sunk cost traps. WHERE invariance assumes exit is a property of the agent's position.
  But when exit IS the extraction mechanism, the WHERE index and the extraction metric become entangled. The constraint
  is most extractive precisely when you try to exercise the exit that the WHERE index says you have.
  3. The Analytical Paradox: The analytical exit option produces a unique immutability configuration at civilizational
  scope: both mountain and rope are accessible. This is the only WHERE value with this dual property. It means the
  analytical agent can recognize both genuine immutability (mountains of physics) and contingent immutability (snares
  perceived as mountains by trapped agents). But this dual access depends on the specific (civilizational, analytical)
  pairing — it's an architectural choice, not a mathematical necessity.

  ---
  4. HOW MUCH (Scope)

  What varies within HOW MUCH

  In the current system: nothing that affects classification.

  Spatial_scope is declared, validated, stored in every context tuple, and present in every constraint_classification
  fact across 691 data files. It is consumed by zero classification predicates.

  This is the most important finding of this analysis.

  What SHOULD vary within HOW MUCH

  Based on the protocol's specification:
  Scope: dyadic/local
  Verification Difficulty: Low — direct observation possible
  Coordination Verifiability: High — can verify coordination in person
  ────────────────────────────────────────
  Scope: community
  Verification Difficulty: Moderate — requires sampling
  Coordination Verifiability: Moderate — coordination visible but not exhaustive
  ────────────────────────────────────────
  Scope: organizational
  Verification Difficulty: Moderate-high — institutional opacity begins
  Coordination Verifiability: Reduced — delegation hides mechanism
  ────────────────────────────────────────
  Scope: national
  Verification Difficulty: High — statistical only
  Coordination Verifiability: Low — coordination claims unverifiable by individuals
  ────────────────────────────────────────
  Scope: global
  Verification Difficulty: Extreme — epistemic opacity
  Coordination Verifiability: Negligible — coordination is always a claim, never an observation
  What SHOULD remain invariant across all HOW MUCH values

  1. The constraint's local mechanism — how it operates at any given interaction
  2. The base extraction rate per-interaction
  3. The structural logic of the constraint

  But these invariants break in a specific way at scale:

  The Threshold of Incoherence

  This is the scale at which the verification invariant — the ability to confirm that a constraint's claimed function
  matches its actual function — breaks down.

  Where the coordination signal becomes unverifiable:

  A Rope requires genuine coordination function. At local scope, you can observe whether the constraint actually
  coordinates. At national scope, you can only observe whether the constraint CLAIMS to coordinate. The transition is
  approximately:

  - local → regional: Coordination still directly verifiable. Multiple observers can compare experiences. Classification
   confidence remains high.
  - regional → national: Coordination becomes statistically verifiable but not individually confirmable. The theater
  ratio becomes harder to distinguish from coordination. This is where Ropes become indistinguishable from Tangled Ropes
   — not because the constraint changed, but because the verification mechanism degraded.
  - national → global: Coordination becomes a narrative, not an observation. All coordination claims at global scope are
   structurally unfalsifiable by individuals. This is where every claimed Rope could be a Snare, and the classifier
  cannot distinguish.

  Where the constraint predictably shifts type by scale:

  Applying the verification difficulty gradient to current thresholds:
  ┌───────────────────────────┬───────────────────────────────────────────┬────────────────────────────────────────────┐
  │ Constraint at local scope │             At national scope             │              At global scope               │
  ├───────────────────────────┼───────────────────────────────────────────┼────────────────────────────────────────────┤
  │ Rope (verified            │ Tangled Rope (unverified coordination +   │ Snare (coordination claim unfalsifiable,   │
  │ coordination)             │ observed extraction)                      │ extraction observed)                       │
  ├───────────────────────────┼───────────────────────────────────────────┼────────────────────────────────────────────┤
  │ Tangled Rope              │ Snare (asymmetric extraction grows with   │ Snare (extraction accumulates,             │
  │                           │ scale)                                    │ coordination evaporates)                   │
  ├───────────────────────────┼───────────────────────────────────────────┼────────────────────────────────────────────┤
  │ Scaffold (verified sunset │ Tangled Rope (sunset unenforceable at     │ Snare or Piton (sunset clause becomes      │
  │  clause)                  │ scale)                                    │ theater)                                   │
  ├───────────────────────────┼───────────────────────────────────────────┼────────────────────────────────────────────┤
  │ Mountain (verified        │ Mountain (still immutable)                │ Mountain (scale doesn't affect physics)    │
  │ immutability)             │                                           │                                            │
  └───────────────────────────┴───────────────────────────────────────────┴────────────────────────────────────────────┘
  The Mountain category is scale-invariant — physical constraints don't change with scope. Every other category degrades
   toward extraction at scale. This is the Threshold of Incoherence: scale is a universal drift-driver toward extraction
   categories, because verification difficulty increases monotonically while extraction accumulation has no
  scale-dependent ceiling.

  Where epistemic opacity forces classification-confidence to drop:

  Classification confidence should be a function of scope:
  confidence(Type, Scope) = base_confidence(Type) × verification_coefficient(Scope)

  Where verification_coefficient decreases from ~1.0 at local to ~0.3 at global. This means:
  - A local Rope classified with 0.90 base confidence retains 0.90 at local, 0.27 at global
  - A local Mountain classified with 0.95 base confidence retains 0.95 at local, 0.95 at global (mountains are
  scale-invariant)

  Where scale itself becomes a drift-driver:

  Extraction accumulates at scale because:
  1. Verification is impossible → accountability structures fail
  2. Coordination overhead grows superlinearly → more of the constraint's "budget" goes to maintenance rather than
  function
  3. Theater scales better than function → the ratio of performance-to-substance increases
  4. Beneficiaries concentrate while victims distribute → the asymmetry deepens

  This means temporal drift (WHEN) and spatial scale (HOW MUCH) compound: a constraint that is drifting toward
  extraction will drift FASTER at larger scope because the corrective mechanisms (verification, accountability, exit)
  all degrade with scale.

  ---
  Meta-Classification: Patterns of Invariance

  The 10 observed shift patterns are not arbitrary combinations. They are structural consequences of invariant
  interactions.

  Meta-Type 1: "Metric Void" — (TR,TR,TR,TR) — 297 constraints

  Invariance pattern: WHO-invariant. The metric system fails at ALL power levels. Classification is entirely
  signature-driven.

  What this means: These constraints have metric values that fall outside all threshold windows for every π value.
  Typically: ε between 0.15 and 0.46 (too high for rope, too low for snare) and/or suppression between 0.05 and 0.40
  (too high for mountain, too low for tangled_rope). The metric system has a blind spot in this region, and the
  signature system fills it.

  Drift implication: These constraints are invisible to metric-based drift detection. Their classification will not
  change even if extraction or suppression shift substantially — as long as they remain in the metric void. This is a
  stability trap: the constraint can degrade significantly without triggering reclassification.

  Scale implication: At larger scope, verification difficulty should push these constraints toward higher-confidence
  snare classification. Their WHO-invariance would break under a scope-sensitive modifier.

  Meta-Type 2: "Snare-Visible" — (TR,Sn,TR,Sn) — 150 constraints

  Invariance pattern: Moderate and analytical agree (both see snare via metrics). Powerless and institutional both fall
  to signature (tangled_rope). The snare is visible to middle-power agents with metric access, invisible to the
  extremes.

  What this means: These constraints have ε ≥ 0.66 AND suppression ≥ 0.60, putting them in metric range for snare at
  π=1.0. But powerless can't access snare (immutability = mountain) and institutional can't (chi negative).

  Dissonance: Moderate. The extraction is real and visible to those who can name it. But the powerless experience it as
  immutable (they can't call it a snare) and institutions experience it as coordinated complexity (they can't see their
  own extraction).

  Drift implication: If suppression drops below 0.60, moderate/analytical lose snare classification and fall to
  signature → (TR,TR,TR,TR). This is a discontinuous reclassification — a small change in suppression causes a
  qualitative shift in the meta-type.

  Meta-Type 3: "Institutional Theater" — (TR,Sn,Pi,Sn) — 134 constraints

  Invariance pattern: Same as Meta-Type 2, but institutional agents uniquely detect piton via theater ≥ 0.70. This is
  the ONLY meta-type where institutional power produces a UNIQUE classification not seen at any other power level.

  What this means: The constraint has high extraction (snare-level), high suppression, AND high theater. The institution
   — which can't see extraction (chi negative) — instead perceives the theatrical maintenance. This is structurally the
  most informative pattern: only institutional agents can detect the piton because only they are positioned to see that
  the "coordination" is performance rather than function.

  Meta-insight: This is where institutional power is most diagnostic. Not because institutions see more accurately, but
  because their unique epistemic position (net beneficiary + detection of theater) reveals a dimension invisible to
  other agents.

  Drift trajectory: If theater drops below 0.70, this pattern collapses to (TR,Sn,TR,Sn). The piton detection
  disappears, and the institutional agent reverts to generic signature classification. The WHEN × theater interaction
  matters here: theater ratio rising over time is what creates pitons, and only long-horizon institutional agents with
  theater-detection capability can observe this process.

  Meta-Type 4: "Powerless Trap" — (TR,R,R,R) — 72 constraints

  Invariance pattern: WHO-variant at only one level. Moderate, institutional, and analytical all agree (rope). Powerless
   alone sees tangled_rope.

  What this means: Low extraction (ε ≤ 0.15) but non-trivial suppression (> 0.05). For moderate+ agents, this is a fair
  coordination mechanism (rope). For powerless agents, the suppression exceeds the mountain ceiling but immutability
  locks them to mountain → metric failure → signature → tangled_rope.

  This is the formal model of "fair constraints that feel coercive to the powerless." The constraint IS a rope — it
  genuinely coordinates, has low extraction. But the powerless agent, trapped with mountain immutability, cannot
  perceive it as a rope. The suppression (which IS structural and WHO-invariant) feels like extraction to someone who
  can't escape.

  Drift implication: If ε rises above 0.15, moderate+ agents lose rope classification and fall to the metric void →
  (TR,TR,TR,TR). The "Powerless Trap" pattern is stable only for genuinely low-extraction constraints. Any drift toward
  extraction collapses the pattern.

  Meta-Type 5: "Hidden Piton" — (TR,TR,Pi,TR) — 24 constraints

  Invariance pattern: Institutional agents uniquely detect piton. Everyone else sees tangled_rope via signature.

  What this means: Theater ≥ 0.70 but extraction and suppression are in the metric void (not snare-level). The metric
  system fails for moderate and analytical as well as powerless. Only institutional agents — through the chi-negative
  piton pathway — can detect the theater.

  Meta-insight: These are constraints where the institutional vantage point is uniquely revelatory. The piton (inertial
  maintenance) is invisible from every other power position. This is the inverse of the "structural gaslighting" pattern
   — here, the institution is the only honest observer.

  Meta-Types 6-10: Scale-Sensitive Patterns

  The remaining patterns (scaffold variants, the rare (TR,R,R,TR)) have small populations but important structural
  implications:

  (Sc,Sc,Sc,Sc) — 4 constraints: WHO-invariant scaffold. The sunset clause and coordination function are SO clear that
  all power levels agree. These are well-designed temporary constraints.

  (TR,TR,Sc,TR) — 4 constraints: Only institutional agents detect the scaffold. Everyone else falls to signature. This
  is the "planned obsolescence visible only to planners" pattern.

  (TR,Sn,Sc,Sn) — 2 constraints: The scaffold is visible at institutional power (where chi allows it), while
  moderate/analytical see snare and powerless sees signature. The temporary nature of the constraint is perceptible only
   to those with enough power to see the sunset clause AND too little power to simply see extraction.

  (TR,R,R,TR) — 2 constraints: The "Truth at the Extremes" pattern. Powerless AND analytical agree on tangled_rope,
  while moderate and institutional see rope. The least powerful and the most analytically distant both perceive the
  entanglement that middle-power agents miss.

  ---
  Second-Order Invariants: What Holds Across Meta-Types

  Invariant 1: The Signature Floor

  Every meta-type includes tangled_rope at the powerless position. This is because:
  - Powerless = (biographical, trapped) → mountain immutability
  - Most constraints have suppression > 0.05 → mountain fails
  - Metric system falls to unknown → signature → constructed_constraint → tangled_rope

  The powerless agent ALWAYS sees tangled_rope for any constraint with non-trivial suppression. This is a structural
  invariant of the current architecture. It means the powerless perspective is informationally flat — it cannot
  distinguish between a genuine tangled_rope, a snare, a piton, or a badly-designed rope. They all look the same from
  the bottom.

  Invariant 2: Moderate-Analytical Parity

  Moderate and analytical always produce the same classification because they share π=1.0. They differ only in WHEN ×
  WHERE (biographical/mobile vs civilizational/analytical), which affects immutability. But in practice, both get rope
  immutability for their standard contexts. So they classify identically.

  This means the "analytical" perspective is not actually independent — it is a duplicate of the moderate perspective
  with different time horizon semantics. For the meta-classification to gain a genuinely independent analytical
  dimension, π_analytical would need to differ from π_moderate, or the analytical context would need to weight
  additional factors.

  Invariant 3: The Institutional Piton Detector

  Institutional agents are the ONLY agents that can detect pitons via metrics (because chi ≤ 0.10 is trivially true for
  negative chi, so the piton clause's chi check always passes for institutional). The piton then depends solely on
  theater ≥ 0.70. This makes institutional agents specialized piton detectors — their negative π is a feature, not a
  limitation, for this specific type.

  Invariant 4: The Missing Scope Effect

  HOW MUCH currently has zero classification impact. This means:
  - A local Rope and a global Rope are classified identically
  - A national Snare and a universal Snare are indistinguishable
  - The Threshold of Incoherence is not modeled

  Every meta-type analysis above is scope-blind. The meta-types would fragment under scope sensitivity. For example,
  "Metric Void" (TR,TR,TR,TR) at local scope might be genuinely ambiguous. At global scope, verification difficulty
  should push it toward snare. The meta-type would split into "Local Ambiguity" and "Global Opacity."

  ---
  Drift Predictions from Invariant Logic

  WHO-drift: Coalition Formation

  When powerless agents organize (π: 1.5 → 0.4), χ drops by 73%. A constraint at χ=0.90 (near tangled_rope ceiling)
  becomes χ=0.24 (rope territory). This predicts that collective organization changes the perceived TYPE of the
  constraint — what was a tangled_rope becomes a rope. This is the formal model of consciousness-raising: the constraint
   didn't change, but the collective agent now perceives it differently.

  WHEN-drift: Generational Reclassification

  At the generational boundary, constrained exit shifts from mountain to rope immutability. This predicts that
  constraints perceived as mountains by one generation may be perceived as snares by the next (if extraction exceeds
  threshold). The formal structure predicts generational conflict: the parent generation literally cannot classify the
  constraint as extractive (mountain immutability), while the child generation can (rope immutability).

  WHERE-drift: Exit Erosion

  If exit options degrade (mobile → constrained → trapped), the immutability perception shifts toward mountain. A
  constraint that was a visible snare becomes an invisible mountain. This predicts that loss of exit options causes
  misclassification — the extraction doesn't decrease, but the agent can no longer perceive it as extraction.

  HOW MUCH-drift (Predicted, Not Implemented): Scale Ratchet

  As constraints scale from local to global:
  1. Verification difficulty increases → theater becomes indistinguishable from function
  2. Coordination overhead grows → extraction per-participant increases
  3. Accountability degrades → sunset clauses become unenforceable
  4. Exit costs increase → WHERE effectively shifts toward trapped

  This predicts a universal drift toward extraction categories at scale. Ropes become Tangled Ropes, Scaffolds lose
  their sunset clauses (becoming Pitons or Snares), and meta-type classification confidence decreases monotonically with
   scope.

  Second-Order Drift: Classifier Degradation

  The signature system's constructed_constraint check (suppression > 0.2 OR resistance > 0.2 OR beneficiary count > 1)
  is extremely broad. As the corpus grows and constraints become more complex, an increasing fraction will match this
  catch-all signature. This predicts that the proportion of constraints classified via signature (rather than metrics)
  will increase over time — a drift in the classifier itself. The meta-type distribution will converge toward
  (TR,TR,TR,TR) dominance as metric-space coverage becomes proportionally smaller.

  ---
  What This Necessitates for Implementation

  1. Scope modifier: HOW MUCH needs a classification effect comparable to WHO's π modifier. The simplest form: a
  verification coefficient that degrades coordination confidence at scale, implemented as a suppression amplifier or
  theater inflation factor.
  2. Classification confidence scores: Each classification should carry a confidence value that degrades with scope and
  increases with temporal data availability. The meta-types above would each have characteristic confidence profiles.
  3. Immutability decoupling: The current WHEN × WHERE → binary immutability function collapses a 2D surface to 1 bit.
  This should be a continuous immutability score that feeds into classification as a weight rather than a gate.
  4. Analytical independence: π_analytical = π_moderate creates a degenerate dimension. If the analytical perspective is
   meant to be independent, it needs its own modifier — perhaps one that weights structural properties more heavily than
   metric thresholds.
  5. Signature audit: The constructed_constraint catch-all should be refined or replaced with type-specific signatures
  that produce more discriminating results for the metric-void region.
