% ============================================================================
% CONSTRAINT STORY: straight_coercion_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_straight_coercion_2025, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: straight_coercion_2025
 *   human_readable: Normalized Taiwan Strait Military Coercion
 *   domain: political/military/geopolitical
 *
 * SUMMARY:
 *   By 2025, Chinese military activity in the Taiwan Strait has undergone a
 *   structural transformation from event-driven signaling (responsive to US
 *   actions, Taiwan political developments, or Beijing policy announcements)
 *   to normalized operational readiness cycles (decoupled from external
 *   triggers and embedded in peacetime military schedules). This
 *   normalization represents the consolidation of coercion into institutional
 *   routine — military exercises now occur on training calendars, rotational
 *   force cycles, and doctrine verification schedules independent of
 *   political messaging. The Taiwan civilian population experiences this as
 *   permanent ambient threat, while Beijing frames it as standard military
 *   readiness. The constraint exhibits all six DR types depending on observer
 *   position: for Taiwan, a snare (trapped, powerless, bearing full
 *   extraction cost); for the PRC military, a rope (solving the coordination
 *   problem of embedding political pressure into operational readiness); for
 *   the US, a tangled rope (both beneficiary of alliance reinforcement and
 *   victim of escalation exposure); for international norms, a piton (rules
 *   invoked performatively while overridden functionally); and from a
 *   civilizational analytical view, a false mountain (naturalized as
 *   strategic necessity when actually contingent on political choices and
 *   institutional design).
 *
 * KEY AGENTS:
 *   - Taiwan Civilian Population: Primary victim (powerless/trapped) — bears psychological pressure, economic uncertainty, and kinetic risk; cannot exit geographic constraint
 *   - Taiwan Democratic Government: Secondary victim (powerful/constrained) — faces military asymmetry and coercive pressure; also benefits from international democratic alliance signaling
 *   - People's Republic of China Military Command: Primary beneficiary (institutional/arbitrage) — exercises generate readiness, political consolidation, deterrence signaling, and doctrinal validation
 *   - People's Republic of China Political Leadership: Secondary beneficiary (institutional/arbitrage) — uses normalized exercises for domestic stability, hawkish constituency satisfaction, and status signaling
 *   - United States Security Establishment: Mixed agent (organized/mobile) — benefits from alliance consolidation and defense contracting; bears escalation risk and credibility exposure
 *   - International Rules-Based Order (UNCLOS, Freedom of Navigation): Tertiary victim (institutional/arbitrage) — rules persist performatively while coercive practice overrides functional enforcement
 *   - Analytical Observer: Neutral position (analytical/analytical) — risks naturalizing contingent coercion as immutable strategic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(straight_coercion_2025, 0.58).
domain_priors:suppression_score(straight_coercion_2025, 0.72).
domain_priors:theater_ratio(straight_coercion_2025, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(straight_coercion_2025, extractiveness, 0.58).
narrative_ontology:constraint_metric(straight_coercion_2025, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(straight_coercion_2025, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(straight_coercion_2025, tangled_rope).
narrative_ontology:human_readable(straight_coercion_2025, "Normalized Taiwan Strait Military Coercion").
narrative_ontology:topic_domain(straight_coercion_2025, "political/military/geopolitical").

domain_priors:requires_active_enforcement(straight_coercion_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(straight_coercion_2025, people_republic_of_china_military_readiness).
narrative_ontology:constraint_beneficiary(straight_coercion_2025, prc_domestic_political_stability).
narrative_ontology:constraint_beneficiary(straight_coercion_2025, prc_deterrence_capability).
narrative_ontology:constraint_victim(straight_coercion_2025, taiwan_civilian_population).
narrative_ontology:constraint_victim(straight_coercion_2025, taiwan_economic_activity).
narrative_ontology:constraint_victim(straight_coercion_2025, us_security_commitments_credibility).
narrative_ontology:constraint_victim(straight_coercion_2025, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TAIWAN CIVILIAN POPULATION (SNARE) — Cannot exit the geographic constraint; bears full cost of military coercion through economic uncertainty, disrupted commerce, psychological pressure, and kinetic risk. No arbitrage available; exit would require relocation. d≈0.93, f(d)≈1.40, σ=0.9 → χ≈0.59.
constraint_indexing:constraint_classification(straight_coercion_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TAIWAN DEMOCRATIC GOVERNMENT (TANGLED ROPE) — Constrained by military asymmetry and economic integration but also benefits from US security commitments and international attention to democratic values. Experiences coercion as the primary function but recognizes coordination benefit (deterrence signaling) as secondary. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.54.
constraint_indexing:constraint_classification(straight_coercion_2025, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRC MILITARY COMMAND (ROPE) — Primary beneficiary. Exercises generate readiness, signaling, deterrence capability, and domestic political consolidation. From the PRC military perspective, the constraint solves a coordination problem: transforming political pressure into operationalized military readiness without formal declaration of intent. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05.
constraint_indexing:constraint_classification(straight_coercion_2025, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: UNITED STATES SECURITY ESTABLISHMENT (TANGLED ROPE) — Both benefits and bears costs. Benefits: justifies force presence, increases defense contractor demand, strengthens alliance politics. Costs: credibility exposure, escalation risk, resource commitment. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45.
constraint_indexing:constraint_classification(straight_coercion_2025, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL RULES-BASED ORDER (PITON) — The UN Convention on the Law of the Sea, freedom of navigation, and territorial integrity norms persist performatively while normalized military coercion proceeds. The rules are invoked rhetorically in diplomatic statements but functionally overridden by coercive practice. theater_ratio≈0.65 reflects this gap: formal rule-invocation ceremonies vs actual coercive operations. The piton classification reflects that the constraint was once enforced (Cold War deterrence regime) but now persists through institutional inertia.
constraint_indexing:constraint_classification(straight_coercion_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER - STRATEGIC NECESSITY (MOUNTAIN) — From a civilizational perspective, military coercion is an immutable feature of great-power competition: structural anarchy makes deterrence signaling inevitable. Geographic proximity makes Taiwan strategically inescapable for China. However, the structural data (ε=0.58, suppression=0.72, theater=0.65, requires_active_enforcement=true) contradicts the mountain classification. The engine detects this as a false summit: what appears as strategic inevitability is a contingent institutional arrangement (military budgets, political signaling cycles, coercive doctrines) not a law of nature.
constraint_indexing:constraint_classification(straight_coercion_2025, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(straight_coercion_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(straight_coercion_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(straight_coercion_2025, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(straight_coercion_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(straight_coercion_2025, TR),
    TR >= 0.70.

:- end_tests(straight_coercion_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The normalized coercion extracts significant economic and psychological costs from Taiwan (supply chain disruption, brain drain, investment uncertainty) and forces Taiwan and the US into reactive military posturing. However, extraction is not as severe as pure snare (ε≥0.66) because: (1) no complete blockade or kinetic action, (2) Taiwan retains some economic integration options, (3) US extended deterrence partially compensates. The value reflects accumulated coercive effect of normalized exercises without open conflict. Suppression (0.72): High. Taiwan has severely limited options to escape or mitigate coercion. The geographic constraint is immutable; military capability asymmetry is deep; economic integration creates additional dependencies; diplomatic channels are severely constrained. The only partial exit (US alliance, international democratic coalition) has low reliability (credibility is itself contested — omega #4). Theater ratio (0.65): Moderate-high. A significant portion of the exercise activity is performative: public announcements preceding exercises, military parade components, media coverage emphasizing intimidation over substantive operational testing. However, much of the activity is genuine training (ammunition expenditure, naval refueling operations, command coordination testing). The rising theater ratio (0.42 → 0.65) indicates that as exercises normalized, their performative component increased — the novelty and shock value faded, requiring greater theatrical emphasis to maintain psychological effect. This is a classic marker of Goodhart degradation: the metric (exercise intensity) was initially a genuine signal but now drives theatrical performance to compensate for signal degradation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. From Beijing's military perspective, it is a pure coordination mechanism (Rope): exercises embed political pressure into operational routine, solving the institutional problem of maintaining readiness while signaling resolve. From Taiwan's perspective, it is a snare: coercion with no coordination benefit, pure extraction. From the US perspective, it is tangled rope: both beneficiary (alliance deepening, deterrence justification) and victim (escalation exposure, credibility risk). From the international norms perspective, it is a piton: the constraint persists through institutional inertia (military budgets, alliance structures, force posture traditions) despite rules being nominally in place. From a civilizational analytical view, the constraint risks appearing as a mountain (strategic necessity) but the structural data reveals it as false summit: the coercion is contingent on political decisions (exercise timing, intensity, messaging) and institutional design (military command structures, budget cycles), not on immutable geopolitics. The perspectival gap is widest between Beijing's beneficiary view (rope) and Taiwan's victim view (snare) — separated by 0.63 on the d-scale (d≈0.08 vs d≈0.93), producing χ values differing by a factor of 12 (χ≈-0.05 vs χ≈0.59).
 *
 * DIRECTIONALITY LOGIC:
 *   Taiwan civilian population: Victim + trapped → d≈0.93, f(d)≈1.40, σ=0.9 → χ≈0.59. Maximal extraction. Taiwan government: Victim + constrained (some alliance options) → d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.54. High extraction but not maximal. PRC military: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Net beneficiary; experiences as coordination, not extraction. US security establishment: Mixed (organized power, mobile exit via disengagement, but reputational cost) → d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.45. Moderate mixed extraction-benefit. International norms: Nominal beneficiary (rules in place) but functionally overridden (victim of coercive precedent) → d≈0.60, f(d)≈0.85, σ=1.2 → χ≈0.66. Piton classification dominates despite χ approaching snare territory because theater_ratio (0.65) exceeds piton gate (0.70) and the functional enforcement has degraded.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION VIA NORMALIZATION PATHOLOGY: This constraint exhibits a specific mandatrophy risk: misclassifying normalized coercion as coordination (Mountain or Rope) when it is actually extraction (Snare). The mandatrophy arises from the temporal transformation of the constraint — it began as event-driven coercion (clearly a snare) but normalized into routine military activity (risk of misclassification as rope/mountain). The resolution requires temporal modeling: the extractiveness INCREASES from 0.35 (2020) to 0.58 (2025) because normalization deepens institutionalization of coercive practice. The theater ratio INCREASES from 0.42 to 0.65 because normalized exercises require greater performative emphasis to maintain psychological effect as the shock value decays. Both metrics point toward accumulating extraction, not toward stabilized coordination. The Tangled Rope classification (rather than pure Snare) acknowledges that some genuine coordination benefits exist (PRC readiness improvement, US alliance clarification, deterrence signaling) alongside the extraction (Taiwan population pressure, economic disruption). The mandatrophy is resolved by showing that the constraint is NOT a natural law (false mountain), is NOT pure coordination (not rope), and is NOT merely degraded ritual (not piton) — it is an actively enforced hybrid that combines coordination function (military readiness, deterrence) with asymmetric extraction (Taiwan bears concentrated costs while dispersed benefits accrue to PRC and secondary US benefits). The active enforcement gate is satisfied: the normalization of exercises requires continuous institutional commitment, political coordination among PRC civilian and military leadership, and sustained budgeting.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exercise_to_warhead_conversion_threshold,
    'At what point do normalized military exercises transition from coercive signaling to kinetic action preparation? What threshold of exercise intensity, frequency, or proximity triggers the conversion?',
    'Forward-looking: intelligence assessment of PRC military doctrine shifts, force posture indicators, command authority decentralization. Backward-looking: comparison of pre-conflict exercise patterns in historical cases (Crimea, Golan Heights) with current Taiwan Strait patterns.',
    'If threshold is high (ε remains 0.58): coercion remains in the snare/tangled_rope range. If threshold is low or ambiguous (ε jumps to 0.75+): the constraint shifts to high-intensity snare or undeclared war.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(exercise_to_warhead_conversion_threshold, empirical, 'Threshold for exercise-to-warhead conversion in escalation dynamics').

omega_variable(
    domestic_legitimacy_dependency,
    'How much of the normalized coercion schedule is driven by PRC domestic political cycles (CCP leadership consolidation, economic pressure relief) vs genuine military readiness requirements or deterrence signaling?',
    'Correlation analysis: exercise timing with domestic political events (Party meetings, economic data releases, leadership transitions). Internal documents (if available) disclosing the actual drivers of scheduling decisions.',
    'If domestic cycles dominate (>70%): the beneficiary relationship shifts entirely to PRC political leadership (not military). The constraint becomes primarily an internal PRC coordination mechanism that externalizes cost to Taiwan. If genuine military readiness drives (>70%): the extraction is justified by deterrence function, elevating the constraint toward rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domestic_legitimacy_dependency, empirical, 'Extent of PRC domestic political cycle influence on military exercise scheduling').

omega_variable(
    taiwan_economic_decoupling_reversibility,
    'Is the psychological and economic pressure on Taiwan (supply chain disruption, investor uncertainty, brain drain) reversible if military exercises cease, or has normalization created path-dependent institutional degradation?',
    'Economic data on Taiwan business formation, FDI inflows, talent retention in sectors sensitive to coercion. Psychological surveys on Taiwan domestic political cohesion and emigration intent as functions of exercise frequency.',
    'If reversible: the harm can be undone, and the snare classification is justified (temporary but real extraction). If path-dependent: Taiwan''s economic structure has adapted to permanent coercion, and the constraint has transitioned from snare (forcible extraction) to piton (institutional degradation disguised as stability).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taiwan_economic_decoupling_reversibility, empirical, 'Path-dependency and reversibility of Taiwan economic pressure').

omega_variable(
    us_extended_deterrence_commitment_credibility,
    'Do US security commitments to Taiwan (naval transits, weapons sales, security dialogue) remain credible deterrents, or have they become performative theater that signals resolve without delivering actual deterrent effect?',
    'Behavioral analysis: does PRC exercise intensity scale inversely with US deterrent signaling, or does it proceed independently? Do Taiwan decision-makers perceive US commitment as credible (survey), and does that perception correlate with exercise timing?',
    'If credible: US perspective as organized/mobile applies (χ≈0.45, tangled_rope). If performative: US deterrence becomes piton (theater_ratio rises, effectiveness declines), and the constraint shifts to pure PRC-Taiwan coercion (higher ε, higher χ for powerless agent).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(us_extended_deterrence_commitment_credibility, empirical, 'Credibility of US extended deterrence commitments to Taiwan').

omega_variable(
    normalization_irreversibility_lock_in,
    'Has the normalization of military coercion created political lock-in where either side cannot de-escalate without domestic political cost (loss of face, perceived weakness)?',
    'Political economy analysis: costs to PRC leadership of ceasing exercises (domestic hawk pressure, signaling weakness), costs to Taiwan of accepting coercion (domestic opposition to accommodation). Game-theoretic assessment of whether there exists a Pareto improvement equilibrium.',
    'If lock-in exists: the constraint becomes self-perpetuating regardless of intent. ε remains high. The snare persists not because PRC actively extracts but because both sides are trapped in a coordination failure. Constraint type shifts from snare (intentional extraction) to piton (inertial persistence despite mutual preference for lower intensity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_irreversibility_lock_in, conceptual, 'Political lock-in and reversibility of normalized coercion patterns').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(straight_coercion_2025, 2020, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(strait_theater_2020, straight_coercion_2025, theater_ratio, 0, 0.42).
narrative_ontology:measurement(strait_theater_2023, straight_coercion_2025, theater_ratio, 3, 0.55).
narrative_ontology:measurement(strait_theater_2025, straight_coercion_2025, theater_ratio, 5, 0.65).

% Extraction over time
narrative_ontology:measurement(strait_extractiveness_2020, straight_coercion_2025, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(strait_extractiveness_2023, straight_coercion_2025, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(strait_extractiveness_2025, straight_coercion_2025, base_extractiveness, 5, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(straight_coercion_2025, enforcement_mechanism).
narrative_ontology:affects_constraint(straight_coercion_2025, taiwan_semiconductor_supply_chain).
narrative_ontology:affects_constraint(straight_coercion_2025, us_indo_pacific_alliance_structure).
narrative_ontology:affects_constraint(straight_coercion_2025, prc_domestic_political_legitimacy).

% DUAL FORMULATION NOTE:
% The normalized Taiwan Strait coercion should be decomposed into three analytically distinct constraints if further precision is needed: (1) Exercise Scheduling Normalization (how military readiness cycles decouple from political signaling) — ε≈0.25, Rope; (2) Coercive Signaling (how normalized exercises maintain psychological pressure on Taiwan) — ε≈0.68, Snare; (3) US Extended Deterrence Theater (how US security commitments become performative) — ε≈0.52, Piton. The current story unifies these at ε=0.58, Tangled Rope, treating them as aspects of a single institutional constraint. The affects_constraints network links to upstream constraints (what drives PRC political legitimacy demand) and downstream constraints (what happens to Taiwan supply chains and US alliance structures when coercion persists).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(straight_coercion_2025, institutional, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
