% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__deterrence_unthinkable
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__deterrence_unthinkable, []).

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
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_winnability_post_1945__deterrence_unthinkable
 *   human_readable: Nuclear Deterrence and the Incoherence of Victory Planning (Deterrence-Unthinkable Reading)
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint instantiates the 'deterrence_unthinkable' reading of the
 *   war_winnability_post_1945 kernel. It holds that nuclear weapons have made
 *   great-power total war categorically unwinnable — not merely very costly
 *   or unacceptable, but logically incoherent in its strategic structure.
 *   Victory in nuclear war is not a thinkable objective because mutual
 *   arsenals ensure that both sides' core objectives (survival of the
 *   nation-state, preservation of its civilization) are destroyed
 *   simultaneously. The constraint operates as a cognitive and institutional
 *   reordering: military establishments are tasked with planning wars they
 *   cannot win, strategic doctrine declares mutual vulnerability as
 *   permanent, and civilian populations are held hostage to the credibility
 *   of deterrence. The reading differs from its siblings:
 *   'countervailing_thinkable' permits limited nuclear war and constrained
 *   victory through counterforce strategies; 'rhetorical_contraction'
 *   preserves operational victory-planning while rendering it unsayable in
 *   public discourse. This reading makes winnability not forbidden or
 *   unstated, but structurally impossible. The structural delta is
 *   operational contraction (winnability exits the reachable strategy space
 *   entirely) and doctrinal shift (strategic planning becomes war prevention
 *   rather than war-winning). Beneficiary: civilian populations and the
 *   strategic stability architecture that prevents great-power war. Victim:
 *   military operational planning coherence and the strategic doctrine that
 *   once organized military establishments around victory objectives.
 *
 * KEY AGENTS:
 *   - Civilian Populations: Primary beneficiary (powerless/trapped) — benefit from prevention of nuclear war but cannot exit the deterrence relationship; survival contingent on deterrent credibility
 *   - Military Operational Planners: Primary victim (moderate/constrained) — tasked with planning wars declared unwinnable; professional coherence extracted
 *   - Strategic Stability Coalition: Beneficiary/Enforcer (institutional/constrained) — nuclear powers, treaty regimes, arms control verification bodies benefit from stability and maintain deterrence through enforcement
 *   - Great-Power Military Establishments: Victim (institutional/arbitrage) — mission structure shifts from victory-seeking to deterrence-maintaining; institutional identity requires redefinition
 *   - Non-Nuclear Allies and Regional Powers: Secondary beneficiary (institutional/arbitrage) — receive deterrent coverage without costs of nuclear weapons development
 *   - Analytical Observer: Civilization-scale view (analytical/analytical) — risks treating contingent institutional arrangements (mutual vulnerability acceptance) as immutable natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, 0.68).
domain_priors:suppression_score(war_winnability_post_1945__deterrence_unthinkable, 0.72).
domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, extractiveness, 0.68).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__deterrence_unthinkable, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__deterrence_unthinkable, "Nuclear Deterrence and the Incoherence of Victory Planning (Deterrence-Unthinkable Reading)").
narrative_ontology:topic_domain(war_winnability_post_1945__deterrence_unthinkable, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__deterrence_unthinkable).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__deterrence_unthinkable, '8f63a9ff-c293-4f3b-97c7-ffd96dc2c672').
narrative_ontology:cs_kernel_codification('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', formalized).
narrative_ontology:cs_authority_grounding('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', extraction).
narrative_ontology:cs_interpretation_layer_present('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672').
narrative_ontology:cs_reading_relation('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', war_winnability_post_1945__countervailing_thinkable, forecloses).
narrative_ontology:cs_reading_relation('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', war_winnability_post_1945__rhetorical_contraction, coexists_with).
narrative_ontology:cs_axiom('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', foundational, mutual_annihilation_eliminates_victory_concept).
narrative_ontology:cs_axiom_status(mutual_annihilation_eliminates_victory_concept, holdable).
narrative_ontology:cs_axiom_grounding('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', mutual_annihilation_eliminates_victory_concept, empirically_contingent).
narrative_ontology:cs_axiom('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', foundational, strategic_doctrine_coherence_requires_thinkable_victory).
narrative_ontology:cs_axiom_status(strategic_doctrine_coherence_requires_thinkable_victory, overridden).
narrative_ontology:cs_axiom_grounding('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', strategic_doctrine_coherence_requires_thinkable_victory, deontological).
narrative_ontology:cs_reference_frame('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', mutual_assured_destruction_framework).
narrative_ontology:cs_drift_state('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', contemporary_great_power_competition, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f63a9ff-c293-4f3b-97c7-ffd96dc2c672', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, civilian_populations).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__deterrence_unthinkable, strategic_stability_architecture).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, military_operational_planning_coherence).
narrative_ontology:constraint_victim(war_winnability_post_1945__deterrence_unthinkable, great_power_strategic_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN POPULATIONS (SNARE) — Trapped in a structure where war winnability has been removed from strategic calculation, yet the capacity to inflict catastrophic harm remains. Cannot exit the deterrence relationship; survival depends entirely on the credibility of mutual vulnerability. Maximum suppression: any attempt to 'think differently' about nuclear strategy risks destabilizing the deterrent itself. The constraint extracts a cognitive cost — the permanent psychological burden of living under conditions where total war is possible but victory is nonsensical.
constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: STRATEGIC STABILITY COALITION (TANGLED ROPE) — Organized actors (nuclear powers, arms control regimes, non-proliferation treaties) benefit from the constraint: the incoherence of victory planning PREVENTS arms races that would destabilize mutual deterrence. The constraint also requires active enforcement — continuous verification, treaty maintenance, capability development that signals 'we can still destroy you' without claiming winnability. Extraction exists: arms control imposes verification burdens and constrains procurement flexibility. But genuine coordination function persists: preventing escalation spirals and accidentally-triggered war.
constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MILITARY OPERATIONAL PLANNERS (SNARE) — Face an incoherence at their professional core: they are tasked with planning war while being told that winning is impossible. The constraint suppresses alternative strategic narratives ('limited nuclear war', 'damage limitation', 'counterforce victory'). Exit options are constrained: renouncing the planning mission means renouncing their institutional role. High experienced extraction — the coherence of their profession has been extracted. They can plan tactical responses but not strategic victory. This is the reading's primary operational victim.
constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: COLD WAR STRATEGIC DISCOURSE (PITON) — The doctrinal language of deterrence ('mutual assured destruction', 'second-strike capability', 'strategic stability') persists as a performative framework. The substance — actual military planning for victory — has atrophied. Institutional actors use the ritualized vocabulary to maintain legitimacy while the underlying operational coherence has degraded. Theater ratio is moderate (0.58) because some real coordination occurs (arms control verification, transparency measures), but much of the doctrinal apparatus is maintained through institutional inertia rather than functional necessity.
constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: NON-NUCLEAR POWERS AND ALLIED STATES (ROPE) — Benefit from the constraint as pure coordination: the incoherence of nuclear victory planning prevents great-power total war that would incinerate regional stakeholders. The deterrent functions as a public good — stability provided without direct negotiation. Exit options remain available (military non-alignment, regional security partnerships), but the constraint provides coordination function with minimal coercive overhead. This perspective experiences the constraint as unambiguous benefit.
constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW READING (MOUNTAIN) — From a civilizational perspective, the constraint appears as an immutable physical and logical law: thermonuclear arsenals create a domain (mutual assured destruction of cities) where the concept of 'victory' becomes analytically incoherent. Victory requires achieving your objectives at acceptable cost; if all objectives (including 'your own survival') vanish in mutual annihilation, the concept collapses to logical nonsense. This reading treats the constraint as emerging naturally from physics and mathematics, not from strategic choice. However, this naturalizes what is actually a contingent institutional reading — the constraint exists only when actors accept the mutual vulnerability frame.
constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(war_winnability_post_1945__deterrence_unthinkable, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__deterrence_unthinkable, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(war_winnability_post_1945__deterrence_unthinkable, TR),
    TR >= 0.70.

:- end_tests(war_winnability_post_1945__deterrence_unthinkable_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The constraint extracts coherence from military planning establishments and imposes a cognitive burden on civilian populations (permanent uncertainty about deterrent credibility). The extractiveness is not maximal (not 0.85+) because genuine coordination occurs: arms control regimes function, deterrence theory produces operational guidance, and the stability architecture delivers a real public good (absence of great-power war 1945-present). The extraction grows over time (0.42→0.68) as doctrinal coherence degrades further and the psychological burden of perpetual vulnerability deepens. Suppression (0.72): High. Significant barriers exist to rejecting the mutual vulnerability frame: nuclear arsenals cannot be un-invented, first-strike advantage disappears as arsenals saturate, and any single power's unilateral attempt to escape deterrence destabilizes the entire architecture. The suppression persists through technological facts (each power's retaliatory capacity is assured) and institutional lock-in (nuclear doctrine is embedded in command structures, alliance commitments, strategic planning bureaucracies). Theater ratio (0.58): Moderate-high. Strategic deterrence doctrine performs a coordination function (arms control verification, transparency, confidence-building measures) but also maintains substantial performative content (strategic rhetoric, doctrinal declarations, ceremony of nuclear command). The theater is higher than in early Cold War (1950s, when nuclear threats were more explicit) because institutional actors have learned that performative restraint increases deterrent credibility. The rising trajectory (0.45→0.58) reflects increasing theatrical maintenance of the deterrence frame as operational victory-planning has become progressively unspeakable.
 *
 * PERSPECTIVAL GAP:
 *   This reading generates stark perspectival divergence. Civilian populations experience the constraint as existential trap (Snare) — survival depends on system they cannot control. Military planners experience it as professional incoherence (Snare) — tasked with planning impossible wars. The strategic stability coalition experiences it as functional coordination with enforcement costs (Tangled Rope) — genuine stability produced, but verification and capability maintenance required. Non-nuclear allies experience it as pure public good (Rope) — deterrent coverage without participation costs. The Cold War doctrinal system experiences it as degraded ritual (Piton) — the apparatus persists through institutional momentum while the core function has atrophied. The civilizational analytical observer risks seeing immutable natural law (Mountain) — thermonuclear physics and mathematics make victory incoherent. But this naturalizes the contingent institutional choice to accept mutual vulnerability as permanent. Actors attempting to escape mutual vulnerability (through missile defense, counterforce doctrine, first-strike capability claims) have never restored coherence to victory-planning; they have only produced denial. This perspectival range validates the tangled_rope classification: the constraint both coordinates and extracts, serves legitimate functions while imposing real costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies dramatically by position. Civilian populations are maximal targets: trapped without exit (d→0.95), experiencing full extraction cost. Military planners are moderate victims: constrained exit (career cost to exit), experiencing extraction of professional coherence but retaining some agency in operational planning (d→0.70). Strategic stability coalition members are beneficiaries: arbitrage exit available (can shift strategies), benefit from deterrence stability and coordination (d→0.15). Non-nuclear allies are beneficiaries with arbitrage: mobile exit (can develop independent deterrents if they choose), receive public good from deterrence (d→0.10). The engine derives these d values from the beneficiary/victim declarations and exit options; the perspectival gap in chi emerges from directionality variation, not from different epsilon values. This reading's ε (0.68) is constant across perspectives; what changes is f(d), which scales experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy of nuclear deterrence is resolved by recognizing that the constraint exhibits genuine tangled_rope structure: it coordinates stable deterrence (preventing great-power war for 80+ years) while extracting a real cost (military doctrinal incoherence, perpetual civilian vulnerability). The readings are not confused classifications of the same thing — they are genuinely different structural claims about what has happened to war winnability. The deterrence_unthinkable reading holds that winnability has become logically impossible (exits the strategy space). The countervailing_thinkable reading holds that winnability persists at limited scales. The rhetorical_contraction reading holds that winnability remains operationally thinkable but became unsayable. These are not alternate perspectives on one phenomenon; they are competing claims about the structure of the constraint itself. The deterrence_unthinkable reading resolves mandatrophy by declaring victory incoherent: planning for victory is not just forbidden or strategically irrational, but undefined. This precludes the countervailing reading from occupying the same framework.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    victory_coherence_threshold,
    'At what level of nuclear arsenal size does victory-planning become logically incoherent rather than merely high-risk?',
    'Game-theoretic analysis of cost-benefit thresholds in counterforce targeting; empirical assessment of how many cities/population centers must be destroyed before ''victory'' becomes mathematically undefined',
    'If threshold is very low (small arsenals): many historical periods should classify as ''deterrence unthinkable'', not just post-1945. If threshold requires near-total arsenals: early nuclear period (1945-1965) may permit ''countervailing thinkable'' classification despite declared deterrence doctrine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victory_coherence_threshold, empirical, 'Threshold at which victory planning becomes logically incoherent').

omega_variable(
    countervailing_doctrine_resilience,
    'Is the persistence of ''countervailing strategy'' and ''damage limitation'' doctrines (1974-present) evidence that victory-planning remained operationally thinkable despite declared deterrence postures, or merely performative hedging against deterrence failure?',
    'Analysis of declarative doctrine vs operational war plans (where accessible); assessment of whether counterforce targeting strategies represent genuine victory-seeking or risk-reduction within deterrence framework',
    'If countervailing doctrines represent genuine victory-seeking: the ''deterrence unthinkable'' reading is aspirational rather than descriptive — military establishments never fully accepted the incoherence. If performative: reading is confirmed — doctrines exist as institutional hedges while fundamental incoherence holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(countervailing_doctrine_resilience, empirical, 'Whether countervailing doctrines represent genuine victory-planning or deterrence risk-reduction').

omega_variable(
    reading_boundary_collapse,
    'At what point does the rhetorical_contraction reading (winnability unsayable but operationally planned) become empirically indistinguishable from the deterrence_unthinkable reading (winnability incoherent)?',
    'Longitudinal discourse analysis: track when military doctrinal language shifts from ''victory-achievable-but-unacceptable'' to ''victory-undefined''. Identify decision points where the incoherence moves from rhetorical taboo to structural impossibility.',
    'If indistinguishable: the readings may represent different theoretical framings of the same empirical phenomenon. If clearly distinguished: rhetorical contraction becomes a leading indicator of operational shift toward true deterrence adoption.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_collapse, conceptual, 'Boundary between rhetorical contraction and operational incoherence').

omega_variable(
    mutual_vulnerability_frame_contingency,
    'Is the constraint dependent on rational actors accepting the mutual vulnerability frame, or does it hold even when actors reject or attempt to escape that frame?',
    'Analysis of strategic behavior during periods of attempted asymmetry (missile defense programs, first-strike doctrine claims, counterforce modernization). Assessment of whether rejection of mutual vulnerability produces strategic coherence or merely denial.',
    'If frame-dependent: the constraint is institutional/cognitive rather than natural law. If frame-independent: nuclear physics and mathematics alone generate the incoherence regardless of strategic intention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mutual_vulnerability_frame_contingency, empirical, 'Whether the constraint depends on accepting the mutual vulnerability frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__deterrence_unthinkable, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(winn_unthinkable_theater_t0, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 0, 0.45).
narrative_ontology:measurement(winn_unthinkable_theater_t20, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 20, 0.52).
narrative_ontology:measurement(winn_unthinkable_theater_t40, war_winnability_post_1945__deterrence_unthinkable, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(winn_unthinkable_extract_t0, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(winn_unthinkable_extract_t20, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(winn_unthinkable_extract_t40, war_winnability_post_1945__deterrence_unthinkable, base_extractiveness, 40, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(winn_unthinkable_suppress_t0, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 0, 0.64).
narrative_ontology:measurement(winn_unthinkable_suppress_t20, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(winn_unthinkable_suppress_t40, war_winnability_post_1945__deterrence_unthinkable, suppression_requirement, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__deterrence_unthinkable, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__countervailing_thinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, war_winnability_post_1945__rhetorical_contraction).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, extended_deterrence_credibility).
narrative_ontology:affects_constraint(war_winnability_post_1945__deterrence_unthinkable, nuclear_proliferation_prevention).

% DUAL FORMULATION NOTE:
% The war_winnability_post_1945 kernel decomposes into three structurally distinct constraints: deterrence_unthinkable (winnability logically impossible), countervailing_thinkable (winnability constrained but achievable), and rhetorical_contraction (winnability operationally planned but unsayable). Each has different epsilon values and different victim/beneficiary structures. The deterrence_unthinkable reading treats winnability as exiting the strategy space entirely (operation contraction). This reading is upstream of extended_deterrence_credibility (which depends on accepting that nuclear war is unwinnable) and nuclear_proliferation_prevention (which treats non-proliferation as necessary because winnability is incoherent for all nuclear-armed states).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(war_winnability_post_1945__deterrence_unthinkable, moderate, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
