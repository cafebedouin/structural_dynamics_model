% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__strict_geographic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__strict_geographic_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_maritime_sovereignty__strict_geographic_reading
 *   human_readable: UNCLOS Strict Geographic Reading: Natural Features Only for Territorial Sea and EEZ
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint is the strict geographic reading of the UNCLOS maritime
 *   sovereignty kernel: only naturally formed features above water at high
 *   tide generate territorial sea and EEZ, while artificial constructions are
 *   legally classified as installations devoid of such entitlement. It
 *   operates as a formalized legal rule within the UNCLOS commitment system,
 *   enforced through international tribunals and freedom-of-navigation
 *   operations. Though presented as a neutral application of physical
 *   geography, the reading creates a structurally asymmetric distribution of
 *   maritime rights: naval powers and non-claimant states benefit from
 *   constrained expansion, while coastal states engaged in artificial
 *   island-building bear the cost of denied sovereignty. This is one of three
 *   contested readings of the same kernel; the strict reading forecloses its
 *   siblings by treating artificial construction as jurisdictionally
 *   irrelevant.
 *
 * KEY AGENTS:
 *   - naval_powers: Primary beneficiary (powerful/arbitrage) â gains freedom of navigation and strategic access
 *   - non_claimant_states: Secondary beneficiary (moderate/constrained) â gains maritime boundary stability
 *   - expansionist_coastal_states: Primary target (powerful/constrained) â bears the cost of denied EEZ/territorial sea from artificial installations
 *   - international_maritime_judiciary: Agenda setter (institutional/analytical) â administers and interprets Article 121
 *   - law_of_the_sea_scholars: Analytical observer (analytical/analytical) â provides interpretive framework
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, 0.48).
domain_priors:suppression_score(unclos_maritime_sovereignty__strict_geographic_reading, 0.58).
domain_priors:theater_ratio(unclos_maritime_sovereignty__strict_geographic_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__strict_geographic_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__strict_geographic_reading, tangled_rope).
narrative_ontology:human_readable(unclos_maritime_sovereignty__strict_geographic_reading, "UNCLOS Strict Geographic Reading: Natural Features Only for Territorial Sea and EEZ").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__strict_geographic_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__strict_geographic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__strict_geographic_reading, 'e62651b2-1824-4793-8988-edbb06aecc88').
narrative_ontology:cs_kernel_codification('e62651b2-1824-4793-8988-edbb06aecc88', formalized).
narrative_ontology:cs_authority_grounding('e62651b2-1824-4793-8988-edbb06aecc88', lineage).
narrative_ontology:cs_interpretation_layer_present('e62651b2-1824-4793-8988-edbb06aecc88').
narrative_ontology:cs_reading_relation('e62651b2-1824-4793-8988-edbb06aecc88', unclos_maritime_sovereignty__expansive_construction_reading, forecloses).
narrative_ontology:cs_reading_relation('e62651b2-1824-4793-8988-edbb06aecc88', unclos_maritime_sovereignty__hybrid_effective_control_reading, forecloses).
narrative_ontology:cs_axiom('e62651b2-1824-4793-8988-edbb06aecc88', foundational, natural_formation_dispositive).
narrative_ontology:cs_axiom_status(natural_formation_dispositive, holdable).
narrative_ontology:cs_axiom_grounding('e62651b2-1824-4793-8988-edbb06aecc88', natural_formation_dispositive, conventional).
narrative_ontology:cs_axiom('e62651b2-1824-4793-8988-edbb06aecc88', foundational, artificial_features_lack_jurisdictional_capacity).
narrative_ontology:cs_axiom_status(artificial_features_lack_jurisdictional_capacity, holdable).
narrative_ontology:cs_axiom_grounding('e62651b2-1824-4793-8988-edbb06aecc88', artificial_features_lack_jurisdictional_capacity, conventional).
narrative_ontology:cs_reference_frame('e62651b2-1824-4793-8988-edbb06aecc88', natural_feature_sovereignty_baseline).
narrative_ontology:cs_drift_state('e62651b2-1824-4793-8988-edbb06aecc88', post_scs_arbitration_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('e62651b2-1824-4793-8988-edbb06aecc88', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__strict_geographic_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain global naval presence and operational freedom in waters that would be restricted if artificial installations generated territorial seas. They enforce the strict geographic reading through freedom of navigation operations and diplomatic pressure, and they benefit from unobstructed passage without negotiating transit rights with coastal states.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, naval_powers, beneficiary,
    powerful, generational, arbitrage, global).

% Coastal states without offshore expansion claims that rely on stable maritime boundaries. They benefit from preventing neighboring claimants from converting artificial installations into expanded exclusive economic zones that would encroach on their own maritime access.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, non_claimant_states, beneficiary,
    moderate, generational, constrained, regional).

% Coastal states that invest in land reclamation and construction on submerged features or low-tide elevations to extend sovereign control. The strict geographic reading classifies these installations as non-islands, denying them territorial sea and exclusive economic zone status and blocking maritime expansion.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, expansionist_coastal_states, payer,
    powerful, generational, constrained, regional).

% Arbitration panels and tribunals that interpret the United Nations Convention on the Law of the Sea, particularly Article 121, applying geological and hydrological evidence to determine whether a feature is naturally formed and above water at high tide. Their rulings bind disputing parties and establish precedential pressure on subsequent claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, international_maritime_judiciary, agenda_setter,
    institutional, generational, analytical, global).

% Academic and legal experts who analyze state practice, tribunal decisions, and negotiating history to construct interpretive frameworks for maritime entitlement. They operate outside the enforcement apparatus but shape how states and courts understand the geographic criterion.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__strict_geographic_reading, law_of_the_sea_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_maritime_sovereignty__strict_geographic_reading, diffuse).
narrative_ontology:fixing_cost_class(unclos_maritime_sovereignty__strict_geographic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral territorial expansion through artificial construction in maritime spaces, preserving a predictable allocation of ocean jurisdiction based on geographically fixed features rather than engineering capacity or effective occupation.
% TRANSFER_FUNCTION: Transfers sovereign maritime entitlement away from expansionist coastal states that build artificial installations and toward the community of states benefitting from open navigation and stable boundaries, by denying such installations territorial sea or exclusive economic zone status.
% ABSENT_VOICES: Marine construction industries and engineering firms that build artificial islands are excluded from the legal determination; small island developing states that might use artificial territory to secure maritime zones lack representation in the interpretive process; the submerged ecosystems being built over have no procedural standing.
% DISAPPEARANCE_RATIONALE: If the strict geographic reading vanished and artificial construction could generate territorial sea and exclusive economic zone, coastal states with engineering capacity would rapidly construct features on every bank and reef, fragmenting maritime jurisdiction, triggering militarized disputes, and collapsing the stability of the Law of the Sea regime.
% FOUNDING_PROBLEM: Mid-twentieth century fear that technologically advanced states could manufacture territory to claim vast ocean areas, destabilizing international order and threatening freedom of navigation.
% FOUNDING_PROBLEM_CORROBORATION: Independent law-of-the-sea historians and maritime security scholars from non-naval-power states corroborate that manufactured territorial claims were a genuine concern during UNCLOS negotiations. Expansionist coastal states argue the problem is now a pretext for preserving Western naval hegemony. Neutral academic observers note that the geographic criterion was advanced by maritime powers during drafting, indicating distributive intent alongside coordination function.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__strict_geographic_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__strict_geographic_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unclos_maritime_sovereignty__strict_geographic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__strict_geographic_reading, 0.48, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__strict_geographic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__strict_geographic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is authored at 0.48 because the constraint asymmetrically strips sovereign maritime entitlement from states that invest in artificial construction, transferring the value of those waters to the community of states upholding freedom of navigation. Suppression (0.58) reflects active enforcement: tribunal rulings, diplomatic pressure, and naval operations are required to prevent expansionist states from treating artificial islands as territorial bases. Theater ratio (0.25) is modest; the legal formalism is largely functional, though ritualized tribunal proceedings add performative overhead. Accessibility collapse (0.60) captures the closure of legal alternatives once a state accepts the UNCLOS framework; resistance (0.50) reflects sustained non-compliance and rhetorical rejection by targeted states. The metrics and claim are authored independently: the claimed type is tangled_rope because the rule coordinates maritime space while enforcing an asymmetric extraction, and the metrics describe that operational reality without tuning toward the claim.
 *
 * PERSPECTIVAL GAP:
 *   The naval power seat experiences the constraint as a coordination mechanism that prevents anarchic territorial manufacture; the expansionist coastal state seat experiences it as an externally imposed deprivation of development rights. The non-claimant state experiences near-symmetric benefit. The engine computes this divergence from beneficiary declarations and exit options: naval powers have arbitrage-grade exit (they can operate outside the treaty if needed), while expansionist states are constrained within the regime.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers are named beneficiaries with high mobility and global scope, placing their directionality near the subsidy end (low d). Non-claimant states are also beneficiaries but with more constrained exit, placing them slightly higher but still below symmetric. Expansionist coastal states are named victims with constrained exit and regional scope, placing their directionality near the full-target end (high d), amplifying effective extraction. The international maritime judiciary is analytical and excluded from the cost/benefit flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâpreventing technologically advanced states from manufacturing territory to claim vast ocean areasâremains contested. Expansionist states argue the problem is overstated, while naval powers treat it as live. Because beneficiary seats actively maintain enforcement (tribunals, FONOPs), the constraint has not decayed into a piton. Were enforcement to lapse while the rule persisted, the classification would drift toward piton; currently it is a contested, actively enforced tangled rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_rule,
    'Is the strict geographic criterion a neutral reflection of physical geography, or a constructed legal rule encoding a specific distribution of maritime power?',
    'Archival analysis of UNCLOS negotiating history to determine whether the natural-feature requirement was selected for its neutrality or to privilege states with large navies and distant-water fleets.',
    'If the criterion was constructed to advantage naval powers, the constraint''s extraction is politically motivated and the mountain-like framing collapses into tangled_rope or snare; if neutral, the rope/coordination framing strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_rule, conceptual, 'Ambiguity between neutral geographic fact and politically constructed legal rule').

omega_variable(
    enforcement_compliance_gap,
    'Does the strict reading effectively constrain powerful expansionist states, or does it operate as legal theater when those states ignore tribunal rulings?',
    'Longitudinal tracking of artificial island construction and territorial claims in the South China Sea post-2016 arbitration to measure behavioral compliance versus rhetorical rejection.',
    'If powerful targets systematically ignore the constraint, suppression is lower than authored, theater_ratio rises, and the effective extraction is concentrated on weaker expansionist states that lack the power to defy rulings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_compliance_gap, empirical, 'Gap between legal enforcement and actual state compliance').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of expansionist claims structural (tribunal rulings, sanctions, naval presence) or internalized (normative acceptance of the geographic criterion by coastal elites)?',
    'Observe whether expansionist states abandon claims after tribunal rulings without material coercion, indicating internalization; if claims persist and require active naval pressure to suppress, suppression is purely structural.',
    'If internalized, the constraint''s effective suppression is higher than structural measures suggest; if purely structural, the constraint persists only through costly enforcement and is more fragile.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__strict_geographic_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t0, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(uncl_tr_t8, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(uncl_tr_t16, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 16, 0.18).
narrative_ontology:measurement(uncl_tr_t24, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(uncl_tr_t32, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 32, 0.24).
narrative_ontology:measurement(uncl_tr_t40, unclos_maritime_sovereignty__strict_geographic_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(uncl_be_t0, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(uncl_be_t8, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 8, 0.28).
narrative_ontology:measurement(uncl_be_t16, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 16, 0.35).
narrative_ontology:measurement(uncl_be_t24, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 24, 0.42).
narrative_ontology:measurement(uncl_be_t32, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(uncl_be_t40, unclos_maritime_sovereignty__strict_geographic_reading, base_extractiveness, 40, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t0, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(uncl_su_t8, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 8, 0.35).
narrative_ontology:measurement(uncl_su_t16, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 16, 0.45).
narrative_ontology:measurement(uncl_su_t24, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(uncl_su_t32, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(uncl_su_t40, unclos_maritime_sovereignty__strict_geographic_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__strict_geographic_reading, resource_allocation).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, expansive_construction_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__strict_geographic_reading, hybrid_effective_control_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the unclos_maritime_sovereignty family. The kernel 'maritime sovereignty from islands' decomposes into three structurally distinct readings because the legal effect of artificial construction is contested. Each reading has a different epsilon, beneficiary/victim structure, and classification. This strict reading has the lowest epsilon of the three but still extracts asymmetrically from expansionist states.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
