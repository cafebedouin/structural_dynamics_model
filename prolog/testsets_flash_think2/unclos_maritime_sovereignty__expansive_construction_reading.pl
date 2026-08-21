% ============================================================================
% CONSTRAINT STORY: unclos_maritime_sovereignty__expansive_construction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_maritime_sovereignty__expansive_construction_reading, []).

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
 *   constraint_id: unclos_maritime_sovereignty__expansive_construction_reading
 *   human_readable: Expansive Construction of Maritime Sovereignty (UNCLOS Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint describes the 'expansive construction' reading of UNCLOS
 *   maritime sovereignty, where artificial island construction on submerged
 *   features or low-tide elevations is asserted to generate de facto
 *   territorial waters through effective occupation and administrative
 *   control. This is one reading of the contested
 *   'unclos_maritime_sovereignty' kernel, distinct from
 *   'strict_geographic_reading' and 'hybrid_effective_control_reading'. This
 *   reading is characterized by high extraction and suppression, as it
 *   involves unilateral claims over contested maritime space.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.85).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.9).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction of Maritime Sovereignty (UNCLOS Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '73eb0ea2-dab2-4998-aeca-cf15c45b2a5b').
narrative_ontology:cs_kernel_codification('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', fixed_text).
narrative_ontology:cs_authority_grounding('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', extraction).
narrative_ontology:cs_interpretation_layer_present('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b').
narrative_ontology:cs_reading_relation('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', unclos_maritime_sovereignty__hybrid_effective_control_reading, coexists_with).
narrative_ontology:cs_axiom('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', foundational, effective_occupation_generates_sovereignty).
narrative_ontology:cs_axiom_status(effective_occupation_generates_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', effective_occupation_generates_sovereignty, conventional).
narrative_ontology:cs_axiom('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', foundational, artificial_features_can_be_islands).
narrative_ontology:cs_axiom_status(artificial_features_can_be_islands, holdable).
narrative_ontology:cs_axiom_grounding('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', artificial_features_can_be_islands, conventional).
narrative_ontology:cs_reference_frame('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', unilateral_maritime_expansion_doctrine).
narrative_ontology:cs_drift_state('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', contemporary_geopolitical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73eb0ea2-dab2-4998-aeca-cf15c45b2a5b', '').
narrative_ontology:cs_kernel_id(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states).
narrative_ontology:constraint_victim(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states actively engage in artificial island construction on submerged features or low-tide elevations, asserting de facto territorial waters and exclusive economic zones. They benefit from expanded strategic control, resource access, and geopolitical leverage. Their exit options are high because they can choose to continue or cease construction, or selectively enforce claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These states have existing or potential maritime claims overlapping with the newly asserted zones. They bear the costs of lost access to resources, increased military presence, and diplomatic pressure. Their exit options are constrained by their geographic proximity and the need to defend their own sovereignty.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    institutional, generational, constrained, regional).

% These states advocate for unimpeded passage through international waters and airspace. They bear the costs of increased friction, potential military confrontations, and the erosion of international maritime law. While they can choose to avoid contested areas, their core interest is maintaining global freedom of navigation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, generational, mobile, global).

% These bodies interpret UNCLOS and adjudicate disputes, but their authority is often challenged or ignored by states pursuing expansive claims. They observe the unfolding legal and geopolitical contest without direct enforcement power.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_maritime_tribunals, observer,
    institutional, generational, analytical, global).

% Administers the UN Convention on the Law of the Sea, providing technical and legal support. It observes state practice and legal interpretations but does not have enforcement authority over sovereign claims.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, unclos_secretariat, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading attempts to coordinate the expansion of national maritime claims by establishing a precedent for effective occupation and administrative control over constructed features, thereby reducing ambiguity for the constructing state.
% TRANSFER_FUNCTION: Transfers de facto sovereignty, control over marine resources (fishing, mineral rights), and strategic military advantage from the international commons or neighboring states to the island-constructing states.
% ABSENT_VOICES: Small island developing states and landlocked states, who lack the resources or geographic position to engage in such construction, are largely absent from the high-level geopolitical and legal debates, despite being disproportionately affected by the erosion of established maritime law.
% DISAPPEARANCE_RATIONALE: If this interpretation and its enforcement vanished overnight, the constructed features would revert to their original legal status (submerged features or low-tide elevations), the asserted territorial waters would dissolve, and the geopolitical landscape of maritime claims would be fundamentally reorganized, likely leading to a reduction in regional tensions and a reassertion of traditional UNCLOS interpretations.
% FOUNDING_PROBLEM: The ambiguity within UNCLOS regarding the legal status and sovereignty-generating capacity of artificial features constructed on submerged or low-tide elevations, particularly in strategically important or resource-rich areas.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, international relations experts, and non-aligned states corroborate that this ambiguity is actively exploited by constructing states. The ongoing diplomatic protests and freedom of navigation operations by other states further attest to the live nature of this contested interpretation, rather than a settled legal principle.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_maritime_sovereignty__expansive_construction_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_maritime_sovereignty__expansive_construction_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_maritime_sovereignty__expansive_construction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the significant strategic and resource gains for constructing states, at the expense of others. Suppression (0.90) is very high because these claims are actively enforced through military presence and administrative control, suppressing alternative interpretations and freedom of navigation. The theater ratio (0.40) indicates a substantial performative aspect of asserting sovereignty, though genuine administrative control is also present. Accessibility collapse (0.80) is high as these claims effectively close off large maritime areas. Resistance (0.75) is also high, evidenced by diplomatic protests and freedom of navigation operations.
 *
 * PERSPECTIVAL GAP:
 *   Island-constructing states perceive this as a legitimate exercise of sovereign rights, interpreting UNCLOS broadly to allow for such claims. Neighboring claimant states and freedom-of-navigation states perceive it as an aggressive, extractive land-grab that undermines international law and stability. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are clear beneficiaries (agenda_setter, arbitrage exit), gaining territory and resources. Neighboring claimant states and freedom-of-navigation states are victims (payer, constrained/mobile exit), losing access and facing increased geopolitical risk. International tribunals and the UNCLOS secretariat are observers, analyzing the situation without direct benefit or cost from this specific constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_status_of_artificial_features,
    'Does international law, specifically UNCLOS, permit artificial features constructed on submerged elevations to generate territorial seas and EEZs, or only limited safety zones?',
    'A definitive ruling by the International Court of Justice or the International Tribunal for the Law of the Sea, accepted by all parties, or a new international convention clarifying the status.',
    'If only safety zones are permitted, the measured extraction and suppression of this reading would be reclassified as illegitimate and highly extractive, shifting its classification more firmly towards a Snare. If full zones are permitted, the extraction would be seen as a legitimate cost of coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_status_of_artificial_features, conceptual, 'Ambiguity in UNCLOS regarding artificial features and sovereignty generation.').

omega_variable(
    effective_occupation_threshold,
    'What constitutes ''effective occupation and administrative control'' sufficient to generate de facto territorial waters, and is this threshold met by current construction activities?',
    'Independent, verifiable assessment of continuous civilian presence, administrative services, and unchallenged enforcement over a prolonged period, as opposed to purely military presence or symbolic acts.',
    'If the threshold for effective occupation is not met, the claims of territorial waters would be seen as purely performative and extractive, increasing the theater_ratio and extractiveness. If met, it would lend more credence to the ''practice'' aspect of this reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_occupation_threshold, empirical, 'Ambiguity in the criteria for ''effective occupation'' in maritime claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(uncl_tr_t1995, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1995, 0.28).
narrative_ontology:measurement(uncl_tr_t2005, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(uncl_tr_t2015, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(uncl_be_t1995, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(uncl_be_t2005, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(uncl_be_t2015, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(uncl_su_t1995, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(uncl_su_t2005, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2005, 0.83).
narrative_ontology:measurement(uncl_su_t2015, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2015, 0.88).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_operations).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, south_china_sea_fishing_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_maritime_sovereignty' kernel, focusing on the expansive interpretation of artificial island construction. It directly influences and is influenced by the other readings and related geopolitical constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
