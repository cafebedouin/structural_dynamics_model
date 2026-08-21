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
 *   domain: international_law/maritime_governance/geopolitics
 *
 * SUMMARY:
 *   This constraint represents an expansive reading of the UNCLOS framework,
 *   where artificial island construction on submerged features or low-tide
 *   elevations is asserted to generate de facto territorial waters through
 *   effective occupation and administrative control. This interpretation
 *   directly challenges the traditional understanding that only naturally
 *   formed features above water at high tide qualify as islands generating
 *   full maritime zones. It is a key element in geopolitical strategies to
 *   expand maritime claims and control in contested regions.
 *
 * KEY AGENTS:
 *   - island_constructing_states: Primary beneficiary and agenda-setter (powerful/constrained)
 *   - neighboring_claimant_states: Primary target/payer (powerful/constrained)
 *   - freedom_of_navigation_states: Secondary target/payer (institutional/mobile)
 *   - international_maritime_tribunals: Observer (institutional/analytical)
 *   - international_law_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_maritime_sovereignty__expansive_construction_reading, 0.85).
domain_priors:suppression_score(unclos_maritime_sovereignty__expansive_construction_reading, 0.9).
domain_priors:theater_ratio(unclos_maritime_sovereignty__expansive_construction_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(unclos_maritime_sovereignty__expansive_construction_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_maritime_sovereignty__expansive_construction_reading, snare).
narrative_ontology:human_readable(unclos_maritime_sovereignty__expansive_construction_reading, "Expansive Construction of Maritime Sovereignty (UNCLOS Reading)").
narrative_ontology:topic_domain(unclos_maritime_sovereignty__expansive_construction_reading, "international_law/maritime_governance/geopolitics").

domain_priors:requires_active_enforcement(unclos_maritime_sovereignty__expansive_construction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_maritime_sovereignty__expansive_construction_reading, '89debb69-c129-4cd2-b736-eff0026eec98').
narrative_ontology:cs_kernel_codification('89debb69-c129-4cd2-b736-eff0026eec98', fixed_text).
narrative_ontology:cs_authority_grounding('89debb69-c129-4cd2-b736-eff0026eec98', extraction).
narrative_ontology:cs_interpretation_layer_present('89debb69-c129-4cd2-b736-eff0026eec98').
narrative_ontology:cs_reading_relation('89debb69-c129-4cd2-b736-eff0026eec98', unclos_maritime_sovereignty__strict_geographic_reading, forecloses).
narrative_ontology:cs_reading_relation('89debb69-c129-4cd2-b736-eff0026eec98', unclos_maritime_sovereignty__hybrid_effective_control_reading, influences).
narrative_ontology:cs_axiom('89debb69-c129-4cd2-b736-eff0026eec98', foundational, effective_occupation_generates_sovereignty_over_artificial_features).
narrative_ontology:cs_axiom_status(effective_occupation_generates_sovereignty_over_artificial_features, holdable).
narrative_ontology:cs_axiom_grounding('89debb69-c129-4cd2-b736-eff0026eec98', effective_occupation_generates_sovereignty_over_artificial_features, empirically_contingent).
narrative_ontology:cs_axiom('89debb69-c129-4cd2-b736-eff0026eec98', foundational, artificial_features_can_be_islands_for_unclos_purposes).
narrative_ontology:cs_axiom_status(artificial_features_can_be_islands_for_unclos_purposes, holdable).
narrative_ontology:cs_axiom_grounding('89debb69-c129-4cd2-b736-eff0026eec98', artificial_features_can_be_islands_for_unclos_purposes, conventional).
narrative_ontology:cs_reference_frame('89debb69-c129-4cd2-b736-eff0026eec98', unclos_text_as_written).
narrative_ontology:cs_drift_state('89debb69-c129-4cd2-b736-eff0026eec98', contemporary_geopolitical_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('89debb69-c129-4cd2-b736-eff0026eec98', '').
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

% Actively constructs artificial islands on submerged features or low-tide elevations and asserts de facto territorial waters and other maritime rights. Benefits from expanded maritime zones, access to resources, and strategic military/economic advantage. Justifies actions through expansive interpretations of international law.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, island_constructing_states, agenda_setter,
    powerful, generational, constrained, global).

% See their own maritime claims diminished or threatened by the expansive construction. Bear the costs of increased regional tension, potential loss of resources, and the need to respond diplomatically or militarily. Their options are limited by the power asymmetry and the cost of direct confrontation.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, neighboring_claimant_states, payer,
    powerful, generational, constrained, regional).

% Advocate for the principles of international law, particularly freedom of navigation and overflight in international waters. Bear the costs of increased naval presence, diplomatic challenges, and the erosion of established norms. Their mobility allows them to avoid direct confrontation but not the systemic costs.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_states, payer,
    institutional, generational, mobile, global).

% Are called upon to adjudicate disputes arising from these claims, but their authority may be challenged or ignored by island-constructing states. Their role is to interpret and apply UNCLOS, but their enforcement power is limited.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_maritime_tribunals, observer,
    institutional, civilizational, analytical, global).

% Analyze the legal implications of state practice, debate the interpretation of UNCLOS, and document the evolution or erosion of international maritime law. They provide critical analysis but have no direct enforcement power.
narrative_ontology:constraint_stakeholder(unclos_maritime_sovereignty__expansive_construction_reading, international_law_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. This reading of UNCLOS serves to justify unilateral expansion of maritime claims, directly undermining the coordination function of established international maritime law regarding the delimitation of maritime zones.
% TRANSFER_FUNCTION: Transfers sovereign rights over maritime space, seabed resources, and strategic control from the global commons and other claimant states to the island-constructing state.
% ABSENT_VOICES: Small island developing states (SIDS) whose maritime zones are threatened by expansive claims, future generations who will inherit a more militarized and contested global ocean, and states lacking the naval or diplomatic capacity to challenge powerful constructing states.
% DISAPPEARANCE_RATIONALE: If this expansive reading vanished, numerous artificial features would revert to non-sovereign status, opening up vast areas of contested maritime space, altering geopolitical balances, and potentially reducing regional tensions. The legal basis for many existing claims would collapse.
% FOUNDING_PROBLEM: The strategic desire of certain states to expand their territorial control, secure access to natural resources (e.g., fisheries, hydrocarbons), and establish military outposts in contested or strategically important maritime regions, often to project power or secure trade routes.
% FOUNDING_PROBLEM_CORROBORATION: Geopolitical analysts, defense strategists, environmental organizations, and international legal scholars widely corroborate the ongoing strategic competition for maritime space and resources, driven by national interests in security and economic gain.
narrative_ontology:disappearance_verdict(unclos_maritime_sovereignty__expansive_construction_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_maritime_sovereignty__expansive_construction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_maritime_sovereignty__expansive_construction_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.85) because this reading enables states to claim vast areas of maritime space and resources that would otherwise be international waters or belong to other claimants. Suppression is also very high (0.90) as these claims are maintained through active military presence, administrative control, and the suppression of alternative activities or counter-claims. Theater ratio is high (0.60) because the justifications for these claims often rely on tenuous interpretations of 'historic rights' or 'natural features' that mask strategic expansion. Accessibility collapse is near total (0.95) as the goal is to convert international access into national control. Resistance is high (0.75) due to strong opposition from other states and international bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of island-constructing states, this constraint is a legitimate exercise of sovereignty and development, often framed as securing national interests or historical claims. From the perspective of neighboring claimant states and freedom-of-navigation states, it is an illegal act of aggression and a violation of international law, representing pure extraction of common resources and rights.
 *
 * DIRECTIONALITY LOGIC:
 *   Island-constructing states are clear beneficiaries, gaining territory and resources. Neighboring claimant states and freedom-of-navigation states are targets, losing access and facing increased geopolitical risk. International tribunals and scholars act as observers, analyzing the legal framework and state practice without directly benefiting or paying.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a Snare because its coordination story (e.g., 'securing historic rights,' 'ensuring regional stability') is a cover for pure extraction of maritime space and resources. Its persistence depends entirely on coercion and the suppression of alternatives, with clear victims. There is no genuine collective action problem solved for all participants; rather, it creates a zero-sum game where one party's gain is another's loss.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_precedent_ambiguity,
    'Is the persistent practice of artificial island construction and subsequent claim assertion establishing new customary international law, or does it remain a persistent violation of existing UNCLOS principles?',
    'A definitive ruling by an international tribunal that is widely accepted and adhered to by all major maritime powers, or a new international convention explicitly addressing artificial features.',
    'If it establishes new custom, the constraint becomes a Mountain or Rope for constructing states; if it remains a violation, it is a Snare that must be actively resisted.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legal_precedent_ambiguity, conceptual, 'Ambiguity over whether state practice constitutes new customary international law.').

omega_variable(
    effective_occupation_threshold,
    'What specific threshold of ''effective occupation and administrative control'' is required for an artificial feature to generate de facto territorial waters, and is this threshold consistently applied or selectively asserted?',
    'Development of clear, objective, and internationally recognized criteria for ''effective occupation'' that are applied uniformly, rather than being subject to unilateral interpretation.',
    'Clearer criteria would reduce the ambiguity that allows for expansive claims, potentially shifting the constraint towards a more Rope-like coordination if universally accepted, or exposing it as a Snare if the criteria are impossible to meet for non-powerful states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_occupation_threshold, empirical, 'Uncertainty regarding the objective criteria for ''effective occupation'' of artificial features.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative claims and freedom of navigation primarily structural (e.g., military presence, coast guard patrols) or is it internalized by other states due to fear of retaliation or diplomatic pressure?',
    'Observation of state behavior in the absence of direct military presence, or through diplomatic disclosures revealing the true nature of compliance mechanisms.',
    'If internalized, the effective suppression is higher than the visible structural measures suggest, making the constraint more resilient to external challenge. If purely structural, removal of physical presence would immediately open up contested areas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for maritime claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_maritime_sovereignty__expansive_construction_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1989, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1989, 0.2).
narrative_ontology:measurement(uncl_tr_t1996, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 1996, 0.35).
narrative_ontology:measurement(uncl_tr_t2003, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2003, 0.45).
narrative_ontology:measurement(uncl_tr_t2010, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2010, 0.55).
narrative_ontology:measurement(uncl_tr_t2017, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2017, 0.58).
narrative_ontology:measurement(uncl_tr_t2024, unclos_maritime_sovereignty__expansive_construction_reading, theater_ratio, 2024, 0.6).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1982, 0.4).
narrative_ontology:measurement(uncl_be_t1989, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1989, 0.5).
narrative_ontology:measurement(uncl_be_t1996, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 1996, 0.6).
narrative_ontology:measurement(uncl_be_t2003, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2003, 0.7).
narrative_ontology:measurement(uncl_be_t2010, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(uncl_be_t2017, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2017, 0.82).
narrative_ontology:measurement(uncl_be_t2024, unclos_maritime_sovereignty__expansive_construction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1982, 0.3).
narrative_ontology:measurement(uncl_su_t1989, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1989, 0.45).
narrative_ontology:measurement(uncl_su_t1996, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 1996, 0.6).
narrative_ontology:measurement(uncl_su_t2003, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(uncl_su_t2010, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2010, 0.83).
narrative_ontology:measurement(uncl_su_t2017, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2017, 0.87).
narrative_ontology:measurement(uncl_su_t2024, unclos_maritime_sovereignty__expansive_construction_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_maritime_sovereignty__expansive_construction_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__strict_geographic_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, unclos_maritime_sovereignty__hybrid_effective_control_reading).
narrative_ontology:affects_constraint(unclos_maritime_sovereignty__expansive_construction_reading, freedom_of_navigation_operations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNCLOS maritime sovereignty kernel, focusing on the expansive interpretation of artificial island construction. Its high extractiveness and suppression directly impact the viability and interpretation of the other readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
