% ============================================================================
% CONSTRAINT STORY: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_sovereignty_boundary__non_ratifier_enforcement_reading, []).

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
 *   constraint_id: unclos_sovereignty_boundary__non_ratifier_enforcement_reading
 *   human_readable: Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)
 *   domain: international_law/maritime_governance/geopolitical_strategy
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the
 *   'unclos_sovereignty_boundary' kernel, asserting that freedom of
 *   navigation principles are customary international law, enforceable by
 *   naval presence, regardless of UNCLOS ratification. This interpretation
 *   allows naval powers to operate in areas like Exclusive Economic Zones
 *   (EEZs) without being bound by UNCLOS provisions, often clashing with
 *   coastal states' claims of exclusive sovereign rights. The constraint is
 *   claimed as a 'tangled_rope' due to its dual function of coordinating
 *   global maritime access while simultaneously extracting from coastal
 *   states through active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.75).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.8).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '378848a8-a3d3-46e9-b4b9-1be01013fabc').
narrative_ontology:cs_kernel_codification('378848a8-a3d3-46e9-b4b9-1be01013fabc', formalized).
narrative_ontology:cs_authority_grounding('378848a8-a3d3-46e9-b4b9-1be01013fabc', practice).
narrative_ontology:cs_interpretation_layer_present('378848a8-a3d3-46e9-b4b9-1be01013fabc').
narrative_ontology:cs_reading_relation('378848a8-a3d3-46e9-b4b9-1be01013fabc', unclos_sovereignty_boundary__strict_eez_reading, forecloses).
narrative_ontology:cs_reading_relation('378848a8-a3d3-46e9-b4b9-1be01013fabc', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('378848a8-a3d3-46e9-b4b9-1be01013fabc', foundational, freedom_of_navigation_customary_law).
narrative_ontology:cs_axiom_status(freedom_of_navigation_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('378848a8-a3d3-46e9-b4b9-1be01013fabc', freedom_of_navigation_customary_law, conventional).
narrative_ontology:cs_axiom('378848a8-a3d3-46e9-b4b9-1be01013fabc', secondary, naval_presence_legitimate_enforcement).
narrative_ontology:cs_axiom_status(naval_presence_legitimate_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('378848a8-a3d3-46e9-b4b9-1be01013fabc', naval_presence_legitimate_enforcement, conventional).
narrative_ontology:cs_reference_frame('378848a8-a3d3-46e9-b4b9-1be01013fabc', traditional_high_seas_freedom).
narrative_ontology:cs_drift_state('378848a8-a3d3-46e9-b4b9-1be01013fabc', post_unclos_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('378848a8-a3d3-46e9-b4b9-1be01013fabc', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_ratifiers).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_shipping_industry).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert the right to conduct naval operations (including transit and exercises) in Exclusive Economic Zones (EEZs) and other maritime areas based on customary international law, independent of their ratification status of UNCLOS. They actively enforce this interpretation through naval presence and operations, benefiting from unrestricted global maritime access.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_ratifiers, agenda_setter,
    institutional, generational, arbitrage, global).

% These states interpret UNCLOS Article 57 as granting them exclusive sovereign rights over their EEZs, including the right to regulate or restrict foreign military activities. They bear the cost of having their claims challenged and effectively suppressed by the naval presence of non-ratifying powers, limiting their perceived sovereignty.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    institutional, generational, constrained, national).

% States that have ratified UNCLOS and generally adhere to its provisions. They benefit from the overall framework of maritime law but may find their interpretation of EEZ rights challenged by the non-ratifier enforcement reading, creating legal ambiguity and diplomatic friction.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_ratifying_states, observer,
    institutional, generational, mobile, global).

% Benefits from the broad interpretation of freedom of navigation, as it ensures predictable and unrestricted transit for commercial vessels across global maritime routes, reducing potential delays and costs associated with stricter coastal state controls.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% Analyze and debate the legal basis and implications of freedom of navigation principles, customary international law, and UNCLOS interpretations. Their work informs state policy and international tribunals but does not directly enforce the constraint.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers_non_ratifiers).
narrative_ontology:fixing_cost_class(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared understanding and expectation for the movement of vessels, particularly military, across global maritime spaces, aiming to prevent unilateral restrictions from impeding international transit and security operations.
% TRANSFER_FUNCTION: Transfers the effective right to control military activities within Exclusive Economic Zones (EEZs) from coastal states (who claim exclusivity under UNCLOS) to naval powers (who assert customary freedom of navigation), ensuring global maritime access for the latter.
% ABSENT_VOICES: Small island developing states (SIDS) and landlocked states, who often advocate for stronger coastal state rights and a more equitable distribution of maritime resources, but lack the naval power or geopolitical leverage to significantly influence the interpretation or enforcement of these principles.
% DISAPPEARANCE_RATIONALE: If the enforcement of freedom of navigation principles by naval presence vanished, coastal states would likely assert much stronger and potentially conflicting claims over their EEZs, leading to increased maritime disputes, blockades, and a significant disruption of international trade and military mobility.
% FOUNDING_PROBLEM: The historical imperative to ensure open access to the seas for trade, exploration, and military projection, preventing individual states from unilaterally closing off vital maritime routes and creating chokepoints.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers and the international shipping industry consistently attest to the ongoing necessity of freedom of navigation for global commerce and security. Coastal states and some international legal scholars acknowledge the underlying problem but contest the *method* of enforcement and the *independence* from UNCLOS, arguing the current arrangement serves specific geopolitical interests rather than universal coordination.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unclos_sovereignty_boundary__non_ratifier_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) reflects the cost imposed on coastal states whose claims of EEZ exclusivity are overridden by naval operations. Suppression (0.80) is high because the constraint's persistence relies on the active, coercive presence of naval forces to deter or counter coastal state assertions. The theater ratio is low (0.10) as naval presence constitutes genuine, direct enforcement, not mere performance. Accessibility collapse (0.70) is substantial for coastal states, as their alternatives for asserting full control over their EEZs are significantly curtailed. Resistance (0.60) is moderate, as coastal states frequently protest and challenge these operations diplomatically and through legal means. The temporal measurements show a gradual increase in both extractiveness and suppression, reflecting the ongoing contestation and the increasing need for active enforcement to maintain this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   Naval powers view this constraint as a necessary mechanism for maintaining global maritime order and security, a 'rope' ensuring common access. Coastal states, however, experience it as an extractive 'snare' that undermines their sovereign rights and economic control over their maritime territories. The engine's per-seat classification will capture this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers (non-ratifiers) are clear beneficiaries and agenda-setters, directly gaining from the ability to operate globally without UNCLOS restrictions. Coastal states asserting EEZ exclusivity are the primary payers/victims, bearing the cost of diminished control. UNCLOS ratifying states are observers, benefiting from general maritime order but facing challenges to their own interpretations. The international shipping industry is a beneficiary of predictable transit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_vs_power_assertion,
    'Is the assertion of freedom of navigation by non-ratifying naval powers genuinely rooted in customary international law, or is it primarily an assertion of geopolitical power?',
    'Analysis of state practice and opinio juris (belief that practice is legally required) from a broad range of states, particularly those without significant naval power, and rulings by international tribunals on specific cases.',
    'If primarily power assertion, the constraint''s extractiveness is higher and its coordination function is weaker, pushing it closer to a ''snare''. If genuinely customary law, its coordination function is stronger, supporting a ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_vs_power_assertion, conceptual, 'Ambiguity regarding the true legal grounding versus power dynamics.').

omega_variable(
    eez_sovereignty_scope,
    'What is the precise scope of coastal state sovereign rights within the EEZ, particularly concerning military activities, under both UNCLOS and customary international law?',
    'Further clarification through state practice, diplomatic negotiations, and potentially advisory opinions or rulings from international courts (e.g., ITLOS, ICJ).',
    'A narrower interpretation of coastal state rights would reduce the perceived extraction from naval powers, while a broader interpretation would increase it, potentially shifting the constraint''s classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(eez_sovereignty_scope, conceptual, 'Uncertainty regarding the extent of coastal state sovereignty in EEZs.').

omega_variable(
    suppression_mechanism_legitimacy,
    'Is the suppression of coastal state EEZ claims by naval presence a legitimate enforcement of customary law, or an illegitimate use of force to maintain an advantageous interpretation?',
    'International legal consensus on the legality of specific naval operations in EEZs, and the outcomes of diplomatic protests and legal challenges by coastal states.',
    'If deemed illegitimate, the suppression metric''s moral valence shifts, and the constraint''s overall classification leans more heavily towards ''snare'' due to the lack of legitimate coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_legitimacy, preference, 'Ethical and legal legitimacy of the enforcement mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.1).
narrative_ontology:measurement(uncl_tr_t1992, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1992, 0.1).
narrative_ontology:measurement(uncl_tr_t2002, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2002, 0.1).
narrative_ontology:measurement(uncl_tr_t2012, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2012, 0.1).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(uncl_be_t1992, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1992, 0.65).
narrative_ontology:measurement(uncl_be_t2002, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2002, 0.7).
narrative_ontology:measurement(uncl_be_t2012, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2012, 0.73).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.65).
narrative_ontology:measurement(uncl_su_t1992, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1992, 0.7).
narrative_ontology:measurement(uncl_su_t2002, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2002, 0.75).
narrative_ontology:measurement(uncl_su_t2012, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2012, 0.78).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_sovereignty_boundary' kernel, each representing a distinct interpretation of maritime sovereignty and freedom of navigation. This reading emphasizes customary international law and naval enforcement, distinct from strict UNCLOS adherence or historical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
