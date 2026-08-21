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
 *   This constraint story instantiates the 'non_ratifier_enforcement_reading'
 *   of the 'unclos_sovereignty_boundary' kernel. It describes the situation
 *   where major naval powers assert that freedom of navigation principles are
 *   customary international law (CIL), enforceable by their naval presence,
 *   irrespective of UNCLOS ratification. This reading positions coastal
 *   states attempting to assert exclusive control over their EEZs as targets
 *   of extraction, as their claims are overridden by the CIL principle. The
 *   constraint is claimed as a 'rope' by its beneficiaries (naval powers) but
 *   operates as a 'tangled_rope' due to its high extraction and active
 *   enforcement against coastal states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.78).
domain_priors:suppression_score(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.85).
domain_priors:theater_ratio(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "Freedom of Navigation as Customary International Law (Non-Ratifier Enforcement Reading)").
narrative_ontology:topic_domain(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, "international_law/maritime_governance/geopolitical_strategy").

domain_priors:requires_active_enforcement(unclos_sovereignty_boundary__non_ratifier_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '3335eff4-0ec5-4aaa-bb46-b99719b22575').
narrative_ontology:cs_kernel_codification('3335eff4-0ec5-4aaa-bb46-b99719b22575', formalized).
narrative_ontology:cs_authority_grounding('3335eff4-0ec5-4aaa-bb46-b99719b22575', practice).
narrative_ontology:cs_interpretation_layer_present('3335eff4-0ec5-4aaa-bb46-b99719b22575').
narrative_ontology:cs_reading_relation('3335eff4-0ec5-4aaa-bb46-b99719b22575', unclos_sovereignty_boundary__strict_eez_reading, influences).
narrative_ontology:cs_reading_relation('3335eff4-0ec5-4aaa-bb46-b99719b22575', unclos_sovereignty_boundary__historical_rights_reading, coexists_with).
narrative_ontology:cs_axiom('3335eff4-0ec5-4aaa-bb46-b99719b22575', foundational, freedom_of_navigation_is_customary_international_law).
narrative_ontology:cs_axiom_status(freedom_of_navigation_is_customary_international_law, holdable).
narrative_ontology:cs_axiom_grounding('3335eff4-0ec5-4aaa-bb46-b99719b22575', freedom_of_navigation_is_customary_international_law, conventional).
narrative_ontology:cs_axiom('3335eff4-0ec5-4aaa-bb46-b99719b22575', secondary, state_practice_establishes_customary_law).
narrative_ontology:cs_axiom_status(state_practice_establishes_customary_law, holdable).
narrative_ontology:cs_axiom_grounding('3335eff4-0ec5-4aaa-bb46-b99719b22575', state_practice_establishes_customary_law, conventional).
narrative_ontology:cs_reference_frame('3335eff4-0ec5-4aaa-bb46-b99719b22575', unhindered_global_navigation_as_cil).
narrative_ontology:cs_drift_state('3335eff4-0ec5-4aaa-bb46-b99719b22575', contemporary_eez_assertions_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('3335eff4-0ec5-4aaa-bb46-b99719b22575', '').
narrative_ontology:cs_kernel_id(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers).
narrative_ontology:constraint_beneficiary(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry).
narrative_ontology:constraint_victim(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert the right to conduct military and commercial operations in all international waters, including claimed Exclusive Economic Zones (EEZs), based on customary international law (CIL), regardless of their UNCLOS ratification status. They actively enforce these principles through naval presence and operations, benefiting from strategic flexibility and unhindered global access.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, naval_powers, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Benefits from predictable and unhindered passage through key maritime chokepoints and claimed EEZs, reducing transit times, insurance costs, and the risk of arbitrary interference. Their operations rely on the consistent application of freedom of navigation principles.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_shipping_industry, beneficiary,
    organized, biographical, mobile, global).

% These states interpret their EEZ rights as granting them greater exclusivity and control over activities within 200 nautical miles of their coastlines, often seeking to regulate or restrict foreign naval operations. They bear the cost of foreign naval presence and surveillance in these zones, with limited practical options to deter or prevent such activities against powerful naval states.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, coastal_states_asserting_eez_exclusivity, payer,
    powerful, generational, constrained, national).

% States that have ratified UNCLOS and generally adhere to its provisions, including those on EEZs. They observe the actions of non-ratifying naval powers and often find themselves in a complex diplomatic position, balancing treaty obligations with geopolitical realities and the need for stable maritime order.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_ratifying_states, observer,
    institutional, generational, constrained, global).

% Organizations like the IMO promote safe and efficient navigation and pollution prevention. They operate within the framework of international law but lack direct enforcement power over sovereign states, primarily influencing through standards, recommendations, and diplomatic channels.
narrative_ontology:constraint_stakeholder(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, international_maritime_organizations, observer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for unhindered passage through international waters, including claimed EEZs, ensuring predictable routes for global commerce and military mobility, thereby preventing fragmentation of the global commons.
% TRANSFER_FUNCTION: Transfers the right to unhindered passage and strategic flexibility to naval powers and the global shipping industry, often at the expense of coastal states' claims of exclusive control and resource management within their asserted EEZs.
% ABSENT_VOICES: Small island developing states and indigenous coastal communities, whose interests in local resource management, environmental protection, and cultural heritage within maritime zones are often overshadowed by great power competition and commercial shipping imperatives.
% DISAPPEARANCE_RATIONALE: If freedom of navigation as CIL were no longer enforceable, coastal states would likely assert much stricter and potentially conflicting controls over their EEZs, leading to increased maritime disputes, disruption of global trade routes, and significant challenges to international security and naval operations.
% FOUNDING_PROBLEM: The historical challenge of balancing coastal state sovereignty over adjacent waters with the imperative for free and safe passage for international trade and naval operations, preventing excessive national enclosure of the seas and ensuring access to global commons.
% FOUNDING_PROBLEM_CORROBORATION: Naval powers and the global shipping industry consistently attest that the problem of ensuring free passage remains live, citing ongoing challenges to navigation. Coastal states and some international legal scholars contest this framing, arguing that the problem has evolved into one of great power projection and resource extraction, rather than genuine coordination. Diplomatic protests and legal challenges from outside the benefiting parties corroborate the contested status.
narrative_ontology:disappearance_verdict(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, world_rearranges).
narrative_ontology:founding_problem_status(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.78) is high because naval powers gain significant strategic and economic advantages by operating freely in zones that coastal states consider under their exclusive jurisdiction. Suppression (0.85) is also high, as the enforcement relies on the overwhelming military power of naval states, leaving coastal states with limited effective recourse. The theater ratio is low (0.20) because naval presence is a direct and functional means of enforcing these claims, not primarily performative. The increasing extractiveness and suppression over time reflect the growing assertiveness of both naval powers and coastal states, leading to more frequent and intense confrontations.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of naval powers, this constraint is a necessary coordination mechanism for global stability and trade. From the perspective of coastal states, it is an extractive imposition of great power interests, undermining their sovereign rights and resource control. The engine's classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Naval powers are clear beneficiaries and agenda-setters, directly gaining from unhindered access and enforcing the rules (low directionality). The global shipping industry also benefits from predictable routes. Coastal states asserting EEZ exclusivity are the primary victims, bearing the costs of foreign naval presence and the erosion of their perceived sovereign rights (high directionality). UNCLOS ratifying states and international maritime organizations act as observers, navigating the complex legal and political landscape without directly benefiting or being targeted by this specific enforcement reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_vs_treaty_law_primacy,
    'To what extent do freedom of navigation principles, as customary international law, genuinely operate independently of and potentially override UNCLOS treaty obligations for both ratifying and non-ratifying states?',
    'Analysis of state practice and opinio juris in international courts and diplomatic exchanges, particularly in cases involving non-ratifying naval powers and UNCLOS-ratifying coastal states.',
    'If CIL is consistently found to override or exist independently, it strengthens the naval powers'' position; if UNCLOS is deemed paramount, it weakens the CIL-based enforcement claims and supports coastal state exclusivity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_vs_treaty_law_primacy, conceptual, 'Ambiguity regarding the hierarchical relationship between customary international law and treaty law (UNCLOS) in maritime governance.').

omega_variable(
    legitimacy_of_unilateral_enforcement,
    'Is enforcement of customary international law by unilateral naval presence a legitimate mechanism, or does it constitute an assertion of power that undermines the consensual basis of international law?',
    'Analysis of international legal scholarship, state protests, and the outcomes of international tribunals regarding the legality of ''freedom of navigation operations'' (FONOPs) and similar actions.',
    'If deemed illegitimate, the constraint''s suppression and extractiveness would be reclassified as pure coercion; if legitimate, it would be seen as a valid, albeit forceful, aspect of international legal order.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unilateral_enforcement, preference, 'Contestation over the legitimacy of unilateral military enforcement of CIL in international waters.').

omega_variable(
    scope_of_eez_rights_ambiguity,
    'What is the precise scope of coastal state rights within the EEZ, particularly concerning military activities by foreign states, and how does this interact with freedom of navigation?',
    'Further clarification through state practice, international legal interpretations, and potential future amendments or protocols to UNCLOS or related conventions.',
    'A narrower interpretation of coastal state rights would reduce the perceived extraction from naval powers; a broader interpretation would increase it, potentially leading to reclassification towards a Snare for naval operations.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_eez_rights_ambiguity, empirical, 'Uncertainty regarding the exact balance of rights and freedoms within the Exclusive Economic Zone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, 1982, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uncl_tr_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1982, 0.2).
narrative_ontology:measurement(uncl_tr_t1990, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(uncl_tr_t1998, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 1998, 0.2).
narrative_ontology:measurement(uncl_tr_t2006, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(uncl_tr_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2014, 0.2).
narrative_ontology:measurement(uncl_tr_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(uncl_be_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1982, 0.6).
narrative_ontology:measurement(uncl_be_t1990, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(uncl_be_t1998, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 1998, 0.7).
narrative_ontology:measurement(uncl_be_t2006, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2006, 0.73).
narrative_ontology:measurement(uncl_be_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2014, 0.76).
narrative_ontology:measurement(uncl_be_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(uncl_su_t1982, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1982, 0.7).
narrative_ontology:measurement(uncl_su_t1990, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(uncl_su_t1998, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 1998, 0.8).
narrative_ontology:measurement(uncl_su_t2006, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2006, 0.82).
narrative_ontology:measurement(uncl_su_t2014, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2014, 0.84).
narrative_ontology:measurement(uncl_su_t2024, unclos_sovereignty_boundary__non_ratifier_enforcement_reading, suppression_requirement, 2024, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_infrastructure).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__strict_eez_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, unclos_sovereignty_boundary__historical_rights_reading).
narrative_ontology:affects_constraint(unclos_sovereignty_boundary__non_ratifier_enforcement_reading, global_trade_routes_security).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'unclos_sovereignty_boundary' kernel, each representing a distinct interpretation of maritime sovereignty and freedom of navigation. This reading emphasizes customary international law and naval enforcement, influencing and coexisting with other interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
