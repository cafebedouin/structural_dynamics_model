% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__functionalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__functionalist_reading, []).

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
 *   constraint_id: separation_of_powers_text__functionalist_reading
 *   human_readable: Separation of Powers (Functionalist Reading)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'functionalist reading' of the
 *   separation of powers kernel. This reading interprets the constitutional
 *   separation of powers as a flexible framework designed to promote
 *   effective governance, permitting overlapping authority and the delegation
 *   of legislative details to administrative agencies, provided Congress sets
 *   'intelligible principles.' It contrasts with formalist views that demand
 *   strict, impermeable boundaries between branches. This reading is
 *   foundational to the legitimacy of the modern regulatory state.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__functionalist_reading, 0.35).
domain_priors:suppression_score(separation_of_powers_text__functionalist_reading, 0.4).
domain_priors:theater_ratio(separation_of_powers_text__functionalist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(separation_of_powers_text__functionalist_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__functionalist_reading, rope).
narrative_ontology:human_readable(separation_of_powers_text__functionalist_reading, "Separation of Powers (Functionalist Reading)").
narrative_ontology:topic_domain(separation_of_powers_text__functionalist_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__functionalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__functionalist_reading, 'f5d8dc6e-17a3-4640-9ce8-dc2a5521714a').
narrative_ontology:cs_kernel_codification('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', fixed_text).
narrative_ontology:cs_authority_grounding('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', lineage).
narrative_ontology:cs_interpretation_layer_present('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a').
narrative_ontology:cs_reading_relation('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', separation_of_powers_text__unitary_executive_reading, coexists_with).
narrative_ontology:cs_axiom('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', foundational, delegation_of_principle_is_legitimate).
narrative_ontology:cs_axiom_status(delegation_of_principle_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', delegation_of_principle_is_legitimate, conventional).
narrative_ontology:cs_axiom('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', foundational, overlapping_authority_promotes_efficiency).
narrative_ontology:cs_axiom_status(overlapping_authority_promotes_efficiency, holdable).
narrative_ontology:cs_axiom_grounding('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', overlapping_authority_promotes_efficiency, instrumental).
narrative_ontology:cs_reference_frame('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', new_deal_administrative_state).
narrative_ontology:cs_drift_state('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', contemporary_regulatory_challenges, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f5d8dc6e-17a3-4640-9ce8-dc2a5521714a', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__functionalist_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, regulatory_agencies).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, congress).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, president).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__functionalist_reading, public_administration).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, formalist_legal_scholars).
narrative_ontology:constraint_victim(separation_of_powers_text__functionalist_reading, anti_regulatory_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to receive broad delegations of authority from Congress, allowing them to develop and implement detailed policy. Their legitimacy and operational scope depend on this flexible interpretation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, regulatory_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the ability to delegate complex legislative details to expert agencies, avoiding legislative gridlock and focusing on broad policy principles. This interpretation allows them to govern effectively in a complex society.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, congress, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the ability to oversee and direct a robust administrative state, exercising executive power through agencies. This reading supports a strong, unified executive branch capable of implementing policy.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, president, agenda_setter,
    institutional, generational, constrained, national).

% Interprets the boundaries of delegated authority, generally upholding the functionalist view by applying doctrines like Chevron deference. Their role is to ensure delegations adhere to 'intelligible principles' rather than strict separation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, judiciary, agenda_setter,
    institutional, generational, analytical, national).

% The broader system of government administration benefits from the efficiency and adaptability enabled by this flexible framework, allowing for effective governance and policy implementation.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, public_administration, beneficiary,
    organized, biographical, mobile, national).

% Bear the cost of their preferred strict separation of powers not being adopted as the prevailing legal interpretation. Their arguments for non-delegation are largely rejected by the courts and the administrative state.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, formalist_legal_scholars, payer,
    analytical, generational, analytical, universal).

% Oppose the expansion of the regulatory state enabled by the functionalist reading. They bear the costs of regulations and the perceived overreach of administrative power, with limited avenues for structural change.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__functionalist_reading, anti_regulatory_advocates, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(separation_of_powers_text__functionalist_reading, public_administration).
narrative_ontology:fixing_cost_class(separation_of_powers_text__functionalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables efficient and adaptable governance by permitting Congress to delegate complex policy details to expert administrative agencies, thereby avoiding legislative gridlock and allowing for specialized policy development and implementation.
% TRANSFER_FUNCTION: Transfers the authority for detailed rulemaking and policy implementation from the legislative branch to administrative agencies, within broad principles set by Congress, facilitating the operation of the modern regulatory state.
% ABSENT_VOICES: While present in public discourse, strict formalist constitutionalists and advocates for a smaller, less regulated state are structurally marginalized from the prevailing legal interpretation that upholds the functionalist view. They would argue for a return to stricter separation and non-delegation.
% DISAPPEARANCE_RATIONALE: If the functionalist reading vanished overnight, the modern administrative state would be rendered unconstitutional, leading to the collapse of countless regulatory frameworks, legislative paralysis, and a massive power vacuum. The entire structure of modern governance would need to be fundamentally reorganized.
% FOUNDING_PROBLEM: The original constitutional framework, designed for a simpler agrarian society, struggled to provide effective and efficient governance for a rapidly industrializing and complex nation, leading to calls for more adaptable governmental structures.
% FOUNDING_PROBLEM_CORROBORATION: Mainstream constitutional scholars, administrative law practitioners, and government officials widely attest to the ongoing necessity of flexible governance to address complex modern challenges, citing the impracticality of strict separation in contemporary policy-making. This corroboration comes from outside the immediate beneficiaries of specific regulations.
narrative_ontology:disappearance_verdict(separation_of_powers_text__functionalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__functionalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__functionalist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(separation_of_powers_text__functionalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__functionalist_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__functionalist_reading_tests).
:- end_tests(separation_of_powers_text__functionalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The functionalist reading is classified as a Rope because it primarily serves a coordination function, enabling efficient governance in a complex society. Extractiveness (0.35) is relatively low, reflecting that its primary goal is not rent-seeking but effective administration, though it does impose costs on those who prefer a less regulated state. Suppression (0.40) is moderate, as it actively defends the legitimacy of agency action against formalist challenges. Theater ratio (0.10) is low, indicating that the framework is genuinely functional, not merely performative. Accessibility collapse (0.40) is moderate, as it closes off strict formalist alternatives but leaves room for judicial review and political contestation. Resistance (0.20) is low, as this reading is the prevailing legal interpretation, though it faces ongoing challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries (agencies, Congress, President), this framework is a necessary and beneficial coordination mechanism. From the perspective of the 'victims' (formalist scholars, anti-regulatory advocates), it represents an illegitimate expansion of governmental power and a departure from constitutional principles. The engine's classification will reflect this divergence based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Regulatory agencies, Congress, and the President are clear beneficiaries, as this reading legitimizes their operational methods and expands their capacity to govern. Public administration as a whole also benefits from the efficiency. Formalist legal scholars and anti-regulatory advocates are 'victims' in the sense that their preferred constitutional order is suppressed by this dominant interpretation, forcing them to operate within a framework they fundamentally disagree with.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    intelligible_principle_ambiguity,
    'What constitutes an ''intelligible principle'' sufficient for constitutional delegation, and how much discretion does it truly permit agencies?',
    'Further judicial clarification through specific case law challenging the scope of agency discretion, or legislative action to provide more precise statutory guidance.',
    'A stricter interpretation of ''intelligible principle'' would increase the constraint''s effective suppression on agencies and shift it closer to a Tangled Rope by limiting their operational flexibility; a looser interpretation would reinforce its Rope-like coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intelligible_principle_ambiguity, conceptual, 'Ambiguity in the legal standard for congressional delegation to agencies.').

omega_variable(
    functionalism_vs_accountability_tradeoff,
    'At what point does the functionalist pursuit of governmental efficiency compromise democratic accountability or individual liberty?',
    'Empirical studies on the impact of agency rulemaking on public participation and judicial review, or comparative analysis with systems employing stricter separation of powers.',
    'If efficiency is found to consistently undermine accountability, the constraint''s effective extraction would increase, pushing it towards a Tangled Rope by revealing hidden costs borne by the public for administrative convenience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functionalism_vs_accountability_tradeoff, empirical, 'Trade-off between administrative efficiency and democratic accountability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__functionalist_reading, 1930, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1930, separation_of_powers_text__functionalist_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(sepa_tr_t1950, separation_of_powers_text__functionalist_reading, theater_ratio, 1950, 0.07).
narrative_ontology:measurement(sepa_tr_t1970, separation_of_powers_text__functionalist_reading, theater_ratio, 1970, 0.08).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__functionalist_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__functionalist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(sepa_tr_t2024, separation_of_powers_text__functionalist_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1930, separation_of_powers_text__functionalist_reading, base_extractiveness, 1930, 0.2).
narrative_ontology:measurement(sepa_be_t1950, separation_of_powers_text__functionalist_reading, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(sepa_be_t1970, separation_of_powers_text__functionalist_reading, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__functionalist_reading, base_extractiveness, 1990, 0.32).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__functionalist_reading, base_extractiveness, 2010, 0.34).
narrative_ontology:measurement(sepa_be_t2024, separation_of_powers_text__functionalist_reading, base_extractiveness, 2024, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1930, separation_of_powers_text__functionalist_reading, suppression_requirement, 1930, 0.25).
narrative_ontology:measurement(sepa_su_t1950, separation_of_powers_text__functionalist_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(sepa_su_t1970, separation_of_powers_text__functionalist_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__functionalist_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__functionalist_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(sepa_su_t2024, separation_of_powers_text__functionalist_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__functionalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, chevron_deference_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__functionalist_reading, non_delegation_doctrine_interpretation).

% DUAL FORMULATION NOTE:
% This is the functionalist reading of the separation of powers kernel, which also includes formalist and unitary executive readings. Each reading constitutes a distinct constraint with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
