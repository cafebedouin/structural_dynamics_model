% ============================================================================
% CONSTRAINT STORY: separation_of_powers_text__unitary_executive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_separation_of_powers_text__unitary_executive_reading, []).

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
 *   constraint_id: separation_of_powers_text__unitary_executive_reading
 *   human_readable: Unitary Executive Principle (Reading of Separation of Powers Text)
 *   domain: constitutional_law/political_theory/administrative_law
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, 0.78).
domain_priors:suppression_score(separation_of_powers_text__unitary_executive_reading, 0.85).
domain_priors:theater_ratio(separation_of_powers_text__unitary_executive_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(separation_of_powers_text__unitary_executive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(separation_of_powers_text__unitary_executive_reading, tangled_rope).
narrative_ontology:human_readable(separation_of_powers_text__unitary_executive_reading, "Unitary Executive Principle (Reading of Separation of Powers Text)").
narrative_ontology:topic_domain(separation_of_powers_text__unitary_executive_reading, "constitutional_law/political_theory/administrative_law").

domain_priors:requires_active_enforcement(separation_of_powers_text__unitary_executive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(separation_of_powers_text__unitary_executive_reading, 'cb8d02f2-51ea-4f3a-9f9d-7ba161763310').
narrative_ontology:cs_kernel_codification('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', fixed_text).
narrative_ontology:cs_authority_grounding('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', lineage).
narrative_ontology:cs_interpretation_layer_present('cb8d02f2-51ea-4f3a-9f9d-7ba161763310').
narrative_ontology:cs_reading_relation('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', separation_of_powers_text__formalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', separation_of_powers_text__functionalist_reading, coexists_with).
narrative_ontology:cs_axiom('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', foundational, all_executive_power_vests_in_president).
narrative_ontology:cs_axiom_status(all_executive_power_vests_in_president, holdable).
narrative_ontology:cs_axiom_grounding('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', all_executive_power_vests_in_president, deontological).
narrative_ontology:cs_axiom('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', secondary, independent_agencies_violate_separation_of_powers).
narrative_ontology:cs_axiom_status(independent_agencies_violate_separation_of_powers, holdable).
narrative_ontology:cs_axiom_grounding('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', independent_agencies_violate_separation_of_powers, conventional).
narrative_ontology:cs_reference_frame('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', original_constitutional_design_single_executive).
narrative_ontology:cs_drift_state('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', contemporary_administrative_state_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('cb8d02f2-51ea-4f3a-9f9d-7ba161763310', '').
narrative_ontology:cs_kernel_id(separation_of_powers_text__unitary_executive_reading, separation_of_powers_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, president_of_the_united_states).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, executive_branch_officials).
narrative_ontology:constraint_beneficiary(separation_of_powers_text__unitary_executive_reading, unitary_executive_theorists).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, independent_agencies).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, congress_of_the_united_states).
narrative_ontology:constraint_victim(separation_of_powers_text__unitary_executive_reading, administrative_state_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate control over all executive functions and personnel, including the power to remove heads of independent agencies at will. Benefits from consolidated power and streamlined executive action.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, president_of_the_united_states, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefit from clearer lines of authority and reduced bureaucratic friction, as presidential directives carry more weight across the executive apparatus. Their careers are often tied to the President's agenda.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, executive_branch_officials, beneficiary,
    institutional, biographical, constrained, national).

% Lose statutory independence and autonomy, becoming more directly subject to presidential control and removal power. Their ability to pursue long-term policy goals insulated from political shifts is diminished.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, independent_agencies, payer,
    institutional, biographical, constrained, national).

% Loses its ability to structure the executive branch with independent agencies accountable to Congress, and its power to delegate authority with specific checks on presidential control. This diminishes legislative influence over administration.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, congress_of_the_united_states, payer,
    institutional, generational, constrained, national).

% Oppose the unitary executive principle, viewing it as undermining effective, expert-driven governance and democratic accountability through independent bodies. They face a constant struggle to defend the existing administrative structure.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, administrative_state_advocates, payer,
    organized, generational, constrained, national).

% Provide the intellectual and legal justification for the unitary executive principle. Their ideas gain prominence and influence policy when administrations adopt this reading, enhancing their professional standing and impact.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, unitary_executive_theorists, beneficiary,
    organized, generational, mobile, national).

% Adjudicates legal challenges to presidential actions based on the unitary executive principle, determining the boundaries of executive power and agency independence. Their rulings shape the constraint's practical application.
narrative_ontology:constraint_stakeholder(separation_of_powers_text__unitary_executive_reading, judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to centralize executive authority in the President to ensure decisive, coherent, and accountable governance, avoiding fragmentation and conflicting directives within the executive branch.
% TRANSFER_FUNCTION: Transfers power and control over administrative functions and personnel from Congress and independent agencies to the President, consolidating authority in the executive office.
% ABSENT_VOICES: Advocates for a robust, independent administrative state, and those who believe in a more distributed model of governance with checks and balances across branches, are often marginalized in the unitary executive discourse. They would argue for the benefits of agency expertise and insulation from political pressure.
% DISAPPEARANCE_RATIONALE: If the unitary executive principle vanished, the balance of power within the federal government would fundamentally shift. Congress would regain greater latitude in structuring agencies, independent agencies would assert more autonomy, and the entire executive branch would require a complete re-evaluation of its structure and accountability mechanisms, leading to a significant reorganization of governance.
% FOUNDING_PROBLEM: The perceived problem of an unaccountable, fragmented executive branch, lacking coherence and direct presidential control, leading to inefficiency, a blurring of constitutional lines, and a departure from the original constitutional design of a singular, energetic executive.
% FOUNDING_PROBLEM_CORROBORATION: Unitary executive theorists and some presidential administrations attest to the ongoing problem of executive fragmentation and lack of accountability. Legal scholars, administrative law experts, and members of Congress often contest this, arguing for the benefits of independent agencies and congressional oversight, citing historical precedent and practical necessity. Corroboration for the 'problem is live' claim primarily comes from within the beneficiary group; external corroboration is limited and often framed as a different problem (e.g., 'too much presidential power').
narrative_ontology:disappearance_verdict(separation_of_powers_text__unitary_executive_reading, world_rearranges).
narrative_ontology:founding_problem_status(separation_of_powers_text__unitary_executive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(separation_of_powers_text__unitary_executive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(separation_of_powers_text__unitary_executive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(separation_of_powers_text__unitary_executive_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(separation_of_powers_text__unitary_executive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(separation_of_powers_text__unitary_executive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(separation_of_powers_text__unitary_executive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ambiguity_of_executive_power,
    'What is the precise scope of ''executive power'' as vested in the President by Article II, and does it inherently preclude independent agencies?',
    'Further constitutional amendment or a definitive Supreme Court ruling that explicitly defines ''executive power'' in relation to agency structure and removal powers.',
    'A narrow interpretation of ''executive power'' would weaken the unitary executive principle, potentially reclassifying it closer to a ''rope'' or ''scaffold'' for specific functions. A broad interpretation would strengthen its extractive nature, pushing it closer to a ''snare'' for independent agencies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ambiguity_of_executive_power, conceptual, 'The fundamental ambiguity of constitutional text regarding executive power and agency independence.').

omega_variable(
    empirical_impact_on_governance,
    'Does a stronger unitary executive actually lead to more efficient, accountable, and effective governance, or does it lead to politicization and reduced expertise?',
    'Longitudinal empirical studies comparing governance outcomes (efficiency, accountability, policy stability) in periods of strong vs. weak unitary executive assertion, controlling for other political variables.',
    'Empirical evidence of improved governance would bolster the coordination narrative, potentially reducing perceived extractiveness. Evidence of politicization and reduced effectiveness would further expose the extractive nature, strengthening the ''snare'' component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_impact_on_governance, empirical, 'The actual, rather than claimed, impact of the unitary executive principle on governmental performance.').

omega_variable(
    political_tool_vs_constitutional_interpretation,
    'To what extent is the unitary executive principle a genuine constitutional interpretation, and to what extent is it a political tool used by presidents to expand their power?',
    'Analysis of presidential actions and legal arguments across different administrations, examining consistency of application regardless of party or policy goals, and the degree to which it is invoked selectively.',
    'If primarily a political tool, the ''theater_ratio'' would be higher, and the ''extractiveness'' would be more clearly seen as rent-seeking, pushing the classification towards ''snare''. If a consistent constitutional interpretation, the ''coordination_function'' would be more robust, supporting the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_tool_vs_constitutional_interpretation, conceptual, 'Distinguishing between genuine constitutional theory and strategic power assertion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(separation_of_powers_text__unitary_executive_reading, 1930, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sepa_tr_t1930, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1930, 0.2).
narrative_ontology:measurement(sepa_tr_t1950, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1950, 0.25).
narrative_ontology:measurement(sepa_tr_t1970, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1970, 0.3).
narrative_ontology:measurement(sepa_tr_t1990, separation_of_powers_text__unitary_executive_reading, theater_ratio, 1990, 0.35).
narrative_ontology:measurement(sepa_tr_t2010, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(sepa_tr_t2030, separation_of_powers_text__unitary_executive_reading, theater_ratio, 2030, 0.4).

% Extraction over time
narrative_ontology:measurement(sepa_be_t1930, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1930, 0.5).
narrative_ontology:measurement(sepa_be_t1950, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1950, 0.6).
narrative_ontology:measurement(sepa_be_t1970, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1970, 0.68).
narrative_ontology:measurement(sepa_be_t1990, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 1990, 0.72).
narrative_ontology:measurement(sepa_be_t2010, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(sepa_be_t2030, separation_of_powers_text__unitary_executive_reading, base_extractiveness, 2030, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(sepa_su_t1930, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1930, 0.55).
narrative_ontology:measurement(sepa_su_t1950, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(sepa_su_t1970, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1970, 0.73).
narrative_ontology:measurement(sepa_su_t1990, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 1990, 0.79).
narrative_ontology:measurement(sepa_su_t2010, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2010, 0.82).
narrative_ontology:measurement(sepa_su_t2030, separation_of_powers_text__unitary_executive_reading, suppression_requirement, 2030, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(separation_of_powers_text__unitary_executive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, administrative_state_legitimacy).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, congressional_delegation_doctrine).
narrative_ontology:affects_constraint(separation_of_powers_text__unitary_executive_reading, presidential_power_expansion).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'separation_of_powers_text' kernel, focusing on the unitary executive principle. It differs from the formalist and functionalist readings in its specific interpretation of executive power and agency independence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
