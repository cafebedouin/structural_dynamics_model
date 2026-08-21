% ============================================================================
% CONSTRAINT STORY: balfour_mandate_instruments__mandatory_interpretive_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_balfour_mandate_instruments__mandatory_interpretive_discretion, []).

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
 *   constraint_id: balfour_mandate_instruments__mandatory_interpretive_discretion
 *   human_readable: Balfour Mandate Instruments: Mandatory Interpretive Discretion
 *   domain: international_law/colonial_administration/state_formation
 *
 * SUMMARY:
 *   This constraint describes the British mandatory power's interpretive
 *   discretion over the Balfour Declaration and League of Nations Mandate
 *   instruments in Palestine (1920-1948). This reading focuses on how the
 *   British maintained authority by adjudicating between competing Arab and
 *   Zionist claims without external review, using interpretive flexibility as
 *   an operational constraint system. This discretion created strategic
 *   uncertainty and path-dependent lock-in for both communities, while
 *   benefiting British colonial administration through policy flexibility and
 *   a 'divide and rule' dynamic. The constraint is classified as a Snare due
 *   to its high extraction and suppression, despite being presented as a
 *   coordination mechanism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.65).
domain_priors:suppression_score(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.75).
domain_priors:theater_ratio(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, extractiveness, 0.65).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(balfour_mandate_instruments__mandatory_interpretive_discretion, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(balfour_mandate_instruments__mandatory_interpretive_discretion, snare).
narrative_ontology:human_readable(balfour_mandate_instruments__mandatory_interpretive_discretion, "Balfour Mandate Instruments: Mandatory Interpretive Discretion").
narrative_ontology:topic_domain(balfour_mandate_instruments__mandatory_interpretive_discretion, "international_law/colonial_administration/state_formation").

domain_priors:requires_active_enforcement(balfour_mandate_instruments__mandatory_interpretive_discretion).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(balfour_mandate_instruments__mandatory_interpretive_discretion, '6509ac3e-9969-4fdf-93f6-ec20bbc15c7b').
narrative_ontology:cs_kernel_codification('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', formalized).
narrative_ontology:cs_authority_grounding('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', extraction).
narrative_ontology:cs_interpretation_layer_present('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b').
narrative_ontology:cs_reading_relation('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', balfour_mandate_instruments__jewish_national_home_primacy, influences).
narrative_ontology:cs_reading_relation('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', balfour_mandate_instruments__dual_obligation_indigenous_rights, influences).
narrative_ontology:cs_axiom('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', foundational, mandatory_power_as_sole_arbiter).
narrative_ontology:cs_axiom_status(mandatory_power_as_sole_arbiter, holdable).
narrative_ontology:cs_axiom_grounding('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', mandatory_power_as_sole_arbiter, conventional).
narrative_ontology:cs_axiom('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', foundational, interpretive_flexibility_as_governance_tool).
narrative_ontology:cs_axiom_status(interpretive_flexibility_as_governance_tool, holdable).
narrative_ontology:cs_axiom_grounding('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', interpretive_flexibility_as_governance_tool, instrumental).
narrative_ontology:cs_reference_frame('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', unilateral_british_interpretive_supremacy).
narrative_ontology:cs_drift_state('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', end_of_mandate_1948, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('6509ac3e-9969-4fdf-93f6-ec20bbc15c7b', '').
narrative_ontology:cs_kernel_id(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community).
narrative_ontology:constraint_victim(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds ultimate authority to interpret the mandate instruments, allowing for policy flexibility and strategic ambiguity. Benefits from maintaining control and leveraging competing claims to manage the territory without external review or fixed legal constraints. Their discretion is the core of the constraint.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, british_colonial_administrators, agenda_setter,
    institutional, generational, arbitrage, global).

% Subject to British policy shifts regarding land, immigration, and political representation, which often contradict their claims to self-determination and existing rights. Unable to appeal to a fixed interpretation of the mandate or external arbitration, leading to strategic uncertainty and loss of control over their future.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, arab_community, payer,
    organized, generational, constrained, regional).

% Subject to British policy shifts that impact their ability to establish a Jewish national home, including restrictions on immigration and land acquisition. While often benefiting from British support, the inherent interpretive discretion means their aspirations are always contingent on British will, creating uncertainty and frustration.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, zionist_community, payer,
    organized, generational, constrained, regional).

% Nominally oversees the British mandate but lacks enforcement power to challenge British interpretive discretion. Receives reports and hears petitions but cannot compel adherence to a specific reading of the mandate, effectively legitimizing British flexibility.
narrative_ontology:constraint_stakeholder(balfour_mandate_instruments__mandatory_interpretive_discretion, league_of_nations_mandates_commission, observer,
    institutional, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for British administration of Palestine, allowing for flexible policy responses to local conditions and international pressures, ostensibly to guide the territory towards self-governance while balancing competing claims.
% TRANSFER_FUNCTION: Transfers political and legal authority over the territory's future from its inhabitants to the British mandatory power, enabling the British to extract strategic geopolitical benefits and maintain control through interpretive ambiguity.
% ABSENT_VOICES: An independent, internationally recognized arbitration body with binding authority to interpret the mandate's terms would challenge British discretion. Such a body would likely be demanded by both Arab and Zionist communities seeking clarity and fixed legal principles.
% DISAPPEARANCE_RATIONALE: If British interpretive discretion vanished, the underlying mandate instruments would immediately require a fixed, externally arbitrated interpretation. This would force a resolution of the competing claims to the territory, fundamentally altering the political landscape and the path to state formation for both communities.
% FOUNDING_PROBLEM: The problem of administering former Ottoman territories after WWI, specifically Palestine, with competing national aspirations (Arab self-determination and Zionist national home) and the need for a transitional authority to guide the region.
% FOUNDING_PROBLEM_CORROBORATION: British administrators consistently asserted the complexity of the situation and the necessity of their flexible interpretive role. Both Arab and Zionist communities, despite their grievances, implicitly acknowledged the British as the de facto adjudicating authority, even as they contested its decisions. Historians and international legal scholars corroborate the existence of this foundational problem and the British claim to interpretive authority.
narrative_ontology:disappearance_verdict(balfour_mandate_instruments__mandatory_interpretive_discretion, world_rearranges).
narrative_ontology:founding_problem_status(balfour_mandate_instruments__mandatory_interpretive_discretion, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(balfour_mandate_instruments__mandatory_interpretive_discretion, 'none', 1).
narrative_ontology:epsilon_provenance(balfour_mandate_instruments__mandatory_interpretive_discretion, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(balfour_mandate_instruments__mandatory_interpretive_discretion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(balfour_mandate_instruments__mandatory_interpretive_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is moderate-high because British discretion allowed for policies that extracted resources (e.g., land sales, taxation) and political agency from both communities, often without clear legal basis beyond the mandatory power's interpretation. Suppression (0.75) is high because both communities were unable to appeal to fixed textual meaning or external arbitration, and any resistance was met with British administrative and military force. Theater ratio (0.20) is low-moderate; while there was genuine administrative effort, a significant portion of the 'coordination' involved managing competing claims to maintain British control rather than genuinely resolving them. The temporal measurements reflect increasing extractiveness and suppression as British policy became more contested and enforcement more necessary, peaking around the 1939 White Paper, then slightly declining as the mandate neared its end.
 *
 * PERSPECTIVAL GAP:
 *   From the British perspective, their interpretive discretion was a necessary, even benevolent, coordination mechanism to manage an intractable conflict. From the perspective of both Arab and Zionist communities, it was an extractive snare that denied them agency and subjected them to arbitrary rule. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   British colonial administrators are the primary beneficiaries (low d) as their interpretive discretion grants them policy flexibility and maintains their control. Both the Arab and Zionist communities are victims (high d) as they are subject to arbitrary policy shifts and lack fixed legal recourse, bearing the costs of strategic uncertainty and unfulfilled aspirations. The League of Nations Mandates Commission is an observer (analytical d), nominally overseeing but lacking the power to alter the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_discretion,
    'Was British interpretive discretion a legitimate exercise of mandatory power, or an overreach that undermined the mandate''s stated goals?',
    'International legal review by a neutral, binding arbitration body, or a historical counterfactual analysis of alternative administrative models.',
    'If deemed legitimate, the constraint''s coordination function is emphasized; if overreach, its extractive and suppressive nature is further highlighted, potentially reclassifying it as a pure Snare from the outset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_discretion, conceptual, 'Ambiguity regarding the legal and moral legitimacy of British interpretive discretion.').

omega_variable(
    impact_on_state_formation,
    'To what extent did British interpretive discretion, rather than the mandate''s explicit terms, shape the eventual state formation and conflict dynamics in the region?',
    'Comparative historical analysis with other mandates where interpretive authority was more constrained, or detailed counterfactual modeling of policy outcomes under fixed interpretations.',
    'If discretion was the primary driver, the constraint''s role as a Snare is reinforced, as it actively shaped outcomes to British advantage. If explicit terms were more influential, the constraint might lean more towards a Tangled Rope, with extraction tied to the text itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_on_state_formation, empirical, 'The causal weight of interpretive discretion versus textual content in shaping regional outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(balfour_mandate_instruments__mandatory_interpretive_discretion, 1920, 1948).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(balf_tr_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(balf_tr_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1925, 0.12).
narrative_ontology:measurement(balf_tr_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1930, 0.15).
narrative_ontology:measurement(balf_tr_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1935, 0.18).
narrative_ontology:measurement(balf_tr_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1939, 0.25).
narrative_ontology:measurement(balf_tr_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1945, 0.22).
narrative_ontology:measurement(balf_tr_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, theater_ratio, 1948, 0.2).

% Extraction over time
narrative_ontology:measurement(balf_be_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1920, 0.55).
narrative_ontology:measurement(balf_be_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1925, 0.58).
narrative_ontology:measurement(balf_be_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1930, 0.62).
narrative_ontology:measurement(balf_be_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1935, 0.64).
narrative_ontology:measurement(balf_be_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1939, 0.68).
narrative_ontology:measurement(balf_be_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1945, 0.66).
narrative_ontology:measurement(balf_be_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, base_extractiveness, 1948, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(balf_su_t1920, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement(balf_su_t1925, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1925, 0.65).
narrative_ontology:measurement(balf_su_t1930, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1930, 0.7).
narrative_ontology:measurement(balf_su_t1935, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1935, 0.72).
narrative_ontology:measurement(balf_su_t1939, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1939, 0.78).
narrative_ontology:measurement(balf_su_t1945, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1945, 0.76).
narrative_ontology:measurement(balf_su_t1948, balfour_mandate_instruments__mandatory_interpretive_discretion, suppression_requirement, 1948, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(balfour_mandate_instruments__mandatory_interpretive_discretion, enforcement_mechanism).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__jewish_national_home_primacy).
narrative_ontology:affects_constraint(balfour_mandate_instruments__mandatory_interpretive_discretion, balfour_mandate_instruments__dual_obligation_indigenous_rights).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'balfour_mandate_instruments' kernel. This reading focuses on British interpretive discretion, while sibling readings emphasize Jewish national home primacy or indigenous rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
