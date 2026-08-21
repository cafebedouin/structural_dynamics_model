% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__liberal_institutional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__liberal_institutional_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: rbio_practice_norm_complex__liberal_institutional_reading
 *   human_readable: RBIO Norms: Liberal Institutional Reading
 *   domain: international_relations/international_law/political_economy
 *
 * SUMMARY:
 *   This constraint story represents the 'liberal institutional' reading of
 *   the RBIO (Rules-Based International Order) practice-norm complex. This
 *   reading asserts that RBIO norms are universal, consent-based, and
 *   legitimately revisable through multilateral processes. It views
 *   enforcement selectivity (e.g., sanctions against some states but not
 *   others) as a practical capacity problem, not a fundamental flaw in the
 *   system's legitimacy. Interventions are justified when authorized by the
 *   UNSC or in cases of grave atrocities, and economic conditionality is seen
 *   as an acceptable contractual term. The system is understood to provide
 *   genuine coordination, despite acknowledged imperfections in its
 *   application.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, 0.65).
domain_priors:suppression_score(rbio_practice_norm_complex__liberal_institutional_reading, 0.75).
domain_priors:theater_ratio(rbio_practice_norm_complex__liberal_institutional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__liberal_institutional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__liberal_institutional_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__liberal_institutional_reading, "RBIO Norms: Liberal Institutional Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__liberal_institutional_reading, "international_relations/international_law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__liberal_institutional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__liberal_institutional_reading, '7245d3e5-c73b-4a2a-b29b-6d486833e953').
narrative_ontology:cs_kernel_codification('7245d3e5-c73b-4a2a-b29b-6d486833e953', formalized).
narrative_ontology:cs_authority_grounding('7245d3e5-c73b-4a2a-b29b-6d486833e953', lineage).
narrative_ontology:cs_interpretation_layer_present('7245d3e5-c73b-4a2a-b29b-6d486833e953').
narrative_ontology:cs_reading_relation('7245d3e5-c73b-4a2a-b29b-6d486833e953', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_reading_relation('7245d3e5-c73b-4a2a-b29b-6d486833e953', rbio_practice_norm_complex__sovereignty_maximalist_reading, coexists_with).
narrative_ontology:cs_axiom('7245d3e5-c73b-4a2a-b29b-6d486833e953', foundational, multilateral_legitimacy_principle).
narrative_ontology:cs_axiom_status(multilateral_legitimacy_principle, holdable).
narrative_ontology:cs_axiom_grounding('7245d3e5-c73b-4a2a-b29b-6d486833e953', multilateral_legitimacy_principle, conventional).
narrative_ontology:cs_axiom('7245d3e5-c73b-4a2a-b29b-6d486833e953', foundational, humanitarian_intervention_exception).
narrative_ontology:cs_axiom_status(humanitarian_intervention_exception, holdable).
narrative_ontology:cs_axiom_grounding('7245d3e5-c73b-4a2a-b29b-6d486833e953', humanitarian_intervention_exception, deontological).
narrative_ontology:cs_reference_frame('7245d3e5-c73b-4a2a-b29b-6d486833e953', un_charter_system_ideal).
narrative_ontology:cs_drift_state('7245d3e5-c73b-4a2a-b29b-6d486833e953', contemporary_geopolitical_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7245d3e5-c73b-4a2a-b29b-6d486833e953', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states).
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__liberal_institutional_reading, international_contractors).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_in_targeted_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that advocate for and participate in interventions or sanctions, often justifying them on humanitarian grounds or as upholding international law. They benefit from the stability and predictability of the international system, and sometimes from economic opportunities arising from interventions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, intervening_states, beneficiary).

% States that are subject to international sanctions or military intervention. They bear the direct economic and social costs, and their sovereignty is curtailed. Their options for exit are limited, often involving regime change or significant policy shifts dictated externally.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(rbio_practice_norm_complex__liberal_institutional_reading, targeted_states, excluded).

% The populations within targeted states who suffer the humanitarian consequences of sanctions, conflict, or instability. They have virtually no agency in the international processes that affect them and are trapped by their geography and political circumstances.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, civilian_populations_in_targeted_states, payer,
    powerless, immediate, trapped, local).

% The primary body for authorizing international interventions and sanctions under the UN Charter. Its decisions are seen as legitimate by this reading, even if its capacity for consistent enforcement is limited. Its permanent members hold veto power.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Private companies and NGOs that secure contracts for reconstruction, security, or humanitarian aid in post-intervention or sanctioned environments. They benefit financially and operationally from the enforcement of RBIO norms, often operating with significant autonomy.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_contractors, beneficiary,
    organized, biographical, mobile, global).

% Organizations that provide aid and advocate for human rights in conflict zones and sanctioned states. They observe the impact of RBIO enforcement, often navigating complex political landscapes to deliver assistance, and sometimes critique the selectivity of interventions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__liberal_institutional_reading, international_humanitarian_organizations, observer,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for collective action to maintain international peace and security, protect human rights, and manage global economic interactions, preventing unilateral aggression and promoting shared norms.
% TRANSFER_FUNCTION: Transfers sovereignty and resources from targeted states to intervening states and international bodies, often through sanctions, asset freezes, or post-conflict reconstruction contracts. It also transfers the costs of enforcement and humanitarian crises to civilian populations.
% ABSENT_VOICES: The populations of targeted states, who bear the brunt of interventions and sanctions, are largely absent from the multilateral processes that determine their fate. Their perspectives are often mediated through state actors or humanitarian organizations, rather than directly represented.
% DISAPPEARANCE_RATIONALE: If RBIO norms vanished, the international system would revert to a more anarchic state, with increased unilateral action, reduced collective security, and a breakdown of multilateral cooperation. Intervening states would lose a key legitimizing framework, and targeted states would face unconstrained power dynamics.
% FOUNDING_PROBLEM: The problem of preventing interstate aggression, managing global commons, and responding to humanitarian crises in a way that is legitimate and collectively supported, avoiding a return to great power competition and unilateralism.
% FOUNDING_PROBLEM_CORROBORATION: The UN Charter and subsequent international legal instruments attest to the founding problem. International legal scholars, diplomats from non-intervening states, and many international organizations corroborate that the problem of global governance and collective security remains live, even if the mechanisms are imperfect.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__liberal_institutional_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__liberal_institutional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__liberal_institutional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rbio_practice_norm_complex__liberal_institutional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__liberal_institutional_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__liberal_institutional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__liberal_institutional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is moderate-high, reflecting the real costs imposed on targeted states and populations, even if this reading frames them as legitimate consequences or necessary evils. Suppression (0.75) is high due to the coercive nature of sanctions and interventions, which actively limit the options of targeted actors. The theater ratio (0.20) is low because this reading genuinely believes in the functional purpose of the norms and their enforcement, seeing performative aspects as minimal. Accessibility collapse (0.60) is moderate, as targeted states have limited, costly alternatives to compliance. Resistance (0.55) is also moderate, coming from targeted states and some civil society groups, but not enough to fundamentally challenge the system's perceived legitimacy from this perspective.
 *
 * PERSPECTIVAL GAP:
 *   The liberal institutional reading acknowledges that targeted states and their populations experience the constraint as highly extractive and suppressive. However, it frames these outcomes as legitimate, necessary, or due to capacity limitations, rather than as evidence of a flawed or extractive system. This contrasts sharply with the 'hegemonic extraction' reading, which would interpret the same facts as evidence of systemic injustice.
 *
 * DIRECTIONALITY LOGIC:
 *   Intervening states and international contractors are beneficiaries, gaining stability, influence, and economic opportunities. Targeted states and their civilian populations are victims, bearing the direct costs of sanctions and interventions. The UN Security Council acts as an agenda-setter, legitimizing actions within the framework. International humanitarian organizations serve as observers, documenting impacts and advocating for affected populations.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_vs_legitimacy_problem,
    'Is the observed selectivity in RBIO enforcement primarily a capacity problem (lack of resources/will to enforce universally) or a legitimacy problem (selective enforcement serving specific state interests)?',
    'Empirical analysis of enforcement patterns across different geopolitical contexts and power asymmetries, coupled with a review of decision-making processes within multilateral institutions for evidence of bias or capture.',
    'If primarily a capacity problem, the liberal institutional reading''s core premise holds, and solutions focus on strengthening multilateral institutions. If primarily a legitimacy problem, the ''hegemonic extraction'' reading gains significant support, challenging the fundamental fairness of the RBIO.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_vs_legitimacy_problem, empirical, 'Distinguishing between practical limitations and structural bias in RBIO enforcement.').

omega_variable(
    consent_vs_coercion_in_norm_formation,
    'To what extent are RBIO norms truly ''consent-based'' for all states, particularly those with less power, or are they effectively imposed through coercive power dynamics?',
    'Historical analysis of norm-setting processes, examining the agency and influence of less powerful states, and assessing the ''costs of non-participation'' for states that ostensibly ''consent'' to norms.',
    'If consent is largely coerced, the ''sovereignty maximalist'' and ''hegemonic extraction'' readings are strengthened, undermining the liberal institutional claim of universal, voluntary adherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_vs_coercion_in_norm_formation, conceptual, 'Assessing the voluntariness of state consent to international norms.').

omega_variable(
    kernel_contest_rbio_framing,
    'Which reading of the RBIO practice-norm complex (liberal institutional, hegemonic extraction, or sovereignty maximalist) best captures the structural reality of international order?',
    'A comprehensive, multi-criteria evaluation by an independent analytical observer, integrating empirical data on enforcement, power dynamics, and state behavior with conceptual analysis of legitimacy claims and normative frameworks.',
    'The resolution would determine the dominant classification of the RBIO, shifting the policy debate and academic discourse towards addressing either capacity gaps, structural extraction, or sovereignty infringements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_contest_rbio_framing, conceptual, 'The overarching contest between different interpretations of the RBIO''s fundamental nature.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__liberal_institutional_reading, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_tr_t1990, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(rbio_tr_t1998, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 1998, 0.18).
narrative_ontology:measurement(rbio_tr_t2006, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2006, 0.2).
narrative_ontology:measurement(rbio_tr_t2014, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2014, 0.22).
narrative_ontology:measurement(rbio_tr_t2024, rbio_practice_norm_complex__liberal_institutional_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rbio_be_t1990, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1990, 0.55).
narrative_ontology:measurement(rbio_be_t1998, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 1998, 0.6).
narrative_ontology:measurement(rbio_be_t2006, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2006, 0.63).
narrative_ontology:measurement(rbio_be_t2014, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2014, 0.66).
narrative_ontology:measurement(rbio_be_t2024, rbio_practice_norm_complex__liberal_institutional_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(rbio_su_t1990, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(rbio_su_t1998, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 1998, 0.7).
narrative_ontology:measurement(rbio_su_t2006, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2006, 0.73).
narrative_ontology:measurement(rbio_su_t2014, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2014, 0.76).
narrative_ontology:measurement(rbio_su_t2024, rbio_practice_norm_complex__liberal_institutional_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__liberal_institutional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__hegemonic_extraction_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__liberal_institutional_reading, rbio_practice_norm_complex__sovereignty_maximalist_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'rbio_practice_norm_complex' kernel. Each reading offers a distinct structural interpretation of the same set of international norms and practices, leading to different ε values and classifications. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
