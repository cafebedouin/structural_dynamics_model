% ============================================================================
% CONSTRAINT STORY: fifth_republic_constitution__cohabitation_equilibrium_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fifth_republic_constitution__cohabitation_equilibrium_reading, []).

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
 *   constraint_id: fifth_republic_constitution__cohabitation_equilibrium_reading
 *   human_readable: Fifth Republic Constitution: Cohabitation Equilibrium Reading
 *   domain: constitutional_law/political_systems
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45).
domain_priors:suppression_score(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.55).
domain_priors:theater_ratio(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fifth_republic_constitution__cohabitation_equilibrium_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fifth_republic_constitution__cohabitation_equilibrium_reading, tangled_rope).
narrative_ontology:human_readable(fifth_republic_constitution__cohabitation_equilibrium_reading, "Fifth Republic Constitution: Cohabitation Equilibrium Reading").
narrative_ontology:topic_domain(fifth_republic_constitution__cohabitation_equilibrium_reading, "constitutional_law/political_systems").

domain_priors:requires_active_enforcement(fifth_republic_constitution__cohabitation_equilibrium_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fifth_republic_constitution__cohabitation_equilibrium_reading, '44581c89-76a0-4b03-b81d-9113ceb3c04c').
narrative_ontology:cs_kernel_codification('44581c89-76a0-4b03-b81d-9113ceb3c04c', fixed_text).
narrative_ontology:cs_authority_grounding('44581c89-76a0-4b03-b81d-9113ceb3c04c', lineage).
narrative_ontology:cs_interpretation_layer_present('44581c89-76a0-4b03-b81d-9113ceb3c04c').
narrative_ontology:cs_reading_relation('44581c89-76a0-4b03-b81d-9113ceb3c04c', fifth_republic_constitution__hyper_presidential_reading, coexists_with).
narrative_ontology:cs_reading_relation('44581c89-76a0-4b03-b81d-9113ceb3c04c', fifth_republic_constitution__parliamentary_constraint_reading, coexists_with).
narrative_ontology:cs_axiom('44581c89-76a0-4b03-b81d-9113ceb3c04c', foundational, executive_power_shared_and_negotiated).
narrative_ontology:cs_axiom_status(executive_power_shared_and_negotiated, holdable).
narrative_ontology:cs_axiom_grounding('44581c89-76a0-4b03-b81d-9113ceb3c04c', executive_power_shared_and_negotiated, conventional).
narrative_ontology:cs_axiom('44581c89-76a0-4b03-b81d-9113ceb3c04c', foundational, parliamentary_accountability_to_electorate_is_paramount).
narrative_ontology:cs_axiom_status(parliamentary_accountability_to_electorate_is_paramount, holdable).
narrative_ontology:cs_axiom_grounding('44581c89-76a0-4b03-b81d-9113ceb3c04c', parliamentary_accountability_to_electorate_is_paramount, deontological).
narrative_ontology:cs_reference_frame('44581c89-76a0-4b03-b81d-9113ceb3c04c', balanced_executive_power).
narrative_ontology:cs_drift_state('44581c89-76a0-4b03-b81d-9113ceb3c04c', contemporary_political_practice, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('44581c89-76a0-4b03-b81d-9113ceb3c04c', '').
narrative_ontology:cs_kernel_id(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_france).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_of_france).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, policy_coherence).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electorate).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly).
narrative_ontology:constraint_victim(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As head of state, leads foreign policy and appoints the Prime Minister. During cohabitation, their domestic policy agenda is significantly constrained by the parliamentary majority, requiring negotiation and compromise.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, president_of_france, agenda_setter,
    institutional, generational, constrained, national).

% As head of government, leads domestic policy and is accountable to the National Assembly. During cohabitation, they must navigate the President's constitutional prerogatives, particularly in foreign affairs and defense.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, prime_minister_of_france, agenda_setter,
    institutional, generational, constrained, national).

% Holds the power to censure the Prime Minister and pass legislation. Its majority determines the Prime Minister, and during cohabitation, it acts as a significant check on presidential power, but also bears the burden of legislative compromise.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(fifth_republic_constitution__cohabitation_equilibrium_reading, national_assembly, payer).

% Adjudicates constitutional disputes and ensures the proper functioning of institutions, playing a critical role in enforcing the balance of powers, especially during cohabitation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, constitutional_council, observer,
    institutional, civilizational, analytical, national).

% Votes for both the President and the National Assembly. While their votes determine the political landscape, they bear the costs of potential policy incoherence, delays, or deadlocks that can arise from the negotiated authority allocation during cohabitation.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, french_electorate, payer,
    powerless, biographical, constrained, national).

% Observe the stability and coherence of French foreign policy, which can be affected by the dynamics of cohabitation, particularly when the President and Prime Minister have differing views.
narrative_ontology:constraint_stakeholder(fifth_republic_constitution__cohabitation_equilibrium_reading, international_partners, observer,
    organized, immediate, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide stable and democratically accountable governance in France by balancing the directly elected President's authority (representing national will) with the Prime Minister's accountability to the National Assembly (representing popular representation), particularly during periods where their political majorities differ.
% TRANSFER_FUNCTION: Transfers authority and policy initiative between the President and the Prime Minister/National Assembly, depending on electoral outcomes and the specific policy domain (e.g., foreign policy to President, domestic policy to Prime Minister), necessitating a negotiated allocation of power.
% ABSENT_VOICES: Proponents of a purely presidential system (where the President controls both executive and legislative agendas) or a purely parliamentary system (where the President is largely ceremonial) are structurally excluded from the current constitutional framework. They would argue for a clearer, less ambiguous locus of power.
% DISAPPEARANCE_RATIONALE: If the constitutional framework for cohabitation vanished, France would immediately face a profound constitutional crisis regarding the locus of executive power. The entire political system would need to be fundamentally redefined, leading to widespread institutional and political reorganization.
% FOUNDING_PROBLEM: The instability and governmental paralysis of the Fourth Republic, characterized by frequent changes in government and a weak executive, which led to a desire for a stronger, more stable executive while retaining democratic accountability.
% FOUNDING_PROBLEM_CORROBORATION: Political historians, comparative constitutional scholars, and public opinion polls consistently reflect the historical context of Fourth Republic instability and the ongoing public desire for stable governance, corroborating the founding problem from outside the immediate political actors.
narrative_ontology:disappearance_verdict(fifth_republic_constitution__cohabitation_equilibrium_reading, world_rearranges).
narrative_ontology:founding_problem_status(fifth_republic_constitution__cohabitation_equilibrium_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fifth_republic_constitution__cohabitation_equilibrium_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fifth_republic_constitution__cohabitation_equilibrium_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).
:- end_tests(fifth_republic_constitution__cohabitation_equilibrium_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cohabitation_stability_ambiguity,
    'Is the cohabitation equilibrium a stable, intended feature of the Fifth Republic, or an unstable compromise that periodically strains the constitutional framework?',
    'Longitudinal analysis of constitutional crises and public satisfaction during cohabitation periods, compared to periods of unified government. Examination of constitutional reform proposals aimed at clarifying executive power.',
    'If primarily unstable, the constraint''s effective extractiveness (χ) is higher due to greater friction and policy costs. If stable, the coordination function is more robust, lowering χ.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cohabitation_stability_ambiguity, empirical, 'Whether cohabitation is a stable constitutional feature or a source of instability.').

omega_variable(
    policy_coherence_cost,
    'What is the quantifiable cost of policy incoherence and delays attributable to the negotiated authority allocation during cohabitation?',
    'Comparative economic and social policy analysis between cohabitation and unified government periods, controlling for external factors. Expert assessment of policy implementation effectiveness.',
    'Higher quantifiable costs would increase the measured extractiveness and strengthen the Tangled Rope classification, highlighting the burden on the French electorate and policy outcomes.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(policy_coherence_cost, empirical, 'The economic and social costs of policy incoherence during cohabitation.').

omega_variable(
    presidential_vs_parliamentary_primacy,
    'Is the Fifth Republic system fundamentally presidential with parliamentary checks, or parliamentary with a strong president, and how does this framing affect the perception of cohabitation?',
    'Analysis of constitutional jurisprudence, political science interpretations, and public discourse. This is a conceptual omega, resolved by adopting a specific interpretive framework.',
    'A ''hyper-presidential'' framing (sibling reading) would view cohabitation as an aberration, increasing perceived extractiveness from the President''s seat. A ''parliamentary-constraint'' framing (sibling reading) would view cohabitation as a necessary check, potentially lowering perceived extractiveness from the Assembly''s seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(presidential_vs_parliamentary_primacy, conceptual, 'Conceptual ambiguity regarding the ultimate locus of power in the Fifth Republic.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fifth_republic_constitution__cohabitation_equilibrium_reading, 1958, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fift_tr_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1958, 0.15).
narrative_ontology:measurement(fift_tr_t1971, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1971, 0.16).
narrative_ontology:measurement(fift_tr_t1984, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1984, 0.18).
narrative_ontology:measurement(fift_tr_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 1997, 0.22).
narrative_ontology:measurement(fift_tr_t2010, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(fift_tr_t2023, fifth_republic_constitution__cohabitation_equilibrium_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(fift_be_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1958, 0.4).
narrative_ontology:measurement(fift_be_t1971, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1971, 0.42).
narrative_ontology:measurement(fift_be_t1984, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1984, 0.44).
narrative_ontology:measurement(fift_be_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 1997, 0.48).
narrative_ontology:measurement(fift_be_t2010, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(fift_be_t2023, fifth_republic_constitution__cohabitation_equilibrium_reading, base_extractiveness, 2023, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fift_su_t1958, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1958, 0.5).
narrative_ontology:measurement(fift_su_t1971, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1971, 0.52).
narrative_ontology:measurement(fift_su_t1984, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1984, 0.54).
narrative_ontology:measurement(fift_su_t1997, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 1997, 0.58).
narrative_ontology:measurement(fift_su_t2010, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2010, 0.56).
narrative_ontology:measurement(fift_su_t2023, fifth_republic_constitution__cohabitation_equilibrium_reading, suppression_requirement, 2023, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fifth_republic_constitution__cohabitation_equilibrium_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__hyper_presidential_reading).
narrative_ontology:affects_constraint(fifth_republic_constitution__cohabitation_equilibrium_reading, fifth_republic_constitution__parliamentary_constraint_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the French Fifth Republic Constitution, focusing on the negotiated authority allocation during cohabitation periods. It is part of a constraint family that includes 'hyper_presidential_reading' and 'parliamentary_constraint_reading', each representing a distinct interpretation of the same constitutional kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
