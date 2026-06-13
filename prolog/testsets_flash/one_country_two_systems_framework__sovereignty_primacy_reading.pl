% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__sovereignty_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__sovereignty_primacy_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: one_country_two_systems_framework__sovereignty_primacy_reading
 *   human_readable: One Country, Two Systems: Sovereignty Primacy Reading
 *   domain: constitutional_law/political_systems/state_sovereignty
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty primacy' reading of the 'One
 *   Country, Two Systems' framework, where Hong Kong's autonomy is understood
 *   as delegated by and revocable through the PRC's sovereign authority.
 *   National security and territorial integrity are paramount and override
 *   local autonomy when conflicts arise. This reading has led to the
 *   imposition of the National Security Law, the operation of mainland
 *   enforcement agents in Hong Kong, and a significant erosion of political
 *   freedoms and judicial independence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, 0.85).
domain_priors:suppression_score(one_country_two_systems_framework__sovereignty_primacy_reading, 0.9).
domain_priors:theater_ratio(one_country_two_systems_framework__sovereignty_primacy_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(one_country_two_systems_framework__sovereignty_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__sovereignty_primacy_reading, snare).
narrative_ontology:human_readable(one_country_two_systems_framework__sovereignty_primacy_reading, "One Country, Two Systems: Sovereignty Primacy Reading").
narrative_ontology:topic_domain(one_country_two_systems_framework__sovereignty_primacy_reading, "constitutional_law/political_systems/state_sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__sovereignty_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__sovereignty_primacy_reading, '84f47807-902b-43fb-9677-8cbf230cd8d0').
narrative_ontology:cs_kernel_codification('84f47807-902b-43fb-9677-8cbf230cd8d0', fixed_text).
narrative_ontology:cs_authority_grounding('84f47807-902b-43fb-9677-8cbf230cd8d0', lineage).
narrative_ontology:cs_interpretation_layer_present('84f47807-902b-43fb-9677-8cbf230cd8d0').
narrative_ontology:cs_reading_relation('84f47807-902b-43fb-9677-8cbf230cd8d0', one_country_two_systems_framework__autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('84f47807-902b-43fb-9677-8cbf230cd8d0', one_country_two_systems_framework__balanced_coexistence_reading, influences).
narrative_ontology:cs_axiom('84f47807-902b-43fb-9677-8cbf230cd8d0', foundational, prc_sovereignty_is_absolute).
narrative_ontology:cs_axiom_status(prc_sovereignty_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('84f47807-902b-43fb-9677-8cbf230cd8d0', prc_sovereignty_is_absolute, deontological).
narrative_ontology:cs_axiom('84f47807-902b-43fb-9677-8cbf230cd8d0', foundational, national_security_overrides_local_law).
narrative_ontology:cs_axiom_status(national_security_overrides_local_law, holdable).
narrative_ontology:cs_axiom_grounding('84f47807-902b-43fb-9677-8cbf230cd8d0', national_security_overrides_local_law, conventional).
narrative_ontology:cs_reference_frame('84f47807-902b-43fb-9677-8cbf230cd8d0', prc_unquestioned_sovereignty).
narrative_ontology:cs_drift_state('84f47807-902b-43fb-9677-8cbf230cd8d0', post_national_security_law_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('84f47807-902b-43fb-9677-8cbf230cd8d0', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__sovereignty_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__sovereignty_primacy_reading, international_human_rights_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts ultimate sovereign authority over Hong Kong, interpreting the Basic Law and enacting national security legislation. Benefits from increased control and perceived stability, consolidating power.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, prc_central_government, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Bear the direct costs of reduced civil liberties, freedom of speech, and assembly. Their identity is tied to Hong Kong's unique system, making exit difficult despite increasing pressure.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_citizens, payer,
    powerless, biographical, identity_locked, local).

% Experiences erosion of judicial independence, particularly in national security cases, where decisions are subject to interpretation by Beijing. Judges face pressure to align with central government directives.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_judiciary, payer,
    organized, generational, constrained, local).

% Critique the erosion of human rights and rule of law in Hong Kong, but lack direct enforcement power. Their advocacy is increasingly suppressed within Hong Kong itself.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_human_rights_advocates, excluded,
    organized, generational, analytical, global).

% Benefits from alignment with the PRC Central Government, gaining political influence and economic opportunities. Actively supports the sovereignty primacy reading and its enforcement.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_pro_beijing_establishment, beneficiary,
    powerful, biographical, mobile, local).

% Monitors the stability and legal predictability of Hong Kong. While benefiting from its financial infrastructure, they face increasing uncertainty and potential risks from the erosion of rule of law, leading some to consider relocating operations.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__sovereignty_primacy_reading, international_business_community, observer,
    institutional, immediate, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To integrate Hong Kong into the PRC while theoretically preserving its distinct economic and social systems, ensuring stability and national unity under a single sovereign authority.
% TRANSFER_FUNCTION: Transfers ultimate legal and political authority from Hong Kong's autonomous institutions to the PRC Central Government, and transfers civil liberties from Hong Kong citizens to the state in the name of national security.
% ABSENT_VOICES: Pro-democracy activists, independent media, and international legal bodies who advocate for Hong Kong's autonomy and human rights are increasingly silenced or excluded from the political and legal discourse within Hong Kong.
% DISAPPEARANCE_RATIONALE: If this reading of the framework disappeared overnight, Hong Kong's legal and political landscape would immediately revert to a more autonomous state, with a resurgence of civil liberties and judicial independence. The PRC's direct control mechanisms would collapse, leading to a significant reorganization of power dynamics and potentially renewed protests.
% FOUNDING_PROBLEM: The problem of integrating a capitalist, democratic-leaning Hong Kong into a socialist, authoritarian China after the 1997 handover, while maintaining its economic vitality and international status.
% FOUNDING_PROBLEM_CORROBORATION: The PRC Central Government claims the founding problem (national security and stability) is live and requires this reading. Hong Kong citizens and international observers argue that the original problem of integration was largely solved, and this reading has created new problems of political repression, as evidenced by mass protests and international condemnation.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__sovereignty_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__sovereignty_primacy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__sovereignty_primacy_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(one_country_two_systems_framework__sovereignty_primacy_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__sovereignty_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__sovereignty_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the framework, under this reading, enables the PRC to appropriate significant political and legal autonomy from Hong Kong. Suppression (0.90) is severe due to the active enforcement of national security laws, leading to arrests, restrictions on speech, and the suppression of dissent. The theater ratio (0.60) reflects that while the 'Two Systems' rhetoric persists, the practical reality of 'One Country' (PRC sovereignty) increasingly dominates, making the autonomy claims largely performative. The metrics show a clear escalation over the 15-year interval, reflecting the increasing assertiveness of the sovereignty primacy reading.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the PRC Central Government, this framework is a legitimate exercise of sovereign authority, ensuring national security and stability (a 'rope' or even 'mountain' of statecraft). From the perspective of Hong Kong citizens and the judiciary, it is a coercive mechanism that systematically dismantles their guaranteed freedoms and legal independence (a 'snare'). The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The PRC Central Government and the Hong Kong pro-Beijing establishment are clear beneficiaries (d=0.0-0.1), gaining control and stability. Hong Kong citizens, the judiciary, and international human rights advocates are victims (d=0.9-1.0), bearing the costs of lost freedoms and legal erosion. The constraint subsidizes the central government's control while extracting from Hong Kong's autonomous institutions and civil society.
 *
 * MANDATROPHY ANALYSIS:
 *   The original mandate of 'One Country, Two Systems' was to preserve Hong Kong's distinct system and freedoms while integrating it into China. Under the sovereignty primacy reading, the 'Two Systems' aspect has atrophied, becoming largely performative, while the 'One Country' aspect has intensified into a mechanism for direct control. This prevents mislabeling a coercive extraction mechanism as a coordination framework by highlighting the shift from a balanced arrangement to one dominated by central authority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_autonomy_ambiguity,
    'Is Hong Kong''s autonomy a fundamental right or a delegated privilege?',
    'International legal adjudication or a shift in PRC constitutional interpretation that explicitly redefines the Basic Law''s status.',
    'If autonomy is a fundamental right, the current framework is a snare; if a delegated privilege, it is a tangled rope where the PRC is the legitimate agenda-setter.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_vs_autonomy_ambiguity, conceptual, 'Ambiguity in the legal nature of Hong Kong''s autonomy.').

omega_variable(
    autonomy_primacy_reading_delta,
    'What would change if the autonomy_primacy_reading of ''One Country, Two Systems'' were adopted?',
    'Analysis of counterfactual legal and political outcomes under the autonomy_primacy_reading.',
    'The constraint would shift from a snare to a rope or tangled rope, with significantly lower extractiveness and suppression, and a stronger, independent judiciary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(autonomy_primacy_reading_delta, conceptual, 'Impact of adopting the autonomy_primacy_reading.').

omega_variable(
    balanced_coexistence_reading_delta,
    'What would change if the balanced_coexistence_reading of ''One Country, Two Systems'' were adopted?',
    'Analysis of counterfactual legal and political outcomes under the balanced_coexistence_reading.',
    'The constraint would likely become a tangled rope, with ongoing political negotiation and less absolute enforcement of national security, but still with potential for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(balanced_coexistence_reading_delta, conceptual, 'Impact of adopting the balanced_coexistence_reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__sovereignty_primacy_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t0, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(one__tr_t5, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 5, 0.25).
narrative_ontology:measurement(one__tr_t10, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 10, 0.45).
narrative_ontology:measurement(one__tr_t15, one_country_two_systems_framework__sovereignty_primacy_reading, theater_ratio, 15, 0.6).

% Extraction over time
narrative_ontology:measurement(one__be_t0, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(one__be_t5, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(one__be_t10, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(one__be_t15, one_country_two_systems_framework__sovereignty_primacy_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t0, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(one__su_t5, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 5, 0.5).
narrative_ontology:measurement(one__su_t10, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(one__su_t15, one_country_two_systems_framework__sovereignty_primacy_reading, suppression_requirement, 15, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__sovereignty_primacy_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_basic_law_interpretation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_electoral_system).
narrative_ontology:affects_constraint(one_country_two_systems_framework__sovereignty_primacy_reading, hong_kong_press_freedom).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'One Country, Two Systems' framework, focusing on the primacy of PRC sovereignty. Other readings (autonomy_primacy_reading, balanced_coexistence_reading) exist as separate constraints, each with distinct structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
