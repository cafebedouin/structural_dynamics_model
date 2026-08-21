% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__sovereigntist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__sovereigntist_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__sovereigntist_reading
 *   human_readable: Rome Statute: Sovereigntist Reading of ICC Jurisdiction
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, 0.68).
domain_priors:suppression_score(rome_statute_jurisdiction__sovereigntist_reading, 0.75).
domain_priors:theater_ratio(rome_statute_jurisdiction__sovereigntist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__sovereigntist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__sovereigntist_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__sovereigntist_reading, "Rome Statute: Sovereigntist Reading of ICC Jurisdiction").
narrative_ontology:topic_domain(rome_statute_jurisdiction__sovereigntist_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__sovereigntist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__sovereigntist_reading, 'fe67f47e-0695-477a-952c-1d498e816a69').
narrative_ontology:cs_kernel_codification('fe67f47e-0695-477a-952c-1d498e816a69', fixed_text).
narrative_ontology:cs_authority_grounding('fe67f47e-0695-477a-952c-1d498e816a69', lineage).
narrative_ontology:cs_interpretation_layer_present('fe67f47e-0695-477a-952c-1d498e816a69').
narrative_ontology:cs_reading_relation('fe67f47e-0695-477a-952c-1d498e816a69', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('fe67f47e-0695-477a-952c-1d498e816a69', rome_statute_jurisdiction__hybrid_complementarity_reading, coexists_with).
narrative_ontology:cs_axiom('fe67f47e-0695-477a-952c-1d498e816a69', foundational, state_sovereignty_primacy).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy, holdable).
narrative_ontology:cs_axiom_grounding('fe67f47e-0695-477a-952c-1d498e816a69', state_sovereignty_primacy, deontological).
narrative_ontology:cs_axiom('fe67f47e-0695-477a-952c-1d498e816a69', foundational, consent_as_basis_for_jurisdiction).
narrative_ontology:cs_axiom_status(consent_as_basis_for_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('fe67f47e-0695-477a-952c-1d498e816a69', consent_as_basis_for_jurisdiction, conventional).
narrative_ontology:cs_reference_frame('fe67f47e-0695-477a-952c-1d498e816a69', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('fe67f47e-0695-477a-952c-1d498e816a69', contemporary_international_law, gap(stable, minor, true)).
narrative_ontology:cs_created_at('fe67f47e-0695-477a-952c-1d498e816a69', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, non_consenting_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__sovereigntist_reading, nationals_of_non_party_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities_in_non_consenting_states).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__sovereigntist_reading, universal_justice_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert their sovereign right to withhold consent from the ICC's jurisdiction, thereby protecting their nationals from prosecution by the Court unless referred by the UN Security Council. They actively defend this interpretation of the Rome Statute.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, non_consenting_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Individuals from states that have not ratified the Rome Statute are generally immune from ICC jurisdiction under this reading, unless their case is referred by the UN Security Council. This provides a shield against international prosecution.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, nationals_of_non_party_states, beneficiary,
    moderate, biographical, constrained, global).

% These individuals are denied access to the ICC for justice, as their state's non-consent (or the nationality of the perpetrator) prevents the Court from exercising jurisdiction. Their only recourse is often a non-functional national justice system.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, victims_of_atrocities_in_non_consenting_states, payer,
    powerless, immediate, trapped, local).

% These groups advocate for a broader interpretation of ICC jurisdiction, seeing the sovereigntist reading as an impediment to universal justice and accountability for international crimes. They bear the cost of a limited ICC reach.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, universal_justice_advocates, payer,
    organized, generational, constrained, global).

% The ICC, as an institution, must operate within the jurisdictional limits defined by the Rome Statute. Under this reading, its authority is strictly conditional on state consent or UN Security Council referral, limiting its ability to pursue cases.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, international_criminal_court_icc, agenda_setter,
    institutional, generational, constrained, global).

% The UNSC retains the power to refer situations in non-party states to the ICC, thereby overriding the consent requirement. This power is exercised selectively and often subject to political considerations.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__sovereigntist_reading, un_security_council, agenda_setter,
    institutional, generational, arbitrage, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a framework for international criminal justice that encourages broad state participation by respecting the fundamental principle of state sovereignty and requiring explicit consent for jurisdiction.
% TRANSFER_FUNCTION: Transfers the primary authority for prosecuting international crimes from the International Criminal Court to national jurisdictions (or effectively grants impunity) for states that have not consented to the Rome Statute.
% ABSENT_VOICES: Victims of atrocities in non-consenting states are largely excluded from the direct mechanisms of international justice, as their access is contingent on state consent or political referral.
% DISAPPEARANCE_RATIONALE: If the requirement for strict sovereign consent vanished overnight, the ICC's jurisdiction would become truly universal. This would fundamentally alter international law, state sovereignty, and the geopolitical landscape, leading to immediate legal challenges and potential prosecutions in many non-consenting states.
% FOUNDING_PROBLEM: To create an effective international criminal court that could achieve widespread ratification and legitimacy among diverse states, balancing the ideal of universal justice with the deeply entrenched principle of state sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars, historical records of the Rome Statute negotiations, and statements from various state foreign ministries (especially non-parties) corroborate the ongoing tension between universal justice and state sovereignty as a live founding problem.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__sovereigntist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__sovereigntist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__sovereigntist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(rome_statute_jurisdiction__sovereigntist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__sovereigntist_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rome_statute_jurisdiction__sovereigntist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rome_statute_jurisdiction__sovereigntist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a distinct reading of the Rome Statute''s jurisdictional framework, or merely a political stance?',
    'Analysis of legal scholarship and state practice: if the interpretation is consistently articulated with a coherent legal basis, it is a distinct reading. If it is merely opportunistic, it is a political stance.',
    'If a distinct reading, it represents a stable structural feature of international law. If a political stance, its persistence is contingent on shifting political power, not legal coherence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint as a specific legal reading of the Rome Statute.').

omega_variable(
    sovereignty_vs_justice_balance,
    'Does this sovereigntist reading strike an optimal balance between state sovereignty and the pursuit of international criminal justice?',
    'Empirical assessment of accountability outcomes (number of prosecutions, victim satisfaction) versus state participation rates and perceived legitimacy of the ICC over time.',
    'If the balance is suboptimal, it suggests the constraint is overly extractive from justice, potentially leading to calls for reform or alternative mechanisms. If optimal, it reinforces the current framework''s legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_justice_balance, preference, 'Assesses the normative balance between sovereignty and justice in this reading.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of universal jurisdiction structural (legal barriers) or internalized by states (ideological commitment to absolute sovereignty)?',
    'Post-treaty-withdrawal behavior: if states continue to resist universal jurisdiction even after withdrawing from the Rome Statute, it suggests an internalized ideological commitment. If resistance diminishes, it''s primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as states carry the suppression with them. If structural, removing legal barriers would more easily open alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression of universal jurisdiction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__sovereigntist_reading, 1998, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 1998, 0.15).
narrative_ontology:measurement(rome_tr_t2003, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2003, 0.15).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2008, 0.15).
narrative_ontology:measurement(rome_tr_t2013, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2013, 0.15).
narrative_ontology:measurement(rome_tr_t2018, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2018, 0.15).
narrative_ontology:measurement(rome_tr_t2023, rome_statute_jurisdiction__sovereigntist_reading, theater_ratio, 2023, 0.15).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 1998, 0.65).
narrative_ontology:measurement(rome_be_t2003, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2003, 0.66).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2008, 0.67).
narrative_ontology:measurement(rome_be_t2013, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2013, 0.67).
narrative_ontology:measurement(rome_be_t2018, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2018, 0.68).
narrative_ontology:measurement(rome_be_t2023, rome_statute_jurisdiction__sovereigntist_reading, base_extractiveness, 2023, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 1998, 0.72).
narrative_ontology:measurement(rome_su_t2003, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2003, 0.73).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2008, 0.74).
narrative_ontology:measurement(rome_su_t2013, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2013, 0.74).
narrative_ontology:measurement(rome_su_t2018, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2018, 0.75).
narrative_ontology:measurement(rome_su_t2023, rome_statute_jurisdiction__sovereigntist_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__sovereigntist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, icc_prosecution_of_non_party_nationals).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, unsc_referral_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__sovereigntist_reading, rome_statute_jurisdiction__hybrid_complementarity_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
