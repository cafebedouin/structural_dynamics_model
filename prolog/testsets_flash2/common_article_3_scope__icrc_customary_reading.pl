% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__icrc_customary_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__icrc_customary_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: common_article_3_scope__icrc_customary_reading
 *   human_readable: Common Article 3 Scope (ICRC Customary Law Reading)
 *   domain: international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint represents the ICRC's reading of Common Article 3 (CA3)
 *   of the Geneva Conventions, which holds that the scope of CA3 is
 *   determined by evolving state practice and opinio juris, as tracked
 *   through customary international law. This reading allows for a dynamic
 *   expansion of CA3's application to non-international armed conflicts
 *   (NIACs) without formal treaty amendments. It functions as a procedural
 *   constraint on interpretation, enabling gradual adaptation of IHL. The
 *   constraint is classified as a Rope because it primarily serves a
 *   coordination function, allowing states and humanitarian actors to adapt
 *   to new conflict realities with relatively low extraction, though states
 *   bear the cost of an evolving legal obligation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.25).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.15).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope (ICRC Customary Law Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, 'cae1c87c-6a2d-426d-8559-539daaeac874').
narrative_ontology:cs_kernel_codification('cae1c87c-6a2d-426d-8559-539daaeac874', formalized).
narrative_ontology:cs_authority_grounding('cae1c87c-6a2d-426d-8559-539daaeac874', expertise).
narrative_ontology:cs_interpretation_layer_present('cae1c87c-6a2d-426d-8559-539daaeac874').
narrative_ontology:cs_reading_relation('cae1c87c-6a2d-426d-8559-539daaeac874', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('cae1c87c-6a2d-426d-8559-539daaeac874', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('cae1c87c-6a2d-426d-8559-539daaeac874', foundational, ihl_evolves_through_custom).
narrative_ontology:cs_axiom_status(ihl_evolves_through_custom, holdable).
narrative_ontology:cs_axiom_grounding('cae1c87c-6a2d-426d-8559-539daaeac874', ihl_evolves_through_custom, conventional).
narrative_ontology:cs_axiom('cae1c87c-6a2d-426d-8559-539daaeac874', secondary, icrc_as_custodian_of_custom).
narrative_ontology:cs_axiom_status(icrc_as_custodian_of_custom, holdable).
narrative_ontology:cs_axiom_grounding('cae1c87c-6a2d-426d-8559-539daaeac874', icrc_as_custodian_of_custom, conventional).
narrative_ontology:cs_reference_frame('cae1c87c-6a2d-426d-8559-539daaeac874', dynamic_customary_evolution).
narrative_ontology:cs_drift_state('cae1c87c-6a2d-426d-8559-539daaeac874', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('cae1c87c-6a2d-426d-8559-539daaeac874', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, icrc).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, humanitarian_organizations).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, victims_of_armed_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, evolving_standards_of_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponent and interpreter of customary international humanitarian law, including the scope of Common Article 3. They collect and analyze state practice and opinio juris to assert the evolving scope, influencing states without direct enforcement power.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    institutional, generational, constrained, global).

% Their practice and declarations (opinio juris) form the basis of customary international law. They bear the cost of compliance with an evolving standard, which can expand the scope of their obligations without formal treaty amendment. Exit means repudiating international law, which is costly.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions, payer,
    institutional, generational, constrained, global).

% Benefit from the expanded and clarified scope of CA3, which provides a legal framework for their operations and advocacy in a wider range of armed conflicts. They rely on the ICRC's interpretation to guide their work.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, humanitarian_organizations, beneficiary,
    organized, biographical, mobile, global).

% Benefit from the application of minimum humanitarian standards in conflicts that might not otherwise be covered by formal IHL. Their protection depends on the evolving interpretation of CA3.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, victims_of_armed_conflict, beneficiary,
    powerless, immediate, trapped, local).

% Refer to the ICRC's customary law studies and state practice when adjudicating cases involving CA3, lending judicial weight to the evolving interpretation. They do not set the scope but apply it.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mechanism for the gradual, consensual expansion of minimum humanitarian standards to new forms of armed violence, ensuring a baseline of protection without requiring formal treaty renegotiation.
% TRANSFER_FUNCTION: Transfers interpretive authority from strict state consent to a dynamic process of state practice and opinio juris, mediated by expert bodies like the ICRC, extending protection to more individuals.
% ABSENT_VOICES: States that resist the expansion of CA3's scope, particularly those engaged in internal conflicts they wish to define as purely domestic matters, are present in the 'states_parties' group but their dissenting voices are often outweighed by the cumulative weight of broader state practice and opinio juris.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the scope of CA3 would revert to a more restrictive, state-centric interpretation, leaving many victims of internal armed conflicts without clear IHL protection. Humanitarian organizations would lose a key legal basis for their advocacy, and the evolution of IHL would be significantly hampered.
% FOUNDING_PROBLEM: The original Geneva Conventions, including Common Article 3, were drafted in a specific historical context and did not fully anticipate the evolving nature of armed conflicts, particularly non-international armed conflicts (NIACs). The problem was how to adapt IHL to new realities without constant treaty amendment.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC, humanitarian organizations, and many legal scholars attest that the problem of adapting IHL to evolving conflict dynamics remains live. While some states may contest specific applications, the general need for an adaptive mechanism is widely acknowledged by international legal bodies and independent experts.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__icrc_customary_reading, 'none', 1).
narrative_ontology:epsilon_provenance(common_article_3_scope__icrc_customary_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__icrc_customary_reading_tests).
:- end_tests(common_article_3_scope__icrc_customary_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25) because the mechanism primarily facilitates coordination and adaptation, with states generally accepting the principle of customary law evolution. Suppression is also low (0.15) as compliance relies on states' self-interest in maintaining a stable international legal order and the persuasive authority of the ICRC, rather than direct coercion. Theater ratio is low (0.1) as the process is genuinely functional in adapting IHL. Accessibility collapse is moderate (0.7) because while states can resist specific interpretations, the overall framework of customary international law is well-established. Resistance is low (0.2) as the process is generally accepted, even if specific applications are debated.
 *
 * PERSPECTIVAL GAP:
 *   States that resist the expansion of CA3's scope would experience this constraint as more extractive, as it imposes obligations beyond their explicit consent. However, the overall benefit of a flexible IHL framework for international stability and humanitarian action means that even these states often tacitly accept the mechanism, if not every outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC and humanitarian organizations are beneficiaries, as this reading empowers their mission and provides a broader legal basis for protection. Victims of armed conflict are also beneficiaries, as the expanded scope offers them greater protection. States parties are payers, as they bear the cost of an evolving legal obligation that can expand their responsibilities. International courts act as observers, applying the evolving customary law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint actively resolves mandatrophy by providing a mechanism for IHL to adapt to new forms of conflict, preventing the core mandate of CA3 from becoming obsolete due to static interpretation. It ensures the 'founding problem' of adapting IHL remains 'live' by offering a procedural solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_practice_vs_opinio_juris_weight,
    'What is the relative weight given to ''state practice'' versus ''opinio juris'' in determining the evolving scope of CA3, and how does this influence the speed and direction of expansion?',
    'Detailed empirical analysis of ICRC methodology and international court judgments, tracking instances where one element was prioritized over the other in establishing a customary rule.',
    'If state practice is heavily weighted, the scope expands more slowly and conservatively. If opinio juris (especially from a broad range of states) is given more weight, expansion could be faster and more aligned with humanitarian principles, potentially increasing the perceived ''cost'' for states resisting such expansion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_practice_vs_opinio_juris_weight, empirical, 'Ambiguity in the balance between objective state actions and subjective legal belief in customary law formation.').

omega_variable(
    resistance_to_icrc_authority,
    'To what extent do states actively resist or challenge the ICRC''s role as the primary interpreter and tracker of customary IHL, and how does this affect the legitimacy and effectiveness of this reading?',
    'Analysis of state reservations, formal objections, and diplomatic statements regarding ICRC customary law studies, particularly in contexts where the ICRC''s interpretation expands state obligations.',
    'Significant, coordinated state resistance could weaken the perceived legitimacy of this reading, slowing the expansion of CA3''s scope and potentially leading to a more fragmented application of IHL. Conversely, broad acceptance reinforces the ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resistance_to_icrc_authority, empirical, 'The degree of state acceptance of the ICRC''s interpretive authority in customary IHL.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.05).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__icrc_customary_reading, theater_ratio, 1970, 0.07).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__icrc_customary_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__icrc_customary_reading, theater_ratio, 2010, 0.09).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2010, 0.23).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.05).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1990, 0.12).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__state_centric_reading).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, common_article_3_scope__expansive_human_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel. This 'icrc_customary_reading' emphasizes the dynamic evolution of IHL through state practice and opinio juris, influencing but not foreclosing other interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
