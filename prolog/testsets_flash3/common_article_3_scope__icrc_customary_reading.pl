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
 *   through customary international law. This reading emphasizes a dynamic,
 *   procedural constraint on interpretation, allowing for gradual expansion
 *   of CA3's application to non-international armed conflicts without formal
 *   treaty amendment. It functions as a coordination mechanism for states to
 *   adapt to new forms of conflict while maintaining minimum humanitarian
 *   standards. This is one reading of the 'common_article_3_scope' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__icrc_customary_reading, 0.25).
domain_priors:suppression_score(common_article_3_scope__icrc_customary_reading, 0.15).
domain_priors:theater_ratio(common_article_3_scope__icrc_customary_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_article_3_scope__icrc_customary_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__icrc_customary_reading, rope).
narrative_ontology:human_readable(common_article_3_scope__icrc_customary_reading, "Common Article 3 Scope (ICRC Customary Law Reading)").
narrative_ontology:topic_domain(common_article_3_scope__icrc_customary_reading, "international_humanitarian_law").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__icrc_customary_reading, '530d9db3-a44a-43ae-9aec-b27f5bed2a7b').
narrative_ontology:cs_kernel_codification('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', formalized).
narrative_ontology:cs_authority_grounding('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', expertise).
narrative_ontology:cs_interpretation_layer_present('530d9db3-a44a-43ae-9aec-b27f5bed2a7b').
narrative_ontology:cs_reading_relation('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', common_article_3_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_axiom('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', foundational, customary_law_as_dynamic_source_of_ihl).
narrative_ontology:cs_axiom_status(customary_law_as_dynamic_source_of_ihl, holdable).
narrative_ontology:cs_axiom_grounding('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', customary_law_as_dynamic_source_of_ihl, conventional).
narrative_ontology:cs_axiom('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', foundational, minimum_humanitarian_standards_are_non_derogable).
narrative_ontology:cs_axiom_status(minimum_humanitarian_standards_are_non_derogable, holdable).
narrative_ontology:cs_axiom_grounding('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', minimum_humanitarian_standards_are_non_derogable, deontological).
narrative_ontology:cs_reference_frame('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', evolving_customary_ihl_framework).
narrative_ontology:cs_drift_state('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('530d9db3-a44a-43ae-9aec-b27f5bed2a7b', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__icrc_customary_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_humanitarian_law_regime).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, victims_of_armed_conflict).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals).
narrative_ontology:constraint_victim(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, customary_international_law_doctrine).
narrative_ontology:constraint_vindicates(common_article_3_scope__icrc_customary_reading, evolving_standards_of_humanity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The International Committee of the Red Cross (ICRC) actively researches, documents, and promotes the customary international law interpretation of CA3, influencing states and international bodies. It acts as a guardian and interpreter of IHL.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, icrc, agenda_setter,
    institutional, generational, constrained, global).

% States are bound by CA3 and its customary interpretation, which imposes obligations even in non-international armed conflicts. This reading requires them to adapt their military doctrine and practice to evolving customary norms, potentially expanding the scope of protection beyond their initial treaty commitments. Exit is constrained by reputational costs and the principle of pacta sunt servanda.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, states_parties_to_geneva_conventions, payer,
    institutional, generational, constrained, global).

% These bodies rely on the customary interpretation of CA3 to adjudicate cases involving non-international armed conflicts, providing a flexible and evolving legal framework. They benefit from a dynamic standard that can adapt to new forms of conflict without formal treaty revision.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, international_courts_and_tribunals, beneficiary,
    institutional, generational, analytical, global).

% Individuals caught in non-international armed conflicts benefit from the expanded protection offered by this reading, which ensures a minimum standard of humanity applies regardless of the conflict's formal classification by states. Their situation is directly improved by the constraint's operation.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, victims_of_armed_conflict, beneficiary,
    powerless, immediate, trapped, local).

% Advocate for the broadest possible application of CA3, often aligning with or pushing beyond the ICRC's customary reading towards a more expansive human rights approach. They monitor state practice and contribute to the discourse on customary law.
narrative_ontology:constraint_stakeholder(common_article_3_scope__icrc_customary_reading, human_rights_advocates, observer,
    organized, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a dynamic mechanism for states to coordinate on the minimum humanitarian standards applicable in non-international armed conflicts, allowing the scope to evolve with state practice and opinio juris without requiring formal treaty amendments.
% TRANSFER_FUNCTION: Transfers responsibility for upholding minimum humanitarian standards to states in a wider range of conflicts, from states to individuals caught in conflict, and transfers interpretive authority from strict treaty text to evolving customary norms.
% ABSENT_VOICES: States that prefer a more restrictive, state-centric interpretation of CA3, limiting its application to only the most intense and organized conflicts, are often resistant to this reading. They would argue for greater state sovereignty over internal affairs and less external legal oversight.
% DISAPPEARANCE_RATIONALE: If this customary reading of CA3's scope vanished, the legal framework for non-international armed conflicts would become significantly more rigid and less adaptable. States would likely revert to narrower interpretations, reducing protections for victims and increasing legal uncertainty in rapidly evolving conflict situations. The international legal order would lose a key mechanism for adapting IHL.
% FOUNDING_PROBLEM: The original Geneva Conventions did not adequately address non-international armed conflicts, leaving a gap in protection for victims of internal strife. CA3 was introduced to provide a minimum standard, but its precise scope remained ambiguous, leading to varied state practice.
% FOUNDING_PROBLEM_CORROBORATION: The ICRC, international courts, and human rights organizations consistently attest that the problem of protecting victims in diverse and evolving non-international armed conflicts remains live. Scholarly consensus in international law also supports the ongoing need for a dynamic interpretive framework for CA3.
narrative_ontology:disappearance_verdict(common_article_3_scope__icrc_customary_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__icrc_customary_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__icrc_customary_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.25) is relatively low because this reading primarily coordinates interpretation and expands protection, rather than extracting rents. However, it does impose evolving obligations on states, which can be seen as a form of 'extraction' of sovereign discretion. Suppression (0.15) is also low, as adherence is largely driven by state consent and reputational concerns, not overt coercion. Theater ratio (0.05) is minimal, as the ICRC's work is genuinely aimed at humanitarian protection and legal clarity. The metrics reflect a Rope-like function, consistent with a coordination mechanism that imposes some costs but provides net benefits.
 *
 * PERSPECTIVAL GAP:
 *   While the ICRC and victims perceive this as a beneficial coordination mechanism, some states may view the evolving customary law as an encroachment on their sovereignty, experiencing it as a higher-extraction constraint. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICRC acts as an agenda-setter, actively shaping and promoting this reading. States Parties are payers, bearing the costs of adapting to evolving customary norms. International courts and victims of armed conflict are beneficiaries, gaining a more robust and adaptable legal framework for protection. Human rights advocates observe and influence the process, pushing for broader application.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    customary_law_legitimacy,
    'To what extent does the ''evolving state practice and opinio juris'' genuinely reflect universal consent, versus being influenced by a subset of powerful states or interpretive bodies?',
    'Detailed empirical analysis of state declarations, military manuals, and judicial decisions across a wide range of states, particularly those from the Global South, to assess the breadth and depth of acceptance.',
    'If consent is found to be narrow or coerced, the legitimacy of the customary reading as a ''Rope'' would be challenged, potentially reclassifying it towards a ''Tangled Rope'' or ''Snare'' for states that feel unduly bound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(customary_law_legitimacy, empirical, 'Ambiguity regarding the true universality and voluntariness of customary international law formation.').

omega_variable(
    scope_of_non_international_armed_conflict,
    'How precisely can ''non-international armed conflict'' be defined in practice, given the evolving nature of armed violence (e.g., cyber warfare, drone strikes, transnational terrorism)?',
    'Development of clearer, universally accepted criteria for classifying new forms of armed violence as NIACs, potentially through UN General Assembly resolutions or further ICRC interpretive guidance.',
    'Lack of clarity could lead to inconsistent application, undermining the coordination function and potentially allowing states to evade obligations, shifting the constraint towards a ''Piton'' or ''Snare'' for victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_non_international_armed_conflict, conceptual, 'The inherent ambiguity in defining the threshold for application of CA3 in contemporary conflicts.').

omega_variable(
    icrc_influence_vs_state_sovereignty,
    'Is the ICRC''s role in defining customary IHL an appropriate exercise of expertise, or does it unduly influence state sovereignty in a way that benefits the IHL regime at the expense of state discretion?',
    'Analysis of state responses to ICRC publications and advocacy, and the extent to which states actively participate in or resist the formation of customary norms, rather than passively accepting ICRC interpretations.',
    'If ICRC influence is perceived as overreaching, some states might resist the customary reading more actively, leading to a higher ''resistance'' metric and potentially reclassifying the constraint as more extractive from the state perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(icrc_influence_vs_state_sovereignty, preference, 'The normative question of the balance between expert interpretation and state consent in international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__icrc_customary_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__icrc_customary_reading, theater_ratio, 1949, 0.01).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__icrc_customary_reading, theater_ratio, 1970, 0.02).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__icrc_customary_reading, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__icrc_customary_reading, theater_ratio, 2010, 0.04).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__icrc_customary_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1949, 0.1).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__icrc_customary_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2010, 0.23).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__icrc_customary_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1949, 0.05).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1970, 0.1).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__icrc_customary_reading, suppression_requirement, 1990, 0.13).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2010, 0.14).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__icrc_customary_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__icrc_customary_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(common_article_3_scope__icrc_customary_reading, state_responsibility_for_ihl_violations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_article_3_scope' kernel, alongside 'state_centric_reading' and 'expansive_human_rights_reading'. Each reading offers a distinct interpretation of CA3's application in non-international armed conflicts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
