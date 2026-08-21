% ============================================================================
% CONSTRAINT STORY: eu_council_unanimity__sovereignty_guarantor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eu_council_unanimity__sovereignty_guarantor_reading, []).

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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity (Sovereignty Guarantor Reading)
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty guarantor' reading of
 *   EU Council unanimity. From this perspective, the unanimity rule is a
 *   foundational protection against majoritarian coercion, ensuring that each
 *   member state, particularly smaller ones, retains a veto over collective
 *   action that implicates its core sovereignty. The rule is seen as a
 *   legitimate exercise of sovereign rights, not as a mechanism for
 *   extraction. The claimed type is 'rope' because it facilitates
 *   coordination by guaranteeing trust and participation, with only moderate,
 *   inherent coordination costs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.3).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity (Sovereignty Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '8c2d34ad-4feb-4e27-8c19-154996e5cec5').
narrative_ontology:cs_kernel_codification('8c2d34ad-4feb-4e27-8c19-154996e5cec5', formalized).
narrative_ontology:cs_authority_grounding('8c2d34ad-4feb-4e27-8c19-154996e5cec5', lineage).
narrative_ontology:cs_interpretation_layer_present('8c2d34ad-4feb-4e27-8c19-154996e5cec5').
narrative_ontology:cs_reading_relation('8c2d34ad-4feb-4e27-8c19-154996e5cec5', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('8c2d34ad-4feb-4e27-8c19-154996e5cec5', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('8c2d34ad-4feb-4e27-8c19-154996e5cec5', foundational, sovereign_equality_principle).
narrative_ontology:cs_axiom_status(sovereign_equality_principle, holdable).
narrative_ontology:cs_axiom_grounding('8c2d34ad-4feb-4e27-8c19-154996e5cec5', sovereign_equality_principle, deontological).
narrative_ontology:cs_axiom('8c2d34ad-4feb-4e27-8c19-154996e5cec5', foundational, protection_against_majoritarian_coercion).
narrative_ontology:cs_axiom_status(protection_against_majoritarian_coercion, holdable).
narrative_ontology:cs_axiom_grounding('8c2d34ad-4feb-4e27-8c19-154996e5cec5', protection_against_majoritarian_coercion, deontological).
narrative_ontology:cs_reference_frame('8c2d34ad-4feb-4e27-8c19-154996e5cec5', founding_treaty_sovereignty_balance).
narrative_ontology:cs_drift_state('8c2d34ad-4feb-4e27-8c19-154996e5cec5', contemporary_eu_enlargement_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8c2d34ad-4feb-4e27-8c19-154996e5cec5', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, eu_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% All member states benefit from the guarantee that their core sovereign interests cannot be overridden by a simple majority, ensuring their continued participation in the Union. They are constrained by the collective benefits of EU membership.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Disproportionately benefit from the unanimity rule as it provides a critical safeguard against the dominance of larger states, ensuring their voice and interests are always considered in decisions affecting national sovereignty. Their exit is constrained by the economic and political benefits of EU membership.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    institutional, generational, constrained, continental).

% Manages the negotiation process within the Council, seeking consensus on proposals. While they facilitate agreement, they are bound by the unanimity requirement for certain decisions, ensuring all states' consent is sought.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_presidency, agenda_setter,
    institutional, immediate, mobile, continental).

% Proposes legislation and monitors its implementation. From this reading, the Commission views unanimity as a necessary, albeit sometimes slow, mechanism for ensuring broad legitimacy and sovereign buy-in for EU policies.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_commission, observer,
    institutional, biographical, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that collective action on sensitive issues (e.g., foreign policy, taxation, treaty changes) is only undertaken with the explicit consent of all member states, thereby protecting national sovereignty and preventing majoritarian coercion.
% TRANSFER_FUNCTION: Transfers the power to block collective action from a simple majority to any single member state on specific issues, effectively distributing veto power as a safeguard for sovereign interests.
% ABSENT_VOICES: Proponents of more efficient, supranational decision-making would argue that the unanimity rule empowers individual states to hold the entire Union hostage, but their arguments are structurally sidelined by the foundational commitment to sovereign equality.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight for all decisions, smaller member states would lose a critical safeguard, potentially leading to their marginalization or even withdrawal from the Union, fundamentally altering the balance of power and the nature of EU integration.
% FOUNDING_PROBLEM: The need to balance national sovereignty with the desire for deeper European integration, ensuring that no state felt its core interests could be unilaterally overridden by others.
% FOUNDING_PROBLEM_CORROBORATION: All member states, particularly smaller ones, consistently reaffirm the importance of unanimity for decisions touching on national sovereignty, viewing it as a cornerstone of their participation in the EU. This is corroborated by treaty texts and ongoing diplomatic discourse.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).
:- end_tests(eu_council_unanimity__sovereignty_guarantor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.3) because the rule's primary function is to protect, not to extract. Any 'cost' is seen as a necessary price for maintaining sovereign equality and trust within the Union. Suppression is low (0.1) as the rule is actively defended by its beneficiaries, not imposed coercively. Theater ratio is negligible (0.05) as the rule genuinely serves its stated purpose of protecting sovereignty. Accessibility collapse is low (0.2) because alternatives (e.g., qualified majority voting) are actively resisted by states seeking to preserve their veto. Resistance is low (0.05) because the rule is widely accepted by its beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the protective aspect of unanimity. Other readings (e.g., 'veto trap' or 'diplomatic capital') would highlight different aspects, potentially leading to higher extractiveness or suppression scores. The engine will compute these divergences from the structural data of each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   All EU member states, especially smaller ones, are beneficiaries (d near 0.0) as the rule directly protects their sovereign interests. The EU Council Presidency and Commission act as observers or facilitators, operating within the bounds of this foundational rule.
 *
 * MANDATROPHY ANALYSIS:
 *   From this reading, the unanimity rule's mandate (protecting sovereignty) is very much alive and continuously reaffirmed by member states. There is no evidence of mandatrophy; the constraint is seen as a vital, ongoing function, not an atrophied one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unanimity_as_veto_trap_ambiguity,
    'Is the unanimity rule primarily a legitimate defense of sovereignty, or does it function as a ''veto trap'' enabling minoritarian extraction?',
    'Empirical analysis of veto usage patterns: frequency of vetoes by states with disproportionate gains from blocking, and the nature of concessions extracted to lift vetoes. If vetoes are consistently used to extract unrelated concessions, it supports the ''veto trap'' reading.',
    'If it functions as a veto trap, the constraint''s effective extractiveness would be significantly higher, and its classification would shift towards a ''snare'' or ''tangled_rope'' for the states being held hostage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unanimity_as_veto_trap_ambiguity, empirical, 'Ambiguity between sovereignty protection and minoritarian extraction.').

omega_variable(
    coordination_cost_vs_extraction,
    'What proportion of the ''costs'' associated with unanimity (e.g., slow decision-making, diluted policies) are genuine coordination costs versus implicit extraction by states leveraging their veto power?',
    'Comparative institutional analysis with similar international bodies using different voting rules, or detailed case studies of specific policy deadlocks to quantify the ''cost'' of achieving consensus versus the ''cost'' of concessions.',
    'A higher proportion of implicit extraction would increase the effective extractiveness, potentially shifting the classification towards ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_cost_vs_extraction, empirical, 'Distinguishing genuine coordination costs from implicit extraction.').

omega_variable(
    framing_underdetermination_unanimity,
    'Is the ''sovereignty guarantor'' framing the most appropriate lens for understanding EU Council unanimity, or do alternative framings (e.g., ''veto trap'', ''diplomatic capital'') offer a more accurate structural description?',
    'Analysis of the political discourse and institutional practices: which framing is most consistently invoked by different actors, and which framing best predicts policy outcomes and institutional evolution. This is a conceptual choice, not an empirical one.',
    'Adopting an alternative framing would lead to a different constraint story with potentially different base properties and classification, reflecting the structural implications of that perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(framing_underdetermination_unanimity, conceptual, 'The choice of framing (sovereignty guarantor vs. veto trap vs. diplomatic capital) fundamentally alters the structural analysis of the unanimity rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(eu_c_tr_t10, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(eu_c_tr_t20, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(eu_c_tr_t40, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(eu_c_tr_t50, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(eu_c_be_t10, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 10, 0.27).
narrative_ontology:measurement(eu_c_be_t20, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.29).
narrative_ontology:measurement(eu_c_be_t40, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 40, 0.3).
narrative_ontology:measurement(eu_c_be_t50, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 50, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(eu_c_su_t10, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 10, 0.09).
narrative_ontology:measurement(eu_c_su_t20, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 20, 0.09).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 30, 0.1).
narrative_ontology:measurement(eu_c_su_t40, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 40, 0.1).
narrative_ontology:measurement(eu_c_su_t50, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 50, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__veto_trap_reading).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity__diplomatic_capital_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'eu_council_unanimity' kernel. Each reading represents a distinct structural claim about the unanimity rule's function and effects, with different ε values and stakeholder dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
