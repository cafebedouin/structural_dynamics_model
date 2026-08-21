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
 *   constraint_id: eu_council_unanimity__sovereignty_guarantor_reading
 *   human_readable: EU Council Unanimity (Sovereignty Guarantor Reading)
 *   domain: institutional_design/international_relations/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'sovereignty guarantor' reading of
 *   the EU Council's unanimity rule. In this reading, the requirement for
 *   unanimous consent in specific policy areas (e.g., foreign policy,
 *   taxation, treaty changes) is understood as a foundational protection
 *   against majoritarian coercion. It ensures that no member state,
 *   particularly smaller ones, can be forced into collective action that
 *   fundamentally implicates its national sovereignty. The rule is seen as a
 *   legitimate exercise of sovereign defense, fostering trust and stability
 *   within the Union, despite the coordination costs it may impose.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_council_unanimity__sovereignty_guarantor_reading, 0.35).
domain_priors:suppression_score(eu_council_unanimity__sovereignty_guarantor_reading, 0.2).
domain_priors:theater_ratio(eu_council_unanimity__sovereignty_guarantor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(eu_council_unanimity__sovereignty_guarantor_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eu_council_unanimity__sovereignty_guarantor_reading, rope).
narrative_ontology:human_readable(eu_council_unanimity__sovereignty_guarantor_reading, "EU Council Unanimity (Sovereignty Guarantor Reading)").
narrative_ontology:topic_domain(eu_council_unanimity__sovereignty_guarantor_reading, "institutional_design/international_relations/political_economy").

domain_priors:requires_active_enforcement(eu_council_unanimity__sovereignty_guarantor_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(eu_council_unanimity__sovereignty_guarantor_reading, '6e4690f1-1548-4fa9-a485-6d72132ed624').
narrative_ontology:cs_kernel_codification('6e4690f1-1548-4fa9-a485-6d72132ed624', formalized).
narrative_ontology:cs_authority_grounding('6e4690f1-1548-4fa9-a485-6d72132ed624', lineage).
narrative_ontology:cs_interpretation_layer_present('6e4690f1-1548-4fa9-a485-6d72132ed624').
narrative_ontology:cs_reading_relation('6e4690f1-1548-4fa9-a485-6d72132ed624', eu_council_unanimity__veto_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e4690f1-1548-4fa9-a485-6d72132ed624', eu_council_unanimity__diplomatic_capital_reading, coexists_with).
narrative_ontology:cs_axiom('6e4690f1-1548-4fa9-a485-6d72132ed624', foundational, national_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(national_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('6e4690f1-1548-4fa9-a485-6d72132ed624', national_sovereignty_is_supreme, deontological).
narrative_ontology:cs_axiom('6e4690f1-1548-4fa9-a485-6d72132ed624', foundational, consent_is_prerequisite_for_integration).
narrative_ontology:cs_axiom_status(consent_is_prerequisite_for_integration, holdable).
narrative_ontology:cs_axiom_grounding('6e4690f1-1548-4fa9-a485-6d72132ed624', consent_is_prerequisite_for_integration, conventional).
narrative_ontology:cs_reference_frame('6e4690f1-1548-4fa9-a485-6d72132ed624', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('6e4690f1-1548-4fa9-a485-6d72132ed624', contemporary_eu_integration_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6e4690f1-1548-4fa9-a485-6d72132ed624', '').
narrative_ontology:cs_kernel_id(eu_council_unanimity__sovereignty_guarantor_reading, eu_council_unanimity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, all_member_states).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states).
narrative_ontology:constraint_victim(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the assurance that their fundamental sovereign interests cannot be overridden by a simple majority, fostering trust in the EU's decision-making processes. They collectively uphold the rule.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, all_member_states, beneficiary,
    organized, generational, constrained, continental).

% Are the primary beneficiaries of the unanimity rule, as it provides a critical safeguard against the potential dominance of larger states in areas touching upon national sovereignty. They actively defend the rule.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, small_member_states, beneficiary,
    moderate, generational, constrained, regional).

% While sometimes frustrated by the delays and compromises necessitated by unanimity, they ultimately accept it as a foundational principle that maintains the stability and legitimacy of the Union, preventing smaller states from feeling coerced into exit. They bear higher coordination costs.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, agenda_setter,
    powerful, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, large_member_states, payer).

% Proposes legislation and seeks consensus, but must navigate the unanimity requirement in sensitive areas. From this reading's perspective, the Commission respects the rule as a necessary check on integration, even if it slows down policy implementation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_commission, agenda_setter,
    institutional, generational, analytical, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, eu_commission, observer).

% Benefit from the long-term stability and legitimacy of the European Union, which the unanimity rule helps to secure. They may indirectly bear the costs of slower decision-making or missed opportunities for deeper integration, but value the protection of national interests.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(eu_council_unanimity__sovereignty_guarantor_reading, eu_citizens, payer).

% Study the institutional design of the EU, recognizing the unanimity rule as a deliberate mechanism to balance supranational ambition with national sovereignty. They analyze its effectiveness in preventing coercion and fostering long-term cooperation.
narrative_ontology:constraint_stakeholder(eu_council_unanimity__sovereignty_guarantor_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures that collective action in sensitive areas does not infringe upon the fundamental sovereignty of any member state, thereby fostering trust, preventing majoritarian coercion, and maintaining the voluntary nature of EU membership.
% TRANSFER_FUNCTION: Transfers the power to block certain collective decisions to individual member states, preventing the transfer of sovereign powers or the imposition of policies without full consent, especially in areas deemed vital to national interest.
% ABSENT_VOICES: In this reading, there are no structurally absent voices, as the unanimity rule ensures every member state has a voice and the power to protect its core interests. The mechanism itself is designed to prevent exclusion.
% DISAPPEARANCE_RATIONALE: If the unanimity rule vanished overnight in areas implicating sovereignty, member states, particularly smaller ones, would lose a critical safeguard against majoritarian overreach. This would fundamentally alter the balance of power within the EU, likely leading to a loss of trust, increased calls for exit, or a complete re-evaluation of the Union's foundational principles and federal structure.
% FOUNDING_PROBLEM: The foundational problem was to create a supranational political and economic union that could achieve deep integration and collective action, while simultaneously safeguarding the national sovereignty and distinct interests of its diverse member states, preventing any single state from being coerced into policies against its will.
% FOUNDING_PROBLEM_CORROBORATION: Member state governments, particularly those of smaller nations, consistently articulate the importance of unanimity as a safeguard for their national interests. Legal scholars specializing in international law and EU constitutionalism also corroborate this foundational role, highlighting the ongoing tension between integration and sovereignty as a live issue in EU governance.
narrative_ontology:disappearance_verdict(eu_council_unanimity__sovereignty_guarantor_reading, world_rearranges).
narrative_ontology:founding_problem_status(eu_council_unanimity__sovereignty_guarantor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(eu_council_unanimity__sovereignty_guarantor_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(eu_council_unanimity__sovereignty_guarantor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(eu_council_unanimity__sovereignty_guarantor_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is classified as a Rope because it solves a genuine collective-action problem (how to integrate without coercing sovereign states) with moderate coordination costs (extractiveness 0.35) but no systematic extraction from victims. Suppression is low (0.20) because the rule's purpose is to prevent suppression, not to enforce it; it actively preserves the 'exit option' of blocking. Theater ratio is low (0.10) as the mechanism is genuinely functional in its stated purpose. The slight increase in extractiveness and suppression over time reflects the growing complexity of EU governance and the increasing pressure for faster decision-making, which makes the unanimity rule's coordination costs more salient.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the protective and trust-building aspects of unanimity. Other readings, such as the 'veto trap' or 'diplomatic capital' readings, would highlight different aspects, such as the potential for minoritarian blocking to paralyze decision-making or to be used as leverage for unrelated concessions. This story focuses solely on the sovereignty guarantor function, with its associated moderate coordination costs and absence of systematic extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   All member states, and especially small member states, are structural beneficiaries (d near 0.0) as the rule directly protects their sovereign interests. Large member states, while sometimes bearing higher coordination costs due to blocked initiatives, are also net beneficiaries in this reading, as the rule ensures the long-term stability and legitimacy of the Union. There are no identifiable victims, as the rule's function is protective, not extractive.
 *
 * MANDATROPHY ANALYSIS:
 *   From this 'sovereignty guarantor' reading, the mandate of the unanimity rule is very much alive. The tension between national sovereignty and supranational integration remains a core, live issue in the EU. The rule continues to serve its original function of preventing coercion and ensuring consent in sensitive areas, thus avoiding mandatrophy. Its persistence is due to its ongoing protective function, not merely inertia or theatrical maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''sovereignty guarantor'' reading of EU Council unanimity, distinct from ''veto trap'' or ''diplomatic capital'' readings?',
    'Analysis of official statements from member state governments, particularly smaller ones, and legal scholarship focusing on the protective function of unanimity in EU constitutional law.',
    'If this reading is not sufficiently distinct or dominant, the classification might need to be subsumed into a broader ''coordination cost'' reading (e.g., diplomatic_capital_reading) or re-evaluated for potential hidden extraction (e.g., veto_trap_reading).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing this specific reading from other interpretations of EU unanimity.').

omega_variable(
    effectiveness_vs_delay,
    'Does the unanimity rule genuinely protect sovereignty, or does it merely delay inevitable collective action, potentially at a higher cost?',
    'Longitudinal studies of policy outcomes in areas subject to unanimity, comparing them to areas decided by QMV, and analyzing instances where vetoes were used or credibly threatened. Qualitative analysis of member state satisfaction with the protective function.',
    'If it primarily causes delay without effective protection, the extractiveness (coordination cost) would be higher, and the ''rope'' classification might shift towards a ''tangled_rope'' if some states systematically bear these costs more than others.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_vs_delay, empirical, 'Assessing the true effectiveness of unanimity as a sovereignty guarantor versus a source of policy paralysis.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eu_council_unanimity__sovereignty_guarantor_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eu_c_tr_t0, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(eu_c_tr_t6, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(eu_c_tr_t12, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 12, 0.11).
narrative_ontology:measurement(eu_c_tr_t18, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 18, 0.11).
narrative_ontology:measurement(eu_c_tr_t24, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement(eu_c_tr_t30, eu_council_unanimity__sovereignty_guarantor_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(eu_c_be_t0, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(eu_c_be_t6, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement(eu_c_be_t12, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 12, 0.32).
narrative_ontology:measurement(eu_c_be_t18, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 18, 0.33).
narrative_ontology:measurement(eu_c_be_t24, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 24, 0.34).
narrative_ontology:measurement(eu_c_be_t30, eu_council_unanimity__sovereignty_guarantor_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(eu_c_su_t0, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(eu_c_su_t6, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 6, 0.21).
narrative_ontology:measurement(eu_c_su_t12, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 12, 0.22).
narrative_ontology:measurement(eu_c_su_t18, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 18, 0.23).
narrative_ontology:measurement(eu_c_su_t24, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 24, 0.24).
narrative_ontology:measurement(eu_c_su_t30, eu_council_unanimity__sovereignty_guarantor_reading, suppression_requirement, 30, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eu_council_unanimity__sovereignty_guarantor_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_budget_negotiations).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_common_foreign_security_policy).
narrative_ontology:affects_constraint(eu_council_unanimity__sovereignty_guarantor_reading, eu_treaty_amendment_process).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'eu_council_unanimity' kernel. The 'sovereignty_guarantor_reading' emphasizes protection against majoritarian coercion, while the 'veto_trap_reading' focuses on minoritarian extraction through blocking, and the 'diplomatic_capital_reading' highlights consensus-building and policy legitimacy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
