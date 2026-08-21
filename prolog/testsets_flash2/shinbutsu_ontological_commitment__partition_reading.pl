% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_commitment__partition_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_commitment__partition_reading, []).

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
 *   constraint_id: shinbutsu_ontological_commitment__partition_reading
 *   human_readable: Shinto-Buddhism Partition (Life-Cycle vs. Afterlife)
 *   domain: religious_studies/japanese_history/ontology_of_practice
 *
 * SUMMARY:
 *   This constraint describes the 'partition reading' of Shinto-Buddhism
 *   relations in Japan, where the two traditions functionally coexist by
 *   occupying separate ritual domains (Shinto for life-cycle events, Buddhism
 *   for death/afterlife) without requiring deep ontological integration. This
 *   reading emphasizes a stable division of labor and practitioner autonomy,
 *   rather than a syncretic fusion or an inherent incoherence. The constraint
 *   is claimed as a Rope due to its genuine coordination function and low
 *   extraction, reflecting a mutually beneficial arrangement for institutions
 *   and practitioners.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_commitment__partition_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_commitment__partition_reading, 0.05).
domain_priors:theater_ratio(shinbutsu_ontological_commitment__partition_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(shinbutsu_ontological_commitment__partition_reading, resistance, 0.02).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_commitment__partition_reading, rope).
narrative_ontology:human_readable(shinbutsu_ontological_commitment__partition_reading, "Shinto-Buddhism Partition (Life-Cycle vs. Afterlife)").
narrative_ontology:topic_domain(shinbutsu_ontological_commitment__partition_reading, "religious_studies/japanese_history/ontology_of_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_commitment__partition_reading, '2112d373-9ab2-4d6e-a1be-502a42c417bc').
narrative_ontology:cs_kernel_codification('2112d373-9ab2-4d6e-a1be-502a42c417bc', implicit).
narrative_ontology:cs_authority_grounding('2112d373-9ab2-4d6e-a1be-502a42c417bc', practice).
narrative_ontology:cs_interpretation_layer_present('2112d373-9ab2-4d6e-a1be-502a42c417bc').
narrative_ontology:cs_reading_relation('2112d373-9ab2-4d6e-a1be-502a42c417bc', shinbutsu_ontological_commitment__syncretic_reading, coexists_with).
narrative_ontology:cs_reading_relation('2112d373-9ab2-4d6e-a1be-502a42c417bc', shinbutsu_ontological_commitment__incoherence_reading, coexists_with).
narrative_ontology:cs_axiom('2112d373-9ab2-4d6e-a1be-502a42c417bc', foundational, functional_differentiation_is_primary).
narrative_ontology:cs_axiom_status(functional_differentiation_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('2112d373-9ab2-4d6e-a1be-502a42c417bc', functional_differentiation_is_primary, conventional).
narrative_ontology:cs_axiom('2112d373-9ab2-4d6e-a1be-502a42c417bc', foundational, ontological_integration_is_not_required).
narrative_ontology:cs_axiom_status(ontological_integration_is_not_required, holdable).
narrative_ontology:cs_axiom_grounding('2112d373-9ab2-4d6e-a1be-502a42c417bc', ontological_integration_is_not_required, deontological).
narrative_ontology:cs_reference_frame('2112d373-9ab2-4d6e-a1be-502a42c417bc', stable_functional_division).
narrative_ontology:cs_drift_state('2112d373-9ab2-4d6e-a1be-502a42c417bc', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2112d373-9ab2-4d6e-a1be-502a42c417bc', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_commitment__partition_reading, shinbutsu_ontological_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, shinto_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, buddhist_monks).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_commitment__partition_reading, japanese_households).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer Shinto rituals primarily for life-cycle events (birth, marriage, festivals, purification). Benefit from a clear domain of practice that avoids direct competition with Buddhist institutions, ensuring a stable role in community life.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, shinto_priests, beneficiary,
    organized, generational, constrained, national).

% Administer Buddhist rituals primarily for death, funerals, and ancestral rites. Benefit from a clear domain of practice that avoids direct competition with Shinto institutions, ensuring a stable role in community life.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, buddhist_monks, beneficiary,
    organized, generational, constrained, national).

% Utilize Shinto for auspicious life events and Buddhism for funerary practices, experiencing a complementary system that addresses different spiritual needs without requiring ontological reconciliation. This functional division simplifies religious practice.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, japanese_households, beneficiary,
    moderate, biographical, mobile, local).

% Analyze the historical and theological development of Shinto-Buddhism relations, interpreting the partition as a stable, functional division of labor rather than a deep syncretism or an inherent contradiction. Their work contributes to the academic understanding of the constraint.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_commitment__partition_reading, religious_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious landscape of Japan by functionally partitioning the domains of Shinto and Buddhist practice, allowing both traditions to thrive without direct competition over core ritual functions or ontological claims.
% TRANSFER_FUNCTION: Transfers ritual responsibilities and associated patronage from Japanese households to distinct Shinto and Buddhist institutions based on the life-cycle stage or spiritual need, ensuring a stable flow of resources to both.
% ABSENT_VOICES: Theological purists from either tradition, who might argue for the exclusive truth of one system over the other, are largely absent from the mainstream discourse that accepts this functional partition. Their voices are marginalized by the practical efficacy of the arrangement.
% DISAPPEARANCE_RATIONALE: If this functional partition vanished, it would create immediate competition and confusion over ritual domains, potentially leading to a decline in patronage for one or both traditions, and forcing households to choose or reconcile conflicting practices. The religious landscape would need to re-organize.
% FOUNDING_PROBLEM: The historical challenge of integrating or distinguishing indigenous Shinto practices with the imported Buddhist tradition without causing irreconcilable conflict or diluting either's distinct identity.
% FOUNDING_PROBLEM_CORROBORATION: Religious historians and anthropologists corroborate that the challenge of managing religious pluralism and maintaining distinct institutional identities remains a live concern, even if the partition offers a stable solution. Practitioner surveys also show a continued functional distinction in household religious choices.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_commitment__partition_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_commitment__partition_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_commitment__partition_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_ontological_commitment__partition_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_commitment__partition_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_commitment__partition_reading_tests).
:- end_tests(shinbutsu_ontological_commitment__partition_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the arrangement primarily facilitates complementary religious services rather than imposing significant costs or rents. Suppression is very low (0.05) as the partition is maintained by custom and mutual benefit, not active coercion. Theater ratio is low (0.1) because the functional division is largely genuine, with minimal performative maintenance. Accessibility collapse is high (0.85) because the functional domains are well-established, making it difficult for a single tradition to unilaterally expand into the other's core area without significant disruption. Resistance is negligible (0.02) as the arrangement is widely accepted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Shinto and Buddhist institutions, this partition is a highly effective coordination mechanism that minimizes conflict and ensures institutional stability. From the perspective of Japanese households, it is a practical and coherent approach to spiritual life. The analytical observer (religious scholars) also largely views it as a functional and stable arrangement, though they may debate its historical origins or deeper ontological implications.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priests and Buddhist monks are beneficiaries, as the partition ensures stable roles and patronage for their respective institutions. Japanese households are also beneficiaries, gaining a comprehensive and non-conflicting religious framework for life's events. There are no identifiable victims, as the arrangement is seen as mutually advantageous.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_contingency_vs_structural_necessity,
    'To what extent was this partition a historically contingent outcome of political and social forces (e.g., Meiji separation policies), versus a structurally necessary solution to managing religious pluralism?',
    'Comparative historical analysis of other multi-religious societies and counterfactual modeling of Japanese history without specific political interventions.',
    'If highly contingent, the constraint is more ''constructed'' and less ''natural'', potentially making it more amenable to future re-negotiation or re-interpretation. If structurally necessary, its persistence is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_contingency_vs_structural_necessity, empirical, 'Assessing the historical roots and structural robustness of the partition.').

omega_variable(
    practitioner_vs_theologian_perspective,
    'Does the functional partition reflect a genuine lack of ontological integration at the practitioner level, or is it a pragmatic arrangement that coexists with deeper syncretic or incoherent theological understandings among specialists?',
    'Extensive ethnographic research into lay practitioner beliefs and comparative analysis with theological treatises from different historical periods.',
    'If practitioners genuinely perceive a partition, the ''rope'' classification holds strongly. If deeper, more complex ontological views are common among practitioners, the ''partition_reading'' might be an oversimplification, pushing towards a ''syncretic_reading'' or ''incoherence_reading'' for some seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(practitioner_vs_theologian_perspective, conceptual, 'Distinguishing functional partition from underlying ontological beliefs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_commitment__partition_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 300, 0.09).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 600, 0.09).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(shin_tr_t1500, shinbutsu_ontological_commitment__partition_reading, theater_ratio, 1500, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 300, 0.12).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 600, 0.13).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 900, 0.14).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1200, 0.15).
narrative_ontology:measurement(shin_be_t1500, shinbutsu_ontological_commitment__partition_reading, base_extractiveness, 1500, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 300, 0.05).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 600, 0.05).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 900, 0.05).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1200, 0.05).
narrative_ontology:measurement(shin_su_t1500, shinbutsu_ontological_commitment__partition_reading, suppression_requirement, 1500, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_commitment__partition_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_ontological_commitment' kernel, focusing on the functional partition between Shinto and Buddhism. It coexists with 'syncretic_reading' and 'incoherence_reading', which offer alternative interpretations of the same historical and theological phenomena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
