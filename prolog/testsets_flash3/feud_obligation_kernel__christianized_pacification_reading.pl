% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__christianized_pacification_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__christianized_pacification_reading, []).

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
 *   constraint_id: feud_obligation_kernel__christianized_pacification_reading
 *   human_readable: Christianized Pacification Reading of Blood-Feud Obligations
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint represents the 'Christianized Pacification' reading of
 *   blood-feud obligations, prevalent in medieval Europe. It frames feuds as
 *   violations of divine law and asserts that legitimate violence authority
 *   resides solely with God, delegated to ecclesiastical and royal
 *   institutions. This reading seeks to suppress traditional feud systems,
 *   reclassifying participants as victims (of spiritual peril and temporal
 *   punishment) and positioning the Church and Crown as beneficiaries (of
 *   expanded jurisdiction and moral authority). The high extractiveness
 *   reflects the transfer of power and autonomy from kinship groups to
 *   centralized authorities, enforced through spiritual and temporal
 *   sanctions.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, 0.85).
domain_priors:suppression_score(feud_obligation_kernel__christianized_pacification_reading, 0.9).
domain_priors:theater_ratio(feud_obligation_kernel__christianized_pacification_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(feud_obligation_kernel__christianized_pacification_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__christianized_pacification_reading, snare).
narrative_ontology:human_readable(feud_obligation_kernel__christianized_pacification_reading, "Christianized Pacification Reading of Blood-Feud Obligations").
narrative_ontology:topic_domain(feud_obligation_kernel__christianized_pacification_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__christianized_pacification_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__christianized_pacification_reading, '702d9aee-5795-466f-8fe5-289eb4ffde9b').
narrative_ontology:cs_kernel_codification('702d9aee-5795-466f-8fe5-289eb4ffde9b', formalized).
narrative_ontology:cs_authority_grounding('702d9aee-5795-466f-8fe5-289eb4ffde9b', lineage).
narrative_ontology:cs_interpretation_layer_present('702d9aee-5795-466f-8fe5-289eb4ffde9b').
narrative_ontology:cs_reading_relation('702d9aee-5795-466f-8fe5-289eb4ffde9b', feud_obligation_kernel__stateless_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('702d9aee-5795-466f-8fe5-289eb4ffde9b', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_axiom('702d9aee-5795-466f-8fe5-289eb4ffde9b', foundational, divine_prohibition_on_private_vengeance).
narrative_ontology:cs_axiom_status(divine_prohibition_on_private_vengeance, holdable).
narrative_ontology:cs_axiom_grounding('702d9aee-5795-466f-8fe5-289eb4ffde9b', divine_prohibition_on_private_vengeance, theological).
narrative_ontology:cs_axiom('702d9aee-5795-466f-8fe5-289eb4ffde9b', foundational, monopoly_on_legitimate_violence_resides_with_god_and_delegated_institutions).
narrative_ontology:cs_axiom_status(monopoly_on_legitimate_violence_resides_with_god_and_delegated_institutions, holdable).
narrative_ontology:cs_axiom_grounding('702d9aee-5795-466f-8fe5-289eb4ffde9b', monopoly_on_legitimate_violence_resides_with_god_and_delegated_institutions, conventional).
narrative_ontology:cs_reference_frame('702d9aee-5795-466f-8fe5-289eb4ffde9b', divine_peace_and_order).
narrative_ontology:cs_drift_state('702d9aee-5795-466f-8fe5-289eb4ffde9b', late_medieval_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('702d9aee-5795-466f-8fe5-289eb4ffde9b', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__christianized_pacification_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__christianized_pacification_reading, royal_authority).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, feuding_families).
narrative_ontology:constraint_victim(feud_obligation_kernel__christianized_pacification_reading, local_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Asserts divine authority over violence, seeking to replace private vengeance with ecclesiastical justice and penitential discipline. Benefits from expanded jurisdictional reach and moral authority.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, ecclesiastical_institutions, agenda_setter,
    institutional, generational, mobile, regional).

% Supports the Church's pacification efforts to consolidate its own power, reduce internal conflict, and establish a monopoly on legitimate violence. Benefits from increased stability and tax revenue.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, royal_authority, agenda_setter,
    institutional, generational, mobile, regional).

% Bound by traditional honor codes and kinship obligations to pursue vengeance, but face spiritual condemnation, excommunication, and royal penalties for doing so. Their identity is deeply tied to the feud system.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, feuding_families, payer,
    moderate, biographical, identity_locked, local).

% Suffer the direct violence and instability caused by feuds, but also face pressure from both traditional obligations and ecclesiastical/royal enforcement. They are caught between competing authorities.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, local_communities, payer,
    powerless, immediate, trapped, local).

% Operate outside the direct control of ecclesiastical or royal authority, maintaining traditional feud systems as their primary form of justice. Their perspective on feuds as legitimate coordination is actively suppressed by the dominant reading.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__christianized_pacification_reading, stateless_kinship_groups, excluded,
    moderate, generational, constrained, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Seeks to coordinate social order by establishing a single, divinely sanctioned authority for justice and violence, replacing decentralized, private vengeance with centralized, public enforcement.
% TRANSFER_FUNCTION: Transfers the right to legitimate violence and dispute resolution from kinship groups to ecclesiastical and royal institutions, along with the associated social control and moral authority.
% ABSENT_VOICES: Kinship groups and communities that view feuds as a legitimate, self-regulating system of justice are excluded. They would argue that the 'divine law' is a political construct designed to disarm them and centralize power.
% DISAPPEARANCE_RATIONALE: If the Christianized pacification reading vanished, the moral and legal framework for centralized authority would collapse. Feuds would likely re-emerge as a primary form of justice in many areas, and the power of Church and Crown would diminish significantly, leading to a reorganization of social and political structures.
% FOUNDING_PROBLEM: The problem of endemic private violence and social instability caused by blood-feuds, which undermined both spiritual order and nascent state authority.
% FOUNDING_PROBLEM_CORROBORATION: Ecclesiastical and royal chronicles consistently attest to the problem of feuds as a threat to order. Historians and legal anthropologists corroborate the historical prevalence and disruptive nature of feuds, supporting the claim that this reading addressed a genuine societal problem, even if its solutions were self-serving for the authorities.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__christianized_pacification_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__christianized_pacification_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__christianized_pacification_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__christianized_pacification_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__christianized_pacification_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(feud_obligation_kernel__christianized_pacification_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(feud_obligation_kernel__christianized_pacification_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because this reading fundamentally redefines legitimate violence, dispossessing kinship groups of their traditional right to vengeance and transferring that power to the Church and Crown. Suppression is also very high (0.90) due to the combined spiritual (excommunication, damnation) and temporal (fines, imprisonment, execution) penalties imposed on those who continue feuding. Theater ratio is low (0.20) as the pacification efforts were genuinely aimed at establishing a new order, though some performative aspects existed to reinforce authority. Accessibility collapse is high (0.70) as the moral and legal alternatives to centralized justice were systematically undermined. Resistance is also high (0.75) reflecting the persistent adherence to traditional feud practices despite severe penalties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of feuding families, the constraint is a snare that strips them of their traditional means of justice and honor, imposing an alien system. From the Church and Crown's perspective, it is a necessary rope or scaffold to establish divine order and civil peace. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Ecclesiastical and royal institutions are clear beneficiaries and agenda-setters, gaining authority and control (low d). Feuding families and local communities are targets, losing autonomy and facing severe penalties (high d). Their 'identity_locked' exit option reflects the deep cultural and social bonds that made abandoning feud obligations extremely difficult, even under duress.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_law_naturalness,
    'Is the prohibition on vengeance a genuine divine law, or a theological construct used to justify ecclesiastical and royal power?',
    'Comparative theological and historical analysis of pre-Christian and non-Christian legal systems regarding vengeance, alongside critical examination of the political context of its promulgation.',
    'If a theological construct, the constraint''s ''naturalness'' is undermined, reclassifying it more firmly as a constructed snare rather than a divinely ordained mountain. If genuine, it reinforces the moral legitimacy of the pacification efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_law_naturalness, conceptual, 'Ambiguity of divine law as a natural vs. constructed constraint.').

omega_variable(
    internalized_suppression_of_honor,
    'To what extent did the spiritual condemnation of feuds lead to internalized suppression of traditional honor codes, even when external enforcement was weak?',
    'Analysis of confessional manuals, penitential literature, and personal narratives for evidence of guilt, shame, or internal conflict regarding feud participation, even in the absence of direct temporal punishment.',
    'If internalized suppression was significant, the effective suppression of the constraint is higher than purely structural measures suggest, as individuals carried the enforcement within themselves. This would amplify the snare-like qualities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_honor, empirical, 'Structural vs. internalized suppression mechanism for honor codes.').

omega_variable(
    pacification_effectiveness_vs_extraction,
    'What was the actual reduction in violence attributable to Christianized pacification efforts, versus the increase in ecclesiastical/royal power and resource extraction?',
    'Quantitative historical analysis of violence rates (e.g., homicides, feuds) before and after pacification movements, correlated with data on church land acquisition, tithe collection, and royal judicial revenues.',
    'If violence reduction was minimal while extraction was high, it strengthens the ''snare'' classification by highlighting the disproportionate benefit to authorities. If violence reduction was substantial, it lends more credence to the ''scaffold'' or ''tangled_rope'' aspects of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pacification_effectiveness_vs_extraction, empirical, 'Measuring the balance between genuine pacification and power/resource extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__christianized_pacification_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_tr_t20, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(feud_tr_t40, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(feud_tr_t60, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(feud_tr_t80, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 80, 0.18).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__christianized_pacification_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 0, 0.7).
narrative_ontology:measurement(feud_be_t20, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(feud_be_t40, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 40, 0.82).
narrative_ontology:measurement(feud_be_t60, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 60, 0.85).
narrative_ontology:measurement(feud_be_t80, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 80, 0.84).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__christianized_pacification_reading, base_extractiveness, 100, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(feud_su_t20, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 20, 0.8).
narrative_ontology:measurement(feud_su_t40, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 40, 0.85).
narrative_ontology:measurement(feud_su_t60, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 60, 0.88).
narrative_ontology:measurement(feud_su_t80, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 80, 0.9).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__christianized_pacification_reading, suppression_requirement, 100, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__christianized_pacification_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'feud_obligation_kernel'. It is linked to 'stateless_coordination_reading' and 'extraction_cycle_reading' as sibling interpretations of the same underlying social phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
