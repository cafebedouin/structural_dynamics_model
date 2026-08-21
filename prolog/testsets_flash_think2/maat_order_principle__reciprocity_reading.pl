% ============================================================================
% CONSTRAINT STORY: maat_order_principle__reciprocity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maat_order_principle__reciprocity_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: maat_order_principle__reciprocity_reading
 *   human_readable: Ma'at Order Principle (Reciprocity Reading)
 *   domain: ancient_history/political_philosophy/religious_studies
 *
 * SUMMARY:
 *   This constraint represents the 'reciprocity_reading' of the Ma'at order
 *   principle in ancient Egypt. It posits that Ma'at imposes mutual
 *   obligations: the Pharaoh must provide justice, stability, and proper
 *   resource distribution to maintain cosmic balance, and in return, the
 *   people provide labor and loyalty. Crucially, this reading implies that if
 *   the Pharaoh fails in these obligations, resistance or withdrawal of
 *   support is justified. The constraint is claimed as a Tangled Rope,
 *   reflecting its genuine coordination function (maintaining cosmic order)
 *   alongside its asymmetric extraction (Pharaoh's power and resource
 *   collection) and the active enforcement required to maintain the balance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, 0.55).
domain_priors:suppression_score(maat_order_principle__reciprocity_reading, 0.7).
domain_priors:theater_ratio(maat_order_principle__reciprocity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(maat_order_principle__reciprocity_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maat_order_principle__reciprocity_reading, tangled_rope).
narrative_ontology:human_readable(maat_order_principle__reciprocity_reading, "Ma'at Order Principle (Reciprocity Reading)").
narrative_ontology:topic_domain(maat_order_principle__reciprocity_reading, "ancient_history/political_philosophy/religious_studies").

domain_priors:requires_active_enforcement(maat_order_principle__reciprocity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(maat_order_principle__reciprocity_reading, '55c8634c-5b0a-4bd6-8677-365606184607').
narrative_ontology:cs_kernel_codification('55c8634c-5b0a-4bd6-8677-365606184607', formalized).
narrative_ontology:cs_authority_grounding('55c8634c-5b0a-4bd6-8677-365606184607', lineage).
narrative_ontology:cs_interpretation_layer_present('55c8634c-5b0a-4bd6-8677-365606184607').
narrative_ontology:cs_reading_relation('55c8634c-5b0a-4bd6-8677-365606184607', maat_order_principle__divine_mandate_reading, forecloses).
narrative_ontology:cs_reading_relation('55c8634c-5b0a-4bd6-8677-365606184607', maat_order_principle__distributed_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('55c8634c-5b0a-4bd6-8677-365606184607', foundational, pharaoh_accountable_to_maat).
narrative_ontology:cs_axiom_status(pharaoh_accountable_to_maat, holdable).
narrative_ontology:cs_axiom_grounding('55c8634c-5b0a-4bd6-8677-365606184607', pharaoh_accountable_to_maat, deontological).
narrative_ontology:cs_axiom('55c8634c-5b0a-4bd6-8677-365606184607', foundational, cosmic_balance_requires_reciprocity).
narrative_ontology:cs_axiom_status(cosmic_balance_requires_reciprocity, holdable).
narrative_ontology:cs_axiom_grounding('55c8634c-5b0a-4bd6-8677-365606184607', cosmic_balance_requires_reciprocity, conventional).
narrative_ontology:cs_reference_frame('55c8634c-5b0a-4bd6-8677-365606184607', balanced_cosmic_order).
narrative_ontology:cs_drift_state('55c8634c-5b0a-4bd6-8677-365606184607', historical_instability_periods, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('55c8634c-5b0a-4bd6-8677-365606184607', '').
narrative_ontology:cs_kernel_id(maat_order_principle__reciprocity_reading, maat_order_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, egyptian_priesthood).
narrative_ontology:constraint_beneficiary(maat_order_principle__reciprocity_reading, elite_officials).
narrative_ontology:constraint_victim(maat_order_principle__reciprocity_reading, common_egyptians).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The divine ruler, obligated to uphold Ma'at by providing justice, stability, and resource distribution. Collects taxes and labor, but is constrained by the cosmic order and the expectation of reciprocity. Failure to uphold Ma'at risks divine displeasure and human resistance.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, pharaoh, agenda_setter,
    institutional, generational, constrained, national).

% Interprets Ma'at, performs rituals, and advises the Pharaoh. Benefits from the stable order maintained by Ma'at, and reinforces the Pharaoh's legitimacy as long as Ma'at is upheld. They also serve as a check on the Pharaoh's power, reminding them of their obligations.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, egyptian_priesthood, beneficiary,
    institutional, generational, constrained, national).

% Administer the Pharaoh's decrees, manage resources, and maintain order. They benefit from the stability and hierarchy established by Ma'at, receiving status and wealth. Their loyalty is tied to the Pharaoh's ability to maintain Ma'at.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, elite_officials, beneficiary,
    powerful, biographical, mobile, national).

% Provide labor, taxes, and loyalty to the Pharaoh and the state. They expect justice, protection, and a stable environment in return. While generally trapped by the system, their suffering due to the Pharaoh's failure to uphold Ma'at can, in extreme cases, justify resistance or withdrawal of support.
narrative_ontology:constraint_stakeholder(maat_order_principle__reciprocity_reading, common_egyptians, payer,
    powerless, immediate, trapped, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(maat_order_principle__reciprocity_reading, pharaoh).
narrative_ontology:fixing_cost_class(maat_order_principle__reciprocity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, predictable social and cosmic order by defining mutual obligations between the Pharaoh, the gods, and the people, thereby preventing chaos (Isfet) and ensuring prosperity.
% TRANSFER_FUNCTION: Transfers labor, resources, and loyalty from common Egyptians to the Pharaoh and the state, in exchange for justice, stability, protection, and the maintenance of cosmic balance.
% ABSENT_VOICES: Those who would advocate for pure individual autonomy, challenge the divine basis of Ma'at, or propose a system without a central, divinely sanctioned ruler. Such voices are suppressed by the pervasive religious, social, and political structure of ancient Egypt.
% DISAPPEARANCE_RATIONALE: If the principle of Ma'at and its associated reciprocal obligations vanished overnight, the entire socio-political and religious structure of ancient Egypt would collapse. The legitimacy of the Pharaoh, the authority of the priesthood, and the very fabric of society, all dependent on Ma'at, would dissolve into chaos and instability.
% FOUNDING_PROBLEM: The inherent instability of human society and the natural world, and the perceived need for a divine principle to impose order, justice, and cosmic balance to prevent chaos (Isfet).
% FOUNDING_PROBLEM_CORROBORATION: Historical records, religious texts, and archaeological evidence from ancient Egypt consistently attest to the central and enduring role of Ma'at in maintaining societal and cosmic order. This is corroborated not only by royal inscriptions but also by wisdom literature, legal texts, and personal appeals to Ma'at from various social strata, indicating a widespread belief in its necessity.
narrative_ontology:disappearance_verdict(maat_order_principle__reciprocity_reading, world_rearranges).
narrative_ontology:founding_problem_status(maat_order_principle__reciprocity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(maat_order_principle__reciprocity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(maat_order_principle__reciprocity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(maat_order_principle__reciprocity_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maat_order_principle__reciprocity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(maat_order_principle__reciprocity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(maat_order_principle__reciprocity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.55) because while the Pharaoh collects significant resources and labor, there is a theoretical 'ceiling' on extraction imposed by the reciprocal obligations of Ma'at; excessive extraction would be seen as a violation. Suppression is high (0.70) due to the Pharaoh's immense authority and the pervasive religious and social structures that reinforce the system, but it's not absolute, as resistance is conceptually justified if Ma'at is violated. The theater ratio is low (0.15) because the belief in Ma'at and its practical application were deeply ingrained and genuinely functional for maintaining order, not merely performative. Accessibility collapse is moderate (0.65) as the alternative (chaos) is dire, but resistance, though risky, is not entirely foreclosed. Resistance is moderate (0.40) reflecting the potential for unrest or rebellion during periods of severe injustice or instability.
 *
 * PERSPECTIVAL GAP:
 *   From the Pharaoh's perspective, upholding Ma'at is a sacred duty that legitimizes their rule and ensures cosmic harmony. From the common Egyptian's perspective, Ma'at is a promise of justice and stability in exchange for their labor and loyalty, with the implicit threat of disorder if the Pharaoh fails. The engine's computation of per-seat classification will reflect this divergence, with the Pharaoh's seat experiencing a more beneficial classification than the commoners, despite both being within the Ma'at framework.
 *
 * DIRECTIONALITY LOGIC:
 *   The Pharaoh, priesthood, and elite officials are beneficiaries, as they directly gain from the stable, hierarchical order and resource flow. Common Egyptians are the primary payers, providing labor and resources. However, the reciprocal nature of Ma'at means that even beneficiaries are constrained by its demands, and payers receive essential benefits (justice, stability). The 'justified resistance' aspect for common Egyptians slightly dampens their directionality towards full target, as they have a theoretical lever, even if rarely exercised.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pharaoh_accountability_vs_divine_mandate,
    'To what extent was the Pharaoh genuinely accountable to Ma''at in practice, versus being an unchallengeable embodiment of it?',
    'Analysis of historical instances of popular unrest or elite challenges to Pharaohs during periods of perceived injustice or instability, and the subsequent justifications for such actions.',
    'If accountability was strong, this reading''s ''tangled_rope'' classification is robust. If the Pharaoh was largely unchallengeable, the constraint leans towards a ''snare'' or ''mountain'' (divine mandate) from the commoner''s perspective, with higher effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pharaoh_accountability_vs_divine_mandate, empirical, 'Ambiguity in Pharaoh''s practical accountability to Ma''at.').

omega_variable(
    reciprocity_vs_distributed_responsibility,
    'Was the maintenance of Ma''at primarily the Pharaoh''s reciprocal obligation, or a distributed responsibility across all societal levels?',
    'Comparative textual analysis of different ancient Egyptian sources (royal decrees vs. wisdom literature vs. private inscriptions) to gauge the emphasis on Pharaoh''s unique role versus collective duty.',
    'If responsibility was highly distributed, the ''reciprocity_reading'' might overstate the Pharaoh''s unique burden and the justification for resistance, potentially shifting the constraint''s classification towards a ''rope'' (distributed maintenance) for commoners, with lower perceived extraction from the Pharaoh.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reciprocity_vs_distributed_responsibility, conceptual, 'Ambiguity in the locus of Ma''at maintenance responsibility.').

omega_variable(
    justified_resistance_practicality,
    'What were the practical mechanisms and actual frequency of ''justified resistance'' against a Pharaoh perceived to be violating Ma''at?',
    'Archaeological and textual evidence of revolts, strikes, or other forms of civil disobedience explicitly framed as responses to a Pharaoh''s failure to uphold Ma''at.',
    'If ''justified resistance'' was rare and ineffective, the common Egyptians'' exit options are more ''trapped'' than ''constrained'', increasing their effective extraction and pushing the constraint closer to a ''snare'' from their seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(justified_resistance_practicality, empirical, 'Practicality and efficacy of resistance against Pharaoh''s failure to uphold Ma''at.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maat_order_principle__reciprocity_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maat_tr_t0, maat_order_principle__reciprocity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(maat_tr_t20, maat_order_principle__reciprocity_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(maat_tr_t40, maat_order_principle__reciprocity_reading, theater_ratio, 40, 0.15).
narrative_ontology:measurement(maat_tr_t60, maat_order_principle__reciprocity_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement(maat_tr_t80, maat_order_principle__reciprocity_reading, theater_ratio, 80, 0.15).
narrative_ontology:measurement(maat_tr_t100, maat_order_principle__reciprocity_reading, theater_ratio, 100, 0.15).

% Extraction over time
narrative_ontology:measurement(maat_be_t0, maat_order_principle__reciprocity_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(maat_be_t20, maat_order_principle__reciprocity_reading, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(maat_be_t40, maat_order_principle__reciprocity_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(maat_be_t60, maat_order_principle__reciprocity_reading, base_extractiveness, 60, 0.56).
narrative_ontology:measurement(maat_be_t80, maat_order_principle__reciprocity_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(maat_be_t100, maat_order_principle__reciprocity_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(maat_su_t0, maat_order_principle__reciprocity_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(maat_su_t20, maat_order_principle__reciprocity_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(maat_su_t40, maat_order_principle__reciprocity_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(maat_su_t60, maat_order_principle__reciprocity_reading, suppression_requirement, 60, 0.71).
narrative_ontology:measurement(maat_su_t80, maat_order_principle__reciprocity_reading, suppression_requirement, 80, 0.7).
narrative_ontology:measurement(maat_su_t100, maat_order_principle__reciprocity_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maat_order_principle__reciprocity_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
