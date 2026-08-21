% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__practice_doctrine_gap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__practice_doctrine_gap, []).

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
 *   constraint_id: marriage_commitment_reversal__practice_doctrine_gap
 *   human_readable: Religious Institution's Marriage Doctrine-Practice Gap (1890-1904)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the structural ambiguity within a religious
 *   institution during a period (1890-1904) where its core
 *   marriage-commitment principle (Section 132) was preserved in doctrine,
 *   while its public practice was suspended to comply with federal law. This
 *   reading, 'practice_doctrine_gap', focuses on the tension and consequences
 *   of this structural ambiguity. The institution's claimed type is 'rope'
 *   (coordinating survival and doctrinal preservation), but the metrics
 *   reflect the high extraction and suppression inherent in maintaining this
 *   gap. Sibling readings, 'exogenous_override_reading' and
 *   'endogenous_reinterpretation_reading', offer alternative explanations for
 *   the observed changes in practice.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, 0.8).
domain_priors:suppression_score(marriage_commitment_reversal__practice_doctrine_gap, 0.75).
domain_priors:theater_ratio(marriage_commitment_reversal__practice_doctrine_gap, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, extractiveness, 0.8).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__practice_doctrine_gap, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__practice_doctrine_gap, rope).
narrative_ontology:human_readable(marriage_commitment_reversal__practice_doctrine_gap, "Religious Institution's Marriage Doctrine-Practice Gap (1890-1904)").
narrative_ontology:topic_domain(marriage_commitment_reversal__practice_doctrine_gap, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__practice_doctrine_gap).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__practice_doctrine_gap, '17c8dfe2-0b58-43f7-9f8f-c70f80da0f23').
narrative_ontology:cs_kernel_codification('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', fixed_text).
narrative_ontology:cs_authority_grounding('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', lineage).
narrative_ontology:cs_interpretation_layer_present('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23').
narrative_ontology:cs_reading_relation('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_axiom('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', foundational, doctrinal_principle_is_immutable).
narrative_ontology:cs_axiom_status(doctrinal_principle_is_immutable, holdable).
narrative_ontology:cs_axiom_grounding('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', doctrinal_principle_is_immutable, deontological).
narrative_ontology:cs_axiom('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', foundational, institutional_flexibility_is_necessary).
narrative_ontology:cs_axiom_status(institutional_flexibility_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', institutional_flexibility_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', doctrinal_integrity_as_ideal).
narrative_ontology:cs_drift_state('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', post_manifesto_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('17c8dfe2-0b58-43f7-9f8f-c70f80da0f23', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, general_membership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Navigated external threats by publicly suspending a core practice while preserving the doctrine, gaining institutional flexibility and ensuring survival. Benefited from the ambiguity that allowed continued, albeit unacknowledged, practice in some areas.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership, agenda_setter,
    institutional, generational, arbitrage, global).

% Experienced bewilderment and a loss of clarity due to the gap between declared doctrine and observed practice. Expected to maintain loyalty and compliance despite the ambiguity, often at the cost of personal conviction or understanding.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, general_membership, payer,
    powerless, biographical, constrained, local).

% Perceived the suspension of practice as a betrayal of core doctrine, leading to internal dissent and eventual schism. Their identity was deeply tied to the strict adherence to the original principle, making compromise or acceptance of ambiguity impossible.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, fundamentalist_factions, payer,
    organized, biographical, identity_locked, regional).

% Exerted external legal and political pressure that forced the institution to publicly alter its practices. While not a direct party to the internal doctrinal gap, its actions were the primary catalyst for the institutional response.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, federal_government, observer,
    institutional, generational, analytical, national).

% Analyze the historical context, institutional responses, and long-term impacts of the doctrine-practice gap, providing an external, analytical perspective on the constraint's operation and consequences.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__practice_doctrine_gap, historical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__practice_doctrine_gap, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__practice_doctrine_gap, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allowed the institution to coordinate its response to external legal threats by publicly complying with federal law while internally preserving its core doctrinal claims, thereby maintaining a semblance of continuity and unity for a broad membership.
% TRANSFER_FUNCTION: Transferred clarity, certainty, and doctrinal consistency from the general membership and fundamentalist factions to the institutional leadership, in exchange for institutional survival and strategic flexibility in a hostile legal environment.
% ABSENT_VOICES: Those who left the institution due to the perceived hypocrisy or irreconcilable ambiguity; future generations seeking clear, unambiguous doctrinal guidance; and those who suffered personal distress or loss of faith due to the institutional maneuvering.
% DISAPPEARANCE_RATIONALE: If this gap had not existed, the institution would have either faced more severe federal persecution (potentially leading to its dissolution or radical transformation) or would have been forced to formally abandon or reinterpret its doctrine, fundamentally altering its theological and social structure. The schism with fundamentalist groups would have been either avoided or more decisively resolved earlier.
% FOUNDING_PROBLEM: The institution faced existential threats from the federal government over its core marriage practices, which were deeply rooted in its theological principles, creating an irreconcilable conflict between religious freedom and state law.
% FOUNDING_PROBLEM_CORROBORATION: Independent historians, legal scholars, and sociological studies of religious movements corroborate the severe federal pressure and the institutional leadership's strategic response. While the specific federal threats of the 1890s are gone, the institutional precedent set by this period continues to influence its internal dynamics and external relations.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__practice_doctrine_gap, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__practice_doctrine_gap, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__practice_doctrine_gap, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_reversal__practice_doctrine_gap, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__practice_doctrine_gap, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__practice_doctrine_gap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__practice_doctrine_gap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.8) because the institutional leadership gained flexibility and survival at the cost of membership clarity and doctrinal consistency. Suppression (0.75) was necessary to manage internal dissent and maintain a unified front despite the ambiguity. The theater ratio (0.5) reflects the performative aspect of preserving doctrine while practice diverged. The claimed type 'rope' reflects the institution's framing of its actions as necessary coordination for survival and doctrinal preservation, even as the metrics reveal the extractive nature of the gap.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, the actions were a necessary, albeit difficult, coordination to preserve the institution and its core principles. From the perspective of the general membership and fundamentalist factions, it was a period of confusion, betrayal, and extraction, where their commitment to clear doctrine was sacrificed for institutional pragmatism.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership is the primary beneficiary (low d) due to gaining flexibility and ensuring survival. General membership and fundamentalist factions are targets (high d) as they bore the costs of bewilderment, loss of clarity, and schism. The federal government acted as an external force, not a direct participant in the internal gap dynamics, but its pressure was the catalyst.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deliberate_strategy_vs_unintended_gap,
    'Was the doctrine-practice gap a deliberate, calculated strategy by the institutional leadership, or an unintended consequence of navigating an impossible situation?',
    'Access to internal leadership communications, diaries, and policy discussions from the period, combined with detailed analysis of decision-making processes.',
    'If deliberate, the extractiveness and suppression metrics are more firmly established as intentional; if unintended, the constraint might be reclassified towards a more ''constrained'' or ''scaffold''-like type, reflecting the difficult circumstances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deliberate_strategy_vs_unintended_gap, conceptual, 'Whether the ambiguity was a strategic choice or an emergent property of crisis.').

omega_variable(
    true_cost_to_membership_clarity,
    'What was the full, long-term cost to membership clarity, faith, and institutional trust resulting from this doctrine-practice gap?',
    'Longitudinal sociological studies of former members, analysis of internal dissent records, and comparative studies with institutions that handled similar crises differently.',
    'A higher demonstrated cost would amplify the effective extraction from the general membership, potentially shifting the constraint''s classification further towards ''snare'' for those seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(true_cost_to_membership_clarity, empirical, 'Quantifying the impact of ambiguity on member experience and institutional trust.').

omega_variable(
    ambiguity_role_in_institutional_survival,
    'To what extent did the maintenance of this ambiguity (rather than a clear doctrinal change or full defiance) contribute to the institution''s survival?',
    'Counterfactual historical analysis, comparing outcomes with hypothetical scenarios of clear doctrinal change or outright defiance, and examining contemporary institutional responses to similar pressures.',
    'If the ambiguity was critical for survival, it strengthens the ''rope'' aspect of the constraint (coordination for survival); if less critical, it highlights the purely extractive nature of the gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ambiguity_role_in_institutional_survival, empirical, 'Assessing the functional necessity of the doctrine-practice gap for institutional survival.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__practice_doctrine_gap, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1890, 0.4).
narrative_ontology:measurement(marr_tr_t1894, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1894, 0.45).
narrative_ontology:measurement(marr_tr_t1898, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1898, 0.48).
narrative_ontology:measurement(marr_tr_t1904, marriage_commitment_reversal__practice_doctrine_gap, theater_ratio, 1904, 0.5).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1890, 0.7).
narrative_ontology:measurement(marr_be_t1894, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1894, 0.75).
narrative_ontology:measurement(marr_be_t1898, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1898, 0.78).
narrative_ontology:measurement(marr_be_t1904, marriage_commitment_reversal__practice_doctrine_gap, base_extractiveness, 1904, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1890, 0.65).
narrative_ontology:measurement(marr_su_t1894, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1894, 0.7).
narrative_ontology:measurement(marr_su_t1898, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1898, 0.73).
narrative_ontology:measurement(marr_su_t1904, marriage_commitment_reversal__practice_doctrine_gap, suppression_requirement, 1904, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__practice_doctrine_gap, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__practice_doctrine_gap, marriage_commitment_reversal__endogenous_reinterpretation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel, focusing on the structural ambiguity between doctrine and practice. It is linked to sibling readings that emphasize external coercion or internal reinterpretation as primary drivers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
