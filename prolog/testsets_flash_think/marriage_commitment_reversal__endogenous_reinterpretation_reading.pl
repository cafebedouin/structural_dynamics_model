% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__endogenous_reinterpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__endogenous_reinterpretation_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation of Plural Marriage Practice (Woodruff's Sept 23 Vision)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the reinterpretation of divine will regarding
 *   plural marriage within a religious institution, specifically through the
 *   lens of an internal divine revelation (Woodruff's Sept 23 vision). This
 *   reading frames the reversal of practice as an endogenous theological
 *   adjustment to changed circumstances, preserving the prophet's
 *   interpretive authority and institutional legitimacy. The claimed type is
 *   'rope' (divine guidance for coordination), but the metrics reflect the
 *   extraction of theological consistency and the imposition of new practice
 *   on adherents.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.5).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.5).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Endogenous Reinterpretation of Plural Marriage Practice (Woodruff's Sept 23 Vision)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'e9e5791e-15f1-4ef4-8265-92da8d0f0cac').
narrative_ontology:cs_kernel_codification('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', fixed_text).
narrative_ontology:cs_authority_grounding('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', lineage).
narrative_ontology:cs_interpretation_layer_present('e9e5791e-15f1-4ef4-8265-92da8d0f0cac').
narrative_ontology:cs_reading_relation('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', marriage_commitment_reversal__practice_doctrine_gap, forecloses).
narrative_ontology:cs_axiom('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', foundational, divine_will_is_dynamic).
narrative_ontology:cs_axiom_status(divine_will_is_dynamic, holdable).
narrative_ontology:cs_axiom_grounding('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', divine_will_is_dynamic, theological).
narrative_ontology:cs_axiom('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', foundational, prophetic_revelation_is_supreme).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', prophetic_revelation_is_supreme, theological).
narrative_ontology:cs_reference_frame('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', prophetic_revelation_as_ultimate_arbiter).
narrative_ontology:cs_drift_state('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', post_woodruff_manifesto, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('e9e5791e-15f1-4ef4-8265-92da8d0f0cac', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, adherents_who_practiced_plural_marriage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, future_generations_of_adherents).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, future_generations_of_adherents).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The prophet and senior leadership who issued the revelation, reinterpreting God's will to suspend the practice of plural marriage. They maintain interpretive authority and institutional legitimacy by framing the change as divine guidance.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% Members who had entered into plural marriages based on prior divine command. They are now required to cease new plural marriages and, in some cases, separate existing ones, facing significant personal and social disruption. Their identity is deeply tied to the faith, making exit unthinkable.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, adherents_who_practiced_plural_marriage, payer,
    powerless, biographical, identity_locked, local).

% The coherence of divine commands across time. This reading sacrifices strict theological consistency by asserting God's will changed, creating a tension with prior, seemingly eternal, commands.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, payer,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% Benefit from the institutional stability and social acceptance gained by conforming to external legal norms. They pay by inheriting a more complex theological history and potentially a less direct connection to foundational practices.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, future_generations_of_adherents, beneficiary,
    powerless, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__endogenous_reinterpretation_reading, future_generations_of_adherents, payer).

% The external political authority whose anti-polygamy laws created the circumstances for the reinterpretation. While not directly involved in the internal theological process, their pressure is the unacknowledged driver of the 'changed circumstances'.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, federal_government, excluded,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__endogenous_reinterpretation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To align the religious practice of plural marriage with a new, divinely revealed interpretation of God's will, ensuring institutional unity and continued adherence to prophetic authority.
% TRANSFER_FUNCTION: Transfers interpretive flexibility and the power to define divine will to the living prophet, from prior fixed doctrine and the lived practice of adherents. It also transfers social and legal risk from the institution to individual adherents.
% ABSENT_VOICES: Adherents who felt betrayed by the reversal, particularly those who had made significant life commitments based on prior divine commands. Also, theological purists who might question the authenticity or timing of a revelation that so conveniently aligned with external pressures.
% DISAPPEARANCE_RATIONALE: If this reinterpretation vanished, the institution's legitimacy would be severely undermined, as its current practice would contradict prior divine commands without a theological resolution. The entire basis of prophetic authority and the social structure of the faith would be thrown into chaos, requiring a fundamental reorganization.
% FOUNDING_PROBLEM: To reconcile a divinely commanded practice (plural marriage) with severe external legal and social pressures (federal anti-polygamy laws) that threatened the very existence of the religious institution, while preserving the authority of the prophet and the integrity of divine guidance.
% FOUNDING_PROBLEM_CORROBORATION: The institutional leadership attests that the challenge of aligning divine will with changing circumstances remains a live issue, requiring ongoing prophetic guidance. External historians and sociologists corroborate the severe external pressures faced by the institution at the time, but not the divine origin of the reinterpretation itself, often framing it as a strategic adaptation.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).
:- end_tests(marriage_commitment_reversal__endogenous_reinterpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45 initially, decreasing to 0.35) because while the reinterpretation imposes significant costs on adherents and theological consistency, it also provides a path for institutional survival and stability. Suppression is high (0.60 initially, peaking at 0.72) as the revelation narrative actively suppresses dissent and alternative interpretations, requiring strict adherence to the new practice. Theater ratio is moderate (0.50 initially, decreasing to 0.25) reflecting the initial performative aspect of presenting a divinely convenient revelation, which gradually becomes more integrated and less theatrical as the new practice solidifies.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this is a divinely guided adjustment (rope) necessary for the survival and continued spiritual progression of the community. From the perspective of adherents who had practiced plural marriage, it is a painful reversal of a prior divine command, requiring significant personal sacrifice and a re-evaluation of their understanding of God's unchanging nature. The engine's computation of per-seat types will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The institutional leadership is the primary beneficiary, gaining enhanced interpretive authority and preserving institutional legitimacy. Adherents who practiced plural marriage are victims, bearing the direct costs of abandoning a divinely sanctioned practice. Theological consistency is also a victim, as the narrative of changing divine will creates internal tensions. Future generations are beneficiaries of institutional stability but pay through a more complex theological inheritance. The federal government is an external force, excluded from the internal theological process but driving the 'changed circumstances'.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as 'rope' (claimed) with significant extraction (metrics) prevents mislabeling this as pure extraction. The coordination function (institutional survival, unity) is real, but it comes at a cost to specific groups and theological principles. The revelation narrative serves to maintain the constraint's legitimacy despite its extractive elements, obscuring the underlying strategic adaptation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    revelation_authenticity_vs_strategic_response,
    'Was the Woodruff Manifesto a genuine divine revelation, or a strategic institutional response to overwhelming federal pressure, framed as revelation to preserve authority?',
    'Analysis of internal institutional records, private correspondence of leaders, and comparative studies of religious adaptations to state pressure. Historical evidence of pre-revelation discussions about legal and political consequences would be key.',
    'If primarily strategic, the extractiveness and theater_ratio would be higher, and the claimed_type (rope) would be a stronger cover story for a tangled_rope or snare. If genuinely revelatory, the ''rope'' classification would be more structurally accurate, with extraction representing the cost of divine adjustment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revelation_authenticity_vs_strategic_response, empirical, 'Ambiguity of divine revelation''s origin: genuine vs. strategic.').

omega_variable(
    theological_consistency_cost,
    'What is the long-term theological cost of asserting that God''s will can change in response to external circumstances, particularly for doctrines previously declared eternal?',
    'Longitudinal study of doctrinal development, internal theological debates, and shifts in hermeneutical approaches within the institution over generations. Analysis of how subsequent generations reconcile this historical precedent with claims of divine immutability.',
    'If the cost is high, it indicates a deeper, ongoing extraction from theological consistency, potentially leading to future internal conflicts or further reinterpretation. If successfully integrated, it suggests the ''divine will is dynamic'' axiom has become a foundational, low-cost principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_consistency_cost, conceptual, 'Theological cost of dynamic divine will.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1895, 0.48).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.45).
narrative_ontology:measurement(marr_tr_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1905, 0.4).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.35).
narrative_ontology:measurement(marr_tr_t1915, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1915, 0.3).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.25).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.45).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1895, 0.42).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.4).
narrative_ontology:measurement(marr_be_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1905, 0.38).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.36).
narrative_ontology:measurement(marr_be_t1915, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1915, 0.35).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1895, 0.65).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(marr_su_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1905, 0.72).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.7).
narrative_ontology:measurement(marr_su_t1915, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1915, 0.68).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
