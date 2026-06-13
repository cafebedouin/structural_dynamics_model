% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_commitment_reversal__exogenous_override_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: LDS Marriage Commitment Reversal (Exogenous Override Reading)
 *   domain: religious_institutional_history/commitment_systems/political_theology
 *
 * SUMMARY:
 *   This constraint describes the reversal of the LDS Church's practice of
 *   plural marriage, specifically through the lens of external coercion by
 *   the United States federal government. It posits that the change was not
 *   primarily driven by internal doctrinal reinterpretation but by
 *   overwhelming federal legislative and judicial pressure, including the
 *   threat of disincorporation and seizure of church assets. Section 132 of
 *   the Doctrine and Covenants, which outlines the principle of plural
 *   marriage, is understood to have remained doctrinally intact, with
 *   practice suspended due to external force.
 *
 * KEY AGENTS:
 *   - federal_government: Primary agenda_setter (institutional/civilizational) — imposed the constraint
 *   - lds_church_sovereignty: Primary victim (institutional/generational) — bore the extraction of autonomy
 *   - polygamous_families: Direct victims (powerless/biographical) — forced to abandon practice
 *   - us_public_opinion: Beneficiary (organized/generational) — saw its moral norms enforced
 *   - lds_leadership: Payer/agenda_setter (institutional/generational) — navigated compliance under duress
 *   - historical_scholars: Observer (analytical/civilizational) — analyze the causal drivers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.9).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "LDS Marriage Commitment Reversal (Exogenous Override Reading)").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/commitment_systems/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '41da0010-a1c7-4060-8d55-b7577bf0e04d').
narrative_ontology:cs_kernel_codification('41da0010-a1c7-4060-8d55-b7577bf0e04d', fixed_text).
narrative_ontology:cs_authority_grounding('41da0010-a1c7-4060-8d55-b7577bf0e04d', extraction).
narrative_ontology:cs_interpretation_layer_present('41da0010-a1c7-4060-8d55-b7577bf0e04d').
narrative_ontology:cs_reading_relation('41da0010-a1c7-4060-8d55-b7577bf0e04d', marriage_commitment_reversal__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('41da0010-a1c7-4060-8d55-b7577bf0e04d', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('41da0010-a1c7-4060-8d55-b7577bf0e04d', foundational, federal_sovereignty_over_territorial_religious_practice).
narrative_ontology:cs_axiom_status(federal_sovereignty_over_territorial_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('41da0010-a1c7-4060-8d55-b7577bf0e04d', federal_sovereignty_over_territorial_religious_practice, conventional).
narrative_ontology:cs_axiom('41da0010-a1c7-4060-8d55-b7577bf0e04d', secondary, religious_freedom_subordinate_to_public_morality).
narrative_ontology:cs_axiom_status(religious_freedom_subordinate_to_public_morality, holdable).
narrative_ontology:cs_axiom_grounding('41da0010-a1c7-4060-8d55-b7577bf0e04d', religious_freedom_subordinate_to_public_morality, deontological).
narrative_ontology:cs_reference_frame('41da0010-a1c7-4060-8d55-b7577bf0e04d', federal_supremacy_over_territorial_morality).
narrative_ontology:cs_drift_state('41da0010-a1c7-4060-8d55-b7577bf0e04d', contemporary_religious_freedom_jurisprudence, gap(authority_erosion, minor, true)).
narrative_ontology:cs_created_at('41da0010-a1c7-4060-8d55-b7577bf0e04d', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, us_public_opinion).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_church_sovereignty).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, polygamous_families).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_commitment_reversal__exogenous_override_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the federal government successfully compelled a sovereign religious institution to abandon a core practice, effectively extracting institutional autonomy. Suppression (0.90) is severe, reflecting the comprehensive legal and military power brought to bear, leaving no viable exit for the Church or its members to continue the practice publicly. The theater ratio (0.40) indicates that while some internal justification for the change was presented, a significant portion of the public messaging and compliance was performative, masking the underlying coercion. Accessibility collapse (0.75) is high as legal and social alternatives for continuing the practice were systematically eliminated. Resistance (0.80) was initially high, leading to federal escalation, but eventually yielded to overwhelming force.
 *
 * PERSPECTIVAL GAP:
 *   The federal government's perspective would frame this as enforcing federal law and moral norms, a legitimate exercise of national sovereignty. The LDS Church, from this reading, experienced it as an existential threat and an infringement on religious freedom, a coercive act. The engine's classification will reflect this divergence, with the federal government as a beneficiary of a Snare, and the Church as a victim.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is a full beneficiary (d=0.0) as it successfully imposed its will and expanded its territorial control over the LDS Church's institutional autonomy. The LDS Church's sovereignty is a full target (d=1.0) as it was compelled to abandon a core practice under duress. Polygamous families are also targets (d=1.0) as they faced direct legal and social consequences. US public opinion is a beneficiary (d=0.0) as its moral norms were enforced. LDS leadership, while acting as agenda-setters in implementing the change, were also payers (d=0.7) due to the immense pressure they faced.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as an internal 'Rope' of religious evolution or a 'Mountain' of inevitable social change. By emphasizing exogenous coercion, it correctly identifies the active, extractive force at play, rather than attributing the outcome to internal dynamics or natural progression. The persistence of Section 132 as doctrine, despite the suspension of practice, is key to this distinction, indicating that the mandate for plural marriage was not internally revoked but externally suppressed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint primarily an exogenous override, an endogenous reinterpretation, or a persistent doctrine-practice gap?',
    'Analysis of primary source documents (e.g., Woodruff Manifesto, official church statements, federal court records) for explicit causal claims and internal doctrinal shifts vs. external pressures. This reading emphasizes federal coercion as the primary driver.',
    'If primarily exogenous, the constraint is a Snare imposed by external power. If endogenous, it''s a Rope or Tangled Rope of internal coordination. If a gap, it''s a Piton of inertial doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''exogenous_override_reading'' of the ''marriage_commitment_reversal'' kernel. Sibling readings include ''endogenous_reinterpretation_reading'' and ''practice_doctrine_gap''.').

omega_variable(
    doctrinal_revision_ambiguity,
    'To what extent did the suspension of polygamous practice constitute an internal doctrinal revision versus a temporary compliance under duress?',
    'Examination of subsequent doctrinal developments and official interpretations within the LDS Church regarding Section 132 of the Doctrine and Covenants. If Section 132 is still affirmed as principle, it supports the duress reading.',
    'If no genuine doctrinal revision occurred, the ''exogenous_override_reading'' is strengthened, highlighting the coercive nature of the constraint. If revision is evident, it leans towards the ''endogenous_reinterpretation_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_revision_ambiguity, empirical, 'Ambiguity regarding the permanence of doctrinal change versus temporary compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 1862, 1890).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 15, 0.4).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 15, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 15, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__exogenous_override_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'marriage_commitment_reversal' kernel, focusing on external coercion. Other readings include 'endogenous_reinterpretation_reading' and 'practice_doctrine_gap', which offer alternative causal accounts for the same historical event.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
