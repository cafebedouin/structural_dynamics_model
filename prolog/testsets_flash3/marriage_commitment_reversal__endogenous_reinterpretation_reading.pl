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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: marriage_commitment_reversal__endogenous_reinterpretation_reading
 *   human_readable: Endogenous Reinterpretation of Marriage Commitment (Woodruff's Vision)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   This constraint describes the shift in a religious institution's marriage
 *   practice, framed internally as a divine revelation received by the
 *   prophet (Woodruff's 1890 Manifesto). This reading emphasizes the
 *   internal, spiritual justification for the change, reinterpreting God's
 *   will to align with new circumstances, thereby preserving the prophet's
 *   interpretive authority and institutional legitimacy. It is one reading of
 *   a contested kernel, where other readings emphasize external coercion or a
 *   persistent doctrine-practice gap.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Endogenous Reinterpretation of Marriage Commitment (Woodruff's Vision)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'ad93720f-6b88-4dd8-b59e-4bb78ce4ed92').
narrative_ontology:cs_kernel_codification('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', formalized).
narrative_ontology:cs_authority_grounding('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', lineage).
narrative_ontology:cs_interpretation_layer_present('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92').
narrative_ontology:cs_reading_relation('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', marriage_commitment_reversal__exogenous_override_reading, influences).
narrative_ontology:cs_reading_relation('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', foundational, prophetic_revelation_is_supreme_interpretive_authority).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_supreme_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', prophetic_revelation_is_supreme_interpretive_authority, theological).
narrative_ontology:cs_axiom('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', foundational, divine_will_adapts_to_circumstance).
narrative_ontology:cs_axiom_status(divine_will_adapts_to_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', divine_will_adapts_to_circumstance, theological).
narrative_ontology:cs_reference_frame('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', prophetic_interpretive_authority).
narrative_ontology:cs_drift_state('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', post_manifesto_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ad93720f-6b88-4dd8-b59e-4bb78ce4ed92', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members_seeking_social_acceptance).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).
narrative_ontology:constraint_victim(marriage_commitment_reversal__endogenous_reinterpretation_reading, members_committed_to_prior_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The prophet and senior leadership who received and promulgated the revelation. They maintain interpretive authority and guide the church through doctrinal shifts, preserving institutional legitimacy amidst external pressure.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% The internal coherence and historical continuity of divine revelation. It 'pays' by being reinterpreted or seemingly contradicted, creating a challenge for future doctrinal development and apologetics.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% Members who had deeply internalized and practiced the prior marriage commitment as a divine command. They bear the cost of cognitive dissonance, social pressure, and potential loss of status if they resist the new interpretation.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, members_committed_to_prior_practice, payer,
    moderate, biographical, identity_locked, local).

% Members who benefit from the church's increased social acceptance and reduced persecution due to the change in practice. They experience relief from external pressure and internal conflict.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members_seeking_social_acceptance, beneficiary,
    moderate, biographical, mobile, local).

% Historians, theologians, and critics outside the institution who analyze the event, often questioning the divine nature of the revelation and pointing to external pressures as the true cause of the change.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, external_observers_critics, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective understanding and practice of marriage within the religious community, ensuring unity and adherence to the current divine mandate as interpreted by leadership.
% TRANSFER_FUNCTION: Transfers interpretive authority and institutional stability to the leadership, while transferring the burden of doctrinal re-alignment and cognitive dissonance to members and the abstract concept of theological consistency.
% ABSENT_VOICES: Those who might have argued for a more literal adherence to prior revelations or a more direct confrontation with external pressures are either marginalized or their dissent is framed as a lack of faith in the prophet's divine mandate.
% DISAPPEARANCE_RATIONALE: If the revelation and its institutional enforcement vanished, the church's current marriage practices would immediately lose their divine sanction, leading to widespread confusion, potential schism, and a return to prior (or entirely new) interpretations, fundamentally altering the institution's structure and identity.
% FOUNDING_PROBLEM: The problem of reconciling a divinely commanded practice with severe external legal and social persecution, threatening the very existence and property of the religious institution.
% FOUNDING_PROBLEM_CORROBORATION: The institutional leadership attests the problem was resolved by divine intervention. External historians and critics corroborate the existence of the external persecution but attribute the 'solution' to pragmatic institutional adaptation rather than solely divine will, pointing to the 'exogenous_override_reading' as a more accurate account.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the community's practice (beneficiaries: institutional_leadership, church_members_seeking_social_acceptance) while simultaneously extracting from theological consistency and members committed to prior practice (victims). Active enforcement is required to ensure adherence to the new interpretation and suppress dissent. Extractiveness is moderate, reflecting the cost of doctrinal re-alignment and the suppression of alternative interpretations. Suppression is high due to the institutional authority of revelation. Theater ratio is low, as the revelation is genuinely believed by many within the institution, even if external factors are also at play.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of institutional leadership, this is a necessary divine adjustment, preserving the church. From the perspective of theological consistency, it's a strain on internal coherence. From the perspective of members committed to prior practice, it's a difficult, identity-challenging shift. The engine will compute these divergences based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership benefits from maintaining interpretive authority and guiding the church through crisis (low d). Theological consistency and members committed to prior practice bear the costs of reinterpretation and adaptation (high d). Church members seeking social acceptance benefit from reduced persecution (low d). External observers are analytical (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Snare, which would ignore the genuine coordination function of maintaining institutional unity and adapting to existential threats. It also prevents mislabeling it as a pure Rope, which would ignore the significant extraction from theological consistency and dissenting members. The 'endogenous reinterpretation' framing is crucial for the institution to avoid mandatrophy, as it re-legitimizes the constraint's function through divine will, even if the founding problem (external persecution) is eventually resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_vs_pragmatic_causation,
    'Was the change in practice primarily driven by genuine divine revelation, or was the revelation a pragmatic institutional response to overwhelming external coercion?',
    'Analysis of internal church records, contemporary diaries, and external political pressures leading up to the revelation. Comparison of the revelation''s timing and content with federal legislative and judicial actions.',
    'If primarily pragmatic, the constraint''s ''divine'' justification becomes theatrical, increasing the theater_ratio and potentially reclassifying it closer to a Snare or Piton, as the coordination story becomes cover for institutional survival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_vs_pragmatic_causation, empirical, 'Ambiguity regarding the primary causal driver of the doctrinal shift.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of dissent structural (institutional authority, social pressure) or internalized (members'' belief in prophetic infallibility, identity fusion with the institution)?',
    'Post-exit suppression trajectory: if suppression of prior practice persists among ex-members after leaving the institution, it suggests internalized suppression. If it dissipates, it points to structural suppression.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — members carry the suppression with them after exit, making resistance harder. If purely structural, removing institutional pressure would lead to faster re-evaluation of prior practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for adherence to new doctrine.').

omega_variable(
    theological_consistency_cost,
    'What is the long-term cost to the institution''s theological framework of reinterpreting prior divine commands through new revelation?',
    'Analysis of subsequent doctrinal developments, apologetic efforts, and internal debates over generations. Does it lead to a more flexible interpretive framework or to ongoing internal contradictions?',
    'If the cost is high, it indicates a deeper, ongoing extraction from the ''theological_consistency'' victim, potentially increasing the constraint''s extractiveness over time as more interpretive work is required to maintain coherence. If low, it suggests the reinterpretation was successfully integrated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(theological_consistency_cost, conceptual, 'Long-term impact of reinterpretation on doctrinal coherence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.2).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1895, 0.25).
narrative_ontology:measurement_basis(marr_tr_t1895, observed).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.2).
narrative_ontology:measurement_basis(marr_tr_t1900, observed).
narrative_ontology:measurement(marr_tr_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1905, 0.18).
narrative_ontology:measurement_basis(marr_tr_t1905, observed).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.15).
narrative_ontology:measurement_basis(marr_tr_t1910, observed).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.2).
narrative_ontology:measurement_basis(marr_tr_t1920, observed).

% Extraction over time
narrative_ontology:measurement(marr_be_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1890, 0.4).
narrative_ontology:measurement_basis(marr_be_t1890, observed).
narrative_ontology:measurement(marr_be_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1895, 0.45).
narrative_ontology:measurement_basis(marr_be_t1895, observed).
narrative_ontology:measurement(marr_be_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1900, 0.45).
narrative_ontology:measurement_basis(marr_be_t1900, observed).
narrative_ontology:measurement(marr_be_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1905, 0.43).
narrative_ontology:measurement_basis(marr_be_t1905, observed).
narrative_ontology:measurement(marr_be_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1910, 0.42).
narrative_ontology:measurement_basis(marr_be_t1910, observed).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement_basis(marr_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.6).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1895, 0.65).
narrative_ontology:measurement_basis(marr_su_t1895, observed).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement_basis(marr_su_t1900, observed).
narrative_ontology:measurement(marr_su_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1905, 0.58).
narrative_ontology:measurement_basis(marr_su_t1905, observed).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.55).
narrative_ontology:measurement_basis(marr_su_t1910, observed).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement_basis(marr_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel. This reading emphasizes the internal, divine reinterpretation. It influences the other readings by providing a theological justification that complicates purely external or gap-focused analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
