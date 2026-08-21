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
 *   This constraint describes the institutional mechanism by which a
 *   religious organization reversed a core practice (plural marriage) in
 *   response to external pressure, framing the reversal as an internal divine
 *   revelation (Woodruff's 1890 Manifesto). This reading emphasizes the
 *   church leadership's agency in reinterpreting God's will under changed
 *   circumstances, preserving institutional legitimacy and prophetic
 *   authority. It is one reading of the broader
 *   'marriage_commitment_reversal' kernel, which also includes
 *   interpretations emphasizing external coercion or a persistent
 *   doctrine-practice gap.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.45).
domain_priors:suppression_score(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.6).
domain_priors:theater_ratio(marriage_commitment_reversal__endogenous_reinterpretation_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(marriage_commitment_reversal__endogenous_reinterpretation_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__endogenous_reinterpretation_reading, tangled_rope).
narrative_ontology:human_readable(marriage_commitment_reversal__endogenous_reinterpretation_reading, "Endogenous Reinterpretation of Marriage Commitment (Woodruff's Vision)").
narrative_ontology:topic_domain(marriage_commitment_reversal__endogenous_reinterpretation_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__endogenous_reinterpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__endogenous_reinterpretation_reading, 'ce7d7703-4727-4bac-bcb1-08ebc17af542').
narrative_ontology:cs_kernel_codification('ce7d7703-4727-4bac-bcb1-08ebc17af542', formalized).
narrative_ontology:cs_authority_grounding('ce7d7703-4727-4bac-bcb1-08ebc17af542', lineage).
narrative_ontology:cs_interpretation_layer_present('ce7d7703-4727-4bac-bcb1-08ebc17af542').
narrative_ontology:cs_reading_relation('ce7d7703-4727-4bac-bcb1-08ebc17af542', marriage_commitment_reversal__exogenous_override_reading, coexists_with).
narrative_ontology:cs_reading_relation('ce7d7703-4727-4bac-bcb1-08ebc17af542', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('ce7d7703-4727-4bac-bcb1-08ebc17af542', foundational, prophetic_revelation_is_supreme_interpretive_authority).
narrative_ontology:cs_axiom_status(prophetic_revelation_is_supreme_interpretive_authority, holdable).
narrative_ontology:cs_axiom_grounding('ce7d7703-4727-4bac-bcb1-08ebc17af542', prophetic_revelation_is_supreme_interpretive_authority, theological).
narrative_ontology:cs_axiom('ce7d7703-4727-4bac-bcb1-08ebc17af542', foundational, divine_will_adapts_to_circumstance).
narrative_ontology:cs_axiom_status(divine_will_adapts_to_circumstance, holdable).
narrative_ontology:cs_axiom_grounding('ce7d7703-4727-4bac-bcb1-08ebc17af542', divine_will_adapts_to_circumstance, theological).
narrative_ontology:cs_reference_frame('ce7d7703-4727-4bac-bcb1-08ebc17af542', prophetic_revelation_as_adaptive_divine_guidance).
narrative_ontology:cs_drift_state('ce7d7703-4727-4bac-bcb1-08ebc17af542', contemporary_secular_pluralism, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ce7d7703-4727-4bac-bcb1-08ebc17af542', '').
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

% The prophet and senior leadership who received and promulgated the revelation. They maintain interpretive authority and institutional legitimacy by framing the change as divine will, navigating external pressures while preserving internal cohesion.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, institutional_leadership, agenda_setter,
    institutional, generational, constrained, global).

% The internal coherence and immutability of divine doctrine. It 'pays' by being reinterpreted or appearing to shift, creating a challenge for future doctrinal stability and requiring apologetic efforts to reconcile past and present 'will of God'.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(marriage_commitment_reversal__endogenous_reinterpretation_reading, theological_consistency).

% Members who had deeply internalized and committed to the prior practice as divine commandment. They bear the cost of cognitive dissonance, social pressure to conform, and potential loss of community if they resist the new interpretation. Their identity is often fused with the church's teachings.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, members_committed_to_prior_practice, payer,
    powerless, biographical, identity_locked, local).

% Members who benefit from the church's increased social acceptance and reduced persecution by conforming to broader societal norms. They experience relief from external pressure and improved integration into the wider community.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, church_members_seeking_social_acceptance, beneficiary,
    moderate, biographical, mobile, local).

% The governmental bodies whose legal and political pressure created the circumstances for the 'revelation'. They observe the church's compliance with secular law, and their continued pressure ensures the new practice is maintained.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__endogenous_reinterpretation_reading, external_federal_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the church's practice with external legal and social demands, allowing the institution to survive and its members to integrate into wider society, while maintaining a narrative of divine guidance for internal cohesion.
% TRANSFER_FUNCTION: Transfers the burden of doctrinal inconsistency and cognitive dissonance from the institutional leadership to the theological framework and individual members, in exchange for institutional survival and reduced external persecution.
% ABSENT_VOICES: Those who left the church or were excommunicated for refusing to abandon the prior practice, or those who questioned the authenticity or timing of the 'revelation'. They would argue the change was a capitulation, not a divine command.
% DISAPPEARANCE_RATIONALE: If this reinterpretation and its enforcement vanished, the church would face renewed legal and social conflict, potentially leading to its dissolution or a schism. The institutional structure and its relationship with the state would fundamentally reorganize.
% FOUNDING_PROBLEM: The church faced existential legal and social threats due to its prior practice, with federal authorities threatening disincorporation, property seizure, and imprisonment of leaders.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, federal court documents, and contemporary journalistic accounts from outside the church's leadership corroborate the severe external pressure. While the church leadership frames the 'revelation' as purely divine, external sources confirm the direct causal link to federal threats, indicating the founding problem (existential threat) is resolved by the change in practice, not by a change in external circumstances.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__endogenous_reinterpretation_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__endogenous_reinterpretation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__endogenous_reinterpretation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is moderate (0.45) as it extracts theological consistency and imposes cognitive costs on members, but also provides the benefit of institutional survival. Suppression is high (0.6) due to the strong social and spiritual pressure to accept the prophet's revelation as binding. Theater ratio is moderate (0.4) as the 'revelation' serves a performative function of maintaining divine authority while adapting to secular demands. The claimed type is Tangled Rope because it coordinates institutional survival with external society while extracting costs from internal doctrinal consistency and dissenting members, requiring active enforcement of the new interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional leadership's perspective, this was a necessary and divinely guided adaptation, preserving the church. From the perspective of members deeply committed to the prior practice, it was a painful reversal, potentially a betrayal of core tenets, or a test of faith. The engine's per-seat classification will reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   Institutional leadership benefits directly from the constraint's operation, as it preserves their authority and the church's existence. Theological consistency and members committed to prior practice are victims, bearing the costs of reinterpretation and conformity. External federal authorities are observers whose pressure drives the change but do not directly benefit from the internal reinterpretation mechanism itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to ensure the survival of the church in the face of existential threats. While the immediate threat (federal persecution) is resolved by the change in practice, the mechanism of 'endogenous reinterpretation' continues to serve the function of maintaining institutional adaptability and prophetic authority. The 'dead' status of the founding problem, combined with the 'world_rearranges' disappearance verdict, indicates a potential for zombie-like persistence where the original problem is gone but the structure continues to operate, now serving the agenda of maintaining leadership's interpretive power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_will_vs_institutional_necessity,
    'To what extent was the ''revelation'' a genuine divine communication, versus an institutional necessity framed as such to preserve authority?',
    'Analysis of internal church documents, private correspondence of leaders, and comparison with similar historical instances of ''revelation'' coinciding with external pressure. This is a conceptual omega, as ''divine will'' is not empirically testable.',
    'If primarily institutional necessity, the extractiveness from theological consistency is higher, and the theater_ratio increases, reclassifying towards a Snare or a more extractive Tangled Rope. If genuine divine will, the constraint''s legitimacy is higher, and extractiveness is lower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_will_vs_institutional_necessity, conceptual, 'Ambiguity between divine command and institutional adaptation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (fear of excommunication, social ostracism) or internalized (deeply held belief in prophetic infallibility, identity fusion with church teachings)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-censorship, guilt) after leaving the church, reclassify as partially internalized. If it dissipates, it was primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making exit less effective. This would push the classification towards a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for dissenting members.').

omega_variable(
    long_term_doctrinal_stability,
    'What are the long-term effects of this reinterpretation on the church''s doctrinal stability and its ability to claim immutable divine guidance?',
    'Longitudinal study of subsequent doctrinal changes, internal debates, and schisms within the church over several generations. Analysis of apologetic literature''s evolution.',
    'If it leads to increased doctrinal fluidity or internal challenges to prophetic authority, the initial extraction from ''theological_consistency'' is amplified over time, potentially leading to a Piton if the original mandate of ''immutable divine guidance'' atrophies into mere performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(long_term_doctrinal_stability, empirical, 'Impact on future doctrinal stability and claims of immutable divine guidance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__endogenous_reinterpretation_reading, 1890, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1890, 0.5).
narrative_ontology:measurement_basis(marr_tr_t1890, observed).
narrative_ontology:measurement(marr_tr_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1895, 0.45).
narrative_ontology:measurement_basis(marr_tr_t1895, observed).
narrative_ontology:measurement(marr_tr_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1900, 0.4).
narrative_ontology:measurement_basis(marr_tr_t1900, observed).
narrative_ontology:measurement(marr_tr_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1905, 0.35).
narrative_ontology:measurement_basis(marr_tr_t1905, observed).
narrative_ontology:measurement(marr_tr_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1910, 0.38).
narrative_ontology:measurement_basis(marr_tr_t1910, observed).
narrative_ontology:measurement(marr_tr_t1915, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1915, 0.4).
narrative_ontology:measurement_basis(marr_tr_t1915, observed).
narrative_ontology:measurement(marr_tr_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, theater_ratio, 1920, 0.4).
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
narrative_ontology:measurement(marr_be_t1915, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1915, 0.43).
narrative_ontology:measurement_basis(marr_be_t1915, observed).
narrative_ontology:measurement(marr_be_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, base_extractiveness, 1920, 0.45).
narrative_ontology:measurement_basis(marr_be_t1920, observed).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1890, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1890, 0.7).
narrative_ontology:measurement_basis(marr_su_t1890, observed).
narrative_ontology:measurement(marr_su_t1895, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1895, 0.65).
narrative_ontology:measurement_basis(marr_su_t1895, observed).
narrative_ontology:measurement(marr_su_t1900, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement_basis(marr_su_t1900, observed).
narrative_ontology:measurement(marr_su_t1905, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1905, 0.55).
narrative_ontology:measurement_basis(marr_su_t1905, observed).
narrative_ontology:measurement(marr_su_t1910, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1910, 0.5).
narrative_ontology:measurement_basis(marr_su_t1910, observed).
narrative_ontology:measurement(marr_su_t1915, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1915, 0.55).
narrative_ontology:measurement_basis(marr_su_t1915, observed).
narrative_ontology:measurement(marr_su_t1920, marriage_commitment_reversal__endogenous_reinterpretation_reading, suppression_requirement, 1920, 0.6).
narrative_ontology:measurement_basis(marr_su_t1920, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_commitment_reversal__endogenous_reinterpretation_reading, identity_coordination).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, exogenous_override_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__endogenous_reinterpretation_reading, practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'marriage_commitment_reversal' kernel. This reading emphasizes internal reinterpretation via revelation, while 'exogenous_override_reading' focuses on external coercion, and 'practice_doctrine_gap' highlights the enduring tension between doctrine and practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
