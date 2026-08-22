% ============================================================================
% CONSTRAINT STORY: marriage_commitment_reversal__exogenous_override_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: marriage_commitment_reversal__exogenous_override_reading
 *   human_readable: Federal Coercive Override of LDS Plural Marriage Practice
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   In 1890, the LDS church issued the Manifesto suspending plural marriage
 *   under severe federal coercion (the Edmunds-Tucker Act's threat of
 *   disincorporation and property seizure). This reading treats the reversal
 *   as externally coerced: the federal government extracted institutional
 *   autonomy from the church, which never renounced Section 132 of the
 *   Doctrine and Covenants. The doctrine-practice gap persists as a
 *   structural feature. The constraint is the standing coercive arrangement
 *   that forces public compliance while preserving the underlying doctrinal
 *   principle.
 *
 * KEY AGENTS:
 *   - federal_government (agenda_setter/beneficiary, institutional/mobile) â enforces the constraint and captures territorial integration benefits
 *   - lds_church (payer, institutional/identity_locked) â bears the loss of autonomy and doctrinal dissonance
 *   - practicing_members (payer, powerless/constrained) â bear prosecution and family disruption
 *   - territorial_elites (beneficiary, moderate/mobile) â gain from the opening of political and economic space
 *   - historical_analyst (observer, analytical/analytical) â documents the doctrine-practice gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_commitment_reversal__exogenous_override_reading, 0.85).
domain_priors:suppression_score(marriage_commitment_reversal__exogenous_override_reading, 0.88).
domain_priors:theater_ratio(marriage_commitment_reversal__exogenous_override_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, accessibility_collapse, 0.82).
narrative_ontology:constraint_metric(marriage_commitment_reversal__exogenous_override_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_commitment_reversal__exogenous_override_reading, snare).
narrative_ontology:human_readable(marriage_commitment_reversal__exogenous_override_reading, "Federal Coercive Override of LDS Plural Marriage Practice").
narrative_ontology:topic_domain(marriage_commitment_reversal__exogenous_override_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(marriage_commitment_reversal__exogenous_override_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_commitment_reversal__exogenous_override_reading, '7b772749-046a-4443-bb1f-3a2613962dd8').
narrative_ontology:cs_kernel_codification('7b772749-046a-4443-bb1f-3a2613962dd8', fixed_text).
narrative_ontology:cs_authority_grounding('7b772749-046a-4443-bb1f-3a2613962dd8', lineage).
narrative_ontology:cs_interpretation_layer_present('7b772749-046a-4443-bb1f-3a2613962dd8').
narrative_ontology:cs_reading_relation('7b772749-046a-4443-bb1f-3a2613962dd8', marriage_commitment_reversal__endogenous_reinterpretation_reading, influences).
narrative_ontology:cs_reading_relation('7b772749-046a-4443-bb1f-3a2613962dd8', marriage_commitment_reversal__practice_doctrine_gap, influences).
narrative_ontology:cs_axiom('7b772749-046a-4443-bb1f-3a2613962dd8', foundational, federal_coercion_primary_cause).
narrative_ontology:cs_axiom_status(federal_coercion_primary_cause, holdable).
narrative_ontology:cs_axiom_grounding('7b772749-046a-4443-bb1f-3a2613962dd8', federal_coercion_primary_cause, empirically_contingent).
narrative_ontology:cs_axiom('7b772749-046a-4443-bb1f-3a2613962dd8', foundational, state_supremacy_over_religious_practice).
narrative_ontology:cs_axiom_status(state_supremacy_over_religious_practice, holdable).
narrative_ontology:cs_axiom_grounding('7b772749-046a-4443-bb1f-3a2613962dd8', state_supremacy_over_religious_practice, conventional).
narrative_ontology:cs_reference_frame('7b772749-046a-4443-bb1f-3a2613962dd8', prophetic_plural_marriage_mandate).
narrative_ontology:cs_drift_state('7b772749-046a-4443-bb1f-3a2613962dd8', post_manifesto_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('7b772749-046a-4443-bb1f-3a2613962dd8', '').
narrative_ontology:cs_kernel_id(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:constraint_beneficiary(marriage_commitment_reversal__exogenous_override_reading, territorial_elites).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, lds_church).
narrative_ontology:constraint_victim(marriage_commitment_reversal__exogenous_override_reading, practicing_members).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, federal_supremacy_doctrine).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, territorial_integration_imperative).
narrative_ontology:constraint_vindicates(marriage_commitment_reversal__exogenous_override_reading, anti_polygamy_moral_framework).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enacts and enforces anti-polygamy statutes (Edmunds Act 1882, Edmunds-Tucker Act 1887) tying Utah statehood to compliance; threatens disincorporation of the church, seizure of temples, and imprisonment of leaders; captures the extraction as expanded federal supremacy over territorial religious institutions.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, federal_government, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_commitment_reversal__exogenous_override_reading, federal_government, beneficiary).

% Forced to issue the 1890 Manifesto suspending plural marriage while preserving D&C 132 as canonized revelation; bears the cost of doctrinal dissonance, internal schism, and the eventual excommunication of post-Manifesto practitioners; cannot renounce Section 132 without unraveling prophetic authority claims, so exit is locked to its theological identity.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, lds_church, payer,
    institutional, generational, identity_locked, national).

% Face criminal prosecution, disenfranchisement, and family dissolution if they continue plural marriage; constrained by religious obligation and legal threat; bear the direct lived cost of the coercion.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, practicing_members, payer,
    powerless, biographical, constrained, regional).

% Benefit from the collapse of LDS political monopoly and the opening of territorial offices, land, and commercial opportunity previously dominated by church networks.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, territorial_elites, beneficiary,
    moderate, biographical, mobile, regional).

% Observes the structural asymmetry between coerced practice and preserved doctrine, documenting the persistence of the doctrine-practice gap.
narrative_ontology:constraint_stakeholder(marriage_commitment_reversal__exogenous_override_reading, historical_analyst, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_commitment_reversal__exogenous_override_reading, federal_government).
narrative_ontology:fixing_cost_class(marriage_commitment_reversal__exogenous_override_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The arrangement coordinates the integration of a territorially anomalous religious polity into the United States under a uniform marriage and property law regime.
% TRANSFER_FUNCTION: Moves institutional autonomy and public compliance from the LDS church and its members to the federal government and non-Mormon territorial elites, via the suspension of plural marriage practice.
% ABSENT_VOICES: Post-Manifesto plural families who continued practice covertly and were excommunicated or prosecuted; they would object that the constraint extracts their family form but are silenced by criminalization and church discipline. Indigenous nations with divergent marriage traditions, also subjected to federal standardization, are absent from the discourse.
% DISAPPEARANCE_RATIONALE: If the federal coercive apparatus vanished overnight in 1890, the LDS church would likely have resumed public plural marriage, the Edmunds-Tucker receivership would end, and the territorial political monopoly would reconstitute; the national integration project would stall.
% FOUNDING_PROBLEM: How to integrate a polygamous, theocratic territory into the United States under a uniform legal framework acceptable to national political majorities.
% FOUNDING_PROBLEM_CORROBORATION: Federal policymakers of the 1880s-90s (congressional sponsors of the Edmunds-Tucker Act) and subsequent territorial historians corroborate the state-integration motive. LDS leadership frames the founding problem as religious persecution, which is a beneficiary-side narrative; independent academic historiography corroborates the integration problem.
narrative_ontology:disappearance_verdict(marriage_commitment_reversal__exogenous_override_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_commitment_reversal__exogenous_override_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_commitment_reversal__exogenous_override_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_commitment_reversal__exogenous_override_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_commitment_reversal__exogenous_override_reading, 0.85, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.85) is high because the constraint strips the LDS church of its governing autonomy over marriage practice without compensation. Suppression (0.88) is higher because the constraint depends on the active threat of criminal prosecution, property seizure, and disincorporation; without this enforcement, public compliance would collapse. Theater ratio (0.55) reflects the high performative content of the Manifesto and subsequent assurances, which maintained the appearance of voluntary compliance while the underlying cause was federal duress. Accessibility collapse (0.82) is high because alternatives (territorial theocracy, legal plural marriage, federal non-interference) were foreclosed by federal statute and military capacity. Resistance (0.68) reflects the church's initial refusal, the underground continuation of plural marriage, and the ultimate capitulation only under existential threat.
 *
 * PERSPECTIVAL GAP:
 *   The federal government seat experiences the constraint as legitimate law enforcement and territorial integration; the LDS church and member seats experience it as existential coercion and extraction of religious autonomy. The engine computes this divergence from the structural data: the agenda-setter is a beneficiary with mobile exit, while the payers are identity-locked or constrained.
 *
 * DIRECTIONALITY LOGIC:
 *   The federal government is the structural beneficiary (d near 0.0) because it collects expanded territorial authority and national integration. The LDS church is the structural target (d near 1.0) because it bears the cost of subordinated sovereignty and doctrinal fracture; its identity_locked exit amplifies effective extraction. Practicing members are also targets (d near 1.0) due to their powerlessness and constrained exit. Territorial elites are minor beneficiaries (low d) via opened political space.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint prevents mislabeling because its founding problem (territorial integration) is dead by 1896, yet the arrangement persisted in the form of federal supremacy and the doctrine-practice gap. A rope or scaffold reading would require the coordination to be symmetrical or transitional; here the coordination (uniform marriage law) is cover for the extraction of autonomy. The R5 genealogy interview flags the mismatch between a dead founding problem and a world_rearranges disappearance verdict, resisting classification as benign coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_practice_gap_persistence,
    'Does the continued canonization of D&C 132 constitute latent resistance or institutional double-bookkeeping?',
    'Analyze post-1904 LDS leadership discourse and temple sealing practices for covert operational continuity.',
    'If doctrine remains operationally alive, the constraint is a snare enforcing shell compliance; if truly inert, the residual structure may be a piton.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap_persistence, empirical, 'Whether the preserved doctrine is latent structure or dead letter.').

omega_variable(
    federal_motive_coordination_vs_extraction,
    'Was federal anti-polygamy enforcement primarily motivated by the genuine coordination problem of integrating a uniform marriage standard, or by the extraction of territorial political control?',
    'Historical analysis of congressional debates, lobbying records, and territorial correspondence.',
    'If extraction was primary, snare classification holds; if coordination was primary, reclassification to tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_motive_coordination_vs_extraction, conceptual, 'Whether the federal constraint is coordination or extraction dominant.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (federal military and legal capacity) or internalized (LDS leadership''s adoption of American legitimacy frameworks)?',
    'Compare resistance levels before and after the Manifesto; if post-Manifesto compliance continues without external enforcement, suppression is partially internalized.',
    'Internalized suppression raises effective extraction beyond the structural measure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    kernel_reading_contest,
    'Is the exogenous override reading a historically accurate description of the constraint, or a polemical framing contested by the endogenous reinterpretation reading?',
    'Archival and historiographic assessment of federal coercion intensity versus internal church records of the 1890 Manifesto.',
    'If the endogenous reading is vindicated, this constraint story describes a phantom or misattributed structure; if exogenous, the snare classification is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contested status of this reading within the kernel.').

omega_variable(
    practice_doctrine_gap_nature,
    'Does the practice-doctrine gap represent a stable structural feature of the kernel, or a temporary transitional scaffold toward full doctrinal revision?',
    'Track post-1904 doctrinal amendments and public teachings on Section 132.',
    'If a full revision eventually occurs, the gap was a scaffold; if it persists indefinitely, it is a permanent feature of this reading''s constraint structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(practice_doctrine_gap_nature, conceptual, 'Whether the gap is permanent or transitional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_commitment_reversal__exogenous_override_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(marr_tr_t5, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 5, 0.4).
narrative_ontology:measurement(marr_tr_t10, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 10, 0.6).
narrative_ontology:measurement(marr_tr_t15, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 15, 0.65).
narrative_ontology:measurement(marr_tr_t20, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(marr_tr_t25, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 25, 0.5).
narrative_ontology:measurement(marr_tr_t30, marriage_commitment_reversal__exogenous_override_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marr_be_t5, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(marr_be_t10, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 10, 0.8).
narrative_ontology:measurement(marr_be_t15, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 15, 0.85).
narrative_ontology:measurement(marr_be_t20, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 20, 0.82).
narrative_ontology:measurement(marr_be_t25, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 25, 0.78).
narrative_ontology:measurement(marr_be_t30, marriage_commitment_reversal__exogenous_override_reading, base_extractiveness, 30, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(marr_su_t5, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 5, 0.8).
narrative_ontology:measurement(marr_su_t10, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 10, 0.9).
narrative_ontology:measurement(marr_su_t15, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 15, 0.88).
narrative_ontology:measurement(marr_su_t20, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(marr_su_t25, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(marr_su_t30, marriage_commitment_reversal__exogenous_override_reading, suppression_requirement, 30, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(marriage_commitment_reversal__exogenous_override_reading, marriage_commitment_reversal__practice_doctrine_gap).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the marriage_commitment_reversal kernel. The exogenous_override_reading decomposes from the other two by locating causal force in federal coercion rather than internal revelation or abstract structural gap.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
