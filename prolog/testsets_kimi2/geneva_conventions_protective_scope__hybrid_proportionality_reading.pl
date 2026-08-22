% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Protections Scaled by Conflict Type with Proportionality Calibration (Hybrid Reading)
 *   domain: legal/international_humanitarian_law
 *
 * SUMMARY:
 *   This constraint instantiates the hybrid proportionality reading of the
 *   Geneva Conventions protective scope kernel. Under this reading, Geneva
 *   protections scale by conflict type â AP I standards for international
 *   armed conflict, AP II and Common Article 3 for non-international armed
 *   conflict â with proportionality analysis serving as the primary
 *   calibrator of application. The constraint coordinates behavior in armed
 *   conflict but extracts from weaker parties through legal ambiguity in
 *   classification and proportionality. The major military powers with
 *   advanced legal capacity act as both agenda-setters and beneficiaries,
 *   while non-state armed groups and civilians in contested zones bear the
 *   costs of interpretive uncertainty. This is one reading of a contested
 *   kernel; sibling readings include the state-centric reading (status
 *   determines scope) and the universal rights reading (human rights floor
 *   regardless of conflict type).
 *
 * KEY AGENTS:
 *   - States with advanced military legal capacity: Primary agenda-setter and beneficiary (institutional/global/arbitrage) â controls proportionality analysis and conflict classification.
 *   - Non-state armed groups: Primary payer (organized/regional/constrained) â subject to targeting and detention determinations they cannot influence.
 *   - Civilians in contested zones: Secondary payer (powerless/local/trapped) â protection level fluctuates based on external legal interpretation.
 *   - Human rights advocates: Excluded voice (organized/global/constrained) â argues for universal floor but excluded from IHL interpretive processes.
 *   - International courts: Analytical observer (institutional/global/analytical) â adjudicates ex post but cannot compel broader interpretations.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.62).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Protections Scaled by Conflict Type with Proportionality Calibration (Hybrid Reading)").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "legal/international_humanitarian_law").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '2daa783f-e0cb-4b44-b1eb-4ccd84e908c7').
narrative_ontology:cs_kernel_codification('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', formalized).
narrative_ontology:cs_authority_grounding('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', lineage).
narrative_ontology:cs_interpretation_layer_present('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7').
narrative_ontology:cs_reading_relation('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', geneva_conventions_protective_scope__state_centric_reading, influences).
narrative_ontology:cs_reading_relation('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', foundational, proportionality_as_primary_calibrator).
narrative_ontology:cs_axiom_status(proportionality_as_primary_calibrator, holdable).
narrative_ontology:cs_axiom_grounding('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', proportionality_as_primary_calibrator, conventional).
narrative_ontology:cs_axiom('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', foundational, conflict_classification_maintains_validity).
narrative_ontology:cs_axiom_status(conflict_classification_maintains_validity, holdable).
narrative_ontology:cs_axiom_grounding('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', conflict_classification_maintains_validity, conventional).
narrative_ontology:cs_reference_frame('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', treaty_based_graduated_protection).
narrative_ontology:cs_drift_state('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', contemporary_asymmetric_warfare_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2daa783f-e0cb-4b44-b1eb-4ccd84e908c7', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, states_with_advanced_military_legal_capacity).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_contested_zones).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Draft military manuals, control proportionality analysis, and determine conflict classification in operational legal review. Benefit from interpretive ambiguity in treaty text to maintain operational flexibility while claiming Geneva compliance.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, states_with_advanced_military_legal_capacity, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, states_with_advanced_military_legal_capacity, beneficiary).

% Operate in armed conflicts where their legal status as protected or unprivileged belligerents depends on proportionality and classification analyses they cannot influence. Lack legal capacity to shape the interpretive frameworks that determine targeting and detention rules applied to them.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    organized, immediate, constrained, regional).

% Reside in territories where the application of Geneva protections fluctuates based on conflict classification and proportionality calculations performed by military legal advisors. Their protected person status is determined by interpretations they do not control.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_contested_zones, payer,
    powerless, immediate, trapped, local).

% Argue for universal human rights floors regardless of conflict classification, but are structurally excluded from the IHL treaty interpretation and military legal review processes that determine protective scope in operational practice.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, human_rights_advocates, excluded,
    organized, generational, constrained, global).

% Adjudicate alleged violations of proportionality and conflict classification ex post through international criminal and humanitarian law jurisprudence, but cannot compel states to accept broader protective interpretations than their military legal frameworks permit.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_courts, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(geneva_conventions_protective_scope__hybrid_proportionality_reading, states_with_advanced_military_legal_capacity).
narrative_ontology:fixing_cost_class(geneva_conventions_protective_scope__hybrid_proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates reciprocal restraint among parties to armed conflict by establishing graduated humanitarian obligations calibrated to conflict type and military necessity through proportionality analysis.
% TRANSFER_FUNCTION: Transfers interpretive authority over protective scope from potential protected persons to the party conducting proportionality and conflict classification analysis; transfers risk of non-protection to non-state actors and civilians in ambiguously classified conflicts.
% ABSENT_VOICES: Non-state actors without legal capacity, universal rights advocates arguing for context-independent protections, and civilian populations in contested classification zones are structurally underrepresented in treaty interpretation and military legal review.
% DISAPPEARANCE_RATIONALE: If the graduated Geneva framework vanished overnight, armed conflict regulation would reorganize around either a universal rights floor, pure state-centric status determinations, or ad hoc humanitarian practice â the current architecture is a specific legal settlement that parties have organized around.
% FOUNDING_PROBLEM: The need to secure state consent to humanitarian limits on warfare while providing minimum protections in conflicts not meeting traditional interstate war criteria.
% FOUNDING_PROBLEM_CORROBORATION: Historical treaty records and ICRC archives attest to the original state-consent coordination problem. Contemporary human rights scholars and conflict studies researchers from outside the major military powers attest that the consent problem is now outweighed by protection gaps in asymmetric and non-international conflicts; corroboration exists from excluded seats.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.68, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.68) reflects that proportionality analysis and conflict classification are not merely technical but distribute protective status asymmetrically: states with legal capacity can argue narrow applications, while non-state actors lack capacity to argue broad ones. Suppression (0.62) captures how alternatives (universal human rights application, pure state-centric formalism) are marginalized within the operational legal framework. Theater ratio (0.48) acknowledges the substantial performative dimension of proportionality discourse â legal briefs, targeting decisions, and military manual drafting that performs compliance while preserving latitude. Accessibility collapse (0.58) indicates that once the Geneva framework is accepted, alternatives like pure human rights law remain visible but are structurally subordinated in armed conflict contexts. Resistance (0.52) reflects ongoing contestation from human rights advocates and some states. The temporal series show extraction and theater increasing as asymmetric warfare and proportionality jurisprudence have matured since 1977.
 *
 * PERSPECTIVAL GAP:
 *   The state party seat experiences this constraint as a genuine coordination achievement â a carefully negotiated legal architecture that preserves operational capacity while reducing suffering. The non-state armed group and civilian seats experience the same structure as an interpretive lottery where their protection depends on the legal arguments of their adversary. The engine computes this divergence from the structural asymmetry in power, exit options, and beneficiary/victim declarations.
 *
 * DIRECTIONALITY LOGIC:
 *   States with advanced military legal capacity are declared beneficiaries and agenda-setters; their structural relationship to the constraint is subsidizing (low directionality, negative effective extraction) because the framework grants them interpretive control and operational latitude. Non-state armed groups and civilians are declared victims/payers; their directionality sits near the full-target end because the constraint extracts protective certainty from them and deposits interpretive authority with states. Human rights advocates are excluded rather than coordinated â their absence from the interpretive table is what allows the extraction to persist without contestation in operational moments.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling by requiring both coordination function and asymmetric extraction. The constraint genuinely coordinates â it is not a pure snare because the laws of war do reduce suffering in interstate conflicts. But the asymmetric extraction is real: the same proportionality analysis that protects in one context justifies narrowed obligations in another. Mandatrophy would occur if the coordination function died (state consent collapsed) but the interpretive machinery persisted; currently the founding problem is contested but not dead, so mandatrophy is not declared resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_sibling_boundary,
    'Does this hybrid proportionality reading foreclose the universal rights reading or merely coexist with it as a parallel legal doctrine?',
    'Analysis of state practice and judicial reasoning: if hybrid proportionality is used to actively resist human rights supplementation, it exerts structural influence; if courts treat them as complementary, they coexist.',
    'If hybrid proportionality structurally forecloses universal rights, effective extraction is higher than measured; if merely coexistent, extraction is bounded by human rights law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_sibling_boundary, conceptual, 'Structural relationship between hybrid and universal rights readings').

omega_variable(
    proportionality_as_extraction,
    'Does proportionality analysis function as a genuine protective calibration or as a discursive mechanism enabling stronger parties to justify operational latitude?',
    'Systematic review of military legal advice, targeting decisions, and judicial outcomes: compare proportionality claims against actual protection levels across symmetric versus asymmetric conflicts.',
    'If proportionality is primarily justificatory, extractiveness approaches the upper bound; if genuinely protective, the coordination function is stronger than the asymmetric extraction measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_as_extraction, empirical, 'Whether proportionality is protective or justificatory').

omega_variable(
    conflict_classification_naturalness,
    'Is the distinction between international and non-international armed conflict a natural feature of warfare or a constructed legal boundary that serves state interests?',
    'Historical sociology of armed conflict: compare pre-1949 legal distinctions against post-1949 conflict patterns to determine whether the IAC/NIAC binary tracks empirical differences or state sovereignty concerns.',
    'If constructed, the constraint is more extractive than its natural-law framing suggests; if natural, the graduated protection is structurally justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_naturalness, conceptual, 'Naturalness of the IAC/NIAC legal boundary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gene_tr_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(gene_tr_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(gene_tr_t30, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(gene_tr_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 40, 0.46).
narrative_ontology:measurement(gene_tr_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 50, 0.5).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(gene_be_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(gene_be_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(gene_be_t30, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(gene_be_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement(gene_be_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 50, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(gene_su_t10, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 10, 0.52).
narrative_ontology:measurement(gene_su_t20, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(gene_su_t30, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(gene_su_t40, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(gene_su_t50, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 50, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__state_centric_reading).
narrative_ontology:affects_constraint(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the geneva_conventions_protective_scope kernel. The kernel decomposes into three structurally distinct constraints because the natural-language label 'Geneva protections' conflates status-based, proportionality-based, and universal-rights-based claims with different epsilon values, beneficiary structures, and failure modes. Each reading has its own constraint story linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
