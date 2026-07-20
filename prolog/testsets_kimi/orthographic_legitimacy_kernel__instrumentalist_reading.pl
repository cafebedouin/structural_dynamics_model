% ============================================================================
% CONSTRAINT STORY: orthographic_legitimacy_kernel__instrumentalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_orthographic_legitimacy_kernel__instrumentalist_reading, []).

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
 *   constraint_id: orthographic_legitimacy_kernel__instrumentalist_reading
 *   human_readable: Instrumentalist Reading of Orthographic Legitimacy
 *   domain: political/linguistic/state_formation
 *
 * SUMMARY:
 *   The instrumentalist reading of orthographic legitimacy treats script
 *   choice as a pragmatic administrative tool justified by measurable
 *   outcomes: literacy rates and bureaucratic throughput. Emerging in
 *   state-formation contexts such as the Turkish alphabet reform and
 *   Indonesian romanization, it frames the abandonment of Arabic script not
 *   as civilizational rupture but as technical optimization. The newly
 *   literate population gains access to state services and modern media; the
 *   Arabic-literate elite sees its accumulated cultural capital devalued. The
 *   constraint coordinates mass literacy while asymmetrically extracting from
 *   the old scribal-religious class, requiring active state enforcement in
 *   education and administration.
 *
 * KEY AGENTS:
 *   - State language commission: agenda_setter, institutional power â designs and enforces the reform
 *   - Newly literate population: beneficiary, moderate power â gains literacy under the new script
 *   - Arabic-literate elite: payer/victim, powerful â bears devaluation of script-specific human capital
 *   - Traditional religious scholars: excluded, organized â marginalized by the instrumentalist framing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42).
domain_priors:suppression_score(orthographic_legitimacy_kernel__instrumentalist_reading, 0.55).
domain_priors:theater_ratio(orthographic_legitimacy_kernel__instrumentalist_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(orthographic_legitimacy_kernel__instrumentalist_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(orthographic_legitimacy_kernel__instrumentalist_reading, tangled_rope).
narrative_ontology:human_readable(orthographic_legitimacy_kernel__instrumentalist_reading, "Instrumentalist Reading of Orthographic Legitimacy").
narrative_ontology:topic_domain(orthographic_legitimacy_kernel__instrumentalist_reading, "political/linguistic/state_formation").

domain_priors:requires_active_enforcement(orthographic_legitimacy_kernel__instrumentalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(orthographic_legitimacy_kernel__instrumentalist_reading, '73dc359f-04c1-4932-840c-4fe9c6bed7c0').
narrative_ontology:cs_kernel_codification('73dc359f-04c1-4932-840c-4fe9c6bed7c0', formalized).
narrative_ontology:cs_authority_grounding('73dc359f-04c1-4932-840c-4fe9c6bed7c0', expertise).
narrative_ontology:cs_interpretation_layer_present('73dc359f-04c1-4932-840c-4fe9c6bed7c0').
narrative_ontology:cs_reading_relation('73dc359f-04c1-4932-840c-4fe9c6bed7c0', orthographic_legitimacy_kernel__continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('73dc359f-04c1-4932-840c-4fe9c6bed7c0', orthographic_legitimacy_kernel__modernist_reading, coexists_with).
narrative_ontology:cs_axiom('73dc359f-04c1-4932-840c-4fe9c6bed7c0', foundational, script_choice_instrumentally_justified).
narrative_ontology:cs_axiom_status(script_choice_instrumentally_justified, holdable).
narrative_ontology:cs_axiom_grounding('73dc359f-04c1-4932-840c-4fe9c6bed7c0', script_choice_instrumentally_justified, empirically_contingent).
narrative_ontology:cs_axiom('73dc359f-04c1-4932-840c-4fe9c6bed7c0', foundational, legitimacy_from_measurable_outcomes).
narrative_ontology:cs_axiom_status(legitimacy_from_measurable_outcomes, holdable).
narrative_ontology:cs_axiom_grounding('73dc359f-04c1-4932-840c-4fe9c6bed7c0', legitimacy_from_measurable_outcomes, instrumental).
narrative_ontology:cs_reference_frame('73dc359f-04c1-4932-840c-4fe9c6bed7c0', pragmatic_administrative_optimality).
narrative_ontology:cs_drift_state('73dc359f-04c1-4932-840c-4fe9c6bed7c0', post_reform_assessment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('73dc359f-04c1-4932-840c-4fe9c6bed7c0', '').
narrative_ontology:cs_kernel_id(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population).
narrative_ontology:constraint_victim(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and enforces script reform policy, selects the new phonetic orthography, mandates its use in state schools and official documentation, and collects literacy statistics to justify the reform's continuation.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, state_language_commission, agenda_setter,
    institutional, generational, analytical, national).

% Gains literacy through the new phonetic script and the expanded educational infrastructure that accompanies reform; accesses state services, newspapers, and modern literature in the new script. Cannot opt out of the script in formal education or bureaucracy.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, newly_literate_population, beneficiary,
    moderate, biographical, constrained, national).

% Previously held gatekeeper status through mastery of Arabic script in religious, legal, and bureaucratic domains; experiences steep devaluation of accumulated cultural capital as the state shifts official prestige, education, and employment to the new script. Retraining is possible but does not restore prior scarcity premium.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, arabic_literate_elite, payer,
    powerful, biographical, constrained, national).

% Would argue that legitimacy derives from continuity with sacred and historical textual traditions in Arabic script; their voice is structurally marginalized by the instrumentalist framing that treats script choice as a technical rather than theological or civilizational matter.
narrative_ontology:constraint_stakeholder(orthographic_legitimacy_kernel__instrumentalist_reading, traditional_religious_scholars, excluded,
    organized, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(orthographic_legitimacy_kernel__instrumentalist_reading, diffuse).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a standardized, phonetic writing system that lowers barriers to mass literacy and streamlines bureaucratic communication between state institutions and citizens.
% TRANSFER_FUNCTION: Moves cultural capital and institutional access from the Arabic-literate elite to the broader population, and moves administrative legibility from a scribal intermediary class to the central state.
% ABSENT_VOICES: Traditional religious scholars and the Arabic-literate elite who would argue for continuity-based legitimacy are structurally excluded from policy design; their objections are preemptively framed as resistance to progress rather than as valid competing claims.
% DISAPPEARANCE_RATIONALE: If the instrumentalist legitimacy claim vanished, the state would lose its primary justification for maintaining the new script against continuity or modernist alternatives; educational curricula would require re-legitimation, and the old elite's cultural capital might be partially revalued.
% FOUNDING_PROBLEM: Low literacy rates and inefficient state-citizen communication under a non-phonetic script that created a bottleneck between the administrative state and the general population.
% FOUNDING_PROBLEM_CORROBORATION: Educational statisticians and development economists outside the immediate beneficiary population attest to post-reform literacy gains; traditional scholars and comparative historians contest that the gains were caused by script change itself rather than by compulsory schooling expansion, corroborating the contested status from outside the beneficiary set.
narrative_ontology:disappearance_verdict(orthographic_legitimacy_kernel__instrumentalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(orthographic_legitimacy_kernel__instrumentalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(orthographic_legitimacy_kernel__instrumentalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(orthographic_legitimacy_kernel__instrumentalist_reading, 0.42, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).
:- end_tests(orthographic_legitimacy_kernel__instrumentalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) because the primary function is genuine coordination (mass literacy), but the devaluation of the old elite's skills is a real, asymmetric cost. Suppression is moderate (0.55) because the new script's dominance requires ongoing state enforcement in schools and official media; alternatives (Arabic script) are not actively destroyed but are structurally excluded from the legitimated public sphere. Theater is low (0.25): literacy statistics are largely sincere performance indicators, though some ritualized reporting emerges over time. The measurement series shows extraction stabilizing as the old elite ages out, while enforcement slowly normalizes and the initial high suppression requirement declines.
 *
 * PERSPECTIVAL GAP:
 *   The newly literate citizen experiences the constraint as enabling coordinationâaccess to education, state services, and modern media. The Arabic-literate elite experiences the same structural shift as extraction: a collapse in the scarcity value and institutional gatekeeper status of their skillset. The state administrator sits between, administering the coordination while aware of the distributional cost. The engine computes this divergence from identical structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The newly literate population is the structural beneficiary (d near the beneficiary end): the constraint subsidizes their entry into literacy and bureaucratic legibility. The Arabic-literate elite is the structural target (d near the target end): the constraint extracts from their accumulated skill premium. The state language commission receives administrative coordination benefits (low d) but is not the direct recipient of the extracted surplus; the extraction is a diffuse devaluation rather than a captured transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying as tangled_rope prevents misreading the instrumentalist claim as pure rope: the presence of an identifiable victim class (Arabic-literate elite) and the requirement for active state enforcement distinguish it from voluntary coordination. Conversely, it prevents misreading as snare because the coordination function (mass literacy) is genuine, the beneficiaries are the broad population rather than a narrow extractive class, and the theater ratio is low. If the founding problem (low literacy) is contested as solved or misattributed, the R5 consumer would flag potential mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Is orthographic legitimacy better understood as instrumental efficiency, civilizational alignment, or historical continuity?',
    'Comparative historical analysis of script reforms: track whether post-reform societies revert to continuity narratives, stabilize on instrumental justifications, or shift to modernist identity claims.',
    'If legitimacy is primarily instrumental, the constraint remains a tangled rope with moderate epsilon; if it collapses to modernist identity or continuity, the epsilon and beneficiary structure shift toward identity_coordination or extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'This constraint is the instrumentalist reading of a contested kernel; sibling readings alter the foundational premise and victim-beneficiary structure.').

omega_variable(
    literacy_causation_ambiguity,
    'Does the new script cause higher literacy rates, or do compulsory schooling and expanded access explain the observed gains independent of script form?',
    'Controlled comparative studies or natural experiments comparing literacy attainment under phonetic versus non-phonetic scripts with schooling held constant.',
    'If script form is not the causal factor, the instrumentalist extraction from the old elite loses its primary empirical justification, potentially reclassifying the constraint toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literacy_causation_ambiguity, empirical, 'Whether literacy gains are attributable to script reform or to broader educational expansion.').

omega_variable(
    coordination_extraction_boundary,
    'Is the devaluation of Arabic literacy an unavoidable transition cost of mass coordination, or an ongoing structural suppression of an alternative knowledge system?',
    'Longitudinal analysis of post-reform elite economic outcomes and the institutional accessibility of Arabic-script knowledge in the revised educational regime.',
    'If the devaluation is transitional, the constraint trends toward rope as the old elite exits; if it is ongoing structural suppression, the constraint remains tangled_rope or shifts toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, conceptual, 'Boundary between coordination cost and extractive devaluation of the old elite.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(orthographic_legitimacy_kernel__instrumentalist_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ortho_inst_tr_t0, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ortho_inst_tr_t10, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(ortho_inst_tr_t20, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(ortho_inst_tr_t30, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 30, 0.24).
narrative_ontology:measurement(ortho_inst_tr_t40, orthographic_legitimacy_kernel__instrumentalist_reading, theater_ratio, 40, 0.25).

% Extraction over time
narrative_ontology:measurement(ortho_inst_be_t0, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ortho_inst_be_t10, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(ortho_inst_be_t20, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(ortho_inst_be_t30, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 30, 0.42).
narrative_ontology:measurement(ortho_inst_be_t40, orthographic_legitimacy_kernel__instrumentalist_reading, base_extractiveness, 40, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(ortho_inst_su_t0, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(ortho_inst_su_t10, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(ortho_inst_su_t20, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(ortho_inst_su_t30, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(ortho_inst_su_t40, orthographic_legitimacy_kernel__instrumentalist_reading, suppression_requirement, 40, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(orthographic_legitimacy_kernel__instrumentalist_reading, information_standard).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__modernist_reading).
narrative_ontology:affects_constraint(orthographic_legitimacy_kernel__instrumentalist_reading, orthographic_legitimacy_kernel__continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the orthographic_legitimacy_kernel. The kernel decomposes into three structurally distinct constraints because the source of legitimacy (instrumental efficiency, historical continuity, or civilizational modernity) produces different epsilon values, beneficiary sets, and enforcement requirements. Each reading should be evaluated independently; this file instantiates only the instrumentalist reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
