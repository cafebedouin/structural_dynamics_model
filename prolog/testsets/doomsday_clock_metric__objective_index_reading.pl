% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__objective_index_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock as Objective Risk Index
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock is a symbolic representation of global existential
 *   risk, maintained by a committee of expert scientists who annually adjust
 *   the Clock's hand position based on their synthesis of empirical
 *   indicators across nuclear, biological, climate, and artificial
 *   intelligence domains. This constraint story instantiates the
 *   OBJECTIVE_INDEX_READING: a framework that treats the Clock's setting as a
 *   legitimate technical measure of measurable existential risk, grounded in
 *   empirical synthesis and expert judgment. Under this reading, the Clock
 *   translates diverse scientific data into actionable global risk signals.
 *   However, this reading suppresses normative deliberation about what should
 *   count as existential risk, which communities' knowledge systems are
 *   legitimate in that assessment, and how democratic publics should
 *   participate in existential-risk governance. The constraint extracted from
 *   this reading is not the Clock's symbolic power itself, but the
 *   institutional monopoly on existential-risk framing that the Clock
 *   operationalizes and legitimates.
 *
 * KEY AGENTS:
 *   - Doomsday Clock setting committee: institutional agenda-setter, interprets indicators, holds authority over annual decision
 *   - Expert risk assessment authority (nuclear physicists, biosecurity, AI safety, climate science): institutional beneficiary, gains legitimacy and influence from Clock's operationalization
 *   - Democratic public accountability: victim, powerless, identity-locked to expert frames, loses participatory voice in risk governance
 *   - Alternative risk framings (social, economic, civilizational): victim, constrained, excluded from Clock synthesis
 *   - Policy stakeholders (governments, foundations): dual-positioned, benefit from Clock legitimacy but constrained by Clock's judgment
 *   - Dissenting expert voices: excluded, structurally barred from alternative Clock setting despite having expertise
 *   - Scientific integrity observers: analytical seat, can monitor but not alter the constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.68).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.79).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock as Objective Risk Index").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '00f47331-81dc-4ebd-80b7-8cbd995a56e6').
narrative_ontology:cs_kernel_codification('00f47331-81dc-4ebd-80b7-8cbd995a56e6', fixed_text).
narrative_ontology:cs_authority_grounding('00f47331-81dc-4ebd-80b7-8cbd995a56e6', expertise).
narrative_ontology:cs_interpretation_layer_present('00f47331-81dc-4ebd-80b7-8cbd995a56e6').
narrative_ontology:cs_reading_relation('00f47331-81dc-4ebd-80b7-8cbd995a56e6', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_reading_relation('00f47331-81dc-4ebd-80b7-8cbd995a56e6', doomsday_clock_metric__performative_tool_reading, coexists_with).
narrative_ontology:cs_axiom('00f47331-81dc-4ebd-80b7-8cbd995a56e6', foundational, existential_risk_measurable_empirically).
narrative_ontology:cs_axiom_status(existential_risk_measurable_empirically, holdable).
narrative_ontology:cs_axiom_grounding('00f47331-81dc-4ebd-80b7-8cbd995a56e6', existential_risk_measurable_empirically, empirically_contingent).
narrative_ontology:cs_axiom('00f47331-81dc-4ebd-80b7-8cbd995a56e6', foundational, expert_synthesis_sufficient_for_governance).
narrative_ontology:cs_axiom_status(expert_synthesis_sufficient_for_governance, holdable).
narrative_ontology:cs_axiom_grounding('00f47331-81dc-4ebd-80b7-8cbd995a56e6', expert_synthesis_sufficient_for_governance, deontological).
narrative_ontology:cs_reference_frame('00f47331-81dc-4ebd-80b7-8cbd995a56e6', expert_scientific_consensus_on_global_risk).
narrative_ontology:cs_drift_state('00f47331-81dc-4ebd-80b7-8cbd995a56e6', contemporary_expanded_risk_domain_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('00f47331-81dc-4ebd-80b7-8cbd995a56e6', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_risk_assessment_authority).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_public_accountability).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, alternative_risk_framings).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__objective_index_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(doomsday_clock_metric__objective_index_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises sharply over the 77-year interval (0.31 to 0.68), tracking the Clock's expanding scope (from nuclear weapons to biosecurity, climate, AI) and deepening institutional entrenchment. Early Clock readings were narrow and somewhat tentative; contemporary readings claim authority over AI risk — a domain without historical precedent and high epistemic uncertainty. Suppression requirement rises in tandem (0.42 to 0.79), indicating that maintaining the Clock's consensus now requires active suppression of dissenting expert voices and alternative risk framings that would fragment the public narrative. Theater ratio remains low-to-moderate (0.08 to 0.22), reflecting that the Clock's core function (crystallizing global risk perception) is real, but growing share of its work is narrative maintenance rather than data integration. Accessibility collapse is high (0.71) because once the Clock frames existential risk as a measurable quantity, alternative frameworks become difficult to articulate — the Clock becomes the common language for risk talk. Resistance is moderate (0.58) because some expert voices and democratic constituencies resist the expert monopoly, but they lack institutional parity with the Clock.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (democratic publics, alternative risk framings) experience the constraint as suppressive expert monopoly because their exit requires challenging the Clock's epistemic authority — identity-locked exit, where accepting the Clock's frame is treated as accepting objective reality rather than a constructed interpretive choice. The agenda-setter and beneficiary seats experience the constraint as necessary coordination because the clock-setting process is genuinely difficult, requires expertise, and produces real public value. Neither experience is false; they both follow from the structural position. The engine computes per-seat classification; the disagreement between seats is not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary seat (expert authority): d derives low because this institutional actor collects legitimacy, influence, and resource allocation guidance from the Clock without running a cost-bearing operation themselves — they interpret and set, others implement. The Clock amplifies their voice; it does not constrain it. Victim seats (democratic accountability, alternative framings): d derives high because these actors bear the cost of epistemic exclusion without collecting benefit. They cannot exit without organizing a competing global risk assessment (prohibitively costly) or rejecting the Clock's frame (identity-locked, requires rejecting expert-consensus-as-truth). The suppression is that once the Clock frame is accepted, alternatives become cognitively inaccessible.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to integrate diverse expert judgments on global existential risk) was substantive through ~2000. The problem has shifted: the contemporary question is not 'can we integrate expertise?' but 'who gets to decide what risks matter and how should that decision be made?' This is a different question — normative and democratic, not technical. The Clock's structure forecloses this shifted question; it continues to operate as if the founding problem is still technical consensus-building, when the real contestation is over whose voice gets to count as existential-risk expertise. This is not mandatrophy in the sense of a dead function maintained theatrically; rather, it is a constraint whose founding problem has evolved into a problem the constraint actively prevents from being addressed. The Clock persists not because its technical function is obsolete but because its beneficiaries have institutional incentive to maintain it as-is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurability_of_existential_risk,
    'Can existential risk be measured through expert synthesis of empirical indicators, or does existential risk assessment inevitably entangle scientific judgment with normative assumptions about what civilizations should value and which futures count as loss?',
    'Meta-analysis of Clock-setting deliberations over 20-year windows: do the annual updates track objectively new empirical evidence, or do they track changes in expert committee composition, funding priorities, and normative commitments about which risks deserve focus?',
    'If existential risk proves measurable independently of normative framing, the objective_index_reading holds and the Clock is a legitimate technical instrument. If measurement proves entangled with values, the hybrid_legitimacy_reading becomes structurally more accurate and the Clock''s authority claim requires reworking.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(measurability_of_existential_risk, conceptual, 'Whether existential risk measurement is epistemically separable from normative value judgments.').

omega_variable(
    expert_monopoly_on_risk_frame,
    'Does the Clock''s expert-committee structure suppress legitimate alternative framings of global existential risk, or does it appropriately concentrate risk assessment in domains where expertise is measurable and necessary?',
    'Comparative institutional analysis: examine whether constraints (democratic, social-science, indigenous-knowledge) on Clock setting increase or decrease public understanding and preparedness for global risks. Monitor dissenting-expert and alternative-framing institutional health over time.',
    'If suppression harms public understanding, the democratic-accountability victim designation is vindicated and remedies (deliberative governance, knowledge pluralism) are warranted. If expert concentration improves outcomes, the expert-monopoly framing is challenged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expert_monopoly_on_risk_frame, empirical, 'Whether expert monopoly on risk framing suppresses or improves existential-risk governance.').

omega_variable(
    extraction_vs_coordination_scope_expansion,
    'Does the Clock''s expansion from nuclear risk (1947) to nuclear + biological + climate + AI (2024) represent legitimate scope expansion of the expert-coordination function, or does it represent institutional mission creep and extraction of authority over domains (AI development, climate policy) where expert authority should be contested rather than concentrated?',
    'Institutional history: examine whether Clock-setting expansions were driven by empirical risk emergence or by expert-community strategic positioning to maintain relevance and funding. Compare Clock risk assessment to independent expert assessment in expanded domains.',
    'If expansion tracks genuine empirical emergence, the coordination framing holds. If expansion is strategic, it evidences extraction through scope creep — the Clock uses its established nuclear-risk authority to colonize new domains without re-legitimating in those domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_scope_expansion, empirical, 'Whether Clock scope expansion represents legitimate coordination or strategic extraction.').

omega_variable(
    reading_identity_committer,
    'This constraint instantiates the objective_index_reading of doomsday_clock_metric, which claims existential risk is measurable through expert empirical synthesis. The hybrid_legitimacy_reading disputes whether measurement can be separated from normative framing. Which reading more accurately captures the Clock''s actual operation: a technical instrument (objective), or an inherently value-laden artifact (hybrid)?',
    'Detailed content analysis of Clock-setting committee deliberations and published rationales over 10-year windows, coded for: (a) empirical-evidence citations vs. (b) normative-value premises about which risks matter, (c) explicit acknowledgment vs. suppression of value premises in public communications.',
    'Evidence heavy on (a) with minimal (b) supports objective_index_reading. Evidence showing strong (b) underpinning all (a) supports hybrid_legitimacy_reading. This would reframe the constraint as a boundary object requiring deliberative governance, not expert monopoly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_committer, conceptual, 'Committer axis: whether the Clock is legitimately objective measurement or inevitably hybrid normative-empirical artifact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__objective_index_reading, theater_ratio, 1947, 0.08).
narrative_ontology:measurement(doom_tr_t1965, doomsday_clock_metric__objective_index_reading, theater_ratio, 1965, 0.11).
narrative_ontology:measurement(doom_tr_t1985, doomsday_clock_metric__objective_index_reading, theater_ratio, 1985, 0.14).
narrative_ontology:measurement(doom_tr_t2000, doomsday_clock_metric__objective_index_reading, theater_ratio, 2000, 0.17).
narrative_ontology:measurement(doom_tr_t2012, doomsday_clock_metric__objective_index_reading, theater_ratio, 2012, 0.19).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__objective_index_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1947, 0.31).
narrative_ontology:measurement(doom_be_t1965, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1965, 0.42).
narrative_ontology:measurement(doom_be_t1985, doomsday_clock_metric__objective_index_reading, base_extractiveness, 1985, 0.51).
narrative_ontology:measurement(doom_be_t2000, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(doom_be_t2012, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2012, 0.63).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__objective_index_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t1947, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1947, 0.42).
narrative_ontology:measurement(doom_su_t1965, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1965, 0.54).
narrative_ontology:measurement(doom_su_t1985, doomsday_clock_metric__objective_index_reading, suppression_requirement, 1985, 0.63).
narrative_ontology:measurement(doom_su_t2000, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(doom_su_t2012, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2012, 0.74).
narrative_ontology:measurement(doom_su_t2024, doomsday_clock_metric__objective_index_reading, suppression_requirement, 2024, 0.79).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__objective_index_reading, 0.12).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__performative_tool_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric__hybrid_legitimacy_reading).

% DUAL FORMULATION NOTE:
% The Doomsday Clock instantiates a contested kernel: the same stabilized artifact (clock face, annual setting, expert committee) is read as three structurally distinct constraints depending on which epistemic frame is foregrounded. The objective_index_reading (this file) treats the Clock as a technical instrument measuring empirical existential risk; it suppresses the insight that risk assessment entangles science and values. The hybrid_legitimacy_reading argues that measurement and normative framing are inseparable in existential-risk governance and that the Clock's structure must be redesigned to acknowledge that entanglement. The performative_tool_reading argues that the Clock is strategically chosen to maximize policy mobilization and that its 'objective' framing is a cover story for influence-seeking. Each reading is a complete, ε-invariant constraint with its own beneficiary structure, suppression mechanism, and type. Links between files: objective_index_reading forecloses hybrid_legitimacy_reading (claims measurement is separable); coexists_with performative_tool_reading (both can claim objective measurement; they differ on motive). The engine computes per-reading classifications; divergence across readings is diagnostic evidence for kernel contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(doomsday_clock_metric__objective_index_reading, powerless, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
