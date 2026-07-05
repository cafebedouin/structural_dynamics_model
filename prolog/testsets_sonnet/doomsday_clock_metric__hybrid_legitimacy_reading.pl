% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__hybrid_legitimacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_doomsday_clock_metric__hybrid_legitimacy_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: doomsday_clock_metric__hybrid_legitimacy_reading
 *   human_readable: Doomsday Clock as Irreducibly Hybrid Scientific-Normative Judgment
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   This story instantiates the hybrid_legitimacy_reading of the
 *   doomsday_clock_metric kernel: the claim that the Clock's setting is not
 *   reducible to either a pure empirical measurement or a pure strategic
 *   communication device, but structurally requires both scientific judgment
 *   and normative stakes to be entangled in order to function at all — and
 *   that this entanglement is the source of its legitimacy, not a defect to
 *   be resolved. Sibling constraints (objective_index_reading,
 *   performative_tool_reading) are NOT this constraint; they instantiate
 *   incompatible premises about what the Clock IS and are authored
 *   separately.
 *
 * KEY AGENTS:
 *   - bulletin_science_security_board: Primary agenda-setter (institutional/arbitrage) — owns and administers the fused judgment
 *   - existential_risk_research_field: Beneficiary (organized/mobile) — gains public leverage from the fusion
 *   - policy_publics_seeking_calibrated_risk_signal: Primary bearer of interpretive cost (powerless/constrained)
 *   - science_policy_scholars: Analytical observer — studies whether the hybrid claim is honestly held
 *   - rival_risk_index_producers: Excluded competing framing (moderate/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__hybrid_legitimacy_reading, 0.28).
domain_priors:suppression_score(doomsday_clock_metric__hybrid_legitimacy_reading, 0.22).
domain_priors:theater_ratio(doomsday_clock_metric__hybrid_legitimacy_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(doomsday_clock_metric__hybrid_legitimacy_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__hybrid_legitimacy_reading, rope).
narrative_ontology:human_readable(doomsday_clock_metric__hybrid_legitimacy_reading, "Doomsday Clock as Irreducibly Hybrid Scientific-Normative Judgment").
narrative_ontology:topic_domain(doomsday_clock_metric__hybrid_legitimacy_reading, "science_communication/normative_epistemology/risk_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__hybrid_legitimacy_reading, '1bc5d703-1487-4f3a-a864-e0780e564d25').
narrative_ontology:cs_kernel_codification('1bc5d703-1487-4f3a-a864-e0780e564d25', distributed).
narrative_ontology:cs_authority_grounding('1bc5d703-1487-4f3a-a864-e0780e564d25', expertise).
narrative_ontology:cs_interpretation_layer_present('1bc5d703-1487-4f3a-a864-e0780e564d25').
narrative_ontology:cs_reading_relation('1bc5d703-1487-4f3a-a864-e0780e564d25', doomsday_clock_metric__objective_index_reading, influences).
narrative_ontology:cs_reading_relation('1bc5d703-1487-4f3a-a864-e0780e564d25', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_axiom('1bc5d703-1487-4f3a-a864-e0780e564d25', foundational, scientific_judgment_and_normative_stakes_are_structurally_inseparable).
narrative_ontology:cs_axiom_status(scientific_judgment_and_normative_stakes_are_structurally_inseparable, holdable).
narrative_ontology:cs_axiom_grounding('1bc5d703-1487-4f3a-a864-e0780e564d25', scientific_judgment_and_normative_stakes_are_structurally_inseparable, conventional).
narrative_ontology:cs_axiom('1bc5d703-1487-4f3a-a864-e0780e564d25', foundational, deliberate_ambiguity_is_a_legitimate_source_of_institutional_authority).
narrative_ontology:cs_axiom_status(deliberate_ambiguity_is_a_legitimate_source_of_institutional_authority, holdable).
narrative_ontology:cs_axiom_grounding('1bc5d703-1487-4f3a-a864-e0780e564d25', deliberate_ambiguity_is_a_legitimate_source_of_institutional_authority, instrumental).
narrative_ontology:cs_reference_frame('1bc5d703-1487-4f3a-a864-e0780e564d25', fused_scientific_normative_judgment_authority).
narrative_ontology:cs_drift_state('1bc5d703-1487-4f3a-a864-e0780e564d25', contemporary_media_saturation_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('1bc5d703-1487-4f3a-a864-e0780e564d25', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board).
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_field).
narrative_ontology:constraint_victim(doomsday_clock_metric__hybrid_legitimacy_reading, policy_publics_seeking_calibrated_risk_signal).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__hybrid_legitimacy_reading, scientific_judgment_cannot_be_fully_separated_from_value_stakes).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes annually to set the minute hand, explicitly blending expert threat assessment with deliberate judgment calls about which risks to foreground and how urgently to communicate them. The Board owns the ambiguity between 'measurement' and 'advocacy' as its institutional franchise — the Clock's authority rests on this fusion being presented as expert synthesis rather than either pure science or pure messaging. They control both the criteria and their application, with no external body reviewing the weighting.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__hybrid_legitimacy_reading, bulletin_science_security_board, beneficiary).

% Researchers across nuclear policy, climate, and biosecurity gain a durable, recognizable public vehicle for their findings once folded into the Clock's annual statement. The hybrid framing lets contested, value-laden judgments about which risks matter travel under the Clock's scientific-sounding authority, giving the field outsized rhetorical leverage relative to what a narrower empirical index would support.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, existential_risk_research_field, beneficiary,
    organized, generational, mobile, global).

% Journalists, policymakers, and ordinary citizens who read the Clock's annual movement as if it were a calibrated risk instrument bear the cost of the ambiguity: they cannot tell how much of a given year's shift reflects new empirical evidence versus a values-driven judgment about communication strategy, and have no mechanism to demand that distinction be made explicit. Their only real exit is disengagement from the metric altogether, which forfeits whatever genuine signal it does carry.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, policy_publics_seeking_calibrated_risk_signal, payer,
    powerless, biographical, constrained, global).

% Study the Clock as a case in the sociology and philosophy of science communication, documenting how the Board's dual mandate (assess risk / mobilize response) resists disentanglement. They neither set the Clock nor bear its communicative costs, but their analysis is the main outside check on whether the hybrid framing is honestly held or is quietly sliding toward one pole while claiming both.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, science_policy_scholars, observer,
    analytical, civilizational, analytical, global).

% Groups building more narrowly empirical existential-risk indices (probabilistic forecasting consortia, insurance-style catastrophe models) compete for public attention against the Clock's seven-decade brand recognition. They would argue the hybrid framing under-serves anyone who actually wants a disentangled empirical estimate, but the Clock's institutional weight and media familiarity crowd out that argument from mainstream coverage.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__hybrid_legitimacy_reading, rival_risk_index_producers, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, memorable, recurring public signal that synthesizes expert judgment across multiple existential-risk domains (nuclear, climate, biosecurity, disruptive technology) into one communicable artifact, solving the coordination problem of getting disparate technical communities' concerns into shared public and policy attention at all.
% TRANSFER_FUNCTION: Moves interpretive authority and rhetorical leverage from the diffuse, harder-to-communicate work of individual risk-domain researchers to the Bulletin's Board, and moves epistemic clarity away from publics who receive a fused signal without the tools to separate its empirical and normative components.
% ABSENT_VOICES: Rival empirical-index producers and forecasting methodologists who would press for disentangling measurement from advocacy are not represented on the Board and get little mainstream coverage set against the Clock's institutional legacy; publics who want a purely calibrated instrument have no seat in the annual setting process at all.
% DISAPPEARANCE_RATIONALE: Researchers in the affected risk fields would lose a shared communicative vehicle and some public visibility would migrate to fragmented, less legible channels — a real rearrangement. But policy publics who currently mistake the Clock for a calibrated index might experience little functional loss, since the underlying empirical risk assessments would continue through other channels; whether the world 'rearranges' or 'stays the same' depends on which function of the Clock one weights, which is exactly the ambiguity this reading holds is irreducible.
% FOUNDING_PROBLEM: Physicists who built the first atomic weapons needed a way to communicate the urgency of nuclear danger to a public and policy establishment that lacked the technical background to evaluate the risk on its own terms — a problem inseparably both empirical (how dangerous is this) and normative (how should people feel and act about it).
% FOUNDING_PROBLEM_CORROBORATION: Science communication scholars outside the Bulletin (e.g. STS researchers studying boundary objects between science and advocacy) corroborate that the entanglement problem the Clock was built to hold together — technical risk assessment inseparable from mobilizing normative stakes — remains structurally unresolved in existential risk communication generally, not merely persisting because the Bulletin benefits from it. No purely empirical successor index has displaced the Clock's public function, which is itself evidence the underlying entanglement problem has not been solved by disaggregation.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__hybrid_legitimacy_reading, contested).
narrative_ontology:founding_problem_status(doomsday_clock_metric__hybrid_legitimacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(doomsday_clock_metric__hybrid_legitimacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__hybrid_legitimacy_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).
:- end_tests(doomsday_clock_metric__hybrid_legitimacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low-moderate (0.28) because the hybrid reading, taken on its own terms, does not claim to deliver a disentangled empirical product — so publics are not being defrauded of something the Clock promised to give cleanly; the cost they bear is interpretive confusion, not diverted resources. Suppression is low (0.22): no one is coercively prevented from building rival indices, though the Clock's institutional weight crowds them out informally. Theater ratio is moderate and rising (0.20 -> 0.42 over the interval) because as the Clock's media profile has grown, an increasing share of the annual announcement's function is public-attention maintenance rather than incremental empirical update — consistent with a genuine coordination function that has partially calcified into performance without becoming purely extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   The Board sits closest to the beneficiary end: it controls both the assessment and its communicative framing, and its institutional standing depends on the fusion being received as legitimate expert judgment rather than either raw data or naked advocacy. The research field benefits secondarily by riding the Clock's authority. Policy publics carry the cost of not being able to separate signal from stance, but this is a diffuse, interpretive cost rather than an extracted resource — which is why victims are named narrowly and extraction stays moderate rather than high.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid_legitimacy_reading's founding problem (communicating technically inaccessible existential risk in a way that also motivates action) remains live per outside STS corroboration, which blocks a mandatrophy verdict: this is not an arrangement whose function has died while the form persists. The contested disappearance_verdict itself demonstrates the reading's core claim — because the entanglement is genuine, even the counterfactual of the Clock's disappearance cannot be cleanly evaluated along a single empirical-vs-normative axis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_legitimacy_vs_disaggregation,
    'Is the entanglement of empirical assessment and normative stakes in the Clock genuinely irreducible, or could a sufficiently careful methodology separate ''how risky is the world'' from ''how should we frame that risk for maximal mobilization'' — meaning the hybrid framing is a convenient but not necessary institutional choice?',
    'Compare the Board''s internal deliberation records (where available) against independent probabilistic forecasting benchmarks over the same years: persistent, principled divergence not explainable by forecasting error would support genuine irreducibility; convergence would suggest the entanglement is a communicative choice rather than a structural necessity.',
    'If irreducible, this reading (hybrid_legitimacy) is the correct structural account and the sibling readings are each partial captures of one pole. If reducible, the objective_index_reading and performative_tool_reading jointly exhaust the phenomenon and this reading''s central claim collapses into a description of institutional convenience rather than structural fact.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_legitimacy_vs_disaggregation, conceptual, 'Whether the scientific-normative entanglement is structurally necessary or a chosen institutional convenience.').

omega_variable(
    beneficiary_structure_of_deliberate_ambiguity,
    'Does the Board''s control over an irreducibly ambiguous instrument constitute a form of unaccountable power (no external body can ever fully audit a judgment call that is legitimately partly normative), or is this the necessary and appropriate discretion of any expert body making value-laden risk communications?',
    'Track whether any formal accountability mechanism (peer review of the Board''s methodology, external audit of the weighting between empirical and normative components) has ever been proposed or adopted, and whether such a mechanism is even conceptually coherent given the hybrid claim.',
    'If no coherent accountability mechanism is possible even in principle, the hybrid framing functions as an accountability void that happens also to be genuine — supporting a partial tangled_rope reading at the accountability layer even while the coordination function remains real. If an accountability mechanism is feasible but simply unadopted, the current absence looks more like an extractable governance gap.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_structure_of_deliberate_ambiguity, conceptual, 'Whether the deliberate ambiguity that grounds legitimacy also forecloses accountability in principle or merely in current practice.').

omega_variable(
    kernel_reading_selection_evidence,
    'What in the Bulletin''s own public statements and the surrounding scholarly literature justifies treating this constraint as the hybrid_legitimacy_reading rather than one of the sibling readings — i.e., is the Bulletin''s own self-description closer to ''we measure'' (objective_index_reading), ''we mobilize'' (performative_tool_reading), or an explicit claim of inseparability (this reading)?',
    'Textual analysis of the Bulletin''s annual statements and Board member public commentary for explicit claims about the nature of the Clock (measurement vs. advocacy vs. fusion), compared against the STS/science-communication literature''s characterization.',
    'If the Bulletin''s own framing leans consistently toward one pole, the hybrid_legitimacy_reading may be more analytically imposed than institutionally self-held, which would weaken (though not necessarily refute) this story''s claim that the ambiguity is the Board''s own deliberate legitimacy strategy rather than an external observer''s interpretive gloss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'Whether the hybrid framing is the Board''s own self-understanding or an outside analytical characterization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__hybrid_legitimacy_reading, 1947, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1947, 0.2).
narrative_ontology:measurement(doom_tr_t1970, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(doom_tr_t1990, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(doom_tr_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2005, 0.35).
narrative_ontology:measurement(doom_tr_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(doom_tr_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(doom_be_t1947, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1947, 0.1).
narrative_ontology:measurement(doom_be_t1970, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(doom_be_t1990, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 1990, 0.18).
narrative_ontology:measurement(doom_be_t2005, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(doom_be_t2015, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2015, 0.25).
narrative_ontology:measurement(doom_be_t2024, doomsday_clock_metric__hybrid_legitimacy_reading, base_extractiveness, 2024, 0.28).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(doomsday_clock_metric__hybrid_legitimacy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__hybrid_legitimacy_reading, information_standard).
narrative_ontology:boltzmann_floor_override(doomsday_clock_metric__hybrid_legitimacy_reading, 0.05).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__objective_index_reading).
narrative_ontology:affects_constraint(doomsday_clock_metric__hybrid_legitimacy_reading, doomsday_clock_metric__performative_tool_reading).

% DUAL FORMULATION NOTE:
% This story is one of three sibling constraints decomposing the natural-language label 'the Doomsday Clock' per the ε-invariance principle. objective_index_reading treats the Clock as (attempted) calibrated measurement — its ε and beneficiary structure differ because it is evaluated against forecasting accuracy. performative_tool_reading treats the Clock as strategically optimized advocacy — its ε and beneficiary structure differ because it is evaluated against mobilization effectiveness. This hybrid_legitimacy_reading claims the other two readings each capture a real but partial pole, and that the fusion itself is what the Board actually administers. All three share the kernel doomsday_clock_metric but are structurally distinct constraints with independent metrics, not the same constraint viewed three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
