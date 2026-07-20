% ============================================================================
% CONSTRAINT STORY: doomsday_clock_metric__objective_index_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: doomsday_clock_metric__objective_index_reading
 *   human_readable: Doomsday Clock Objective Index Reading
 *   domain: science_communication/normative_epistemology/risk_governance
 *
 * SUMMARY:
 *   The Doomsday Clock, maintained by the Bulletin of the Atomic Scientists,
 *   is read here as an objective index that synthesizes empirical indicators
 *   of existential risk into a single communicable metric. Under this
 *   reading, the clock setting is a scientific measurement produced by expert
 *   deliberation. Structurally, however, the constraint concentrates
 *   interpretive authority over existential risk in an unelected expert body,
 *   suppressing the normative framing that inevitably enters risk weighting
 *   and priority-setting. The scientific authority benefits from
 *   agenda-setting power and institutional legitimacy; democratic
 *   accountability is the victim, as publics and their representatives are
 *   excluded from the synthesis and reduced to passive recipients of expert
 *   judgment.
 *
 * KEY AGENTS:
 *   - Bulletin of the Atomic Scientists and affiliated board (expert_risk_community): Agenda-setter and primary beneficiary â synthesizes indicators, controls methodology, and garners institutional legitimacy.
 *   - Global democratic publics (democratic_public): Primary payer/victim â bear the democratic deficit of excluded deliberation and constrained risk interpretation.
 *   - Elected policy makers (policy_makers): Secondary payer â constrained by the objective-index framing when setting policy agendas.
 *   - Science and technology studies scholars (critical_scholars): Observer â analyze the epistemic and normative commitments embedded in the clock.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(doomsday_clock_metric__objective_index_reading, 0.72).
domain_priors:suppression_score(doomsday_clock_metric__objective_index_reading, 0.82).
domain_priors:theater_ratio(doomsday_clock_metric__objective_index_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(doomsday_clock_metric__objective_index_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(doomsday_clock_metric__objective_index_reading, tangled_rope).
narrative_ontology:human_readable(doomsday_clock_metric__objective_index_reading, "Doomsday Clock Objective Index Reading").
narrative_ontology:topic_domain(doomsday_clock_metric__objective_index_reading, "science_communication/normative_epistemology/risk_governance").

domain_priors:requires_active_enforcement(doomsday_clock_metric__objective_index_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(doomsday_clock_metric__objective_index_reading, '2bcc6377-ce28-4755-9d38-e8c436ece017').
narrative_ontology:cs_kernel_codification('2bcc6377-ce28-4755-9d38-e8c436ece017', formalized).
narrative_ontology:cs_authority_grounding('2bcc6377-ce28-4755-9d38-e8c436ece017', expertise).
narrative_ontology:cs_interpretation_layer_present('2bcc6377-ce28-4755-9d38-e8c436ece017').
narrative_ontology:cs_reading_relation('2bcc6377-ce28-4755-9d38-e8c436ece017', doomsday_clock_metric__performative_tool_reading, influences).
narrative_ontology:cs_reading_relation('2bcc6377-ce28-4755-9d38-e8c436ece017', doomsday_clock_metric__hybrid_legitimacy_reading, forecloses).
narrative_ontology:cs_axiom('2bcc6377-ce28-4755-9d38-e8c436ece017', foundational, empirical_risk_commensurable).
narrative_ontology:cs_axiom_status(empirical_risk_commensurable, holdable).
narrative_ontology:cs_axiom_grounding('2bcc6377-ce28-4755-9d38-e8c436ece017', empirical_risk_commensurable, empirically_contingent).
narrative_ontology:cs_axiom('2bcc6377-ce28-4755-9d38-e8c436ece017', foundational, expert_synthesis_objectivity).
narrative_ontology:cs_axiom_status(expert_synthesis_objectivity, holdable).
narrative_ontology:cs_axiom_grounding('2bcc6377-ce28-4755-9d38-e8c436ece017', expert_synthesis_objectivity, instrumental).
narrative_ontology:cs_reference_frame('2bcc6377-ce28-4755-9d38-e8c436ece017', empirical_risk_index_framework).
narrative_ontology:cs_drift_state('2bcc6377-ce28-4755-9d38-e8c436ece017', contemporary_multi_risk_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2bcc6377-ce28-4755-9d38-e8c436ece017', '').
narrative_ontology:cs_kernel_id(doomsday_clock_metric__objective_index_reading, doomsday_clock_metric).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(doomsday_clock_metric__objective_index_reading, expert_risk_community).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, democratic_public).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(doomsday_clock_metric__objective_index_reading, policy_makers).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, fact_value_separability).
narrative_ontology:constraint_vindicates(doomsday_clock_metric__objective_index_reading, expert_supremacy_in_risk_governance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes the expert board, synthesizes empirical indicators across nuclear weapons, climate change, biosecurity, and artificial intelligence, and announces the annual clock setting. Derives institutional legitimacy, media attention, and agenda-setting power from the claim that the clock is an objective index of empirical risk. Controls access to the deliberative process and the methodology behind the setting.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, expert_risk_community, agenda_setter,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(doomsday_clock_metric__objective_index_reading, expert_risk_community, beneficiary).

% Receives the clock announcement as authoritative risk information through mass media diffusion. Lacks access to the expert deliberation or methodology. Bears the democratic cost of having existential risk priorities and normative trade-offs determined by an unelected expert body, with no democratic recourse or alternative authoritative interpretive framework.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, democratic_public, payer,
    powerless, generational, trapped, global).

% Referenced by the clock setting in legislative and executive discourse. The objective-index framing pressures them to adopt the expert risk hierarchy, constraining independent democratic deliberation on normative trade-offs. Can ignore the clock but at significant reputational cost in the scientific and media environment.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, policy_makers, payer,
    powerful, biographical, constrained, national).

% Study the clock as a science communication device with embedded normative commitments. Publish critiques arguing the clock conflates empirical indicators with value judgments. Hold no institutional power over the setting process.
narrative_ontology:constraint_stakeholder(doomsday_clock_metric__objective_index_reading, critical_scholars, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(doomsday_clock_metric__objective_index_reading, expert_risk_community).
narrative_ontology:fixing_cost_class(doomsday_clock_metric__objective_index_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aggregates complex, multi-domain existential risk indicators into a single communicable metric so that inattentive publics and policy makers can grasp global catastrophic risk without scientific literacy across nuclear, climate, biosecurity, and AI domains.
% TRANSFER_FUNCTION: Moves interpretive authority over existential risk prioritization from democratic publics and elected representatives to a closed expert body; moves media and policy attention toward the risks the experts weight most heavily.
% ABSENT_VOICES: Lay publics without scientific credentials, participatory deliberation bodies, indigenous knowledge holders, and civil society risk panels are excluded from the synthesis process. They would contest the normative weighting of risks and the commensuration of disparate threats if admitted.
% DISAPPEARANCE_RATIONALE: The clock is a focal point for existential risk discourse. Its disappearance would force media and policy to rely on more diffuse or democratically accountable risk assessments, rearranging the epistemic authority structure of risk governance and opening space for competing indices.
% FOUNDING_PROBLEM: How to communicate complex, multi-domain existential risks to the public and policy makers in a way that motivates preventive action without requiring universal scientific literacy.
% FOUNDING_PROBLEM_CORROBORATION: The expert community attests the problem remains live, citing public inattention and policy fragmentation. Critical scholars in science and technology studies and independent media scholars attest that alternative communication mechanisms and competing indices exist, suggesting the founding problem has alternative solutions that do not concentrate authority. No corroborating source outside the benefiting parties fully supports the claim that this specific authority concentration is necessary.
narrative_ontology:disappearance_verdict(doomsday_clock_metric__objective_index_reading, world_rearranges).
narrative_ontology:founding_problem_status(doomsday_clock_metric__objective_index_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(doomsday_clock_metric__objective_index_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(doomsday_clock_metric__objective_index_reading, 'none', 1).
narrative_ontology:epsilon_provenance(doomsday_clock_metric__objective_index_reading, 0.72, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.72) is high because the constraint transfers interpretive monopoly over existential risk from democratic publics to a closed expert body. Suppression (0.82) is higher still: the objective-index framing actively suppresses visibility of the normative weighting and value choices embedded in the synthesis. Theater_ratio (0.45) reflects that while genuine expert deliberation occurs, the 'minutes to midnight' metaphor and press-event structure perform alarm that outstrips methodological precision. Accessibility_collapse (0.65) captures how the clock's brand dominance crowds out participatory or alternative risk indices. Resistance (0.35) is moderate: critical scholars and some policy actors contest the monopoly, but the scientific authority dampens effective challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the expert seat, the constraint is a necessary coordination mechanism that solves the genuinely hard problem of communicating complex, multi-domain risk to inattentive publics and policy makers. From the democratic public seat, the same structure is an extractive epistemic gate: a small group decides which risks count, how much they count, and what 'midnight' means, while presenting these choices as inevitable empirical findings. The engine should compute high directionality (near-target) for the democratic public and low directionality (near-beneficiary) for the expert community, yielding divergent per-seat classifications.
 *
 * DIRECTIONALITY LOGIC:
 *   The expert_risk_community is the structural beneficiary: they set the rules, control the methodology, collect legitimacy and media attention, and face low effective extraction (d near 0.0). The democratic_public is the structural target: they receive the risk framing but cannot modify it, and their democratic alternatives (deliberation, competing indices) are suppressed (d near 1.0). Policy_makers sit in the middle â they partially benefit from expert cover but are constrained by it (d ~0.5). Critical_scholars are analytical observers with arbitrage-grade exit (d not computed for observers or derived as analytical).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resists simple mandatrophy classification because its founding coordination function â communicating existential risk â remains live. However, the objective-index reading suppresses awareness that the coordination is not pure: it embeds an expert monopoly. Classifying it as a pure rope would ignore the democratic victim; classifying it as a pure snare would ignore the genuine information aggregation. Tangled_rope captures the hybridity: the clock does coordinate global attention, but the same structure extracts democratic accountability through asymmetric control of the synthesis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_of_risk_synthesis,
    'Does the expert synthesis process genuinely eliminate normative framing, or does it merely embed tacit value judgments behind empirical indicators?',
    'Ethnographic access to deliberation transcripts, methodological audits, and comparative analysis of how different risk domains are weighted against each other.',
    'If normative framing is irreducible, the objective-index claim is a false summit and the constraint is more extractive than coordinated, potentially pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_of_risk_synthesis, empirical, 'Whether the clock''s empirical synthesis is genuinely value-free.').

omega_variable(
    democratic_accountability_tradeoff,
    'Is the expert monopoly on interpretation a necessary cost of accurate risk communication, or is it an avoidable democratic deficit?',
    'Comparative analysis of participatory risk governance models that achieve similar communication outcomes without expert monopoly over synthesis and interpretation.',
    'If participatory models work, the victim status of democratic accountability is strengthened; if they fail, the coordination function may justify the extraction, reinforcing tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_accountability_tradeoff, conceptual, 'Whether expert monopoly is necessary for the coordination function.').

omega_variable(
    kernel_reading_objectivity_commit,
    'Does the objective-index reading represent the actual epistemic structure of the Doomsday Clock, or is it a legitimacy narrative that coexists with performative and hybrid functions within the same institution?',
    'Historical institutional sociology of the Bulletin and discourse analysis of public communications versus internal deliberations across audiences.',
    'If the reading is primarily a legitimacy narrative, the constraint''s coordination function is weaker and its extractiveness is stronger, potentially pushing classification toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_objectivity_commit, conceptual, 'Whether the objective-index reading is an operational epistemology or a legitimacy strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(doomsday_clock_metric__objective_index_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(doom_tr_t0, doomsday_clock_metric__objective_index_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(doom_tr_t8, doomsday_clock_metric__objective_index_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(doom_tr_t16, doomsday_clock_metric__objective_index_reading, theater_ratio, 16, 0.35).
narrative_ontology:measurement(doom_tr_t24, doomsday_clock_metric__objective_index_reading, theater_ratio, 24, 0.38).
narrative_ontology:measurement(doom_tr_t32, doomsday_clock_metric__objective_index_reading, theater_ratio, 32, 0.42).
narrative_ontology:measurement(doom_tr_t40, doomsday_clock_metric__objective_index_reading, theater_ratio, 40, 0.45).

% Extraction over time
narrative_ontology:measurement(doom_be_t0, doomsday_clock_metric__objective_index_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(doom_be_t8, doomsday_clock_metric__objective_index_reading, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(doom_be_t16, doomsday_clock_metric__objective_index_reading, base_extractiveness, 16, 0.55).
narrative_ontology:measurement(doom_be_t24, doomsday_clock_metric__objective_index_reading, base_extractiveness, 24, 0.62).
narrative_ontology:measurement(doom_be_t32, doomsday_clock_metric__objective_index_reading, base_extractiveness, 32, 0.68).
narrative_ontology:measurement(doom_be_t40, doomsday_clock_metric__objective_index_reading, base_extractiveness, 40, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(doom_su_t0, doomsday_clock_metric__objective_index_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(doom_su_t8, doomsday_clock_metric__objective_index_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(doom_su_t16, doomsday_clock_metric__objective_index_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(doom_su_t24, doomsday_clock_metric__objective_index_reading, suppression_requirement, 24, 0.76).
narrative_ontology:measurement(doom_su_t32, doomsday_clock_metric__objective_index_reading, suppression_requirement, 32, 0.8).
narrative_ontology:measurement(doom_su_t40, doomsday_clock_metric__objective_index_reading, suppression_requirement, 40, 0.82).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(doomsday_clock_metric__objective_index_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
